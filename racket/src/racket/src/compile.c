/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.
*/

/* This file implements macro expansion and front-end compilation.
   Instead of always fully expanding code and then compiling it to an
   intermediate format, the compiler front-end expands as it goes,
   which enables some shortcuts compared to fully expanding first.

   The intermediate format generated from here accumulates references
   to non-local variables in a prefix, and it indicates whether each
   local variable is mutatble.

   See "eval.c" for an overview of compilation passes.

   The main compile/expand loop is compile_expand_expr(). */

#include "schpriv.h"
#include "schmach.h"

/* symbols */
ROSYM static Scheme_Object *lambda_symbol;
ROSYM static Scheme_Object *case_lambda_symbol;
ROSYM static Scheme_Object *ref_symbol;
ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *if_symbol;
ROSYM static Scheme_Object *set_symbol;
ROSYM static Scheme_Object *let_values_symbol;
ROSYM static Scheme_Object *letrec_values_symbol;
ROSYM static Scheme_Object *begin_symbol;
ROSYM static Scheme_Object *begin0_symbol;
ROSYM static Scheme_Object *with_cont_mark_symbol;
ROSYM static Scheme_Object *define_values_symbol;

ROSYM static Scheme_Object *compiler_inline_hint_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *values_symbol;
ROSYM static Scheme_Object *call_with_values_symbol;
ROSYM static Scheme_Object *inferred_name_symbol;

/* locals */
static Scheme_Object *lambda_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *case_lambda_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *ref_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *quote_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *if_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *set_compile(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *letrec_values_compile (Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *begin_compile (Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *begin0_compile (Scheme_Object *form, Scheme_Comp_Env *env);
static Scheme_Object *with_cont_mark_compile(Scheme_Object *form, Scheme_Comp_Env *env);

static Scheme_Object *compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, int app_position);
static Scheme_Object *compile_list(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Comp_Env *last_env,
                                   int start_app_position);
static Scheme_Object *compile_app(Scheme_Object *form, Scheme_Comp_Env *env);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)
#define icons(a,b) scheme_make_pair(a,b)

/**********************************************************************/
/*                          initialization                            */
/**********************************************************************/

void scheme_init_compile (Scheme_Startup_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(lambda_symbol);
  REGISTER_SO(case_lambda_symbol);
  REGISTER_SO(ref_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(if_symbol);
  REGISTER_SO(set_symbol);
  REGISTER_SO(let_values_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(begin0_symbol);
  REGISTER_SO(with_cont_mark_symbol);
  REGISTER_SO(define_values_symbol);

  lambda_symbol = scheme_intern_symbol("lambda");
  case_lambda_symbol = scheme_intern_symbol("case-lambda");
  ref_symbol = scheme_intern_symbol("#%variable-reference");
  quote_symbol = scheme_intern_symbol("quute");
  if_symbol = scheme_intern_symbol("if");
  set_symbol = scheme_intern_symbol("set!");
  let_values_symbol = scheme_intern_symbol("let-values");
  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  begin_symbol = scheme_intern_symbol("begin");
  begin0_symbol = scheme_intern_symbol("begin0");
  with_cont_mark_symbol = scheme_intern_symbol("with-continuation-mark");
  define_values_symbol = scheme_intern_symbol("define-values");

  REGISTER_SO(compiler_inline_hint_symbol);
  REGISTER_SO(inferred_name_symbol);

  scheme_undefined->type = scheme_undefined_type;
  
  compiler_inline_hint_symbol = scheme_intern_symbol("compiler-hint:cross-module-inline");

  inferred_name_symbol = scheme_intern_symbol("inferred-name");

  REGISTER_SO(protected_symbol);
  REGISTER_SO(values_symbol);
  REGISTER_SO(call_with_values_symbol);

  protected_symbol = scheme_intern_symbol("protected");
  values_symbol = scheme_intern_symbol("values");
  call_with_values_symbol = scheme_intern_symbol("call-with-values");

  scheme_init_marshal(env);
}

void scheme_init_compile_places()
{
}

/**********************************************************************/
/*                            utilities                               */
/**********************************************************************/

static int check_form(Scheme_Object *form, Scheme_Object *base_form)
{
  int i;

  for (i = 0; SCHEME_STX_PAIRP(form); i++) {
    form = SCHEME_STX_CDR(form);
  }

  if (!SCHEME_STX_NULLP(form)) {
    scheme_wrong_syntax(NULL, form, base_form, IMPROPER_LIST_FORM);
  }

  return i;
}

static void bad_form(Scheme_Object *form, int l)
{ 
  scheme_wrong_syntax(NULL, NULL, form, 
		      "bad syntax;\n has %d part%s after keyword", 
		      l - 1, (l != 2) ? "s" : "");
}

static Scheme_Object *simplify_inferred_name(Scheme_Object *name);

static Scheme_Object *simplify_inferred_name_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *name = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return (void *)simplify_inferred_name(name);
}


static Scheme_Object *simplify_inferred_name(Scheme_Object *name)
{
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)name;

      return scheme_handle_stack_overflow(simplify_inferred_name_k);
    }
  }

  if (SCHEME_PAIRP(name)) {
    Scheme_Object *name_car = SCHEME_CAR(name), *name_cdr = SCHEME_CDR(name);
    name_car = simplify_inferred_name(name_car);
    name_cdr = simplify_inferred_name(name_cdr);
    if (SAME_OBJ(name_car, name_cdr))
      return name_car;
  }

  return name;
}

Scheme_Comp_Env *scheme_check_name_property(Scheme_Object *code, Scheme_Comp_Env *env)
{
  Scheme_Object *name;

  name = scheme_stx_property(code, inferred_name_symbol, NULL);
  name = simplify_inferred_name(name);
  if (name && SCHEME_SYMBOLP(name))
    return name;
  else
    return current_val;
}

/**********************************************************************/
/*                           lambda utils                             */
/**********************************************************************/

static Scheme_Object *lambda_check(Scheme_Object *form)
{
  if (SCHEME_STX_PAIRP(form)
      && SCHEME_STX_PAIRP(SCHEME_STX_CDR(form))) {
    Scheme_Object *rest;
    rest = SCHEME_STX_CDR(form);
    if (SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest))) {
      int len;
      len = check_form(form, form);
      if (len != 3)
        bad_form(form, len);

      return form;
    }
  }

  scheme_wrong_syntax(NULL, NULL, form, NULL);
  return NULL;
}

static void lambda_check_args(Scheme_Object *args, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *v, *a;
  DupCheckRecord r;

  if (!SCHEME_STX_SYMBOLP(args)) {
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      a = SCHEME_STX_CAR(v);
      scheme_check_identifier(NULL, a, NULL, form);
    }

    if (!SCHEME_STX_NULLP(v)) {
      if (!SCHEME_STX_SYMBOLP(v)) {
	scheme_check_identifier(NULL, v, NULL, form);
      }
    }

    /* Check for duplicate names: */
    scheme_begin_dup_symbol_check(&r);
    for (v = args; SCHEME_STX_PAIRP(v); v = SCHEME_STX_CDR(v)) {
      Scheme_Object *name;

      name = SCHEME_STX_CAR(v);
      scheme_dup_symbol_check(&r, NULL, name, "argument", form);
    }
    if (!SCHEME_STX_NULLP(v)) {
      scheme_dup_symbol_check(&r, NULL, v, "argument", form);
    }
  }
}

Scheme_Object *scheme_source_to_name(Scheme_Object *code)
/* Makes up a procedure name when there's not a good one in the source */
{
  Scheme_Stx *cstx = (Scheme_Stx *)code;
  if ((cstx->srcloc->col >= 0) || (cstx->srcloc->pos >= 0)) {
    char buf[50], src[20];
    Scheme_Object *name, *bstr;
    int convert_backslash = 0;

    if (cstx->srcloc->src) {
      if (SCHEME_PATHP(cstx->srcloc->src)) {
        bstr = cstx->srcloc->src;
        /* for generating consistent names on machines with different platform
           conventions, convert "\" to "/" */
        convert_backslash = 1;
      } else if (SCHEME_CHAR_STRINGP(cstx->srcloc->src))
        bstr = scheme_char_string_to_byte_string(cstx->srcloc->src);
      else
        bstr = NULL;
    } else
      bstr = NULL;

    if (bstr) {
      if (SCHEME_BYTE_STRLEN_VAL(bstr) < 20)
	memcpy(src, SCHEME_BYTE_STR_VAL(bstr), SCHEME_BYTE_STRLEN_VAL(bstr) + 1);
      else {
	memcpy(src, SCHEME_BYTE_STR_VAL(bstr) + SCHEME_BYTE_STRLEN_VAL(bstr) - 19, 20);
	src[0] = '.';
	src[1] = '.';
	src[2] = '.';
      }
      if (convert_backslash) {
        int i;
        for (i = 0; src[i]; i++) {
          if (src[i] == '\\')
            src[i] = '/';
        }
      }
    } else {
      return NULL;
    }

    if (cstx->srcloc->line >= 0) {
      sprintf(buf, "%s%s%" PRIdPTR ":%" PRIdPTR,
	      src, (src[0] ? ":" : ""), cstx->srcloc->line, cstx->srcloc->col - 1);
    } else {
      sprintf(buf, "%s%s%" PRIdPTR,
	      src, (src[0] ? "::" : ""), cstx->srcloc->pos);
    }

    name = scheme_intern_exact_symbol(buf, strlen(buf));
    return name;
  }

  return NULL;
}

Scheme_Object *combine_name_with_srcloc(Scheme_Object *name, Scheme_Object *code, int src_based_name)
{
  Scheme_Stx *cstx = (Scheme_Stx *)code;

  if (((cstx->srcloc->col >= 0) || (cstx->srcloc->pos >= 0))
      && cstx->srcloc->src) {
    Scheme_Object *vec;
    vec = scheme_make_vector(7, NULL);
    SCHEME_VEC_ELS(vec)[0] = name;
    SCHEME_VEC_ELS(vec)[1] = cstx->srcloc->src;
    if (cstx->srcloc->line >= 0) {
      SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(cstx->srcloc->line);
      SCHEME_VEC_ELS(vec)[3] = scheme_make_integer(cstx->srcloc->col-1);
    } else {
      SCHEME_VEC_ELS(vec)[2] = scheme_false;
      SCHEME_VEC_ELS(vec)[3] = scheme_false;
    }
    if (cstx->srcloc->pos >= 0)
      SCHEME_VEC_ELS(vec)[4] = scheme_make_integer(cstx->srcloc->pos);
    else
      SCHEME_VEC_ELS(vec)[4] = scheme_false;
    if (cstx->srcloc->span >= 0)
      SCHEME_VEC_ELS(vec)[5] = scheme_make_integer(cstx->srcloc->span);
    else
      SCHEME_VEC_ELS(vec)[5] = scheme_false;
    SCHEME_VEC_ELS(vec)[6] = (src_based_name ? scheme_true : scheme_false);
    
    return vec;
  }

  return name;
}

Scheme_Object *scheme_build_closure_name(Scheme_Object *code, Scheme_Comp_Env *env)
{
  Scheme_Object *name;

  name = scheme_stx_property(code, inferred_name_symbol, NULL);
  name = simplify_inferred_name(name);
  if (name && SCHEME_SYMBOLP(name)) {
    name = combine_name_with_srcloc(name, code, 0);
  } else if (name && SCHEME_VOIDP(name)) {
    name = scheme_source_to_name(code);
    if (name)
      name = combine_name_with_srcloc(name, code, 1);
  } else {
    name = env->value_name;
    if (!name || SCHEME_FALSEP(name)) {
      name = scheme_source_to_name(code);
      if (name)
	name = combine_name_with_srcloc(name, code, 1);
    } else {
      name = combine_name_with_srcloc(name, code, 0);
    }
  }
  return name;
}

static Scheme_Object *make_lambda(Scheme_Comp_Env *env, Scheme_Object *code)
/* Compiles a `lambda' expression */
{
  Scheme_Object *allparams, *params, *forms, *param, *name, *scope;
  Scheme_Lambda *lam;
  intptr_t num_params;
  Scheme_IR_Local *var, **vars;
  Scheme_IR_Lambda_Info *cl;

  lam  = MALLOC_ONE_TAGGED(Scheme_Lambda);

  lam->iso.so.type = scheme_ir_lambda_type;

  params = SCHEME_STX_CDR(code);
  params = SCHEME_STX_CAR(params);
  allparams = params;

  num_params = 0;
  for (; SCHEME_STX_PAIRP(params); params = SCHEME_STX_CDR(params)) {
    num_params++;
  }
  SCHEME_LAMBDA_FLAGS(lam) = 0;
  if (!SCHEME_STX_NULLP(params)) {
    SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_HAS_REST;
    num_params++;
  }
  lam->num_params = num_params;
  if ((lam->num_params > 0) && scheme_has_method_property(code))
    SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_IS_METHOD;

  forms = SCHEME_STX_CDR(code);
  forms = SCHEME_STX_CDR(forms);

  env = scheme_check_name_property(form, env);
  name = scheme_build_closure_name(code, env);
  lam->name = name;

  env = scheme_set_comp_env_name(env, NULL);

  vars = MALLOC_N(Scheme_IR_Local*, num_params);

  params = allparams;
  for (i = 0; i < num_params; i++) {
    if (!SCHEME_STX_PAIRP(params))
      param = params;
    else
      param = SCHEME_STX_CAR(params);
    var = scheme_make_local_variable(param);
    vars[i] = var;
    env = scheme_extend_comp_env(env, param, (Scheme_Object *)var, i > 0);
    if (SCHEME_STX_PAIRP(params))
      params = SCHEME_STX_CDR (params);
  }

  if (SCHEME_STX_NULLP(forms))
    scheme_wrong_syntax(NULL, NULL, code, "empty body not allowed");

  forms = scheme_datum_to_syntax(forms, code, 0);

  {
    Scheme_Object *body;
    body = compile_expr(SChEME_STX_CAR(forms), env);
    lam->body = body;
  }

  scheme_merge_lambda_rec(rec, drec, &lrec, 0);

  cl = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
  SET_REQUIRED_TAG(cl->type = scheme_rt_ir_lambda_info);
  cl->vars = vars;
  lam->ir_info = cl;

  return (Scheme_Object *)lam;
}

static Scheme_Object *lambda_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *args;

  form = lambda_check(form);

  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);
  lambda_check_args(args, form, env);

  return make_lambda(env, form);
}

Scheme_Object *scheme_clone_vector(Scheme_Object *lam, int skip, int set_type)
{
  Scheme_Object *naya;
  int i, size;

  size = SCHEME_VEC_SIZE(lam);
  naya = scheme_make_vector(size - skip, NULL);
  for (i = skip; i < size; i++) {
    SCHEME_VEC_ELS(naya)[i - skip] = SCHEME_VEC_ELS(lam)[i];
  }

  if (set_type)
    naya->type = lam->type;

  return naya;
}

/**********************************************************************/
/*                               quote                                */
/**********************************************************************/

static Scheme_Object *quote_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *v, *rest;

  rest = SCHEME_STX_CDR(form);
  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "wrong number of parts");

  v = SCHEME_STX_CAR(rest);

  if (SCHEME_STXP(v))
    return scheme_syntax_to_datum(v, 0, NULL);
  else
    return v;
}

/**********************************************************************/
/*                                if                                  */
/**********************************************************************/

static void check_if_len(Scheme_Object *form, int len)
{
  if (len != 4) {
    if (len == 3) {
      scheme_wrong_syntax(NULL, NULL, form, 
                          "missing an \"else\" expression");
    } else {
      bad_form(form, len);
    }
  }
}

Scheme_Object *scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
                                  Scheme_Object *elsep)
{
  Scheme_Branch_Rec *b;

  if (SCHEME_TYPE(test) > _scheme_ir_values_types_) {
    if (SCHEME_FALSEP(test))
      return elsep;
    else
      return thenp;
  }

  b = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
  b->so.type = scheme_branch_type;

  b->test = test;
  b->tbranch = thenp;
  b->fbranch = elsep;

  return (Scheme_Object *)b;
}

static Scheme_Object *if_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name, *rest;

  len = check_form(form, form);
  check_if_len(form, len);

  env = scheme_check_name_property(form, env);

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  if (len == 4) {
    rest = SCHEME_STX_CDR(rest);
    elsep = SCHEME_STX_CAR(rest);
  } else
    elsep = scheme_compiled_void();

  test = scheme_compile_expr(test, scheme_set_comp_env_name(env, NULL));

  if (SCHEME_TYPE(test) > _scheme_ir_values_types_) {
    opt = 1;
    
    if (SCHEME_FALSEP(test)) {
      /* compile other branch only to get syntax checking: */
      env = scheme_comp_env_set_flags(env, COMP_ENV_DONT_COUNT_AS_USE);
      env->value_name = name;
      scheme_compile_expr(thenp, env);
  
      if (len == 4) {
        env->value_name = name;
	test = scheme_compile_expr(elsep, env);
      } else
	test = elsep;
    } else {
      if (len == 4) {
	/* compile other branch only to get syntax checking: */
        env = scheme_comp_env_set_flags(env, COMP_ENV_DONT_COUNT_AS_USE);
        env->value_name = name;
	scheme_compile_expr(elsep, env);
      }

      env->value_name = name;
      test = scheme_compile_expr(thenp, env);
    }
  } else {
    opt = 0;
    env->value_name = name;
    thenp = scheme_compile_expr(thenp, env);
    if (len == 4) {
      env->value_name = name;
      elsep = scheme_compile_expr(elsep, env);
    }
  }
  
  if (opt)
    return test;
  else
    return scheme_make_branch(test, thenp, elsep);
}

/**********************************************************************/
/*                    with-continuation-mark                          */
/**********************************************************************/

static Scheme_Object *with_cont_mark_compile(Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *key, *val, *expr;
  Scheme_Comp_Env *k_env;
  Scheme_With_Continuation_Mark *wcm;
  int len;

  len = check_form(form, form);

  if (len != 4)
    bad_form(form, len);

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  k_env = scheme_set_comp_env_name(env, NULL);

  key = scheme_compile_expr(key, k_env);
  val = scheme_compile_expr(val, k_env);
  expr = scheme_compile_expr(expr, env);

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_cont_mark_type;
  wcm->key = key;
  wcm->val = val;
  wcm->body = expr;
  
  return (Scheme_Object *)wcm;
}

/**********************************************************************/
/*                               set!                                 */
/**********************************************************************/

static Scheme_Object *set_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Set_Bang *sb;
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *val, *name, *body, *rest;
  int l, set_undef;

  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  body = SCHEME_STX_CAR(rest);
  
  scheme_check_identifier("set!", name, NULL, form);

  var = scheme_compile_lookup(name, env, SCHEME_SETTING);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_toplevel_variable_type)) {
    if (((Scheme_IR_Toplevel *)var)->import_pos != -1)
      scheme_wrong_syntax(NULL, form, name, "cannot mutate imported variable");
    SCHEME_IR_TOPLEVEL_FLAGS(((Scheme_IR_Toplevel *)var)) |= SCHEME_IR_TOPLEVEL_MUTATED;
  }
  
  env = scheme_set_comp_env_name(env, SCHEME_STX_SYM(name));

  val = scheme_compile_expr(body, env);
  
  set_undef = (env->flags & COMP_ENV_ALLOW_SET_UNDEFINED);
 
  sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);  
  sb->so.type = scheme_set_bang_type;
  sb->var = var;
  sb->val = val;
  sb->set_undef = set_undef;

  return (Scheme_Object *)sb;
}

/**********************************************************************/
/*                     #%variable-reference                           */
/**********************************************************************/

static Scheme_Object *ref_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *name, *rest, *dummy;
  int l, ok;

  l = check_form(form, form);

  /* retaining `dummy' ensures that the environment stays
     linked from the actual variable */
  if ((l == 1) || !rec[drec].testing_constantness)
    dummy = scheme_make_environment_dummy(env);
  else
    dummy = NULL;

  if (l == 1) {
    var = dummy;
  } else {
    if (l != 2)
      bad_form(form, l);

    rest = SCHEME_STX_CDR(form);
    name = SCHEME_STX_CAR(rest);
    ok = SCHEME_STX_SYMBOLP(name);

    if (!ok) {
      scheme_wrong_syntax("#%variable-reference", name, 
                          form, 
                          "not an identifier");
      return NULL;
    }

    var = scheme_compile_lookup(name, env, SCHEME_REFERENCING);
    
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)) {
      var = scheme_register_toplevel_in_prefix(var, env);
      if (!(env->flags & COMP_ENV_TESTING_CONSTANTNESS))
        SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
    } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_local_type)) {
      /* ok */
    } else {
      scheme_wrong_syntax(NULL, name, form, "identifier does not refer to a variable");
    }
  }

  {
    Scheme_Object *o;
    o = scheme_alloc_object();
    o->type = scheme_varref_form_type;
    SCHEME_PTR1_VAL(o) = (Scheme_Object *)var;
    if (!dummy) dummy = scheme_false;
    SCHEME_PTR2_VAL(o) = (Scheme_Object *)dummy;
    return o;
  }
}

/**********************************************************************/
/*                             case-lambda                            */
/**********************************************************************/

Scheme_Object *scheme_unclose_case_lambda(Scheme_Object *expr, int mode)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)expr;
  Scheme_Closure *c;
  int i;

  for (i = cl->count; i--; ) {
    c = (Scheme_Closure *)cl->array[i];
    if (!ZERO_SIZED_CLOSUREP(c)) {
      break;
    }
  }

  if (i < 0) {
    /* We can reconstruct a case-lambda syntactic form. */
    Scheme_Case_Lambda *cl2;

    cl2 = (Scheme_Case_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						     + ((cl->count - mzFLEX_DELTA) * sizeof(Scheme_Object*)));
    
    cl2->so.type = scheme_case_lambda_sequence_type;
    cl2->count = cl->count;
    cl2->name = cl->name;

    for (i = cl->count; i--; ) {
      c = (Scheme_Closure *)cl->array[i];
      cl2->array[i] = (Scheme_Object *)c->code;
    }

    if (mode == 2) {
      /* sfs */
      return (Scheme_Object *)cl2;
#ifdef MZ_USE_JIT
    } else if (mode == 1) {
      /* JIT */
      return scheme_case_lambda_jit((Scheme_Object *)cl2);
#endif
    } else
      return (Scheme_Object *)cl2;
  }
  
  return expr;
}

static void case_lambda_check_line(Scheme_Object *line, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *body, *args;

  if (!SCHEME_STX_PAIRP(line))
    scheme_wrong_syntax(NULL, line, form, NULL);
  
  body = SCHEME_STX_CDR(line);
  args = SCHEME_STX_CAR(line);
  
  lambda_check_args(args, form, env);
  
  if (!SCHEME_STX_PAIRP(body))
    scheme_wrong_syntax(NULL, line, form, "%s",
			SCHEME_STX_NULLP(body) ? "empty body not allowed" : IMPROPER_LIST_FORM);
}

static Scheme_Object *case_lambda_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;

  form = SCHEME_STX_CDR(form);

  env = scheme_check_name_property(form, orig_form);
  name = scheme_build_closure_name(orig_form, env);
  
  if (SCHEME_STX_NULLP(form)) {
    /* Case where there are no cases... */
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - (mzFLEX_DELTA * sizeof(Scheme_Object*)));

    form->type = scheme_case_lambda_sequence_type;
    ((Scheme_Case_Lambda *)form)->count = 0;
    ((Scheme_Case_Lambda *)form)->name = name;

    if (scheme_has_method_property(orig_form)) {
      /* See note in schpriv.h about the IS_METHOD hack */
      if (!name)
	name = scheme_false;
      name = scheme_box(name);
      ((Scheme_Case_Lambda *)form)->name = name;
    }

    return form;
  }

  if (!SCHEME_STX_PAIRP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(form))) {
    c = SCHEME_STX_CAR(form);

    case_lambda_check_line(c, orig_form, env);

    c = cons(scheme_datum_to_syntax(lambda_symbol, scheme_false, 0),
             c);
    c = scheme_datum_to_syntax(c, orig_form, DTS_COPY_PROPS);
    
    return lambda_compile(c, env, rec, drec);
  }

  list = last = NULL;
  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *clause;
    clause = SCHEME_STX_CAR(form);
    case_lambda_check_line(clause, orig_form, env);

    c = cons(lambda_symbol, clause);

    c = scheme_datum_to_syntax(c, clause, 0);

    c = cons(c, scheme_null);

    if (list)
      SCHEME_CDR(last) = c;
    else
      list = c;

    last = c;
    form = SCHEME_STX_CDR(form);

    count++;
  }

  if (!SCHEME_STX_NULLP(form))
    scheme_wrong_syntax(NULL, form, orig_form, NULL);

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (count - mzFLEX_DELTA) * sizeof(Scheme_Object *));
  cl->so.type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_TRUEP(name) ? name : NULL;

  env = scheme_set_comp_env_name(env, NULL);

  for (i = 0; i < count; i++) {
    Scheme_Object *ce;
    ce = SCHEME_CAR(list);
    ce = scheme_compile_expr(ce, env);
    cl->array[i] = ce;
    list = SCHEME_CDR(list);
  }

  if (scheme_has_method_property(orig_form)) {
    Scheme_Lambda *lam;
    /* Make sure no branch has 0 arguments: */
    for (i = 0; i < count; i++) {
      lam = (Scheme_Lambda *)cl->array[i];
      if (!lam->num_params)
	break;
    }
    if (i >= count) {
      lam = (Scheme_Lambda *)cl->array[0];
      SCHEME_LAMBDA_FLAGS(lam) |= LAMBDA_IS_METHOD;
    }
  }

  return (Scheme_Object *)cl;
}

/**********************************************************************/
/*                  let, let-values, letrec, etc.                     */
/**********************************************************************/

static Scheme_IR_Let_Header *make_header(Scheme_Object *first, int num_bindings, int num_clauses, 
                                         int flags)
{
  Scheme_IR_Let_Header *head;

  head = MALLOC_ONE_TAGGED(Scheme_IR_Let_Header);
  head->iso.so.type = scheme_ir_let_header_type;
  head->body = first;
  head->count = num_bindings;
  head->num_clauses = num_clauses;
  SCHEME_LET_FLAGS(head) = flags;

  return head;
}

static Scheme_Object *do_let_compile (Scheme_Object *form, Scheme_Comp_Env *origenv, char *formname,
                                      int recursive)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, *forms;
  int num_clauses, num_bindings, i, j, k, m, pre_k, mutate_frame = 0;
  Scheme_Comp_Env *frame, *rhs_env;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL;
  Scheme_IR_Let_Value *last = NULL, *lv;
  Scheme_IR_Local *var, **vars;
  DupCheckRecord r;
  Scheme_IR_Let_Header *head;

  i = check_form(form, form);
  if (len != 3)
    bad_form(form, len);

  bindings = SCHEME_STX_CDR(form);
  bindings = SCHEME_STX_CAR(bindings);
  num_clauses = scheme_stx_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(NULL, bindings, form, NULL);

  /* forms ends up being the let body */
  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = SCHEME_STX_CAR(forms);

  origenv = scheme_check_name_property(form, origenv);

  if (!num_clauses)
    return scheme_compile_expr(forms, origenv);
  
  num_bindings = 0;
  l = bindings;
  while (!SCHEME_STX_NULLP(l)) {
    Scheme_Object *clause, *names, *rest;
    int num_names;

    clause = SCHEME_STX_CAR(l);
      
    if (!SCHEME_STX_PAIRP(clause))
      rest = NULL;
    else {
      rest = SCHEME_STX_CDR(clause);
      if (!SCHEME_STX_PAIRP(rest))
        rest = NULL;
      else {
        rest = SCHEME_STX_CDR(rest);
        if (!SCHEME_STX_NULLP(rest))
          rest = NULL;
      }
    }
    if (!rest)
      scheme_wrong_syntax(NULL, clause, form, NULL);
      
    names = SCHEME_STX_CAR(clause);

    num_names = scheme_stx_proper_list_length(names);
    if (num_names < 0)
      scheme_wrong_syntax(NULL, names, form, NULL);
     
    num_bindings += num_names;
 
    l = SCHEME_STX_CDR(l);
  }

  names = MALLOC_N(Scheme_Object *, num_bindings);

  recs = MALLOC_N_ATOMIC(Scheme_Compile_Info, (num_clauses + 1));

  frame = scheme_set_comp_env_name(origenv, NULL);
  
  scheme_begin_dup_symbol_check(&r);

  k = 0;

  for (i = 0; i < num_clauses; i++) {
    if (!SCHEME_STX_PAIRP(bindings))
      scheme_wrong_syntax(NULL, bindings, form, NULL);
    binding = SCHEME_STX_CAR(bindings);
    if (!SCHEME_STX_PAIRP(binding) || !SCHEME_STX_PAIRP(SCHEME_STX_CDR(binding)))
      scheme_wrong_syntax(NULL, binding, form, NULL);

    {
      Scheme_Object *rest;
      rest = SCHEME_STX_CDR(binding);
      if (!SCHEME_STX_NULLP(SCHEME_STX_CDR(rest)))
	scheme_wrong_syntax(NULL, binding, form, NULL);
    }

    pre_k = k;

    name = SCHEME_STX_CAR(binding);
    while (!SCHEME_STX_NULLP(name)) {
      Scheme_Object *n;
      n = SCHEME_STX_CAR(name);
      names[k] = n;
      scheme_check_identifier(NULL, names[k], NULL, form);
      scheme_dup_symbol_check(&r, NULL, names[k], "binding", form);
      k++;
      name = SCHEME_STX_CDR(name);
    }
      
    vars = MALLOC_N(Scheme_IR_Local*, k-pre_k);

    lv = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
    lv->iso.so.type = scheme_ir_let_value_type;
    if (!last)
      first = (Scheme_Object *)lv;
    else
      last->body = (Scheme_Object *)lv;
    last = lv;
    lv->count = (k - pre_k);
    lv->vars = vars;

    {
      Scheme_Object *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      if (!recursive) {
        if (lv->count == 1)
          rhs_env = scheme_set_comp_env_name(origenv, names[pre_k]);
        else
          rhs_env = scheme_set_comp_env_name(origenv, NULL);
        rhs = SCHEME_STX_CDR(binding);
        rhs = SCHEME_STX_CAR(rhs);
        rhs = scheme_compile_expr(rhs, rhs_env);
      }
      lv->value = rhs;
    }

    for (m = pre_k; m < k; m++) {
      var = scheme_make_local_variable(names[m]);
      vars[m-pre_k] = var;
      frame = scheme_extend_comp_env(frame, names[m], (Scheme_Object *)var, mutate_frame);
      mutate_frame = 1;
    }
    
    bindings = SCHEME_STX_CDR(bindings);
  }

  head = make_header(first, num_bindings, num_clauses,
                     (recursive ? SCHEME_LET_RECURSIVE : 0));

  if (recursive) {
    Scheme_Object *rhs;
    k = 0;
    lv = (Scheme_IR_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_IR_Let_Value *)lv->body) {
      rhs = lv->value;
      if (lv->count == 1)
        rhs_env = scheme_set_comp_env_name(frame, names[k]);
      else
        rhs_env = scheme_set_comp_env_name(frame, NULL);
      rhs = scheme_compile_expr(rhs, rhs_env);
      lv->value = rhs;
      k += lv->count;
    }
  }

  frame = scheme_set_comp_env_name(frame, origenv->value_name);

  forms = scheme_compile_expr(forms, frame);
  last->body = forms;

  return (Scheme_Object *)head;
}

static Scheme_Object *let_values_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  return do_let_compile(form, env, "let-values", 0);
}

static Scheme_Object *letrec_values_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  return do_let_compile(form, env, "letrec-values", 1);
}

/**********************************************************************/
/*                   begin, begin0, implicit begins                   */
/**********************************************************************/

Scheme_Object *scheme_compiled_void()
{
  return scheme_void;
}

static Scheme_Object *do_begin_compile(char *name,
                                       Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
                                       int zero)
{
  Scheme_Comp_Env *nontail_env;
  Scheme_Object *forms, *body;

  forms = SCHEME_STX_CDR(form);
  
  if (SCHEME_STX_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(NULL, NULL, form, "empty form not allowed");
    return NULL;
  }

  check_form(form, form);

  env = scheme_check_name_property(form, env);
  nontail_env = scheme_set_comp_env_name(env, NULL);

  /* if the `begin` has only one expression inside, drop the `begin`;
     this is allowed even for `begin0`, where the initial expression
     is considered in tail position if it's syntactically the only
     expression */
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    forms = SCHEME_STX_CAR(forms);
    return scheme_compile_expr(forms, env);
  }

  if (zero) {
    Scheme_Object *first, *rest, *vname;

    first = SCHEME_STX_CAR(forms);
    first = scheme_compile_expr(first, env);
    rest = SCHEME_STX_CDR(forms);
    rest = compile_list(rest, nontail_env, nontail_env, 0);

    body = cons(first, rest);
  } else {
    body = compile_list(forms, env, nontail_env, env, 0);
  }

  forms = scheme_make_sequence_compilation(body, zero ? -1 : 1, 0);

  return forms;
}

static Scheme_Object *begin_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  return do_begin_compile("begin", form, env, 0);
}

static Scheme_Object *begin0_compile (Scheme_Object *form, Scheme_Comp_Env *env)
{
  return do_begin_compile("begin0", form, env, 1);
}

Scheme_Sequence *scheme_malloc_sequence(int count)
{
  return (Scheme_Sequence *)scheme_malloc_tagged(sizeof(Scheme_Sequence)
						 + (count - mzFLEX_DELTA) 
						 * sizeof(Scheme_Object *));
}

Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *seq, int opt, int resolved)
{
  /* We have to be defensive in processing `seq'; it might be bad due
     to a bad .zo */
  Scheme_Object *list, *v, *good;
  Scheme_Sequence *o;
  int count, i, k, total, last, first, setgood;
  Scheme_Type type;

  type = scheme_sequence_type;

  list = seq;
  count = i = 0;
  good = NULL;
  total = 0;
  first = 1;
  setgood = 1;
  while (SCHEME_PAIRP(list)) {
    v = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
    last = SCHEME_NULLP(list);

    if (((opt > 0) || !first) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      /* "Inline" nested begins */
      count += ((Scheme_Sequence *)v)->count;
      total++;
    } else if (opt
               && (((opt > 0) && !last) || ((opt < 0) && !first))
               && scheme_omittable_expr(v, -1, -1,
                                        (resolved ? OMITTABLE_RESOLVED : OMITTABLE_KEEP_VARS),
                                        NULL, NULL)) {
      /* A value that is not the result. We'll drop it. */
      total++;
    } else {
      if (setgood)
	good = v;
      count++;
      total++;
    }
    i++;
    if (first) {
      if (opt < 0)
	setgood = 0;
      first = 0;
    }
  }

  if (!SCHEME_NULLP(list))
    return NULL; /* bad .zo */

  if (!count)
    return scheme_compiled_void();
  
  if (count == 1) {
    if (opt < -1) {
      /* can't optimize away a begin0 reading a .zo time */
    } else if ((opt < 0)
               && !scheme_omittable_expr(SCHEME_CAR(seq), 1, -1,
                                         (resolved ? OMITTABLE_RESOLVED : OMITTABLE_KEEP_VARS),
                                         NULL, NULL)) {
      /* We can't optimize (begin0 expr cont) to expr because
	 exp is not in tail position in the original (so we'd mess
	 up continuation marks). */
    } else
      return good;
  }

  o = scheme_malloc_sequence(count);

  o->so.type = ((opt < 0) ? scheme_begin0_sequence_type : scheme_sequence_type);
  o->count = count;
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array; /* <-- mismaligned for precise GC */
      for (j = 0; j < c; j++) {
	o->array[i++] = a[j];
      }
    } else if (opt 
	       && (((opt > 0) && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v, -1, -1,
                                        (resolved ? OMITTABLE_RESOLVED : OMITTABLE_KEEP_VARS),
                                        NULL, NULL)) {
      /* Value not the result. Do nothing. */
    } else
      o->array[i++] = v;
  }

  return (Scheme_Object *)o;
}

/**********************************************************************/
/*                          environment access                        */
/**********************************************************************/

Scheme_Object *scheme_make_environment_dummy(Scheme_Comp_Env *env)
{ 
  /* Get a prefixed-based accessor for a dummy top-level bucket. It's
     used to "link" to the right instance at run time. */
  return scheme_register_toplevel_in_prefix(NULL, env, NULL);
}

Scheme_Env *scheme_environment_from_dummy(Scheme_Object *dummy)
{
  Scheme_Prefix *toplevels;
  Scheme_Bucket *b;

  toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(dummy)];
  b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(dummy)];
  return scheme_get_bucket_home(b);
}

/*========================================================================*/
/*                            applications                                */
/*========================================================================*/

int scheme_get_eval_type(Scheme_Object *obj)
     /* Categories for short-cutting recursive calls to the evaluator */
{
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  if (type > _scheme_values_types_)
    return SCHEME_EVAL_CONSTANT;
  else if (SAME_TYPE(type, scheme_ir_local_type)
           || SAME_TYPE(type, scheme_local_type))
    return SCHEME_EVAL_LOCAL;
  else if (SAME_TYPE(type, scheme_local_unbox_type))
    return SCHEME_EVAL_LOCAL_UNBOX;
  else if (SAME_TYPE(type, scheme_toplevel_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

Scheme_Object *scheme_try_apply(Scheme_Object *f, Scheme_Object *args, Optimize_Info *info)
     /* Apply `f' to `args' and ignore failues --- used for constant
        folding attempts */
{
  Scheme_Object * volatile result;
  Scheme_Object * volatile exn = NULL;
  mz_jmp_buf *savebuf, newbuf;

  scheme_current_thread->reading_delayed = NULL;
  scheme_current_thread->constant_folding = (info ? info : (Optimize_Info *)scheme_false);
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    result = NULL;
    exn = scheme_current_thread->reading_delayed;
  } else
    result = _scheme_apply_to_list(f, args);
  
  scheme_current_thread->error_buf = savebuf;
  scheme_current_thread->constant_folding = NULL;
  scheme_current_thread->reading_delayed = NULL;

  if (scheme_current_thread->cjs.is_kill) {
    scheme_longjmp(*scheme_current_thread->error_buf, 1);
  }

  if (exn)
    scheme_raise(exn);

  return result;
}

static int foldable_body(Scheme_Object *f)
{
  Scheme_Lambda *d;
  
  d = SCHEME_CLOSURE_CODE(f);

  scheme_delay_load_closure(d);

  return (SCHEME_TYPE(d->body) > _scheme_values_types_);
}

int scheme_is_foldable_prim(Scheme_Object *f)
{
  if (SCHEME_PRIMP(f)
      && ((((Scheme_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
          == SCHEME_PRIM_OPT_FOLDING))
    return 1;

  if (SCHEME_CLSD_PRIMP(f)
      && ((((Scheme_Closed_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
          == SCHEME_PRIM_OPT_FOLDING))
    return 1;

  return 0;
}

Scheme_Object *scheme_make_application(Scheme_Object *v, Optimize_Info *info)
{
  Scheme_Object *o;
  int i, nv;
  volatile int n;

  o = v;
  n = 0;
  nv = 0;
  while (!SCHEME_NULLP(o)) {
    Scheme_Type type;
    
    n++;
    type = SCHEME_TYPE(SCHEME_CAR(o));
    if (type < _scheme_ir_values_types_)
      nv = 1;
    o = SCHEME_CDR(o);
  }

  if (!nv) {
    /* They're all values. Applying folding prim or closure? */
    Scheme_Object *f;

    f = SCHEME_CAR(v);

    if (scheme_is_foldable_prim(f)
	|| (SAME_TYPE(SCHEME_TYPE(f), scheme_closure_type)
	    && (foldable_body(f)))) {
      f = scheme_try_apply(f, SCHEME_CDR(v), info);
      
      if (f)
	return f;
    }
  }

  if (n == 2) {
    Scheme_App2_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app->iso.so.type = scheme_application2_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand = SCHEME_CAR(v);
    
    return (Scheme_Object *)app;
  } else if (n == 3) {
    Scheme_App3_Rec *app;

    app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
    app->iso.so.type = scheme_application3_type;

    app->rator = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand1 = SCHEME_CAR(v);
    v = SCHEME_CDR(v);
    app->rand2 = SCHEME_CAR(v);

    return (Scheme_Object *)app;
  } else {
    Scheme_App_Rec *app;

    app = scheme_malloc_application(n);
    
    for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
      app->args[i] = SCHEME_CAR(v);
    }

    return (Scheme_Object *)app;
  }
}

Scheme_App_Rec *scheme_malloc_application(int n)
{
  Scheme_App_Rec *app;
  intptr_t size;

  if (n < 0) {
    scheme_signal_error("bad application count");
    app = NULL;
  } else if (n > 4096) {
    size = scheme_check_overflow(n, 
                                 sizeof(char),
                                 (sizeof(Scheme_App_Rec) 
                                  + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
    app = (Scheme_App_Rec *)scheme_malloc_fail_ok(scheme_malloc_tagged, size);
    if (!app) scheme_signal_error("out of memory allocating application bytecode");
  } else {
    size = (sizeof(Scheme_App_Rec) 
            + ((n - mzFLEX_DELTA) * sizeof(Scheme_Object *))
            + n * sizeof(char));
    app = (Scheme_App_Rec *)scheme_malloc_tagged(size);
  }

  app->iso.so.type = scheme_application_type;

  app->num_args = n - 1;

  return app;
}

void scheme_finish_application(Scheme_App_Rec *app)
{
  int i, devals, n;

  n = app->num_args + 1;

  devals = sizeof(Scheme_App_Rec) + ((app->num_args + 1 - mzFLEX_DELTA) * sizeof(Scheme_Object *));

  for (i = 0; i < n; i++) {
    char etype;
    etype = scheme_get_eval_type(app->args[i]);
    ((char *)app XFORM_OK_PLUS devals)[i] = etype;
  }
}

/*========================================================================*/
/*                              application                               */
/*========================================================================*/

static Scheme_Object *
compile_list(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Comp_Env *last_env,
             int start_app_position)
{
  int len;

  len = scheme_stx_proper_list_length(form);

  if (!len) {
    return scheme_null;
  } else if (len > 0) {
    int i;
    Scheme_Object *c, *p, *comp_first, *comp_last, *name, *first, *rest;

    name = env->value_name;

    comp_first = comp_last = NULL;

    for (i = 0, rest = form; i < len; i++) {
      first = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);

      c = compile_expand_expr(first,
                              ((i == (len-1)) ? last_env : env),
                              !i && start_app_position);

      p = scheme_make_pair(c, scheme_null);
      if (comp_last)
	SCHEME_CDR(comp_last) = p;
      else
	comp_first = p;
      comp_last = p;

      if (!i && start_app_position && (len == 2)
          && SAME_OBJ(c, scheme_varref_const_p_proc)) {
        last_env = scheme_set_comp_env_flags(last_env, COMP_ENV_TESTING_CONSTANTNESS);
      }
    }

    return comp_first;
  } else {
    scheme_signal_error("internal error: compile-list on non-list");
    return NULL;
  }
}

static Scheme_Object *compile_plain_app(Scheme_Object *form, Scheme_Comp_Env *env,
                                        Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *result, *rator;
  int len;

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, NULL);

  env = scheme_set_comp_env_name(env, NULL);

  form = compile_list(form, env, env, 1);

  result = scheme_make_application(form, NULL);

  /* Record which application this is for a variable that is used only in
     application positions. */
  if (SAME_TYPE(SCHEME_TYPE(result), scheme_application_type))
    rator = ((Scheme_App_Rec *)result)->args[0];
  else if (SAME_TYPE(SCHEME_TYPE(result), scheme_application2_type))
    rator = ((Scheme_App2_Rec *)result)->rator;
  else if (SAME_TYPE(SCHEME_TYPE(result), scheme_application3_type))
    rator = ((Scheme_App3_Rec *)result)->rator;
  else
    rator = NULL;
  if (rator) {
    rator = scheme_optimize_extract_tail_inside(rator);
    if (SAME_TYPE(SCHEME_TYPE(rator), scheme_ir_local_type)) {
      if (SCHEME_VAR(rator)->use_count < SCHEME_USE_COUNT_INF) {
        if (SAME_TYPE(SCHEME_TYPE(result), scheme_application_type))
          SCHEME_APPN_FLAGS((Scheme_App_Rec *)result) |= SCHEME_VAR(rator)->use_count;
        else if (SAME_TYPE(SCHEME_TYPE(result), scheme_application2_type))
          SCHEME_APPN_FLAGS((Scheme_App2_Rec *)result) |= SCHEME_VAR(rator)->use_count;
        else if (SAME_TYPE(SCHEME_TYPE(result), scheme_application3_type))
          SCHEME_APPN_FLAGS((Scheme_App3_Rec *)result) |= SCHEME_VAR(rator)->use_count;
      }
    }
  }

  return result;
}

static int arg_count(Scheme_Object *lam)
{
  Scheme_Object *l, *id, *form = lam;
  int cnt = 0;
  DupCheckRecord r;

  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  l = SCHEME_STX_CAR(lam);

  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  while (SCHEME_STX_PAIRP(lam)) { lam = SCHEME_STX_CDR(lam); }
  if (!SCHEME_STX_NULLP(lam)) return -1;

  scheme_begin_dup_symbol_check(&r);

  while (SCHEME_STX_PAIRP(l)) {
    id = SCHEME_STX_CAR(l);
    scheme_check_identifier("lambda", id, form);
    scheme_dup_symbol_check(&r, NULL, id, "argument", form);
    l = SCHEME_STX_CDR(l);
    cnt++;
  }
  if (!SCHEME_STX_NULLP(l)) return -1;

  return cnt;
}

static Scheme_Object *compile_app(Scheme_Object *orig_form, Scheme_Comp_Env *env)
{
  Scheme_Object *form, *naya, *forms, *orig_vname = env->value_name;

  forms = orig_form;
  form = forms;
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    return scheme_null;
  } else if (!SCHEME_STX_PAIRP(form)) {
     /* will end in error */
    return compile_plain_app(form, env, rec, drec);
  } else {
    Scheme_Object *name, *origname, *gval, *orig_rest_form, *rest_form;
    name = SCHEME_STX_CAR(form);
    origname = name;

    /* look for ((lambda (x ...) ....) ....) or ((lambda x ....) ....) */
    if (SAME_OBJ(SCHEME_STX_VAL(name), lambda_symbol)) {
      Scheme_Object *argsnbody;

      argsnbody = SCHEME_STX_CDR(name);
      if (SCHEME_STX_PAIRP(argsnbody)) {
        Scheme_Object *args, *body;

        args = SCHEME_STX_CAR(argsnbody);
        body = SCHEME_STX_CDR(argsnbody);
	  
        if (SCHEME_STX_PAIRP(body)) {
          int pl;
          pl = scheme_stx_proper_list_length(args);
          if ((pl >= 0) || SCHEME_STX_SYMBOLP(args)) {
            Scheme_Object *bindings = scheme_null, *last = NULL;
            Scheme_Object *rest;
            int al;
            
            rest = SCHEME_STX_CDR(form);
            al = scheme_stx_proper_list_length(rest);

            if ((pl < 0) || (al == pl)) {
              DupCheckRecord r;

              scheme_begin_dup_symbol_check(&r);
	      
              while (!SCHEME_STX_NULLP(args)) {
                Scheme_Object *v, *n;
		  
                if (pl < 0)
                  n = args;
                else
                  n = SCHEME_STX_CAR(args);
                scheme_check_identifier("lambda", n, NULL, name);

                /* If we don't check here, the error is in terms of `let': */
                scheme_dup_symbol_check(&r, NULL, n, "argument", name);
  
                if (pl < 0) {
                  v = scheme_intern_symbol("list");
                  v = scheme_datum_to_syntax(v, scheme_false, 0);
                  v = cons(v, rest);
                } else
                  v = SCHEME_STX_CAR(rest);
                v = cons(cons(cons(n, scheme_null), cons(v, scheme_null)), scheme_null);
                if (last)
                  SCHEME_CDR(last) = v;
                else
                  bindings = v;
		  
                last = v;
                if (pl < 0) {
                  /* rator is (lambda rest-x ....) */
                  break;
                } else {
                  args = SCHEME_STX_CDR(args);
                  rest = SCHEME_STX_CDR(rest);
                }
              }
              
              body = scheme_datum_to_syntax(cons(let_values_symbol,
                                                 cons(bindings, body)),
                                            form,
                                            DTS_COPY_PROPS);

              env->value_name = orig_vname;

              return compile_expand_expr(body, env, rec, drec, 0);
            }
          }
        }
      }
    }

    orig_rest_form = SCHEME_STX_CDR(form);

    /* Look for (call-with-values (lambda () M) (lambda (id ...) N)) */
    if (SCHEME_STX_SYMBOLP(name)) {
      Scheme_Object *at_first, *at_second, *the_end;
      at_first = SCHEME_STX_CDR(form);
      if (SCHEME_STX_PAIRP(at_first)) {
        at_second = SCHEME_STX_CDR(at_first);
        if (SCHEME_STX_PAIRP(at_second)) {
          the_end = SCHEME_STX_CDR(at_second);
          if (SCHEME_STX_NULLP(the_end)) {
            Scheme_Object *orig_at_second = at_second;

            if (SAME_OBJ(SCHEME_STX_VAL(name), call_with_values_symbol, 0)) {
              Scheme_Object *first, *orig_first;
              orig_first = SCHEME_STX_CAR(at_first);
              first = scheme_check_immediate_macro(orig_first, env, rec, drec, &gval, 0);
              if (SAME_OBJ(gval, scheme_lambda_syntax) 
                  && SCHEME_STX_PAIRP(first)
                  && (arg_count(first, env) == 0)) {
                Scheme_Object *second, *orig_second;
                orig_second = SCHEME_STX_CAR(at_second);
                second = scheme_check_immediate_macro(orig_second, env, rec, drec, &gval, 0);
                if (SAME_OBJ(gval, scheme_lambda_syntax) 
                    && SCHEME_STX_PAIRP(second)
                    && (arg_count(second, env) >= 0)) {
                  Scheme_Object *lhs, *orig_post_first, *orig_post_second;
                  orig_post_first = first;
                  orig_post_second = second;
                  second = SCHEME_STX_CDR(second);
                  lhs = SCHEME_STX_CAR(second);
                  second = SCHEME_STX_CDR(second);
                  first = SCHEME_STX_CDR(first);
                  first = SCHEME_STX_CDR(first);
                  first = icons(begin_symbol, first);
                  first = scheme_datum_to_syntax(first, orig_post_first, DTS_COPY_PROPS);
                  second = icons(begin_symbol, second);
                  second = scheme_datum_to_syntax(second, orig_post_second, DTS_COPY_PROPS);
                  /* Convert to let-values: */
                  name = icons(let_values_symbol,
                               icons(icons(icons(lhs, icons(first, scheme_null)), 
                                           scheme_null),
                                     icons(second, scheme_null)));
                  form = scheme_datum_to_syntax(name, forms, DTS_COPY_PROPS);
                  env->value_name = orig_vname;
                  return compile_expand_expr(form, env, rec, drec, 0);
                }
                if (!SAME_OBJ(second, orig_second)) {
                  at_second = scheme_datum_to_syntax(icons(second, the_end), at_second, DTS_COPY_PROPS);
                } 
              }
              if (!SAME_OBJ(first, orig_first)
                  || !SAME_OBJ(at_second, orig_at_second)) {
                at_first = scheme_datum_to_syntax(icons(first, at_second), at_first, DTS_COPY_PROPS);
              }
            }
          }
        }
      }
      rest_form = at_first;
    } else {
      rest_form = orig_rest_form;
    }

    if (NOT_SAME_OBJ(name, origname)
        || NOT_SAME_OBJ(rest_form, orig_rest_form)) {
      form = scheme_datum_to_syntax(scheme_make_pair(name, rest_form), forms, DTS_COPY_PROPS);
    }

    return compile_plain_app(form, env);
  }
}

/*========================================================================*/
/*                   expression compilation dispatcher                    */
/*========================================================================*/

static Scheme_Object *compile_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return compile_expr(form, env, p->ku.k.i1);
}

Scheme_Object *compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, int app_position)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.i1 = app_position;

      var = scheme_handle_stack_overflow(compile_expand_expr_k);

      return var;
    }
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_thread, ;);

  MZ_ASSERT(SCHEME_STXP(form));

  if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      var = scheme_compile_lookup(find_name, env, (app_position ? SCHEME_APP_POS : 0));

      if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type))
        var = scheme_register_toplevel_in_prefix(var, env);
    }
  } else {
    name = SCHEME_STX_CAR(form);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* check for primitive expression forms */
      name = SCHEME_STX_VAL(name);
      if (SAME_OBJ(name, quote_symbol))
        return quote_compile(form, env);
      else if (SAME_OBJ(name, let_values_symbol))
        return let_values_compile(form, env);
      else if (SAME_OBJ(name, letrec_values_symbol))
        return letrec_values_compile(form, env);
      else if (SAME_OBJ(name, lambda_symbol))
        return lambda_compile(form, env);
      else if (SAME_OBJ(name, case_lambda_symbol))
        return case_lambda_compile(form, env);
      else if (SAME_OBJ(name, if_symbol))
        return if_compile(form, env);
      else if (SAME_OBJ(name, begin_symbol))
        return begin_compile(form, env);
      else if (SAME_OBJ(name, begin0_symbol))
        return begin0_compile(form, env);
      else if (SAME_OBJ(name, with_cont_mark_symbol))
        return with_cont_mark_compile(form, env);
      else if (SAME_OBJ(name, ref_symbol))
        return ref_compile(form, env);
      else if (SAME_OBJ(name, ref_symbol))
        return ref_compile(form, env);
    }
  }

  return compile_app(form, env);
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return compile_expr(form, env, rec, drec, 0);
}

/*========================================================================*/
/*                           linklet compilation                          */
/*========================================================================*/

static int is_define_values(Scheme_Object *form)
{
  if (!SCHEME_STX_PAIRP(form))
    return 0;

  rest = SCHEME_STX_CAR(form);
  if (!SAME_OBJ(SCHEME_STX_VAL(rest), define_values_symbol))
    return 0;

  return 1;
}

static Scheme_Object *define_parse(Scheme_Object *form, 
                                   Scheme_Object **_vars, Scheme_Object **_val,
                                   Scheme_Comp_Env **_env,
                                   DupCheckRecord *r,
                                   int *_extra_vars_pos,
                                   int pos_after_mports)
{
  Scheme_Object *vars, *rest, *name, *v, *extra_vars = scheme_null;
  Scheme_Env *env;
  int len;

  len = check_form(form, form);
  if (len != 3)
    bad_form(form, len);
  
  rest = SCHEME_STX_CDR(form);
  vars = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  *_val = SCHEME_STX_CAR(rest);

  *_vars = vars;
   
  while (SCHEME_STX_PAIRP(vars)) {
    name = SCHEME_STX_CAR(vars);
    scheme_check_identifier(NULL, name, NULL, form);

    vars = SCHEME_STX_CDR(vars);

    scheme_dup_symbol_check(r, NULL, name, "binding", form);

    v = scheme_compile_lookup(name, *_env, SCHEME_NULL_FOR_UNBOUND);
    if (v && (!SAME_TYPE(SCHEME_TYPE(v), scheme_ir_toplevel_type)
              || ((Scheme_IR_Toplevel *)v)->instance_pos != -1))
      scheme_wrong_syntax(NULL, name, form, "not a definable variable");

    if (!v) {
      pos = *_extra_vars_pos + pos_after_imports;
      env = scheme_extend_comp_env(*_env, name,
                                   (Scheme_Object *)make_ir_toplevel_variable(name, i, j, pos),
                                   1);
      *_env = env;
      extra_vars = scheme_make_pair(name, extra_vars);
      *_extra_vars_pos++;
    }
  }  

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(NULL, vars, form, "bad variable list");

  return extra_vars;
}

Scheme_Object *scheme_compile_linklet(Scheme_Object *form, int set_undef)
{
  Scheme_Linklet *linklet;
  Scheme_Object *orig_form = form, *imports, *exports, *a, *e, *extra_vars, *vec, *v;
  Scheme_Object *import_syms, *import_symss, *bodies;
  Scheme_IR_Toplevel **toplevels, **new_toplevels, *tl;
  int body_len, len, islen, elen, i, j, extra_vars_pos, pos = 0, num_toplevels;
  Scheme_Env *env;
  DupCheckRecord r;
  
  body_len = check_form(form, form);
  if (body_len < 3)
    bad_form(form, body_len);

  env = scheme_new_comp_env(0);

  if (set_undef)
    env->flags |= COMP_ENV_ALLOW_SET_UNDEFINED;

  form = SCHEME_STX_CDR(form);
  imports = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  exports = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  body_len -= 3;

  num_toplevels = 16;
  toplevels = MALLOC_N(Scheme_IR_Toplevel*, num_toplevels);

  /* Parse imports, filling in `ilens` and `import_syms`, and also
     extending `env`. */
  islen = scheme_stx_proper_list_length(imports);
  if (islen < 0)
    scheme_wrong_syntax(NULL, imports, orig_form, IMPROPER_LIST_FORM);
  
  import_symss = scheme_make_vector(islen, scheme_false);

  for (i = 0; i < islen; i++, imports = SCHEME_STX_CDR(imports)) {
    a = SCHEME_STX_CAR(imports);
    len = scheme_stx_proper_list_length(a);
    
    import_syms = scheme_make_vector(len, NULL);
    SCHEME_VEC_ELSE(import_symss)[i] = import_syms;

    for (j = 0; j < len; j++, a = SCHEME_STX_CDR(a)) {
      e = SCHEME_STX_CAR(a);
      check_import_export_clause(e, orig_form);
      if (SCHEME_STX_SYMP(e)) {
        SChEME_VEC_ELS(import_syms)[j] = SCHEME_STX_VAL(e);
      } else {
        SChEME_VEC_ELS(import_syms)[j] = SCHEME_STX_VAL(SCHEME_STX_CAR(e));
        e = SCHEME_STX_CADR(e);
      }
      if (pos >= num_toplevels) {
        new_toplevels = MALLOC_N(Scheme_IR_Toplevel*, 2 * num_toplevels);
        memcpy(new_toplevels, toplevels, sizeof(Scheme_IR_Toplevel*)* num_toplevels);
        num_toplevels *= 2;
        toplevels = new_toplevels;
      }
      tl = make_ir_toplevel_variable(e, i, j, pos);
      toplevels[pos] = tl;
      env = scheme_extend_comp_env(env, e, (Scheme_Object *)tl, 1);
      pos++;
    }
  }

  pos_after_imports = pos;

  /* Parse exports, filling in `export_syms` and `defn_syms` and extending `env`. */
  len = scheme_stx_proper_list_length(exports);
  if (len < 0)
    scheme_wrong_syntax(NULL, exports, orig_form, IMPROPER_LIST_FORM);

  scheme_begin_dup_symbol_check(&r);

  export_syms = scheme_make_vector(len, NULL);
  defn_syms = scheme_make_vector(len, NULL);

  for (j = 0; j < len; j++, exports = SCHEME_STX_CDR(exports)) {
    e = SCHEME_STX_CAR(exports);
    check_import_export_clause(e, orig_form);
    if (SCHEME_STX_SYMP(e)) {
      SCHEME_VEC_ELS(export_syms)[j] = SCHEME_STX_VAL(e);
    } else {
      SCHEME_VEC_ELS(export_syms)[j] = SCHEME_STX_VAL(SCHEME_STX_CADR(e));
      e = SCHEME_STX_CAR(e);
    }
    SCHEM_VEC_ELS(defn_syms)[j] = SCHEME_STX_VAL(e);
    if (pos >= num_toplevels) {
      new_toplevels = MALLOC_N(Scheme_IR_Toplevel*, 2 * num_toplevels);
      memcpy(new_toplevels, toplevels, sizeof(Scheme_IR_Toplevel*)* num_toplevels);
      num_toplevels *= 2;
      toplevels = new_toplevels;
    }
    tl = make_ir_toplevel_variable(e, -1, j, pos);
    toplevels[pos] = tl;
    env = scheme_extend_comp_env(env, e, tl, 1);
    pos++;
  }

  /* Looks for `define-values` forms to detect variables that are defined but
     not exported */
  extra_vars_pos = len;
  all_extra_vars = scheme_null;
  
  for (i = 0; i < body_len; i++, form = SCHEME_STX_CDR(form)) {
    e = SCHEME_STX_CAR(form);
    if (is_define_values(e)) {
      extra_vars = define_parse(e, &vars, &vals, &env, &r, &extra_vars_pos, pos_after_imports);
      if (extra_vars) {
        all_extra_vars = scheme_append(extra_vars, all_extra_vars);
      }
    }
  }

  all_extra_vars = scheme_reverse(all_extra_vars);
  for (i = len; i < extra_vars_pos; i++, all_extra_vars = SCHEME_CDR(all_extra_vars)) {
    defn_syms[i] = SCHEME_CAR(all_extra_vars);
  }

  /* Prepare linklet record */

  linklet = MALLOC_ONE_TAGGED(Scheme_Linklet);
  linklet->so.type = scheme_linklet_type;

  linklet->importss = import_symss;
  linklet->exports = export_syms;
  linklet->defns = defn_syms;

  /* Compile body forms */
  bodies = scheme_make_vector(body_len, scheme_false);

  linklet->bodies = bodies;

  linklet->num_toplevels = pos;
  linklet->toplevels = toplevels;

  for (i = 0; i < body_len; i++, form = SCHEME_STX_CDR(form)) {
    e = SCHEME_STX_CAR(form);
    if (is_define_values(e)) {
      a = SCHEME_STX_CADR(e);
      len = scheme_stx_proper_list_length(a);
      vec = scheme_make_vector(len+1, SCHEME_NULL);

      for (j = 0; j < len; j++, a = SCHEME_STX_CDR(a)) {
        v = scheme_compile_lookup(SCHEME_STX_CAR(a), env, 0);
        MZ_ASSERT(SAME_TYPE(SCHEME_TYPE(v), scheme_import_export_variable_type));
        MZ_ASSERT(((Scheme_IR_Toplevel *)v)->instance_pos == -1);
        SCHEME_DEFN_VAR_(vec, j) = v;
      }
      
      a = scheme_compile_expr(SCHEME_STX_CADR(SCHEME_STX_CDR(e)), env);
      SCHEME_DEFN_RHS(vec) = a;
 
      e = vec;
      e->so.type = scheme_define_values_type;
    } else {
      e = scheme_compile_expr(e, env);
    }
    
    SCHEME_VEC_ELS(bodies)[i] = e;
  }

  return linklet;
}

/**********************************************************************/
/*                            precise GC                              */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_compile.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_ir_lambda_info, mark_ir_lambda_info);
}

END_XFORM_SKIP;

#endif
