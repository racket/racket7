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
#include "schexpobs.h"

/* symbols */
ROSYM static Scheme_Object *lambda_symbol;
ROSYM static Scheme_Object *letrec_values_symbol;
ROSYM static Scheme_Object *let_values_symbol;
ROSYM static Scheme_Object *begin_symbol;
ROSYM static Scheme_Object *compiler_inline_hint_symbol;
ROSYM static Scheme_Object *protected_symbol;
ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *values_symbol;
ROSYM static Scheme_Object *call_with_values_symbol;
ROSYM static Scheme_Object *inferred_name_symbol;
ROSYM static Scheme_Object *local_keyword;
ROSYM static Scheme_Object *existing_variables_symbol;

/* locals */
static Scheme_Object *lambda_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *define_values_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *ref_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *quote_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *if_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *set_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *case_lambda_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *let_values_expand(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Expand_Info *erec, int drec);
static Scheme_Object *letrec_values_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *begin0_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *with_cont_mark_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);
static Scheme_Object *app_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);

static Scheme_Object *compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
                                   Scheme_Compile_Expand_Info *rec, int drec, 
                                   int app_position);
static Scheme_Object *compile_sequence(Scheme_Object *forms, Scheme_Comp_Env *env,
                                       Scheme_Compile_Info *rec, int drec,
                                       int as_intdef);
static Scheme_Object *compile_list(Scheme_Object *form, Scheme_Comp_Env *env,
                                   Scheme_Compile_Info *rec, int drec);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define cons(a,b) scheme_make_pair(a,b)
#define icons(a,b) scheme_make_pair(a,b)

/**********************************************************************/
/*                          initialization                            */
/**********************************************************************/

void scheme_init_compile (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(lambda_symbol);
  REGISTER_SO(letrec_values_symbol);
  REGISTER_SO(let_values_symbol);
  REGISTER_SO(begin_symbol);
  REGISTER_SO(compiler_inline_hint_symbol);

  REGISTER_SO(inferred_name_symbol);

  REGISTER_SO(local_keyword);

  REGISTER_SO(existing_variables_symbol);

  scheme_undefined->type = scheme_undefined_type;
  
  lambda_symbol = scheme_intern_symbol("lambda");

  letrec_values_symbol = scheme_intern_symbol("letrec-values");
  let_values_symbol = scheme_intern_symbol("let-values");

  begin_symbol = scheme_intern_symbol("begin");

  disappeared_binding_symbol = scheme_intern_symbol("disappeared-binding");
  compiler_inline_hint_symbol = scheme_intern_symbol("compiler-hint:cross-module-inline");

  inferred_name_symbol = scheme_intern_symbol("inferred-name");

  local_keyword = scheme_intern_exact_keyword("local", 5);

  existing_variables_symbol = scheme_make_symbol("existing-variables");

  REGISTER_SO(protected_symbol);
  REGISTER_SO(quote_symbol);
  REGISTER_SO(values_symbol);
  REGISTER_SO(call_with_values_symbol);

  protected_symbol = scheme_intern_symbol("protected");
  quote_symbol  = scheme_intern_symbol("quote");
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

Scheme_Object *scheme_check_name_property(Scheme_Object *code, Scheme_Object *current_val)
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
    if (SCHEME_STX_PAIRP(SCHEME_STX_CDR(rest)))
      return form;
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
      scheme_check_identifier(NULL, a, NULL, env, form);
    }

    if (!SCHEME_STX_NULLP(v)) {
      if (!SCHEME_STX_SYMBOLP(v)) {
	scheme_check_identifier(NULL, v, NULL, env, form);
      }
    }

    /* Check for duplicate names: */
    scheme_begin_dup_symbol_check(&r, env);
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

static Scheme_Object *
make_lambda(Scheme_Comp_Env *env, Scheme_Object *code,
            Scheme_Compile_Info *rec, int drec)
/* Compiles a `lambda' expression */
{
  Scheme_Object *allparams, *params, *forms, *param, *name, *scope;
  Scheme_Lambda *lam;
  Scheme_Compile_Info lrec;
  Scheme_Comp_Env *frame;
  int i;
  intptr_t num_params;
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

  scope = scheme_new_scope(SCHEME_STX_LOCAL_BIND_SCOPE);

  frame = scheme_new_compilation_frame(lam->num_params, SCHEME_LAMBDA_FRAME, scope, env);
  params = allparams;
  for (i = 0; i < lam->num_params; i++) {
    if (!SCHEME_STX_PAIRP(params))
      param = params;
    else
      param = SCHEME_STX_CAR(params);
    scheme_add_compilation_binding(i, param, frame);
    if (SCHEME_STX_PAIRP(params))
      params = SCHEME_STX_CDR (params);
  }

  scheme_env_make_variables(frame);

  if (SCHEME_STX_NULLP(forms))
    scheme_wrong_syntax(NULL, NULL, code, "empty body not allowed");

  forms = scheme_datum_to_syntax(forms, code, code, 0, 0);
  forms = scheme_stx_add_scope(forms, scope, scheme_env_phase(env->genv));

  name = scheme_build_closure_name(code, env);
  lam->name = name;

  scheme_compile_rec_done_local(rec, drec);

  scheme_init_lambda_rec(rec, drec, &lrec, 0);

  {
    Scheme_Object *body;
    body = compile_sequence(forms,
                            scheme_no_defines(frame),
                            &lrec, 0,
                            1);
    lam->body = body;
  }

  scheme_merge_lambda_rec(rec, drec, &lrec, 0);

  cl = MALLOC_ONE_RT(Scheme_IR_Lambda_Info);
  SET_REQUIRED_TAG(cl->type = scheme_rt_ir_lambda_info);
  cl->vars = frame->vars;
  lam->ir_info = cl;

  return (Scheme_Object *)lam;
}

static Scheme_Object *
lambda_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *args;

  form = lambda_check(form);

  args = SCHEME_STX_CDR(form);
  args = SCHEME_STX_CAR(args);
  lambda_check_args(args, form, env);

  return make_lambda(env, form, rec, drec);
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

void scheme_define_parse(Scheme_Object *form, 
                         Scheme_Object **var, Scheme_Object **_stk_val,
                         int defmacro,
                         Scheme_Comp_Env *env,
                         int no_toplevel_check)
{
  Scheme_Object *vars, *rest;
  int len;
  DupCheckRecord r;

  len = check_form(form, form);
  if (len != 3)
    bad_form(form, len);
  
  rest = SCHEME_STX_CDR(form);
  vars = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  *_stk_val = SCHEME_STX_CAR(rest);

   *var = vars;

  scheme_begin_dup_symbol_check(&r, env);

  while (SCHEME_STX_PAIRP(vars)) {
    Scheme_Object *name;

    name = SCHEME_STX_CAR(vars);
    scheme_check_identifier(NULL, name, NULL, env, form);

    vars = SCHEME_STX_CDR(vars);

    scheme_dup_symbol_check(&r, NULL, name, "binding", form);
  }  

  if (!SCHEME_STX_NULLP(vars))
    scheme_wrong_syntax(NULL, *var, form, "bad variable list");
}

/**********************************************************************/
/*                               quote                                */
/**********************************************************************/

static Scheme_Object *
quote_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *v, *rest;

  rest = SCHEME_STX_CDR(form);
  if (!(SCHEME_STX_PAIRP(rest) && SCHEME_STX_NULLP(SCHEME_STX_CDR(rest))))
    scheme_wrong_syntax(NULL, NULL, form, "wrong number of parts");

  scheme_compile_rec_done_local(rec, drec);
  scheme_default_compile_rec(rec, drec);
  
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

Scheme_Object *
scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
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

static Scheme_Object *
if_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  int len, opt;
  Scheme_Object *test, *thenp, *elsep, *name, *rest;
  Scheme_Compile_Info recs[3];

  len = check_form(form, form);
  check_if_len(form, len);

  name = env->value_name;
  env->value_name = NULL;
  scheme_compile_rec_done_local(rec, drec);

  name = scheme_check_name_property(form, name);

  rest = SCHEME_STX_CDR(form);
  test = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  thenp = SCHEME_STX_CAR(rest);
  if (len == 4) {
    rest = SCHEME_STX_CDR(rest);
    elsep = SCHEME_STX_CAR(rest);
  } else
    elsep = scheme_compiled_void();

  scheme_init_compile_recs(rec, drec, recs, 3);

  env = scheme_no_defines(env);

  test = scheme_compile_expr(test, env, recs, 0);

  if (SCHEME_TYPE(test) > _scheme_ir_values_types_) {
    opt = 1;
    
    if (SCHEME_FALSEP(test)) {
      /* compile other branch only to get syntax checking: */
      recs[2].dont_mark_local_use = 1;
      env->value_name = name;
      scheme_compile_expr(thenp, env, recs, 2);
  
      if (len == 4) {
        env->value_name = name;
	test = scheme_compile_expr(elsep, env, recs, 1);
      } else
	test = elsep;
    } else {
      if (len == 4) {
	/* compile other branch only to get syntax checking: */
	recs[2].dont_mark_local_use = 1;
        env->value_name = name;
	scheme_compile_expr(elsep, env, recs, 2);
      }

      env->value_name = name;
      test = scheme_compile_expr(thenp, env, recs, 1);
    }
  } else {
    opt = 0;
    env->value_name = name;
    thenp = scheme_compile_expr(thenp, env, recs, 1);
    if (len == 4) {
      env->value_name = name;
      elsep = scheme_compile_expr(elsep, env, recs, 2);
    }
  }

  scheme_merge_compile_recs(rec, drec, recs, (opt || (len == 3)) ? 2 : 3);
  
  if (opt)
    return test;
  else
    return scheme_make_branch(test, thenp, elsep);
}

/**********************************************************************/
/*                    with-continuation-mark                          */
/**********************************************************************/

static Scheme_Object *
with_cont_mark_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *key, *val, *expr, *value_name;
  Scheme_Compile_Info recs[3];
  Scheme_With_Continuation_Mark *wcm;
  int len;

  len = check_form(form, form);

  if (len != 4)
    bad_form(form, len);

  value_name = env->value_name;
  env = scheme_no_defines(env);
  env->value_name = NULL;

  form = SCHEME_STX_CDR(form);
  key = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  val = SCHEME_STX_CAR(form);
  form = SCHEME_STX_CDR(form);
  expr = SCHEME_STX_CAR(form);

  scheme_compile_rec_done_local(rec, drec);

  scheme_init_compile_recs(rec, drec, recs, 3);

  key = scheme_compile_expr(key, env, recs, 0);
  val = scheme_compile_expr(val, env, recs, 1);

  env->value_name = value_name;
  expr = scheme_compile_expr(expr, env, recs, 2);

  scheme_merge_compile_recs(rec, drec, recs, 3);

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

static Scheme_Object *
set_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Set_Bang *sb;
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *val, *name, *body, *rest, *find_name;
  int l, set_undef;

  l = check_form(form, form);
  if (l != 3)
    bad_form(form, l);

  rest = SCHEME_STX_CDR(form);
  name = SCHEME_STX_CAR(rest);
  rest = SCHEME_STX_CDR(rest);
  body = SCHEME_STX_CAR(rest);
  
  scheme_check_identifier("set!", name, NULL, env, form);

  find_name = name;

  var = scheme_compile_lookup(find_name, env, 
                              SCHEME_SETTING 
                              + SCHEME_GLOB_ALWAYS_REFERENCE
                              + (rec[drec].dont_mark_local_use 
                                 ? SCHEME_DONT_MARK_USE 
                                 : 0)
                              + (rec[drec].resolve_module_ids
                                 ? SCHEME_RESOLVE_MODIDS
                                 : 0),
                              env->in_modidx, 
                              &menv, NULL,
                              NULL, NULL,
                              NULL);

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
      || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
    var = scheme_register_toplevel_in_prefix(var, env, rec, drec, 0, NULL);
    if (env->genv->module)
      SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
    env->prefix->non_phaseless = 1;
  }

  scheme_compile_rec_done_local(rec, drec);

  env = scheme_no_defines(env);
  env->value_name = SCHEME_STX_SYM(name);

  val = scheme_compile_expr(body, env, rec, drec);

  env->value_name = NULL;
  
  set_undef = (rec[drec].comp_flags & COMP_ALLOW_SET_UNDEFINED);
 
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

static Scheme_Object *
ref_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  Scheme_Env *menv = NULL;
  Scheme_Object *var, *name, *rest, *dummy, *bind_id;
  int l, ok;

  env->prefix->non_phaseless = 1;

  l = check_form(form, form);

  /* retaining `dummy' ensures that the environment stays
     linked from the actual variable */
  if ((l == 1) || !rec[drec].testing_constantness)
    dummy = scheme_make_environment_dummy(env);
  else
    dummy = NULL;

  if (l == 1) {
    var = dummy;
    bind_id = NULL;
  } else {
    if (l != 2)
      bad_form(form, l);

    rest = SCHEME_STX_CDR(form);
    name = SCHEME_STX_CAR(rest);

    if (SCHEME_STX_PAIRP(name)) {
      rest = SCHEME_STX_CAR(name);
      if (env->genv->phase == 0) {
        var = scheme_top_stx;
      } else {
        var = scheme_datum_to_syntax(SCHEME_STX_VAL(scheme_top_stx), scheme_false, scheme_sys_wraps(env), 0, 0);
      }
      ok = scheme_stx_free_eq(rest, var, env->genv->phase);
    } else 
      ok = SCHEME_STX_SYMBOLP(name);

    if (!ok) {
      scheme_wrong_syntax("#%variable-reference", name, 
                          form, 
                          "not an identifier or #%%top form");
      return NULL;
    }

    if (SCHEME_STX_PAIRP(name)) {
      /* FIXME: when using #%top, need to set mutated flag */
      env->value_name = NULL;
      if (rec[drec].comp)
        var = scheme_compile_expr(name, env, rec, drec);
      else
        var = scheme_expand_expr(name, env, rec, drec);
    } else {
      var = scheme_compile_lookup(name, env, 
                                  SCHEME_REFERENCING 
                                  + SCHEME_GLOB_ALWAYS_REFERENCE
                                  + (rec[drec].dont_mark_local_use 
                                     ? SCHEME_DONT_MARK_USE 
                                     : 0)
                                  + (rec[drec].resolve_module_ids
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0)
                                  + (!rec[drec].comp
                                     ? SCHEME_STOP_AT_FREE_EQ
                                     : 0),
                                  env->in_modidx, 
                                  &menv, NULL,
                                  &bind_id, NULL, NULL);

      if (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)
          || SAME_TYPE(SCHEME_TYPE(var), scheme_module_variable_type)) {
        int imported = 0;
        imported = scheme_is_imported(var, env);

        if (rec[drec].comp) {
          var = scheme_register_toplevel_in_prefix(var, env, rec, drec, imported, NULL);
          if (!imported && env->genv->module && !rec[drec].testing_constantness)
            SCHEME_TOPLEVEL_FLAGS(var) |= SCHEME_TOPLEVEL_MUTATED;
        }
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_ir_local_type)) {
        /* ok */
      } else {
        scheme_wrong_syntax(NULL, name, form, "identifier does not refer to a variable");
      }

      if (rec[drec].comp)
        scheme_compile_rec_done_local(rec, drec);
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

static Scheme_Object *
case_lambda_compile (Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *list, *last, *c, *orig_form = form, *name;
  Scheme_Case_Lambda *cl;
  int i, count = 0;
  Scheme_Compile_Info *recs;

  form = scheme_stx_taint_disarm(form, NULL);
  
  form = SCHEME_STX_CDR(form);

  name = scheme_build_closure_name(orig_form, env);
  
  if (SCHEME_STX_NULLP(form)) {
    /* Case where there are no cases... */
    form = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
						 - (mzFLEX_DELTA * sizeof(Scheme_Object*)));

    form->type = scheme_case_lambda_sequence_type;
    ((Scheme_Case_Lambda *)form)->count = 0;
    ((Scheme_Case_Lambda *)form)->name = name;

    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);

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

    c = cons(scheme_datum_to_syntax(lambda_symbol, scheme_false, scheme_sys_wraps(env), 0, 0),
	      c);
    c = scheme_datum_to_syntax(c, orig_form, orig_form, 0, 2);
    
    return lambda_compile(c, env, rec, drec);
  }

  scheme_compile_rec_done_local(rec, drec);

  list = last = NULL;
  while (SCHEME_STX_PAIRP(form)) {
    Scheme_Object *clause;
    clause = SCHEME_STX_CAR(form);
    case_lambda_check_line(clause, orig_form, env);

    c = cons(lambda_symbol, clause);

    c = scheme_datum_to_syntax(c, clause, scheme_sys_wraps(env), 0, 0);

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

  scheme_compile_rec_done_local(rec, drec);
  recs = MALLOC_N_ATOMIC(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, drec, recs, count);

  env->value_name = NULL;

  for (i = 0; i < count; i++) {
    Scheme_Object *ce;
    ce = SCHEME_CAR(list);
    ce = scheme_compile_expr(ce, env, recs, i);
    cl->array[i] = ce;
    list = SCHEME_CDR(list);
  }

  scheme_merge_compile_recs(rec, drec, recs, count);

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

static Scheme_Object *
do_let_compile (Scheme_Object *form, Scheme_Comp_Env *origenv, char *formname,
		int recursive, int multi, Scheme_Compile_Info *rec, int drec,
		Scheme_Comp_Env *frame_already)
{
  Scheme_Object *bindings, *l, *binding, *name, **names, *forms, *defname, *scope;
  int num_clauses, num_bindings, i, j, k, m, pre_k;
  Scheme_Comp_Env *frame, *env, *rhs_env;
  Scheme_Compile_Info *recs;
  Scheme_Object *first = NULL, *existing_vars;
  Scheme_IR_Let_Value *last = NULL, *lv;
  DupCheckRecord r;
  int rec_env_already = rec[drec].env_already, body_block;
  Scheme_IR_Let_Header *head;

  if (rec_env_already >= 2) {
    body_block = (rec_env_already > 2);
    l = detect_traditional_letrec(form, origenv);
    if (!SAME_OBJ(l, form)) {
      rec_env_already = 1;
      form = l;
    } else
      rec_env_already = 2;
  } else
    body_block = !rec_env_already;

  i = scheme_stx_proper_list_length(form);
  if (i < 3)
    scheme_wrong_syntax(NULL, NULL, form, (!i ? "empty body not allowed" : NULL));

  bindings = SCHEME_STX_CDR(form);
  bindings = SCHEME_STX_CAR(bindings);
  num_clauses = scheme_stx_proper_list_length(bindings);

  if (num_clauses < 0)
    scheme_wrong_syntax(NULL, bindings, form, NULL);

  /* forms ends up being the let body */
  forms = SCHEME_STX_CDR(form);
  forms = SCHEME_STX_CDR(forms);
  forms = scheme_datum_to_syntax(forms, form, form, 0, 0);

  if (!num_clauses) {
    if (!body_block)
      scheme_signal_error("internal error: no local bindings, but body is not in a block");

    /* Even though there are no bindings, we need a scope to
       indicate a nested binding context */
    scope = scheme_new_scope(SCHEME_STX_LOCAL_BIND_SCOPE);
    env = scheme_new_compilation_frame(0, 0, scope, origenv);
    forms = scheme_stx_add_scope(forms, scope, scheme_env_phase(env->genv));

    name = scheme_check_name_property(form, origenv->value_name);
    env->value_name = name;

    return compile_sequence(forms, env, rec, drec, body_block);
  }
  
  if (multi) {
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
  } else
    num_bindings = num_clauses;

  if (rec_env_already)
    scope = NULL;
  else
    scope = scheme_new_scope(SCHEME_STX_LOCAL_BIND_SCOPE);

  names = MALLOC_N(Scheme_Object *, num_bindings);
  if (frame_already)
    frame = frame_already;
  else {
    frame = scheme_new_compilation_frame(num_bindings, 
                                         (rec_env_already ? SCHEME_INTDEF_SHADOW : 0),
                                         scope,
                                         origenv);
    if (rec_env_already)
      frame_already = frame;
  }
  env = frame;
  if (!recursive)
    rhs_env = scheme_no_defines(origenv);
  else
    rhs_env = env;

  recs = MALLOC_N_ATOMIC(Scheme_Compile_Info, (num_clauses + 1));

  defname = origenv->value_name;
  scheme_compile_rec_done_local(rec, drec);
  scheme_init_compile_recs(rec, drec, recs, num_clauses + 1);

  defname = scheme_check_name_property(form, defname);
  
  if (!frame_already) {
    scheme_begin_dup_symbol_check(&r, env);
  }

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

    existing_vars = scheme_stx_property(binding, existing_variables_symbol, NULL);

    name = SCHEME_STX_CAR(binding);
    if (multi) {
      while (!SCHEME_STX_NULLP(name)) {
	Scheme_Object *n;
	n = SCHEME_STX_CAR(name);
	names[k] = n;
	scheme_check_identifier(NULL, names[k], NULL, env, form);
	k++;
	name = SCHEME_STX_CDR(name);
      }

      for (j = pre_k; j < k; j++) {
	for (m = j + 1; m < k; m++) {
	  if (scheme_stx_bound_eq(names[m], names[j], scheme_make_integer(env->genv->phase)))
	    scheme_wrong_syntax(NULL, NULL, form,
				"multiple bindings of `%S' in the same clause", 
				SCHEME_STX_SYM(names[m]));
	}
      }
    } else {
      scheme_check_identifier(NULL, name, NULL, env, form);
      names[k++] = name;
    }
    
    if (!frame_already) {
      for (m = pre_k; m < k; m++) {
	scheme_dup_symbol_check(&r, NULL, names[m], "binding", form);
      }
    }

    lv = MALLOC_ONE_TAGGED(Scheme_IR_Let_Value);
    lv->iso.so.type = scheme_ir_let_value_type;
    if (!last)
      first = (Scheme_Object *)lv;
    else
      last->body = (Scheme_Object *)lv;
    last = lv;
    lv->count = (k - pre_k);

    if (lv->count == 1)
      rhs_env->value_name = SCHEME_STX_SYM(names[pre_k]);

    if (!recursive) {
      Scheme_Object *ce, *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      ce = scheme_compile_expr(rhs, rhs_env, recs, i);
      lv->value = ce;
    } else {
      Scheme_Object *rhs;
      rhs = SCHEME_STX_CDR(binding);
      rhs = SCHEME_STX_CAR(rhs);
      lv->value = rhs;
    }

    rhs_env->value_name = NULL;

    if (recursive) {
      for (m = pre_k; m < k; m++) {
	scheme_add_compilation_binding(m, names[m], frame);
      }
    }

    if (SCHEME_TRUEP(existing_vars)) {
      /* Install variables already generated by a lift: */
      scheme_set_compilation_variables(frame, (Scheme_IR_Local **)SCHEME_CDR(existing_vars),
                                       pre_k, k - pre_k);
    }
    
    bindings = SCHEME_STX_CDR(bindings);
  }
  
  if (!recursive) {
    for (i = 0; i < num_bindings; i++) {
      scheme_add_compilation_binding(i, names[i], frame);
    }
  }

  scheme_env_make_variables(env);

  k = 0;
  lv = (Scheme_IR_Let_Value *)first;
  for (i = 0; i < num_clauses; i++) {
    Scheme_IR_Local **vars;

    vars = MALLOC_N(Scheme_IR_Local*, lv->count);
    lv->vars = vars;
    for (j = lv->count; j--; ) {
      vars[j] = env->vars[k+j];
    }

    k += lv->count;
    lv = (Scheme_IR_Let_Value *)lv->body;
  }

  head = make_header(first, num_bindings, num_clauses,
                     (recursive ? SCHEME_LET_RECURSIVE : 0));

  if (recursive) {
    int prev_might_invoke = 0;
    int group_clauses = 0;

    k = 0;
    lv = (Scheme_IR_Let_Value *)first;
    for (i = 0; i < num_clauses; i++, lv = (Scheme_IR_Let_Value *)lv->body) {
      Scheme_Object *ce, *rhs;
      rhs = lv->value;
      if (scope)
        rhs = scheme_stx_add_scope(rhs, scope, scheme_env_phase(env->genv));
      if (lv->count == 1)
        env->value_name = lv->vars[0]->name;
      else
        env->value_name = NULL;
      ce = scheme_compile_expr(rhs, env, recs, i);
      env->value_name = NULL;
      lv->value = ce;
        
      /* Record when this binding doesn't use any or later
         bindings in the same set. In internal-definition mode,
         always break bindings into smaller sets based on this
         information; otherwise, we have to be more conservative as reflected
         by scheme_might_invoke_call_cc(), so record with
         SCHEME_IRLV_NO_GROUP_LATER_USES and check again at the end. */
      if ((rec_env_already == 2) /* int def: semantics is `let' */
          || (!prev_might_invoke
              && !scheme_might_invoke_call_cc(ce))) {
        group_clauses++;
        if ((group_clauses == 1)
            && !scheme_env_max_use_above(env, k)) {
          /* A clause that should be in its own `let' */
          SCHEME_IRLV_FLAGS(lv) |= SCHEME_IRLV_NO_GROUP_USES;
          group_clauses = 0;
        } else if (!scheme_env_max_use_above(env, k + lv->count)) {
          /* End a recursive `letrec' group */
          SCHEME_IRLV_FLAGS(lv) |= SCHEME_IRLV_NO_GROUP_LATER_USES;
          group_clauses = 0;
        }
      } else
        prev_might_invoke = 1;
      
      k += lv->count;
    }

    if (!prev_might_invoke) {
      Scheme_IR_Let_Header *current_head = head;
      Scheme_IR_Let_Value *next = NULL;
      int group_count = 0;
      lv = (Scheme_IR_Let_Value *)first;
      group_clauses = 0;
      for (i = 0; i < num_clauses; i++, lv = next) {
        next = (Scheme_IR_Let_Value *)lv->body;
        group_clauses++;
        group_count += lv->count;
        if (SCHEME_IRLV_FLAGS(lv) & (SCHEME_IRLV_NO_GROUP_USES
                                    | SCHEME_IRLV_NO_GROUP_LATER_USES)) {
          /* A clause that should be in its own `let' */
          Scheme_IR_Let_Header *next_head;
          int single = (SCHEME_IRLV_FLAGS(lv) & SCHEME_IRLV_NO_GROUP_USES);
          MZ_ASSERT(!single || (group_clauses == 1));
          if (current_head->num_clauses - group_clauses) {
            next_head = make_header(lv->body, 
                                    current_head->count - group_count,
                                    current_head->num_clauses - group_clauses,
                                    SCHEME_LET_RECURSIVE);
            lv->body = (Scheme_Object *)next_head;
            current_head->num_clauses = group_clauses;
            current_head->count = group_count;
          } else
            next_head = NULL;
          if (single)
            SCHEME_LET_FLAGS(current_head) -= SCHEME_LET_RECURSIVE;
          current_head = next_head;
          group_clauses = 0;
          group_count = 0;
        }
      }
    }
  }

  env->value_name = defname ? SCHEME_STX_SYM(defname) : NULL;
  {
    Scheme_Object *cs;
    if (scope) forms = scheme_stx_add_scope(forms, scope, scheme_env_phase(env->genv));
    cs = compile_sequence(forms, env, recs, num_clauses, body_block);
    last->body = cs;
  }
  env->value_name = NULL;

  scheme_merge_compile_recs(rec, drec, recs, num_clauses + 1);

  return (Scheme_Object *)head;
}

static Scheme_Object *
let_values_compile (Scheme_Object *form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec, int drec)
{
  return do_let_compile(form, env, "let-values", 0, 1, rec, drec, NULL);
}

static Scheme_Object *
letrec_values_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_let_compile(form, env, "letrec-values", 1, 1, rec, drec, NULL);
}

/**********************************************************************/
/*                   begin, begin0, implicit begins                   */
/**********************************************************************/

static Scheme_Object *compile_sequence(Scheme_Object *forms,
				       Scheme_Comp_Env *env, 
				       Scheme_Compile_Info *rec, int drec,
                                       int as_intdef)
{
  if (scheme_stx_proper_list_length(forms) < 0) {
    scheme_wrong_syntax(scheme_begin_stx_string, NULL, 
                        scheme_datum_to_syntax(cons(begin_symbol, forms), forms, forms, 0, 0),
                        IMPROPER_LIST_FORM);
    return NULL;
  } else {
    Scheme_Object *body;
    if (as_intdef)
      body = compile_block(forms, env, rec, drec);
    else
      body = compile_list(forms, env, rec, drec);
    return scheme_make_sequence_compilation(body, 1, 0);
  }
}

Scheme_Object *scheme_compiled_void()
{
  return scheme_void;
}

static Scheme_Object *
do_begin_compile(char *name,
		Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec, 
		int zero)
{
  Scheme_Object *forms, *body, *vname;

  form = scheme_stx_taint_disarm(form, NULL);

  forms = SCHEME_STX_CDR(form);
  
  if (SCHEME_STX_NULLP(forms)) {
    if (!zero && scheme_is_toplevel(env))
      return scheme_compiled_void();
    scheme_wrong_syntax(NULL, NULL, form, "empty form not allowed");
    return NULL;
  }

  check_form(form, form);

  if (zero) {
    vname = env->value_name;
    env = scheme_no_defines(env);
    env->value_name = vname;
  }

  /* if the begin has only one expression inside, drop the begin 
     TODO: is this right */
  if (SCHEME_STX_NULLP(SCHEME_STX_CDR(forms))) {
    forms = SCHEME_STX_CAR(forms);
    return scheme_compile_expr(forms, env, rec, drec);
  }

  if (!scheme_is_toplevel(env)) {
    /* Not at top-level */
    if (zero) {
      /* First expression is not part of the block: */
      Scheme_Compile_Info recs[2];
      Scheme_Object *first, *rest, *vname;

      vname = env->value_name;
      scheme_compile_rec_done_local(rec, drec);

      vname = scheme_check_name_property(form, vname);

      scheme_init_compile_recs(rec, drec, recs, 2);

      first = SCHEME_STX_CAR(forms);
      env->value_name = vname;
      first = scheme_compile_expr(first, env, recs, 0);
      env->value_name = NULL;
      rest = SCHEME_STX_CDR(forms);
      rest = compile_list(rest, env, recs, 1);
      
      scheme_merge_compile_recs(rec, drec, recs, 2);

      body = cons(first, rest);
    } else {
      Scheme_Object *v;
      v = scheme_check_name_property(form, env->value_name);
      env->value_name = v;

      body = compile_list(forms, env, rec, drec);

      env->value_name = NULL;
    }
  } else {
    /* Top level */
    body = compile_list(forms, env, rec, drec);
  }

  forms = scheme_make_sequence_compilation(body, zero ? -1 : 1, 0);

  if (!zero
      && SAME_TYPE(SCHEME_TYPE(forms), scheme_sequence_type)
      && scheme_is_toplevel(env)) {
    forms->type = scheme_splice_sequence_type;
    return forms;
  }

  return forms;
}

static Scheme_Object *
begin_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_compile("begin", form, env, rec, drec, 0);
}

static Scheme_Object *
begin0_compile (Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_begin_compile("begin0", form, env, rec, drec, 1);
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
     used to "link" to the right environment at run time. The #f as
     a toplevel is handled in the prefix linker specially. */
  return scheme_register_toplevel_in_prefix(scheme_false, env, NULL, 0, 0, NULL);
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
/*                         compilation dispatcher                         */
/*========================================================================*/

static Scheme_Object *
inner_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
                   Scheme_Compile_Info *rec, int drec, int start_app_position)
{
  int len;

  len = scheme_stx_proper_list_length(form);

  if (!len) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_null;
  } else if (len > 0) {
    Scheme_Compile_Info *recs, quick[5];
    int i;
    Scheme_Object *c, *p, *comp_first, *comp_last, *name, *first, *rest;

    name = env->value_name;
    scheme_compile_rec_done_local(rec, drec);

    if (len <= 5)
      recs = quick;
    else
      recs = MALLOC_N_ATOMIC(Scheme_Compile_Info, len);
    scheme_init_compile_recs(rec, drec, recs, len);

    comp_first = comp_last = NULL;

    for (i = 0, rest = form; i < len; i++) {
      first = SCHEME_STX_CAR(rest);
      rest = SCHEME_STX_CDR(rest);

      if (SCHEME_STX_NULLP(rest))
        env->value_name = name;
      
      c = compile_expand_expr(first, env, recs, i,
                              !i && start_app_position);
      env->value_name = NULL;

      p = scheme_make_pair(c, scheme_null);
      if (comp_last)
	SCHEME_CDR(comp_last) = p;
      else
	comp_first = p;
      comp_last = p;

      if (!i && start_app_position && (len == 2)
          && SAME_OBJ(c, scheme_varref_const_p_proc)) {
        recs[1].testing_constantness = 1;
      }
    }

    scheme_merge_compile_recs(rec, drec, recs, len);

    return comp_first;
  } else {
    scheme_signal_error("internal error: compile-list on non-list");
    return NULL;
  }
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  Scheme_Object *result, *rator;
  int len;

  form = scheme_stx_taint_disarm(form, NULL);

  len = scheme_stx_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax(scheme_application_stx_string, NULL, form, NULL);

  env->value_name = NULL;

  scheme_compile_rec_done_local(rec, drec);
  form = inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

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

Scheme_Object *compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
                            Scheme_Compile_Info *rec, int drec)
{
  return inner_compile_list(form, env, rec, drec, 0);
}

static Scheme_Object *compile_expr_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return compile_expr(form, 
                      env,
                      rec,
                      p->ku.k.i3,
                      p->ku.k.i2);
}

Scheme_Object *
compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
             Scheme_Compile_Expand_Info *rec, int drec, 
             int app_position)
{
  Scheme_Object *name, *var, *stx, *normal, *can_recycle_stx = NULL, *orig_unbound_name = NULL;
  Scheme_Env *menv = NULL;
  GC_CAN_IGNORE char *not_allowed;
  int has_orig_unbound = 0, need_macro_scope = 0;

 top:

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Compile_Expand_Info *recx;

      recx = MALLOC_ONE_ATOMIC(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));

      p->ku.k.p1 = (void *)form;
      p->ku.k.p2 = (void *)env;
      p->ku.k.p3 = (void *)recx;
      p->ku.k.i3 = 0;
      p->ku.k.i2 = app_position;

      var = scheme_handle_stack_overflow(compile_expand_expr_k);

      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
      return var;
    }
  }
#endif

  DO_CHECK_FOR_BREAK(scheme_current_thread, ;);

  MZ_ASSERT(SCHEME_STXP(form));

  scheme_default_compile_rec(rec, drec);

  if (!SCHEME_STX_PAIRP(form)) {
    if (SCHEME_STX_SYMBOLP(form)) {
      Scheme_Object *find_name = form, *inline_variant, *bind_id;
      int protected = 0;

      inline_variant = NULL;
      var = scheme_compile_lookup(find_name, env, 
                                  SCHEME_NULL_FOR_UNBOUND
                                  + SCHEME_ENV_CONSTANTS_OK
                                  + (rec[drec].comp
                                     ? SCHEME_ELIM_CONST 
                                     : 0)
                                  + (app_position 
                                     ? SCHEME_APP_POS 
                                     : 0)
                                  + ((rec[drec].comp && rec[drec].dont_mark_local_use) ? 
                                     SCHEME_DONT_MARK_USE 
                                     : 0)
                                  + ((rec[drec].comp && rec[drec].resolve_module_ids)
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0)
                                  + ((!rec[drec].comp && (rec[drec].depth == -2))
                                     ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                     : 0)
                                  + (!rec[drec].comp
                                     ? SCHEME_STOP_AT_FREE_EQ
                                     : 0),
                                  env->in_modidx, 
                                  &menv, &protected,
                                  &bind_id, &need_macro_scope,
                                  &inline_variant);

      if (!var) {
	/* Top variable */
        not_allowed = "reference to an unbound identifier";
      } else {
	if (SAME_TYPE(SCHEME_TYPE(var), scheme_primitive_syntax_type)) {
	  scheme_wrong_syntax(NULL, NULL, form, "bad syntax");
	}
      }
    } else {
      compile_application;
    }
  } else {
    name = scheme_stx_taint_disarm(form, NULL);
    name = SCHEME_STX_CAR(name);
    if (SCHEME_STX_SYMBOLP(name)) {
      /* Check for macros: */
      Scheme_Object *find_name = name;
      Scheme_Expand_Info erec1;

      /* While resolving name, we used to need taints from `form' */
      scheme_init_expand_recs(rec, drec, &erec1, 1);

      var = scheme_compile_lookup(find_name, env, 
                                  SCHEME_APP_POS
                                  + SCHEME_NULL_FOR_UNBOUND
                                  + SCHEME_ENV_CONSTANTS_OK
                                  + (rec[drec].comp
                                     ? SCHEME_ELIM_CONST
                                     : 0)
                                  + SCHEME_DONT_MARK_USE
                                  + ((rec[drec].comp && rec[drec].resolve_module_ids)
                                     ? SCHEME_RESOLVE_MODIDS
                                     : 0)
                                  + ((!rec[drec].comp && (rec[drec].depth == -2))
                                     ? (SCHEME_OUT_OF_CONTEXT_OK | SCHEME_OUT_OF_CONTEXT_LOCAL)
                                     : 0)
                                  + (!rec[drec].comp
                                     ? SCHEME_STOP_AT_FREE_EQ
                                     : 0),
                                  env->in_modidx, 
                                  &menv, NULL,
                                  NULL, &need_macro_scope,
                                  NULL);
      
      if (!var) {
        not_allowed = "reference to an unbound identifier";
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_primitive_syntax_type)) {
        Scheme_Syntax *f;
        f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
        return f(form, env, rec, drec);
      } else
        compile_application;
    }
  }
}

static int arg_count(Scheme_Object *lam, Scheme_Comp_Env *env)
{
  Scheme_Object *l, *id, *form = lam;
  int cnt = 0;
  DupCheckRecord r;

  lam = scheme_stx_taint_disarm(lam, NULL);
  
  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  l = SCHEME_STX_CAR(lam);

  lam = SCHEME_STX_CDR(lam);
  if (!SCHEME_STX_PAIRP(lam)) return -1;

  while (SCHEME_STX_PAIRP(lam)) { lam = SCHEME_STX_CDR(lam); }
  if (!SCHEME_STX_NULLP(lam)) return -1;
  

  scheme_begin_dup_symbol_check(&r, env);

  while (SCHEME_STX_PAIRP(l)) {
    id = SCHEME_STX_CAR(l);
    scheme_check_identifier("lambda", id, NULL, env, form);
    scheme_dup_symbol_check(&r, NULL, id, "argument", form);
    l = SCHEME_STX_CDR(l);
    cnt++;
  }
  if (!SCHEME_STX_NULLP(l)) return -1;

  return cnt;
}

static Scheme_Object *
compile_app(Scheme_Object *orig_form, Scheme_Comp_Env *env, 
            Scheme_Compile_Expand_Info *rec, int drec)
{
  Scheme_Object *form, *naya, *forms, *orig_vname = env->value_name;
  int tsc;

  forms = orig_form;

  tsc = rec[drec].pre_unwrapped;
  rec[drec].pre_unwrapped = 0;

  if (tsc) {
    form = forms;
  } else {
    form = SCHEME_STX_CDR(forms);
    form = scheme_datum_to_syntax(form, forms, forms, 0, 0);
  }
  
  if (SCHEME_STX_NULLP(form)) {
    /* Compile/expand empty application to null list: */
    return scheme_null;
  } else if (!SCHEME_STX_PAIRP(form)) {
     /* will end in error */
    return compile_application(form, env, rec, drec);
  } else {
    Scheme_Object *name, *origname, *gval, *orig_rest_form, *rest_form;
    name = SCHEME_STX_CAR(form);
    origname = name;

    /* look for ((lambda (x ...) ....) ....) or ((lambda x ....) ....) */
    if (SAME_OBJ(SCHEME_STX_VAL(name), lambda_symbol)) {
      Scheme_Object *argsnbody, *d_name;

      d_name = scheme_stx_taint_disarm(name, NULL);
      argsnbody = SCHEME_STX_CDR(d_name);
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

              scheme_begin_dup_symbol_check(&r, env);
	      
              while (!SCHEME_STX_NULLP(args)) {
                Scheme_Object *v, *n;
		  
                if (pl < 0)
                  n = args;
                else
                  n = SCHEME_STX_CAR(args);
                scheme_check_identifier("lambda", n, NULL, env, name);

                /* If we don't check here, the error is in terms of `let': */
                scheme_dup_symbol_check(&r, NULL, n, "argument", name);
  
                if (pl < 0) {
                  v = scheme_intern_symbol("list");
                  v = scheme_datum_to_syntax(v, scheme_false, scheme_sys_wraps(env), 0, 0);
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

              body = scheme_datum_to_syntax(icons(begin_symbol, body), form, 
                                            scheme_sys_wraps(env), 
                                            0, 2);
              
              body = scheme_datum_to_syntax(cons(let_values_symbol,
                                                 cons(bindings,
                                                      cons(body, scheme_null))),
                                            form, 
                                            scheme_sys_wraps(env), 
                                            0, 2);

              body = scheme_syntax_taint_rearm(body, orig_form);

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
                  first = scheme_stx_taint_disarm(first, NULL);
                  second = scheme_stx_taint_disarm(second, NULL);
                  second = SCHEME_STX_CDR(second);
                  lhs = SCHEME_STX_CAR(second);
                  second = SCHEME_STX_CDR(second);
                  first = SCHEME_STX_CDR(first);
                  first = SCHEME_STX_CDR(first);
                  first = icons(begin_symbol, first);
                  first = scheme_datum_to_syntax(first, orig_post_first, scheme_sys_wraps(env), 0, 1);
                  second = icons(begin_symbol, second);
                  second = scheme_datum_to_syntax(second, orig_post_second, scheme_sys_wraps(env), 0, 1);
                  /* Convert to let-values: */
                  name = icons(let_values_symbol,
                               icons(icons(icons(lhs, icons(first, scheme_null)), 
                                           scheme_null),
                                     icons(second, scheme_null)));
                  form = scheme_datum_to_syntax(name, forms, scheme_sys_wraps(env), 0, 2);
                  env->value_name = orig_vname;
                  return compile_expand_expr(form, env, rec, drec, 0);
                }
                if (!SAME_OBJ(second, orig_second)) {
                  at_second = scheme_datum_to_syntax(icons(second, the_end), at_second, at_second, 0, 2);
                } 
              }
              if (!SAME_OBJ(first, orig_first)
                  || !SAME_OBJ(at_second, orig_at_second)) {
                at_first = scheme_datum_to_syntax(icons(first, at_second), at_first, at_first, 0, 2);
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
      form = scheme_datum_to_syntax(scheme_make_pair(name, rest_form), forms, forms, 0, 2);
    }

    return compile_application(form, env, rec, drec);
  }
}

static Scheme_Object *
app_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return compile_expand_app(form, env, rec, drec);
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return compile_expr(form, env, rec, drec, 0);
}
static Scheme_Object *
compile_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
              Scheme_Compile_Expand_Info *rec, int drec,
              int mixed)
{
  just_compile;
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
