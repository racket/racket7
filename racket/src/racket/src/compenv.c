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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schexpobs.h"

#define TABLE_CACHE_MAX_SIZE 2048

/* Pre-allocate local variable reference objects.
   first dimension: position in the current stack frame
   second dimension: 0 for local variables, 1 for unboxed local variables
   third dimension: flags. TODO has to do with whether something is an unboxed fixnum, flonum, or extnum */
READ_ONLY static Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][MAX_CONST_LOCAL_TYPES][MAX_CONST_LOCAL_FLAG_VAL + 1];
READ_ONLY static Scheme_Object *toplevels[MAX_CONST_TOPLEVEL_DEPTH][MAX_CONST_TOPLEVEL_POS][SCHEME_TOPLEVEL_FLAGS_MASK + 1];

ROSYM static Scheme_Object *undefined_error_name_symbol;

/* If locked, these are probably sharable: */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *toplevels_ht);
THREAD_LOCAL_DECL(static Scheme_Hash_Table *locals_ht[2]);

static void init_compile_data(Scheme_Comp_Env *env);

static void init_scheme_local();
static void init_toplevels();

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_compenv()
{
  init_scheme_local();
  init_toplevels();

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_compenv_places(void)
{
  REGISTER_SO(toplevels_ht);
  REGISTER_SO(locals_ht[0]);
  REGISTER_SO(locals_ht[1]);

  {
    Scheme_Hash_Table *ht;
    toplevels_ht = scheme_make_hash_table_equal();
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[0] = ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[1] = ht;
  }
}

void scheme_init_compenv_symbol(void)
{
  REGISTER_SO(undefined_error_name_symbol);
  undefined_error_name_symbol = scheme_intern_symbol("undefined-error-name");
}

/*========================================================================*/
/*                       compilation info management                      */
/*========================================================================*/

void scheme_default_compile_rec(Scheme_Compile_Info *rec, int drec)
{
}

void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec, 
			      Scheme_Compile_Info *dest, int n)
{
  int i;

  for (i = 0; i < n; i++) {
    dest[i].comp = 1;
    dest[i].dont_mark_local_use = src[drec].dont_mark_local_use;
    dest[i].resolve_module_ids = src[drec].resolve_module_ids;
    dest[i].pre_unwrapped = 0;
    dest[i].testing_constantness = 0;
    dest[i].env_already = 0;
    dest[i].comp_flags = src[drec].comp_flags;
  }
}

void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec, 
			       Scheme_Compile_Info *dest, int n)
{
  /* Nothing to do anymore, since we moved max_let_depth to resolve phase */
}

void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec)
{
  lam[dlrec].comp = 1;
  lam[dlrec].dont_mark_local_use = src[drec].dont_mark_local_use;
  lam[dlrec].resolve_module_ids = src[drec].resolve_module_ids;
  lam[dlrec].substitute_bindings = src[dlrec].substitute_bindings;
  lam[dlrec].pre_unwrapped = 0;
  lam[dlrec].testing_constantness = 0;
  lam[dlrec].env_already = 0;
  lam[dlrec].comp_flags = src[drec].comp_flags;
}

void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			     Scheme_Compile_Info *lam, int dlrec)
{
}

void scheme_compile_rec_done_local(Scheme_Compile_Info *rec, int drec)
{
}

/*========================================================================*/
/*        compile-time env, constructors and simple queries               */
/*========================================================================*/

static void init_compile_data(Scheme_Comp_Env *env)
{
  env->max_use = -1;
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, Scheme_Object *scopes, Scheme_Comp_Env *base)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Comp_Env);
#ifdef MZTAG_REQUIRED
  frame->type = scheme_rt_comp_env;
#endif

  frame->scopes = scopes;

  {
    Scheme_Object **vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->binders = vals;
    vals = MALLOC_N(Scheme_Object *, count);
    frame->bindings = vals;
  }

  frame->num_bindings = num_bindings;
  frame->flags = flags;
  frame->next = base;
  frame->genv = base->genv;
  frame->insp = base->insp;
  frame->prefix = base->prefix;
  frame->in_modidx = base->in_modidx;
  frame->observer = base->observer;

  if (base->next)
    frame->skip_depth = base->skip_depth + 1;
  else
    frame->skip_depth = 0;

  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_new_comp_env(Scheme_Env *genv, Scheme_Object *insp, Scheme_Object *scopes, int flags)
{
  Scheme_Comp_Env *e;
  Comp_Prefix *cp;

  if (!insp)
    insp = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);

  e = (Scheme_Comp_Env *)MALLOC_ONE_RT(Scheme_Comp_Env);
#ifdef MZTAG_REQUIRED
  e->type = scheme_rt_comp_env;
#endif
  e->num_bindings = 0;
  e->next = NULL;
  e->genv = genv;
  e->insp = insp;
  e->flags = flags;
  init_compile_data(e);

  cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
  cp->type = scheme_rt_comp_prefix;
#endif

  e->prefix = cp;

  e->scopes = scopes;

  return e;
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  Scheme_Object *binding;

  if ((index >= frame->num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);

  if (frame->scopes) {
    /* sometimes redundant: */
    val = scheme_stx_adjust_frame_bind_scopes(val, frame->scopes, scheme_env_phase(frame->genv),
                                              SCHEME_STX_ADD);
  }
  
  frame->binders[index] = val;
  
  if (!frame->bindings[index]) {
    if (frame->flags & SCHEME_INTDEF_SHADOW) {
      binding = scheme_stx_lookup(val, scheme_env_phase(frame->genv));
    } else {
      binding = scheme_gensym(SCHEME_STX_VAL(val));
      scheme_add_local_binding(val, scheme_env_phase(frame->genv), binding);
    }

    frame->bindings[index] = binding;
  }

  frame->skip_table = NULL;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Object *scope, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  
  len = scheme_stx_list_length(vals);
  count = len;

  frame = scheme_new_compilation_frame(count, flags, scope, env);

  for (i = 0; i < len ; i++) {
    if (SCHEME_STX_SYMBOLP(vals)) {
      scheme_add_compilation_binding(i, vals, frame);
    } else {
      Scheme_Object *a;
      a = SCHEME_STX_CAR(vals);
      scheme_add_compilation_binding(i, a, frame);
      vals = SCHEME_STX_CDR(vals);
    }
  }
  
  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env)
      || scheme_is_module_env(env)
      || scheme_is_module_begin_env(env)
      || (env->flags & SCHEME_INTDEF_FRAME))
    return scheme_new_compilation_frame(0, 0, NULL, env);
  else
    return env;
}

int scheme_is_toplevel(Scheme_Comp_Env *env)
{
  return !env->next || (env->flags & SCHEME_TOPLEVEL_FRAME);
}

int scheme_is_nested_module(Scheme_Comp_Env *env)
{
  return (env->flags & SCHEME_NESTED_MODULE_FRAME);
}

int scheme_is_module_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_FRAME);
}

int scheme_is_module_begin_env(Scheme_Comp_Env *env)
{
  return !!(env->flags & SCHEME_MODULE_BEGIN_FRAME);
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, NULL, env);
}

Scheme_Object *scheme_make_toplevel(mzshort depth, int position, int resolved, int flags)
{
  Scheme_Toplevel *tl;
  Scheme_Object *v, *pr;

  /* Important: non-resolved can't be cached, because the ISCONST
     field is modified to track mutated module-level variables. But
     the value for a specific toplevel is cached in the environment
     layer. */

  if (resolved) {
    if ((depth < MAX_CONST_TOPLEVEL_DEPTH)
	&& (position < MAX_CONST_TOPLEVEL_POS))
      return toplevels[depth][position][flags];

    if ((position < 0xFFFF) && (depth < 0xFF)) {
      int ep = position | (depth << 16) | (flags << 24);
      pr = scheme_make_integer(ep);
    } else {
      pr = scheme_make_vector(3, NULL);
      SCHEME_VEC_ELS(pr)[0] = scheme_make_integer(position);
      SCHEME_VEC_ELS(pr)[1] = scheme_make_integer(flags);
      SCHEME_VEC_ELS(pr)[2] = scheme_make_integer(depth);
    }
    v = scheme_hash_get_atomic(toplevels_ht, pr);
    if (v)
      return v;
  } else
    pr = NULL;

  tl = (Scheme_Toplevel *)scheme_malloc_atomic_tagged(sizeof(Scheme_Toplevel));
  tl->iso.so.type = (resolved ? scheme_toplevel_type : scheme_ir_toplevel_type);
  tl->depth = depth;
  tl->position = position;
  SCHEME_TOPLEVEL_FLAGS(tl) = flags | HIGH_BIT_TO_DISABLE_HASHING;

  if (resolved) {
    if (toplevels_ht->count > TABLE_CACHE_MAX_SIZE) {
      toplevels_ht = scheme_make_hash_table_equal();
    }
    scheme_hash_set_atomic(toplevels_ht, pr, (Scheme_Object *)tl);
  }

  return (Scheme_Object *)tl;
}

Scheme_Object *scheme_register_toplevel_in_comp_prefix(Scheme_Object *var, Comp_Prefix *cp,
                                                       int imported, Scheme_Object *inline_variant)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *o;

  ht = cp->toplevels;
  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    cp->toplevels = ht;
  }

  o = scheme_hash_get(ht, var);
  if (o)
    return o;

  o = scheme_make_toplevel(0, cp->num_toplevels, 0, 
                           (imported 
                            ? ((SCHEME_MODVAR_FLAGS(var) & SCHEME_MODVAR_CONST)
                               ? SCHEME_TOPLEVEL_CONST
                               : ((SCHEME_MODVAR_FLAGS(var) & SCHEME_MODVAR_FIXED)
                                  ? SCHEME_TOPLEVEL_FIXED
                                  : SCHEME_TOPLEVEL_READY))
                            : 0));

  scheme_hash_set(ht, var, o);

  if (inline_variant) {
    ht = cp->inline_variants;
    if (!ht) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      cp->inline_variants = ht;
    }
    scheme_hash_set(ht, scheme_make_integer(cp->num_toplevels), inline_variant);
  }
  
  cp->num_toplevels++;

  return o;
}

Scheme_Object *scheme_register_toplevel_in_prefix(Scheme_Object *var, Scheme_Comp_Env *env,
						  Scheme_Compile_Info *rec, int drec,
                                                  int imported, Scheme_Object *inline_variant)
{
  Comp_Prefix *cp = env->prefix;

  if (rec && rec[drec].dont_mark_local_use) {
    /* Make up anything; it's going to be ignored. */
    return scheme_make_toplevel(0, 0, 0, 0);
  }

  return scheme_register_toplevel_in_comp_prefix(var, cp, imported, inline_variant);
}

Scheme_Object *scheme_toplevel_to_flagged_toplevel(Scheme_Object *_tl, int flags)
{
  Scheme_Toplevel *tl = (Scheme_Toplevel *)_tl;
  return scheme_make_toplevel(tl->depth, tl->position, 0, flags);
}

/*========================================================================*/
/*                     compile-time env, lookup bindings                  */
/*========================================================================*/

static void init_scheme_local() 
{
  int i, k, cor;

#ifndef USE_TAGGED_ALLOCATION
  GC_CAN_IGNORE Scheme_Local *all;

  all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) 
                                              * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                                              * MAX_CONST_LOCAL_TYPES
                                              * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
  scheme_misc_count += (sizeof(Scheme_Local) 
                        * (MAX_CONST_LOCAL_FLAG_VAL + 1)
                        * MAX_CONST_LOCAL_TYPES
                        * MAX_CONST_LOCAL_POS);
# endif    
#endif

  for (i = 0; i < MAX_CONST_LOCAL_POS; i++) {
    for (k = 0; k < MAX_CONST_LOCAL_TYPES; k++) {
      for (cor = 0; cor < (MAX_CONST_LOCAL_FLAG_VAL + 1); cor++) {
        Scheme_Object *v;

#ifndef USE_TAGGED_ALLOCATION
        v = (Scheme_Object *)(all++);
#else
        v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
        v->type = k + scheme_local_type;
        SCHEME_LOCAL_POS(v) = i;
        SCHEME_LOCAL_FLAGS(v) = cor | HIGH_BIT_TO_DISABLE_HASHING;

        scheme_local[i][k][cor] = v;
      }
    }
  }
}

static void init_toplevels()
{
  int i, k, cnst;

#ifndef USE_TAGGED_ALLOCATION
  GC_CAN_IGNORE Scheme_Toplevel *all;

  all = (Scheme_Toplevel *)scheme_malloc_eternal(sizeof(Scheme_Toplevel) 
      * MAX_CONST_TOPLEVEL_DEPTH 
      * MAX_CONST_TOPLEVEL_POS
      * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# ifdef MEMORY_COUNTING_ON
  scheme_misc_count += (sizeof(Scheme_Toplevel) 
      * MAX_CONST_TOPLEVEL_DEPTH 
      * MAX_CONST_TOPLEVEL_POS
      * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
# endif
#endif

  for (i = 0; i < MAX_CONST_TOPLEVEL_DEPTH; i++) {
    for (k = 0; k < MAX_CONST_TOPLEVEL_POS; k++) {
      for (cnst = 0; cnst <= SCHEME_TOPLEVEL_FLAGS_MASK; cnst++) {
        Scheme_Toplevel *v;

#ifndef USE_TAGGED_ALLOCATION
        v = (all++);
#else
        v = (Scheme_Toplevel *)scheme_malloc_eternal_tagged(sizeof(Scheme_Toplevel));
#endif
        v->iso.so.type = scheme_toplevel_type;
        v->depth = i;
        v->position = k;
        SCHEME_TOPLEVEL_FLAGS(v) = cnst | HIGH_BIT_TO_DISABLE_HASHING;

        toplevels[i][k][cnst] = (Scheme_Object *)v;
      }
    }
  }
}

static Scheme_Object *alloc_local(short type, int pos)
{
  Scheme_Object *v;

  v = (Scheme_Object *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  v->type = type;
  SCHEME_LOCAL_POS(v) = pos;

  return (Scheme_Object *)v;
}

/* type should be either scheme_local_type or scheme_local_unbox_type
   TODO: double check that */
Scheme_Object *scheme_make_local(Scheme_Type type, int pos, int flags)
{
  int k;
  Scheme_Object *v, *key;

  /* k is 0 if type is scheme_local_type and 1 if type is scheme_local_unbox_type */
  k = type - scheme_local_type;
  
  /* Helper for reading bytecode: make sure flags is a valid value */
  if ((flags < 0) || (flags > (SCHEME_MAX_LOCAL_TYPE + SCHEME_LOCAL_TYPE_OFFSET)))
    flags = SCHEME_LOCAL_OTHER_CLEARS;

  if (pos < MAX_CONST_LOCAL_POS) {
    return scheme_local[pos][k][flags];
  }

  key = scheme_make_integer(pos);
  if (flags) {
    key = scheme_make_pair(scheme_make_integer(flags), key);
  }

  v = scheme_hash_get(locals_ht[k], key);
  if (v)
    return v;

  v = alloc_local(type, pos);
  SCHEME_LOCAL_FLAGS(v) = flags | HIGH_BIT_TO_DISABLE_HASHING;

  if (locals_ht[k]->count > TABLE_CACHE_MAX_SIZE) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    locals_ht[k] = ht;
  }

  scheme_hash_set(locals_ht[k], key, v);

  return v;
}

Scheme_Object *scheme_hash_module_variable(Scheme_Env *env, Scheme_Object *modidx, 
					   Scheme_Object *stxsym, Scheme_Object *insp,
					   int pos, intptr_t mod_phase, int is_constant,
                                           Scheme_Object *shape)
/* is_constant == 2 => constant over all instantiations and phases */
{
  Scheme_Object *val;
  Scheme_Hash_Table *ht;

  if (!env->modvars) {
    ht = scheme_make_hash_table_equal_modix_eq();
    env->modvars = ht;
  }

  stxsym = SCHEME_STX_SYM(stxsym);

  ht = (Scheme_Hash_Table *)scheme_hash_get(env->modvars, modidx);

  if (!ht) {
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(env->modvars, modidx, (Scheme_Object *)ht);
  }

  /* Loop for inspector-specific hash table, maybe: */
  while (1) {
    
    val = scheme_hash_get(ht, stxsym);
    
    if (!val) {
      Module_Variable *mv;
      
      mv = MALLOC_ONE_TAGGED(Module_Variable);
      mv->iso.so.type = scheme_module_variable_type;
      
      mv->modidx = modidx;
      mv->sym = stxsym;
      mv->insp = insp;
      mv->pos = pos;
      mv->mod_phase = (int)mod_phase;
      mv->shape = shape;

      if (is_constant > 1)
        SCHEME_MODVAR_FLAGS(mv) |= SCHEME_MODVAR_CONST;
      else if (is_constant)
        SCHEME_MODVAR_FLAGS(mv) |= SCHEME_MODVAR_FIXED;
      
      val = (Scheme_Object *)mv;
      
      scheme_hash_set(ht, stxsym, val);
      
      break;
    } else {
      /* Check that inspector is the same. */
      Module_Variable *mv = (Module_Variable *)val;
      
      if (!SAME_OBJ(mv->insp, insp)) {
	/* Need binding for a different inspector. Try again. */
	val = scheme_hash_get(ht, insp);
	if (!val) {
	  Scheme_Hash_Table *ht2;
	  /* Make a table for this specific inspector */
	  ht2 = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(ht, insp, (Scheme_Object *)ht2);
	  ht = ht2;
	  /* loop... */
	} else
	  ht = (Scheme_Hash_Table *)val;
      } else
	break;
    }
  }

  return val;
}

/*********************************************************************/

Scheme_Object *scheme_intern_struct_proc_shape(int shape)
{
  char buf[20];
  sprintf(buf, "struct%d", shape);
  return scheme_intern_symbol(buf);
}

/*********************************************************************/

static Scheme_Object *get_local_name(Scheme_Object *id)
{
  Scheme_Object *name;

  name = scheme_stx_property(id, undefined_error_name_symbol, NULL);
  if (name && SCHEME_SYMBOLP(name))
    return name;
  else
    return SCHEME_STX_VAL(id);
}

static Scheme_IR_Local *scheme_make_variable(Scheme_Object *id)
{
  Scheme_IR_Local *var;

  var = MALLOC_ONE_TAGGED(Scheme_IR_Local);
  var->so.type = scheme_ir_local_type;
  if (id) {
    id = get_local_name(id);
    var->name = id;
  }

  return var;
}

static void record_local_use(Scheme_IR_Local *var, int flags)
{
  if (var->use_count < SCHEME_USE_COUNT_INF)
    var->use_count++;
  if (flags & SCHEME_SETTING)
    var->mutated = 1;
  if (!(flags & (SCHEME_APP_POS | SCHEME_SETTING)))
    if (var->non_app_count < SCHEME_USE_COUNT_INF)
      var->non_app_count++;
}

Scheme_Object *
scheme_compile_lookup(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags,
                      Scheme_Object **_inline_variant)
{
  Scheme_Object *v;

  v = scheme_hash_tree_get(env->vars, SCHEME_STX_VAL(find_id));

  if (!v)
    v = scheme_hash_get(scheme_primitives, SCHEME_STX_VAL(find_id));

  if (!v)
    scheme_wrong_syntax(NULL, NULL, find_id, "free identifier");

  if (SAME_TYPE(SCHEME_TYPE(v), scheme_ir_local_type)) {
    if (!(env->flags & COMP_ENV_DONT_COUNT_AS_USE))
      record_local_use((Scheme_IR_Local *)v, flags);
  }
  
  return v;
}

Scheme_Object *scheme_extract_unsafe(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_unsafe_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_flfxnum(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_flfxnum_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_extfl(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_extfl_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_futures(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_futures_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

Scheme_Object *scheme_extract_foreign(Scheme_Object *o)
{
  Scheme_Env *home;
  home = scheme_get_bucket_home((Scheme_Bucket *)o);
  if (home && home->module && scheme_is_foreign_modname(home->module->modname))
    return (Scheme_Object *)((Scheme_Bucket *)o)->val;
  else
    return NULL;
}

int scheme_env_check_reset_any_use(Scheme_Comp_Env *frame)
{
  int any_use;

  any_use = frame->any_use;
  frame->any_use = 0;

  return any_use;
}

int scheme_env_max_use_above(Scheme_Comp_Env *frame, int pos)
{
  return frame->max_use >= pos;
}

void scheme_mark_all_use(Scheme_Comp_Env *frame)
{
  /* Mark all variables as used for the purposes of `letrec-syntaxes+values`
     splitting */
  while (frame && (frame->max_use < frame->num_bindings)) {
    frame->max_use = frame->num_bindings;
    frame = frame->next;
  }
}

/*========================================================================*/
/*                          syntax-checking utils                         */
/*========================================================================*/

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where, Scheme_Object *form)
{
  if (!where)
    where = "";

  if (!SCHEME_STX_SYMBOLP(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"not an identifier%s", where);
}

void scheme_begin_dup_symbol_check(DupCheckRecord *r)
{
  r->count = 0;
}

void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form)
{
  int i;

  if (r->count <= 5) {
    for (i = 0; i < r->count; i++) {
      if (SAME_OBJ(SCHEME_STX_VAL(symbol), SCHEME_STX_VAL(r->syms[i])))
	scheme_wrong_syntax(where, symbol, form,
			    "duplicate %s name", what);
    }

    if (r->count < 5) {
      r->syms[r->count++] = symbol;
      return;
    } else {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      r->ht = ht;
      for (i = 0; i < r->count; i++) {
	scheme_hash_set(ht, SCHEME_STX_VAL(r->syms[i]), r->syms[i]);
      }
      r->count++;
    }
  }

  if (scheme_hash_get(r->ht, SCHEME_STX_VAL(symbol))) {
    scheme_wrong_syntax(where, symbol, form,
                        "duplicate %s name", what);
    return;
  }

  scheme_hash_set(r->ht, SCHEME_STX_VAL(symbol), symbol);
}


/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_compenv.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_comp_env, mark_comp_env);
}

END_XFORM_SKIP;

#endif
