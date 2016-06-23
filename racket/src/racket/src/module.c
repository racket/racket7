/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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

/* This file implements the first-order, top-level module system --
   both the expander and compiler front-end, as well as run-time
   support for modules. An initiantiated module is implemented
   essentially as a namespace. The bindings at the top level of a
   module are namespace top-level bindings. */

#include "schpriv.h"
#include "schmach.h"
#include "schexpobs.h"

#define mz_MIN(l,o) ((l) < (o) ? (l) : (o))

/* globals */
SHARED_OK Scheme_Object *(*scheme_module_demand_hook)(int, Scheme_Object **);
THREAD_LOCAL_DECL(Scheme_Bucket_Table *scheme_module_code_cache);

SHARED_OK static Scheme_Bucket_Table *modpath_table;
#ifdef MZ_USE_PLACES
SHARED_OK static mzrt_mutex *modpath_table_mutex;
#else
# define mzrt_mutex_lock(l) /* empty */
# define mzrt_mutex_unlock(l) /* empty */
#endif

/* locals */
static Scheme_Object *module_compiled_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_indirect_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_submodules(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_compiled_phaseless_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_namespace(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_lang_info(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_imports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_to_indirect_exports(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_is_declared(int argc, Scheme_Object *argv[]);
static Scheme_Object *module_is_predefined(int argc, Scheme_Object *argv[]);

static Scheme_Object *module_export_protected_p(int argc, Scheme_Object **argv);

/* syntax */
static Scheme_Object *module_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec);

static Scheme_Object *do_module_begin_at_phase(Scheme_Object *form, Scheme_Comp_Env *env, 
                                               Scheme_Compile_Expand_Info *rec, int drec,
                                               Scheme_Compile_Expand_Info *erec, int derec,
                                               int phase, Scheme_Object *body_lists,
                                               Module_Begin_Expand_State *bxs);
#define cons scheme_make_pair

/* global read-only kernel stuff */
READ_ONLY static Scheme_Object *kernel_modname;
READ_ONLY static Scheme_Object *kernel_symbol;
READ_ONLY static Scheme_Object *kernel_modidx;
READ_ONLY static Scheme_Module *kernel;
READ_ONLY static Scheme_Object *flfxnum_modname;
READ_ONLY static Scheme_Object *extfl_modname;
READ_ONLY static Scheme_Object *futures_modname;
READ_ONLY static Scheme_Object *unsafe_modname;
READ_ONLY static Scheme_Object *foreign_modname;

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, Scheme_Object *syntax_idx, 
                         int eval_exp, int eval_run, intptr_t base_phase, Scheme_Object *cycle_list,
                         int not_new);
static void eval_module_body(Scheme_Env *menv, Scheme_Env *env);

/**********************************************************************/
/*                           initialization                           */
/**********************************************************************/

void scheme_init_module(Scheme_Env *env)
{
  REGISTER_SO(kernel_symbol);
  REGISTER_SO(kernel_modname);
  REGISTER_SO(kernel_modidx);
  REGISTER_SO(unsafe_modname);
  REGISTER_SO(flfxnum_modname);
  REGISTER_SO(extfl_modname);
  REGISTER_SO(futures_modname);
  REGISTER_SO(foreign_modname);
  kernel_symbol = scheme_intern_symbol("#%kernel");
  kernel_modname = scheme_intern_resolved_module_path(kernel_symbol);
  kernel_modidx = scheme_make_modidx(scheme_make_pair(quote_symbol,
                                                      scheme_make_pair(kernel_symbol, 
                                                                       scheme_null)),
                                     scheme_false, kernel_modname);
  (void)scheme_hash_key(kernel_modidx);
  unsafe_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%unsafe"));
  flfxnum_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%flfxnum"));
  extfl_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%extfl"));
  futures_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%futures"));
  foreign_modname = scheme_intern_resolved_module_path(scheme_intern_symbol("#%foreign"));

  GLOBAL_PRIM_W_ARITY("compiled-module-expression?",      module_compiled_p,          1, 1, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-name",             module_compiled_name,       1, 2, env);
  GLOBAL_PRIM_W_ARITY("module-compiled-imports",          module_compiled_imports,    1, 1, env);
  GLOBAL_PRIM_W_ARITY2("module-compiled-exports",         module_compiled_exports,    1, 1, 2, 2, env);
  GLOBAL_PRIM_W_ARITY2("module-compiled-indirect-exports",module_compiled_indirect_exports, 1, 1, 2, 2, env);
}

void scheme_finish_kernel(Scheme_Env *env)
{
  /* When this function is called, the initial namespace has all the
     primitive bindings for syntax and procedures. This function fills
     in the module wrapper for #%kernel. */
  char *running;

  REGISTER_SO(kernel);

  kernel = MALLOC_ONE_TAGGED(Scheme_Module);
  kernel->so.type = scheme_module_type;
  kernel->predefined = 1;
  kernel->phaseless = scheme_true;
  env->module = kernel;

  {
    Scheme_Object *insp;
    insp = scheme_get_current_inspector();

    env->guard_insp = insp; /* nothing is protected, anyway */
    env->access_insp = insp;
    kernel->insp = insp;
  }

  kernel->modname = kernel_modname;
  kernel->modsrc = kernel_modname;
  kernel->requires = scheme_null;
  kernel->et_requires = scheme_null;
  kernel->tt_requires = scheme_null;
  kernel->dt_requires = scheme_null;
  kernel->other_requires = NULL;
  add_exp_infos(kernel);
  
  {
    Scheme_Bucket_Table *ht;
    int i, j, count, syntax_start = 0;
    Scheme_Bucket **bs;
    Scheme_Object **exs;
    /* Provide all syntax and variables: */
    count = 0;
    for (j = 0; j < 2; j++) {
      if (!j)
        ht = env->toplevel;
      else {
        ht = env->syntax;
        syntax_start = count;
      }

      bs = ht->buckets;
      for (i = ht->size; i--; ) {
        Scheme_Bucket *b = bs[i];
        if (b && b->val)
          count++;
      }
    }

    exs = MALLOC_N(Scheme_Object *, count);
    count = 0;
    for (j = 0; j < 2; j++) {
      if (!j)
        ht = env->toplevel;
      else
        ht = env->syntax;

      bs = ht->buckets;
      for (i = ht->size; i--; ) {
        Scheme_Bucket *b = bs[i];
        if (b && b->val)
          exs[count++] = (Scheme_Object *)b->key;
      }
    }

    {
      Scheme_Module_Exports *me;
      me = scheme_make_module_exports();
      kernel->me = me;
      kernel->me->modsrc = kernel_modname;
    }

    kernel->me->rt->provides = exs;
    kernel->me->rt->provide_srcs = NULL;
    kernel->me->rt->provide_src_names = exs;
    kernel->me->rt->num_provides = count;
    kernel->me->rt->num_var_provides = syntax_start;
    scheme_populate_pt_ht(kernel->me->rt);

    running = (char *)scheme_malloc_atomic(2);
    running[0] = 1;
    running[1] = 1;
    env->running = running;
    env->attached = 1;
  }
}

void scheme_init_syntax_bindings()
{
}

int scheme_is_kernel_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, kernel_modname);
}

int scheme_is_unsafe_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, unsafe_modname);
}

int scheme_is_flfxnum_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, flfxnum_modname);
}

int scheme_is_extfl_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, extfl_modname);
}

int scheme_is_futures_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, futures_modname);
}

int scheme_is_foreign_modname(Scheme_Object *modname)
{
  return SAME_OBJ(modname, foreign_modname);
}

Scheme_Module *get_special_module(Scheme_Object *name)
{
  if (SAME_OBJ(name, kernel_modname))
    return kernel;
  else if (SAME_OBJ(name, unsafe_modname))
    return scheme_get_unsafe_env()->module;
  else if (SAME_OBJ(name, flfxnum_modname))
    return scheme_get_flfxnum_env()->module;
  else if (SAME_OBJ(name, extfl_modname))
    return scheme_get_extfl_env()->module;
  else if (SAME_OBJ(name, futures_modname))
    return scheme_get_futures_env()->module;
  else if (SAME_OBJ(name, foreign_modname))
    return scheme_get_foreign_env()->module;
  else
    return NULL;
}

Scheme_Env *get_special_modenv(Scheme_Object *name)
{
  if (SAME_OBJ(name, kernel_modname))
    return scheme_get_kernel_env();
  else if (SAME_OBJ(name, flfxnum_modname))
    return scheme_get_flfxnum_env();
  else if (SAME_OBJ(name, extfl_modname))
    return scheme_get_extfl_env();
  else if (SAME_OBJ(name, futures_modname))
    return scheme_get_futures_env();
  else if (SAME_OBJ(name, unsafe_modname))
    return scheme_get_unsafe_env();
  else if (SAME_OBJ(name, foreign_modname))
    return scheme_get_foreign_env();
  else
    return NULL;
}

static int is_builtin_modname(Scheme_Object *modname) 
{
  return (SAME_OBJ(modname, kernel_modname)
          || SAME_OBJ(modname, unsafe_modname)
          || SAME_OBJ(modname, flfxnum_modname)
          || SAME_OBJ(modname, extfl_modname)
          || SAME_OBJ(modname, futures_modname)
          || SAME_OBJ(modname, foreign_modname));
}

/**********************************************************************/
/*                      linklets and instances                        */
/**********************************************************************/

/* A minimal linklet API to support bootstrapping. */

static Scheme_Object *get_primitive_instance(int argc, Scheme_Object *argv[]);
static Scheme_Object *instance_variable_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object *argv[]);

void scheme_init_linklet(Scheme_Env *env)
{
  Scheme_Env *newenv;
  Scheme_Object *modname;

  modname = scheme_intern_symbol("#%linklet");
  newenv = scheme_primitive_module(modname, env);

  GLOBAL_PRIM_W_ARITY("get-primitive-instance", get_primitive_instance, 1, 2, newenv);
  GLOBAL_PRIM_W_ARITY("instance-variable-value", instance_variable_value, 2, 2, newenv);
  GLOBAL_PRIM_W_ARITY("instance-set-variable-value!", instance_set_variable_value, 3, 3, newenv);
  
  scheme_finish_primitive_module(newenv);
  scheme_protect_primitive_provide(newenv, NULL);
}

static Scheme_Object *get_primitive_instance(int argc, Scheme_Object *argv[])
{
  Scheme_Env *env, *menv;
  Scheme_Object *name;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("get-primitive-instance", "symbol?", 0, argc, argv);

  name = scheme_intern_resolved_module_path(argv[0]);

  env = scheme_get_env(NULL);
  menv = get_special_modenv(name);
  if (!menv)
    menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), name);
  if (!menv && (argc > 1) && SCHEME_TRUEP(argv[1])) {
    menv = scheme_primitive_module(argv[0], env);
    scheme_finish_primitive_module(menv);

    start_module(menv->module, env, 0, name, 0, 1, 0, scheme_null, 0);
  }

  return (menv ? (Scheme_Object *)menv : scheme_false);
}

static Scheme_Object *instance_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_contract("instance-variable-value", "namespace?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-variable-value", "symbol?", 1, argc, argv);

  v = scheme_lookup_global(argv[1], (Scheme_Env *)argv[0]);

  return (v ? v : scheme_false);
}

static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object *argv[])
{
  Scheme_Bucket *bucket;

  if (!SCHEME_NAMESPACEP(argv[0]))
    scheme_wrong_contract("instance-set-variable-value!", "namespace?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-set-variable-value!", "symbol?", 1, argc, argv);

  bucket = scheme_global_bucket(argv[1], (Scheme_Env *)argv[0]);
  scheme_set_global_bucket("instance-set-variable-value!", bucket, argv[2], 1);

  return scheme_void;
}

/**********************************************************************/
/*                            procedures                              */
/**********************************************************************/

int scheme_is_predefined_module_p(Scheme_Object *name)
{
  Scheme_Object *a[1];
  Scheme_Module *m;

  a[0] = name;
  m = module_to_("module-predefined?", 1, a, 1);
  
  return m && m->predefined;
}

static Scheme_Object *extract_compiled_imports(Scheme_Module *m)
{
  Scheme_Object *l;
  int i;

  l = scheme_null;
  if (!SCHEME_NULLP(m->requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(0),
                                          m->requires),
                         l);
  if (!SCHEME_NULLP(m->et_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(1),
                                          m->et_requires),
                         l);
  if (!SCHEME_NULLP(m->tt_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_make_integer(-1),
                                          m->tt_requires),
                         l);
  if (!SCHEME_NULLP(m->dt_requires))
    l = scheme_make_pair(scheme_make_pair(scheme_false,
                                          m->dt_requires),
                         l);

  if (m->other_requires) {
    for (i = 0; i < m->other_requires->size; i++) {
      if (m->other_requires->vals[i]) {
        l = scheme_make_pair(scheme_make_pair(m->other_requires->keys[i],
                                              m->other_requires->vals[i]),
                             l);
      }
    }
  }
    
  return l;
}

static Scheme_Object *make_provide_desc(Scheme_Module_Phase_Exports *pt, int i)
{
  return scheme_make_pair(pt->provides[i],
                          scheme_make_pair((pt->provide_nominal_srcs
                                            ? pt->provide_nominal_srcs[i]
                                            : scheme_null),
                                           scheme_null));
}

static Scheme_Object *extract_compiled_exports(Scheme_Module *m)
{
  Scheme_Object *a[2];
  Scheme_Object *ml, *vl, *val_l, *mac_l;
  Scheme_Module_Phase_Exports *pt;
  int i, n, k;

  val_l = scheme_null;
  mac_l = scheme_null;

  for (k = -3; k < (m->me->other_phases ? m->me->other_phases->size : 0); k++) {
    switch(k) {
    case -3:
      pt = m->me->rt;
      break;
    case -2:
      pt = m->me->et;
      break;
    case -1:
      pt = m->me->dt;
      break;
    default:
      pt = (Scheme_Module_Phase_Exports *)m->me->other_phases->vals[k];
      break;
    }

    if (pt) {
      ml = scheme_null;
      vl = scheme_null;
      n = pt->num_var_provides;
      for (i = pt->num_provides - 1; i >= n; --i) {
        ml = scheme_make_pair(make_provide_desc(pt, i), ml);
      }
      for (; i >= 0; --i) {
        vl = scheme_make_pair(make_provide_desc(pt, i), vl);
      }

      if (!SCHEME_NULLP(vl))
        val_l = scheme_make_pair(scheme_make_pair(pt->phase_index, vl), 
                                 val_l);

      if (!SCHEME_NULLP(ml))
        mac_l = scheme_make_pair(scheme_make_pair(pt->phase_index, ml),
                                 mac_l);
    }
  }
    
  a[0] = val_l;
  a[1] = mac_l;
  return scheme_values(2, a);
}

static Scheme_Object *extract_compiled_indirect_exports(Scheme_Module *m)
{
  int k, i;
  Scheme_Object *l, *a;
  Scheme_Module_Export_Info *ei;

  l = scheme_null;

  for (k = m->num_phases; k--; ) {
    ei = m->exp_infos[k];
    if (ei && ei->num_indirect_provides) {
      a = scheme_null;
      for (i = ei->num_indirect_provides; i--; ) {
        a = scheme_make_pair(ei->indirect_provides[i], a);
      }
      a = scheme_make_pair(scheme_make_integer(k), a);
      l = scheme_make_pair(a, l);
    }
  }

  return l;
}

static int is_procedure_expression(Scheme_Object *e)
{
  Scheme_Type t;

  if (SCHEME_PROCP(e))
    return 1;

  t = SCHEME_TYPE(e);

  return ((t == scheme_lambda_type)
          || (t == scheme_case_lambda_sequence_type));
}

static void get_procedure_shape(Scheme_Object *e, Scheme_Object **_c)
{
  Scheme_Object *p, *v;

  p = scheme_get_or_check_procedure_shape(e, NULL);

  v = scheme_alloc_small_object();
  v->type = scheme_proc_shape_type;
  SCHEME_PTR_VAL(v) = p;

  *_c = v;
}

static void setup_accessible_table(Scheme_Module *m)
{
  if (!m->exp_infos[0]->accessible) {
    Scheme_Module_Phase_Exports *pt;
    int j;

    for (j = 0; j < m->num_phases; j++) {
      if (!j)
        pt = m->me->rt;
      else if (j == 1)
        pt = m->me->et;
      else {
        if (m->me->other_phases)
          pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(m->me->other_phases,
                                                              scheme_make_integer(j));
        else
          pt = NULL;
      }
      
      if (pt) {
        Scheme_Hash_Table *ht;
        int i, count, nvp;
        
        ht = scheme_make_hash_table(SCHEME_hash_ptr);
        nvp = pt->num_var_provides;
        for (i = 0; i < nvp; i++) {
          if (SCHEME_FALSEP(pt->provide_srcs[i])) {
            scheme_hash_set(ht, pt->provide_src_names[i], scheme_make_integer(i));
          }
        }
        
        count = m->exp_infos[j]->num_indirect_provides;
        for (i = 0; i < count; i++) {
          scheme_hash_set(ht, m->exp_infos[j]->indirect_provides[i], scheme_make_integer(i + nvp));
        }
        
        /* Add syntax as negative ids: */
        count = pt->num_provides;
        for (i = nvp; i < count; i++) {
          if (SCHEME_FALSEP(pt->provide_srcs[i]))
            scheme_hash_set(ht, pt->provide_src_names[i], scheme_make_integer(-(i+1)));
        }

        if (!j) {
          /* find constants: */
          int i, cnt = SCHEME_VEC_SIZE(m->bodies[0]), k;
          Scheme_Object *form, *tl;

          for (i = 0; i < cnt; i++) {
            form = SCHEME_VEC_ELS(m->bodies[0])[i];
            if (SAME_TYPE(SCHEME_TYPE(form), scheme_define_values_type)) {
              int checked_st = 0, is_st = 0;
              Simple_Stuct_Type_Info stinfo;
              for (k = SCHEME_VEC_SIZE(form); k-- > 1; ) {
                tl = SCHEME_VEC_ELS(form)[k];
                if (SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_SEAL) {
                  int pos = SCHEME_TOPLEVEL_POS(tl);
                  if (pos < m->prefix->num_toplevels) {
                    tl = m->prefix->toplevels[pos];
                    if (SCHEME_SYMBOLP(tl)) {
                      Scheme_Object *v;
                      v = scheme_hash_get(ht, tl);
                      if (!v) { 
                        /* The defined name is inaccessible. The bytecode compiler
                           won't generate such modules, but synthesized module bytecode
                           might leave bindings out of the `toplevels' table. */
                      } else {
                        if (SCHEME_VEC_SIZE(form) == 2) {
                          if (scheme_ir_duplicate_ok(SCHEME_VEC_ELS(form)[0], 1)) {
                            /* record simple constant from cross-module propagation: */
                            v = scheme_make_pair(v, SCHEME_VEC_ELS(form)[0]);
                          } else if (SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(form)[0]), scheme_inline_variant_type)) {
                            /* record a potentially inlineable function */
                            if (SCHEME_VEC_ELS(SCHEME_VEC_ELS(form)[0])[2] != (Scheme_Object *)m->prefix)
                              SCHEME_VEC_ELS(SCHEME_VEC_ELS(form)[0])[2] = (Scheme_Object *)m->prefix;
                            v = scheme_make_pair(v, SCHEME_VEC_ELS(form)[0]);
                          } else if (is_procedure_expression(SCHEME_VEC_ELS(form)[0])) {
                            /* that it's a procedure: */
                            v = scheme_make_vector(2, v);
                            SCHEME_VEC_ELS(v)[1] = SCHEME_VEC_ELS(form)[0];
                          } else {
                            /* record that it's fixed for any given instantiation: */
                            v = scheme_make_pair(v, scheme_fixed_key);
                          }
                        } else {
                          if (!checked_st) {
                            is_st = !!scheme_is_simple_make_struct_type(SCHEME_VEC_ELS(form)[0],
                                                                        SCHEME_VEC_SIZE(form)-1,
                                                                        1, 0, 1, NULL, &stinfo,
                                                                        NULL, NULL, NULL, 0,
                                                                        m->prefix->toplevels, ht,
                                                                        5);
                            checked_st = 1;
                          }
                          if (is_st) {
                            intptr_t shape;
                            shape = scheme_get_struct_proc_shape(k-1, &stinfo);
                            v = scheme_make_vector(3, v);
                            SCHEME_VEC_ELS(v)[1] = scheme_make_integer(shape);
                          }
                        }
                        scheme_hash_set(ht, tl, v);
                      }
                    } else
                      scheme_signal_error("internal error: strange defn target %d", SCHEME_TYPE(tl));
                  }
                }
              }
            }
          }
        }

        m->exp_infos[j]->accessible = ht;
      }
    }
  }
}

/**********************************************************************/
/*                          running modules                           */
/**********************************************************************/

static void start_module(Scheme_Module *m, Scheme_Env *env, int restart, 
                         Scheme_Object *syntax_idx, int eval_exp, int eval_run, intptr_t base_phase,
                         Scheme_Object *cycle_list, int not_new)
/* Make an instance of module `m' in `env', which means that phase level 0 of module `m'
   will be shifted to phase `env->phase'.
   Let P=`base_phase'-`env->phase'. 
    - If `eval_run', then instantiate phase-level P of `m' (which is at `base_phase' in `env').
    - If `eval_exp' is -1, then (also) make its P+1 phase-level ready.
    - If `eval_exp' is 1, then visit at phase P => run phase P+1. */
{
  Scheme_Env *menv;
  Scheme_Object *l;
  int prep_namespace = 0, i;

  if (is_builtin_modname(m->modname))
    return;

  for (l = cycle_list; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (SAME_OBJ(m->modname, SCHEME_CAR(l))) {
      scheme_contract_error("module",
                            "import cycle detected",
                            "module in cycle", 1, scheme_get_modsrc(m),
                            NULL);
    }
  }

  menv = instantiate_module(m, env, restart, syntax_idx, not_new);

  check_phase(menv, env, 0);

  show("chck", menv, eval_exp, eval_run, 0, base_phase);

  if (did_start(menv->did_starts, base_phase, eval_exp, eval_run))
    return;
  
  show("strt", menv, eval_exp, eval_run, 0, base_phase);
  show_indent(+1);

  {
    Scheme_Object *v;
    v = add_start(menv->did_starts, base_phase, eval_exp, eval_run);
    menv->did_starts = v;
  }

  chain_start_module_w_push(menv, env, eval_exp, eval_run, base_phase, cycle_list, syntax_idx);

  if (restart) {
    if (menv->rename_set_ready) {
      menv->rename_set_ready = 0;
      prep_namespace = 1;
    }
  }
}

static void *eval_module_body_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Env *menv, *env;

  menv = (Scheme_Env *)p->ku.k.p1;
  env = (Scheme_Env *)p->ku.k.p2;
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  eval_module_body(menv, env);
  
  return NULL;
}

#if 0
# define LOG_RUN_DECLS intptr_t start_time
# define LOG_START_RUN(mod) (start_time = scheme_get_process_milliseconds())
# define LOG_END_RUN(mod) (printf("Ran %s [%d msec]\n", \
                                  scheme_write_to_string(mod->modname, NULL), \
                                  scheme_get_process_milliseconds() - start_time))
#else
# define LOG_RUN_DECLS /* empty */
# define LOG_START_RUN(mod) /* empty */
# define LOG_END_RUN(mod) /* empty */
#endif

static void eval_module_body(Scheme_Env *menv, Scheme_Env *env)
{
  if (menv->module->phaseless) {
    /* Phaseless modules are implemented by last-minute sharing of the
       `toplevels' table. In principle, much more repeated work up to
       this point could be skipped, but this is the simplest point to
       implement the sharing. */
    if (SAME_OBJ(scheme_true, menv->module->phaseless)) {
      menv->module->phaseless = (Scheme_Object *)menv->toplevel;
    } else {
      menv->toplevel = (Scheme_Bucket_Table *)menv->module->phaseless;
      return;
    }
  }

#ifdef MZ_USE_JIT
  (void)scheme_module_run_start(menv, env, scheme_make_pair(scheme_get_modsrc(menv->module), scheme_true));
#else
  (void)scheme_module_run_finish(menv, env);
#endif
}

static Scheme_Object *body_one_expr(void *prefix_plus_expr, int argc, Scheme_Object **argv)
{
  Scheme_Object *v, **saved_runstack;

  saved_runstack = scheme_resume_prefix(SCHEME_CAR((Scheme_Object *)prefix_plus_expr));
  v = _scheme_eval_linked_expr_multi(SCHEME_CDR((Scheme_Object *)prefix_plus_expr));
  scheme_suspend_prefix(saved_runstack);

  scheme_ignore_result(v);

  return scheme_void;
}

static int needs_prompt(Scheme_Object *e)
{
  Scheme_Type t;
  
  while (1) {
    t = SCHEME_TYPE(e);
    if (t > _scheme_values_types_)
      return 0;
  
    switch (t) {
    case scheme_lambda_type:
    case scheme_toplevel_type:
    case scheme_local_type:
    case scheme_local_unbox_type:
      return 0;
    case scheme_case_lambda_sequence_type:
      return 0;
    case scheme_define_values_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    case scheme_inline_variant_type:
      e = SCHEME_VEC_ELS(e)[0];
      break;
    default:
      return 1;
    }
  }
}

void *scheme_module_run_finish(Scheme_Env *menv, Scheme_Env *env)
{
  Scheme_Thread *p;
  Scheme_Module *m = menv->module;
  Scheme_Object *body, **save_runstack, *save_prefix;
  int depth;
  int i, cnt;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  int volatile save_phase_shift;
  mz_jmp_buf newbuf, * volatile savebuf;
  LOG_RUN_DECLS;

  menv->running[0] = 1;
  menv->ran = 1;

  depth = m->max_let_depth + scheme_prefix_depth(m->prefix);
  if (!scheme_check_runstack(depth)) {
    p = scheme_current_thread;
    p->ku.k.p1 = menv;
    p->ku.k.p2 = env;
    (void)scheme_enlarge_runstack(depth, eval_module_body_k);
    return NULL;
  }

  LOG_START_RUN(menv->module);

  save_runstack = scheme_push_prefix(menv, 0, m->prefix,
				     m->me->src_modidx, menv->link_midx,
				     0, menv->phase, NULL,
                                     menv->access_insp);

  p = scheme_current_thread;
  save_phase_shift = p->current_phase_shift;
  p->current_phase_shift = menv->phase;
  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2;
    p2 = scheme_current_thread;
    p2->error_buf = savebuf;
    p2->current_phase_shift = save_phase_shift;
    scheme_longjmp(*savebuf, 1);
  } else {
    if (env && menv->phase) {
      config = scheme_extend_config(scheme_current_config(),
                                    MZCONFIG_ENV,
                                    (Scheme_Object *)env);
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
    }

    cnt = SCHEME_VEC_SIZE(m->bodies[0]);
    for (i = 0; i < cnt; i++) {
      body = SCHEME_VEC_ELS(m->bodies[0])[i];
      if (needs_prompt(body)) {
        /* We need to push the prefix after the prompt is set, so
           restore the runstack and then add the prefix back. */
        save_prefix = scheme_suspend_prefix(save_runstack);
        (void)_scheme_call_with_prompt_multi(body_one_expr, 
                                             scheme_make_raw_pair(save_prefix, body));
        scheme_resume_prefix(save_prefix);

        /* Double-check that the definition-installing part of the
           continuation was not skipped. Otherwise, the compiler would
           not be able to assume that a variable reference that is
           lexically later (incuding a reference to an imported
           variable) always references a defined variable. Putting the
           prompt around a definition's RHS might be a better
           approach, but that would change the language (so mabe next
           time). */
        if (SAME_TYPE(SCHEME_TYPE(body), scheme_define_values_type)) {
          int vcnt, j;
          
          vcnt = SCHEME_VEC_SIZE(body) - 1;
          for (j = 0; j < vcnt; j++) {
            Scheme_Object *var;
            Scheme_Prefix *toplevels;
            Scheme_Bucket *b;
            
            var = SCHEME_VEC_ELS(body)[j+1];
            toplevels = (Scheme_Prefix *)MZ_RUNSTACK[SCHEME_TOPLEVEL_DEPTH(var)];
            b = (Scheme_Bucket *)toplevels->a[SCHEME_TOPLEVEL_POS(var)];
            
            if (!b->val) {
              scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE, 
                               b->key,
                               "define-values: skipped variable definition;\n"
                               " cannot continue without defining variable\n"
                               "  variable: %S\n"
                               "  in module: %D",
                               (Scheme_Object *)b->key,
                               menv->module->modsrc);
            }
          }
        }
      } else
        scheme_ignore_result(_scheme_eval_linked_expr_multi(body));
    }

    if (scheme_module_demand_hook) {
      Scheme_Object *a[1], *val, *sym;
      a[0] = menv->module->modname;
      sym = scheme_module_demand_hook(1, a);
      if (sym) {
        val = scheme_lookup_global(sym, menv);
        if (val) {
          a[0] = val;
          val = scheme_module_demand_hook(3, a);
          if (val) {
            scheme_add_global_symbol(sym, val, menv);
          }
        }
      }
    }

    if (env && menv->phase) {
      scheme_pop_continuation_frame(&cframe);
    }

    p = scheme_current_thread;
    p->error_buf = savebuf;
    p->current_phase_shift = save_phase_shift;

    scheme_pop_prefix(save_runstack);
  }

  LOG_END_RUN(menv->module);

  return NULL;
}

static void run_module(Scheme_Env *menv, int set_ns)
{
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  if (set_ns) {
    config = scheme_extend_config(scheme_current_config(),
                                  MZCONFIG_ENV,
                                  (Scheme_Object *)menv);
    
    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
  }
  
  eval_module_body(menv, NULL);

  if (set_ns) {
    scheme_pop_continuation_frame(&cframe);
  }
  
}

Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env)
{
  Scheme_Module *m;
  Scheme_Env *env;
  Scheme_Object *prefix, *insp, *src, *midx;
  Scheme_Config *config;
  char *running;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  m->predefined = scheme_starting_up;
  m->phaseless = scheme_true;
  
  env = scheme_new_module_env(for_env, m, 0, 0);

  if (!scheme_defining_primitives) {
    config = scheme_current_config();
    prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_NAME);
    if (SCHEME_MODNAMEP(prefix))
      name = prefix;
    else
      name = scheme_intern_resolved_module_path(name);
    src = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_SRC);
    if (SCHEME_FALSEP(src))
      src = prefix;
    else
      src = scheme_intern_resolved_module_path(src);
    if (SCHEME_FALSEP(src))
      src = name;
    insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
  }
  else {
    name = scheme_intern_resolved_module_path(name);
    src = name;
    insp = scheme_get_current_inspector();
  }

  m->modname = name;
  m->modsrc = src;
  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->tt_requires = scheme_null;
  m->dt_requires = scheme_null;
  m->primitive = env;
  m->insp = insp;
  
  midx = scheme_make_modidx(scheme_false, scheme_false, name);
  m->self_modidx = midx;

  {
    Scheme_Module_Exports *me;
    me = scheme_make_module_exports();
    m->me = me;
    me->modsrc = src;
  }

  scheme_hash_set(for_env->module_registry->exports, m->modname, (Scheme_Object *)m->me);

  env->access_insp = insp;
  insp = scheme_make_inspector(insp);
  env->guard_insp = insp;

  scheme_hash_set(for_env->module_registry->loaded, m->modname, (Scheme_Object *)m);

  running = scheme_malloc_atomic(2);
  running[0] = 1;
  running[1] = 1;
  env->running = running;

  return env;
}

void scheme_set_primitive_module_phaseless(Scheme_Env *env, int phaseless)
{
  env->module->phaseless = (phaseless ? scheme_true : NULL); 
}

void scheme_finish_primitive_module(Scheme_Env *env)
{
  Scheme_Module *m = env->module;
  Scheme_Bucket_Table *ht;
  Scheme_Bucket **bs;
  Scheme_Object **exs;
  int i, count;

  if (!m->exp_infos)
    add_exp_infos(m);

  /* Provide all variables: */
  count = 0;
  ht = env->toplevel;

  bs = ht->buckets;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      count++;
  }

  exs = MALLOC_N(Scheme_Object *, count);
  count = 0;
  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && b->val)
      exs[count++] = (Scheme_Object *)b->key;
  }
 
  m->me->rt->provides = exs;
  m->me->rt->provide_srcs = NULL;
  m->me->rt->provide_src_names = exs;
  m->me->rt->num_provides = count;
  m->me->rt->num_var_provides = count;

  qsort_provides(exs, NULL, NULL, NULL, NULL, NULL, 0, count, 1);

  env->running[0] = 1;
}

Scheme_Bucket *scheme_module_bucket(Scheme_Object *modname, Scheme_Object *var, int pos, Scheme_Env *env)
{
  Scheme_Object *a[2];

  if (SAME_OBJ(modname, kernel_symbol))
    a[0] = ((Scheme_Modidx *)kernel_modidx)->path;
  else
    a[0] = modname;
  a[1] = var;

  return (Scheme_Bucket *)_dynamic_require(2, a, env, 1, 0, 0, 1, 1, pos);
}

Scheme_Object *scheme_builtin_value(const char *name)
{
  Scheme_Object *a[2], *v;

  a[1] = scheme_intern_symbol(name);

  /* Try kernel first: */
  a[0] = kernel_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try flfxnum next: */
  a[0] = flfxnum_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try extfl next: */
  a[0] = extfl_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Try unsafe next: */
  a[0] = unsafe_modname;
  v = _dynamic_require(2, a, scheme_get_env(NULL), 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  /* Also try #%utils... */
  a[0] = scheme_make_pair(quote_symbol,
                          scheme_make_pair(scheme_intern_symbol("#%utils"),
                                           scheme_null));
  v = _dynamic_require(2, a, initial_modules_env, 0, 0, 0, 0, 0, -1);
  if (v)
    return v;

  return NULL;
}

Scheme_Module *scheme_extract_compiled_module(Scheme_Object *o)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *c = (Scheme_Compilation_Top *)o;

    if (!c->prefix) /* => compiled module is in `code' field */
      return (Scheme_Module *)c->code;
    
    if (SAME_TYPE(SCHEME_TYPE(c->code), scheme_module_type)) {
      return (Scheme_Module *)c->code;
    }
  }

  return NULL;
}

Scheme_Module_Exports *scheme_make_module_exports()
{
  Scheme_Module_Exports *me;
  Scheme_Module_Phase_Exports *pt;

  me = MALLOC_ONE_RT(Scheme_Module_Exports);
  SET_REQUIRED_TAG(me->type = scheme_rt_module_exports);

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_make_integer(0);
  me->rt = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_make_integer(1);
  me->et = pt;

  pt = MALLOC_ONE_RT(Scheme_Module_Phase_Exports);
  pt->so.type = scheme_module_phase_exports_type;
  pt->phase_index = scheme_false;
  me->dt = pt;

  return me;
}

/**********************************************************************/
/*                               module                               */
/**********************************************************************/

static Scheme_Object *do_module_execute(Scheme_Object *data, Scheme_Env *genv, 
                                        int set_cache, int set_in_pre, 
                                        Scheme_Object *prefix,
                                        Scheme_Object *supermodule);

static Scheme_Object *do_module_execute_k()
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *data = (Scheme_Object *)p->ku.k.p1;
  Scheme_Env *genv = (Scheme_Env *)p->ku.k.p2;
  Scheme_Object *prefix = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *supermodule = (Scheme_Object *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return do_module_execute(data, genv, p->ku.k.i1, p->ku.k.i2, prefix, supermodule);
}

static Scheme_Object *do_module_execute_recur(Scheme_Object *data, Scheme_Env *genv, 
                                              int set_cache, int set_in_pre, 
                                              Scheme_Object *prefix,
                                              Scheme_Object *supermodule)
{
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;
    p->ku.k.p1 = (void *)data;
    p->ku.k.p2 = (void *)genv;
    p->ku.k.i1 = set_cache;
    p->ku.k.i2 = set_in_pre;
    p->ku.k.p3 = (void *)prefix;
    p->ku.k.p4 = (void *)supermodule;
    return scheme_handle_stack_overflow(do_module_execute_k);
  } else {
    return do_module_execute(data, genv, set_cache, set_in_pre, prefix, supermodule);
  }
}

static Scheme_Object *do_module_execute(Scheme_Object *data, Scheme_Env *genv, 
                                        int set_cache, int set_in_pre, 
                                        Scheme_Object *prefix,
                                        Scheme_Object *supermodule)
{
  Scheme_Module *m, *old_m;
  Scheme_Env *env;
  Scheme_Env *old_menv;
  Scheme_Config *config;
  Scheme_Object *src, *insp;

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));

  if (set_cache && m->code_key
      && (!m->pre_submodules || SCHEME_NULLP(m->pre_submodules))
      && (!m->post_submodules || SCHEME_NULLP(m->post_submodules))) {
    if (!scheme_module_code_cache) {
      REGISTER_SO(scheme_module_code_cache);
      scheme_module_code_cache = scheme_make_weak_equal_table();
    }
    scheme_add_to_table(scheme_module_code_cache,
                        (const char *)m->code_key,
                        scheme_make_ephemeron(m->code_key, data),
                        0);
  }

  if (m->code_key) {
    /* clone `requires', etc., so that different uses of the cached
       module don't share resolution of modiule paths in modidxs */
    clone_all_require_names(m);
  }

  config = scheme_current_config();

  if (!prefix)
    prefix = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_NAME);
  
  if (SCHEME_MODNAMEP(prefix)) {
    if (m->submodule_path && !SCHEME_NULLP(m->submodule_path)) {
      prefix = scheme_make_pair(scheme_resolved_module_path_value(prefix),
                                m->submodule_path);
      prefix = scheme_intern_resolved_module_path(prefix);
    }

    m->modname = prefix;
    
    if (m->self_modidx) {
      if (!SCHEME_SYMBOLP(m->self_modidx)) {
	Scheme_Modidx *midx = (Scheme_Modidx *)m->self_modidx;
	Scheme_Object *nmidx;

	nmidx = scheme_make_modidx(midx->path, midx->base, m->modname);
	m->self_modidx = nmidx;

	if (m->rn_stx && !SAME_OBJ(scheme_true, m->rn_stx)) {
	  /* Delay the shift: */
	  Scheme_Object *v;
          v = m->rn_stx;
	  v = scheme_make_pair(v, (Scheme_Object *)midx);
	  m->rn_stx = v;
	}
      }
    }
  } else
    prefix = m->modname; /* used for submodules */

  /* printf("declare %s\n", scheme_write_to_string(m->modname, NULL)); */

  src = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_SRC);
  if (!SCHEME_FALSEP(src)) {
    src = scheme_intern_resolved_module_path(src);
    m->modsrc = src;
  } else {
    src = m->modname;
    if (m->submodule_path && !SCHEME_NULLP(m->submodule_path)) {
      src = scheme_resolved_module_path_value(src);
      if (SCHEME_PAIRP(src))
        src = SCHEME_CAR(src);
      src = scheme_intern_resolved_module_path(src);
    }
    m->modsrc = src;
  }

  if (supermodule)
    m->supermodule = supermodule;

  if (genv)
    env = genv;
  else
    env = scheme_environment_from_dummy(m->dummy);

  old_menv = get_special_modenv(m->modname);
  if (!old_menv)
    old_menv = (Scheme_Env *)scheme_hash_get(MODCHAIN_TABLE(env->modchain), m->modname);

  insp = scheme_get_param(config, MZCONFIG_CODE_INSPECTOR);
  
  if (old_menv) {
    if (scheme_module_protected_wrt(old_menv->guard_insp, insp) || old_menv->attached) {
      scheme_contract_error("module->namespace",
                            "current code inspector cannot redeclare module",
                            "module name", 1, m->modname,
                            NULL);
      return NULL;
    }
  }

  if (old_menv)
    old_m = old_menv->module;
  else
    old_m = (Scheme_Module *)scheme_hash_get(env->module_registry->loaded, m->modname);
  
  if (old_m && old_m->phaseless) {
    scheme_contract_error("module->namespace",
                          "cannot redeclare cross-phase persistent module",
                          "module name", 1, m->modname,
                          NULL);
    return NULL;
  }

  if (!set_in_pre) {
    /* execute pre-submodules: */
    execute_submodules(m, 1, genv, set_cache, set_in_pre, prefix);
  }

  if (!SAME_OBJ(m->me->modsrc, m->modsrc)) {
    /* have to clone m->me, etc. */
    Scheme_Module_Exports *naya_me;

    naya_me = MALLOC_ONE_TAGGED(Scheme_Module_Exports);
    memcpy(naya_me, m->me, sizeof(Scheme_Module_Exports));
    m->me = naya_me;
    m->me->modsrc = m->modsrc;
  }

  m->insp = insp;
  if (set_in_pre) {
    if (!env->module_pre_registry->loaded) {
      Scheme_Hash_Table *ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      env->module_pre_registry->loaded = ht;
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      MZ_OPT_HASH_KEY(&(ht->iso)) |= 0x1; /* print (for debugging) as opqaue */
      env->module_pre_registry->exports = ht;
    }
    scheme_hash_set(env->module_pre_registry->loaded, m->modname, (Scheme_Object *)m);
    scheme_hash_set(env->module_pre_registry->exports, m->modname, (Scheme_Object *)m->me);
  } else {
    scheme_hash_set(env->module_registry->loaded, m->modname, (Scheme_Object *)m);
    scheme_hash_set(env->module_registry->exports, m->modname, (Scheme_Object *)m->me);
  }

  if (!set_in_pre) {
    Scheme_Object *resolver, *a[2];
    resolver = scheme_get_param(config, MZCONFIG_CURRENT_MODULE_RESOLVER);
    a[0] = m->modname;
    a[1] = scheme_false;
    scheme_apply(resolver, 2, a);
  }

  /* Replacing an already-running or already-syntaxing module? */
  if (old_menv) {
    old_menv->interactive_bindings = 1;
    start_module(m, env, 1, NULL, 
                 ((m->num_phases > 1) ? old_menv->running[1] : 0), 
                 old_menv->running[0], 
                 env->phase, scheme_null, 1);
  }

  /* execute post-submodules: */
  execute_submodules(m, 0, genv, set_cache, set_in_pre, prefix);

  return scheme_void;
}

Scheme_Object *scheme_module_execute(Scheme_Object *data, Scheme_Env *genv)
{
  return do_module_execute(data, genv, 1, 0, NULL, NULL);
}

Scheme_Object *scheme_get_modsrc(Scheme_Module *mod)
{
  Scheme_Object *p, *p2;

  p = scheme_resolved_module_path_value(mod->modname);
  if (SCHEME_PAIRP(p)) {
    /* Construct a submodule path based on `modsrc` instead of `modname`. */
    p2 = scheme_resolved_module_path_value(mod->modsrc);
    if (SAME_OBJ(SCHEME_CAR(p), p2))
      return mod->modname;
    else
      return scheme_intern_resolved_module_path(scheme_make_pair(p2, SCHEME_CDR(p)));
  } else
    return mod->modsrc;
}

static Scheme_Object *rebuild_et_vec(Scheme_Object *naya, Scheme_Object *vec, Resolve_Prefix *rp)
{
  Scheme_Object *vec2;
  int i;
  
  i = SCHEME_VEC_SIZE(vec);
  vec2 = scheme_make_vector(i, NULL);
  while (i--) {
    SCHEME_VEC_ELS(vec2)[i] = SCHEME_VEC_ELS(vec)[i];
  }
  SCHEME_VEC_ELS(vec2)[1] = naya;
  SCHEME_VEC_ELS(vec2)[3] = (Scheme_Object *)rp;

  return vec2;
}

static Scheme_Object *jit_vector(Scheme_Object *orig_l, int in_vec, int jit)
{
  Scheme_Object *orig, *naya = NULL;
  Resolve_Prefix *orig_rp, *rp;
  int i, cnt;

  cnt = SCHEME_VEC_SIZE(orig_l);
  for (i = 0; i < cnt; i++) {
    orig = SCHEME_VEC_ELS(orig_l)[i];
    if (in_vec) {
      orig_rp = (Resolve_Prefix *)SCHEME_VEC_ELS(orig)[3];
      rp = scheme_prefix_eval_clone(orig_rp);
      orig = SCHEME_VEC_ELS(orig)[1];
    } else {
      orig_rp = rp = NULL;
    }

    if (jit)
      naya = scheme_jit_expr(orig);
    else
      naya = orig;

    if (!SAME_OBJ(orig, naya)
        || !SAME_OBJ(orig_rp, rp))
      break;
  }

  if (i < cnt) {
    Scheme_Object *new_l;
    int j;
    new_l = scheme_make_vector(cnt, NULL);
    for (j = 0; j < i; j++) {
      SCHEME_VEC_ELS(new_l)[j] = SCHEME_VEC_ELS(orig_l)[j];
    }
    if (in_vec)
      naya = rebuild_et_vec(naya, SCHEME_VEC_ELS(orig_l)[i], rp);
    SCHEME_VEC_ELS(new_l)[i] = naya;
    for (i++; i < cnt; i++) {
      orig = SCHEME_VEC_ELS(orig_l)[i];
      if (in_vec) {
        orig_rp = (Resolve_Prefix *)SCHEME_VEC_ELS(orig)[3];
        rp = scheme_prefix_eval_clone(orig_rp);
        orig = SCHEME_VEC_ELS(orig)[1];        
      } else {
        orig_rp = rp = NULL;
      }

      if (jit)
        naya = scheme_jit_expr(orig);
      else
        naya = orig;

      if (in_vec) {
	if (!SAME_OBJ(orig, naya)
            || !SAME_OBJ(rp, orig_rp))
	  naya = rebuild_et_vec(naya, SCHEME_VEC_ELS(orig_l)[i], rp);
	else
	  naya = SCHEME_VEC_ELS(orig_l)[i];
      }
      SCHEME_VEC_ELS(new_l)[i] = naya;
    }
    return new_l;
  } else
    return orig_l;
}

static Scheme_Object *do_module_clone(Scheme_Object *data, int jit)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *l1, *l2, *pre_submods, *post_submods, *sm, **naya = NULL;
  int j, i, submod_changed;
  Resolve_Prefix *rp;
  
  rp = scheme_prefix_eval_clone(m->prefix);

  for (j = m->num_phases; j--; ) {
    if (!jit && !j) {
      if (naya) 
        naya[0] = m->bodies[0];
      break;
    }
    l1 = jit_vector(m->bodies[j], j > 0, jit);
    if (naya)
      naya[j] = l1;
    else if (!SAME_OBJ(l1, m->bodies[j])) {
      naya = MALLOC_N(Scheme_Object*, m->num_phases);
      for (i = m->num_phases; i-- > j; ) {
        naya[i] = m->bodies[i];
      }
      naya[j] = l1;
    }
  }

  pre_submods = m->pre_submodules;
  post_submods = m->post_submodules;
  submod_changed = 0;

  for (j = 0; j < 2; j++) {
    l1 = (j ? post_submods : pre_submods);
    if (l1 && !SCHEME_NULLP(l1)) {
      l2 = scheme_null;
      while (!SCHEME_NULLP(l1)) {
        sm = do_module_clone(SCHEME_CAR(l1), jit);
        if (!SAME_OBJ(sm, SCHEME_CAR(l1)))
          submod_changed = 1;
        l2 = scheme_make_pair(sm, l2);
        l1 = SCHEME_CDR(l1);
      }
      if (submod_changed) {
        l2 = scheme_reverse(l2);
        if (j)
          post_submods = l2;
        else
          pre_submods = l2;
      }
    }
  }

  if (!naya) {
    if (SAME_OBJ(rp, m->prefix) && !submod_changed)
      return data;
    naya = m->bodies;
  }
  
  m = MALLOC_ONE_TAGGED(Scheme_Module);
  memcpy(m, data, sizeof(Scheme_Module));
  m->bodies = naya;
  m->prefix = rp;

  m->pre_submodules = pre_submods;
  m->post_submodules = post_submods;

  return (Scheme_Object *)m;
}

Scheme_Object *scheme_module_jit(Scheme_Object *data)
{
  return do_module_clone(data, 1);
}

Scheme_Object *scheme_module_eval_clone(Scheme_Object *data)
{
  return do_module_clone(data, 0);
}

static Scheme_Object *do_module(Scheme_Object *form, Scheme_Comp_Env *env, 
				Scheme_Compile_Expand_Info *rec, int drec,
                                Scheme_Object *submodule_ancestry, Scheme_Object *submodule_path, int post,
                                Module_Begin_Expand_State *super_bxs,
                                Scheme_Object *super_phase_shift)
{
  Scheme_Object *fm, *disarmed_form;
  Scheme_Object *nm, *ii, *iidx, *self_modidx, *rmp, *rn_set, *mb_ctx, *ctx_form;
  Scheme_Module *iim;
  Scheme_Env *menv, *top_env;
  Scheme_Comp_Env *benv;
  Scheme_Module *m;
  Scheme_Object *mbval, *orig_ii;
  Scheme_Object *this_empty_self_modidx, **sub_iidx_ptrs;
  int saw_mb, check_mb = 0, shift_back = 0;
  Scheme_Object *restore_confusing_name = NULL;
  LOG_EXPAND_DECLS;

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_PRIM_MODULE(env->observer);
    if (rec[drec].depth > 0)
      rec[drec].depth++;
  }

  if (!scheme_is_toplevel(env))
    scheme_wrong_syntax(NULL, NULL, form, "not in a module-definition context");

  disarmed_form = scheme_stx_taint_disarm(form, NULL);

  fm = SCHEME_STX_CDR(disarmed_form);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax(NULL, NULL, form, NULL);
  nm = SCHEME_STX_CAR(fm);
  if (!SCHEME_STX_SYMBOLP(nm))
    scheme_wrong_syntax(NULL, nm, form, "module name is not an identifier");
  fm = SCHEME_STX_CDR(fm);
  if (!SCHEME_STX_PAIRP(fm))
    scheme_wrong_syntax(NULL, NULL, form, NULL);
  ii = SCHEME_STX_CAR(fm);
  fm = SCHEME_STX_CDR(fm);

  orig_ii = ii;

  if (post && SCHEME_FALSEP(SCHEME_STX_VAL(ii))) {
    ii = NULL;
    ctx_form = disarmed_form;
  } else {
    /* "Punch a hole" in the enclosing context by removing the
       immediately enclosing module context: */
    fm = disarmed_form;
    fm = scheme_revert_use_site_scopes(fm, env);
    fm = scheme_stx_unintroduce_from_module_context(fm, env->genv->stx_context);
    ctx_form = fm;
    fm = SCHEME_STX_CDR(fm);
    nm = SCHEME_STX_CAR(fm);
    fm = SCHEME_STX_CDR(fm);
    ii = SCHEME_STX_CAR(fm);
    fm = SCHEME_STX_CDR(fm);
    super_phase_shift = scheme_make_integer(0);
    orig_ii = ii;
  }

  if (!SCHEME_STXP(fm))
    fm = scheme_datum_to_syntax(fm, scheme_false, scheme_false, 0, 0);

  m = MALLOC_ONE_TAGGED(Scheme_Module);
  m->so.type = scheme_module_type;
  m->predefined = scheme_starting_up;
  m->phaseless = (scheme_starting_up ? scheme_true : NULL);

  /* must set before calling new_module_env: */
  rmp = SCHEME_STX_VAL(nm);
  rmp = scheme_intern_resolved_module_path(rmp);
  m->modname = rmp;
  if (super_bxs)
    m->modsrc = super_bxs->modsrc;
  else
    m->modsrc = rmp;

  if (!SCHEME_NULLP(submodule_ancestry))
    submodule_path = scheme_append(submodule_path, scheme_make_pair(SCHEME_STX_VAL(nm), scheme_null));
  m->submodule_ancestry = submodule_ancestry;
  m->submodule_path = submodule_path;
  
  if (!SCHEME_NULLP(submodule_path)) {
    Scheme_Object *self_name;
    self_name = scheme_resolved_module_path_value(extract_root_module_name(m));
    self_name = scheme_intern_resolved_module_path(scheme_make_pair(self_name, submodule_path));
    m->modname = self_name;
  }

  LOG_START_EXPAND(m);

  if (SAME_OBJ(m->modname, kernel_modname)
      || SAME_OBJ(m->modname, unsafe_modname)
      || SAME_OBJ(m->modname, flfxnum_modname)
      || SAME_OBJ(m->modname, extfl_modname)
      || SAME_OBJ(m->modname, futures_modname)
      || SAME_OBJ(m->modname, foreign_modname)) {
    /* Too confusing. Give it a different name while compiling. */
    Scheme_Object *k2;
    const char *kname;
    if (SAME_OBJ(m->modname, kernel_modname))
      kname = "#%kernel";
    else if (SAME_OBJ(m->modname, flfxnum_modname))
      kname = "#%flfxnum";
    else if (SAME_OBJ(m->modname, extfl_modname))
      kname = "#%extfl";
    else if (SAME_OBJ(m->modname, futures_modname))
      kname = "#%futures";
    else if (SAME_OBJ(m->modname, foreign_modname))
      kname = "#%foreign";
    else
      kname = "#%unsafe";
    k2 = scheme_intern_resolved_module_path(scheme_make_symbol(kname)); /* uninterned! */
    restore_confusing_name = m->modname;
    m->modname = k2;
  }

  {
    Scheme_Module_Exports *me;
    me = scheme_make_module_exports();
    m->me = me;
    me->modsrc = m->modsrc;
  }

  top_env = env->genv;
  /* Create module env from phase-0 env. This doesn't create bad
     sharing, because compile-time module instances for compiling this
     module are all fresh instances. */
  while (top_env->phase) {
    scheme_prepare_template_env(top_env);
    top_env = top_env->template_env;
  }

  /* Create module environment. This environment gets a fresh table
     for phase-1 instances: */
  menv = scheme_new_module_env(top_env, m, 1, SCHEME_NULLP(submodule_ancestry));

  menv->disallow_unbound = 1;
  
  self_modidx = scheme_make_modidx(scheme_false, scheme_false, m->modname);
  m->self_modidx = self_modidx;
  m->me->src_modidx = self_modidx;

  m->insp = env->insp;

  if (ii) {
    m->ii_src = ii;

    ii = scheme_syntax_to_datum(ii, 0, NULL);

    if (!scheme_is_module_path(ii)) {
      scheme_wrong_syntax(NULL, m->ii_src, form, "initial import is not a well-formed module path");
    }

    iidx = scheme_make_modidx(ii, 
                              self_modidx,
                              scheme_false);
  } else {
    void **super_bxs_info;
    Scheme_Object *shift;

    iidx = scheme_make_modidx(scheme_make_pair(submod_symbol,
                                               scheme_make_pair(scheme_make_utf8_string(".."),
                                                                scheme_null)),
                              self_modidx,
                              scheme_false);

    shift = scheme_make_pair(iidx, *super_bxs->sub_iidx_ptrs);
    *super_bxs->sub_iidx_ptrs = shift;

    super_phase_shift = scheme_bin_minus(scheme_make_integer(0), super_phase_shift);

    shift = scheme_make_shift(super_phase_shift, 
                              top_env->module->self_modidx, iidx, 
                              menv->module_registry->exports,
                              m->insp, m->insp);
    
    super_bxs_info = MALLOC_N(void*, 6);
    super_bxs_info[0] = super_bxs;
    super_bxs_info[1] = shift;
    super_bxs_info[2] = top_env->module->self_modidx;
    super_bxs_info[3] = iidx;
    super_bxs_info[4] = top_env;
    super_bxs_info[5] = super_phase_shift;
    m->super_bxs_info = super_bxs_info;
  }

  sub_iidx_ptrs = MALLOC_N(Scheme_Object*, 1);
  *sub_iidx_ptrs = scheme_null;
  m->sub_iidx_ptrs = sub_iidx_ptrs;

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_PREPARE_ENV(env->observer);
  }

  /* load the module for the initial require */
  if (iidx) {
    iim = module_load(_module_resolve(iidx, m->ii_src, NULL, 1), menv, NULL); 
    start_module(iim, find_env(menv, SCHEME_INT_VAL(super_phase_shift)), 0, iidx, 1, 0, menv->phase, scheme_null, 0);
  } else
    iim = NULL;

  m->requires = scheme_null;
  m->et_requires = scheme_null;
  m->tt_requires = scheme_null;
  m->dt_requires = scheme_null;

  if (iim && iim->phaseless)
    m->phaseless = scheme_true;

  if (iidx) {
    Scheme_Object *ins;
    ins = cons(iidx, scheme_null);
    if (SAME_OBJ(super_phase_shift, scheme_make_integer(0))) {
      m->requires = ins;
    } else if (SAME_OBJ(super_phase_shift, scheme_make_integer(-1))) {
      m->tt_requires = ins;
    } else {
      Scheme_Hash_Table *oht;
      oht = m->other_requires;
      if (!oht) {
        oht = scheme_make_hash_table_eqv();
        m->other_requires = oht;
      }
      scheme_hash_set(oht, super_phase_shift, ins);
    }
  }

  scheme_prepare_env_stx_context(menv);

  rn_set = menv->stx_context;

  {
    Scheme_Object *insp;
    menv->access_insp = env->insp;
    insp = scheme_make_inspector(env->insp);
    menv->guard_insp = insp;
  }

  scheme_prepare_exp_env(menv);

  /* Allow phase-1 references to unbound identifiers; we check
     at the end of body expansion to make sure that all referenced
     identifiers were eventually bound. Meanwhile, 
     reference-before-definition errors are possible. */
  menv->exp_env->disallow_unbound = -1;

  mb_ctx = scheme_false;

  /* For each provide in iim, add a module rename to fm */
  orig_ii = scheme_stx_add_module_context(orig_ii, rn_set);
  if (ii) {
    saw_mb = add_simple_require_renames(orig_ii, rn_set, menv, NULL, iim, iidx, scheme_make_integer(0),
                                        NULL, 1, 0);
    mb_ctx = scheme_datum_to_syntax(scheme_false, scheme_false, orig_ii, 0, 0);
  } else {
    Scheme_Object *shift;
    shift = (Scheme_Object *)m->super_bxs_info[1];
    fm = scheme_stx_add_shift(fm, shift);
    mb_ctx = scheme_stx_add_shift(ctx_form, shift);
    orig_ii = scheme_stx_add_shift(orig_ii, shift);
    shift_back = 1;
    /* there must be a `#%module-begin' in the enclosing module; if it's
       shadowed, then we want a different error message than the one for 
       saw_mb == 0 */
    saw_mb = 1;
  }

  m->ii_src = orig_ii;

  {
    Scheme_Object *frame_scopes;
    frame_scopes = scheme_module_context_frame_scopes(rn_set, NULL);
    if (rec[drec].comp)
      benv = scheme_new_comp_env(menv, env->insp, frame_scopes,
                                 SCHEME_MODULE_BEGIN_FRAME | SCHEME_KEEP_SCOPES_FRAME);
    else
      benv = scheme_new_expand_env(menv, env->insp, frame_scopes,
                                   SCHEME_MODULE_BEGIN_FRAME | SCHEME_KEEP_SCOPES_FRAME);
    benv->observer = env->observer;
  }

  /* If fm isn't a single expression, it certainly needs a
     `#%module-begin': */
  if (SCHEME_STX_PAIRP(fm) && SCHEME_STX_NULLP(SCHEME_STX_CDR(fm))) {
    /* Perhaps expandable... */
    fm = SCHEME_STX_CAR(fm);
    check_not_tainted(fm);
  } else {
    fm = scheme_make_pair(scheme_datum_to_syntax(module_begin_symbol, form, mb_ctx, 0, 2), 
			  fm);
    check_mb = 1;
  }

  fm = scheme_datum_to_syntax(fm, form, mb_ctx, 0, 2);

  if (!rec[drec].comp) {
    if (check_mb) {
      SCHEME_EXPAND_OBSERVE_TAG(env->observer, fm);
    }
  }

  fm = scheme_stx_property(fm, module_name_symbol, scheme_resolved_module_path_value(rmp));

  this_empty_self_modidx = scheme_get_submodule_empty_self_modidx(submodule_path, 1);

  /* phase shift to replace self_modidx of previous expansion: */
  fm = scheme_stx_shift(fm, NULL, this_empty_self_modidx, self_modidx, NULL,
                        m->insp, m->insp);

  fm = scheme_stx_add_module_frame_context(fm, rn_set);

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_RENAME_ONE(env->observer, fm);
  }

  if (!check_mb) {
    fm = scheme_check_immediate_macro(fm, benv, rec, drec, &mbval, 1);

    /* If expansion is not the primitive `#%module-begin', add local one: */
    if (!SAME_OBJ(mbval, modbeg_syntax)) {
      Scheme_Object *mb;
      mb = scheme_datum_to_syntax(module_begin_symbol, form, mb_ctx, 0, 0);
      fm = scheme_make_pair(mb, scheme_make_pair(fm, scheme_null));
      fm = scheme_datum_to_syntax(fm, form, mb_ctx, 0, 2);
      fm = scheme_stx_property(fm, module_name_symbol, scheme_resolved_module_path_value(rmp));

      if (!rec[drec].comp) {
        SCHEME_EXPAND_OBSERVE_TAG(env->observer, fm);
      }

      check_mb = 1;
    }
  }

  if (check_mb && !saw_mb) {
    scheme_wrong_syntax(NULL, NULL, form, 
			"no #%%module-begin binding in the module's language");
  }

  if (rec[drec].comp) {
    Scheme_Object *dummy, *pv;

    dummy = scheme_make_environment_dummy(env);
    m->dummy = dummy;
    
    scheme_compile_rec_done_local(rec, drec);
    fm = scheme_compile_expr(fm, benv, rec, drec);

    /* result should be a module body value: */
    if (!SAME_OBJ(fm, (Scheme_Object *)m)) {
      scheme_wrong_syntax(NULL, NULL, form, "compiled body was not built with #%%module-begin");
    }

    if (restore_confusing_name)
      m->modname = restore_confusing_name;

    m->ii_src = NULL;
    m->super_bxs_info = NULL;
    m->sub_iidx_ptrs = NULL;

    pv = scheme_stx_property(form, scheme_intern_symbol("module-language"), NULL);
    if (pv && SCHEME_TRUEP(pv)) {
      if (SCHEME_VECTORP(pv)
          && (3 == SCHEME_VEC_SIZE(pv))
          && scheme_is_module_path(SCHEME_VEC_ELS(pv)[0])
          && SCHEME_SYMBOLP(SCHEME_VEC_ELS(pv)[1]))
        m->lang_info = pv;
    }

    fm = (Scheme_Object *)m;
  } else {
    Scheme_Object *hints, *formname, *ps;
    Scheme_Object *shift;

    fm = scheme_expand_expr(fm, benv, rec, drec);

    if (shift_back) {
      shift = (Scheme_Object *)m->super_bxs_info[5];
      fm = scheme_stx_add_shift(fm, scheme_bin_minus(scheme_make_integer(0), shift));
    }

    m->ii_src = NULL;
    m->super_bxs_info = NULL;
    m->sub_iidx_ptrs = NULL;

    hints = m->hints;
    m->hints = NULL;

    formname = SCHEME_STX_CAR(disarmed_form);
    fm = cons(formname,
	      cons(nm,
		   cons(orig_ii,
                        cons(fm, scheme_null))));

    fm = scheme_datum_to_syntax(fm, form, ctx_form, 0, 2);

    /* for future expansion, shift away from self_modidx: */
    ps = scheme_make_shift(NULL, self_modidx, this_empty_self_modidx, NULL, NULL, NULL);
    fm = scheme_stx_add_shift(fm, ps);
    
    if (hints) {
      Scheme_Object *stx, *l;

      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-requires"),
			       m->requires);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-for-syntax-requires"),
			       m->et_requires);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-for-template-requires"),
			       m->tt_requires);

      l = scheme_null;
      if (!SCHEME_NULLP(m->dt_requires))
        l = scheme_make_pair(scheme_make_pair(scheme_false, m->dt_requires),
                             l);
      if (m->other_requires) {
        int i;
        for (i = 0; i < m->other_requires->size; i++) {
          if (m->other_requires->vals[i]) {
            l = scheme_make_pair(scheme_make_pair(m->other_requires->keys[i],
                                                  m->other_requires->vals[i]),
                                 l);
          }
        }
      }      
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-direct-for-meta-requires"),
			       l);
      
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-variable-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-syntax-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-indirect-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-indirect-for-meta-provides"),
			       SCHEME_CAR(hints));
      hints = SCHEME_CDR(hints);
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-kernel-reprovide-hint"),
			       SCHEME_CAR(hints));
      fm = scheme_stx_property(fm, 
			       scheme_intern_symbol("module-self-path-index"),
			       this_empty_self_modidx);

      fm = scheme_stx_property(fm, 
                               scheme_intern_symbol("module-body-context-simple?"),
                               (SAME_OBJ(scheme_true, m->rn_stx)
                                ? scheme_true
                                : scheme_false));

      stx = scheme_datum_to_syntax(scheme_intern_symbol("inside"), scheme_false, scheme_false, 0, 0);
      stx = scheme_stx_add_module_context(stx, rn_set);
      fm = scheme_stx_property(fm,
                               scheme_intern_symbol("module-body-context"),
                               scheme_stx_add_shift(stx, ps));

      stx = scheme_datum_to_syntax(scheme_intern_symbol("outside"), scheme_false, scheme_false, 0, 0);
      stx = scheme_stx_introduce_to_module_context(stx, rn_set);
      fm = scheme_stx_property(fm,
                               scheme_intern_symbol("module-body-inside-context"),
                               scheme_stx_add_shift(stx, ps));
    }

    /* make self_modidx like the empty modidx; this update plays the
       role of applying a shift to identifiers that are in syntax
       properties, such as the 'origin property */
    if (SAME_OBJ(this_empty_self_modidx, empty_self_modidx))
      ((Scheme_Modidx *)self_modidx)->resolved = empty_self_modname;
    else
      ((Scheme_Modidx *)self_modidx)->resolved = ((Scheme_Modidx *)this_empty_self_modidx)->resolved;

    while (SCHEME_PAIRP(*sub_iidx_ptrs)) {
      /* Each in `*sub_iidx_ptrs` corresponds to the implicit `..` import for
         a `(module* name #f ...)` submodule: */
      ((Scheme_Modidx *)SCHEME_CAR(*sub_iidx_ptrs))->resolved = ((Scheme_Modidx *)self_modidx)->resolved;
      *sub_iidx_ptrs = SCHEME_CDR(*sub_iidx_ptrs);
    }
  }

  if (rec[drec].comp || (rec[drec].depth != -2)) {
    /* rename tables no longer needed; NULL them out */
    menv->stx_context = NULL;
  }

  m->submodule_ancestry = NULL; /* ancestry no longer needed; NULL to avoid leak */

  LOG_END_EXPAND(m);

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_RENAME_ONE(env->observer, fm);
  }
  return fm;
}

static Scheme_Object *
module_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module(form, env, rec, drec, scheme_null, scheme_null, 0,
                   NULL, scheme_make_integer(0));
}


/**********************************************************************/
/*                          #%module-begin                            */
/**********************************************************************/

static Scheme_Object *do_module_begin(Scheme_Object *orig_form, Scheme_Comp_Env *env, 
				      Scheme_Compile_Expand_Info *rec, int drec)
{
  int num_phases, *_num_phases, i, exicount, *all_simple_bindings, has_submodules;
  Scheme_Hash_Tree *all_defs;
  Scheme_Hash_Table *tables, *all_defs_out, *all_provided, *all_reprovided, *modidx_cache;
  Scheme_Module_Export_Info **exp_infos, *exp_info;
  Scheme_Module_Phase_Exports *pt;
  Scheme_Object *form, *redef_modname, *rn_set, *observer, **exis, *body_lists, *expanded_l;
  Scheme_Env *genv;
  Module_Begin_Expand_State *bxs;
  Scheme_Expand_Info crec;

  form = scheme_stx_taint_disarm(orig_form, NULL);

  if (!(env->flags & SCHEME_MODULE_BEGIN_FRAME))
    scheme_wrong_syntax(NULL, NULL, form, "illegal use (not a module body)");

  if (scheme_stx_proper_list_length(form) < 0)
    scheme_wrong_syntax(NULL, NULL, form, IMPROPER_LIST_FORM);

  if (!env->genv->module)
    scheme_wrong_syntax(NULL, NULL, form, "not currently transforming a module");

  /* Redefining a module? */
  redef_modname = env->genv->module->modname;
  if (!scheme_hash_get(env->genv->module_registry->loaded, redef_modname))
    redef_modname = NULL;

  tables = scheme_make_hash_table_equal();

  modidx_cache = scheme_make_hash_table_equal();

  all_provided = scheme_make_hash_table_eqv();
  all_reprovided = scheme_make_hash_table_eqv();
  all_defs = scheme_make_hash_tree(2);
  all_defs_out = scheme_make_hash_table_eqv();

  rn_set = env->genv->stx_context;

  /* For `module->namespace`: */
  {
    Scheme_Object *rn_stx;
    rn_stx = scheme_module_context_to_stx(rn_set, env->genv->module->ii_src);
    env->genv->module->rn_stx = rn_stx;
  }

  /* It's possible that #%module-begin expansion introduces
     scoped identifiers for definitions. */
  form = introduce_to_module_context(form, rn_set);

  observer = env->observer;
  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, form);
  }

  _num_phases = MALLOC_ONE_ATOMIC(int);
  *_num_phases = 0;

  all_simple_bindings = (int *)scheme_malloc_atomic(sizeof(int));
  *all_simple_bindings = 1;

  if (env->genv->module->super_bxs_info) {
    *all_simple_bindings = 0;
  }

  bxs = scheme_malloc(sizeof(Module_Begin_Expand_State));
  bxs->tables = tables;
  bxs->all_provided = all_provided;
  bxs->all_reprovided = all_reprovided;
  bxs->all_defs = all_defs;
  bxs->all_defs_out = all_defs_out;
  bxs->all_simple_bindings = all_simple_bindings;
  bxs->_num_phases = _num_phases;
  bxs->saved_provides = scheme_null;
  bxs->saved_submodules = scheme_null;
  bxs->submodule_names = NULL;
  bxs->modidx_cache = modidx_cache;
  bxs->redef_modname = redef_modname;
  bxs->end_statementss = scheme_null;
  bxs->modsrc = env->genv->module->modsrc;
  bxs->sub_iidx_ptrs = env->genv->module->sub_iidx_ptrs;

  if (env->genv->module->super_bxs_info) {
    /* initialize imports that are available for export from the enclosing module's
       `all_defs' and `imports' (within `tables'): */
    void **super_bxs_info = env->genv->module->super_bxs_info;
    propagate_imports(bxs,
                      (Module_Begin_Expand_State *)super_bxs_info[0],
                      (Scheme_Object *)super_bxs_info[1],
                      (Scheme_Object *)super_bxs_info[2],
                      (Scheme_Object *)super_bxs_info[3],
                      (Scheme_Env *)super_bxs_info[4],
                      env->genv,
                      (Scheme_Object *)super_bxs_info[5]);
  }

  if (!rec[drec].comp) {
    /* In expand mode, we need to compile anyway in case of nested modules. */
    crec.comp = 1;
    crec.dont_mark_local_use = 0;
    crec.resolve_module_ids = 0;
    crec.substitute_bindings = 1;
    crec.pre_unwrapped = 0;
    crec.env_already = 0;
    crec.comp_flags = rec[drec].comp_flags;

    if (!env->prefix) {
      Comp_Prefix *cp;
      cp = MALLOC_ONE_RT(Comp_Prefix);
#ifdef MZTAG_REQUIRED
      cp->type = scheme_rt_comp_prefix;
#endif
      env->prefix = cp;
    }
  }

  body_lists = do_module_begin_at_phase(form, env, 
                                        rec[drec].comp ? rec : &crec, 
                                        rec[drec].comp ? drec : 0,
                                        rec[drec].comp ? NULL : rec, drec, 
                                        0, 
                                        scheme_null,
                                        bxs);
  num_phases = *_num_phases;

  if (!rec[drec].comp) {
    expanded_l = SCHEME_CAR(body_lists);
    body_lists = SCHEME_CDR(body_lists);
  } else
    expanded_l = body_lists;

  /* Compute provides for re-provides and all-defs-out: */
  (void)compute_reprovides(all_provided,
                           all_reprovided, 
                           env->genv->module, 
                           tables,
                           env->genv, 
                           num_phases,
                           bxs->all_defs, all_defs_out, 
                           "require", NULL, NULL);

  exp_infos = MALLOC_N(Scheme_Module_Export_Info*, num_phases);
  for (i = 0; i < num_phases; i++) {
    exp_info = MALLOC_ONE_RT(Scheme_Module_Export_Info);
    SET_REQUIRED_TAG(exp_info->type = scheme_rt_export_info);
    exp_infos[i] = exp_info;
  }

  /* Compute provide arrays */
  compute_provide_arrays(all_provided, tables,
                         env->genv->module->me,
                         env->genv,
                         form, 
                         num_phases, exp_infos);

  /* Compute indirect provides (which is everything at the top-level): */
  genv = env->genv;
  for (i = 0; i < num_phases; i++) {
    switch (i) {
    case 0:
      pt = env->genv->module->me->rt;
      break;
    case 1:
      pt = env->genv->module->me->et;
      break;
    default:
      if (env->genv->module->me->other_phases)
        pt = (Scheme_Module_Phase_Exports *)scheme_hash_get(env->genv->module->me->other_phases,
                                                            scheme_make_integer(i));
      else 
        pt = NULL;
      break;
    }
    if (pt) {
      exis = compute_indirects(genv, pt, &exicount, 1);
      exp_infos[i]->indirect_provides = exis;
      exp_infos[i]->num_indirect_provides = exicount;
      exis = compute_indirects(genv, pt, &exicount, 0);
      exp_infos[i]->indirect_syntax_provides = exis;
      exp_infos[i]->num_indirect_syntax_provides = exicount;
    }
    genv = genv->exp_env;
  }

  has_submodules = (!SCHEME_NULLP(bxs->saved_submodules)
                    || (env->genv->module->submodule_path
                        && !SCHEME_NULLP(env->genv->module->submodule_path)));

  if (!rec[drec].comp) {
    Scheme_Module_Phase_Exports *rt = env->genv->module->me->rt;
    int excount = rt->num_provides;
    int exvcount = rt->num_var_provides;
    Scheme_Object **exsns = rt->provide_src_names;
    Scheme_Object **exs = rt->provides;
    Scheme_Object **exss = rt->provide_srcs;

    /* Produce annotations (in the form of properties)
       for module information:
         'module-variable-provides = '(item ...)
         'module-syntax-provides = '(item ...)
	 'module-indirect-provides = '(id ...)
	 'module-indirect-for-meta-provides = '((phase id ...) ...)
         'module-kernel-reprovide-hint = 'kernel-reexport

      item = name
           | (ext-id . def-id)
           | (modidx ext-id . def-id)
     kernel-reexport = #f
                     | #t
                     | exclusion-id
    */
    int j, k;
    Scheme_Object *e, *a, *result;

    result = scheme_null;

    /* kernel re-export info (now always #f): */
    result = scheme_make_pair(scheme_false, result);

    /* Indirect provides for phases other than 0 */
    e = scheme_null;
    for (k = num_phases; k--; ) {
      if (exp_infos[k]->num_indirect_provides) {
        a = scheme_null;
        for (j = exp_infos[k]->num_indirect_provides; j--; ) {
          a = scheme_make_pair(exp_infos[k]->indirect_provides[j], a);
        }
        a = scheme_make_pair(scheme_make_integer(k), a);
        e = scheme_make_pair(a, e);
      }
    }
    result = scheme_make_pair(e, result);

    /* Indirect provides */ 
    a = scheme_null;
    for (j = exp_infos[0]->num_indirect_provides; j--; ) {
      a = scheme_make_pair(exp_infos[0]->indirect_provides[j], a);
    }
    result = scheme_make_pair(a, result);

    /* add syntax and value exports: */
    for (j = 0; j < 2; j++) {
      int top, i;

      e = scheme_null;

      if (!j) {
	i = exvcount;
	top = excount;
      } else {
	i = 0;
	top = exvcount;
      }
      
      for (; i < top; i++) {
	if (SCHEME_FALSEP(exss[i])
	    && SAME_OBJ(exs[i], exsns[i]))
	  a = exs[i];
	else {
	  a = scheme_make_pair(exs[i], exsns[i]);
	  if (!SCHEME_FALSEP(exss[i])) {
	    a = scheme_make_pair(exss[i], a);
	  }
	}
	e = scheme_make_pair(a, e);
      }
      result = scheme_make_pair(e, result);
    }

    env->genv->module->hints = result;
  }

  if (rec[drec].comp || has_submodules) {
    Scheme_Object *a, **bodies;

    bodies = MALLOC_N(Scheme_Object*, num_phases);
    for (i = 0; i < num_phases; i++) {
      a = SCHEME_CAR(body_lists);
      if (i > 0) a = scheme_reverse(a);
      a = scheme_list_to_vector(a);
      bodies[i] = a;
      body_lists = SCHEME_CDR(body_lists);
    }
    env->genv->module->bodies = bodies;
    env->genv->module->num_phases = num_phases;

    env->genv->module->exp_infos = exp_infos;

    if (!*all_simple_bindings) {
      /* No need to keep indirect syntax provides */
      for (i = 0; i < num_phases; i++) {
        exp_infos[i]->indirect_syntax_provides = NULL;
        exp_infos[i]->num_indirect_syntax_provides = 0;
      }
    }

    if (*all_simple_bindings && env->genv->module->rn_stx) {
      /* We will be able to reconstruct binding for `module->namespace`: */
      env->genv->module->rn_stx = scheme_true;
    } else {
      Scheme_Env *bnenv = env->genv;
      env->genv->module->binding_names = bnenv->binding_names;
      if (bnenv->exp_env) {
        bnenv = bnenv->exp_env;
        env->genv->module->et_binding_names = bnenv->binding_names;
        for (bnenv = bnenv->exp_env; bnenv; bnenv = bnenv->exp_env) {
          add_binding_names_from_environment(env->genv->module, bnenv);
        }
        bnenv = env->genv;
      }
      for (bnenv = bnenv->template_env; bnenv; bnenv = bnenv->template_env) {
        add_binding_names_from_environment(env->genv->module, bnenv);
      }
    }
  } else {
    /* For a property on the expanded module: */
    if (*all_simple_bindings && env->genv->module->rn_stx) {
      /* We will be able to reconstruct binding for `module->namespace`: */
      env->genv->module->rn_stx = scheme_true;
    }
  }

  if (rec[drec].comp || has_submodules) {
    Scheme_Object *dummy;
    dummy = scheme_make_environment_dummy(env);
    env->genv->module->dummy = dummy;
  }

  if (!rec[drec].comp) {
    SCHEME_EXPAND_OBSERVE_NEXT(observer);
  }

  /* Submodules */
  if (has_submodules) {
    Scheme_Object *expanded_modules, *root_module_name;

    root_module_name = extract_root_module_name(env->genv->module);

    /* Need to declare the just-finished module, so it can be
       referenced by nested modules: */
    {
      Optimize_Info *oi;
      Resolve_Prefix *rp;
      Resolve_Info *ri;
      Scheme_Object *o;
      int max_let_depth;
      int use_jit;

      /* Since we optimize & resolve the module here, it won't need to
         be optimized and resolved later. The resolve pass
         sets m->comp_prefix to NULL, which is how optimize & resolve
         know to avoid re-optimizing and re-resolving. */

      /* Note: don't use MZCONFIG_USE_JIT for module bodies */
      use_jit = scheme_startup_use_jit;

      o = scheme_letrec_check_expr((Scheme_Object *)env->genv->module);

      oi = scheme_optimize_info_create(env->prefix, env->genv, env->insp, 1);
      scheme_optimize_info_enforce_const(oi, rec[drec].comp_flags & COMP_ENFORCE_CONSTS);
      if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
        scheme_optimize_info_never_inline(oi);
      o = scheme_optimize_expr(o, oi, 0);

      rp = scheme_resolve_prefix(0, env->prefix, env->insp);
      ri = scheme_resolve_info_create(rp);
      scheme_resolve_info_enforce_const(ri, rec[drec].comp_flags & COMP_ENFORCE_CONSTS);

      o = scheme_resolve_expr(o, ri);
      max_let_depth = scheme_resolve_info_max_let_depth(ri);
      o = scheme_sfs(o, NULL, max_let_depth);

      if (use_jit)
        o = scheme_jit_expr(o);
      else
        o = scheme_eval_clone(o);
      
      (void)do_module_execute(o, env->genv, 0, 1, root_module_name, NULL);
    }

    if (!rec[drec].comp && (is_modulestar_stop(env))) {
      Scheme_Object *l = bxs->saved_submodules;
      expanded_modules =  NULL;
      while (!SCHEME_NULLP(l)) {
        expanded_modules = scheme_make_pair(SCHEME_CAR(SCHEME_CAR(l)),
                                            expanded_modules);
        l = SCHEME_CDR(l);
      }
      bxs->saved_submodules = scheme_null;
    } else
      expanded_modules = expand_submodules(rec, drec, env, bxs->saved_submodules, 1, bxs, !rec[drec].comp);

    if (!rec[drec].comp) {
      (void)fixup_expanded(expanded_l, expanded_modules, 0, MODULE_MODFORM_KIND);
    }
  }

  /* Return module or expanded code: */
  if (rec[drec].comp) {
    return (Scheme_Object *)env->genv->module;
  } else {
    Scheme_Object *p;

    if (rec[drec].depth == -2) {
      /* This was a local expand. Flush definitions, because the body expand may start over. */
      Scheme_Env *f_genv = env->genv;
      while (f_genv) {
        flush_definitions(f_genv);
        f_genv = f_genv->exp_env;
      }
    }

    p = SCHEME_STX_CAR(form);

    return scheme_datum_to_syntax(cons(p, expanded_l), orig_form, orig_form, 0, 2);
  }
}

static Scheme_Object *do_module_begin_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  void **args = p->ku.k.p1;
  Scheme_Object *form = (Scheme_Object *)args[0];
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)args[1];
  Scheme_Compile_Expand_Info *rec = (Scheme_Compile_Expand_Info *)args[2];
  Scheme_Compile_Expand_Info *erec = (Scheme_Compile_Expand_Info *)args[3];
  int phase = SCHEME_INT_VAL((Scheme_Object *)args[4]);
  Scheme_Object *body_lists = (Scheme_Object *)args[5];
  Module_Begin_Expand_State *bxs = (Module_Begin_Expand_State *)args[6];

  p->ku.k.p1 = NULL;
  
  return do_module_begin_at_phase(form, env, rec, 0, erec, 0,
                                  phase, body_lists, bxs);
}

static Scheme_Object *do_module_begin_at_phase(Scheme_Object *form, Scheme_Comp_Env *env, 
                                               Scheme_Compile_Expand_Info *rec, int drec,
                                               Scheme_Compile_Expand_Info *erec, int derec,
                                               int phase, 
                                               Scheme_Object *body_lists, /* starts from phase + 1; null in expand mode */
                                               Module_Begin_Expand_State *bxs)
/* Result in expand mode is expressions in order.
   Result in compile mode is a body_lists starting with `phase',
   where a body_lists has each phase in order, with each list after the first in reverse order.
   If both rec[drec].comp && erec, cons results.
   If !rec[drec].comp, then erec is non-NULL. */
{
  Scheme_Object *fm, *first, *last, *p, *rn_set, *exp_body, *self_modidx, *prev_p;
  Scheme_Object *expanded_l;
  Scheme_Comp_Env *xenv, *cenv, *rhs_env;
  Scheme_Hash_Table *required;    /* name -> (vector nominal-modidx-list modidx srcname var? prntname)
                                     first nominal-modidx goes with modidx, rest are for re-provides */
  Scheme_Hash_Table *provided;    /* exname -> (cons locname-stx-or-sym protected?) */
  Scheme_Object *all_rt_defs;        /* list of stxid; this is almost redundant to the syntax and toplevel
                                        tables, but it preserves the original name for exporting */
  Scheme_Hash_Tree *adt;
  Scheme_Object *lift_data;
  Scheme_Object *lift_ctx;
  Scheme_Object *lifted_reqs = scheme_null, *req_data, *unbounds = scheme_null;
  int maybe_has_lifts = 0, expand_ends = (phase == 0), non_phaseless, requested_phaseless;
  int requested_empty_namespace;
  Scheme_Object *observer, *vec, *end_statements;
  Scheme_Object *begin_for_syntax_stx, *non_phaseless_form = NULL;
  const char *who = "module";

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *pt = scheme_current_thread;
    Scheme_Compile_Expand_Info *recx, *erecx;
    void **args;

    if (rec) {
      recx = MALLOC_ONE_ATOMIC(Scheme_Compile_Expand_Info);
      memcpy(recx, rec + drec, sizeof(Scheme_Compile_Expand_Info));
    } else
      recx = NULL;
    
    if (erec) {
      erecx = MALLOC_ONE_ATOMIC(Scheme_Compile_Expand_Info);
      memcpy(erecx, erec + derec, sizeof(Scheme_Compile_Expand_Info));
    } else
      erecx = NULL;

    args = MALLOC_N(void*, 7);

    args[0] = form;
    args[1] = env;
    args[2] = recx;
    args[3] = erecx;
    args[4] = scheme_make_integer(phase);
    args[5] = body_lists;
    args[6] = bxs;

    pt->ku.k.p1 = (void *)args;
    
    fm = scheme_handle_stack_overflow(do_module_begin_k);

    if (recx)
      memcpy(rec + drec, recx, sizeof(Scheme_Compile_Expand_Info));
    if (erecx)
      memcpy(erec + derec, erecx, sizeof(Scheme_Compile_Expand_Info));

    return fm;
  }
#endif

  if (*bxs->_num_phases < phase + 1)
    *bxs->_num_phases = phase + 1;

  non_phaseless = (env->genv->module->phaseless ? 0 : NON_PHASELESS_IMPORT);
  requested_phaseless = 0;
  requested_empty_namespace = 0;
  env->genv->module->phaseless = NULL;

  /* Expand each expression in form up to `begin', `define-values', `define-syntax', 
     `require', `provide', `#%app', etc. */
  xenv = scheme_new_compilation_frame(0, (SCHEME_CAPTURE_WITHOUT_RENAME
					  | SCHEME_MODULE_FRAME
					  | SCHEME_FOR_STOPS),
                                      NULL,
				      env);

  install_stops(xenv, phase, &begin_for_syntax_stx);

  first = scheme_null;
  last = NULL;

  rn_set = env->genv->stx_context;

  xenv->expand_result_adjust = introduce_to_module_context;
  xenv->expand_result_adjust_arg = rn_set;
    
  vec = get_table(bxs->tables, scheme_make_integer(phase));
  if (SCHEME_FALSEP(SCHEME_VEC_ELS(vec)[0]))
    SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)env->genv->toplevel;
  if (SCHEME_FALSEP(SCHEME_VEC_ELS(vec)[2]))
    SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)env->genv->syntax;
  required = (Scheme_Hash_Table *)SCHEME_VEC_ELS(vec)[1];

  if (phase == 0) {
    /* Put initial requires into the table:
       (This is redundant for the rename set, but we need to fill
       the `all_requires' table, etc.) */
    if (env->genv->module->ii_src && SCHEME_TRUEP(SCHEME_STX_VAL(env->genv->module->ii_src))) {
      Scheme_Module *iim;
      Scheme_Object *nmidx, *orig_src;

      /* stx src of original import: */
      orig_src = env->genv->module->ii_src;
      if (!orig_src)
        orig_src = scheme_false;
      else if (!SCHEME_STXP(orig_src))
        orig_src = scheme_false;
    
      nmidx = SCHEME_CAR(env->genv->module->requires);
      iim = module_load(scheme_module_resolve(nmidx, 1), env->genv, NULL);

      add_simple_require_renames(orig_src, rn_set, env->genv, bxs->tables, 
                                 iim, nmidx,
                                 scheme_make_integer(0),
                                 NULL, 1, 1);

      scheme_hash_set(bxs->modidx_cache, ((Scheme_Modidx *)nmidx)->path, nmidx);
    }
  }

  provided = (Scheme_Hash_Table *)scheme_hash_get(bxs->all_provided, scheme_make_integer(phase));
  if (!provided) {
    provided = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(bxs->all_provided, scheme_make_integer(phase), (Scheme_Object *)provided);
  }

  all_rt_defs = scheme_hash_tree_get(bxs->all_defs, scheme_make_integer(phase));
  if (!all_rt_defs) all_rt_defs = scheme_null;

  if (SCHEME_NULLP(body_lists))
    exp_body = scheme_null;
  else {
    exp_body = SCHEME_CAR(body_lists);
    body_lists = SCHEME_CDR(body_lists);
  }

  self_modidx = env->genv->module->self_modidx;

  /* For syntax-local-context, etc., in a d-s RHS: */
  rhs_env = scheme_new_comp_env(env->genv, env->insp, NULL, SCHEME_TOPLEVEL_FRAME);

  observer = env->observer;
  rhs_env->observer = observer;

  maybe_has_lifts = 0;
  lift_ctx = scheme_generate_lifts_key();

  req_data = package_require_data(self_modidx, env->genv, env->genv->module,
                                  rn_set,
                                  bxs->tables,
                                  bxs->redef_modname, 
                                  bxs->all_simple_bindings,
                                  bxs->submodule_names);

  if (SCHEME_PAIRP(bxs->end_statementss)) {
    end_statements = SCHEME_CAR(bxs->end_statementss);
    bxs->end_statementss = SCHEME_CDR(bxs->end_statementss);
  } else
    end_statements = scheme_null;

  /* Pass 1 */

  /* Partially expand all expressions, and process definitions, requires,
     and provides. Also, flatten top-level `begin' expressions: */
  for (fm = SCHEME_STX_CDR(form); !SCHEME_STX_NULLP(fm); ) {
    Scheme_Object *e;
    int kind;

    while (1) {
      Scheme_Object *fst;

      if (erec) {
        SCHEME_EXPAND_OBSERVE_NEXT(observer);
      }

      e = SCHEME_STX_CAR(fm);

      p = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(xenv) 
           : end_statements);
      prev_p = (maybe_has_lifts 
                ? scheme_frame_get_provide_lifts(xenv)
                : scheme_null);
      scheme_frame_captures_lifts(xenv, scheme_make_lifted_defn, scheme_sys_wraps(xenv), 
                                  p, lift_ctx, req_data, prev_p, scheme_void);
      maybe_has_lifts = 1;

      {
	Scheme_Expand_Info erec1;
	erec1.comp = 0;
	erec1.depth = -1;
        erec1.pre_unwrapped = 0;
        erec1.substitute_bindings = 1;
        erec1.env_already = 0;
        erec1.comp_flags = rec[drec].comp_flags;
	e = scheme_expand_expr(e, xenv, &erec1, 0);
      }

      lifted_reqs = scheme_frame_get_require_lifts(xenv);
      if (erec && !SCHEME_NULLP(lifted_reqs)) {
        p = scheme_make_pair(scheme_make_pair(lifted_reqs, scheme_make_integer(LIFTREQ_MODFORM_KIND)), scheme_null);
        if (last)
          SCHEME_CDR(last) = p;
        else
          first = p;
        last = p;
      }

      fst = scheme_frame_get_lifts(xenv);
      if (!SCHEME_NULLP(fst)) {
	/* Expansion lifted expressions, so add them to
	   the front and try again. */
        *bxs->all_simple_bindings = 0;
	fm = SCHEME_STX_CDR(fm);
        e = introduce_to_module_context(e, rn_set);
        fm = scheme_named_map_1(NULL, introduce_to_module_context, fm, rn_set);
        fm = scheme_make_pair(e, fm);
        if (erec) {
          SCHEME_EXPAND_OBSERVE_RENAME_LIST(observer, fm);
        }
	fm = scheme_append(fst, fm);
        if (erec) {
          SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, fst);
        }
      } else {
	/* No definition lifts added... */
	if (SCHEME_STX_PAIRP(e))
	  fst = SCHEME_STX_CAR(e);
	else
	  fst = NULL;
	
	if (fst && SCHEME_STX_SYMBOLP(fst) && scheme_stx_free_eq_x(scheme_begin_stx, fst, phase)) {
	  fm = SCHEME_STX_CDR(fm);
	  e = introduce_to_module_context(e, rn_set);
          if (erec) {
            SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
          }
	  fm = scheme_flatten_begin(e, fm);
          if (erec) {
            SCHEME_EXPAND_OBSERVE_SPLICE(observer, fm);
          }
	  if (SCHEME_STX_NULLP(fm)) {
            e = scheme_frame_get_provide_lifts(xenv);
            e = scheme_reverse(e);
            if (expand_ends) {
              fm = scheme_frame_get_end_statement_lifts(xenv);
              fm = reverse_and_introduce_module_context(fm, rn_set);
              if (!SCHEME_NULLP(e))
                fm = scheme_append(fm, e);
              maybe_has_lifts = 0;
            } else
              fm = e;
            if (SCHEME_NULLP(fm) && expand_ends)
              fm = get_higher_phase_lifts(bxs, begin_for_syntax_stx);
            if (erec) {
              SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
            }
            if (SCHEME_NULLP(fm)) {
              e = NULL;
              break;
            }
	  }
	} else
          break;
      }
    }
    if (!e) break; /* (begin) expansion at end */

    e = introduce_to_module_context(e, rn_set);

    if (erec) {
      SCHEME_EXPAND_OBSERVE_RENAME_ONE(observer, e);
    }

    if (SCHEME_STX_PAIRP(e)) {
      Scheme_Object *fst;

      fst = SCHEME_STX_CAR(e);

      if (SCHEME_STX_SYMBOLP(fst)) {
	if (scheme_stx_free_eq_x(scheme_define_values_stx, fst, phase)) {
	  /************ define-values *************/
	  Scheme_Object *vars, *val;
          int var_count = 0;

          if (erec) {
            SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
            SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_VALUES(observer);
          }

	  /* Create top-level vars; uses revert_use_site_scopes() on the vars */
	  scheme_define_parse(e, &vars, &val, 0, xenv, 1);

	  while (SCHEME_STX_PAIRP(vars)) {
	    Scheme_Object *name, *orig_name, *binding;

	    name = SCHEME_STX_CAR(vars);

	    orig_name = name;

	    /* Remember the original: */
	    all_rt_defs = scheme_make_pair(name, all_rt_defs);

            binding = scheme_stx_lookup_exact(name, scheme_make_integer(phase));

            if (!SCHEME_FALSEP(binding)) {
              if (SCHEME_SYMBOLP(binding)) {
                scheme_wrong_syntax(who, orig_name, e, "out-of-context identifier for definition");
                return NULL;
              } else if (SAME_OBJ(SCHEME_VEC_ELS(binding)[0], self_modidx)
                         && check_already_defined(SCHEME_VEC_ELS(binding)[1], env->genv)) {
                scheme_wrong_syntax(who, orig_name, e, "duplicate definition for identifier");
                return NULL;
              } else if (check_already_required(required, name, phase, binding))
                warn_previously_required(env->genv->module->modname, orig_name);
            }

            /* Generate symbol for this binding: */
            name = scheme_global_binding(name, env->genv, 0);

	    /* Create the bucket, indicating that the name will be defined: */
	    scheme_add_global_symbol(name, scheme_undefined, env->genv);

	    if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)
                || !scheme_stx_equal_module_context(orig_name, env->genv->module->rn_stx))
              *bxs->all_simple_bindings = 0;

	    vars = SCHEME_STX_CDR(vars);
            var_count++;
	  }

          if (!(non_phaseless & NON_PHASELESS_FORM) && !phaseless_rhs(val, var_count, phase)) {
            non_phaseless |= NON_PHASELESS_FORM;
            non_phaseless_form = val;
          }

          if (!rec[drec].comp) {
            /* Reconstruct to remove scopes that don't belong on the binding names in the expansion: */
            e = scheme_datum_to_syntax(scheme_make_pair(fst, scheme_make_pair(vars,
                                                                              scheme_make_pair(val,
                                                                                               scheme_null))),
                                       e, e, 0, 2);
          }
          
          if (erec) {
            SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
          }
	  kind = DEFN_MODFORM_KIND;
        } else if (scheme_stx_free_eq_x(scheme_define_syntaxes_stx, fst, phase)
                   || scheme_stx_free_eq_x(scheme_begin_for_syntax_stx, fst, phase)) {
	  /************ define-syntaxes & begin-for-syntax *************/
	  /* Define the macro: */
	  Scheme_Compile_Info mrec, erec1;
	  Scheme_Object *names, *orig_names, *l, *code, *m, *vec, *boundname, *frame_scopes;
	  Resolve_Prefix *rp;
	  Resolve_Info *ri;
	  Scheme_Comp_Env *oenv, *eenv;
	  Optimize_Info *oi;
	  int count = 0;
	  int for_stx;
          int max_let_depth;

	  for_stx = scheme_stx_free_eq_x(scheme_begin_for_syntax_stx, fst, phase);

          if (erec) {
            SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
          }

          if (for_stx) {
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PRIM_BEGIN_FOR_SYNTAX(observer);
            }
            if (scheme_stx_proper_list_length(e) < 0)
              scheme_wrong_syntax(NULL, NULL, e, NULL);
            code = e;
          } else {
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PRIM_DEFINE_SYNTAXES(observer);
            }
            scheme_define_parse(e, &names, &code, 1, env, 1);
          }

          if (!for_stx && SCHEME_STX_PAIRP(names) && SCHEME_STX_NULLP(SCHEME_STX_CDR(names)))
            boundname = SCHEME_STX_CAR(names);
          else
            boundname = scheme_false;

          if (erec) {
            SCHEME_EXPAND_OBSERVE_PREPARE_ENV(observer);
          }

	  scheme_prepare_exp_env(env->genv);
	  scheme_prepare_compile_env(env->genv->exp_env);

          frame_scopes = scheme_module_context_use_site_frame_scopes(env->genv->exp_env->stx_context);

	  eenv = scheme_new_comp_env(env->genv->exp_env, env->insp,
                                     frame_scopes,
                                     SCHEME_KEEP_SCOPES_FRAME);
          eenv->observer = observer;
          if (!for_stx)
            scheme_frame_captures_lifts(eenv, NULL, NULL, scheme_false, scheme_false, 
                                        req_data, scheme_false, scheme_false);

	  oenv = env;
	  
          if (!for_stx) {
            orig_names = scheme_null;
            for (l = names; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
              Scheme_Object *name, *orig_name, *binding;
              name = SCHEME_STX_CAR(l);

              orig_name = name;

              /* Remember the original: */
              all_rt_defs = scheme_make_pair(name, all_rt_defs);
              orig_names = scheme_make_pair(name, orig_names);
	    
              binding = scheme_stx_lookup_exact(name, scheme_make_integer(phase));

              if (!SCHEME_FALSEP(binding)) {
                if (SCHEME_SYMBOLP(binding)) {
                  scheme_wrong_syntax(who, orig_name, e, "out-of-context identifier for definition");
                  return NULL;
                } else if (SAME_OBJ(SCHEME_VEC_ELS(binding)[0], self_modidx)
                           && check_already_defined(SCHEME_VEC_ELS(binding)[1], env->genv)) {
                  scheme_wrong_syntax(who, orig_name, e, 
                                      "duplicate definition for identifier");
                  return NULL;
                } else if (check_already_required(required, name, phase, binding))
                  warn_previously_required(oenv->genv->module->modname, orig_name);
              }

              /* Generate symbol for this binding: */
              name = scheme_global_binding(name, env->genv, 0);

              if (!SAME_OBJ(SCHEME_STX_VAL(orig_name), name)
                  || !scheme_stx_equal_module_context(orig_name, env->genv->module->rn_stx))
                *bxs->all_simple_bindings = 0;

              count++;
            }
            orig_names = scheme_reverse(orig_names);
          } else
            orig_names = NULL;

          if (for_stx)
            names = NULL;
          else
            names = scheme_named_map_1(NULL, stx_sym, names, (Scheme_Object *)oenv->genv);
	  
	  mrec.comp = 1;
	  mrec.dont_mark_local_use = 0;
	  mrec.resolve_module_ids = 0;
          mrec.substitute_bindings = 1;
          mrec.pre_unwrapped = 0;
          mrec.env_already = 0;
          mrec.comp_flags = rec[drec].comp_flags;

          if (erec) {
            erec1.comp = 0;
            erec1.depth = -1;
            erec1.pre_unwrapped = 0;
            erec1.substitute_bindings = 1;
            erec1.env_already = 0;
            erec1.comp_flags = rec[drec].comp_flags;
          }

	  if (for_stx) {
            adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
            bxs->all_defs = adt;
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
              /* We expand & compile the for-syntax code in one pass. */
            }
            m = do_module_begin_at_phase(code, eenv, 
                                         &mrec, 0, 
                                         (erec ? &erec1 : NULL), 0,
                                         phase + 1, body_lists,
                                         bxs);
            if (erec) {
              code = SCHEME_STX_CAR(code);
              code = scheme_make_pair(code, SCHEME_CAR(m));
              m = SCHEME_CDR(m);
            }
            if (rec[drec].comp)
              body_lists = SCHEME_CDR(m);
            m = SCHEME_CAR(m);
            /* turn list of compiled expressions into a splice: */
            m = scheme_make_sequence_compilation(m, 0, 0);
            if (m->type == scheme_sequence_type)
              m->type = scheme_splice_sequence_type;
          } else {
            if (erec) {
              SCHEME_EXPAND_OBSERVE_PHASE_UP(observer);
              eenv->value_name = boundname;
              eenv->observer = xenv->observer;
              code = scheme_expand_expr_lift_to_let(code, eenv, &erec1, 0);
            }
            eenv->value_name = boundname;
            eenv->observer = NULL;
            m = scheme_compile_expr_lift_to_let(code, eenv, &mrec, 0);
            eenv->value_name = NULL;
          }

          if (!for_stx) {
            lifted_reqs = scheme_frame_get_require_lifts(eenv);
            if (erec && !SCHEME_NULLP(lifted_reqs)) {
              p = scheme_make_pair(scheme_make_pair(lifted_reqs, scheme_make_integer(LIFTREQ_MODFORM_KIND)), scheme_null);
              if (last)
                SCHEME_CDR(last) = p;
              else
                first = p;
              last = p;
            }
          }

          m = scheme_letrec_check_expr(m);

	  oi = scheme_optimize_info_create(eenv->prefix, eenv->genv, env->insp, 1);
          scheme_optimize_info_set_context(oi, (Scheme_Object *)env->genv->module);
          if (!(rec[drec].comp_flags & COMP_CAN_INLINE))
            scheme_optimize_info_never_inline(oi);
	  m = scheme_optimize_expr(m, oi, 0);
	  
	  rp = scheme_resolve_prefix(1, eenv->prefix, env->insp);
	  ri = scheme_resolve_info_create(rp);
          scheme_enable_expression_resolve_lifts(ri);
	  m = scheme_resolve_expr(m, ri);
          m = scheme_merge_expression_resolve_lifts(m, rp, ri);
          rp = scheme_remap_prefix(rp, ri);

          max_let_depth = scheme_resolve_info_max_let_depth(ri);

	  /* Add code with names and lexical depth to exp-time body: */
	  vec = scheme_make_vector(5, NULL);
	  SCHEME_VEC_ELS(vec)[0] = (for_stx
                                    ? scheme_false
                                    : ((SCHEME_PAIRP(names) && SCHEME_NULLP(SCHEME_CDR(names)))
                                       ? SCHEME_CAR(names)
                                       : names));
	  SCHEME_VEC_ELS(vec)[1] = m;
          SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(max_let_depth);
	  SCHEME_VEC_ELS(vec)[3] = (Scheme_Object *)rp;
	  SCHEME_VEC_ELS(vec)[4] = (for_stx ? scheme_true : scheme_false);
	  exp_body = scheme_make_pair(vec, exp_body);

          if (eenv->prefix->unbound)
            unbounds = scheme_make_pair(eenv->prefix->unbound, unbounds);
	
          m = scheme_sfs(m, NULL, max_let_depth);
	  if (scheme_startup_use_jit /* Note: not scheme_resolve_info_use_jit(ri) */)
	    m = scheme_jit_expr(m);
          rp = scheme_prefix_eval_clone(rp);
          
	  eval_exptime(names, count, m, eenv->genv, rhs_env, rp, max_let_depth, 0, 
                       (for_stx ? env->genv->exp_env->toplevel : env->genv->syntax), 
                       phase + 1,
                       for_stx ? scheme_false : orig_names, NULL);
          
	  if (erec) {
            if (for_stx) {
              m = code;
            } else {
              m = SCHEME_STX_CDR(e);
              m = SCHEME_STX_CAR(m);
              m = scheme_make_pair(fst,
                                   scheme_make_pair(orig_names, scheme_make_pair(code, scheme_null)));
            }
	    e = scheme_datum_to_syntax(m, e, e, 0, 2);
	  } else
            e = NULL;
          
          if (erec) {
            SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
          }

          kind = DONE_MODFORM_KIND;

          non_phaseless |= NON_PHASELESS_FORM;
          if (!non_phaseless_form)
            non_phaseless_form = e;
	} else if (scheme_stx_free_eq_x(require_stx, fst, phase)) {
	  /************ require *************/
          if (erec) {
            SCHEME_EXPAND_OBSERVE_ENTER_PRIM(observer, e);
            SCHEME_EXPAND_OBSERVE_PRIM_REQUIRE(observer);
          }

          e = revert_use_site_scopes_via_context(e, rn_set, phase);

	  /* Adds requires to renamings and required modules to requires lists: */
	  parse_requires(e, phase, self_modidx, env->genv, env->genv->module,
                         rn_set,
                         check_require_name, bxs->tables,
                         bxs->redef_modname, 
                         0, 
                         1, phase ? 1 : 0,
                         bxs->all_simple_bindings, bxs->modidx_cache,
                         bxs->submodule_names,
                         &non_phaseless);

	  if (!erec)
	    e = NULL;

          if (erec) {
            SCHEME_EXPAND_OBSERVE_EXIT_PRIM(observer, e);
          }
	  kind = DONE_MODFORM_KIND;
	} else if (scheme_stx_free_eq_x(provide_stx, fst, phase)) {
	  /************ provide *************/
          /* remember it for pass 3 */
          p = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)),
                               bxs->saved_provides);
          bxs->saved_provides = p;
          kind = PROVIDE_MODFORM_KIND;
	} else if (scheme_stx_free_eq_x(declare_stx, fst, phase)) {
	  /************ declare *************/
          Scheme_Object *kws, *kw;
          
          kws = SCHEME_STX_CDR(e);
          while (SCHEME_STX_PAIRP(kws)) {
            kw = SCHEME_STX_CAR(kws);
            if (SCHEME_KEYWORDP(SCHEME_STX_VAL(kw))) {
              if (SAME_OBJ(SCHEME_STX_VAL(kw), phaseless_keyword)) {
                if (requested_phaseless)
                  scheme_wrong_syntax(who, kw, e, "duplicate declaration");
                requested_phaseless = 1;
              } else if (SAME_OBJ(SCHEME_STX_VAL(kw), empty_namespace_keyword)) {
                if (requested_empty_namespace)
                  scheme_wrong_syntax(who, kw, e, "duplicate declaration");
                requested_empty_namespace = 1;
              } else {
                scheme_wrong_syntax(who, kw, e, "unrecognized keyword");
              }
            } else {
              scheme_wrong_syntax(who, kw, e, "expected a keyword");
            }
            kws = SCHEME_STX_CDR(kws);
          }
          if (!SCHEME_STX_NULLP(kws))
            scheme_wrong_syntax(who, NULL, e, IMPROPER_LIST_FORM);
          
          kind = DECLARE_MODFORM_KIND;
	} else if (scheme_stx_free_eq_x(scheme_module_stx, fst, phase)
                   || scheme_stx_free_eq_x(scheme_modulestar_stx, fst, phase)) {
	  /************ module[*] *************/
          /* check outer syntax & name, then expand pre-module or remember for post-module pass */
          int k;

          e = handle_submodule_form(who,
                                    e, env, phase,
                                    rn_set, observer,
                                    bxs,
                                    rec, drec, erec, derec,
                                    &k);
          kind = k;
	} else {
	  kind = EXPR_MODFORM_KIND;
          non_phaseless |= NON_PHASELESS_FORM;
          if (!non_phaseless_form)
            non_phaseless_form = e;
        }
      } else {
	kind = EXPR_MODFORM_KIND;
        non_phaseless |= NON_PHASELESS_FORM;
        if (!non_phaseless_form)
            non_phaseless_form = e;
      }
    } else {
      kind = EXPR_MODFORM_KIND;
      non_phaseless |= NON_PHASELESS_FORM;
      if (!non_phaseless_form)
        non_phaseless_form = e;
    }

    if (e) {
      p = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(kind)), scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
    }

    fm = SCHEME_STX_CDR(fm);

    /* If we're out of declarations, check for lifted-to-end: */
    if (SCHEME_STX_NULLP(fm) && maybe_has_lifts) {
      e = scheme_frame_get_provide_lifts(xenv);
      e = scheme_reverse(e);
      if (expand_ends) {
        fm = scheme_frame_get_end_statement_lifts(xenv);
        fm = reverse_and_introduce_module_context(fm, rn_set);
        if (!SCHEME_NULLP(e))
          fm = scheme_append(fm, e);
        maybe_has_lifts = 0;
        if (SCHEME_NULLP(fm))
          fm = get_higher_phase_lifts(bxs, begin_for_syntax_stx);
      } else
        fm = e;
      if (erec) {
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, fm);
      }
    }
  }
  /* first =  a list of (cons semi-expanded-expression kind) */

  if (!expand_ends) {
    if (maybe_has_lifts)
      end_statements = scheme_frame_get_end_statement_lifts(xenv);
  }

  if (!phase) {
    /* Check that all bindings used in phase-N expressions (for N >= 1) 
       were defined by now: */
    check_formerly_unbound(unbounds, env);
  }

  /* Pass 2 */
  if (erec) {
    SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer);
  }
  
  {
    /* Module and each `begin-for-syntax' group manages its own prefix: */
    Scheme_Object *frame_scopes;
    frame_scopes = scheme_module_context_frame_scopes(rn_set, xenv->scopes);
    cenv = scheme_new_comp_env(env->genv, env->insp, frame_scopes,
                               SCHEME_TOPLEVEL_FRAME | SCHEME_KEEP_SCOPES_FRAME);
    cenv->observer = env->observer;
  }

  lift_data = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(lift_data)[0] = (Scheme_Object *)cenv;
  SCHEME_VEC_ELS(lift_data)[1] = self_modidx;
  SCHEME_VEC_ELS(lift_data)[2] = rn_set;

  maybe_has_lifts = 0;

  prev_p = NULL;
  expanded_l = scheme_null;
  for (p = first; !SCHEME_NULLP(p); ) {
    Scheme_Object *e, *l, *ll;
    int kind;

    e = SCHEME_CAR(p);
    kind = SCHEME_INT_VAL(SCHEME_CDR(e));
    e = SCHEME_CAR(e);
    
    if (erec) {
      SCHEME_EXPAND_OBSERVE_NEXT(observer);
    }

    if (kind == SAVED_MODFORM_KIND) {
      expanded_l = scheme_make_pair(SCHEME_CDR(e), expanded_l);
      SCHEME_CAR(p) = SCHEME_CAR(e);
      prev_p = p;
      p = SCHEME_CDR(p);      
    } else if (kind == DECLARE_MODFORM_KIND) {
      expanded_l = scheme_make_pair(e, expanded_l);
      p = SCHEME_CDR(p);
    } else if (kind == LIFTREQ_MODFORM_KIND) {
      expanded_l = scheme_append(e, expanded_l);
      p = SCHEME_CDR(p);
    } else if ((kind == PROVIDE_MODFORM_KIND)
               || (kind == MODULE_MODFORM_KIND)) {
      /* handle `provide's and `module's in later passes */
      if (erec)
        expanded_l = scheme_make_pair(e, expanded_l);
      if (rec[drec].comp) {
        if (!prev_p)
          first = SCHEME_CDR(p);
        else
          SCHEME_CDR(prev_p) = SCHEME_CDR(p);
      }
      p = SCHEME_CDR(p);
    } else if ((kind == EXPR_MODFORM_KIND)
               || (kind == DEFN_MODFORM_KIND)) {
      Scheme_Comp_Env *nenv;

      l = (maybe_has_lifts 
           ? scheme_frame_get_end_statement_lifts(cenv) 
           : end_statements);
      ll = (maybe_has_lifts 
            ? scheme_frame_get_provide_lifts(cenv) 
            : scheme_null);
      scheme_frame_captures_lifts(cenv, add_lifted_defn, lift_data, l, lift_ctx, req_data, ll, scheme_void);
      maybe_has_lifts = 1;

      if (kind == DEFN_MODFORM_KIND)
        nenv = cenv;
      else
        nenv = scheme_new_compilation_frame(0, 0, NULL, cenv);

      if (erec) {
	Scheme_Expand_Info erec1;
	scheme_init_expand_recs(erec, derec, &erec1, 1);
	e = scheme_expand_expr(e, nenv, &erec1, 0);
        expanded_l = scheme_make_pair(e, expanded_l);
      }

      if (rec[drec].comp) {
	Scheme_Compile_Info crec1;
	scheme_init_compile_recs(rec, drec, &crec1, 1);
	crec1.resolve_module_ids = 0;
        nenv->observer = NULL;
	e = scheme_compile_expr(e, nenv, &crec1, 0);
        nenv->observer = env->observer;
      }

      lifted_reqs = scheme_frame_get_require_lifts(cenv);
      if (erec && !SCHEME_NULLP(lifted_reqs))
        expanded_l = scheme_make_pair(SCHEME_CAR(expanded_l),
                                      scheme_append(lifted_reqs, SCHEME_CDR(expanded_l)));
      
      l = scheme_frame_get_lifts(cenv);
      if (SCHEME_NULLP(l)) {
	/* No lifts - continue normally */
	SCHEME_CAR(p) = e;
	prev_p = p;
	p = SCHEME_CDR(p);
      } else {
	/* Lifts - insert them and try again */
        Scheme_Object *fst;
        *bxs->all_simple_bindings = 0;
        if (erec) {
          SCHEME_EXPAND_OBSERVE_MODULE_LIFT_LOOP(observer, scheme_copy_list(l));
        }
        if (erec) {
          e = scheme_make_pair(scheme_make_pair(e, SCHEME_CAR(expanded_l)),
                               scheme_make_integer(SAVED_MODFORM_KIND)); /* kept both expanded & maybe compiled */
          /* add back expanded at correct position later: */
          expanded_l = SCHEME_CDR(expanded_l);
        } else
          e = scheme_make_pair(e, scheme_make_integer(DONE_MODFORM_KIND)); /* don't re-compile/-expand */
	SCHEME_CAR(p) = e;
	for (ll = l; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
          e = SCHEME_CAR(ll);
          if (SCHEME_STX_PAIRP(SCHEME_CAR(e)))
            fst = SCHEME_STX_CAR(SCHEME_CAR(e));
          else
            fst = NULL;
          if (fst
              && (scheme_stx_free_eq3(fst, scheme_module_stx, scheme_make_integer(phase), scheme_make_integer(0))
                  || scheme_stx_free_eq3(fst, scheme_modulestar_stx, scheme_make_integer(phase), scheme_make_integer(0)))) {
            /* a `module` or `module*` form; handle as in first pass */
            int k;
            e = handle_submodule_form(who,
                                      e, env, phase,
                                      rn_set, observer,
                                      bxs,
                                      rec, drec, erec, derec,
                                      &k);
            if (e)
              e = scheme_make_pair(e, scheme_make_integer(k));
            else
              e = scheme_make_pair(scheme_void, DONE_MODFORM_KIND);
          } else {
            e = scheme_make_pair(e, scheme_make_integer(DEFN_MODFORM_KIND));
          }
	  SCHEME_CAR(ll) = e;
	}
	p = scheme_append(l, p);
	if (prev_p) {
	  SCHEME_CDR(prev_p) = p;
	} else {
	  first = p;
	}
      }
    } else {
      if (erec)
        expanded_l = scheme_make_pair(e, expanded_l);
      SCHEME_CAR(p) = e;
      prev_p = p;
      p = SCHEME_CDR(p);
    }

    /* If we're out of declarations, check for lifted-to-end: */
    if (SCHEME_NULLP(p) && maybe_has_lifts) {
      int expr_cnt;
      Scheme_Object *sp;
      e = scheme_frame_get_provide_lifts(cenv);
      e = scheme_reverse(e);
      if (expand_ends) {
        p = scheme_frame_get_end_statement_lifts(cenv);
        p = scheme_reverse(p);
        expr_cnt = scheme_list_length(p);
        if (!SCHEME_NULLP(e))
          p = scheme_append(p, e);
      } else {
        p = e;
        expr_cnt = 0;
      }
      if (erec) {
        SCHEME_EXPAND_OBSERVE_MODULE_LIFT_END_LOOP(observer, p);
      }
      for (ll = p; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll)) {
        e = SCHEME_CAR(ll);
        if (expr_cnt <= 0) {
          sp = scheme_make_pair(scheme_make_pair(e, scheme_make_integer(phase)),
                                bxs->saved_provides);
          bxs->saved_provides = sp;
        }
        e = scheme_make_pair(e, ((expr_cnt > 0) 
                                 ? scheme_make_integer(EXPR_MODFORM_KIND) 
                                 : scheme_make_integer(PROVIDE_MODFORM_KIND)));
        SCHEME_CAR(ll) = e;
        expr_cnt--;
      }
      maybe_has_lifts = 0;
      if (prev_p) {
        SCHEME_CDR(prev_p) = p;
      } else {
        first = p;
      }
    }
  }
  if (erec) expanded_l = scheme_reverse(expanded_l);

  /* If not phase 0, save end statements */
  if (!expand_ends) {
    if (maybe_has_lifts)
      end_statements = scheme_frame_get_end_statement_lifts(cenv);
    if (!SCHEME_NULLP(end_statements) || !SCHEME_NULLP(bxs->end_statementss)) {
      p = scheme_make_pair(end_statements, bxs->end_statementss);
      bxs->end_statementss = p;
    }
  }

  adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
  bxs->all_defs = adt;

  /* Pass 3 */
  /* if at phase 0, expand provides for all phases */
  if (erec) {
    SCHEME_EXPAND_OBSERVE_NEXT_GROUP(observer);
  }
  
  if (phase == 0) {
    Scheme_Object *expanded_provides;

    expanded_provides = expand_all_provides(form, cenv, 
                                            (erec ? erec : rec), (erec ? derec : drec),
                                            self_modidx,
                                            bxs, !!erec);
  
    if (erec) {
      expanded_provides = scheme_reverse(expanded_provides);
      (void)fixup_expanded(expanded_l, expanded_provides, 0, PROVIDE_MODFORM_KIND);
    }
  }

  /* first = a list of compiled expressions */
  /* expanded_l = list of expanded expressions */

  /* If compiling, drop expressions that are constants: */
  if (rec[drec].comp) {
    Scheme_Object *prev = NULL, *next;
    for (p = first; !SCHEME_NULLP(p); p = next) {
      next = SCHEME_CDR(p);
      if (scheme_omittable_expr(SCHEME_CAR(p), -1, -1, 0, NULL, NULL)) {
	if (prev)
	  SCHEME_CDR(prev) = next;
	else
	  first = next;
      } else
	prev = p;
    }
  }

  adt = scheme_hash_tree_set(bxs->all_defs, scheme_make_integer(phase), all_rt_defs);
  bxs->all_defs = adt;

  if (cenv->prefix->non_phaseless)
    non_phaseless |= NON_PHASELESS_IMPORT;

  if (!phase)
    env->genv->module->comp_prefix = cenv->prefix;
  else
    env->prefix = cenv->prefix;

  if (!SCHEME_NULLP(exp_body)) {
    if (*bxs->_num_phases < phase + 2)
      *bxs->_num_phases = phase + 2;
  }

  if (requested_phaseless) {
    if (!non_phaseless)
      env->genv->module->phaseless = scheme_true;
    else {
      if (non_phaseless & NON_PHASELESS_IMPORT)
        scheme_wrong_syntax(who, NULL, form, "cannot be cross-phase persistent due to required modules");
      else
        scheme_wrong_syntax(who, non_phaseless_form, form, "does not satisfy cross-phase persistent grammar");
    }
  }

  if (requested_empty_namespace)
    env->genv->module->rn_stx = NULL;

  if (rec[drec].comp) {
    body_lists = scheme_make_pair(first, scheme_make_pair(exp_body, body_lists));
    if (erec)
      return scheme_make_pair(expanded_l, body_lists);
    else
      return body_lists;
  } else
    return expanded_l;
}


static Scheme_Object *
module_begin_compile(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec, int drec)
{
  return do_module_begin(form, env, rec, drec);
}
