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
