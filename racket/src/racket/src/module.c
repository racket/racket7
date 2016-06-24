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
