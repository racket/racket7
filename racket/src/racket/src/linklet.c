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
#include "schrunst.h"

SHARED_OK static int validate_compile_result = 0;
SHARED_OK static int recompile_every_compile = 0;

static Scheme_Object *primitive_table(int argc, Scheme_Object **argv);

static Scheme_Object *linklet_p(int argc, Scheme_Object **argv);
static Scheme_Object *compile_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *recompile_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *instantiate_linklet(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_import_variables(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_export_variables(int argc, Scheme_Object **argv);

static Scheme_Object *instance_p(int argc, Scheme_Object **argv);
static Scheme_Object *make_instance(int argc, Scheme_Object **argv);
static Scheme_Object *instance_name(int argc, Scheme_Object **argv);
static Scheme_Object *instance_data(int argc, Scheme_Object **argv);
static Scheme_Object *instance_variable_names(int argc, Scheme_Object **argv);
static Scheme_Object *instance_variable_value(int argc, Scheme_Object **argv);
static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object **argv);
static Scheme_Object *instance_unset_variable(int argc, Scheme_Object **argv);

static Scheme_Object *linklet_directory_p(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_directory_to_hash(int argc, Scheme_Object **argv);
static Scheme_Object *hash_to_linklet_directory(int argc, Scheme_Object **argv);

static Scheme_Object *linklet_bundle_p(int argc, Scheme_Object **argv);
static Scheme_Object *linklet_bundle_to_hash(int argc, Scheme_Object **argv);
static Scheme_Object *hash_to_linklet_bundle(int argc, Scheme_Object **argv);

static Scheme_Object *variable_p(int argc, Scheme_Object **argv);
static Scheme_Object *variable_instance(int argc, Scheme_Object **argv);
static Scheme_Object *variable_const_p(int argc, Scheme_Object **argv);

static Scheme_Linklet *compile_and_or_optimize_linklet(Scheme_Object *form, Scheme_Linklet *linklet, Scheme_Object *name);
static Scheme_Linklet *clone_linklet(Scheme_Linklet *linklet);

static Scheme_Object *_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                 int num_instances, Scheme_Instance **instances);

static Scheme_Hash_Tree *push_prefix(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                     int num_instances, Scheme_Instance **instances,
                                     Scheme_Hash_Tree *source_names);
static void pop_prefix(Scheme_Object **rs);
static Scheme_Object *suspend_prefix(Scheme_Object **rs);
static Scheme_Object **resume_prefix(Scheme_Object *v);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_linklet(Scheme_Startup_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  scheme_switch_prim_instance(env, "#%linklet");

  ADD_PRIM_W_ARITY("primitive-table", primitive_table, 1, 2, env);

  ADD_FOLDING_PRIM("linklet?", linklet_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY("compile-linklet", compile_linklet, 1, 2, env);
  ADD_PRIM_W_ARITY("recompile-linklet", recompile_linklet, 1, 1, env);
  ADD_PRIM_W_ARITY2("instantiate-linklet", instantiate_linklet, 2, 3, 0, -1, env);
  ADD_PRIM_W_ARITY("linklet-import-variables", linklet_import_variables, 1, 1, env);
  ADD_PRIM_W_ARITY("linklet-export-variables", linklet_export_variables, 1, 1, env);

  ADD_FOLDING_PRIM("instance?", instance_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY("make-instance", make_instance, 1, 2, env);
  ADD_PRIM_W_ARITY("instance-name", instance_name, 1, 1, env);
  ADD_PRIM_W_ARITY("instance-data", instance_data, 1, 1, env);
  ADD_PRIM_W_ARITY("instance-variable-names", instance_variable_names, 1, 1, env);
  ADD_PRIM_W_ARITY2("instance-variable-value", instance_variable_value, 2, 3, 0, -1, env);
  ADD_PRIM_W_ARITY("instance-set-variable-value!", instance_set_variable_value, 3, 3, env);
  ADD_PRIM_W_ARITY("instance-unset-variable!", instance_unset_variable, 2, 2, env);

  ADD_FOLDING_PRIM("linklet-directory?", linklet_directory_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY("hash->linklet-directory", hash_to_linklet_directory, 1, 1, env);
  ADD_PRIM_W_ARITY("linklet-directory->hash", linklet_directory_to_hash, 1, 1, env);

  ADD_FOLDING_PRIM("linklet-bundle?", linklet_bundle_p, 1, 1, 1, env);
  ADD_PRIM_W_ARITY("hash->linklet-bundle", hash_to_linklet_bundle, 1, 1, env);
  ADD_PRIM_W_ARITY("linklet-bundle->hash", linklet_bundle_to_hash, 1, 1, env);

  ADD_PRIM_W_ARITY("variable-reference?", variable_p, 1, 1, env);
  ADD_PRIM_W_ARITY("variable-reference->instance", variable_instance, 1, 1, env);

  REGISTER_SO(scheme_varref_const_p_proc);
  scheme_varref_const_p_proc = scheme_make_prim_w_arity(variable_const_p, 
                                                        "variable-reference-constant?", 
                                                        1, 1);
  scheme_addto_prim_instance("variable-reference-constant?", scheme_varref_const_p_proc, env);

  scheme_restore_prim_instance(env);

  if (scheme_getenv("PLT_VALIDATE_COMPILE")) {
    /* Enables validation of bytecode as it is generated,
       to double-check that the compiler is producing
       valid bytecode as it should. */
    validate_compile_result = 1;
  }

  {
    /* Enables re-running the optimizer N times on every compilation. */
    const char *s;
    s = scheme_getenv("PLT_RECOMPILE_COMPILE");
    if (s) {
      int i = 0;
      while ((s[i] >= '0') && (s[i] <= '9')) {
        recompile_every_compile = (recompile_every_compile * 10) + (s[i]-'0');
        i++;
      }
      if (recompile_every_compile <= 0)
        recompile_every_compile = 1;
      else if (recompile_every_compile > 32)
        recompile_every_compile = 32;
    }
  }
}

/*========================================================================*/
/*                    linklet and instance functions                      */
/*========================================================================*/

static Scheme_Object *primitive_table(int argc, Scheme_Object *argv[])
{
  Scheme_Hash_Table *table;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("primitive-table", "symbol?", 0, argc, argv);
  if ((argc > 1) && !SCHEME_HASHTRP(argv[1]))
    scheme_wrong_contract("primitive-table", "(and/c hash? immutable?)", 1, argc, argv);

  table = (Scheme_Hash_Table *)scheme_hash_get(scheme_startup_env->primitive_tables, argv[0]);
  if (!table) {
    if (argc > 1) {
      table = scheme_make_hash_table(SCHEME_hash_ptr);
      scheme_hash_set(scheme_startup_env->primitive_tables, argv[0], (Scheme_Object *)table);
    } else
      return scheme_false;
  }

  if (argc < 2)
    return (Scheme_Object *)table;
  else
    return scheme_void;
}

static Scheme_Object *linklet_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *compile_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Object *name, *e;

  if (argc > 1)
    name = argv[1];
  else
    name = scheme_intern_symbol("anonymous-linklet");

  e = argv[0];
  if (!SCHEME_STXP(e))
    e = scheme_datum_to_syntax(e, scheme_false, DTS_CAN_GRAPH);

  return (Scheme_Object *)compile_and_or_optimize_linklet(e, NULL, name);
}

static Scheme_Object *recompile_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Object *name;
    
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("recompile-linklet", "linklet?", 0, argc, argv);

  name = ((Scheme_Linklet *)argv[0])->name;
  
  return (Scheme_Object *)compile_and_or_optimize_linklet(NULL, (Scheme_Linklet *)argv[0], name);
}

static Scheme_Object *instantiate_linklet(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  Scheme_Object *l;
  Scheme_Instance *inst, **instances;
  int len = 0, num_importss;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("instantiate-linklet", "linklet?", 0, argc, argv);

  l = argv[1];
  while (!SCHEME_NULLP(l)) {
    if (!SAME_TYPE(SCHEME_TYPE(SCHEME_CAR(l)), scheme_instance_type))
      break;
    l = SCHEME_CDR(l);
    len++;
  }
  if (!SCHEME_NULLP(l))
    scheme_wrong_contract("instantiate-linklet", "(listof instance?)", 1, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];
  num_importss = SCHEME_VEC_SIZE(linklet->importss);
  if (len != num_importss)
    scheme_contract_error("instantiate-linklet",
                          "given number of instances does not match import count of linklet",
                          "linklet", 1, linklet,
                          "expected imports", 1, scheme_make_integer(num_importss),
                          "given instances", 1, scheme_make_integer(len),
                          NULL);

  if (argc > 2) {
    if (!SAME_TYPE(SCHEME_TYPE(argv[2]), scheme_instance_type))
      scheme_wrong_contract("instantiate-linklet", "instance?", 2, argc, argv);
    inst = (Scheme_Instance *)argv[2];
  } else
    inst = scheme_make_instance(linklet->name, scheme_false);

  instances = MALLOC_N(Scheme_Instance*, len);
  l = argv[1];
  len = 0;
  while (!SCHEME_NULLP(l)) {
    instances[len++] = (Scheme_Instance *)SCHEME_CAR(l);
    l = SCHEME_CDR(l);
  }

  if (argc > 2)
    return _instantiate_linklet_multi(linklet, inst, len, instances);
  else {
    (void)_instantiate_linklet_multi(linklet, inst, len, instances);
    return (Scheme_Object *)inst;
  }
}

static Scheme_Object *linklet_import_variables(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  int i, j;
  Scheme_Object *l, *ll = scheme_null;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("linklet-import-variables", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  for (i = SCHEME_VEC_SIZE(linklet->importss); i--; ) {
    l = scheme_null;
    for (j = SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[i]); j--; ) {
      l = scheme_make_pair(SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[i])[j], l);
    }
    ll = scheme_make_pair(l, ll);
  }

  return ll;
}

static Scheme_Object *linklet_export_variables(int argc, Scheme_Object **argv)
{
  Scheme_Linklet *linklet;
  int i;
  Scheme_Object *l = scheme_null;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_type))
    scheme_wrong_contract("linklet-export-variables", "linklet?", 0, argc, argv);

  linklet = (Scheme_Linklet *)argv[0];

  for (i = linklet->num_exports; i--; ) {
    l = scheme_make_pair(SCHEME_VEC_ELS(linklet->defns)[i], l);
  }

  return l;
}

static Scheme_Object *instance_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *make_instance(int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)scheme_make_instance(argv[0], (argc > 1) ? argv[1] : scheme_false);
}

static Scheme_Object *instance_name(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-name", "instance?", 0, argc, argv);

  return ((Scheme_Instance *)argv[0])->name;
}

static Scheme_Object *instance_data(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-data", "instance?", 0, argc, argv);

  return ((Scheme_Instance *)argv[0])->data;
}
  
static Scheme_Object *instance_variable_names(int argc, Scheme_Object **argv)
{
  Scheme_Bucket_Table *variables;
  Scheme_Bucket *b;
  int i;
  Scheme_Object *l = scheme_null;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-variable-names", "instance?", 0, argc, argv);

  variables = ((Scheme_Instance *)argv[0])->variables;

  for (i = variables->size; i--; ) {
    b = variables->buckets[i];
    if (b && b->val) {
      l = scheme_make_pair((Scheme_Object *)b->key, l);
    }
  }

  return l;
}

static Scheme_Object *instance_variable_value(int argc, Scheme_Object **argv)
{
  Scheme_Instance *inst;
  Scheme_Bucket *b;
    
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-variable-value", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-variable-value", "symbol?", 1, argc, argv);

  inst = (Scheme_Instance *)argv[0];
    
  b = scheme_instance_variable_bucket_or_null(argv[1], inst);
  if (b && b->val)
    return b->val;

  if (argc > 2) {
    if (SCHEME_PROCP(argv[2]))
      return _scheme_tail_apply(argv[2], 0, NULL);
    return argv[2];
  }

  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                   "instance-variable-value: instance variable not found\n"
                   "  instance: %V\n"
                   "  name: %S",
                   inst->name,
                   argv[1]);
  return NULL;
}

static Scheme_Object *instance_set_variable_value(int argc, Scheme_Object **argv)
{
  Scheme_Bucket *b;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-set-variable-value!", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-set-variable-value!", "symbol?", 1, argc, argv);

  b = scheme_instance_variable_bucket(argv[1], (Scheme_Instance *)argv[0]);
  b->val = argv[2];

  return scheme_void;
}

static Scheme_Object *instance_unset_variable(int argc, Scheme_Object **argv)
{
  Scheme_Bucket *b;
    
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_instance_type))
    scheme_wrong_contract("instance-unset-variable!", "instance?", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_contract("instance-unset-variable!", "symbol?", 1, argc, argv);

  b = scheme_instance_variable_bucket(argv[1], (Scheme_Instance *)argv[0]);
  b->val = NULL;

  return scheme_void;
}

static Scheme_Object *linklet_directory_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_directory_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *linklet_directory_to_hash(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_directory_type))
    scheme_wrong_contract("linklet-directory->hash", "linklet-directory?", 0, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *hash_to_linklet_directory(int argc, Scheme_Object **argv)
{
  mzlonglong pos;
  Scheme_Object *k, *v;
  Scheme_Hash_Tree *hash;
  
  if (!SCHEME_HASHTRP(argv[0])
      || !SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(argv[0])))
    scheme_wrong_contract("hash->linklet-directory",
                          "(and/c hash? hash-eq? immutable? (not/c impersonator?))",
                          0, argc, argv);
  hash = (Scheme_Hash_Tree *)argv[0];

  /* mapping: #f -> bundle, sym -> linklet directory */

  pos = scheme_hash_tree_next(hash, -1);
  while (pos != -1) {
    scheme_hash_tree_index(hash, pos, &k, &v);
    if (SCHEME_FALSEP(k)) {
      if (!SAME_TYPE(SCHEME_TYPE(v), scheme_linklet_bundle_type))
        scheme_contract_error("hash->linklet-directory",
                              "value for #f key is not a linklet bundle",
                              "value", 1, v,
                              NULL);
    } else if (SCHEME_SYMBOLP(k)) {
      if (!SAME_TYPE(SCHEME_TYPE(v), scheme_linklet_directory_type))
        scheme_contract_error("hash->linklet-directory",
                              "value for symbol key is not a linklet directory",
                              "key", 1, k,
                              "value", 1, v,
                              NULL);
    } else {
      scheme_contract_error("hash->linklet-directory",
                            "key in given hash is not #f or a symbol",
                            "key", 1, k,
                            NULL);
    }
    pos = scheme_hash_tree_next(hash, pos);
  }

  v = scheme_alloc_small_object();
  v->type = scheme_linklet_directory_type;
  SCHEME_PTR_VAL(v) = argv[0];
  return v;
}

static Scheme_Object *linklet_bundle_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_bundle_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *linklet_bundle_to_hash(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_linklet_bundle_type))
    scheme_wrong_contract("linklet-bundle->hash", "linklet-bundle?", 0, argc, argv);

  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *hash_to_linklet_bundle(int argc, Scheme_Object **argv)
{
  mzlonglong pos;
  Scheme_Object *k, *v;
  Scheme_Hash_Tree *hash;
  
  if (!SCHEME_HASHTRP(argv[0])
      || !SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(argv[0])))
    scheme_wrong_contract("hash->linklet-bundle",
                          "(and/c hash? hash-eq? immutable? (not/c impersonator?))",
                          0, argc, argv);

  hash = (Scheme_Hash_Tree *)argv[0];

  /* mapping: keys must be symbols and fixnums */

  pos = scheme_hash_tree_next(hash, -1);
  while (pos != -1) {
    scheme_hash_tree_index(hash, pos, &k, &v);
    if (!SCHEME_SYMBOLP(k) && !SCHEME_INTP(k)) {
      scheme_contract_error("hash->linklet-bundle",
                            "key in given hash is not a symbol or fixnum",
                            "key", 1, k,
                            NULL);
    }
    pos = scheme_hash_tree_next(hash, pos);
  }

  v = scheme_alloc_small_object();
  v->type = scheme_linklet_bundle_type;
  SCHEME_PTR_VAL(v) = argv[0];
  return v;
}

static Scheme_Object *variable_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_global_ref_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *variable_instance(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference-instance", "variable-reference?", 0, argc, argv);

  v = SCHEME_PTR2_VAL(argv[0]);
  if (SCHEME_FALSEP(v))
    return scheme_false;

  return v;
}

static Scheme_Object *variable_const_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(v), scheme_global_ref_type))
    scheme_wrong_contract("variable-reference-constant?", "variable-reference?", 0, argc, argv);

  if (SCHEME_VARREF_FLAGS(v) & 0x1)
    return scheme_true;

  v = SCHEME_PTR1_VAL(v);
  if (((Scheme_Bucket_With_Flags *)v)->flags & GLOB_IS_IMMUTATED)
    return scheme_true;

  return scheme_false;
}

/*========================================================================*/
/*                       instance variable buckets                        */
/*========================================================================*/

Scheme_Object *scheme_get_home_weak_link(Scheme_Instance *i)
{
  if (!i->weak_self_link) {
    Scheme_Object *wb;
    if (scheme_starting_up)
      wb = scheme_box((Scheme_Object *)i);
    else
      wb = scheme_make_weak_box((Scheme_Object *)i);
    i->weak_self_link = wb;
  }

  return i->weak_self_link;
}

Scheme_Instance *scheme_get_bucket_home(Scheme_Bucket *b)
{
  Scheme_Object *l;

  l = ((Scheme_Bucket_With_Home *)b)->home_link;
  if (l) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      return (Scheme_Instance *)l;
    else
      return (Scheme_Instance *)SCHEME_WEAK_BOX_VAL(l);
  } else
    return NULL;
}

void scheme_set_bucket_home(Scheme_Bucket *b, Scheme_Instance *e)
{
  if (!((Scheme_Bucket_With_Home *)b)->home_link) {
    if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_STRONG_HOME_LINK)
      ((Scheme_Bucket_With_Home *)b)->home_link = (Scheme_Object *)e;
    else {
      Scheme_Object *link;
      link = scheme_get_home_weak_link(e);
      ((Scheme_Bucket_With_Home *)b)->home_link = link;
    }
  }
}

Scheme_Instance *scheme_make_instance(Scheme_Object *name, Scheme_Object *data)
{
  Scheme_Instance *inst;
  Scheme_Bucket_Table *variables;
  Scheme_Hash_Tree *ht;

  inst = MALLOC_ONE_TAGGED(Scheme_Instance);
  inst->so.type = scheme_instance_type;

  inst->name = name;
  inst->data = data;

  variables = scheme_make_bucket_table(7, SCHEME_hash_ptr);
  variables->with_home = 1;
  inst->variables = variables;

  ht = scheme_make_hash_tree(0);
  inst->source_names = ht;

  return inst;
}

Scheme_Bucket *scheme_instance_variable_bucket(Scheme_Object *symbol, Scheme_Instance *inst)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_from_table(inst->variables, (char *)symbol);
  ASSERT_IS_VARIABLE_BUCKET(b);
  if (SCHEME_FALSEP(symbol))
    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_STRONG_HOME_LINK;

  scheme_set_bucket_home(b, inst);

  return b;
}

Scheme_Bucket *scheme_instance_variable_bucket_or_null(Scheme_Object *symbol, Scheme_Instance *inst)
{
  Scheme_Bucket *b;
    
  b = scheme_bucket_or_null_from_table(inst->variables, (char *)symbol, 0);
  if (b) {
    ASSERT_IS_VARIABLE_BUCKET(b);
    scheme_set_bucket_home(b, inst);
  }

  return b;
}

/*========================================================================*/
/*                          managing bucket names                         */
/*========================================================================*/

static Scheme_Object *generate_bucket_name(Scheme_Object *old_name, Scheme_Instance *instance)
{
  int search_start = 0;
  char buf[32];
  Scheme_Object *n;
  
  while (1) {
    sprintf(buf, ".%d", search_start);
    n = scheme_intern_exact_parallel_symbol(buf, strlen(buf));
    n = scheme_symbol_append(old_name, n);
    if (!scheme_instance_variable_bucket_or_null(n, instance))
      return n;
    search_start++;
  }
}

static Scheme_Hash_Tree *update_source_names(Scheme_Hash_Tree *source_names,
                                             Scheme_Object *old_name, Scheme_Object *new_name)
{
  Scheme_Object *v;

  v = scheme_hash_tree_get(source_names, old_name);
  if (v)
    return scheme_hash_tree_set(source_names, new_name, v);
  else
    return source_names;
}

/*========================================================================*/
/*                            compiling linklets                          */
/*========================================================================*/

static Scheme_Linklet *compile_and_or_optimize_linklet(Scheme_Object *form, Scheme_Linklet *linklet,
                                                       Scheme_Object *name)
{
  Scheme_Config *config;
  int enforce_const, set_undef, can_inline;
 
  config = scheme_current_config();
  enforce_const = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_COMPILE_MODULE_CONSTS));
  set_undef = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ALLOW_SET_UNDEFINED));
  can_inline = SCHEME_FALSEP(scheme_get_param(config, MZCONFIG_DISALLOW_INLINE));

  if (!linklet) {
    linklet = scheme_compile_linklet(form, set_undef);
    linklet = scheme_letrec_check_linklet(linklet);
  } else {
    linklet = clone_linklet(linklet);
  }
  linklet->name = name;
  linklet = scheme_optimize_linklet(linklet, enforce_const, can_inline);
  linklet = scheme_resolve_linklet(linklet, enforce_const);
  linklet = scheme_sfs_linklet(linklet);
  
  if (recompile_every_compile) {
    int i;
    for (i = recompile_every_compile; i--; ) {
      linklet = scheme_unresolve_linklet(linklet, (set_undef ? COMP_ALLOW_SET_UNDEFINED : 0));
      linklet = scheme_optimize_linklet(linklet, enforce_const, can_inline);
      linklet = scheme_resolve_linklet(linklet, enforce_const);
      linklet = scheme_sfs_linklet(linklet);
    }
  }

  if (validate_compile_result)
    scheme_validate_linklet(NULL, linklet);

  return linklet;
}

Scheme_Linklet *scheme_compile_and_optimize_linklet(Scheme_Object *form, Scheme_Object *name)
{
  return compile_and_or_optimize_linklet(form, NULL, name);
}

static Scheme_Linklet *clone_linklet(Scheme_Linklet *linklet)
{
  Scheme_Linklet *linklet2;

  linklet2 = MALLOC_ONE_TAGGED(Scheme_Linklet);
  memcpy(linklet2, linklet, sizeof(Scheme_Linklet));

  return linklet2;
}
  
/*========================================================================*/
/*                          instantiating linklets                        */
/*========================================================================*/

static Scheme_Object *body_one_expr(void *prefix_plus_expr, int argc, Scheme_Object **argv)
{
  Scheme_Object *v, **saved_runstack;

  saved_runstack = resume_prefix(SCHEME_CAR((Scheme_Object *)prefix_plus_expr));
  v = _scheme_eval_linked_expr_multi(SCHEME_CDR((Scheme_Object *)prefix_plus_expr));
  suspend_prefix(saved_runstack);

  return v;
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

Scheme_Object *scheme_linklet_run_finish(Scheme_Linklet* linklet, Scheme_Instance *instance)
{
  Scheme_Thread *p;
  Scheme_Object *body, **save_runstack, *save_prefix, *v = scheme_void;
  int i, cnt;
  mz_jmp_buf newbuf, * volatile savebuf;

  p = scheme_current_thread;
  savebuf = p->error_buf;
  p->error_buf = &newbuf;

  if (scheme_setjmp(newbuf)) {
    Scheme_Thread *p2;
    p2 = scheme_current_thread;
    p2->error_buf = savebuf;
    scheme_longjmp(*savebuf, 1);
  } else {
    cnt = SCHEME_VEC_SIZE(linklet->bodies);
    for (i = 0; i < cnt; i++) {
      body = SCHEME_VEC_ELS(linklet->bodies)[i];
      if (needs_prompt(body)) {
        /* We need to push the prefix after the prompt is set, so
           restore the runstack and then add the prefix back. */
        save_prefix = suspend_prefix(save_runstack);
        v = _scheme_call_with_prompt_multi(body_one_expr, 
                                           scheme_make_raw_pair(save_prefix, body));
        resume_prefix(save_prefix);

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
                               instance->name);
            }
          }
        }
      } else
        v = _scheme_eval_linked_expr_multi(body);

      if (i < (cnt - 1))
        scheme_ignore_result(v);
    }

    p = scheme_current_thread;
    p->error_buf = savebuf;
  }

  return v;
}

static Scheme_Object *eval_linklet_body(Scheme_Linklet *linklet, Scheme_Instance *instance)
{
#ifdef MZ_USE_JIT
  return scheme_linklet_run_start(linklet, instance, scheme_make_pair(instance->name, scheme_true));
#else
  return scheme_linklet_run_finish(linklet, instance);
#endif
}

static void *instantiate_linklet_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Linklet *linklet = (Scheme_Linklet *)p->ku.k.p1;
  Scheme_Instance *instance = (Scheme_Instance *)p->ku.k.p2;
  Scheme_Instance **instances = (Scheme_Instance **)p->ku.k.p3;
  int multi = p->ku.k.i1;
  int num_instances = p->ku.k.i2;
  int depth;
  Scheme_Object *b, *v;
  Scheme_Object **save_runstack;
  Scheme_Hash_Tree *source_names;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  depth = linklet->max_let_depth;  
  if (!scheme_check_runstack(depth)) {
    p->ku.k.p1 = linklet;
    p->ku.k.p2 = instance;
    p->ku.k.p3 = instances;
    p->ku.k.i1 = multi;
    p->ku.k.i2 = num_instances;
    return (Scheme_Object *)scheme_enlarge_runstack(depth, instantiate_linklet_k);
  }

  b = scheme_get_param(scheme_current_config(), MZCONFIG_USE_JIT);
  if (SCHEME_TRUEP(b))
    linklet = scheme_jit_linklet(linklet);

  /* Pushng the prefix looks up imported variables */
  save_runstack = MZ_RUNSTACK;
  source_names = push_prefix(linklet, instance, num_instances, instances, linklet->source_names);

  /* For variables in this instances, merge source-name info from the
     linklet to the instance */
  if (source_names->count) {
    if (instance->source_names->count) {
      mzlonglong pos;
      Scheme_Hash_Tree *ht = instance->source_names;
      Scheme_Object *k, *v;
      pos = scheme_hash_tree_next(source_names, -1);
      while (pos != -1) {
        scheme_hash_tree_index(source_names, pos, &k, &v);
        ht = scheme_hash_tree_set(ht, k, v);
        pos = scheme_hash_tree_next(source_names, pos);
      }
      instance->source_names = ht;
    } else
      instance->source_names = source_names;
  }

  v = eval_linklet_body(linklet, instance);

  pop_prefix(save_runstack);

  if (!multi)
    v = scheme_check_one_value(v);

  return (void *)v;
}

static Scheme_Object *do_instantiate_linklet(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                             int num_instances, Scheme_Instance **instances,
                                             int multi, int top)
{
  Scheme_Thread *p = scheme_current_thread;
  
  p->ku.k.p1 = linklet;
  p->ku.k.p2 = instance;
  p->ku.k.p3 = instances;
  
  p->ku.k.i1 = multi;
  p->ku.k.i2 = num_instances;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(instantiate_linklet_k, 1);
  else
    return (Scheme_Object *)instantiate_linklet_k();
}

static Scheme_Object *_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                 int num_instances, Scheme_Instance **instances)
{
  return do_instantiate_linklet(linklet, instance, num_instances, instances, 1, 0);
}

Scheme_Object *scheme_instantiate_linklet_multi(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                                int num_instances, Scheme_Instance **instances)
{
  return do_instantiate_linklet(linklet, instance, num_instances, instances, 1, 1);
}

/*========================================================================*/
/*        creating/pushing prefix for top-levels and syntax objects       */
/*========================================================================*/

static Scheme_Hash_Tree *push_prefix(Scheme_Linklet *linklet, Scheme_Instance *instance,
                                     int num_instances, Scheme_Instance **instances,
                                     Scheme_Hash_Tree *source_names)
{
  Scheme_Object **rs, *v;
  Scheme_Prefix *pf;
  int i, j, pos, tl_map_len, num_importss, num_defns;

  rs = MZ_RUNSTACK;

  num_importss = SCHEME_VEC_SIZE(linklet->importss);
  num_defns = SCHEME_VEC_SIZE(linklet->defns);

  i = 1;
  for (j = num_importss; j--; ) {
    i += SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[j]);
  }
  i += num_defns;
  tl_map_len = (i + 31) / 32;

  pf = scheme_malloc_tagged(sizeof(Scheme_Prefix) 
                            + ((i-mzFLEX_DELTA) * sizeof(Scheme_Object *))
                            + (tl_map_len * sizeof(int)));
  pf->iso.so.type = scheme_prefix_type;
  pf->num_slots = i;
  --rs;
  MZ_RUNSTACK = rs;
  rs[0] = (Scheme_Object *)pf;

  pos = 0;
  v = (Scheme_Object *)scheme_instance_variable_bucket(scheme_false, instance);
  pf->a[pos++] = v;
  
  for (j = 0; j < num_importss; j++) {
    int num_imports = SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[j]);
    for (i = 0; i < num_imports; i++) {
      v = SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[j])[i];
      v = (Scheme_Object *)scheme_instance_variable_bucket(v, (Scheme_Instance *)instances[j]);
      if (!v) {
        scheme_signal_error("instantiate-linklet: mismatch;\n"
                            " possibly, bytecode file needs re-compile because dependencies changed\n"
                            "  name: %V\n"
                            "  exporting instance: %V\n"
                            "  importing instance: %V",
                            SCHEME_VEC_ELS(SCHEME_VEC_ELS(linklet->importss)[j])[i],
                            instances[j]->name,
                            instance->name);
      }
      pf->a[pos++] = v;
    }
  }

  for (i = 0; i < num_defns; i++) {
    v = SCHEME_VEC_ELS(linklet->defns)[i];
    if (i >= linklet->num_exports) {
      /* avoid conflict with any existing bucket */
      if (scheme_instance_variable_bucket_or_null(v, instance)) {
        v = generate_bucket_name(v, instance);
        source_names = update_source_names(source_names, SCHEME_VEC_ELS(linklet->defns)[i], v);
      }
    }
    v = (Scheme_Object *)scheme_instance_variable_bucket(v, instance);
    pf->a[pos++] = v;
  }

  return source_names;
}

static void pop_prefix(Scheme_Object **rs)
{
  /* This function must not allocate, since a relevant multiple-values
     result may be in the thread record (and we don't want it zerod) */
  MZ_RUNSTACK = rs;
}

static Scheme_Object *suspend_prefix(Scheme_Object **rs)
{
  if (rs != MZ_RUNSTACK) {
    Scheme_Object *v;
    v = MZ_RUNSTACK[0];
    MZ_RUNSTACK++;
    return v;
  } else
    return NULL;
}

static Scheme_Object **resume_prefix(Scheme_Object *v)
{
  if (v) {
    --MZ_RUNSTACK;
    MZ_RUNSTACK[0] = v;
    return MZ_RUNSTACK + 1;
  } else
    return MZ_RUNSTACK;
}

#ifdef MZ_PRECISE_GC
static void mark_pruned_prefixes(struct NewGC *gc) XFORM_SKIP_PROC
{
  if (!GC_is_partial(gc)) {
    if (scheme_inc_prefix_finalize != (Scheme_Prefix *)0x1) {
      Scheme_Prefix *pf = scheme_inc_prefix_finalize;
      while (pf->next_final != (Scheme_Prefix *)0x1) {
        pf = pf->next_final;
      }
      pf->next_final = scheme_prefix_finalize;
      scheme_prefix_finalize = scheme_inc_prefix_finalize;
      scheme_inc_prefix_finalize = (Scheme_Prefix *)0x1;
    }
  }
  
  if (scheme_prefix_finalize != (Scheme_Prefix *)0x1) {
    Scheme_Prefix *pf = scheme_prefix_finalize, *next;
    Scheme_Object *clo;
    int i, *use_bits, maxpos;
    
    scheme_prefix_finalize = (Scheme_Prefix *)0x1;
    while (pf != (Scheme_Prefix *)0x1) {
      /* If not marked, only references are through closures: */
      if (!GC_is_marked2(pf, gc)) {
        /* Clear slots that are not use in map */
        maxpos = pf->num_slots;
        use_bits = PREFIX_TO_USE_BITS(pf);
        for (i = (maxpos + 31) / 32; i--; ) {
          int j;
          for (j = 0; j < 32; j++) {
            if (!(use_bits[i] & ((unsigned)1 << j))) {
              int pos;
              pos = (i * 32) + j;
              pf->a[pos] = NULL;
            }
          }
          use_bits[i] = 0;
        }
        /* Should mark/copy pf, but not trigger or require mark propagation: */
#ifdef MZ_GC_BACKTRACE
        GC_set_backpointer_object(pf->backpointer);
#endif
        GC_mark_no_recur(gc, 1);
        gcMARK2(pf, gc);
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);
        GC_retract_only_mark_stack_entry(pf, gc);
        GC_mark_no_recur(gc, 0);
      } else
        pf = (Scheme_Prefix *)GC_resolve2(pf, gc);

      /* Clear use map */
      use_bits = PREFIX_TO_USE_BITS(pf);
      maxpos = pf->num_slots;
      for (i = (maxpos + 31) / 32; i--; )
        use_bits[i] = 0;

      /* Fix up closures that reference this prefix: */
      clo = (Scheme_Object *)GC_resolve2(pf->fixup_chain, gc);
      pf->fixup_chain = NULL;
      while (clo) {
        Scheme_Object *next;
        if (SCHEME_TYPE(clo) == scheme_closure_type) {
          Scheme_Closure *cl = (Scheme_Closure *)clo;
          int closure_size = ((Scheme_Lambda *)GC_resolve2(cl->code, gc))->closure_size;
          next = cl->vals[closure_size - 1];
          cl->vals[closure_size-1] = (Scheme_Object *)pf;
        } else if (SCHEME_TYPE(clo) == scheme_native_closure_type) {
          Scheme_Native_Closure *cl = (Scheme_Native_Closure *)clo;
          int closure_size = ((Scheme_Native_Lambda *)GC_resolve2(cl->code, gc))->closure_size;
          next = cl->vals[closure_size - 1];
          cl->vals[closure_size-1] = (Scheme_Object *)pf;
        } else {
          MZ_ASSERT(0);
          next = NULL;
        }
        clo = (Scheme_Object *)GC_resolve2(next, gc);
      }
      if (SCHEME_PREFIX_FLAGS(pf) & 0x1)
        SCHEME_PREFIX_FLAGS(pf) -= 0x1;

      /* Next */
      next = pf->next_final;
      pf->next_final = NULL;

      pf = next;
    }
  }
}

int check_pruned_prefix(void *p) XFORM_SKIP_PROC
{
  Scheme_Prefix *pf = (Scheme_Prefix *)p;
  return SCHEME_PREFIX_FLAGS(pf) & 0x1;
}
#endif

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_linklet.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
}

END_XFORM_SKIP;

#endif
