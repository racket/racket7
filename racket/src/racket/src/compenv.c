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
/*        compile-time env, constructors and simple queries               */
/*========================================================================*/

Scheme_Object *scheme_make_toplevel(mzshort depth, int position, int flags)
{
  Scheme_Toplevel *tl;
  Scheme_Object *v, *pr;

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

  tl = (Scheme_Toplevel *)scheme_malloc_atomic_tagged(sizeof(Scheme_Toplevel));
  tl->iso.so.type = scheme_toplevel_type;
  tl->depth = depth;
  tl->position = position;
  SCHEME_TOPLEVEL_FLAGS(tl) = flags | HIGH_BIT_TO_DISABLE_HASHING;

  if (toplevels_ht->count > TABLE_CACHE_MAX_SIZE) {
    toplevels_ht = scheme_make_hash_table_equal();
  }
  scheme_hash_set_atomic(toplevels_ht, pr, (Scheme_Object *)tl);

  return (Scheme_Object *)tl;
}

Scheme_Object *scheme_toplevel_to_flagged_toplevel(Scheme_Object *_tl, int flags)
{
  Scheme_Toplevel *tl = (Scheme_Toplevel *)_tl;
  return scheme_make_toplevel(tl->depth, tl->position, flags);
}

Scheme_IR_Toplevel *scheme_make_ir_toplevel(Scheme_Object *id, int instance_pos, int variable_pos, int identity_pos, int flags)
{
  Scheme_IR_Toplevel *tl;
  
  tl = MALLOC_ONE_TAGGED(Scheme_IR_Toplevel);
  tl->iso.so.type = scheme_ir_toplevel_type;
  SCHEME_TOPLEVEL_FLAGS(tl) = flags | HIGH_BIT_TO_DISABLE_HASHING;

  tl->name = id;
  tl->instance_pos = instance_pos;
  tl->variable_pos = variable_pos;
  tl->identity_pos = identity_pos;

  return tl;
}

Scheme_IR_Toplevel *scheme_ir_toplevel_to_flagged_toplevel(Scheme_IR_Toplevel *tl, int flags)
{
  return scheme_make_ir_toplevel(tl->name, tl->instance_pos, tl->variable_pos, tl->identity_pos,
                                 SCHEME_TOPLEVEL_FLAGS(tl) | flags);
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

Scheme_IR_Local *scheme_make_ir_local(Scheme_Object *id)
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
scheme_compile_lookup(Scheme_Object *find_id, Scheme_Comp_Env *env, int flags)
{
  Scheme_Object *v;

  v = scheme_hash_tree_get(env->vars, SCHEME_STX_VAL(find_id));

  if (!v)
    v = scheme_hash_get(scheme_startup_env->all_primitives_table, find_id);

  if (!v)
    scheme_wrong_syntax(NULL, NULL, find_id, "free identifier");

  if (SAME_TYPE(SCHEME_TYPE(v), scheme_ir_local_type)) {
    if (!(env->flags & COMP_ENV_DONT_COUNT_AS_USE))
      record_local_use((Scheme_IR_Local *)v, flags);
  }
  
  return v;
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
