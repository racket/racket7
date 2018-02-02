/* This file is #included by expander.inc when it is built via cify */

/* XFORM is disabled here */

#include "schmach.h"

/* Disable the use of source or bytecode: */
#define SCHEME_STARTUP_DEFINED

#define __RUNSTACK_INIT_VAL NULL

static void scheme_instance_add(Scheme_Instance *inst, const char *name, Scheme_Object *val)
{
  Scheme_Bucket *b;
  b = scheme_instance_variable_bucket(scheme_intern_symbol(name), inst);
  b->val = val;
  ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST | GLOB_IS_CONSISTENT;
}

static MZ_INLINE int __check_runstack_space(int max_depth, Scheme_Object **runstack)
{  
  return ((runstack - MZ_RUNSTACK_START) < (max_depth + SCHEME_TAIL_COPY_THRESHOLD));
}

static int __check_overflow_or_runstack_space(int max_depth, Scheme_Object **runstack)
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      return 1;
    }
  }
#endif
  return __check_runstack_space(max_depth, runstack);
}

static void __check_top_runstack_depth(int max_depth)
{
  if (__check_runstack_space(max_depth, MZ_RUNSTACK)) {
    scheme_log_abort("initial runstack is too small to start up");
    abort();
  }
}


static Scheme_Object *do_apply_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  
  if (__check_runstack_space(p->ku.k.i2, MZ_RUNSTACK)) {
    return (Scheme_Object *)scheme_enlarge_runstack(p->ku.k.i2, (void *(*)())do_apply_k);
  } else {
    Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
    Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;
    
    p->ku.k.p1 = NULL;
    p->ku.k.p2 = NULL;
    
    return _scheme_apply_multi(o, p->ku.k.i1, argv);
  }
}

static Scheme_Object *__handle_overflow_or_space(Scheme_Object *proc, int argc, Scheme_Object **argv, int runstack_space)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **argv2;
  argv2 = MALLOC_N(Scheme_Object*, argc);
  memcpy(argv2, argv, sizeof(Scheme_Object *) * argc);
  memset(argv, 0, sizeof(Scheme_Object *) * argc); /* space safety */
  p->ku.k.p1 = (void *)proc;
  p->ku.k.i1 = argc;
  p->ku.k.i2 = runstack_space;
  p->ku.k.p2 = (void *)argv2;
  
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    return scheme_handle_stack_overflow(do_apply_k);
  }
#endif
  
  return (Scheme_Object *)scheme_enlarge_runstack(runstack_space, (void *(*)())do_apply_k);
}

static Scheme_Object *__ensure_args_in_place_rest(int argc, Scheme_Object **argv, Scheme_Object **runbase,
                                                  int direct_args, int rest_args,
                                                  Scheme_Object *self)
{
  Scheme_Object **runstack = runbase - direct_args - rest_args;
  int i;

  if (argc == direct_args) {
    /* Copy into runbase. If there's a rest arg not supplied, then the
       copy may be shifting down, and we need to add a `null` value
       for the rest arg. */
    for (i = 0; i < direct_args; i++)
      runstack[i] = argv[i];
    if (rest_args)
      argv[direct_args] = scheme_null;
  } else {
    /* Need to build a list and then copy or shift up */
    Scheme_Object *l = scheme_null;
    MZ_GC_DECL_REG(2);
    
    MZ_GC_VAR_IN_REG(0, argv);
    MZ_GC_VAR_IN_REG(1, self);
    MZ_GC_REG();
    for (i = argc; i-- > direct_args; )
      l = scheme_make_pair(argv[i], l);
    MZ_GC_UNREG();
    
    runstack[direct_args] = l;
    for (i = direct_args; i--; )
      runstack[i] = argv[i];
  }

  return self;
}

#define __ensure_args_in_place(argc, argv, runbase) \
  if (argv != (runbase - argc)) (void)__ensure_args_in_place_rest(argc, argv, runbase, argc, 0, NULL)

static mzshort *convert_arities(int mina, const char *a)
{
  /* FIXME: On a big-endian machine, we need to reverse the byte order in arities */
  return (mzshort *)a;
}

static Scheme_Object *scheme_make_prim_w_case_arity(Scheme_Prim *prim, const char *name, mzshort mina, const char *arities)
{
  Scheme_Object *p;
  mzshort *a;
  p = scheme_make_prim_w_arity(prim, name, 0, 0);
  ((Scheme_Primitive_Proc *)p)->mina = mina;
  a = convert_arities(mina, arities);
  ((Scheme_Primitive_Proc *)p)->mu.cases = a;
  return p;
}

static Scheme_Object *scheme_make_prim_closure_w_case_arity(Scheme_Primitive_Closure_Proc *prim,
                                                            int size, Scheme_Object **vals,
                                                            const char *name,
                                                            mzshort mina, const char *arities)
{
  Scheme_Object *p;
  mzshort *a;
  p = scheme_make_prim_closure_w_arity(prim, size, vals, name, 0, 0);
  ((Scheme_Primitive_Proc *)p)->mina = mina;
  a = convert_arities(mina, arities);
  ((Scheme_Primitive_Proc *)p)->mu.cases = a;
  return p;
}

static MZ_INLINE int __same_obj(Scheme_Object *a, Scheme_Object *b)
{
  return SAME_OBJ(a, b);
}

static MZ_INLINE Scheme_Object *__malloc_struct(int c)
{
  return scheme_malloc_tagged(sizeof(Scheme_Structure) + (((c) - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
}

static MZ_INLINE void __struct_set_type(Scheme_Object *s, Scheme_Object *_st)
{
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)_st;
  s->type = (stype->proc_attr ? scheme_proc_struct_type : scheme_structure_type);
  ((Scheme_Structure *)s)->stype = stype;
}

#define __STRUCT_ELS(o) (((Scheme_Structure *)(o))->slots)

static MZ_INLINE int __is_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

static MZ_INLINE int __is_authentic_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

static MZ_INLINE Scheme_Object *__struct_ref(Scheme_Object *v, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    return scheme_struct_ref(v, pos);
  else
    return ((Scheme_Structure *)v)->slots[pos];
}

static MZ_INLINE Scheme_Object *__authentic_struct_ref(Scheme_Object *v, int pos)
{
  return ((Scheme_Structure *)v)->slots[pos];
}

static MZ_INLINE Scheme_Object *__struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    scheme_struct_set(v, pos, a);
  else
    ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *__authentic_struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

static MZ_INLINE int __int_lt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) < SCHEME_INT_VAL(b);
}

static MZ_INLINE int __int_gt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) > SCHEME_INT_VAL(b);
}

static MZ_INLINE Scheme_Object *__int_add(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) + SCHEME_INT_VAL(b));
}

#define __SCHEME_PREDFUNC(id, ID) static MZ_INLINE int id(Scheme_Object *v) { return ID(v); }

__SCHEME_PREDFUNC(__scheme_truep, SCHEME_TRUEP)
__SCHEME_PREDFUNC(__scheme_falsep, SCHEME_FALSEP)
__SCHEME_PREDFUNC(__scheme_nullp, SCHEME_NULLP)
__SCHEME_PREDFUNC(__scheme_boolp, SCHEME_BOOLP)
__SCHEME_PREDFUNC(__scheme_pairp, SCHEME_PAIRP)
__SCHEME_PREDFUNC(__scheme_numberp, SCHEME_NUMBERP)
__SCHEME_PREDFUNC(__scheme_chaperone_vectorp, SCHEME_CHAPERONE_VECTORP)
__SCHEME_PREDFUNC(__scheme_chaperone_boxp, SCHEME_CHAPERONE_BOXP)
__SCHEME_PREDFUNC(__scheme_symbolp, SCHEME_SYMBOLP)
__SCHEME_PREDFUNC(__scheme_keywordp, SCHEME_KEYWORDP)
__SCHEME_PREDFUNC(__scheme_char_stringp, SCHEME_CHAR_STRINGP)
__SCHEME_PREDFUNC(__scheme_byte_stringp, SCHEME_BYTE_STRINGP)

static MZ_INLINE Scheme_Object *__authentic_vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static MZ_INLINE Scheme_Object *__vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  if (SCHEME_NP_CHAPERONEP(v))
    return scheme_chaperone_vector_ref(v, SCHEME_INT_VAL(i));
  else
    return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static MZ_INLINE Scheme_Object *__vector_set(Scheme_Object *v, Scheme_Object *i, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(v))
    scheme_chaperone_vector_set(v, SCHEME_INT_VAL(i), a);
  SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *__vector_length(Scheme_Object *v)
{
  if (SCHEME_NP_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return scheme_make_integer(SCHEME_VEC_SIZE(v));
}

static MZ_INLINE Scheme_Object *__box_ref(Scheme_Object *b)
{
  if (SCHEME_NP_CHAPERONEP(b))
    return scheme_unbox(b);
  else
    return SCHEME_BOX_VAL(b);
}

static MZ_INLINE Scheme_Object *__box_set(Scheme_Object *b, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(b))
    scheme_set_box(b, a);
  else
    SCHEME_BOX_VAL(b) = a;
  return scheme_void;
}

static MZ_INLINE Scheme_Object *__pair_car(Scheme_Object *p)
{
  return SCHEME_CAR(p);
}

static MZ_INLINE Scheme_Object *__pair_cdr(Scheme_Object *p)
{
  return SCHEME_CDR(p);
}

static MZ_INLINE Scheme_Object *__pair_caar(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CAR(p));
}

static MZ_INLINE Scheme_Object *__pair_cdar(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CAR(p));
}

static MZ_INLINE Scheme_Object *__pair_cadr(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CDR(p));
}

static MZ_INLINE Scheme_Object *__pair_cddr(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CDR(p));
}

/* Only when `default` is definitely not a procedure: */
static Scheme_Object *__hash_ref(Scheme_Object *ht, Scheme_Object *key, Scheme_Object *defval)
{
  Scheme_Object *v;
  
  /* The fast path doesn't trigger any GCs: */
  if (SCHEME_HASHTP(ht)) {
    if (!((Scheme_Hash_Table *)ht)->make_hash_indices) {
      v = scheme_eq_hash_get((Scheme_Hash_Table *)ht, key);
      if (v)
        return v;
      else
        return defval;
    }
  } else if (SCHEME_HASHTRP(ht)) {
    if (SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(ht))) {
      v = scheme_eq_hash_tree_get((Scheme_Hash_Tree *)ht, key);
      if (v)
        return v;
      else
        return defval;
    }
  }

  {
    Scheme_Object *argv[3];
    MZ_GC_DECL_REG(3);

    MZ_GC_VAR_IN_REG(0, argv[0]);
    MZ_GC_VAR_IN_REG(1, argv[1]);
    MZ_GC_VAR_IN_REG(2, argv[2]);
    MZ_GC_REG();

    argv[0] = ht;
    argv[1] = key;
    argv[2] = defval;

    v = scheme_checked_hash_ref(3, argv);

    MZ_GC_UNREG();

    return v;
  }
}

static Scheme_Object *__hash_ref2(Scheme_Object *ht, Scheme_Object *key)
{
  Scheme_Object *argv[2], *v;
  MZ_GC_DECL_REG(2);

  MZ_GC_VAR_IN_REG(0, argv[0]);
  MZ_GC_VAR_IN_REG(1, argv[1]);
  MZ_GC_REG();

  argv[0] = ht;
  argv[1] = key;

  v = scheme_checked_hash_ref(2, argv);

  MZ_GC_UNREG();

  return v;
}

static Scheme_Object *__unsafe_immutable_hash_iterate_next(Scheme_Object *ht, Scheme_Object *i)
{
  if (SCHEME_NP_CHAPERONEP(ht)) ht = SCHEME_CHAPERONE_VAL(ht);
  return scheme_unsafe_hash_tree_next((Scheme_Hash_Tree *)ht, i);
}

static Scheme_Object *__zero_values()
{
  Scheme_Thread *p = scheme_current_thread;
  p->ku.multiple.count = 0;
  p->ku.multiple.array = NULL;
  return SCHEME_MULTIPLE_VALUES;
}

static MZ_INLINE Scheme_Object *__last_use(Scheme_Object **r, int i)
{
  Scheme_Object *v = r[i];
  r[i] = NULL;
  return v;
}

/* static MZ_INLINE void __no_use(Scheme_Object **r, int i) { r[i] = NULL; } */
#define __no_use(r, i) r[i] = NULL

#ifndef __VALIDATE_DEBUG

# define SCHEME_UNBOX_VARIABLE(var) (*(Scheme_Object **)(var))
# define SCHEME_UNBOX_VARIABLE_LHS(var) SCHEME_UNBOX_VARIABLE(var)

static Scheme_Object *scheme_box_variable(Scheme_Object *v)
{
  Scheme_Object **b;
  b = MALLOC_ONE(Scheme_Object *);
  b[0] = v;
  return (Scheme_Object *)b;
}

#else

# define SCHEME_UNBOX_VARIABLE(var) SCHEME_BOX_VAL(var)
# define SCHEME_UNBOX_VARIABLE_LHS(var) SCHEME_BOX_VAL(var)

static Scheme_Object *scheme_box_variable(Scheme_Object *v)
{
  return scheme_box(v);
}

static Scheme_Object *__validate(Scheme_Object *s)
{
  if ((SCHEME_TYPE(s) < 0) || (SCHEME_TYPE(s) > _scheme_last_type_))
    abort();
  return s;
}

#endif
