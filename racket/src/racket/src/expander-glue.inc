/* This file is #included by expander.inc when it is built via cify */

#include "schmach.h"

/* Disable the use of source or bytecode: */
#define SCHEME_STARTUP_DEFINED

static void scheme_instance_add(Scheme_Instance *inst, const char *name, Scheme_Object *val)
{
  Scheme_Bucket *b;
  b = scheme_instance_variable_bucket(scheme_intern_symbol(name), inst);
  b->val = val;
  ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST | GLOB_IS_CONSISTENT;
}

XFORM_NONGCING static int check_overflow()
{
#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      return 1;
    }
  }
#endif
  return 0;
}

static Scheme_Object *do_apply_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return _scheme_apply_multi(o, p->ku.k.i1, argv);
}

static Scheme_Object *handle_overflow(Scheme_Object *proc, int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object **argv2;
  argv2 = MALLOC_N(Scheme_Object*, argc);
  memcpy(argv2, argv, sizeof(Scheme_Object *) * argc);
  memset(argv, 0, sizeof(Scheme_Object *) * argc); /* space safety */
  p->ku.k.p1 = (void *)proc;
  p->ku.k.i1 = argc;
  p->ku.k.p2 = (void *)argv2;
  return scheme_handle_stack_overflow(do_apply_k);
}

static Scheme_Object *__make_args_list(int argc, Scheme_Object **argv, int skip)
{
  Scheme_Object *l = scheme_null;
  while (argc-- > skip) {
    l = scheme_make_pair(argv[argc], l);
  }
  return l;
}

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

XFORM_NONGCING static int __same_obj(Scheme_Object *a, Scheme_Object *b)
{
  return SAME_OBJ(a, b);
}

XFORM_NONGCING static int __is_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  if (SCHEME_CHAPERONEP(v))
    v = SCHEME_CHAPERONE_VAL(v);
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

XFORM_NONGCING static int __is_authentic_struct_instance(Scheme_Object *v, Scheme_Object *_st)
{
  Scheme_Struct_Type *st = (Scheme_Struct_Type *)_st;
  return (SCHEME_STRUCTP(v)
          && (((Scheme_Structure *)v)->stype->parent_types[st->name_pos] == st));
}

static Scheme_Object *__struct_ref(Scheme_Object *v, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    return scheme_struct_ref(v, pos);
  else
    return ((Scheme_Structure *)v)->slots[pos];
}

XFORM_NONGCING static Scheme_Object *__authentic_struct_ref(Scheme_Object *v, int pos)
{
  return ((Scheme_Structure *)v)->slots[pos];
}

static Scheme_Object *__struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  if (SCHEME_CHAPERONEP(v))
    scheme_struct_set(v, pos, a);
  else
    ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

XFORM_NONGCING static Scheme_Object *__authentic_struct_set(Scheme_Object *v, Scheme_Object *a, int pos)
{
  ((Scheme_Structure *)v)->slots[pos] = a;
  return scheme_void;
}

XFORM_NONGCING static int __int_lt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) < SCHEME_INT_VAL(b);
}

XFORM_NONGCING static int __int_gt(Scheme_Object *a, Scheme_Object *b)
{
  return SCHEME_INT_VAL(a) > SCHEME_INT_VAL(b);
}

XFORM_NONGCING static Scheme_Object *__int_add(Scheme_Object *a, Scheme_Object *b)
{
  return scheme_make_integer(SCHEME_INT_VAL(a) + SCHEME_INT_VAL(b));
}

XFORM_NONGCING static Scheme_Object *__authentic_vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static Scheme_Object *__vector_ref(Scheme_Object *v, Scheme_Object *i)
{
  if (SCHEME_NP_CHAPERONEP(v))
    return scheme_chaperone_vector_ref(v, SCHEME_INT_VAL(i));
  else
    return SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)];
}

static Scheme_Object *__vector_set(Scheme_Object *v, Scheme_Object *i, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(v))
    scheme_chaperone_vector_set(v, SCHEME_INT_VAL(i), a);
  SCHEME_VEC_ELS(v)[SCHEME_INT_VAL(i)] = a;
  return scheme_void;
}

static Scheme_Object *__vector_length(Scheme_Object *v)
{
  if (SCHEME_NP_CHAPERONEP(v)) v = SCHEME_CHAPERONE_VAL(v);
  return scheme_make_integer(SCHEME_VEC_SIZE(v));
}

static Scheme_Object *__box_ref(Scheme_Object *b)
{
  if (SCHEME_NP_CHAPERONEP(b))
    return scheme_unbox(b);
  else
    return SCHEME_BOX_VAL(b);
}

static Scheme_Object *__box_set(Scheme_Object *b, Scheme_Object *a)
{
  if (SCHEME_NP_CHAPERONEP(b))
    scheme_set_box(b, a);
  else
    SCHEME_BOX_VAL(b) = a;
  return scheme_void;
}

XFORM_NONGCING static Scheme_Object *__pair_car(Scheme_Object *p)
{
  return SCHEME_CAR(p);
}

XFORM_NONGCING static Scheme_Object *__pair_cdr(Scheme_Object *p)
{
  return SCHEME_CDR(p);
}

XFORM_NONGCING static Scheme_Object *__pair_caar(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CAR(p));
}

XFORM_NONGCING static Scheme_Object *__pair_cdar(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CAR(p));
}

XFORM_NONGCING static Scheme_Object *__pair_cadr(Scheme_Object *p)
{
  return SCHEME_CAR(SCHEME_CDR(p));
}

XFORM_NONGCING static Scheme_Object *__pair_cddr(Scheme_Object *p)
{
  return SCHEME_CDR(SCHEME_CDR(p));
}

#define SCHEME_UNBOX_VARIABLE(var) (*(Scheme_Object **)(var))
#define SCHEME_UNBOX_VARIABLE_LHS(var) SCHEME_UNBOX_VARIABLE(var)

Scheme_Object *scheme_box_variable(Scheme_Object *v)
{
  Scheme_Object **b;
  b = MALLOC_ONE(Scheme_Object *);
  b[0] = v;
  return (Scheme_Object *)b;
}
