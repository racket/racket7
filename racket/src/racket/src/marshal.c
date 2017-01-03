/*
  Racket
  Copyright (c) 2004-2017 PLT Design Inc.
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

#define cons(a,b) scheme_make_pair(a,b)
#define CONS(a,b) scheme_make_pair(a,b)

static Scheme_Object *write_let_value(Scheme_Object *obj);
static Scheme_Object *read_let_value(Scheme_Object *obj);
static Scheme_Object *write_let_void(Scheme_Object *obj);
static Scheme_Object *read_let_void(Scheme_Object *obj);
static Scheme_Object *write_letrec(Scheme_Object *obj);
static Scheme_Object *read_letrec(Scheme_Object *obj);
static Scheme_Object *write_let_one(Scheme_Object *obj);
static Scheme_Object *read_let_one(Scheme_Object *obj);
static Scheme_Object *write_case_lambda(Scheme_Object *obj);
static Scheme_Object *read_case_lambda(Scheme_Object *obj);

static Scheme_Object *read_define_values(Scheme_Object *obj);
static Scheme_Object *write_define_values(Scheme_Object *obj);
static Scheme_Object *read_set_bang(Scheme_Object *obj);
static Scheme_Object *write_set_bang(Scheme_Object *obj);
static Scheme_Object *read_boxenv(Scheme_Object *obj);
static Scheme_Object *write_boxenv(Scheme_Object *obj);
static Scheme_Object *read_varref(Scheme_Object *obj);
static Scheme_Object *write_varref(Scheme_Object *obj);
static Scheme_Object *read_apply_values(Scheme_Object *obj);
static Scheme_Object *write_apply_values(Scheme_Object *obj);
static Scheme_Object *read_with_immed_mark(Scheme_Object *obj);
static Scheme_Object *write_with_immed_mark(Scheme_Object *obj);
static Scheme_Object *read_inline_variant(Scheme_Object *obj);
static Scheme_Object *write_inline_variant(Scheme_Object *obj);

static Scheme_Object *write_application(Scheme_Object *obj);
static Scheme_Object *read_application(Scheme_Object *obj);
static Scheme_Object *write_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence_save_first(Scheme_Object *obj);
static Scheme_Object *write_branch(Scheme_Object *obj);
static Scheme_Object *read_branch(Scheme_Object *obj);
static Scheme_Object *write_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *read_with_cont_mark(Scheme_Object *obj);

static Scheme_Object *write_toplevel(Scheme_Object *obj);
static Scheme_Object *read_toplevel(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);

static Scheme_Object *write_lambda(Scheme_Object *obj);
static Scheme_Object *read_lambda(Scheme_Object *obj);

static Scheme_Object *write_linklet(Scheme_Object *obj);
static Scheme_Object *read_linklet(Scheme_Object *obj);

static Scheme_Object *closure_marshal_name(Scheme_Object *name);

void scheme_init_marshal(Scheme_Startup_Env *env) 
{
  scheme_install_type_writer(scheme_application_type, write_application);
  scheme_install_type_reader(scheme_application_type, read_application);
  scheme_install_type_writer(scheme_application2_type, write_application);
  scheme_install_type_reader(scheme_application2_type, read_application);
  scheme_install_type_writer(scheme_application3_type, write_application);
  scheme_install_type_reader(scheme_application3_type, read_application);
  scheme_install_type_writer(scheme_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_sequence_type, read_sequence);
  scheme_install_type_writer(scheme_branch_type, write_branch);
  scheme_install_type_reader(scheme_branch_type, read_branch);
  scheme_install_type_writer(scheme_with_cont_mark_type, write_with_cont_mark);
  scheme_install_type_reader(scheme_with_cont_mark_type, read_with_cont_mark);
  scheme_install_type_writer(scheme_begin0_sequence_type, write_sequence);
  scheme_install_type_reader(scheme_begin0_sequence_type, read_sequence_save_first);
  
 scheme_install_type_writer(scheme_let_value_type, write_let_value);
  scheme_install_type_reader(scheme_let_value_type, read_let_value);
  scheme_install_type_writer(scheme_let_void_type, write_let_void);
  scheme_install_type_reader(scheme_let_void_type, read_let_void);
  scheme_install_type_writer(scheme_letrec_type, write_letrec);
  scheme_install_type_reader(scheme_letrec_type, read_letrec);
  scheme_install_type_writer(scheme_let_one_type, write_let_one);
  scheme_install_type_reader(scheme_let_one_type, read_let_one);
  scheme_install_type_writer(scheme_case_lambda_sequence_type, write_case_lambda);
  scheme_install_type_reader(scheme_case_lambda_sequence_type, read_case_lambda);

  scheme_install_type_writer(scheme_define_values_type, write_define_values);
  scheme_install_type_reader(scheme_define_values_type, read_define_values);
  scheme_install_type_writer(scheme_set_bang_type, write_set_bang);
  scheme_install_type_reader(scheme_set_bang_type, read_set_bang);
  scheme_install_type_writer(scheme_boxenv_type, write_boxenv);
  scheme_install_type_reader(scheme_boxenv_type, read_boxenv);
  scheme_install_type_writer(scheme_varref_form_type, write_varref);
  scheme_install_type_reader(scheme_varref_form_type, read_varref);
  scheme_install_type_writer(scheme_apply_values_type, write_apply_values);
  scheme_install_type_reader(scheme_apply_values_type, read_apply_values);
  scheme_install_type_writer(scheme_with_immed_mark_type, write_with_immed_mark);
  scheme_install_type_reader(scheme_with_immed_mark_type, read_with_immed_mark);
  scheme_install_type_writer(scheme_inline_variant_type, write_inline_variant);
  scheme_install_type_reader(scheme_inline_variant_type, read_inline_variant);

  scheme_install_type_writer(scheme_lambda_type, write_lambda);
  scheme_install_type_reader(scheme_lambda_type, read_lambda);

  scheme_install_type_writer(scheme_toplevel_type, write_toplevel);
  scheme_install_type_reader(scheme_toplevel_type, read_toplevel);
  scheme_install_type_writer(scheme_local_type, write_local);
  scheme_install_type_reader(scheme_local_type, read_local);
  scheme_install_type_writer(scheme_local_unbox_type, write_local);
  scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);

  scheme_install_type_writer(scheme_linklet_type, write_linklet);
  scheme_install_type_reader(scheme_linklet_type, read_linklet);
}


static Scheme_Object *write_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)obj;

  return cons(scheme_make_integer(lv->count),
	      cons(scheme_make_integer(lv->position),
		   cons(SCHEME_LET_VALUE_AUTOBOX(lv) ? scheme_true : scheme_false,
			cons(scheme_protect_quote(lv->value), 
			     scheme_protect_quote(lv->body)))));
}

static Scheme_Object *read_let_value(Scheme_Object *obj)
{
  Scheme_Let_Value *lv;
 
  lv = (Scheme_Let_Value *)scheme_malloc_tagged(sizeof(Scheme_Let_Value));
  lv->iso.so.type = scheme_let_value_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->position = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_VALUE_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->value = SCHEME_CAR(obj);
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)obj;

  return cons(scheme_make_integer(lv->count), 
	      cons(SCHEME_LET_VOID_AUTOBOX(lv) ? scheme_true : scheme_false,
		   scheme_protect_quote(lv->body)));
}

static Scheme_Object *read_let_void(Scheme_Object *obj)
{
  Scheme_Let_Void *lv;
 
  lv = (Scheme_Let_Void *)scheme_malloc_tagged(sizeof(Scheme_Let_Void));
  lv->iso.so.type = scheme_let_void_type;
  
  if (!SCHEME_PAIRP(obj)) return NULL;
  lv->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;
  SCHEME_LET_VOID_AUTOBOX(lv) = SCHEME_TRUEP(SCHEME_CAR(obj));
  lv->body = SCHEME_CDR(obj);

  return (Scheme_Object *)lv;
}

static Scheme_Object *write_let_one(Scheme_Object *obj)
{
  scheme_signal_error("let-one writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_let_one(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)obj;
  Scheme_Object *l = scheme_null;
  int i = lr->count;
  
  while (i--) {
    l = cons(scheme_protect_quote(lr->procs[i]), l);
  }

  return cons(scheme_make_integer(lr->count), 
	      cons(scheme_protect_quote(lr->body), l));
}

static Scheme_Object *read_letrec(Scheme_Object *obj)
{
  Scheme_Letrec *lr;
  int i, c;
  Scheme_Object **sa;

  lr = MALLOC_ONE_TAGGED(Scheme_Letrec);

  lr->so.type = scheme_letrec_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  c = lr->count = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  lr->body = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  if (c < 0) return NULL;
  if (c < 4096)
    sa = MALLOC_N(Scheme_Object*, c);
  else {
    sa = scheme_malloc_fail_ok(scheme_malloc, scheme_check_overflow(c, sizeof(Scheme_Object *), 0));
    if (!sa) scheme_signal_error("out of memory allocating letrec bytecode");
  }
  lr->procs = sa;
  for (i = 0; i < c; i++) {
    if (!SCHEME_PAIRP(obj)) return NULL;
    lr->procs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  return (Scheme_Object *)lr;
}

static Scheme_Object *write_case_lambda(Scheme_Object *obj)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)obj;
  int i;
  Scheme_Object *l;

  i = cl->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(cl->array[i], l);
  }
  
  return cons(closure_marshal_name(cl->name),
	      l);
}

static Scheme_Object *read_case_lambda(Scheme_Object *obj)
{
  Scheme_Object *s, *a;
  int count, i, all_closed = 1;
  Scheme_Case_Lambda *cl;

  if (!SCHEME_PAIRP(obj)) return NULL;
  s = SCHEME_CDR(obj);
  for (count = 0; SCHEME_PAIRP(s); s = SCHEME_CDR(s)) {
    count++;
  }

  cl = (Scheme_Case_Lambda *)
    scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
			 + (count - mzFLEX_DELTA) * sizeof(Scheme_Object *));

  cl->so.type = scheme_case_lambda_sequence_type;
  cl->count = count;
  cl->name = SCHEME_CAR(obj);
  if (SCHEME_NULLP(cl->name))
    cl->name = NULL;

  s = SCHEME_CDR(obj);
  for (i = 0; i < count; i++, s = SCHEME_CDR(s)) {
    a = SCHEME_CAR(s);
    cl->array[i] = a;
    if (!SCHEME_PROCP(a)) {
      if (!SAME_TYPE(SCHEME_TYPE(a), scheme_lambda_type))
        return NULL;
      all_closed = 0;
    }
    else {
      if (!SAME_TYPE(SCHEME_TYPE(a), scheme_closure_type))
        return NULL;
    }
  }

  if (all_closed) {
    /* Empty closure: produce procedure value directly.
       (We assume that this was generated by a direct write of
        a case-lambda data record in print.c, and that it's not
	in a CASE_LAMBDA_EXPD syntax record.) */
    return scheme_case_lambda_execute((Scheme_Object *)cl);
  }

  return (Scheme_Object *)cl;
}

static Scheme_Object *read_define_values(Scheme_Object *obj)
{
  if (!SCHEME_VECTORP(obj)) return NULL;

  obj = scheme_clone_vector(obj, 0, 0);
  obj->type = scheme_define_values_type;
  return obj;
}

static Scheme_Object *write_define_values(Scheme_Object *obj)
{
  Scheme_Object *e;

  obj = scheme_clone_vector(obj, 0, 0);
  e = scheme_protect_quote(SCHEME_VEC_ELS(obj)[0]);
  SCHEME_VEC_ELS(obj)[0] = e;

  return obj;
}

static Scheme_Object *read_set_bang(Scheme_Object *obj)
{
  Scheme_Set_Bang *sb;

  sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  sb->so.type = scheme_set_bang_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  sb->set_undef = SCHEME_TRUEP(SCHEME_CAR(obj));

  obj = SCHEME_CDR(obj);
  if (!SCHEME_PAIRP(obj)) return NULL;

  sb->var = SCHEME_CAR(obj);
  sb->val = SCHEME_CDR(obj);

  return (Scheme_Object *)sb;
}

static Scheme_Object *write_set_bang(Scheme_Object *obj)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)obj;
  return scheme_make_pair((sb->set_undef ? scheme_true : scheme_false),
                          scheme_make_pair(sb->var, 
                                           scheme_protect_quote(sb->val)));
}

Scheme_Object *write_varref(Scheme_Object *o)
{
  int is_const = (SCHEME_VARREF_FLAGS(o) & 0x1);

  if (is_const) {
    if (SCHEME_PTR1_VAL(o) != SCHEME_PTR2_VAL(o))
      scheme_signal_error("internal error: expected varref halves to be the same");
  }
  
  return scheme_make_pair((is_const ? scheme_true : SCHEME_PTR1_VAL(o)), 
                          SCHEME_PTR2_VAL(o));
}

Scheme_Object *read_varref(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_varref_form_type;
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  if (SAME_OBJ(SCHEME_CAR(o), scheme_true)) {
    SCHEME_VARREF_FLAGS(data) |= 0x1;
    SCHEME_PTR1_VAL(data) = SCHEME_CDR(o);
  } else
    SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  
  return data;
}

Scheme_Object *write_apply_values(Scheme_Object *o)
{
  return scheme_make_pair(scheme_protect_quote(SCHEME_PTR1_VAL(o)), 
                          scheme_protect_quote(SCHEME_PTR2_VAL(o)));
}

Scheme_Object *read_apply_values(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_apply_values_type;
  SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  
  return data;
}

Scheme_Object *write_with_immed_mark(Scheme_Object *o)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *vec, *v;

  vec = scheme_make_vector(3, NULL);

  v = scheme_protect_quote(wcm->key);
  SCHEME_VEC_ELS(vec)[0] = v;
  v = scheme_protect_quote(wcm->val);
  SCHEME_VEC_ELS(vec)[1] = v;
  v = scheme_protect_quote(wcm->body);
  SCHEME_VEC_ELS(vec)[2] = v;
  
  return vec;
}

Scheme_Object *read_with_immed_mark(Scheme_Object *o)
{
  Scheme_With_Continuation_Mark *wcm;

  if (!SCHEME_VECTORP(o)) return NULL;
  if (SCHEME_VEC_SIZE(o) != 3) return NULL;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_immed_mark_type;
  
  wcm->key = SCHEME_VEC_ELS(o)[0];
  wcm->val = SCHEME_VEC_ELS(o)[1];
  wcm->body = SCHEME_VEC_ELS(o)[2];

  return (Scheme_Object *)wcm;
}

Scheme_Object *write_boxenv(Scheme_Object *o)
{
  return scheme_make_pair(SCHEME_PTR1_VAL(o), SCHEME_PTR2_VAL(o));
}

Scheme_Object *read_boxenv(Scheme_Object *o)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(o)) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_boxenv_type;
  SCHEME_PTR1_VAL(data) = SCHEME_CAR(o);
  SCHEME_PTR2_VAL(data) = SCHEME_CDR(o);
  
  return data;
}

static Scheme_Object *read_inline_variant(Scheme_Object *obj)
{
  Scheme_Object *data;

  if (!SCHEME_PAIRP(obj)) return NULL;

  data = scheme_make_vector(3, scheme_false);
  data->type = scheme_inline_variant_type;
  SCHEME_VEC_ELS(data)[0] = SCHEME_CAR(obj);
  SCHEME_VEC_ELS(data)[1] = SCHEME_CDR(obj);
  /* third slot is filled when linklet->accessible table is made */
  
  return data;
}

static Scheme_Object *write_inline_variant(Scheme_Object *obj)
{
  return scheme_make_pair(SCHEME_VEC_ELS(obj)[0],
                          SCHEME_VEC_ELS(obj)[1]);
}


#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_application(Scheme_Object *obj)
{
  scheme_signal_error("app writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l;
  int i;

  i = ((Scheme_Sequence *)obj)->count;

  l = scheme_null;
  for (; i--; ) {
    l = cons(scheme_protect_quote(((Scheme_Sequence *)obj)->array[i]), l);
  }
  
  return l;
}

static Scheme_Object *read_sequence(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, 1, 1);
}

static Scheme_Object *read_sequence_save_first(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, -2, 1);
}

static Scheme_Object *write_branch(Scheme_Object *obj)
{
  scheme_signal_error("branch writer shouldn't be used");
  return NULL;
}

static Scheme_Object *read_branch(Scheme_Object *obj)
{
  return NULL;
}

static Scheme_Object *write_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = (Scheme_With_Continuation_Mark *)obj;

  return cons(scheme_protect_quote(wcm->key),
	      cons(scheme_protect_quote(wcm->val),
		   scheme_protect_quote(wcm->body)));
}

static Scheme_Object *read_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  if (!SCHEME_PAIRP(obj) || !SCHEME_PAIRP(SCHEME_CDR(obj)))
    return NULL; /* bad .zo */

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->so.type = scheme_with_cont_mark_type;
  wcm->key = SCHEME_CAR(obj);
  wcm->val = SCHEME_CADR(obj);
  wcm->body = SCHEME_CDR(SCHEME_CDR(obj));

  return (Scheme_Object *)wcm;
}

#define BOOL(x) (x ? scheme_true : scheme_false)

static int not_relative_path(Scheme_Object *p, Scheme_Hash_Table *cache)
{
  Scheme_Object *dir, *rel_p;

  dir = scheme_get_param(scheme_current_config(),
                         MZCONFIG_WRITE_DIRECTORY);
  if (SCHEME_TRUEP(dir)) {
    rel_p = scheme_extract_relative_to(p, dir, cache);
    if (SCHEME_PATHP(rel_p))
      return 1;
  }
  
  return 0;
}

static Scheme_Object *closure_marshal_name(Scheme_Object *name)
{
  if (name) {
    if (SCHEME_VECTORP(name)) {
      /* We can only save marshalable src names, which includes
	 paths, symbols, and strings: */
      Scheme_Object *src;
      src = SCHEME_VEC_ELS(name)[1];
      if ((!SCHEME_PATHP(src)
           /* If MZCONFIG_WRITE_DIRECTORY, drop any non-relative path
              (which might happen due to function inlining, for example)
              to avoid embedding absolute paths in bytecode files: */
           || not_relative_path(src, scheme_current_thread->current_mt->path_cache))
	  && !SCHEME_CHAR_STRINGP(src)
	  && !SCHEME_SYMBOLP(src)) {
	/* Just keep the name */
	name = SCHEME_VEC_ELS(name)[0];
      }
    }
  } else
    name = scheme_null;

  return name;
}

static Scheme_Object *write_lambda(Scheme_Object *obj)
{
  Scheme_Lambda *data;
  Scheme_Object *name, *l, *code, *ds, *tl_map;
  int svec_size, pos;
  Scheme_Marshal_Tables *mt;

  data = (Scheme_Lambda *)obj;

  name = closure_marshal_name(data->name);

  svec_size = data->closure_size;
  if (SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS) {
    svec_size += scheme_boxmap_size(data->num_params + data->closure_size);
    {
      int k, mv;
      for (k = data->num_params + data->closure_size; --k; ) {
        mv = scheme_boxmap_get(data->closure_map, k, data->closure_size);
        if (mv > (LAMBDA_TYPE_TYPE_OFFSET + SCHEME_MAX_LOCAL_TYPE))
          scheme_signal_error("internal error: inconsistent closure/argument type");
      }
    }
  }

  if (SCHEME_RPAIRP(data->body)) {
    /* This can happen if loaded bytecode is printed out and the procedure
       body has never been needed before.
       It's also possible in non-JIT mode if an empty closure is embedded 
       as a 3-D value in compiled code. */
    scheme_delay_load_closure(data);
  }

  /* If the body is simple enough, write it directly.
     Otherwise, create a delay indirection so that the body
     is loaded on demand. */
  code = data->body;
  switch (SCHEME_TYPE(code)) {
  case scheme_toplevel_type:
  case scheme_local_type:
  case scheme_local_unbox_type:
  case scheme_true_type:
  case scheme_false_type:
  case scheme_void_type:
    ds = code;
    break;
  default:
    if (SCHEME_NUMBERP(code))
      ds = code;
    else
      ds = NULL;
    break;
  }
  
  if (!ds) {
    mt = scheme_current_thread->current_mt;
    if (mt->pass < 0) {
      /* nothing to do, yet */
      ds = scheme_false;
    } else {
      if (!mt->pass) {
        int key;

        pos = mt->cdata_counter;
        if ((!mt->cdata_map || (pos >= 32))
            && !(pos & (pos - 1))) {
          /* Need to grow the array */
          Scheme_Object **a;
          a = MALLOC_N(Scheme_Object *, (pos ? 2 * pos : 32));
          if (pos)
            memcpy(a, mt->cdata_map, pos * sizeof(Scheme_Object *));
          mt->cdata_map = a;
        }
        mt->cdata_counter++;

        key = pos & 255;
        MZ_OPT_HASH_KEY(&data->iso) = ((int)MZ_OPT_HASH_KEY(&data->iso) & 0x00FF) | (key << 8);
      } else {
        pos = ((int)MZ_OPT_HASH_KEY(&data->iso) & 0xFF00) >> 8;

        while (pos < mt->cdata_counter) {
          ds = mt->cdata_map[pos];
          if (ds) {
            ds = SCHEME_PTR_VAL(ds);
            if (SAME_OBJ(data->body, ds))
              break;
            if (SAME_TYPE(scheme_quote_compilation_type, SCHEME_TYPE(ds)))
              if (SAME_OBJ(data->body, SCHEME_PTR_VAL(ds)))
                break;
          }
          pos += 256;
        }
        if (pos >= mt->cdata_counter) {
          scheme_signal_error("didn't find delay record");
        }
      }

      ds = mt->cdata_map[pos];
      if (!ds) {
        if (mt->pass)
          scheme_signal_error("broken closure-data table\n");

        code = scheme_protect_quote(data->body);

        ds = scheme_alloc_small_object();
        ds->type = scheme_delay_syntax_type;
        SCHEME_PTR_VAL(ds) = code;

        MZ_OPT_HASH_KEY(&((Scheme_Small_Object *)ds)->iso) |= 1; /* => hash on ds, not contained data */

        mt->cdata_map[pos] = ds;
      }
    }
  }

  /* Encode data->tl_map as either a fixnum or a vector of 16-bit values */
  if (!data->tl_map)
    tl_map = scheme_false;
  else if ((uintptr_t)data->tl_map & 0x1) {
    if (((uintptr_t)data->tl_map & 0xFFFFFFF) == (uintptr_t)data->tl_map) {
      /* comfortably a fixnum */
      tl_map = (Scheme_Object *)data->tl_map;
    } else {
      uintptr_t v;
      tl_map = scheme_make_vector(2, NULL);
      v = ((uintptr_t)data->tl_map >> 1) & 0x7FFFFFFF;
      SCHEME_VEC_ELS(tl_map)[0] = scheme_make_integer(v & 0xFFFF);
      SCHEME_VEC_ELS(tl_map)[1] = scheme_make_integer((v >> 16) & 0xFFFF);
    }
  } else {
    int len = ((int *)data->tl_map)[0], i, v;
    tl_map = scheme_make_vector(2 * len, NULL);
    for (i = 0; i < len; i++) {
      v = ((int *)data->tl_map)[i+1];
      SCHEME_VEC_ELS(tl_map)[2*i] = scheme_make_integer(v & 0xFFFF);
      SCHEME_VEC_ELS(tl_map)[(2*i)+1] = scheme_make_integer((v >> 16) & 0xFFFF);
    }
  }

  l = CONS(scheme_make_svector(svec_size,
                               data->closure_map),
           ds);

  if (SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS)
    l = CONS(scheme_make_integer(data->closure_size),
             l);

  return CONS(scheme_make_integer(SCHEME_LAMBDA_FLAGS(data) & 0x7F),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
                        CONS(tl_map,
                             CONS(name,
                                  l)))));
}

static Scheme_Object *read_lambda(Scheme_Object *obj)
{
  Scheme_Lambda *data;
  Scheme_Object *v, *tl_map;

#define BAD_CC "bad compiled closure"
#define X_SCHEME_ASSERT(x, y)

  data  = (Scheme_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Lambda));

  data->iso.so.type = scheme_lambda_type;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  SCHEME_LAMBDA_FLAGS(data) = (short)(SCHEME_INT_VAL(v));

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  data->num_params = SCHEME_INT_VAL(v);
  if (data->num_params < 0) return NULL;

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  if (data->max_let_depth < 0) return NULL;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return NULL;
  tl_map = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (!SCHEME_FALSEP(tl_map)) {
    if (SCHEME_INTP(tl_map))
      data->tl_map = (void *)tl_map;
    else if (SCHEME_VECTORP(tl_map)) {
      int *n, i, len, v1, v2;
      len = SCHEME_VEC_SIZE(tl_map);
      if (len & 0x1)
        return NULL;
      n = (int *)scheme_malloc_atomic(((len/2) + 1) * sizeof(int));
      n[0] = len/2;
      for (i = 0; i < len/2; i++) {
        v1 = SCHEME_INT_VAL(SCHEME_VEC_ELS(tl_map)[2*i]);
        v2 = SCHEME_INT_VAL(SCHEME_VEC_ELS(tl_map)[(2*i) + 1]);
        v2 = ((unsigned int)v2 << 16) | v1;
        n[i+1] = v2;
      }
      if ((len == 2) && (!(n[1] & 0x80000000)))
        data->tl_map = (void *)(intptr_t)(((uintptr_t)n[1] << 1) | 0x1);
      else
        data->tl_map = n;
    } else
      return NULL;
  }

  if (!SCHEME_PAIRP(obj)) return NULL;
  data->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_NULLP(data->name))
    data->name = NULL;

  if (!SCHEME_PAIRP(obj)) return NULL;
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  /* v is an svector or an integer... */
  if (SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS) {
    if (!SCHEME_INTP(v)) return NULL;
    data->closure_size = SCHEME_INT_VAL(v);
    
    if (!SCHEME_PAIRP(obj)) return NULL;
    v = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  data->body = obj;

  if (!SAME_TYPE(scheme_svector_type, SCHEME_TYPE(v))) return NULL;

  if (!(SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS))
    data->closure_size = SCHEME_SVEC_LEN(v);

  if ((SCHEME_LAMBDA_FLAGS(data) & LAMBDA_HAS_TYPED_ARGS))
    if (data->closure_size + scheme_boxmap_size(data->closure_size + data->num_params) != SCHEME_SVEC_LEN(v))
      return NULL;

  data->closure_map = SCHEME_SVEC_VEC(v);

  /* If the closure is empty, create the closure now */
  if (!data->closure_size)
    return scheme_make_closure(NULL, (Scheme_Object *)data, 0);
  else
    return (Scheme_Object *)data;
}


static Scheme_Object *write_toplevel(Scheme_Object *obj)
{
  int pos, flags;
  Scheme_Object *pr;

  pos = SCHEME_TOPLEVEL_POS(obj);
  flags = (SCHEME_TOPLEVEL_FLAGS(obj) & SCHEME_TOPLEVEL_FLAGS_MASK);

  pr = (flags
	? scheme_make_pair(scheme_make_integer(pos),
			   scheme_make_integer(flags))
	: scheme_make_integer(pos));

  return scheme_make_pair(scheme_make_integer(SCHEME_TOPLEVEL_DEPTH(obj)),
			  pr);
}

static Scheme_Object *read_toplevel(Scheme_Object *obj)
{
  int pos, depth, flags;

  if (!SCHEME_PAIRP(obj)) return NULL;

  depth = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (SCHEME_PAIRP(obj)) {
    pos = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
    flags = SCHEME_INT_VAL(SCHEME_CDR(obj)) & SCHEME_TOPLEVEL_FLAGS_MASK;
  } else {
    pos = (int)SCHEME_INT_VAL(obj);
    flags = 0;
  }

  if (depth < 0) return NULL;
  if (pos < 0) return NULL;

  return scheme_make_toplevel(depth, pos, flags);
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *do_read_local(Scheme_Type t, Scheme_Object *obj)
{
  int n, flags;

  if (SCHEME_PAIRP(obj)) {
    flags = (int)SCHEME_INT_VAL(SCHEME_CAR(obj));
    obj = SCHEME_CDR(obj);
  } else
    flags = 0;

  n = (int)SCHEME_INT_VAL(obj);
  if (n < 0) return NULL;

  return scheme_make_local(t, n, flags);
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return do_read_local(scheme_local_type, obj);
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return do_read_local(scheme_local_unbox_type, obj);
}

static Scheme_Object *hash_tree_to_vector(Scheme_Hash_Tree *ht)
{
  Scheme_Object **keys;
  Scheme_Object *vec, *k, *v;
  int i = 0, pos = 0;

  vec = scheme_make_vector(2 * ht->count, NULL);

  keys = scheme_extract_sorted_keys((Scheme_Object *)ht);

  for (i = 0; i < ht->count; i++) {
    k = keys[i];
    v = scheme_hash_tree_get(ht, k);
    SCHEME_VEC_ELS(vec)[pos++] = k;
    SCHEME_VEC_ELS(vec)[pos++] = v;
  }

  return vec;
}

static Scheme_Object *write_linklet(Scheme_Object *obj)
{
  Scheme_Linklet *linklet = (Scheme_Linklet *)obj;
  Scheme_Object *l;

  if (linklet->jit_ready)
    scheme_arg_mismatch("write",
                        "cannot marshal linklet that has been evaluated",
                        obj);

  l = scheme_null;
  
  if (linklet->import_shapes)
    l = scheme_make_pair(linklet->import_shapes, l);
  else
    l = scheme_make_pair(scheme_false, l);

  l = scheme_make_pair(linklet->importss, l);
  l = scheme_make_pair(linklet->defns, l);
  l = scheme_make_pair(hash_tree_to_vector(linklet->source_names), l);

  l = scheme_make_pair(linklet->bodies, l);

  l = scheme_make_pair(scheme_make_integer(linklet->num_exports), l);
  l = scheme_make_pair(scheme_make_integer(linklet->num_lifts), l);
  l = scheme_make_pair(scheme_make_integer(linklet->max_let_depth), l);
  l = scheme_make_pair((linklet->need_instance_access ? scheme_true : scheme_false), l);

  l = scheme_make_pair(linklet->name, l);

  return l;
}

#if 0
# define return_NULL() return (printf("%d\n", __LINE__), NULL)
#else
# define return_NULL() return NULL
#endif

static int is_vector_of_symbols(Scheme_Object *v, int false_ok)
{
  int i;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[i])
        && (!false_ok || !SCHEME_FALSEP(SCHEME_VEC_ELS(v)[i])))
      return 0;
  }

  return 1;
}

static int is_vector_of_shapes(Scheme_Object *v)
{
  int i;
  Scheme_Object *s;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    s = SCHEME_VEC_ELS(v)[i];
    if (SCHEME_TRUEP(s)
        && !SCHEME_SYMBOLP(s)
        && !SCHEME_INTP(s)
        && !SAME_OBJ(s, scheme_true)
        && !SAME_OBJ(s, scheme_void))
      return 0;
  }

  return 1;
}

static int is_vector_of_vector_of_symbols(Scheme_Object *v)
{
  int i;

  if (!SCHEME_VECTORP(v))
    return 0;
  
  for (i = SCHEME_VEC_SIZE(v); i--; ) {
    if (!is_vector_of_symbols(SCHEME_VEC_ELS(v)[i], 0))
      return 0;
  }

  return 1;
}

static Scheme_Object *vector_to_hash_tree(Scheme_Object *vec)
{
  Scheme_Hash_Tree *ht;
  int i = 0;

  if (!SCHEME_VECTORP(vec))
    return NULL;
  if (SCHEME_VEC_SIZE(vec) & 0x1)
    return NULL;

  ht = scheme_make_hash_tree(0);
  for (i = SCHEME_VEC_SIZE(vec) - 2; i >= 0; i -= 2) {
    if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(vec)[i])
        || !SCHEME_SYMBOLP(SCHEME_VEC_ELS(vec)[i+1]))
      return NULL;
    ht = scheme_hash_tree_set(ht, SCHEME_VEC_ELS(vec)[i], SCHEME_VEC_ELS(vec)[i+1]);
  }

  return (Scheme_Object *)ht;
}

static Scheme_Object *read_linklet(Scheme_Object *obj)
{
  Scheme_Linklet *linklet = (Scheme_Linklet *)obj;
  Scheme_Object *e, *a;

  linklet = MALLOC_ONE_TAGGED(Scheme_Linklet);
  linklet->so.type = scheme_linklet_type;

  if (!SCHEME_PAIRP(obj)) return_NULL();
  linklet->name = SCHEME_CAR(obj);
  if (!SCHEME_SYMBOLP(linklet->name)) return_NULL();
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  linklet->need_instance_access = SCHEME_TRUEP(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->max_let_depth = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->num_lifts = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  e = SCHEME_CAR(obj);
  linklet->num_exports = SCHEME_INT_VAL(e);
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!SCHEME_VECTORP(a)) return_NULL();
  linklet->bodies = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = vector_to_hash_tree(SCHEME_CAR(obj));
  if (!a) return_NULL();
  linklet->source_names = (Scheme_Hash_Tree *)a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!is_vector_of_symbols(a, 1)) return_NULL();
  linklet->defns = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!is_vector_of_vector_of_symbols(a)) return_NULL();
  linklet->importss = a;
  obj = SCHEME_CDR(obj);

  if (!SCHEME_PAIRP(obj)) return_NULL();
  a = SCHEME_CAR(obj);
  if (!SCHEME_FALSEP(a)) {
    if (!is_vector_of_shapes(a)) return_NULL();
    linklet->import_shapes = a;
  }
  
  if (linklet->num_exports > SCHEME_VEC_SIZE(linklet->defns))
    return_NULL();
  if (linklet->num_lifts > (SCHEME_VEC_SIZE(linklet->defns) - linklet->num_exports))
    return_NULL();

  {
    int i = 0, j;
    for (j = SCHEME_VEC_SIZE(linklet->importss); j--; ) {
      i += SCHEME_VEC_SIZE(SCHEME_VEC_ELS(linklet->importss)[j]);
    }
    linklet->num_total_imports = i;
  }

  if (linklet->import_shapes) {
    if (linklet->num_total_imports != SCHEME_VEC_SIZE(linklet->import_shapes))
      return_NULL();
  }

  return (Scheme_Object *)linklet;
}
