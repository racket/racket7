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
#include "schminc.h"
#include "schmach.h"
#ifdef MZ_USE_FUTURES
# include "future.h"
#endif

#define GLOBAL_TABLE_SIZE 500

/* #define TIME_STARTUP_PROCESS */

/* global flags */
SHARED_OK int scheme_allow_set_undefined;
void scheme_set_allow_set_undefined(int v) { scheme_allow_set_undefined =  v; }
int scheme_get_allow_set_undefined() { return scheme_allow_set_undefined; }
THREAD_LOCAL_DECL(int scheme_starting_up);

/* globals READ-ONLY SHARED */
Scheme_Object *scheme_varref_const_p_proc;

READ_ONLY static Scheme_Object *kernel_symbol;

READ_ONLY Scheme_Startup_Env *scheme_startup_env;

static int builtin_ref_counter;
static int builtin_unsafe_start;

THREAD_LOCAL_DECL(static Scheme_Bucket_Table *literal_string_table);
THREAD_LOCAL_DECL(static Scheme_Bucket_Table *literal_number_table);

/* local functions */
static void init_startup_env(void);
static Scheme_Startup_Env make_startup_env();

static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);

static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

static void boot_module_resolver()
{
  Scheme_Object *boot, *a[2];
  a[0] = scheme_make_pair(scheme_intern_symbol("quote"),
                          scheme_make_pair(scheme_intern_symbol("#%boot"),
                                           scheme_null));
  a[1] = scheme_intern_symbol("boot");
  boot = scheme_dynamic_require(2, a);
  scheme_apply(boot, 0, NULL);
}

void scheme_seal_parameters()
{
  Scheme_Object *seal, *a[2];
  a[0] = scheme_make_pair(scheme_intern_symbol("quote"),
                          scheme_make_pair(scheme_intern_symbol("#%boot"),
                                           scheme_null));
  a[1] = scheme_intern_symbol("seal");
  seal = scheme_dynamic_require(2, a);
  scheme_apply(seal, 0, NULL);
}

void os_platform_init() {
#ifdef UNIX_LIMIT_STACK
  struct rlimit rl;

  getrlimit(RLIMIT_STACK, &rl);
  if (rl.rlim_cur > UNIX_LIMIT_STACK) {
    rl.rlim_cur = UNIX_LIMIT_STACK;
    setrlimit(RLIMIT_STACK, &rl);
  }
#endif
#ifdef UNIX_LIMIT_FDSET_SIZE
  struct rlimit rl;

  getrlimit(RLIMIT_NOFILE, &rl);
  if (rl.rlim_cur > FD_SETSIZE) {
    rl.rlim_cur = FD_SETSIZE;
    setrlimit(RLIMIT_NOFILE, &rl);
  }
#endif
}

Scheme_Env *scheme_restart_instance()
{
  Scheme_Env *env;
  void *stack_base;
  stack_base = (void *) scheme_get_current_os_thread_stack_base();

  /* Reset everything: */
  scheme_do_close_managed(NULL, skip_certain_things);
  scheme_main_thread = NULL;

  scheme_reset_finalizations();
  scheme_init_stack_check();
#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif
  scheme_reset_overflow();

  scheme_make_thread(stack_base);
  scheme_init_error_escape_proc(NULL);
  scheme_init_module_resolver();

  env = scheme_make_empty_env();
  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, (Scheme_Object *)env); 

  scheme_init_port_config();
  scheme_init_port_fun_config();
  scheme_init_error_config();
  scheme_init_logger_config();
  scheme_init_exn_config();

  boot_module_resolver();

  return env;
}

Scheme_Env *scheme_basic_env()
{
  Scheme_Startup_Env *env;
  void *stack_base;

  if (scheme_main_thread) {
    return scheme_restart_instance();
  }
  
  stack_base = (void *) scheme_get_current_os_thread_stack_base();

  os_platform_init();

#ifdef TIME_STARTUP_PROCESS
  printf("#if 0\nengine_instance_init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

#if defined(MZ_LONG_DOUBLE_API_IS_EXTERNAL) || defined(LONG_DOUBLE_STRING_OP_API_IS_EXTERNAL)
  scheme_load_long_double_dll();
#endif

  scheme_starting_up = 1;

  scheme_init_finalization();

  scheme_init_portable_case();
  scheme_init_compenv();
  scheme_init_letrec_check();
  scheme_init_optimize();
  scheme_init_resolve();
  scheme_init_sfs();
  scheme_init_validate();

  scheme_init_process_globals();

  scheme_init_true_false();

#ifdef MZ_PRECISE_GC
  /* scheme_register_traversers(); --- already done in scheme_set_stack_base() */
  register_traversers();
  scheme_init_hash_key_procs();
#endif

  scheme_init_getenv(); /* checks PLTNOJIT */

#ifdef WINDOWS_PROCESSES
  /* Must be called before first scheme_make_thread() */
  scheme_init_thread_memory();
#endif

#ifndef MZ_PRECISE_GC
  scheme_init_ephemerons();
#endif

  /* These calls must be made here so that they allocate out of the master GC */
  scheme_init_symbol_table();
  scheme_init_module_path_table();
  scheme_init_type();
  scheme_init_custodian_extractors();
#ifndef DONT_USE_FOREIGN
  scheme_init_foreign_globals();
#endif
  init_startup_env();

  scheme_init_logging_once();

  scheme_init_compenv_symbol();

#if defined(MZ_PLACES_WAITPID)
  scheme_places_start_child_signal_handler();
#endif

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  GC_switch_out_master_gc();

  scheme_spawn_master_place();
#endif

  /* Create the initial place with its initial namespace */

  env = place_instance_init(stack_base, 1);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  {
    void *signal_handle;
    REGISTER_SO(place_object);
    place_object = (Scheme_Place_Object*) scheme_make_place_object();
    signal_handle = scheme_get_signal_handle();
    GC_set_put_external_event_fd(signal_handle);
    place_object->signal_handle = signal_handle;
  }
#endif

  return env;
}

static void init_startup_env(void)
{
  Scheme_Startup_Env *env;
#ifdef TIME_STARTUP_PROCESS
  intptr_t startt;
#endif

  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  env = make_startup_env();

  REGISTER_SO(scheme_startup_env);
  scheme_startup_env = env;
    
  scheme_defining_primitives = 1;
  builtin_ref_counter = 0;

#ifdef TIME_STARTUP_PROCESS
   printf("init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
# define MZTIMEIT(n, f) (MARK_START_TIME(), f, DONE_TIME(n))
# define MARK_START_TIME() startt = scheme_get_process_milliseconds()
# define DONE_TIME(n) (printf(#n ": %" PRIdPTR "\n", (intptr_t)(scheme_get_process_milliseconds() - startt)))
#else
# define MZTIMEIT(n, f) f
# define MARK_START_TIME() /**/
# define DONE_TIME(n) /**/
#endif

  /* The ordering of the first few init calls is important, so add to
     the end of the list, not the beginning. */
  MZTIMEIT(symbol-type, scheme_init_symbol_type(env));
  MZTIMEIT(fun, scheme_init_fun(env));
  MZTIMEIT(symbol, scheme_init_symbol(env));
  MZTIMEIT(list, scheme_init_list(env));
  MZTIMEIT(number, scheme_init_number(env));
  MZTIMEIT(numarith, scheme_init_numarith(env));
  MZTIMEIT(numcomp, scheme_init_numcomp(env));
  MZTIMEIT(numstr, scheme_init_numstr(env));
  MZTIMEIT(bignum, scheme_init_bignum());
  MZTIMEIT(char-const, scheme_init_char_constants());
  MZTIMEIT(stx, scheme_init_stx(env));
  MZTIMEIT(module, scheme_init_module(env));
  MZTIMEIT(port, scheme_init_port(env));
  MZTIMEIT(portfun, scheme_init_port_fun(env));
  MZTIMEIT(string, scheme_init_string(env));
  MZTIMEIT(vector, scheme_init_vector(env));
  MZTIMEIT(char, scheme_init_char(env));
  MZTIMEIT(bool, scheme_init_bool(env));
  MZTIMEIT(syntax, scheme_init_compile(env));
  MZTIMEIT(eval, scheme_init_eval(env));
  MZTIMEIT(struct, scheme_init_struct(env));
  MZTIMEIT(error, scheme_init_error(env));
#ifndef NO_SCHEME_EXNS
  MZTIMEIT(exn, scheme_init_exn(env));
#endif
  MZTIMEIT(process, scheme_init_thread(env));
  scheme_init_port_wait();
  scheme_init_inspector();
  scheme_init_logger_wait();
  scheme_init_struct_wait();
  MZTIMEIT(reduced, scheme_init_reduced_proc_struct(env));
#ifndef NO_SCHEME_THREADS
  MZTIMEIT(sema, scheme_init_sema(env));
#endif
  MZTIMEIT(read, scheme_init_read(env));
  MZTIMEIT(print, scheme_init_print(env));
  MZTIMEIT(file, scheme_init_file(env));
  MZTIMEIT(dynamic-extension, scheme_init_dynamic_extension(env));
#ifndef NO_REGEXP_UTILS
  MZTIMEIT(regexp, scheme_regexp_initialize(env));
#endif
  MZTIMEIT(params, scheme_init_parameterization());
  MZTIMEIT(futures, scheme_init_futures_once());
  MZTIMEIT(places, scheme_init_places_once());
  MZTIMEIT(linklet, scheme_init_linklet(env));
#ifndef NO_TCP_SUPPORT
  MZTIMEIT(network, scheme_init_network(env));
#endif
  MZTIMEIT(paramz, scheme_init_paramz(env));
  MZTIMEIT(place, scheme_init_place(env));

  scheme_register_network_evts();

  MARK_START_TIME();

  scheme_finish_kernel(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != EXPECTED_PRIM_COUNT) {
    printf("Primitive count %d doesn't match expected count %d\n"
	   "Turn off USE_COMPILED_STARTUP in src/schminc.h\n",
	   builtin_ref_counter, EXPECTED_PRIM_COUNT);
    abort();
  }
#endif

  init_flfxnum(env);
  init_extfl(env);
  init_futures(env);

  builtin_unsafe_start = builtin_ref_counter;
  init_unsafe(env);
  init_foreign(env);
  
  scheme_init_variable_references_constants();

  scheme_init_longdouble_fixup();

  scheme_defining_primitives = 0;
}

static void init_unsafe(Scheme_Startup_Env *env)
{
  scheme_switch_prim_instance(env, "#%unsafe");

  scheme_init_unsafe_number(env);
  scheme_init_unsafe_numarith(env);
  scheme_init_unsafe_numcomp(env);
  scheme_init_unsafe_list(env);
  scheme_init_unsafe_hash(env);
  scheme_init_unsafe_vector(env);
  scheme_init_unsafe_fun(env);

  scheme_init_extfl_unsafe_number(env);
  scheme_init_extfl_unsafe_numarith(env);
  scheme_init_extfl_unsafe_numcomp(env);

  scheme_restore_prim_instance(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT
                              + EXPECTED_UNSAFE_COUNT)) {
    printf("Unsafe count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT
           - EXPECTED_FLFXNUM_COUNT - EXPECTED_EXTFL_COUNT 
           - EXPECTED_FUTURES_COUNT, EXPECTED_UNSAFE_COUNT);
    abort();
  }
#endif
}

static void init_flfxnum(Scheme_Startup_Env *env)
{
  scheme_switch_prim_instance(env, "#%flfxnum");
 
  scheme_init_flfxnum_number(env);
  scheme_init_flfxnum_numarith(env);
  scheme_init_flfxnum_numcomp(env);

  scheme_restore_prim_instance(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT)) {
    printf("Flfxnum count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT, 
           EXPECTED_FLFXNUM_COUNT);
    abort();
  }
#endif
}

static void init_extfl(Scheme_Startup_Env *env)
{
  scheme_switch_prim_instance(env, "#%extfl");

  scheme_init_extfl_number(env);
  scheme_init_extfl_numarith(env);
  scheme_init_extfl_numcomp(env);
  scheme_init_extfl_numstr(env);

  scheme_restore_prim_instance(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT)) {
    printf("extfl count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT,
           EXPECTED_EXTFL_COUNT);
    abort();
  }
#endif
}

static void init_unsafe(Scheme_Startup_Env *env);
static void init_flfxnum(Scheme_Startup_Env *env);
static void init_extfl(Scheme_Startup_Env *env);
static void init_futures(Scheme_Startup_Env *env);
static void init_foreign(Scheme_Startup_Env *env);

static void init_futures(Scheme_Startup_Env *env)
{
  scheme_switch_prim_instance(env, "#%futures");

  scheme_init_futures(env);

  scheme_restore_prim_instance(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT)) {
    printf("Futures count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT
           - EXPECTED_EXTFL_COUNT, 
           EXPECTED_FUTURES_COUNT);
    abort();
  }
#endif
}

static void init_foreign(Scheme_Startup_Env *env)
{
  scheme_init_foreign(env);

#if USE_COMPILED_STARTUP
  if (builtin_ref_counter != (EXPECTED_PRIM_COUNT + EXPECTED_FLFXNUM_COUNT
                              + EXPECTED_EXTFL_COUNT + EXPECTED_FUTURES_COUNT
                              + EXPECTED_UNSAFE_COUNT + EXPECTED_FOREIGN_COUNT)) {
    printf("Foreign count %d doesn't match expected count %d\n",
	   builtin_ref_counter - EXPECTED_PRIM_COUNT - EXPECTED_FLFXNUM_COUNT
           - EXPECTED_EXTFL_COUNT - EXPECTED_FUTURES_COUNT
           - EXPECTED_UNSAFE_COUNT,
           EXPECTED_FOREIGN_COUNT);
    abort();
  }
#endif
}

/*========================================================================*/
/*                     place-specific intialization                       */
/*========================================================================*/

static Scheme_Env *place_instance_init(void *stack_base, int initial_main_os_thread)
{
  Scheme_Env *env;

#ifdef TIME_STARTUP_PROCESS
  printf("place_init @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif
  scheme_set_current_os_thread_stack_base(stack_base);

#ifndef MZ_PRECISE_GC
  scheme_init_setjumpup();
#endif

  scheme_init_stack_check();
  scheme_init_overflow();

  scheme_init_thread_lwc();

  scheme_init_compenv_places();

#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

  scheme_init_file_places();

  scheme_make_thread(stack_base);

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  /* each place now has a local symbol table */
  scheme_init_place_local_symbol_table();
#endif

  {
    Scheme_Object *sym;
    sym = scheme_intern_symbol("mzscheme");
    scheme_current_thread->name = sym;
  }

  scheme_init_stx_places(initial_main_os_thread);

  scheme_init_syntax_bindings();

  scheme_init_module_resolver();

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

  /* error handling and buffers */
  /* this check prevents initializing orig ports twice for the first initial
   * place.  The kernel initializes orig_ports early. */
  scheme_init_fun_places();
  scheme_init_port_places();
  scheme_init_error_escape_proc(NULL);
  scheme_init_print_buffers_places();
  scheme_init_thread_places();
  scheme_init_string_places();
  scheme_init_logger();
  scheme_init_eval_places();
  scheme_init_compile_places();
  scheme_init_regexp_places();
  scheme_init_sema_places();
  scheme_init_gmp_places();
  scheme_init_kqueue();
  scheme_alloc_global_fdset();
#ifndef DONT_USE_FOREIGN
  scheme_init_foreign_places();
#endif

  env = scheme_make_empty_env();
  scheme_set_param(scheme_current_config(), MZCONFIG_ENV, (Scheme_Object *)env); 
 
  /*initialize config */
  scheme_init_port_config();
  scheme_init_port_fun_config();
  scheme_init_error_config();
  scheme_init_logger_config();
#ifndef NO_SCHEME_EXNS
  scheme_init_exn_config();
#endif
  scheme_init_error_config();
  scheme_init_place_per_place();
  
#if defined(MZ_USE_PLACES) && defined(MZ_USE_JIT)
  scheme_jit_fill_threadlocal_table();
#endif
  scheme_init_futures_per_place();


  REGISTER_SO(literal_string_table);
  REGISTER_SO(literal_number_table);
  literal_string_table = scheme_make_weak_equal_table();
  literal_number_table = scheme_make_weak_eqv_table();

  scheme_starting_up = 1; /* in case it's not set already */

#ifdef TIME_STARTUP_PROCESS
  printf("pre-embedded @ %" PRIdPTR "\n", scheme_get_process_milliseconds());
#endif

  scheme_add_embedded_builtins(env);

  boot_module_resolver();

  scheme_starting_up = 0;

  --scheme_current_thread->suspend_break; /* created with breaks suspended */

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %" PRIdPTR "\n#endif\n", scheme_get_process_milliseconds());
#endif

#if defined(MZ_USE_PLACES)
  REGISTER_SO(place_channel_links);
#endif

  return env;
}

#ifdef MZ_USE_PLACES
Scheme_Startup_Env *scheme_place_instance_init(void *stack_base, struct NewGC *parent_gc, intptr_t memory_limit)
{
  Scheme_Env *env;
# if defined(MZ_PRECISE_GC)
  int *signal_fd;
  GC_construct_child_gc(parent_gc, memory_limit);
# endif
  env = place_instance_init(stack_base, 0);
# if defined(MZ_PRECISE_GC)
  signal_fd = scheme_get_signal_handle();
  GC_set_put_external_event_fd(signal_fd);
# endif
  scheme_set_can_break(1);
  return env; 
}
#endif

static void force_more_closed(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  /* no need to shut down threads: */
  if (!f || SCHEME_THREADP(o))
    return;

  f(o, data);
}

static void force_more_closed_after(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  scheme_run_atexit_closers(o, f, data);
  force_more_closed(o, f, data);
}

void scheme_place_instance_destroy(int force)
{
  /* run atexit handlers to flush file ports, and also
     force file-stream ports closed */
  if (force)
    scheme_run_atexit_closers_on_all(force_more_closed);
  else
    scheme_run_atexit_closers_on_all(force_more_closed_after);

#ifdef WINDOWS_PROCESSES
  scheme_release_process_job_object();
#endif

  scheme_release_file_descriptor();

  scheme_end_futures_per_place();
#if defined(MZ_USE_PLACES)
  scheme_kill_green_thread_timer();
  scheme_free_place_bi_channels();
#endif
#if defined(MZ_PRECISE_GC) && defined(MZ_USE_PLACES)
  GC_destruct_child_gc();
#endif
  scheme_free_all_code();
  scheme_free_ghbn_data();
  scheme_release_kqueue();
  scheme_release_inotify();
}

/* Shutdown procedure for resetting a namespace: */
static void skip_certain_things(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if ((o == scheme_orig_stdin_port)
      || (o == scheme_orig_stdout_port)
      || (o == scheme_orig_stderr_port))
    return;

  /* f is NULL for threads */
  if (f)
    f(o, data);
}

/*========================================================================*/
/*                     instances and startup env                          */
/*========================================================================*/

static Scheme_Startup_Env *make_startup_env(void)
{
  Scheme_Startup_Env *e;
  Scheme_Hash_Table *table;
  Scheme_Hash_Table *primitive_tables;

  e = MALLOC_ONE_TAGGED(Scheme_Startup_Env);
  e->so.type = scheme_startup_env_type;

  primitive_tables = scheme_make_hash_table(SCHEME_hash_ptr);
  e->primitive_tables = primitive_instances;

  table = scheme_make_hash_table(SCHEME_hash_ptr);
  e->current_table = table;
  scheme_hash_set(e->primitive_tables, kernel_symbol, (Scheme_Object *)table);

  table = scheme_make_hash_table(SCHEME_hash_ptr);
  e->all_primitives_table = table;

  table = scheme_make_hash_table(SCHEME_hash_ptr);
  e->primitive_ids_table = table;

  return e;
}

void scheme_switch_prim_instance(Scheme_Startup_Env *env, const char *name)
{
  Scheme_Hash_Table *table;
  Scheme_Object *sym;

  sym = scheme_intern_symbol(name);
  
  table = (Scheme_Hash_Table *)scheme_hash_get(env->primitive_tables, sym);
  if (!table) {
    table = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(e->primitive_tables, sym, (Scheme_Object *)table);
  }

  e->current_table = table;
}

void scheme_restore_prim_instance(Scheme_Startup_Env *env)
{
  Scheme_Hash_Table *table;
  table = (Scheme_Hash_Table *)scheme_hash_get(env->primitive_tables, kernel_symbol);
  e->current_table = tables;
}

void scheme_add_to_primitive_instance(const char *name, Scheme_Object *obj,
                                      Scheme_Startup_Env *env)
{
  scheme_add_to_primitive_intance_by_symbol(scheme_intern_symbol(name), obj, env->current_instance);
}

void
scheme_add_to_primitive_intance_by_symbol(Scheme_Object *name, Scheme_Object *obj,
                                          Scheme_Startup_Env *env)
{
  scheme_hash_set(env->current_table, name, obj);
  scheme_hash_set(env->all_primitives_table, name, obj);

  scheme_hash_set(env->primitive_ids_table, obj, scheme_make_integer(builtin_ref_counter));
  builtin_ref_counter++;
}

Scheme_Object **scheme_make_builtin_references_table(int *_unsafe_start)
{
  Scheme_Object **t, v;
  int i;

  t = MALLOC_N(Scheme_Object *, (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  for (j = builtin_ref_counter + 1; j--; ) {
    t[j] = scheme_false;
  }

  for (i = scheme_startup_env->primitive_ids_table->size; i--; ) {
    v = scheme_startup_env->primitive_ids_table->vals[i];
    if (v) {
      t[SCHEME_INT_VAL(scheme_startup_env->primitive_ids_table->keys[i])] = v;
    }
  }

  *_unsafe_start = builtin_unsafe_start;

  return t;
}

const char *scheme_look_for_primitive(void *code)
{
  intptr_t i;
  Scheme_Object *val;

  for (i = scheme_startup_env->primitive_ids_table->size; i--; ) {
    val = scheme_startup_env->primitive_ids_table->vals[i];
    if (val && SCHEME_PRIMP(val)) {
      if (SCHEME_PRIM(val) == code)
        return ((Scheme_Primitive_Proc *)val)->name;
    }
  }

  return NULL;
}

/*========================================================================*/
/*                           namespace bindings                           */
/*========================================================================*/

Scheme_Object *
scheme_lookup_global(Scheme_Object *symbol, Scheme_Env *env)
{
  ...
}

Scheme_Bucket *
scheme_global_bucket(Scheme_Object *symbol, Scheme_Env *env)
{
  ...
}

void
scheme_add_global(const char *name, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 1, 0);
}

void
scheme_add_global_symbol(Scheme_Object *sym, Scheme_Object *obj, Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, sym, obj, 1, 0);
}

void
scheme_add_global_keyword(const char *name, Scheme_Object *obj, 
			  Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, scheme_intern_symbol(name), obj, 0, 0);
}

void
scheme_add_global_keyword_symbol(Scheme_Object *name, Scheme_Object *obj, 
				 Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, name, obj, 0, 0);
}

/*========================================================================*/
/*                  intern literal strings and numbers                    */
/*========================================================================*/

Scheme_Object *scheme_intern_literal_string(Scheme_Object *str)
{
  Scheme_Bucket *b;

  scheme_start_atomic();
  b = scheme_bucket_from_table(literal_string_table, (const char *)str);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return(Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

Scheme_Object *scheme_intern_literal_number(Scheme_Object *num)
{
  Scheme_Bucket *b;

  scheme_start_atomic();
  b = scheme_bucket_from_table(literal_number_table, (const char *)num);
  scheme_end_atomic_no_swap();
  if (!b->val)
    b->val = scheme_true;

  return(Scheme_Object *)HT_EXTRACT_WEAK(b->key);
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_env.inc"

static void register_traversers(void)
{

}

END_XFORM_SKIP;

#endif
