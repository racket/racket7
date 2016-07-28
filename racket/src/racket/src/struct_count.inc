#if 0

/* Structure profiling --- prints allocated counts (not necessarily
   still live) after every `NUM_ALLOCS_BEFORE_REPORT` structure
   allocations. Adjust that constant to match a test program. Also,
   run with `racket -j` so that structure allocation is not inlined,
   and don't use places. */

#define NUM_ALLOCS_BEFORE_REPORT 100000

static Scheme_Hash_Table *allocs;
static int alloc_count;

#include "../gc2/my_qsort.c"
typedef struct alloc_count_result { int pos; int count; } alloc_count_result;

static int smaller_alloc_count(const void *a, const void *b) {
  return ((alloc_count_result*)a)->count - ((alloc_count_result*)b)->count;
}

static void count_alloc(Scheme_Struct_Type *stype)
{
  int i;
  if (!allocs) {
    REGISTER_SO(allocs);
    allocs = scheme_make_hash_table(SCHEME_hash_ptr);
  }
  {
    Scheme_Object *c;
    alloc_count_result *a;
    int count = allocs->count;
    int k = 0;
    c = scheme_hash_get(allocs, stype->name);
    if (!c) c = scheme_make_integer(0);
    a = MALLOC_N_ATOMIC(alloc_count_result, count);
    scheme_hash_set(allocs, stype->name, scheme_make_integer(SCHEME_INT_VAL(c)+1));
    alloc_count++;
    if (alloc_count == NUM_ALLOCS_BEFORE_REPORT) {
      printf("\n");
      for (i = allocs->size; i--; ) {
        if (allocs->vals[i]) {
          a[k].pos = i;
          a[k].count = SCHEME_INT_VAL(allocs->vals[i]);
          k++;
        }
      }
      my_qsort(a, allocs->count, sizeof(alloc_count_result), smaller_alloc_count);

      for (i = 0; i < count; i++) {
          printf("%d %s\n", a[i].count, SCHEME_SYM_VAL(allocs->keys[a[i].pos]));
      }

      alloc_count = 0;
    }
  }
}
#else
# define count_alloc(x) /* empty */
#endif
