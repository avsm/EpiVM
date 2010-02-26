# ifndef WIN32
#  include <pthread.h>
#  define GC_THREADS
# else
#  define GC_WIN32_THREADS
# endif

#include "closure.h"

void* _do__U_main();

void** _epic_top_of_stack;

int main(int argc, char* argv[]) {
    void* stacktop = NULL;
    _epic_top_of_stack = (void**)&stacktop;

    GC_init();
    init_evm();
    GC_use_entire_heap = 1;
    GC_free_space_divisor = 2;
//    GC_enable_incremental();
//    GC_time_limit = GC_TIME_UNLIMITED;

//    GC_full_freq=15;
//    fprintf(stderr, "Heap: %d\n", GC_get_heap_size());
    GC_expand_hp(1000000);
//    fprintf(stderr, "Heap: %d\n", GC_get_heap_size());

//    GC_disable();

    _do___U__main();

    GC_gcollect();
/*    fprintf(stderr, "%d\n", GC_gc_no);
    fprintf(stderr, "Heap: %d\n", GC_get_heap_size());
    fprintf(stderr, "Free: %d\n", GC_get_free_bytes());
    fprintf(stderr, "Total: %d\n", GC_get_total_bytes());*/
    return 0; 
}
