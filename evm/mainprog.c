# ifndef WIN32
#  include <pthread.h>
#  define GC_THREADS
# else
#  define GC_WIN32_THREADS
# endif

#include "closure.h"

void* _do__U_main();

int main(int argc, char* argv[]) {
    GC_init();
    init_evm();
    GC_use_entire_heap = 1;
//    GC_free_space_divisor = 1;
    GC_expand_hp(10000000);

//    GC_disable();

    _do___U__main();
    fprintf(stderr, "%d\n", GC_gc_no);
    fprintf(stderr, "Heap: %d\n", GC_get_heap_size());
    fprintf(stderr, "Total: %d\n", GC_get_total_bytes());
    return 0; 
}
