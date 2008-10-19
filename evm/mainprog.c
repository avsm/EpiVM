# ifndef WIN32
#  include <pthread.h>
#  define GC_THREADS
# else
#  define GC_WIN32_THREADS
# endif

#include "closure.h"

void* _do__U_main();

int main() {
    GC_init();
    init_evm();
    EVAL(_do__U_main()); 
    return 0; 
}
