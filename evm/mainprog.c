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
    _do___U__main();
    return 0; 
}
