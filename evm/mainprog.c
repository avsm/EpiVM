#include "closure.h"
#include <gc/gc.h>

void* _do__U_main();

int main() {
    GC_init();
    init_evm();
    _do__U_main(); 
    return 0; 
}
