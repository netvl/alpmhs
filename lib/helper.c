#include <bindings.cmacros.h>
#include <alpm.h>

void alpm_list_free_full(alpm_list_t *lst) {
    FREELIST(lst);
}
