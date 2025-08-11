#include <stdio.h>
#include <sql.h>
#include <sqlext.h>

__attribute__((visibility("default")))
void cob_sqlallochandle(int *handleType, void **inputHandle, void **outputHandle, int *retcode) {
    printf("DEBUG: handleType=%d, inputHandle=%p (-> %p), outputHandle=%p\n", *handleType, inputHandle, *inputHandle, outputHandle);
    void *inHandle = NULL;
    if (*handleType == 1) {
        inHandle = NULL;
    } else {
        inHandle = *inputHandle;
    }
    *retcode = SQLAllocHandle(*handleType, inHandle, outputHandle);
    printf("DEBUG: SQLAllocHandle returned %d, outputHandle now %p\n", *retcode, *outputHandle);
}
