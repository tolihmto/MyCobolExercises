#include <sql.h>
#include <sqlext.h>
#include <stdio.h>
__attribute__((visibility("default")))
void cob_sqlgetdiagrec(void *handle, short *handleType, int *recNumber, char *sqlState, int *nativeError, char *msg, short *msgLen, int *retcode) {
    *retcode = SQLGetDiagRec(
        *handleType, handle, *recNumber,
        (SQLCHAR*)sqlState, (SQLINTEGER*)nativeError,
        (SQLCHAR*)msg, 256, (SQLSMALLINT*)msgLen
    );
    msg[255] = '\0';
    printf("DEBUG: SQLGetDiagRec ret=%d, sqlState=%s, nativeError=%d, msg=%s\n", *retcode, sqlState, *nativeError, msg);
}
