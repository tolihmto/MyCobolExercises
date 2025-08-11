#include <stdio.h>
#include <sql.h>
#include <sqlext.h>

__attribute__((visibility("default")))
void cob_sqlallocenv_and_setodbc3(void **henv, int *retcode) {
    SQLRETURN r;
    r = SQLAllocHandle(SQL_HANDLE_ENV, NULL, henv);
    printf("DEBUG: SQLAllocHandle ENV ret=%d, henv=%p\n", r, *henv);
    *retcode = SQLSetEnvAttr(*henv, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);
    printf("DEBUG: SQLSetEnvAttr ret=%d\n", *retcode);
}
