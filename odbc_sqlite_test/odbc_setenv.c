#include <sql.h>
#include <sqlext.h>
#include <stdio.h>

__attribute__((visibility("default")))
void cob_sqlsetenvattr(void *henv, int *attr, void *value, int *valuelen, int *retcode) {
    printf("DEBUG: SQLSetEnvAttr henv=%p, attr=%d, value=%p, len=%d\n", henv, *attr, value, *valuelen);
    *retcode = SQLSetEnvAttr((SQLHENV)henv, *attr, value, *valuelen);
}

