#include <sql.h>
#include <sqlext.h>
__attribute__((visibility("default")))
void cob_sqlsetodbc3(void *henv, int *retcode) {
    *retcode = SQLSetEnvAttr((SQLHENV)henv, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);
}
