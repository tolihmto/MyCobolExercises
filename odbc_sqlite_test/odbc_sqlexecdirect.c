#include <sql.h>
#include <sqlext.h>
#include <stdio.h>
#include <string.h>
__attribute__((visibility("default")))
void cob_sqlexecdirect(void *hstmt, char *sql, int *sql_len, int *retcode) {
    char sql_buf[512] = {0};
    int len = (*sql_len < 511) ? *sql_len : 511;
    memcpy(sql_buf, sql, len); sql_buf[len] = 0;
    printf("DEBUG: cob_sqlexecdirect hstmt=%p sql='%s' len=%d\n", hstmt, sql_buf, len);
    *retcode = SQLExecDirect(hstmt, (SQLCHAR*)sql_buf, SQL_NTS);
}
