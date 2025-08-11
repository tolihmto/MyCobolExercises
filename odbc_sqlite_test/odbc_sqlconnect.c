#include <sql.h>
#include <sqlext.h>
#include <stdio.h>

__attribute__((visibility("default")))
#include <string.h>
void cob_sqlconnect(void *hdbc, char *dsn, int *dsnlen, char *user, int *userlen, char *pass, int *passlen, int *retcode) {
    char dsn_buf[256] = {0};
    char user_buf[256] = {0};
    char pass_buf[256] = {0};
    int dlen = *dsnlen < 255 ? *dsnlen : 255;
    int ulen = *userlen < 255 ? *userlen : 255;
    int plen = *passlen < 255 ? *passlen : 255;
    memcpy(dsn_buf, dsn, dlen); dsn_buf[dlen] = 0;
    memcpy(user_buf, user, ulen); user_buf[ulen] = 0;
    memcpy(pass_buf, pass, plen); pass_buf[plen] = 0;
    printf("DEBUG: cob_sqlconnect hdbc=%p dsn='%s' dsnlen=%d user='%s' userlen=%d pass='%s' passlen=%d\n",
        hdbc, dsn_buf, dlen, user_buf, ulen, pass_buf, plen);
    *retcode = SQLConnect(
        hdbc,
        (SQLCHAR*)dsn_buf, SQL_NTS,
        (SQLCHAR*)user_buf, SQL_NTS,
        (SQLCHAR*)pass_buf, SQL_NTS
    );
}


