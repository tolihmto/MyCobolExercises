# 📌 Configuration ODBC avec SQLite sous Linux

Ce guide explique comment installer, configurer et utiliser **ODBC** avec **SQLite** sur Linux (via `unixODBC`) afin de se connecter à une base de données `.db` depuis COBOL ou un autre langage compatible ODBC.

---

## 1️⃣ Installer les paquets nécessaires

Sous Debian / Ubuntu :

```bash
sudo apt update
sudo apt install unixodbc unixodbc-dev odbcinst sqlite3 libsqlite3-odbc
```

- unixodbc → Gestionnaire ODBC pour Linux
- unixodbc-dev → Fichiers de développement ODBC
- libsqlite3-odbc → Pilote ODBC pour SQLite
- sqlite3 → Client SQLite (optionnel mais pratique pour tester)

## 2️⃣ Vérifier l’installation du pilote SQLite ODBC

Liste des pilotes installés :

```bash
odbcinst -q -d
```

Exemple de sortie :

```
[SQLite3]
```

Afficher le chemin du pilote SQLite :

```bash
odbcinst -q -d -n "SQLite3"
```

## 3️⃣ Configurer le fichier odbc.ini

### 📍 Fichier de configuration

Global : /etc/odbc.ini

Utilisateur : ~/.odbc.ini

### 📄 Exemple /etc/odbc.ini

```ini
[sqlite-test]
Description = SQLite test database
Driver = /usr/lib/x86_64-linux-gnu/odbc/libsqlite3odbc.so
Database = /mnt/d/!0! Documents/COBOL_PROGRAM/test.db
Timeout = 2000
```

Explications :

- [sqlite-test] → Nom DSN (Data Source Name), utilisé dans le code COBOL
- Driver → Chemin vers la bibliothèque du pilote ODBC SQLite
- Database → Chemin absolu du fichier .db
- Timeout → (Optionnel) Délai d’attente pour les requêtes

## 4️⃣ Configurer odbcinst.ini (si nécessaire)

Si le pilote SQLite n’est pas déjà enregistré :

```bash
sudo nano /etc/odbcinst.ini
```

Ajouter :

```ini
[SQLite3]
Description = ODBC for SQLite
Driver      = /usr/lib/x86_64-linux-gnu/odbc/libsqlite3odbc.so
Setup       = /usr/lib/x86_64-linux-gnu/odbc/libsqlite3odbc.so
Threading   = 2
```

## 5️⃣ Tester la connexion ODBC

```bash
isql sqlite-test
```

Si la connexion fonctionne, vous aurez :

```
Connected!
SQL>
```

## 6️⃣ Utiliser l'exemple COBOL qui utilise les wrappers C ODBC

```bash
make clean
make
./odbc_sqlite_demo
```

## 7️⃣ Utiliser ODBC avec COBOL et les wrappers C ODBC

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODBC-SQLITE-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  HENV-RAW    USAGE POINTER.
       77  HENV       REDEFINES HENV-RAW USAGE POINTER.
       77  HDBC        USAGE POINTER.
       77  HSTMT       USAGE POINTER.
       77  RETCODE     PIC S9(9) COMP-5.

       *> Nom DSN défini dans odbc.ini
       77  DSN         PIC X(64) VALUE "sqlite-test".

       77  USER        PIC X(16) VALUE SPACES.
       77  PASS        PIC X(16) VALUE SPACES.
       77  OUTSTR      PIC X(256).
       77  NAME-OUT    PIC X(20).
       77  NAME-LEN    PIC S9(9) USAGE COMP-5.
       77  SQL-LEN     PIC S9(9) COMP-5 VALUE 0.
       77  DSN-LEN     PIC S9(9) COMP-5 VALUE 0.
       77  USER-LEN    PIC S9(9) COMP-5 VALUE 0.
       77  PASS-LEN    PIC S9(9) COMP-5 VALUE 0.
       77  SQLSTATE   PIC X(6).
       77  NATIVE-ERR PIC S9(9) COMP-5.
       77  DIAG-MSG   PIC X(256).
       77  DIAG-LEN   PIC S9(4) COMP-5.
       77  REC-NUM    PIC S9(4) COMP-5 VALUE 1.

       *> Chemin vers la bibliothèque ODBC
       77  LIBNAME     PIC X(64) VALUE "/usr/lib/x86_64-linux-gnu/libodbc.so".

       77  DL_MODE     USAGE BINARY-LONG VALUE 1.
       77  NULL-HANDLE USAGE POINTER.
       77  HANDLE-TYPE-ENV  PIC S9(9) COMP-5 VALUE 1.
       77  HANDLE-TYPE-DBC  PIC S9(9) COMP-5 VALUE 2.

       PROCEDURE DIVISION.
           SET NULL-HANDLE TO NULL
           CALL "cob_sqlallocenv_and_setodbc3" USING BY REFERENCE HENV BY REFERENCE RETCODE
           DISPLAY "RETCODE alloc+setenv : " RETCODE
           DISPLAY "HENV COBOL : " HENV
           DISPLAY "HENV utilisé pour alloc DBC : " HENV
           CALL "cob_sqlallochandle" USING BY REFERENCE HANDLE-TYPE-DBC BY REFERENCE HENV BY REFERENCE HDBC BY REFERENCE RETCODE
           DISPLAY "RETCODE HDBC : " RETCODE
           MOVE ZERO TO DSN-LEN
           INSPECT DSN TALLYING DSN-LEN FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE ZERO TO USER-LEN
           INSPECT USER TALLYING USER-LEN FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE ZERO TO PASS-LEN
           INSPECT PASS TALLYING PASS-LEN FOR CHARACTERS BEFORE INITIAL SPACE
           DISPLAY "HDBC utilisé pour SQLConnect : " HDBC
           CALL "cob_sqlconnect" USING BY VALUE HDBC
                                 BY REFERENCE DSN BY REFERENCE DSN-LEN
                                 BY REFERENCE USER BY REFERENCE USER-LEN
                                 BY REFERENCE PASS BY REFERENCE PASS-LEN
                                 BY REFERENCE RETCODE
           DISPLAY "RETCODE SQLConnect : " RETCODE
           IF RETCODE < 0
               DISPLAY "Erreur connexion ODBC"
               CALL "cob_sqlgetdiagrec" USING BY REFERENCE HDBC
                                            BY REFERENCE HANDLE-TYPE-DBC
                                            BY REFERENCE REC-NUM
                                            BY REFERENCE SQLSTATE
                                            BY REFERENCE NATIVE-ERR
                                            BY REFERENCE DIAG-MSG
                                            BY REFERENCE DIAG-LEN
                                            BY REFERENCE RETCODE
               DISPLAY "ODBC ERROR SQLSTATE: " SQLSTATE
               DISPLAY "ODBC ERROR NATIVE: " NATIVE-ERR
               DISPLAY "ODBC ERROR MSG: " DIAG-MSG
               STOP RUN
           END-IF

           MOVE "CREATE TABLE IF NOT EXISTS demo (id INTEGER, name TEXT)" TO OUTSTR
           COMPUTE SQL-LEN = FUNCTION LENGTH(OUTSTR)
           DISPLAY "HDBC pour SQLAllocHandle : " HDBC
           CALL "SQLAllocHandle" USING BY VALUE 3 BY VALUE HDBC BY REFERENCE HSTMT BY REFERENCE RETURN-CODE
           DISPLAY "RETCODE SQLAllocHandle (STMT): " RETURN-CODE
           DISPLAY "HSTMT après alloc : " HSTMT
           CALL "cob_sqlexecdirect" USING BY VALUE HSTMT OUTSTR BY REFERENCE SQL-LEN BY REFERENCE RETURN-CODE
           DISPLAY "RETCODE SQLExecDirect (CREATE): " RETURN-CODE
           DISPLAY "Table créée (si besoin)"

           MOVE "INSERT INTO demo (id, name) VALUES (1, 'Alice')" TO OUTSTR
           COMPUTE SQL-LEN = FUNCTION LENGTH(OUTSTR)
           CALL "cob_sqlexecdirect" USING BY VALUE HSTMT OUTSTR BY REFERENCE SQL-LEN BY REFERENCE RETURN-CODE
           DISPLAY "RETCODE SQLExecDirect (INSERT): " RETURN-CODE

           MOVE "SELECT name FROM demo WHERE id=1" TO OUTSTR
           COMPUTE SQL-LEN = FUNCTION LENGTH(OUTSTR)
           CALL "cob_sqlexecdirect" USING BY VALUE HSTMT OUTSTR BY REFERENCE SQL-LEN BY REFERENCE RETURN-CODE
           DISPLAY "RETCODE SQLExecDirect (SELECT): " RETURN-CODE
           DISPLAY "Avant FETCH, RETURN-CODE : " RETURN-CODE

           PERFORM UNTIL RETURN-CODE <> 0
               CALL "SQLFetch" USING BY VALUE HSTMT RETURN-CODE
               DISPLAY "SQLFetch RETURN-CODE : " RETURN-CODE
               IF RETURN-CODE = 0
                   CALL "SQLGetData" USING BY VALUE HSTMT BY VALUE 1 BY VALUE 1 BY REFERENCE NAME-OUT BY VALUE 20 BY REFERENCE NAME-LEN
                   DISPLAY "Nom trouvé (ODBC) : " NAME-OUT
               END-IF
           END-PERFORM

           CALL "SQLDisconnect" USING HDBC
           CALL "SQLFreeHandle" USING BY VALUE 2 BY REFERENCE HDBC
           CALL "SQLFreeHandle" USING BY VALUE 1 BY REFERENCE HENV

           STOP RUN.

```

## 8️⃣ Schéma visuel en ASCII

```
                          (optionnel pour test)
      +----------------+     isql / unixODBC CLI
      |     isql      |------------------------------+
      +----------------+                              |
                                                     \|/
+----------------+     appels COBOL (CALL ...)   +--------------------+
|    COBOL      |-------------------------------->| Wrappers C ODBC   |
| (cobc -x ...) |   (cob_sqlconnect, execdirect)  |  (fonctions C)    |
+----------------+                                 +--------------------+
                                                        |
                                                        |  API ODBC (SQLConnect,
                                                        |  SQLExecDirect, SQLFetch...)
                                                        v
                                              +-------------------------+
                                              |  ODBC Driver Manager    |
                                              |     (unixODBC)          |
                                              +-------------------------+
                                               /|\                 /|\
                                                |                   |
                        Lit le DSN et le pilote |                   | Charge le pilote
                                                |                   |
                             +------------------+----+        +-----+----------------+
                             |  /etc/odbc.ini        |        |  /etc/odbcinst.ini  |
                             |  ~/.odbc.ini          |        |  (registre pilotes) |
                             +-----------------------+        +---------------------+
                             DSN -> Driver + Database              "SQLite3" -> .so

                                                        |
                                                        v
                                            +---------------------------+
                                            | SQLite ODBC Driver (.so) |
                                            +---------------------------+
                                                        |
                                                        |  accès fichier (POSIX)
                                                        v
                                            +---------------------------+
                                            |  Fichier SQLite .db       |
                                            |  (ex: /path/to/test.db)   |
                                            +---------------------------+
```
