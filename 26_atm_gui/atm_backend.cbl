       >>SOURCE FORMAT FREE
      *>
      *> ATM Backend (GnuCOBOL) - NDJSON over stdin/stdout
      *> Ops: login/balance/deposit/withdraw/exit
      *> NOTE: naive JSON parsing; expects simple shapes shown above.
      *>
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-BACKEND.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERS-FILE ASSIGN TO "users.json"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  USERS-FILE.
       01  USERS-REC                 PIC X(512).
       WORKING-STORAGE SECTION.
      *> I/O buffers
       01  IN-LINE                  PIC X(512).
       01  OUT-LINE                 PIC X(512).
      *> Parsed fields
       01  OP                       PIC X(16).
       01  PIN                      PIC X(8).
       01  HASH-CARTE               PIC X(64).
       01  AMOUNT-STR               PIC X(32).
       01  AMOUNT-NUM               PIC S9(13)V99 VALUE 0.
      *> State
       01  AUTHENTICATED            PIC 9 VALUE 0.
       01  BALANCE                  PIC S9(13)V99 VALUE 0.
       01  CUR-USER-IX              PIC 9(4) COMP VALUE 0.
      *> Users DB (loaded from users.json)
       01  USER-COUNT               PIC 9(4) COMP VALUE 0.
       01  USER-TABLE.
           05  USER-ENTRY OCCURS 100 TIMES.
               10 U-HASH            PIC X(64).
               10 U-PIN             PIC X(8).
               10 U-BAL             PIC S9(13)V99.
      *> Display helper for BALANCE
       01  BALANCE-STR              PIC -9(11).99.
       01  OUT-AMOUNT-STR           PIC -9(13).99.
      *> Temps de travail
       01  TMP                      PIC X(512).
       01  POS1                     PIC 9(4) COMP.
       01  POS2                     PIC 9(4) COMP.
       01  LEN                      PIC 9(4) COMP.
       01  USERS-BUF                PIC X(16384).
       01  USERS-LEN                PIC 9(5) COMP VALUE 0.
       01  I                        PIC 9(4) COMP VALUE 0.
       01  J                        PIC 9(4) COMP VALUE 0.
       01  P                        PIC 9(4) COMP VALUE 0.
       01  T-HASH                   PIC X(64).
       01  T-PIN                    PIC X(8).
       01  T-SOLDE                  PIC S9(13)V99 VALUE 0.
       01  T-HAVE-HASH              PIC 9 VALUE 0.
       01  T-HAVE-PIN               PIC 9 VALUE 0.
       01  T-HAVE-SOLDE             PIC 9 VALUE 0.
       01  T-PARSED-SOLDE           PIC 9 VALUE 0.
      *> Debug
       01  DEBUG-FLAG               PIC 9 VALUE 0.
       01  DBG-MSG                  PIC X(256).

       PROCEDURE DIVISION.
       MAIN-LOOP.
           IF DEBUG-FLAG = 1
              DISPLAY "[BACKEND] startup v2" UPON STDERR
           END-IF
           PERFORM LOAD-USERS
           PERFORM UNTIL 1 = 0
              ACCEPT IN-LINE
                 ON EXCEPTION
                    *> EOF or input error: exit quietly (avoid writing
                    *> after GUI closes pipe)
                    STOP RUN
              END-ACCEPT
              IF IN-LINE = SPACES
                 CONTINUE
              ELSE
                 PERFORM TRIM-SPACES
                 IF DEBUG-FLAG = 1
                    DISPLAY "[BACKEND] request=" UPON STDERR
                    DISPLAY FUNCTION TRIM(TMP) UPON STDERR
                 END-IF
                 MOVE SPACES TO OP PIN HASH-CARTE AMOUNT-STR
                 PERFORM PARSE-OP
                 EVALUATE OP
                   WHEN "login"
                      PERFORM PARSE-HASH
                      PERFORM PARSE-PIN
                      PERFORM DBG-LOGIN-START
                      PERFORM FIND-USER-BY-HASH
                      PERFORM DBG-LOGIN-AFTER-FIND
                      IF CUR-USER-IX > 0
                         IF PIN = U-PIN(CUR-USER-IX)
                            MOVE 1 TO AUTHENTICATED
                            MOVE U-BAL(CUR-USER-IX) TO BALANCE
                            MOVE "login_ok" TO TMP
                            PERFORM RESP-OK-MSG
                         ELSE
                            MOVE 0 TO AUTHENTICATED
                            MOVE "bad_pin" TO TMP
                            PERFORM RESP-ERROR
                         END-IF
                      ELSE
                         MOVE 0 TO AUTHENTICATED
                         MOVE "bad_pin" TO TMP
                         PERFORM RESP-ERROR
                      END-IF
                   WHEN "balance"
                      PERFORM CHECK-AUTH
                      IF AUTHENTICATED = 1
                         MOVE "balance_ok" TO TMP
                         PERFORM RESP-OK-BAL
                      END-IF
                   WHEN "deposit"
                      PERFORM CHECK-AUTH
                      IF AUTHENTICATED = 1
                         PERFORM PARSE-AMOUNT
                         IF AMOUNT-NUM < 0
                            MOVE "amount_negative" TO TMP
                            PERFORM RESP-ERROR
                         ELSE
                            ADD AMOUNT-NUM TO BALANCE
                            IF CUR-USER-IX > 0
                               ADD AMOUNT-NUM TO U-BAL(CUR-USER-IX)
                            END-IF
                            PERFORM SAVE-USERS
                            MOVE "deposit_ok" TO TMP
                            PERFORM RESP-OK-BAL
                         END-IF
                      END-IF
                   WHEN "withdraw"
                      PERFORM CHECK-AUTH
                      IF AUTHENTICATED = 1
                         PERFORM PARSE-AMOUNT
                         IF AMOUNT-NUM <= 0
                            MOVE "amount_invalid" TO TMP
                            PERFORM RESP-ERROR
                         ELSE
                            IF AMOUNT-NUM > BALANCE
                               MOVE "insufficient_funds" TO TMP
                               PERFORM RESP-ERROR
                             ELSE
                                SUBTRACT AMOUNT-NUM FROM BALANCE
                                IF CUR-USER-IX > 0
                                   SUBTRACT AMOUNT-NUM
                                   FROM U-BAL(CUR-USER-IX)
                                END-IF
                                PERFORM SAVE-USERS
                                MOVE "withdraw_ok" TO TMP
                                PERFORM RESP-OK-BAL
                             END-IF
                          END-IF
                       END-IF
                    WHEN "exit"
                      MOVE "bye" TO TMP
                      PERFORM RESP-OK-MSG
                       STOP RUN
                    WHEN OTHER
                      MOVE "unknown_op" TO TMP
                      PERFORM RESP-ERROR
                  END-EVALUATE
               END-IF
            END-PERFORM.
            STOP RUN.
       *> ---------- helpers ----------
       LOAD-USERS.
           MOVE 0 TO USER-COUNT
           MOVE SPACES TO T-HASH T-PIN
           MOVE 0 TO T-SOLDE T-HAVE-HASH T-HAVE-PIN T-HAVE-SOLDE
           OPEN INPUT USERS-FILE
           IF DEBUG-FLAG = 1
              DISPLAY "[BACKEND] opened users.json" UPON STDERR
           END-IF
           PERFORM UNTIL 1 = 0
              READ USERS-FILE INTO USERS-REC
                 AT END EXIT PERFORM
              END-READ
              MOVE USERS-REC TO TMP
              PERFORM TRIM-TMP
              IF DEBUG-FLAG = 1
                 DISPLAY "[BACKEND] line=" UPON STDERR
                 DISPLAY FUNCTION TRIM(TMP) UPON STDERR
              END-IF
              *> try parse hash
              PERFORM PARSE-HASH
              IF HASH-CARTE NOT = SPACES
                 MOVE HASH-CARTE TO T-HASH
                 MOVE 1 TO T-HAVE-HASH
                 IF DEBUG-FLAG = 1
                    DISPLAY "  found hash=" UPON STDERR
                    DISPLAY FUNCTION TRIM(T-HASH) UPON STDERR
                 END-IF
              END-IF
              *> try parse pin
              PERFORM PARSE-PIN
              IF PIN NOT = SPACES
                 MOVE PIN TO T-PIN
                 MOVE 1 TO T-HAVE-PIN
                 IF DEBUG-FLAG = 1
                    DISPLAY "  found pin=" UPON STDERR
                    DISPLAY FUNCTION TRIM(T-PIN) UPON STDERR
                 END-IF
              END-IF
              *> try parse solde
              PERFORM PARSE-SOLDE
              IF T-PARSED-SOLDE = 1
                 MOVE AMOUNT-NUM TO T-SOLDE
                 MOVE 1 TO T-HAVE-SOLDE
                 IF DEBUG-FLAG = 1
                    DISPLAY "  found solde=" UPON STDERR
                    MOVE T-SOLDE TO BALANCE-STR
                    DISPLAY FUNCTION TRIM(BALANCE-STR) UPON STDERR
                 END-IF
              END-IF
              *> end of object (accept '}' or '},')
              MOVE FUNCTION TRIM(TMP) TO OUT-LINE
              MOVE 0 TO I
              IF OUT-LINE = "}"
                 MOVE 1 TO I
              ELSE
                 IF OUT-LINE = "},"
                    MOVE 1 TO I
                 END-IF
              END-IF
              IF I = 1
                 IF DEBUG-FLAG = 1
                    DISPLAY "[BACKEND] end object flags H/P/S=" UPON STDERR
                    DISPLAY T-HAVE-HASH UPON STDERR
                    DISPLAY T-HAVE-PIN UPON STDERR
                    DISPLAY T-HAVE-SOLDE UPON STDERR
                 END-IF
                 IF T-HAVE-HASH = 1 AND T-HAVE-PIN = 1 AND T-HAVE-SOLDE = 1
                    ADD 1 TO USER-COUNT
                    IF USER-COUNT <= 100
                       MOVE T-HASH TO U-HASH(USER-COUNT)
                       MOVE T-PIN TO U-PIN(USER-COUNT)
                       MOVE T-SOLDE TO U-BAL(USER-COUNT)
                       IF DEBUG-FLAG = 1
                          DISPLAY "[BACKEND] add user #" UPON STDERR
                          DISPLAY USER-COUNT UPON STDERR
                          DISPLAY " hash=" UPON STDERR
                          DISPLAY FUNCTION TRIM(T-HASH) UPON STDERR
                          DISPLAY " pin=" UPON STDERR
                          DISPLAY FUNCTION TRIM(T-PIN) UPON STDERR
                       END-IF
                    END-IF
                    MOVE SPACES TO T-HASH T-PIN
                    MOVE 0 TO T-SOLDE T-HAVE-HASH T-HAVE-PIN
                    T-HAVE-SOLDE
                 END-IF
              END-IF
           END-PERFORM
           CLOSE USERS-FILE
           IF DEBUG-FLAG = 1
              DISPLAY "[BACKEND] users loaded=" UPON STDERR
              DISPLAY USER-COUNT UPON STDERR
           END-IF
           .
      *> Rewrite users.json from in-memory USER-TABLE
       SAVE-USERS.
           OPEN OUTPUT USERS-FILE
           MOVE "[" TO USERS-REC
           WRITE USERS-REC
           MOVE 1 TO J
           PERFORM UNTIL J > USER-COUNT
              MOVE SPACES TO USERS-REC
              MOVE 1 TO P
              STRING
                 "  {" DELIMITED BY SIZE
              INTO USERS-REC WITH POINTER P
              END-STRING
              WRITE USERS-REC

              *>   "hash-carte": "<hash>",
              MOVE SPACES TO USERS-REC
              MOVE 1 TO P
              STRING
                 '    "hash-carte": "' DELIMITED BY SIZE
                 FUNCTION TRIM(U-HASH(J)) DELIMITED BY SIZE
                 '",' DELIMITED BY SIZE
              INTO USERS-REC WITH POINTER P
              END-STRING
              WRITE USERS-REC

              *>   "solde": <amount>,
              MOVE U-BAL(J) TO OUT-AMOUNT-STR
              MOVE SPACES TO USERS-REC
              MOVE 1 TO P
              STRING
                 '    "solde": ' DELIMITED BY SIZE
                 FUNCTION TRIM(OUT-AMOUNT-STR) DELIMITED BY SIZE
                 ',' DELIMITED BY SIZE
              INTO USERS-REC WITH POINTER P
              END-STRING
              WRITE USERS-REC

              *>   "pin": <pin>
              MOVE SPACES TO USERS-REC
              MOVE 1 TO P
              STRING
                 '    "pin": ' DELIMITED BY SIZE
                 FUNCTION TRIM(U-PIN(J)) DELIMITED BY SIZE
              INTO USERS-REC WITH POINTER P
              END-STRING
              WRITE USERS-REC

              *>   }, or   }
              MOVE SPACES TO USERS-REC
              MOVE 1 TO P
              IF J < USER-COUNT
                 STRING "  }," DELIMITED BY SIZE INTO USERS-REC
                 WITH POINTER P
                 END-STRING
              ELSE
                 STRING "  }" DELIMITED BY SIZE INTO USERS-REC
                 WITH POINTER P
                 END-STRING
              END-IF
              WRITE USERS-REC

              ADD 1 TO J
           END-PERFORM
           MOVE "]" TO USERS-REC
           WRITE USERS-REC
           CLOSE USERS-FILE
           .
       CHECK-AUTH.
           IF AUTHENTICATED NOT = 1
              MOVE "not_authenticated" TO TMP
              PERFORM RESP-ERROR
           END-IF
           .
       TRIM-SPACES.
           MOVE IN-LINE TO TMP
           INSPECT TMP REPLACING ALL X"0D" BY ' '
           INSPECT TMP REPLACING ALL X"0A" BY ' '
           MOVE FUNCTION TRIM(TMP) TO TMP
           .
       TRIM-TMP.
           INSPECT TMP REPLACING ALL X"0D" BY ' '
           INSPECT TMP REPLACING ALL X"0A" BY ' '
           MOVE FUNCTION TRIM(TMP) TO TMP
           .
      *> Extract value of "op":"...".
       PARSE-OP.
           MOVE SPACES TO OP
           MOVE TMP TO IN-LINE
           MOVE FUNCTION LENGTH(IN-LINE) TO LEN
           MOVE 1 TO POS1
           PERFORM UNTIL POS1 > LEN - 3
              IF IN-LINE(POS1:4) = '"op"'
                 ADD 4 TO POS1
                 *> skip spaces
                 PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) NOT = ' '
                    ADD 1 TO POS1
                 END-PERFORM
                 *> expect ':' then optional spaces
                 IF POS1 <= LEN AND IN-LINE(POS1:1) = ':'
                    ADD 1 TO POS1
                    PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1)
                    NOT = ' '
                       ADD 1 TO POS1
                    END-PERFORM
                    *> value should start with '"', then read until next '"'
                    IF POS1 <= LEN AND IN-LINE(POS1:1) = '"'
                       ADD 1 TO POS1
                       MOVE 0 TO POS2
                       PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = '"'
                          ADD 1 TO POS2
                          IF POS2 <= LENGTH OF OP
                             MOVE IN-LINE(POS1:1) TO OP(POS2:1)
                          END-IF
                          ADD 1 TO POS1
                       END-PERFORM
                    END-IF
                 END-IF
                 EXIT PERFORM
              ELSE
                 ADD 1 TO POS1
              END-IF
           END-PERFORM
           *> Fallback: if OP not found, search bare 'op' without quotes
           IF OP = SPACES
              MOVE 1 TO POS1
              PERFORM UNTIL POS1 > LEN - 1
                 IF IN-LINE(POS1:2) = 'op'
                    ADD 2 TO POS1
                    PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = ':'
                       ADD 1 TO POS1
                    END-PERFORM
                    IF POS1 <= LEN AND IN-LINE(POS1:1) = ':'
                       ADD 1 TO POS1
                       PERFORM UNTIL POS1 > LEN OR
                       IN-LINE(POS1:1) NOT = ' '
                          ADD 1 TO POS1
                       END-PERFORM
                       IF POS1 <= LEN AND IN-LINE(POS1:1) = '"'
                          ADD 1 TO POS1
                          MOVE 0 TO POS2
                          PERFORM UNTIL POS1 > LEN OR
                          IN-LINE(POS1:1) = '"'
                             ADD 1 TO POS2
                             IF POS2 <= LENGTH OF OP
                                MOVE IN-LINE(POS1:1) TO OP(POS2:1)
                             END-IF
                             ADD 1 TO POS1
                          END-PERFORM
                       END-IF
                    END-IF
                    EXIT PERFORM
                 ELSE
                    ADD 1 TO POS1
                 END-IF
              END-PERFORM
           END-IF
           *> Final fallback removed (FUNCTION INDEX not available in
           *> this dialect)
           .
       PARSE-PIN.
            MOVE SPACES TO PIN
            MOVE TMP TO IN-LINE
            MOVE FUNCTION LENGTH(IN-LINE) TO LEN
            MOVE 1 TO POS1
            PERFORM UNTIL POS1 > LEN - 6
               IF IN-LINE(POS1:6) = '"pin":'
                  ADD 6 TO POS1
                  *> skip spaces
                  PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) NOT = ' '
                     ADD 1 TO POS1
                  END-PERFORM
                  MOVE 0 TO POS2
                  IF POS1 <= LEN AND IN-LINE(POS1:1) = '"'
                     *> quoted pin
                     ADD 1 TO POS1
                     PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = '"'
                        ADD 1 TO POS2
                        IF POS2 <= LENGTH OF PIN
                           MOVE IN-LINE(POS1:1) TO PIN(POS2:1)
                        END-IF
                        ADD 1 TO POS1
                     END-PERFORM
                  ELSE
                     *> numeric pin (unquoted)
                     PERFORM UNTIL POS1 > LEN
                        EVALUATE IN-LINE(POS1:1)
                           WHEN '0' THRU '9'
                              ADD 1 TO POS2
                              IF POS2 <= LENGTH OF PIN
                                 MOVE IN-LINE(POS1:1) TO PIN(POS2:1)
                              END-IF
                              ADD 1 TO POS1
                           WHEN OTHER
                              EXIT PERFORM
                        END-EVALUATE
                     END-PERFORM
                  END-IF
                  EXIT PERFORM
               ELSE
                  ADD 1 TO POS1
               END-IF
            END-PERFORM
            .
       PARSE-HASH.
           MOVE SPACES TO HASH-CARTE
           MOVE TMP TO IN-LINE
           MOVE FUNCTION LENGTH(IN-LINE) TO LEN
           MOVE 1 TO POS1
           PERFORM UNTIL POS1 > LEN - 12
              IF IN-LINE(POS1:12) = '"hash-carte"'
                 ADD 12 TO POS1
                 PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = ':'
                    ADD 1 TO POS1
                 END-PERFORM
                 IF POS1 <= LEN AND IN-LINE(POS1:1) = ':'
                    ADD 1 TO POS1
                    PERFORM UNTIL POS1 > LEN OR
                    IN-LINE(POS1:1) NOT = ' '
                       ADD 1 TO POS1
                    END-PERFORM
                    IF POS1 <= LEN AND IN-LINE(POS1:1) = '"'
                       ADD 1 TO POS1
                       MOVE 0 TO POS2
                       PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = '"'
                          ADD 1 TO POS2
                          IF POS2 <= LENGTH OF HASH-CARTE
                             MOVE IN-LINE(POS1:1) TO HASH-CARTE(POS2:1)
                          END-IF
                          ADD 1 TO POS1
                       END-PERFORM
                    ELSE
                       *> unquoted numeric
                       MOVE 0 TO POS2
                       PERFORM UNTIL POS1 > LEN
                          EVALUATE IN-LINE(POS1:1)
                             WHEN '0' THRU '9'
                                ADD 1 TO POS2
                                IF POS2 <= LENGTH OF HASH-CARTE
                                   MOVE IN-LINE(POS1:1) TO
                                   HASH-CARTE(POS2:1)
                                END-IF
                                ADD 1 TO POS1
                             WHEN OTHER
                                EXIT PERFORM
                          END-EVALUATE
                       END-PERFORM
                    END-IF
                 END-IF
                 EXIT PERFORM
              ELSE
                 ADD 1 TO POS1
              END-IF
           END-PERFORM
           *> Fallback: search bare key without quotes
           IF HASH-CARTE = SPACES
              MOVE 1 TO POS1
              PERFORM UNTIL POS1 > LEN - 10
                 IF IN-LINE(POS1:10) = 'hash-carte'
                    ADD 10 TO POS1
                    PERFORM UNTIL POS1 > LEN OR IN-LINE(POS1:1) = ':'
                       ADD 1 TO POS1
                    END-PERFORM
                    IF POS1 <= LEN AND IN-LINE(POS1:1) = ':'
                       ADD 1 TO POS1
                       PERFORM UNTIL POS1 > LEN OR
                       IN-LINE(POS1:1) NOT = ' '
                          ADD 1 TO POS1
                       END-PERFORM
                       IF POS1 <= LEN AND IN-LINE(POS1:1) = '"'
                          ADD 1 TO POS1
                          MOVE 0 TO POS2
                          PERFORM UNTIL POS1 > LEN OR
                          IN-LINE(POS1:1) = '"'
                             ADD 1 TO POS2
                             IF POS2 <= LENGTH OF HASH-CARTE
                                MOVE IN-LINE(POS1:1) TO
                                HASH-CARTE(POS2:1)
                             END-IF
                             ADD 1 TO POS1
                          END-PERFORM
                       ELSE
                          MOVE 0 TO POS2
                          PERFORM UNTIL POS1 > LEN
                             EVALUATE IN-LINE(POS1:1)
                                WHEN '0' THRU '9'
                                   ADD 1 TO POS2
                                   IF POS2 <= LENGTH OF HASH-CARTE
                                      MOVE IN-LINE(POS1:1) TO
                                      HASH-CARTE(POS2:1)
                                   END-IF
                                   ADD 1 TO POS1
                                WHEN OTHER
                                   EXIT PERFORM
                             END-EVALUATE
                          END-PERFORM
                       END-IF
                    END-IF
                    EXIT PERFORM
                 ELSE
                    ADD 1 TO POS1
                 END-IF
              END-PERFORM
           END-IF
           .
       PARSE-SOLDE.
           MOVE ZERO TO AMOUNT-NUM
           MOVE 0 TO T-PARSED-SOLDE
           MOVE TMP TO IN-LINE
           MOVE FUNCTION LENGTH(IN-LINE) TO LEN
           MOVE 1 TO POS1
           PERFORM UNTIL POS1 > LEN - 8
              IF IN-LINE(POS1:8) = '"solde":'
                 ADD 8 TO POS1
                 PERFORM UNTIL IN-LINE(POS1:1) NOT = ' ' AND POS1 <= LEN
                    ADD 1 TO POS1
                 END-PERFORM
                 MOVE 0 TO POS2
                 MOVE SPACES TO AMOUNT-STR
                 PERFORM UNTIL POS1 > LEN
                    EVALUATE IN-LINE(POS1:1)
                       WHEN '0' THRU '9' WHEN '.' WHEN '-'
                          ADD 1 TO POS2
                          MOVE IN-LINE(POS1:1) TO AMOUNT-STR(POS2:1)
                          ADD 1 TO POS1
                       WHEN OTHER
                          EXIT PERFORM
                    END-EVALUATE
                 END-PERFORM
                 MOVE FUNCTION NUMVAL(FUNCTION TRIM(AMOUNT-STR))
                 TO AMOUNT-NUM
                 MOVE 1 TO T-PARSED-SOLDE
                 EXIT PERFORM
              ELSE
                 ADD 1 TO POS1
              END-IF
           END-PERFORM
           .
       FIND-USER-BY-HASH.
           MOVE 0 TO CUR-USER-IX
           IF USER-COUNT = 0
              EXIT PARAGRAPH
           END-IF
           MOVE 1 TO I
           PERFORM UNTIL I > USER-COUNT
              IF FUNCTION TRIM(U-HASH(I)) = FUNCTION TRIM(HASH-CARTE)
                 MOVE I TO CUR-USER-IX
                 EXIT PERFORM
              END-IF
              ADD 1 TO I
           END-PERFORM
           .
      *> Extract "amount":123.45
       PARSE-AMOUNT.
           MOVE ZERO TO AMOUNT-NUM
           MOVE SPACES TO AMOUNT-STR
           MOVE TMP TO IN-LINE
           MOVE FUNCTION LENGTH(IN-LINE) TO LEN
           MOVE 1 TO POS1
           PERFORM UNTIL POS1 > LEN - 9
              IF IN-LINE(POS1:9) = '"amount":'
                 ADD 9 TO POS1
                 PERFORM UNTIL IN-LINE(POS1:1) NOT = ' ' AND POS1 <= LEN
                    ADD 1 TO POS1
                 END-PERFORM
                 MOVE 0 TO POS2
                 MOVE SPACES TO AMOUNT-STR
                 PERFORM UNTIL POS1 > LEN
                    EVALUATE IN-LINE(POS1:1)
                       WHEN '0' THRU '9' WHEN '.' WHEN '-'
                          ADD 1 TO POS2
                          MOVE IN-LINE(POS1:1) TO AMOUNT-STR(POS2:1)
                          ADD 1 TO POS1
                       WHEN OTHER
                          EXIT PERFORM
                    END-EVALUATE
                 END-PERFORM
                 MOVE FUNCTION NUMVAL(FUNCTION TRIM(AMOUNT-STR))
                 TO AMOUNT-NUM
                 EXIT PERFORM
              ELSE
                 ADD 1 TO POS1
              END-IF
           END-PERFORM
           .
      *> Old generic scanning helpers removed; direct parsing used
       *> ---------- debug ----------
        DBG-LOGIN-START.
            IF DEBUG-FLAG = 1
               DISPLAY "[BACKEND] login start" UPON STDERR
               DISPLAY " hash=" UPON STDERR
               DISPLAY FUNCTION TRIM(HASH-CARTE) UPON STDERR
               DISPLAY " pin=" UPON STDERR
               DISPLAY FUNCTION TRIM(PIN) UPON STDERR
            END-IF
            .
        DBG-LOGIN-AFTER-FIND.
            IF DEBUG-FLAG = 1
               DISPLAY "[BACKEND] after find cur_ix=" UPON STDERR
               DISPLAY CUR-USER-IX UPON STDERR
            END-IF
            .
       *> ---------- responses ----------
       RESP-OK-MSG.
           MOVE SPACES TO OUT-LINE
           STRING
             '{"ok":true,"msg":"'
             TMP DELIMITED BY SIZE
             '"}'
             DELIMITED BY SIZE
             INTO OUT-LINE
           END-STRING
           DISPLAY FUNCTION TRIM(OUT-LINE)
           .
       RESP-OK-BAL.
           MOVE SPACES TO OUT-LINE
           MOVE BALANCE TO BALANCE-STR
           STRING
             '{"ok":true,"balance":'
             FUNCTION TRIM(BALANCE-STR)
             ',"msg":"'
             TMP
             '"}'
             DELIMITED BY SIZE
             INTO OUT-LINE
           END-STRING
           DISPLAY FUNCTION TRIM(OUT-LINE)
           .
       RESP-ERROR.
           MOVE SPACES TO OUT-LINE
           STRING
             '{"ok":false,"error":"'
             TMP
             '"}'
             DELIMITED BY SIZE
             INTO OUT-LINE
           END-STRING
           DISPLAY FUNCTION TRIM(OUT-LINE)
           .
