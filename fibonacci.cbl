       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "numbers.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD               PIC X(10).

       WORKING-STORAGE SECTION.
       77  WS-N                     PIC 999 COMP.
       77  WS-INDEX                 PIC 999 COMP.
       77  WS-PREV                  PIC 9(38) VALUE 0.
       77  WS-CURR                  PIC 9(38) VALUE 1.
       77  WS-TEMP                  PIC 9(38) VALUE 0.
       77  WS-EOF                   PIC X     VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE
           STOP RUN.

       PROCESS-RECORD.
           MOVE FUNCTION NUMVAL(INPUT-RECORD) TO WS-N
           DISPLAY "Fibonacci sequence for n = " WS-N ":"
           IF WS-N = 1
               DISPLAY "0"
           ELSE
               MOVE 0 TO WS-PREV
               MOVE 1 TO WS-CURR
               DISPLAY WS-PREV
               DISPLAY WS-CURR
               PERFORM VARYING WS-INDEX FROM 3 BY 1
               UNTIL WS-INDEX > WS-N
                   COMPUTE WS-TEMP = WS-PREV + WS-CURR
                   DISPLAY WS-TEMP
                   MOVE WS-CURR TO WS-PREV
                   MOVE WS-TEMP TO WS-CURR
               END-PERFORM
           END-IF
           DISPLAY "------------------------------".
