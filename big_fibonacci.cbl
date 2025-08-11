       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIG-FIB-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "numbers.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD       PIC X(10).

       WORKING-STORAGE SECTION.
       77 MAX-DIGITS         PIC 9(4) VALUE 300.
       77 N                  PIC 9(4).
       77 I                  PIC 9(4).
       77 IDX                PIC 9(4).
       77 CARRY              PIC 9 VALUE 0.
       77 DIGIT1             PIC 9 VALUE 0.
       77 DIGIT2             PIC 9 VALUE 0.
       77 TEMP-SUM           PIC 99.
       77 WS-EOF             PIC X VALUE 'N'.
       77 FIRST-NONSPACE     PIC 9(4).
       77 DISP-IDX           PIC 9(4).
       77 CURRENT-DIGIT      PIC X.
       77 LINE-OUT           PIC X(300).
       77 PADDED-N           PIC 999.

       01 FIB1               PIC X(300) VALUE SPACES.
       01 FIB2               PIC X(300) VALUE SPACES.
       01 RESULT             PIC X(301) VALUE SPACES.

       01 DIGIT-CHAR-TABLE.
           05 D-CHAR-0 PIC X VALUE "0".
           05 D-CHAR-1 PIC X VALUE "1".
           05 D-CHAR-2 PIC X VALUE "2".
           05 D-CHAR-3 PIC X VALUE "3".
           05 D-CHAR-4 PIC X VALUE "4".
           05 D-CHAR-5 PIC X VALUE "5".
           05 D-CHAR-6 PIC X VALUE "6".
           05 D-CHAR-7 PIC X VALUE "7".
           05 D-CHAR-8 PIC X VALUE "8".
           05 D-CHAR-9 PIC X VALUE "9".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-AND-COMPUTE
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE
           STOP RUN.

       PARSE-AND-COMPUTE.
           MOVE FUNCTION NUMVAL(INPUT-RECORD) TO N
           MOVE N TO PADDED-N
           DISPLAY "Fibonacci sequence for n = " PADDED-N ":"
           PERFORM INIT-FIBONACCI

           MOVE SPACES TO RESULT
           MOVE FIB1 TO RESULT(2:300)
           PERFORM DISPLAY-ZERO-PADDED-FIB

           MOVE SPACES TO RESULT
           MOVE FIB2 TO RESULT(2:300)
           PERFORM DISPLAY-ZERO-PADDED-FIB

           MOVE 3 TO I
           PERFORM VARYING I FROM 3 BY 1 UNTIL I > N
               PERFORM DO-BIG-ADD
               MOVE FIB2 TO FIB1
               MOVE RESULT(2:300) TO FIB2
               PERFORM DISPLAY-ZERO-PADDED-FIB
           END-PERFORM
           DISPLAY "------------------------------".

       INIT-FIBONACCI.
           MOVE SPACES TO FIB1
           MOVE SPACES TO FIB2
           MOVE "0" TO FIB1(300:1)
           MOVE "1" TO FIB2(300:1)
           MOVE 3 TO I
           .

       FIND-FIRST-NONSPACE.
           MOVE 1 TO FIRST-NONSPACE
           PERFORM UNTIL RESULT(FIRST-NONSPACE + 1:1) NOT = SPACE
               ADD 1 TO FIRST-NONSPACE
               IF FIRST-NONSPACE > 300
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       DISPLAY-ZERO-PADDED-FIB.
           PERFORM FIND-FIRST-NONSPACE
           MOVE SPACES TO LINE-OUT
           COMPUTE DISP-IDX = 301 - FIRST-NONSPACE
           PERFORM VARYING IDX FROM DISP-IDX BY 1 UNTIL IDX > 300
               CONTINUE
           END-PERFORM
           MOVE RESULT(FIRST-NONSPACE + 1:DISP-IDX) TO LINE-OUT(300 - DISP-IDX + 1:DISP-IDX)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 300
               IF LINE-OUT(IDX:1) = SPACE
                   MOVE "0" TO LINE-OUT(IDX:1)
               END-IF
           END-PERFORM
           DISPLAY LINE-OUT.

       DO-BIG-ADD.
           MOVE SPACES TO RESULT
           MOVE 0 TO CARRY
           PERFORM VARYING IDX FROM MAX-DIGITS BY -1 UNTIL IDX = 0
               IF FIB1(IDX:1) = SPACE
                   MOVE 0 TO DIGIT1
               ELSE
                   COMPUTE DIGIT1 = FUNCTION NUMVAL(FIB1(IDX:1))
               END-IF

               IF FIB2(IDX:1) = SPACE
                   MOVE 0 TO DIGIT2
               ELSE
                   COMPUTE DIGIT2 = FUNCTION NUMVAL(FIB2(IDX:1))
               END-IF

               COMPUTE TEMP-SUM = DIGIT1 + DIGIT2 + CARRY

               IF TEMP-SUM > 9
                   COMPUTE TEMP-SUM = TEMP-SUM - 10
                   MOVE 1 TO CARRY
               ELSE
                   MOVE 0 TO CARRY
               END-IF

               EVALUATE TEMP-SUM
                   WHEN 0 MOVE D-CHAR-0 TO RESULT(IDX + 1:1)
                   WHEN 1 MOVE D-CHAR-1 TO RESULT(IDX + 1:1)
                   WHEN 2 MOVE D-CHAR-2 TO RESULT(IDX + 1:1)
                   WHEN 3 MOVE D-CHAR-3 TO RESULT(IDX + 1:1)
                   WHEN 4 MOVE D-CHAR-4 TO RESULT(IDX + 1:1)
                   WHEN 5 MOVE D-CHAR-5 TO RESULT(IDX + 1:1)
                   WHEN 6 MOVE D-CHAR-6 TO RESULT(IDX + 1:1)
                   WHEN 7 MOVE D-CHAR-7 TO RESULT(IDX + 1:1)
                   WHEN 8 MOVE D-CHAR-8 TO RESULT(IDX + 1:1)
                   WHEN 9 MOVE D-CHAR-9 TO RESULT(IDX + 1:1)
               END-EVALUATE
           END-PERFORM

           IF CARRY = 1
               MOVE "1" TO RESULT(1:1)
           ELSE
               MOVE " " TO RESULT(1:1)
           END-IF.
