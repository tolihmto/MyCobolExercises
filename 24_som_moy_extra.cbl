       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOMMOY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-N-NUM        PIC S9(3)       VALUE 0.
       01  WS-NUM          PIC S9(9)       VALUE 0.
       01  WS-SUM          PIC S9(11)      VALUE 0.
       01  WS-MOY          PIC S9(9)V9(2)  VALUE 0.
       01  I               PIC 9(3)        VALUE 0.

       01  I-EDIT          PIC ZZ9.
       01  WS-SUM-EDIT     PIC -ZZZZ9.
       01  WS-MOY-EDIT     PIC -ZZZZ9.99.

       01  TEMP-ALPHA      PIC X(32).
       01  WS-SUM-STR      PIC X(20).
       01  WS-MOY-STR      PIC X(20).
       01  WS-TEMP         PIC X(20).
       01  PTR             PIC 99          VALUE 1.
       01  LENG            PIC 99          VALUE 0.
       01  SUM-LEN         PIC 99          VALUE 6.
       01  MOY-LEN         PIC 99          VALUE 9.

       PROCEDURE DIVISION.
           PERFORM UNTIL WS-N-NUM > 0
               DISPLAY "Combien de nombres ? " WITH NO ADVANCING
               ACCEPT  TEMP-ALPHA
               MOVE FUNCTION NUMVAL(TEMP-ALPHA) TO WS-N-NUM
               IF WS-N-NUM <= 0
                   DISPLAY "Erreur : merci d'entrer un nombre"
                   DISPLAY "strictement positif."
               END-IF
           END-PERFORM

           MOVE 0 TO WS-SUM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-N-NUM
               MOVE I TO I-EDIT
               DISPLAY "Entrez le nombre " I-EDIT " : "
                  WITH NO ADVANCING
               ACCEPT TEMP-ALPHA
               MOVE FUNCTION NUMVAL(TEMP-ALPHA) TO WS-NUM
               ADD WS-NUM TO WS-SUM
           END-PERFORM

           IF WS-N-NUM > 0
               COMPUTE WS-MOY ROUNDED = WS-SUM / WS-N-NUM
           END-IF

           DISPLAY " "

      *---- Compactage retro: somme (-ZZZZ9)
           MOVE WS-SUM TO WS-SUM-EDIT
           MOVE WS-SUM-EDIT TO WS-TEMP
           MOVE 1 TO PTR
           PERFORM UNTIL PTR > SUM-LEN
               IF WS-TEMP(PTR:1) NOT = SPACE
                   EXIT PERFORM
               END-IF
               ADD 1 TO PTR
           END-PERFORM
           COMPUTE LENG = SUM-LEN - PTR + 1
           MOVE SPACES TO WS-SUM-STR
           IF LENG > 0
               MOVE WS-TEMP(PTR:LENG) TO WS-SUM-STR
           END-IF

      *---- Compactage retro: moyenne (-ZZZZ9.99)
           MOVE WS-MOY TO WS-MOY-EDIT
           MOVE WS-MOY-EDIT TO WS-TEMP
           MOVE 1 TO PTR
           PERFORM UNTIL PTR > MOY-LEN
               IF WS-TEMP(PTR:1) NOT = SPACE
                   EXIT PERFORM
               END-IF
               ADD 1 TO PTR
           END-PERFORM
           COMPUTE LENG = MOY-LEN - PTR + 1
           MOVE SPACES TO WS-MOY-STR
           IF LENG > 0
               MOVE WS-TEMP(PTR:LENG) TO WS-MOY-STR
           END-IF

           DISPLAY "Somme = " WS-SUM-STR
           DISPLAY "Moyenne = " WS-MOY-STR

           STOP RUN.
