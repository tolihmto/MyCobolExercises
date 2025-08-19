       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOMMOY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-N-NUM        PIC 9(2).
       01  WS-NUM          PIC 9(3).
       01  WS-SUM          PIC 9(5)       VALUE 0.
       01  WS-MOY          PIC 9(5)V9(2)  VALUE 0.
       01  I               PIC 9(3).

       PROCEDURE DIVISION.
           DISPLAY "Combien de nombres ? ".
           ACCEPT WS-N-NUM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-N-NUM
               DISPLAY "Entrez le nombre " I " : "
               ACCEPT WS-NUM
               ADD WS-NUM TO WS-SUM
           END-PERFORM.

           IF WS-N-NUM > 0
               COMPUTE WS-MOY ROUNDED = WS-SUM / WS-N-NUM
           ELSE
               MOVE 0 TO WS-MOY
           END-IF.

           DISPLAY " ".
           DISPLAY "Somme = " WS-SUM.
           DISPLAY "Moyenne = " WS-MOY.

           STOP RUN.
