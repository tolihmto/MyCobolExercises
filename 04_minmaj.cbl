       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINMAJ.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AGE  PIC 9(3).
       
       PROCEDURE DIVISION.
           DISPLAY "Quel âge avez-vous ?".
           ACCEPT WS-AGE.
       
           IF WS-AGE >= 18
               DISPLAY "Vous êtes majeur."
           ELSE
               DISPLAY "Vous êtes mineur."
           END-IF.
       
           STOP RUN.
