       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRENOMAGE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRENOM       PIC X(16).
       01  WS-AGE          PIC 9(3).
       01  WS-MESSAGE      PIC X(60).
       
       PROCEDURE DIVISION.
           DISPLAY "Comment vous appelez-vous ?".
           ACCEPT WS-PRENOM.
           DISPLAY "Quel âge avez-vous ?".
           ACCEPT WS-AGE.
       
           EVALUATE TRUE
               WHEN WS-AGE >= 100
                   MOVE "vous êtes une légende vivante !" 
                   TO WS-MESSAGE
               WHEN WS-AGE >= 18
                   MOVE "vous êtes majeur." TO WS-MESSAGE
               WHEN OTHER
                   MOVE "vous êtes mineur." TO WS-MESSAGE
           END-EVALUATE.
       
           DISPLAY "Bonjour ", WS-PRENOM, ", ", WS-MESSAGE.
           STOP RUN.
