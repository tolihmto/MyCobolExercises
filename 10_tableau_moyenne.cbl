       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOYENNEENTIERS. 
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ENTIERS. 
           05  ENTIER  PIC 99  OCCURS 3 TIMES. 
       01  I  PIC 9(1).
       01  TOTAL     PIC 999 VALUE 0.
       01  MOYENNE   PIC 99V9.
       
       PROCEDURE DIVISION.
       
           DISPLAY "Saisie des entiers :". 
       
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY "Entier ", I, " :" 
               ACCEPT ENTIER(I) 
           END-PERFORM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               COMPUTE TOTAL = TOTAL + ENTIER(I)
           END-PERFORM.
           COMPUTE MOYENNE = TOTAL / 3.
       
       
           DISPLAY "Récapitulatif :".
       
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY "Entier ", I, " : ", ENTIER(I) 
           END-PERFORM.
           DISPLAY "Moyenne : ", MOYENNE.
       
           STOP RUN.
