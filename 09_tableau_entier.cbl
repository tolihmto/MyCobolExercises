       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAISIRENTIERS. 
       
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ENTIERS. 
           05  ENTIER  PIC 99  OCCURS 3 TIMES. 
       01  I  PIC 9(1).
       
       
       
       PROCEDURE DIVISION.
       
           DISPLAY "Saisie des entiers :". 
       
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY "Entier ", I, " :" 
               ACCEPT ENTIER(I) 
           END-PERFORM.
       
           DISPLAY "Récapitulatif :".
       
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               DISPLAY "Entier ", I, " : ", ENTIER(I) 
           END-PERFORM.
       
           STOP RUN.
           