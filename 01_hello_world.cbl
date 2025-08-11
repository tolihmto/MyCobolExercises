       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWORLD.
       AUTHOR. Thomas LIHOREAU.

       PROCEDURE DIVISION.
           DISPLAY "Hello world!".
           
           perform 3000-AFFICHAGE-DEB
              thru 3000-AFFICHAGE-FIN
           
           DISPLAY "This is a simple COBOL program.".
           STOP RUN.

       3000-AFFICHAGE-DEB.
           DISPLAY "Starting the display section...".
       3000-AFFICHAGE-FIN.
           DISPLAY "Ending the display section...".
           EXIT.

       END PROGRAM HELLOWORLD.
       