       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUESTION-03.
       AUTHOR. Thomas LIHOREAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME   PIC X(30).
       01  WS-NAME-LEN     PIC 99.

       PROCEDURE DIVISION.
           DISPLAY "Bonjour, très cher utilisateur!".
           DISPLAY "Quel est votre nom?".
           ACCEPT WS-NAME.
           INSPECT FUNCTION REVERSE(WS-NAME)
           TALLYING WS-NAME-LEN FOR LEADING SPACES
           COMPUTE WS-NAME-LEN = 30 - WS-NAME-LEN
           DISPLAY "Bonjour, " WS-NAME(1:WS-NAME-LEN + 1) "!".

           STOP RUN.
