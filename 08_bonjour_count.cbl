       IDENTIFICATION DIVISION.
       PROGRAM-ID. BONJOURCOUNT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01    I    PIC 9(2).
       
       PROCEDURE DIVISION.
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
                  DISPLAY "Bonjour ", I
              END-PERFORM.
              STOP RUN.
