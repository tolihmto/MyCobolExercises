       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANSI-CATALOG.
       AUTHOR. Thomas LIHOREAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ESC             PIC X VALUE X'1B'.

       01  FG-NAME.
           05 FG-N-TXT  OCCURS 8 TIMES PIC X(10).
       01  FG-CODE.
           05 FG-C-TXT  OCCURS 8 TIMES PIC X(5).

       01  BG-NAME.
           05 BG-N-TXT  OCCURS 8 TIMES PIC X(10).
       01  BG-CODE.
           05 BG-C-TXT  OCCURS 8 TIMES PIC X(5).

       01  STYLE-NAME.
           05 STY-N-TXT OCCURS 8 TIMES PIC X(20).
       01  STYLE-CODE.
           05 STY-C-TXT OCCURS 8 TIMES PIC X(5).

       01  ANSI-RESET   PIC X(4) VALUE X'1B' & "[0m".

       01  I            PIC 9 VALUE 0.
       01  J            PIC 9 VALUE 0.

       PROCEDURE DIVISION.

      * Initialisation couleurs texte
           MOVE "Black"   TO FG-N-TXT(1)
           MOVE "Red"     TO FG-N-TXT(2)
           MOVE "Green"   TO FG-N-TXT(3)
           MOVE "Yellow"  TO FG-N-TXT(4)
           MOVE "Blue"    TO FG-N-TXT(5)
           MOVE "Magenta" TO FG-N-TXT(6)
           MOVE "Cyan"    TO FG-N-TXT(7)
           MOVE "White"   TO FG-N-TXT(8)

           MOVE X'1B' & "[30m" TO FG-C-TXT(1)
           MOVE X'1B' & "[31m" TO FG-C-TXT(2)
           MOVE X'1B' & "[32m" TO FG-C-TXT(3)
           MOVE X'1B' & "[33m" TO FG-C-TXT(4)
           MOVE X'1B' & "[34m" TO FG-C-TXT(5)
           MOVE X'1B' & "[35m" TO FG-C-TXT(6)
           MOVE X'1B' & "[36m" TO FG-C-TXT(7)
           MOVE X'1B' & "[37m" TO FG-C-TXT(8)

      * Initialisation couleurs fond
           MOVE "Black"   TO BG-N-TXT(1)
           MOVE "Red"     TO BG-N-TXT(2)
           MOVE "Green"   TO BG-N-TXT(3)
           MOVE "Yellow"  TO BG-N-TXT(4)
           MOVE "Blue"    TO BG-N-TXT(5)
           MOVE "Magenta" TO BG-N-TXT(6)
           MOVE "Cyan"    TO BG-N-TXT(7)
           MOVE "White"   TO BG-N-TXT(8)

           MOVE X'1B' & "[40m" TO BG-C-TXT(1)
           MOVE X'1B' & "[41m" TO BG-C-TXT(2)
           MOVE X'1B' & "[42m" TO BG-C-TXT(3)
           MOVE X'1B' & "[43m" TO BG-C-TXT(4)
           MOVE X'1B' & "[44m" TO BG-C-TXT(5)
           MOVE X'1B' & "[45m" TO BG-C-TXT(6)
           MOVE X'1B' & "[46m" TO BG-C-TXT(7)
           MOVE X'1B' & "[47m" TO BG-C-TXT(8)

      * Initialisation styles
           MOVE "Bold"              TO STY-N-TXT(1)
           MOVE "Italic"            TO STY-N-TXT(2)
           MOVE "Underline"         TO STY-N-TXT(3)
           MOVE "Double Underline"  TO STY-N-TXT(4)
           MOVE "Blink"             TO STY-N-TXT(5)
           MOVE "Reverse"           TO STY-N-TXT(6)
           MOVE "Strikethrough"     TO STY-N-TXT(7)
           MOVE "Hidden"            TO STY-N-TXT(8)

           MOVE X'1B' & "[1m"  TO STY-C-TXT(1)
           MOVE X'1B' & "[3m"  TO STY-C-TXT(2)
           MOVE X'1B' & "[4m"  TO STY-C-TXT(3)
           MOVE X'1B' & "[21m" TO STY-C-TXT(4)
           MOVE X'1B' & "[5m"  TO STY-C-TXT(5)
           MOVE X'1B' & "[7m"  TO STY-C-TXT(6)
           MOVE X'1B' & "[9m"  TO STY-C-TXT(7)
           MOVE X'1B' & "[8m"  TO STY-C-TXT(8)

      * Affichage du catalogue
           DISPLAY ANSI-RESET "=== PALETTE DE COULEURS (Texte/Fond) ===".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 8
                   DISPLAY FG-C-TXT(I) BG-C-TXT(J)
                           FG-N-TXT(I) " / " BG-N-TXT(J)
                           ANSI-RESET
               END-PERFORM
           END-PERFORM.

           DISPLAY ANSI-RESET "=== STYLES ANSI ===".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               DISPLAY STY-C-TXT(I) STY-N-TXT(I) ANSI-RESET
           END-PERFORM.

           DISPLAY ANSI-RESET "=== FIN DU CATALOGUE ===".

           STOP RUN.
