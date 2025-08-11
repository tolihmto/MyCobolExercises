       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUTCOLORS18.
       AUTHOR. Thomas LIHOREAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ESC                 PIC X VALUE X'1B'.
       01  CSI                 PIC X(2) VALUE "[".
       01  ANSI-RESET          PIC X(4)  VALUE X'1B' & "[" & "0m".
       01  ANSI-BOLD           PIC X(4)  VALUE X'1B' & "[" & "1m".
       01  ANSI-UNDER          PIC X(4)  VALUE X'1B' & "[" & "4m".

       01  FG-BLACK            PIC X(5)  VALUE X'1B' & "[" & "30m".
       01  FG-RED              PIC X(5)  VALUE X'1B' & "[" & "31m".
       01  FG-GREEN            PIC X(5)  VALUE X'1B' & "[" & "32m".
       01  FG-YELLOW           PIC X(5)  VALUE X'1B' & "[" & "33m".
       01  FG-BLUE             PIC X(5)  VALUE X'1B' & "[" & "34m".
       01  FG-MAGENTA          PIC X(5)  VALUE X'1B' & "[" & "35m".
       01  FG-CYAN             PIC X(5)  VALUE X'1B' & "[" & "36m".
       01  FG-WHITE            PIC X(5)  VALUE X'1B' & "[" & "37m".
       01  FG-RESET            PIC X(5)  VALUE X'1B' & "[" & "39m".

       01  BG-BLACK            PIC X(5)  VALUE X'1B' & "[" & "40m".
       01  BG-RED              PIC X(5)  VALUE X'1B' & "[" & "41m".
       01  BG-GREEN            PIC X(5)  VALUE X'1B' & "[" & "42m".
       01  BG-YELLOW           PIC X(5)  VALUE X'1B' & "[" & "43m".
       01  BG-BLUE             PIC X(5)  VALUE X'1B' & "[" & "44m".
       01  BG-MAGENTA          PIC X(5)  VALUE X'1B' & "[" & "45m".
       01  BG-CYAN             PIC X(5)  VALUE X'1B' & "[" & "46m".
       01  BG-WHITE            PIC X(5)  VALUE X'1B' & "[" & "47m".
       01  BG-RESET            PIC X(5)  VALUE X'1B' & "[" & "49m".

       01  ANSI-BLINK          PIC X(4) VALUE X'1B' & "[" & "5m".
       01  ANSI-REVERSE        PIC X(4) VALUE X'1B' & "[" & "7m".
       01  ANSI-HIDDEN         PIC X(4) VALUE X'1B' & "[" & "8m".
       01  ANSI-STRIKETHROUGH  PIC X(4) VALUE X'1B' & "[" & "9m".
       01  ANSI-ITALIC         PIC X(4) VALUE X'1B' & "[" & "3m".
       01  ANSI-UNDERLINE      PIC X(4) VALUE X'1B' & "[" & "4m".
       01  ANSI-DOUBLEUNDERLINE PIC X(5) VALUE X'1B' & "[" & "21m".
         

       
       PROCEDURE DIVISION.

           DISPLAY ANSI-BOLD "Bienvenue dans le programme" 
                             " de couleurs !".
           DISPLAY ANSI-UNDER "Voici quelques exemples de couleurs :"
                   ANSI-RESET.

           DISPLAY FG-RED "Texte en rouge" FG-RESET.
           DISPLAY FG-GREEN "Texte en vert" FG-RESET.
           DISPLAY FG-YELLOW "Texte en jaune" FG-RESET.
           DISPLAY FG-BLUE "Texte en bleu" FG-RESET.
           DISPLAY FG-MAGENTA "Texte en magenta" FG-RESET.
           DISPLAY FG-CYAN "Texte en cyan" FG-RESET.
           DISPLAY FG-WHITE "Texte en blanc" FG-RESET.

           DISPLAY BG-RED "Fond rouge" BG-RESET.
           DISPLAY BG-GREEN "Fond vert" BG-RESET.
           DISPLAY BG-YELLOW "Fond jaune" BG-RESET.
           DISPLAY BG-BLUE "Fond bleu" BG-RESET.
           DISPLAY BG-MAGENTA "Fond magenta" BG-RESET.
           DISPLAY BG-CYAN "Fond cyan" BG-RESET.
           DISPLAY BG-WHITE "Fond blanc" BG-RESET.

           DISPLAY ANSI-BLINK "Texte clignotant" ANSI-RESET.
           DISPLAY ANSI-REVERSE "Texte inversé" ANSI-RESET.
           DISPLAY ANSI-HIDDEN "Texte caché" ANSI-RESET.
           DISPLAY ANSI-STRIKETHROUGH "Texte barré" ANSI-RESET.
           DISPLAY ANSI-ITALIC "Texte en italique" ANSI-RESET.
           DISPLAY ANSI-UNDERLINE "Texte souligné" ANSI-RESET.
           DISPLAY ANSI-DOUBLEUNDERLINE "Texte double souligné" 
                   ANSI-RESET.
           DISPLAY ANSI-BOLD "Texte en gras" ANSI-RESET.
           
           DISPLAY ANSI-RESET "Fin du programme. Merci d'avoir utilisé
      -                       " les couleurs !".

           STOP RUN.
