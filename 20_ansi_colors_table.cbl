       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANSI-COLORS-TABLE.
       AUTHOR. Thomas LIHOREAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ESC                 PIC X VALUE X'1B'.
       01  ANSI-RESET           PIC X(4) VALUE X'1B' & "[" & "0m".
       01  FG-RED               PIC X(5) VALUE X'1B' & "[" & "31m".
       01  FG-GREEN             PIC X(5) VALUE X'1B' & "[" & "32m".
       01  FG-YELLOW            PIC X(5) VALUE X'1B' & "[" & "33m".
       01  FG-BLUE              PIC X(5) VALUE X'1B' & "[" & "34m".
       01  FG-MAGENTA           PIC X(5) VALUE X'1B' & "[" & "35m".
       01  FG-CYAN              PIC X(5) VALUE X'1B' & "[" & "36m".
       01  FG-WHITE             PIC X(5) VALUE X'1B' & "[" & "37m".
       01  BG-RED               PIC X(5) VALUE X'1B' & "[" & "41m".
       01  BG-GREEN             PIC X(5) VALUE X'1B' & "[" & "42m".
       01  BG-YELLOW            PIC X(5) VALUE X'1B' & "[" & "43m".
       01  BG-BLUE              PIC X(5) VALUE X'1B' & "[" & "44m".
       01  BG-MAGENTA           PIC X(5) VALUE X'1B' & "[" & "45m".
       01  BG-CYAN              PIC X(5) VALUE X'1B' & "[" & "46m".
       01  BG-WHITE             PIC X(5) VALUE X'1B' & "[" & "47m".
       01  ANSI-BOLD            PIC X(4) VALUE X'1B' & "[" & "1m".
       01  ANSI-UNDERLINE       PIC X(4) VALUE X'1B' & "[" & "4m".
       01  ANSI-BLINK           PIC X(4) VALUE X'1B' & "[" & "5m".
       01  ANSI-REVERSE         PIC X(4) VALUE X'1B' & "[" & "7m".
       01  ANSI-STRIKETHROUGH   PIC X(4) VALUE X'1B' & "[" & "9m".
       01  ANSI-ITALIC          PIC X(4) VALUE X'1B' & "[" & "3m".
       01  ANSI-DOUBLEUNDERLINE PIC X(5) VALUE X'1B' & "[" & "21m".

       01  BOX-H   PIC X(6) VALUE "──".
       01  BOX-V   PIC X(3) VALUE "│".
       01  BOX-TL  PIC X(3) VALUE "┌".
       01  BOX-TR  PIC X(3) VALUE "┐".
       01  BOX-BL  PIC X(3) VALUE "└".
       01  BOX-BR  PIC X(3) VALUE "┘".
       01  BOX-T   PIC X(3) VALUE "┬".
       01  BOX-M   PIC X(3) VALUE "┼".
       01  BOX-L   PIC X(3) VALUE "├".
       01  BOX-R   PIC X(3) VALUE "┤".
       01  BOX-B   PIC X(3) VALUE "┴".

       PROCEDURE DIVISION.
           DISPLAY BOX-TL BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-T
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-T BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-TR

           DISPLAY BOX-V "        Texte         " BOX-V "      Fond    "
               "  " BOX-V "        Style         " BOX-V

           DISPLAY BOX-L BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-M
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-M BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-R

           DISPLAY BOX-V "        " FG-RED "Rouge" ANSI-RESET "        "
               " " BOX-V
               "      " BG-RED "Rouge" ANSI-RESET "     " BOX-V
               "        " ANSI-UNDERLINE "Souligné" ANSI-RESET "      "
               BOX-V

           DISPLAY BOX-V "        " FG-GREEN "Vert" ANSI-RESET "       "
               "   " BOX-V
               "      " BG-GREEN "Vert" ANSI-RESET "      " BOX-V
               "        " ANSI-BOLD "Gras" ANSI-RESET "          " BOX-V

           DISPLAY BOX-V "        " FG-YELLOW "Jaune" ANSI-RESET "     "
               "    " BOX-V
               "      " BG-YELLOW "Jaune" ANSI-RESET "     " BOX-V
               "        " ANSI-BLINK "Clignotant" ANSI-RESET "    " 
               BOX-V

           DISPLAY BOX-V "        " FG-BLUE "Bleu" ANSI-RESET "        "
               "  " BOX-V
               "      " BG-BLUE "Bleu" ANSI-RESET "      " BOX-V
               "        " ANSI-REVERSE "Inversé" ANSI-RESET "       " 
               BOX-V

           DISPLAY BOX-V "        " FG-MAGENTA "Magenta" ANSI-RESET "  "
               "     " BOX-V
               "      " BG-MAGENTA "Magenta" ANSI-RESET "   " BOX-V
               "        " ANSI-STRIKETHROUGH "Barré" ANSI-RESET "      "
      -        "   " BOX-V

           DISPLAY BOX-V "        " FG-CYAN "Cyan" ANSI-RESET "        "
               "  " BOX-V
               "      " BG-CYAN "Cyan" ANSI-RESET "      " BOX-V
               "        " ANSI-ITALIC "Italique" ANSI-RESET "      " 
               BOX-V

           DISPLAY BOX-V "        " FG-WHITE "Blanc" ANSI-RESET "      "
               "   " BOX-V
               "      " BG-WHITE "Blanc" ANSI-RESET "     " BOX-V
               "        " ANSI-DOUBLEUNDERLINE "Double" ANSI-RESET
               "        " BOX-V

           DISPLAY BOX-BL BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-B
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-B BOX-H BOX-H BOX-H BOX-H BOX-H BOX-H
               BOX-H BOX-H BOX-H BOX-H BOX-H BOX-BR

           STOP RUN.
