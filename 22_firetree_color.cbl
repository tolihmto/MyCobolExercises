       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIRETREE-COLOR-23.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N             PIC 99 VALUE ZERO.
       77  I             PIC 99.
       77  J             PIC 99.
       77  K             PIC 99.
       77  LEVEL         PIC 99.
       77  T             PIC 99.
       77  TSTART        PIC 99.
       77  WS-LINES         PIC 99.
       77  BASE          PIC 9(4).
       77  WS-WIDTH      PIC 9(4).
       77  WS-MAX        PIC 9(4).
       77  COLOR-ID      PIC 99.
       77  DECOR-ID      PIC 99.
       77  WS-RESET      PIC X(4) VALUE X"1B5B306D".
       77  STAR-CHAR     PIC X VALUE "*".
       77  LEAF-CHAR     PIC X VALUE "*".
       77  BALL-CHAR     PIC X VALUE "o".
       77  GARLAND-CHAR  PIC X VALUE "~".
       77  TRUNK-CHAR    PIC X VALUE "|".
       01  COLORS.
           05  FG-COLOR OCCURS 13 TIMES PIC X(5).
       01  DECORS.
           05  DECOR-TEXT OCCURS 3 TIMES PIC X.

       PROCEDURE DIVISION.
       MAIN-START.
      * Couleurs vives
           MOVE X"1B5B33316D" TO FG-COLOR(1)
           MOVE X"1B5B33326D" TO FG-COLOR(2)
           MOVE X"1B5B33336D" TO FG-COLOR(3)
           MOVE X"1B5B33346D" TO FG-COLOR(4)
           MOVE X"1B5B33356D" TO FG-COLOR(5)
           MOVE X"1B5B33366D" TO FG-COLOR(6)
           MOVE X"1B5B33376D" TO FG-COLOR(7)
           MOVE X"1B5B39316D" TO FG-COLOR(8)
           MOVE X"1B5B39326D" TO FG-COLOR(9)
           MOVE X"1B5B39336D" TO FG-COLOR(10)
           MOVE X"1B5B39346D" TO FG-COLOR(11)
           MOVE X"1B5B39356D" TO FG-COLOR(12)
           MOVE X"1B5B39366D" TO FG-COLOR(13)

      * Décors
           MOVE "*" TO DECOR-TEXT(1)
           MOVE "o" TO DECOR-TEXT(2)
           MOVE "~" TO DECOR-TEXT(3)

      * Taille
           ACCEPT N FROM ARGUMENT-VALUE
           IF N <= 0
               DISPLAY "USAGE: ./firetree <taille>"
               STOP RUN
           END-IF

      * Largeur totale
           COMPUTE WS-MAX = N * N + 3 * N + 3

      * Etoile (jaune)
           MOVE 1 TO WS-WIDTH
           PERFORM DRAW-CENTER-LINE-STAR

      * Sections
           PERFORM VARYING LEVEL FROM 1 BY 1 UNTIL LEVEL > N
               COMPUTE WS-LINES = LEVEL + 3
               COMPUTE BASE  = LEVEL * LEVEL + LEVEL - 1
               IF LEVEL = 1
                   MOVE 1 TO TSTART
               ELSE
                   MOVE 0 TO TSTART
               END-IF
               PERFORM VARYING T FROM TSTART BY 1
                   UNTIL T > (WS-LINES - 1)
                   COMPUTE WS-WIDTH = BASE + 2 * T
                   PERFORM DRAW-CENTER-LINE
               END-PERFORM
           END-PERFORM

      * Tronc
           COMPUTE WS-WIDTH = 2 * N - 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM DRAW-CENTER-LINE-TRUNK
           END-PERFORM
           STOP RUN.

      * ---------- Feuillage ----------
       DRAW-CENTER-LINE.
           MOVE 1 TO J
           PERFORM UNTIL J > ((WS-MAX - WS-WIDTH) / 2)
               DISPLAY " " WITH NO ADVANCING
               ADD 1 TO J
           END-PERFORM
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > WS-WIDTH
               COMPUTE DECOR-ID = FUNCTION RANDOM * 10 + 1
               IF DECOR-ID <= 8
                   DISPLAY FG-COLOR(2) WITH NO ADVANCING
                   DISPLAY LEAF-CHAR  WITH NO ADVANCING
               ELSE
                   COMPUTE DECOR-ID = FUNCTION RANDOM * 2 + 2
                   COMPUTE COLOR-ID = FUNCTION RANDOM * 13 + 1
                   DISPLAY FG-COLOR(COLOR-ID) WITH NO ADVANCING
                   DISPLAY DECOR-TEXT(DECOR-ID) WITH NO ADVANCING
               END-IF
           END-PERFORM
           DISPLAY WS-RESET.

      * ---------- Etoile ----------
       DRAW-CENTER-LINE-STAR.
           MOVE 1 TO J
           PERFORM UNTIL J > ((WS-MAX - WS-WIDTH) / 2)
               DISPLAY " " WITH NO ADVANCING
               ADD 1 TO J
           END-PERFORM
           DISPLAY X"1B5B33336D" WITH NO ADVANCING
           DISPLAY STAR-CHAR    WITH NO ADVANCING
           DISPLAY WS-RESET.

      * ---------- Tronc ----------
       DRAW-CENTER-LINE-TRUNK.
           MOVE 1 TO J
           PERFORM UNTIL J > ((WS-MAX - WS-WIDTH) / 2)
               DISPLAY " " WITH NO ADVANCING
               ADD 1 TO J
           END-PERFORM
           DISPLAY X"1B5B33376D" WITH NO ADVANCING
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > WS-WIDTH
               DISPLAY TRUNK-CHAR WITH NO ADVANCING
           END-PERFORM
           DISPLAY WS-RESET.
