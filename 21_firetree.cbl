       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIRETREE.
       AUTHOR. Thomas LIHOREAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARG                PIC X(20).
       01  TAILLE             PIC 9(3) VALUE 0.
       01  S                  PIC 9(3).
       01  H                  PIC 9(3).
       01  I                  PIC 9(3).
       01  LIGNE              PIC X(300).
       01  FILL-ETOILES       PIC X(300) VALUE ALL "*".
       01  FILL-BARRES        PIC X(300) VALUE ALL "|".
       01  WMAX               PIC 9(5) VALUE 0.
       01  WCUR               PIC 9(5) VALUE 0.
       01  WLAST              PIC 9(5) VALUE 0.
       01  LEFTPAD            PIC 9(5) VALUE 0.
       01  HAUTEUR-SEG        PIC 9(3) VALUE 0.
       01  LARGEUR-TRONC      PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
      * Lire l'argument 1 (taille)
           ACCEPT ARG FROM ARGUMENT-VALUE
           IF FUNCTION LENGTH(FUNCTION TRIM(ARG)) = 0
               DISPLAY "USAGE: ./firetree <taille>"
               STOP RUN
           END-IF

           MOVE FUNCTION NUMVAL(ARG) TO TAILLE
           IF TAILLE < 1
               DISPLAY "Erreur: taille doit etre >= 1"
               STOP RUN
           END-IF

      * Calcul largeur max: N^2 + 3N + 3
           COMPUTE WMAX = TAILLE * TAILLE + 3 * TAILLE + 3

      * Segment 1 : largeur finale = 7
           MOVE 7 TO WLAST

      * Boucle segments 1..N
           PERFORM VARYING S FROM 1 BY 1 UNTIL S > TAILLE
               COMPUTE HAUTEUR-SEG = S + 3
               IF S = 1
                   MOVE 1 TO WCUR
               ELSE
                   COMPUTE WCUR = WLAST - 2
               END-IF

               PERFORM VARYING H FROM 1 BY 1
                        UNTIL H > HAUTEUR-SEG
                   COMPUTE LEFTPAD = (WMAX - WCUR) / 2
                   MOVE ALL " " TO LIGNE
                   IF WCUR > 0
                       MOVE FILL-ETOILES(1:WCUR)
      -                    TO LIGNE(LEFTPAD + 1:WCUR)
                   END-IF
                   DISPLAY LIGNE(1:WMAX)
                   ADD 2 TO WCUR
               END-PERFORM

               COMPUTE WLAST = WCUR - 2
           END-PERFORM

      * Tronc : largeur = 2*N-1, hauteur = N (centré)
           COMPUTE LARGEUR-TRONC = 2 * TAILLE - 1
           COMPUTE LEFTPAD = (WMAX - LARGEUR-TRONC) / 2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TAILLE
               MOVE ALL " " TO LIGNE
               IF LARGEUR-TRONC > 0
                   MOVE FILL-BARRES(1:LARGEUR-TRONC)
      -                TO LIGNE(LEFTPAD + 1:LARGEUR-TRONC)
               END-IF
               DISPLAY LIGNE(1:WMAX)
           END-PERFORM

           STOP RUN.
