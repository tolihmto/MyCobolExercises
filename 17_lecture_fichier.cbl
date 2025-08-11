       IDENTIFICATION DIVISION.
       PROGRAM-ID. LECTURE-FICHIER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-TEXTE ASSIGN TO NOM-FICHIER
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-TEXTE.
       01  LIGNE-TEXTE             PIC X(100).

       WORKING-STORAGE SECTION.
       01  NOM-FICHIER             PIC X(100).
       01  ARGUMENT                PIC X(100).
       01  FIN-LU                  PIC X VALUE 'N'.
           88 FIN-DE-FICHIER      VALUE 'O'.
           88 ENCORE-DONNEES      VALUE 'N'.
       01  NB-MOTS-LIGNE           PIC 99 VALUE ZERO.
       01  NB-MOTS-TOTAL           PIC 999 VALUE ZERO.
       01  POS-CARACTERE           PIC 99 VALUE 1.
       01  CARACTERE               PIC X.
       01  DANS-MOT                PIC X VALUE 'N'.
       01  LONGUEUR-LIGNE          PIC 99 VALUE 0.
       01  LONGUEUR-ARG            PIC 9(3).

       PROCEDURE DIVISION.
           ACCEPT ARGUMENT FROM ARGUMENT-VALUE
           COMPUTE LONGUEUR-ARG =
               FUNCTION LENGTH(FUNCTION TRIM(ARGUMENT))

           IF LONGUEUR-ARG = 0
               DISPLAY "USAGE: ./lecture_fichier_17 <chemin-fichier>"
               STOP RUN
           END-IF

           MOVE ARGUMENT TO NOM-FICHIER

           OPEN INPUT FICHIER-TEXTE

           PERFORM UNTIL FIN-DE-FICHIER
               READ FICHIER-TEXTE
                   AT END
                       DISPLAY "Fin du fichier atteinte."
                       SET FIN-DE-FICHIER TO TRUE
                   NOT AT END
                       INSPECT LIGNE-TEXTE
                           REPLACING ALL X'0D' BY SPACE

                       DISPLAY "Ligne lue : " LIGNE-TEXTE

                       MOVE 0 TO NB-MOTS-LIGNE
                       MOVE 1 TO POS-CARACTERE
                       MOVE 'N' TO DANS-MOT

                       COMPUTE LONGUEUR-LIGNE =
                           FUNCTION LENGTH(FUNCTION TRIM(LIGNE-TEXTE))

                       PERFORM UNTIL POS-CARACTERE > LONGUEUR-LIGNE
                           MOVE LIGNE-TEXTE(POS-CARACTERE:1)
                               TO CARACTERE

                           IF (CARACTERE >= 'A' AND CARACTERE <= 'Z') OR
                              (CARACTERE >= 'a' AND CARACTERE <= 'z') OR
                              (CARACTERE >= '0' AND CARACTERE <= '9') OR
                              (CARACTERE = '-')
                               IF DANS-MOT = 'N'
                                   ADD 1 TO NB-MOTS-LIGNE
                                   MOVE 'O' TO DANS-MOT
                               END-IF
                           ELSE
                               MOVE 'N' TO DANS-MOT
                           END-IF

                           ADD 1 TO POS-CARACTERE
                       END-PERFORM

                       DISPLAY "Nombre de mots : " NB-MOTS-LIGNE
                       ADD NB-MOTS-LIGNE TO NB-MOTS-TOTAL
               END-READ
           END-PERFORM

           CLOSE FICHIER-TEXTE

           DISPLAY "Total de mots dans le fichier : " NB-MOTS-TOTAL

           STOP RUN.
