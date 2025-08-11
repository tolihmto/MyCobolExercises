       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOUBLONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-SORTIE ASSIGN TO "resultats.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FICHIER-SORTIE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS LIGNE-SORTIE.

       01  LIGNE-SORTIE         PIC X(80).

       WORKING-STORAGE SECTION.

       01  ENTIERS.
           05 ENTIER            PIC S99 OCCURS 10 TIMES.

       01  DOUBLONS-TROUVES.
           05 VALEUR            PIC S99 OCCURS 10 TIMES.
           05 OCCURENCE         PIC 9(2) OCCURS 10 TIMES.

       01  I                    PIC 9(2).
       01  J                    PIC 9(2).
       01  L                    PIC 9(2).
       01  K                    PIC 9(2) VALUE 1.

       01  DEJA-AJOUTE          PIC X VALUE 'N'.
       01  OK-SAISIE            PIC X VALUE 'N'.

       01  TEMP-ALPHA           PIC X(4).
       01  TEMP-NUM             PIC S999.

       01  TMP-VALEUR           PIC S99.
       01  TMP-OCCURENCE        PIC 9(2).
       01  NB-ETOILES           PIC 9(2).

       01  ETOILES-LIGNE        PIC X(50).

       01 TEXTE-VALEUR   PIC S99 SIGN IS LEADING SEPARATE.



       PROCEDURE DIVISION.

           DISPLAY "Saisie de 10 entiers (de -99 a 99) :"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE 'N' TO OK-SAISIE
               PERFORM UNTIL OK-SAISIE = 'O'
                   DISPLAY "Entier " I " :"
                   ACCEPT TEMP-ALPHA
                   MOVE FUNCTION NUMVAL (TEMP-ALPHA) TO TEMP-NUM
                   IF TEMP-NUM >= -99 AND TEMP-NUM <= 99
                       MOVE TEMP-NUM TO ENTIER(I)
                       MOVE 'O' TO OK-SAISIE
                   ELSE
                       DISPLAY "Valeur invalide. Entrez un entier"
      -                " entre -99 et 99."
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               MOVE 'N' TO DEJA-AJOUTE
               PERFORM VARYING L FROM 1 BY 1 UNTIL L >= K
                   IF ENTIER(I) = VALEUR(L)
                       ADD 1 TO OCCURENCE(L)
                       MOVE 'O' TO DEJA-AJOUTE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF DEJA-AJOUTE = 'N'
                   MOVE ENTIER(I) TO VALEUR(K)
                   MOVE 1 TO OCCURENCE(K)
                   ADD 1 TO K
               END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= K - 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J >= K - I
                   IF OCCURENCE(J) < OCCURENCE(J + 1)
                       MOVE OCCURENCE(J) TO TMP-OCCURENCE
                       MOVE OCCURENCE(J + 1) TO OCCURENCE(J)
                       MOVE TMP-OCCURENCE TO OCCURENCE(J + 1)
                       MOVE VALEUR(J) TO TMP-VALEUR
                       MOVE VALEUR(J + 1) TO VALEUR(J)
                       MOVE TMP-VALEUR TO VALEUR(J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM

           OPEN OUTPUT FICHIER-SORTIE

           MOVE "Histogramme des frequences (tri decroissant) :"
               TO LIGNE-SORTIE
           WRITE LIGNE-SORTIE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= K
               MOVE SPACES TO ETOILES-LIGNE
               MOVE OCCURENCE(I) TO NB-ETOILES
               MOVE VALEUR(I) TO TEXTE-VALEUR

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NB-ETOILES
                   MOVE "*" TO ETOILES-LIGNE(J:1)
               END-PERFORM

               STRING
                   "Valeur " DELIMITED BY SIZE
                   TEXTE-VALEUR DELIMITED BY SIZE
                   " : " DELIMITED BY SIZE
                   ETOILES-LIGNE DELIMITED BY SIZE
                   INTO LIGNE-SORTIE
               END-STRING

               WRITE LIGNE-SORTIE
           END-PERFORM

           CLOSE FICHIER-SORTIE

           DISPLAY "Histogramme des frequences :"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= K
               DISPLAY "Valeur ", VALEUR(I), " : " WITH NO ADVANCING
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > OCCURENCE(I)
                   DISPLAY "*" WITH NO ADVANCING
               END-PERFORM
               DISPLAY " "
           END-PERFORM

           STOP RUN.
