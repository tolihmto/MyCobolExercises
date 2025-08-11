       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARE-FICHIERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-A ASSIGN TO "A.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-B ASSIGN TO "B.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FICHIER-A.
       01 LIGNE-A.
           05 CLE-A      PIC X(10).
           05 DONNEES-A  PIC X(70).

       FD FICHIER-B.
       01 LIGNE-B.
           05 CLE-B      PIC X(10).
           05 DONNEES-B  PIC X(70).

       WORKING-STORAGE SECTION.
       77 EOF-A        PIC X VALUE "N".
       77 EOF-B        PIC X VALUE "N".
       77 COMPARE-RESULT PIC S9(1) COMP.

       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT FICHIER-A FICHIER-B

           PERFORM LIRE-A
           PERFORM LIRE-B

           PERFORM UNTIL EOF-A = "O" AND EOF-B = "O"
               IF EOF-A = "N" AND (EOF-B = "O" OR CLE-A < CLE-B)
                   PERFORM TRAITEMENT-A
                   PERFORM LIRE-A
               ELSE IF EOF-B = "N" AND (EOF-A = "O" OR CLE-B < CLE-A)
                   PERFORM TRAITEMENT-B
                   PERFORM LIRE-B
               ELSE IF CLE-A = CLE-B
                   PERFORM TRAITEMENT-AB
                   PERFORM LIRE-A
                   PERFORM LIRE-B
               END-IF
           END-PERFORM

           CLOSE FICHIER-A FICHIER-B
           STOP RUN.

       LIRE-A.
           READ FICHIER-A
               AT END MOVE "O" TO EOF-A
           END-READ.

       LIRE-B.
           READ FICHIER-B
               AT END MOVE "O" TO EOF-B
           END-READ.

       TRAITEMENT-A.
           DISPLAY ">>> Uniquement dans A : " CLE-A " - " DONNEES-A.

       TRAITEMENT-B.
           DISPLAY ">>> Uniquement dans B : " CLE-B " - " DONNEES-B.

       TRAITEMENT-AB.
           DISPLAY ">>> Présent dans A et B :"
           DISPLAY "    A : " CLE-A " - " DONNEES-A
           DISPLAY "    B : " CLE-B " - " DONNEES-B.
