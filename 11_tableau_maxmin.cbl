       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAXMINENTIERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ENTIERS.
           05 ENTIER         PIC S99 OCCURS 5 TIMES.

       01 I                 PIC 9(1).
       01 TEMP-ALPHA        PIC X(4).
       01 TEMP-NUM          PIC S999.
       01 OK-SAISIE         PIC X VALUE 'N'.

       01 MAXENTIER         PIC S99 VALUE 0.
       01 POSMAX            PIC 9(1) VALUE 1.

       01 MINENTIER         PIC S99 VALUE 0.
       01 POSMIN            PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.

           DISPLAY "Saisie de 5 entiers (de -99 à 99) :"

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE 'N' TO OK-SAISIE
               PERFORM UNTIL OK-SAISIE = 'O'
                   DISPLAY "Entier " I " :"
                   ACCEPT TEMP-ALPHA
                   MOVE FUNCTION NUMVAL(TEMP-ALPHA)
                       TO TEMP-NUM
                   IF TEMP-NUM >= -99 AND TEMP-NUM <= 99
                       MOVE TEMP-NUM TO ENTIER(I)
                       MOVE 'O' TO OK-SAISIE
                   ELSE
                       DISPLAY "Valeur invalide. Entrez un entier"
                               " entre -99 et 99."
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE ENTIER(1) TO MAXENTIER
           MOVE ENTIER(1) TO MINENTIER
           MOVE 1 TO POSMAX
           MOVE 1 TO POSMIN

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 5
               IF ENTIER(I) > MAXENTIER
                   MOVE ENTIER(I) TO MAXENTIER
                   MOVE I TO POSMAX
               END-IF
               IF ENTIER(I) < MINENTIER
                   MOVE ENTIER(I) TO MINENTIER
                   MOVE I TO POSMIN
               END-IF
           END-PERFORM

           DISPLAY "Récapitulatif des entiers saisis :"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY "Entier " I " : " ENTIER(I)
           END-PERFORM

           DISPLAY "Plus grand entier : " MAXENTIER
               " (position : " POSMAX ")"
           DISPLAY "Plus petit entier : " MINENTIER
               " (position : " POSMIN ")"

           STOP RUN.
