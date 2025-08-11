       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOUBLONS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ENTIERS.
           05 ENTIER            PIC S99 OCCURS 6 TIMES.

       01 DOUBLONS-TROUVES      PIC S99 OCCURS 6 TIMES.

       01 I                     PIC 9(1).
       01 J                     PIC 9(1).
       01 L                     PIC 9(1).
       01 K                     PIC 9(1) VALUE 1.

       01 OK-TROUVE             PIC X VALUE 'N'.
       01 DEJA-AJOUTE           PIC X VALUE 'N'.

       01 TEMP-ALPHA            PIC X(4).
       01 TEMP-NUM              PIC S999.
       01 OK-SAISIE             PIC X VALUE 'N'.

       PROCEDURE DIVISION.

           DISPLAY "Saisie de 6 entiers (de -99 à 99) :"

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
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
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE I TO J
               ADD 1 TO J

               MOVE 'N' TO OK-TROUVE

               PERFORM VARYING J FROM J BY 1 UNTIL J > 6
                   IF ENTIER(I) = ENTIER(J)
                       MOVE 'O' TO OK-TROUVE
                   END-IF
               END-PERFORM

               IF OK-TROUVE = 'O'
                   MOVE 'N' TO DEJA-AJOUTE

                   PERFORM VARYING L FROM 1 BY 1 UNTIL L >= K
                       IF ENTIER(I) = DOUBLONS-TROUVES(L)
                           MOVE 'O' TO DEJA-AJOUTE
                       END-IF
                   END-PERFORM

                   IF DEJA-AJOUTE = 'N'
                       MOVE ENTIER(I) TO DOUBLONS-TROUVES(K)
                       ADD 1 TO K
                   END-IF
               END-IF
           END-PERFORM


           IF K = 1
               DISPLAY "Aucun doublon trouvé."
           ELSE
               DISPLAY "Doublons trouvés :"
               PERFORM VARYING I FROM 1 BY 1 UNTIL I >= K
                   DISPLAY "Doublon : " DOUBLONS-TROUVES(I)
               END-PERFORM
           END-IF

           STOP RUN.
