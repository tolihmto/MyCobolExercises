       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTAIRE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INV-FILE ASSIGN TO DYNAMIC SAVE-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-FILE ASSIGN TO "inv_list.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INV-FILE.
       01  INV-REC                PIC X(200).
       FD  LIST-FILE.
       01  LIST-REC               PIC X(200).

       WORKING-STORAGE SECTION.
       77  WS-NB-ART              PIC 9(3) VALUE 0.
       77  WS-I                   PIC 9(3) VALUE 0.
       77  WS-J                   PIC 9(3) VALUE 0.
       77  WS-TOTAL-QTE           PIC S9(9) VALUE 0.
       77  WS-TOTAL-MONT          PIC S9(9)V99 VALUE 0.
       77  WS-TVA                 PIC 9(3) VALUE 20.
       77  WS-TOTAL-TTC           PIC S9(9)V99 VALUE 0.
       77  TMP-PU                 PIC S9(5)V99 VALUE 0.
       77  TMP-QTE                PIC S9(5)    VALUE 0.
       77  OK                     PIC X VALUE 'N'.
       77  REPLY                  PIC X VALUE SPACE.
       77  CHOICE                 PIC 9(4) VALUE 0.
       77  NB-SAVES               PIC 9(4) VALUE 0.
       77  CMD-LS                 PIC X(200).
       77  SAVE-FILENAME          PIC X(64) VALUE SPACES.
       77  SAVE-BASENAME          PIC X(32) VALUE SPACES.
       77  RAND-ID                PIC 9(5) VALUE 0.
       77  SEP                    PIC X VALUE "|".
       77  TWO-SP                 PIC X(2) VALUE "  ".
       77  ARG1                   PIC X(32) VALUE SPACES.
       77  DUP-FLAG               PIC X VALUE 'N'.

       01  TAB-ART OCCURS 100.
           05  T-CODE             PIC X(10).
           05  T-LIB              PIC X(20).
           05  T-PU               PIC S9(5)V99.
           05  T-QTE              PIC S9(5).
           05  T-MONT             PIC S9(9)V99.

       01  ART-TMP.
           05  X-CODE             PIC X(10).
           05  X-LIB              PIC X(20).
           05  X-PU               PIC S9(5)V99.
           05  X-QTE              PIC S9(5).
           05  X-MONT             PIC S9(9)V99.

       01  INP-CODE               PIC X(10).
       01  INP-LIB                PIC X(20).
       01  INP-PU-ALPHA           PIC X(32).
       01  INP-QTE-ALPHA          PIC X(32).

       01  PU-EDIT                PIC Z(5)9.99.
       01  MONT-EDIT              PIC Z(7)9.99.
       01  QTE-EDIT               PIC Z(7)9.
       01  TOT-EDIT               PIC Z(9)9.99.
       01  TTC-EDIT               PIC Z(9)9.99.
       01  TOT-QTE-EDIT           PIC Z(9).
       01  TVA-EDIT               PIC ZZ9.

       01  OUT-CODE               PIC X(10).
       01  OUT-LIB                PIC X(20).

       01  SAVES      OCCURS 200.
           05  SAVE-NAME          PIC X(64).

       01  P1 PIC X(64).
       01  P2 PIC X(64).
       01  P3 PIC X(64).
       01  P4 PIC X(64).

       PROCEDURE DIVISION.
      *--- Lister sauvegardes disponibles --------------------------------
           MOVE "sh -c 'ls inventaire_*.txt 2>/dev/null > inv_list.txt'"
                TO CMD-LS
           CALL "SYSTEM" USING CMD-LS
           MOVE 0 TO NB-SAVES
           OPEN INPUT LIST-FILE
           PERFORM UNTIL 1 = 0
               READ LIST-FILE
                   AT END EXIT PERFORM
                   NOT AT END
                       ADD 1 TO NB-SAVES
                       MOVE LIST-REC TO SAVE-NAME(NB-SAVES)
               END-READ
           END-PERFORM
           CLOSE LIST-FILE

           IF NB-SAVES > 0
               DISPLAY "Voulez-vous recuperer votre sauvegarde (o/n) ? "
                   WITH NO ADVANCING
               ACCEPT REPLY
               IF REPLY = "o" OR REPLY = "O"
                   PERFORM SHOW-SAVES
                   PERFORM CHOOSE-SAVE
                   PERFORM LOAD-SAVE
               END-IF
           END-IF

      *--- Option TVA en argument 1 --------------------------------------
           ACCEPT ARG1 FROM ARGUMENT-VALUE
           IF ARG1 NOT = SPACES
               MOVE FUNCTION NUMVAL(ARG1) TO TMP-PU
               IF TMP-PU >= 0 AND TMP-PU <= 999
                   MOVE TMP-PU TO WS-TVA
               END-IF
           END-IF

      *--- Saisie des articles (fin sur CODE = "FIN", anti-doublon) ------
           PERFORM UNTIL INP-CODE = "FIN"
               DISPLAY "Code (FIN pour terminer) : "
                   WITH NO ADVANCING
               ACCEPT INP-CODE
               INSPECT INP-CODE CONVERTING
               "abcdefghijklmnopqrstuvwxyz" TO
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               IF INP-CODE = "FIN"
                   EXIT PERFORM
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(INP-CODE)) = 0
                   DISPLAY "Code vide, recommencez."
                   CONTINUE
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(INP-CODE)) > 10
                   MOVE INP-CODE(1:10) TO INP-CODE
               END-IF

               PERFORM DUP-CODE
               IF DUP-FLAG = 'O'
                   DISPLAY "Code deja existant, recommencez."
               ELSE
                   DISPLAY "Libelle : " WITH NO ADVANCING
                   ACCEPT INP-LIB
                   MOVE 'N' TO OK
                   PERFORM UNTIL OK = 'O'
                       DISPLAY "Prix unitaire (ex 12.50) : "
                               WITH NO ADVANCING
                       ACCEPT INP-PU-ALPHA
                       MOVE FUNCTION NUMVAL(INP-PU-ALPHA)
                           TO TMP-PU
                       IF TMP-PU >= 0
                           MOVE 'O' TO OK
                       ELSE
                           DISPLAY "PU invalide, recommencez."
                       END-IF
                   END-PERFORM
                   MOVE 'N' TO OK
                   PERFORM UNTIL OK = 'O'
                       DISPLAY "Quantite (entier > 0) : "
                               WITH NO ADVANCING
                       ACCEPT INP-QTE-ALPHA
                       MOVE FUNCTION NUMVAL(INP-QTE-ALPHA)
                           TO TMP-QTE
                       IF TMP-QTE > 0
                           MOVE 'O' TO OK
                       ELSE
                           DISPLAY "Quantite invalide, recommencez."
                       END-IF
                   END-PERFORM
                   ADD 1 TO WS-NB-ART
                   IF WS-NB-ART > 100
                       DISPLAY "Limite 100 atteinte."
                       MOVE "FIN" TO INP-CODE
                   ELSE
                       MOVE INP-CODE
                           TO T-CODE(WS-NB-ART)
                       MOVE INP-LIB
                           TO T-LIB(WS-NB-ART)
                       MOVE TMP-PU
                           TO T-PU(WS-NB-ART)
                       MOVE TMP-QTE
                           TO T-QTE(WS-NB-ART)
                       COMPUTE T-MONT(WS-NB-ART) =
                           T-PU(WS-NB-ART) * T-QTE(WS-NB-ART)
                   END-IF
               END-IF
           END-PERFORM

      *--- Si aucun article, terminer proprement -------------------------
           IF WS-NB-ART = 0
               DISPLAY "Aucun article saisi."
               STOP RUN
           END-IF

      *--- Totaux --------------------------------------------------------
           MOVE 0 TO WS-TOTAL-QTE WS-TOTAL-MONT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NB-ART
               ADD T-QTE(WS-I)   TO WS-TOTAL-QTE
               ADD T-MONT(WS-I)  TO WS-TOTAL-MONT
           END-PERFORM
           COMPUTE WS-TOTAL-TTC =
               (WS-TOTAL-MONT * (100 + WS-TVA)) / 100

      *--- Tri decroissant par montant -----------------------------------
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-NB-ART
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-NB-ART - WS-I
                   IF T-MONT(WS-J) < T-MONT(WS-J + 1)
                       MOVE T-CODE(WS-J)     TO X-CODE
                       MOVE T-LIB(WS-J)      TO X-LIB
                       MOVE T-PU(WS-J)       TO X-PU
                       MOVE T-QTE(WS-J)      TO X-QTE
                       MOVE T-MONT(WS-J)     TO X-MONT

                       MOVE T-CODE(WS-J + 1) TO T-CODE(WS-J)
                       MOVE T-LIB(WS-J + 1)  TO T-LIB(WS-J)
                       MOVE T-PU(WS-J + 1)   TO T-PU(WS-J)
                       MOVE T-QTE(WS-J + 1)  TO T-QTE(WS-J)
                       MOVE T-MONT(WS-J + 1) TO T-MONT(WS-J)

                       MOVE X-CODE           TO T-CODE(WS-J + 1)
                       MOVE X-LIB            TO T-LIB(WS-J + 1)
                       MOVE X-PU             TO T-PU(WS-J + 1)
                       MOVE X-QTE            TO T-QTE(WS-J + 1)
                       MOVE X-MONT           TO T-MONT(WS-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM

      *--- Impression entete (console) -----------------------------------
           MOVE SPACES TO INV-REC
           STRING "CODE        LIBELLE                    PU          "
               "QTE       MONTANT"
               DELIMITED BY SIZE INTO INV-REC
           END-STRING
           DISPLAY INV-REC

      *--- Lignes articles (console) -------------------------------------
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NB-ART
               MOVE T-PU(WS-I)    TO PU-EDIT
               MOVE T-MONT(WS-I)  TO MONT-EDIT
               MOVE T-QTE(WS-I)   TO QTE-EDIT
               MOVE T-CODE(WS-I)  TO OUT-CODE
               MOVE T-LIB(WS-I)   TO OUT-LIB
               MOVE SPACES        TO INV-REC
               STRING OUT-CODE TWO-SP OUT-LIB TWO-SP
                   PU-EDIT TWO-SP QTE-EDIT TWO-SP MONT-EDIT
                   DELIMITED BY SIZE INTO INV-REC
               END-STRING
               DISPLAY INV-REC
           END-PERFORM.

      *--- Ligne de separation et totaux -------------------------------
           DISPLAY "-----------------------------------------------"
                   "---------------------"
           MOVE WS-TOTAL-QTE  TO TOT-QTE-EDIT
           MOVE WS-TOTAL-MONT TO TOT-EDIT
           MOVE WS-TOTAL-TTC  TO TTC-EDIT
           MOVE WS-TVA        TO TVA-EDIT
           DISPLAY "TOTAL QTE:           " TOT-QTE-EDIT
           DISPLAY "TOTAL HT :           " TOT-EDIT
           DISPLAY "TVA " TVA-EDIT "%  TOTAL TTC: " TTC-EDIT

           PERFORM SAVE-CURRENT
           DISPLAY "Inventaire enregistre."

      *--- Fin du programme principal -----------------------------------
           STOP RUN.

      *============================================================
      *  DUP-CODE : met DUP-FLAG = 'O' si INP-CODE deja present
      *============================================================
       DUP-CODE.
           MOVE 'N' TO DUP-FLAG.
           IF WS-NB-ART = 0
               EXIT PARAGRAPH
           END-IF.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NB-ART
               IF FUNCTION TRIM(T-CODE(WS-I)) =
                  FUNCTION TRIM(INP-CODE)
                   MOVE 'O' TO DUP-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           EXIT PARAGRAPH.

      *============================================================
      *  SHOW-SAVES : affiche la liste des sauvegardes detectees
      *============================================================
       SHOW-SAVES.
           IF NB-SAVES = 0
               DISPLAY "Aucune sauvegarde trouvee."
               EXIT PARAGRAPH
           END-IF.
           DISPLAY "Sauvegardes detectees :".
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > NB-SAVES
               DISPLAY WS-I ")  " SAVE-NAME(WS-I)
           END-PERFORM.
           EXIT PARAGRAPH.

      *============================================================
      *  CHOOSE-SAVE : demande le numero et fixe SAVE-FILENAME
      *============================================================
       CHOOSE-SAVE.
           IF NB-SAVES = 0
               EXIT PARAGRAPH
           END-IF.
           MOVE 0 TO CHOICE.
           PERFORM UNTIL CHOICE >= 1 AND CHOICE <= NB-SAVES
               DISPLAY "Quelle sauvegarde ? " WITH NO ADVANCING
               ACCEPT CHOICE
           END-PERFORM.
           MOVE SAVE-NAME(CHOICE) TO SAVE-FILENAME.
           DISPLAY "Chargement du fichier de sauvegarde...".
           EXIT PARAGRAPH.

      *============================================================
      *  LOAD-SAVE : recharge TVA et articles depuis SAVE-FILENAME
      *  Format: 1ere ligne "TVA=nn", puis "CODE|LIB|PU|QTE"
      *============================================================
       LOAD-SAVE.
           OPEN INPUT INV-FILE.
           MOVE 0 TO WS-NB-ART.
           READ INV-FILE
               AT END GO TO L-END-LOAD
               NOT AT END
                   IF INV-REC(1:4) = "TVA="
                       MOVE FUNCTION NUMVAL(INV-REC(5:10))
                           TO WS-TVA
                   END-IF
           END-READ.
           PERFORM UNTIL 1 = 0
               READ INV-FILE
                   AT END EXIT PERFORM
                   NOT AT END
                       PERFORM PARSE-LINE
               END-READ
           END-PERFORM.
       L-END-LOAD.
           CLOSE INV-FILE.
           EXIT PARAGRAPH.

      *------------------------------------------------------------
      *  PARSE-LINE : UNSTRING "CODE|LIB|PU|QTE" vers le tableau
      *------------------------------------------------------------
       PARSE-LINE.
           MOVE SPACES TO INP-CODE INP-LIB
                            INP-PU-ALPHA INP-QTE-ALPHA.
           UNSTRING INV-REC DELIMITED BY ALL "|"
               INTO INP-CODE INP-LIB
                    INP-PU-ALPHA INP-QTE-ALPHA
           END-UNSTRING.
           INSPECT INP-PU-ALPHA REPLACING ALL "," BY ".".
           MOVE FUNCTION NUMVAL(INP-PU-ALPHA)  TO TMP-PU.
           MOVE FUNCTION NUMVAL(INP-QTE-ALPHA) TO TMP-QTE.
           IF FUNCTION LENGTH(FUNCTION TRIM(INP-CODE)) = 0
               EXIT PARAGRAPH
           END-IF.
           ADD 1 TO WS-NB-ART.
           IF WS-NB-ART > 100
               SUBTRACT 1 FROM WS-NB-ART
               EXIT PARAGRAPH
           END-IF.
           MOVE INP-CODE  TO T-CODE(WS-NB-ART).
           MOVE INP-LIB   TO T-LIB(WS-NB-ART).
           MOVE TMP-PU    TO T-PU(WS-NB-ART).
           MOVE TMP-QTE   TO T-QTE(WS-NB-ART).
           COMPUTE T-MONT(WS-NB-ART) =
               T-PU(WS-NB-ART) * T-QTE(WS-NB-ART).
           EXIT PARAGRAPH.

      *============================================================
      *  SAVE-CURRENT : sauvegarde rechargeable inventaire_#####.txt
      *============================================================
       SAVE-CURRENT.
           PERFORM GEN-RAND-ID
           MOVE SPACES TO SAVE-FILENAME
           STRING "inventaire_" RAND-ID ".txt"
               DELIMITED BY SIZE INTO SAVE-FILENAME
           END-STRING

      * Ensure file is closed before opening for output (avoid status 41)
           CLOSE INV-FILE
           OPEN OUTPUT INV-FILE
           MOVE SPACES TO INV-REC
           MOVE WS-TVA TO TVA-EDIT
           STRING "TVA=" TVA-EDIT DELIMITED BY SIZE INTO INV-REC
           END-STRING
           WRITE INV-REC
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NB-ART
               MOVE SPACES TO INV-REC
               MOVE T-PU(WS-I)   TO PU-EDIT
               MOVE T-QTE(WS-I)  TO QTE-EDIT
               STRING T-CODE(WS-I) "|" T-LIB(WS-I) "|"
                      PU-EDIT "|" QTE-EDIT
                   DELIMITED BY SIZE INTO INV-REC
               END-STRING
               WRITE INV-REC
           END-PERFORM
           CLOSE INV-FILE
           DISPLAY "Sauvegarde : " SAVE-FILENAME
           EXIT PARAGRAPH.

      *------------------------------------------------------------
      *  GEN-RAND-ID : fabrique un identifiant 5 chiffres
      *------------------------------------------------------------
       GEN-RAND-ID.
           COMPUTE RAND-ID = FUNCTION RANDOM * 90000 + 10000
           EXIT PARAGRAPH.

