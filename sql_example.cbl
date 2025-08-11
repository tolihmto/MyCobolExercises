       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQL-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       77  WS-NAME      PIC X(20).
       77  WS-ID        PIC 9(4).
       EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION.
           EXEC SQL
               CONNECT TO 'test.db'
           END-EXEC

           EXEC SQL
               CREATE TABLE IF NOT EXISTS mytable (id INTEGER, name TEXT)
           END-EXEC

           MOVE 1 TO WS-ID
           MOVE "Alice" TO WS-NAME
           EXEC SQL
               INSERT INTO mytable (id, name) VALUES (:WS-ID, :WS-NAME)
           END-EXEC

           EXEC SQL
               SELECT name INTO :WS-NAME FROM mytable WHERE id = 1
           END-EXEC

           DISPLAY "Nom trouvé : " WS-NAME

           EXEC SQL
               DISCONNECT
           END-EXEC

           STOP RUN.
