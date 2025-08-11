       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALBUM-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ALBUM-RECORD.
           05  ALBUM-TITLE         PIC X(30).
           05  ALBUM-GENRE         PIC X(10).
           05  ALBUM-ARTIST.
               10  ARTIST-FIRST-NAME    PIC X(20).
               10  ARTIST-LAST-NAME     PIC X(20).
               10  ARTIST-BAND-NAME     PIC X(20).
           05  ALBUM-ID             PIC X(10).
           05  ALBUM-YEAR           PIC 9(4).

       01  MUSIC-VIDEO-RECORD.
           05  VIDEO-ARTIST.
               10  VIDEO-ARTIST-FIRST-NAME  PIC X(20).
               10  VIDEO-ARTIST-LAST-NAME   PIC X(20).
               10  VIDEO-BAND-NAME          PIC X(20).
           05  VIDEO-FIRST-BROADCAST.
               10  VIDEO-MONTH         PIC 99.
               10  VIDEO-DAY           PIC 99.
               10  VIDEO-YEAR          PIC 999.


       PROCEDURE DIVISION. 
           MOVE "The Wall" TO ALBUM-TITLE
           MOVE "Rock" TO ALBUM-GENRE
           MOVE "Roger" TO ARTIST-FIRST-NAME
           MOVE "Waters" TO ARTIST-LAST-NAME
           MOVE "Pink Floyd" TO ARTIST-BAND-NAME
           MOVE "PF-123" TO ALBUM-ID
           MOVE 1979 TO ALBUM-YEAR

           DISPLAY "Album Title: " ALBUM-TITLE
           DISPLAY "Artist: " ARTIST-FIRST-NAME " " ARTIST-LAST-NAME
           DISPLAY "Band: " ARTIST-BAND-NAME
           DISPLAY "Genre: " ALBUM-GENRE
           DISPLAY "Year: " ALBUM-YEAR

           STOP RUN.
