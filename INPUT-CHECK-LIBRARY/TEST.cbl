      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBAS2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION VERIFY-INTEGER
           FUNCTION VERIFY-INTEGER-ADV
           FUNCTION VERIFY-NUM-DEC-POINT
           FUNCTION VERIFY-NUM-DEC-POINT-ADV
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER  PIC X(58).
       01 WS-NUMBER2 PIC X(58).
       01 WS-NUMBER3 PIC X(58).
       01 WS-RESULT  PIC 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "INPUT INTEGER NUMBER. " WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER
           DISPLAY SPACES

           MOVE VERIFY-INTEGER (WS-NUMBER) TO WS-RESULT

           IF WS-RESULT = 1 THEN

               DISPLAY "YOU HAVE INPUT AN INTEGER NUMBER!"

           END-IF

           IF WS-RESULT = 2 THEN

               DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"

           END-IF

           IF VERIFY-INTEGER (WS-NUMBER) = 0

               DISPLAY "YOU HAVE NOT INPUT AN INTEGER NUMBER."

           END-IF

           DISPLAY SPACES

           DISPLAY "INPUT INTEGER NUMBER. " WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER2
           DISPLAY SPACES

           MOVE VERIFY-INTEGER-ADV (WS-NUMBER2) TO WS-RESULT

           EVALUATE WS-RESULT

               WHEN 0

                   DISPLAY "ERROR! YOU HAVE INPUT A BLANK AMOUNT!"

               WHEN 1

                   DISPLAY "YOU HAVE INPUT AN INTEGER NUMBER!"

               WHEN 2

                   DISPLAY "YOUR INPUT CONBTAINS NON-NUMERIC "
                   "CHARACTERS!"

               WHEN 3

                   DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"

               WHEN 4

                   DISPLAY "YOUR INPUT HAS ONE DECIMAL POINT!"

               WHEN 5

                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL"
                   " POINT!"

               WHEN 6

                   DISPLAY "YOUR INPUT HAS ONE DECIMAL COMMA!"

               WHEN 7

                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL COMMA!"

               WHEN 8

                   DISPLAY "YOUR INPUT HAS BOTH ONE DECIMAL PERIOD AND"
                   " ONE DECIMAL COMMA!"

               WHEN 9

                   DISPLAY "YOUR INPUT HAS MULTIPLE DECIMAL PERIODS AND"
                   " MULTIPLE DECIMAL COMMAS!"

               WHEN OTHER

                   DISPLAY "I REALLY HAVE NO IDEA OF WHAT'S GOING ON!"

           END-EVALUATE

           DISPLAY SPACES

           DISPLAY "LET'S PLAY SOME REALLY NICE VIDEO GAMES!"

           DISPLAY SPACES

           DISPLAY "INPUT DECIMAL NUMBER WITH POINT. "
           WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER3
           DISPLAY SPACES

           MOVE VERIFY-NUM-DEC-POINT (WS-NUMBER3) TO WS-RESULT

           IF WS-RESULT = 1 THEN

               DISPLAY "YOU HAVE INPUT A DECIMAL NUMBER!"

           END-IF

           IF WS-RESULT = 2 THEN

               DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"

           END-IF

           IF VERIFY-NUM-DEC-POINT (WS-NUMBER3) = 0

               DISPLAY "YOU HAVE NOT INPUT A DECIMAL NUMBER."

           END-IF

           DISPLAY SPACES

           DISPLAY "INPUT DECIMAL NUMBER WITH POINT. "
           WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER3
           DISPLAY SPACES

           MOVE VERIFY-NUM-DEC-POINT-ADV (WS-NUMBER3) TO WS-RESULT

            EVALUATE WS-RESULT

               WHEN 0

                   DISPLAY "ERROR! YOU HAVE INPUT A BLANK AMOUNT!"

               WHEN 1

                   DISPLAY "YOU HAVE INPUT A DECIMAL NUMBER!"

               WHEN 2

                   DISPLAY "YOUR INPUT CONBTAINS NON-NUMERIC "
                   "CHARACTERS!"

               WHEN 3

                   DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"

               WHEN 4

                   DISPLAY "YOU HAVE INPUT A DECIMAL NUMBER!"

               WHEN 5

                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL"
                   " POINT!"

               WHEN 6

                   DISPLAY "YOUR INPUT HAS ONE DECIMAL COMMA!"

               WHEN 7

                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL COMMA!"

               WHEN 8

                   DISPLAY "YOUR INPUT HAS BOTH ONE DECIMAL PERIOD AND"
                   " ONE DECIMAL COMMA!"

               WHEN 9

                   DISPLAY "YOUR INPUT HAS MULTIPLE DECIMAL PERIODS AND"
                   " MULTIPLE DECIMAL COMMAS!"

               WHEN 11

                   DISPLAY "I REALLY HAVE NO IDEA OF WHAT'S GOING ON!"

               WHEN OTHER

                   DISPLAY "I REALLY HAVE NO IDEA OF WHAT'S GOING ON!"

           END-EVALUATE

           DISPLAY SPACES



           ACCEPT OMITTED

           STOP RUN RETURNING 2.

       END PROGRAM PRUEBAS2.
