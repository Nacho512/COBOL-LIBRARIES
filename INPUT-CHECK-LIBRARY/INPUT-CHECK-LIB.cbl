      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       FUNCTION-ID. VERIFY-INTEGER.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 LS-WRKING-VAR.
           05 LS-INPUT-NUMBER-LENGTH    PIC 9(09).
           05 LS-COUNTER                PIC 9(02) VALUE 1.
           05 LS-VECTOR.
               10 LS-EACH-CHARCT    OCCURS 40 TIMES PIC X.
      *
       LINKAGE SECTION.
           01 LN-NUMBER                 PIC X ANY LENGTH.
           01 LN-RESULT                 PIC 9.
      *
       PROCEDURE DIVISION
       USING     LN-NUMBER
       RETURNING LN-RESULT.
      *
       MAIN-PAR.
      *
           IF LN-NUMBER = SPACES THEN
      *
               MOVE 0 TO LN-RESULT
               PERFORM EXIT-PAR
      *
           END-IF
      *
           MOVE LENGTH (TRIM (LN-NUMBER) )
           TO LS-INPUT-NUMBER-LENGTH
      *
           IF LS-INPUT-NUMBER-LENGTH > 39 THEN
      *
               MOVE    2 TO LN-RESULT
               PERFORM EXIT-PAR
      *
           END-IF
      *
           MOVE LN-NUMBER TO LS-VECTOR
      *
           IF LS-EACH-CHARCT (1) = "-" OR "+" THEN
      *
               MOVE 2 TO LS-COUNTER
      *
           END-IF
      *
           PERFORM
           VARYING LS-COUNTER FROM LS-COUNTER BY 1
           UNTIL   LS-COUNTER > LENGTH (TRIM (LN-NUMBER) )
      *
               EVALUATE LS-EACH-CHARCT (LS-COUNTER)
      *
               WHEN "0" THROUGH "9"
      *
                   MOVE 1 TO LN-RESULT
                   CONTINUE
      *
               WHEN OTHER
      *
                   MOVE 0 TO LN-RESULT
                   PERFORM   EXIT-PAR
      *
               END-EVALUATE
      *
           END-PERFORM
      *
           PERFORM EXIT-PAR.
      *
       EXIT-PAR.
      *
           EXIT FUNCTION.
      *
       END FUNCTION VERIFY-INTEGER.



       IDENTIFICATION DIVISION.
       FUNCTION-ID. VERIFY-INTEGER-ADV.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 LS-WRKING-VAR.
           05 LS-INPUT-NUMBER-LENGTH    PIC 9(09).
           05 LS-COUNTER             PIC 9(02) VALUE 1.
           05 LS-PERIOD-COUNTER PIC 9(02).
           05 LS-COMMA-COUNTER PIC 9(02).
           05 LS-BOOL-VAR.
               10 LS-PERIODS   PIC 9.
                   88 LS-CONTAINS-PERIODS   VALUE 1.
               10 LS-COMMAS    PIC 9.
                   88 LS-CONTAINS-COMMAS    VALUE 1.
               10 LS-NON-NUMERIC   PIC 9.
                   88 LS-CONTAINS-NON-NUM    VALUE 1.
           05 LS-VECTOR.
               10 LS-EACH-CHARCT   OCCURS 40 TIMES PIC X.
      *
       LINKAGE SECTION.
           01 LN-NUMBER            PIC X ANY LENGTH.
           01 LN-RESULT            PIC 99.
      *
       PROCEDURE DIVISION
       USING     LN-NUMBER
       RETURNING LN-RESULT.
      *
       MAIN-PAR.

           IF LN-NUMBER = SPACES THEN
      *
               MOVE 0 TO LN-RESULT
               PERFORM EXIT-PAR
      *
           END-IF
      *
           MOVE LENGTH (TRIM (LN-NUMBER) )
           TO LS-INPUT-NUMBER-LENGTH
      *
           IF LS-INPUT-NUMBER-LENGTH > 40 THEN
      *
               MOVE    3 TO LN-RESULT
               PERFORM EXIT-PAR
      *
           END-IF
      *
           MOVE LN-NUMBER TO LS-VECTOR
      *
           IF LS-EACH-CHARCT (1) = "-" OR "+" THEN
      *
               MOVE 2 TO LS-COUNTER
      *
           END-IF
      *
           PERFORM
           VARYING LS-COUNTER FROM LS-COUNTER BY 1
           UNTIL   LS-COUNTER > LENGTH (TRIM (LN-NUMBER) )
      *
               EVALUATE LS-EACH-CHARCT (LS-COUNTER)
      *
               WHEN "0" THROUGH "9"
      *
                   CONTINUE
      *
               WHEN "."
      *
                   SET LS-CONTAINS-PERIODS TO TRUE
                   ADD 1 TO LS-PERIOD-COUNTER
      *
               WHEN ","
      *
                   SET LS-CONTAINS-COMMAS TO TRUE
                   ADD 1 TO LS-COMMA-COUNTER
      *
               WHEN OTHER
      *
                   SET LS-CONTAINS-NON-NUM TO TRUE
                   CONTINUE
      *
               END-EVALUATE
      *
           END-PERFORM
      *
           PERFORM CHECKING-PAR.
      *
       CHECKING-PAR.
      *
           IF LS-CONTAINS-NON-NUM THEN
      *
               MOVE 2 TO LN-RESULT
      *
           ELSE IF LS-CONTAINS-PERIODS AND NOT LS-CONTAINS-COMMAS
           THEN
      *
               IF LS-PERIOD-COUNTER = 1 THEN
      *
                   MOVE 4 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 5 TO LN-RESULT
      *
               END-IF
      *
           ELSE IF NOT LS-CONTAINS-PERIODS AND LS-CONTAINS-COMMAS
           THEN
      *
               IF LS-COMMA-COUNTER = 1 THEN
      *
                   MOVE 6 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 7 TO LN-RESULT
      *
               END-IF
      *
           ELSE IF LS-CONTAINS-PERIODS AND LS-CONTAINS-COMMAS
           THEN
      *
               IF  LS-PERIOD-COUNTER = 1
               AND LS-COMMA-COUNTER = 1
               THEN
      *
                   MOVE 8 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 9 TO LN-RESULT
      *
               END-IF
      *
           ELSE
      *
               MOVE 1 TO LN-RESULT
      *
           END-IF
      *
           PERFORM EXIT-PAR.
      *
       EXIT-PAR.
      *
           EXIT FUNCTION.
      *
       END FUNCTION VERIFY-INTEGER-ADV.



       IDENTIFICATION DIVISION.
       FUNCTION-ID. VERIFY-NUM-DEC-POINT.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION VERIFY-INTEGER-ADV
           FUNCTION ALL INTRINSIC.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 LS-RESULT PIC 9.
      *
       LINKAGE SECTION.
       01 LN-NUMBER PIC X ANY LENGTH.
       01 LN-RESULT PIC 99.
      *
       PROCEDURE DIVISION
       USING     LN-NUMBER
       RETURNING LN-RESULT.
      *
       MAIN-PAR.
      *
       MOVE VERIFY-INTEGER-ADV (LN-NUMBER) TO LS-RESULT
      *
       EVALUATE LS-RESULT
      *
           WHEN 0
      *
               MOVE 0 TO LN-RESULT
      *
           WHEN 1
      *
               MOVE 1 TO LN-RESULT
      *
           WHEN 2
      *
               MOVE 0 TO LN-RESULT
      *
           WHEN 3
      *
               MOVE 2 TO LN-RESULT
      *
           WHEN 4
      *
               MOVE 1 TO LN-RESULT
      *
           WHEN 5 THRU 9
      *
               MOVE 0 TO LN-RESULT
      *
           WHEN OTHER
      *
               MOVE 11 TO LN-RESULT
      *
       END-EVALUATE.
      *
       END FUNCTION VERIFY-NUM-DEC-POINT.



       IDENTIFICATION DIVISION.
       FUNCTION-ID. VERIFY-NUM-DEC-POINT-ADV.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION VERIFY-INTEGER-ADV
           FUNCTION ALL INTRINSIC.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 LS-RESULT PIC 9.
      *
       LINKAGE SECTION.
       01 LN-NUMBER PIC X ANY LENGTH.
       01 LN-RESULT PIC 99.
      *
       PROCEDURE DIVISION
       USING     LN-NUMBER
       RETURNING LN-RESULT.
      *
       MAIN-PAR.
      *
       MOVE VERIFY-INTEGER-ADV (LN-NUMBER) TO LS-RESULT
      *
       EVALUATE LS-RESULT
      *
           WHEN 0
      *
               MOVE 0 TO LN-RESULT
      *
           WHEN 1
      *
               MOVE 1 TO LN-RESULT
      *
           WHEN 2
      *
               MOVE 2 TO LN-RESULT
      *
           WHEN 3
      *
               MOVE 3 TO LN-RESULT
      *
           WHEN 4
      *
               MOVE 1 TO LN-RESULT
      *
           WHEN 5
      *
               MOVE 5 TO LN-RESULT
      *
           WHEN 6
      *
               MOVE 6 TO LN-RESULT
      *
           WHEN 7
      *
               MOVE 7 TO LN-RESULT
      *
           WHEN 8
      *
               MOVE 8 TO LN-RESULT
      *
           WHEN 9
      *
               MOVE 9 TO LN-RESULT
      *
           WHEN OTHER
      *
               MOVE 11 TO LN-RESULT
      *
       END-EVALUATE.
      *
       END FUNCTION VERIFY-NUM-DEC-POINT-ADV.



      * B"H.
