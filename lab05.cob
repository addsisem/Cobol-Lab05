       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB5.
       AUTHOR. Addyson Sisemore
      * LAB EXERCISE 5.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DA-S-INPUT'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(80).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(63).
       WORKING-STORAGE SECTION.
       01 TOTAL         PIC 9(5)V99     VALUE 0.
       01 REC-CT        PIC 99.
       01 SUB           PIC 999999.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 INPUT-DATA.
         03 I-NAME       PIC X(20).
         03 I-DEGREE     PIC X(4).
         03 I-YEAR       PIC X(4).
         03 I-LOAN       PIC 9(5)V99.
         03 I-PAID OCCURS 4 TIMES PIC 9(4)V99.
         03 FILLER       PIC X(21).
      **************************************************************
      * LAYOUT FOR THE 1ST DATA BLOCK OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 L-NAME1     PIC X(20).
         03 L-DEGREE1   PIC X(4).
         03 FILLER      PIC X(4)        VALUE SPACES.
         03 L-YEAR1     PIC X(4).
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-LOAN1     PIC 99999.99.

       01 PRNT-DATA2.
         03 FILLER      PIC X(53)       VALUE SPACES.
         03 L-PAID1     PIC 9999.99.

       01 PRNT-DATA3.
         03 FILLER      PIC X(40)       VALUE SPACES.
         03 FILLER      PIC X(10)       VALUE 'TOTAL PAID'.
         03 FILLER      PIC X(2)        VALUE SPACES.
         03 L-TOTAL1    PIC $$$,$$9.99.

       01 PRNT-BALANCE1.
         03 FILLER      PIC X(7)        VALUE 'BALANCE'.
         03 FILLER      PIC X(12)        VALUE SPACES.
         03 L-BALANCE   PIC $$$,$$9.99-.

       01 PRNT-BALANCE2.
         03 FILLER      PIC X(16)       VALUE 'OVERPAID BALANCE'.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 N-BALANCE   PIC $$$,$$9.99-.
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 FILLER      PIC X(6)        VALUE SPACES.
         03 FILLER      PIC X(20)       VALUE 'NAME'.
         03 FILLER      PIC X(8)        VALUE 'DEGREE'.
         03 FILLER      PIC X(7)        VALUE 'YEAR'.
         03 FILLER      PIC X(11)       VALUE 'LOAN'.
         03 FILLER      PIC X(10)       VALUE 'PAID'.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
             OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.

       1400-PRINT-HEAD.
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING PAGE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.

       1500-LOOP.
           PERFORM 1700-CALC-TOTAL.
           PERFORM 1800-CALC-BALANCE.
           PERFORM 1600-PRINT-DATA.
           PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-DATA.
           MOVE I-NAME          TO L-NAME1.
           MOVE I-DEGREE        TO L-DEGREE1.
           MOVE I-YEAR          TO L-YEAR1.
           MOVE I-LOAN          TO L-LOAN1.
           MOVE TOTAL           TO L-TOTAL1.
           IF REC-CT IS EQUAL TO 8 THEN
      /
             WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING PAGE
               SUBTRACT 8 FROM REC-CT
             ELSE
               WRITE PRNT-REC FROM PRNT-DATA1
                 AFTER ADVANCING 1 LINE
             END-IF.
           PERFORM 1650-PRINT-PAID
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 4.
             WRITE PRNT-REC FROM PRNT-DATA3
               AFTER ADVANCING 1 LINE.
           IF TOTAL <= I-LOAN
             WRITE PRNT-REC FROM PRNT-BALANCE1
               BEFORE ADVANCING 1 LINE
           ELSE
             MOVE L-BALANCE TO N-BALANCE
               WRITE PRNT-REC FROM PRNT-BALANCE2
                 BEFORE ADVANCING 1 LINE
           END-IF.
           ADD 1 TO REC-CT.

       1650-PRINT-PAID.
           MOVE I-PAID (SUB) TO L-PAID1.
             WRITE PRNT-REC FROM PRNT-DATA2
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * CALCULATE THE TOTAL AMOUNT PAID
      **************************************************************
       1700-CALC-TOTAL.
           MOVE 0 TO TOTAL.
           PERFORM 1750-ADD-PAID
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 4.

       1750-ADD-PAID.
           ADD I-PAID (SUB) TO TOTAL.
      **************************************************************
      * CALCULATE LEFTOVER BALANCE
      **************************************************************
       1800-CALC-BALANCE.
           SUBTRACT TOTAL FROM I-LOAN GIVING
                L-BALANCE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
             AT END MOVE 1 TO EOF-I.
