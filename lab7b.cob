      
      *Tyler Zysberg
      *checks a sudoku puzzle

       IDENTIFICATION DIVISION.
       PROGRAM-ID. lab7a.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT Infile ASSIGN TO "lab7b-in.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Infile.
               01 INPUT-ROW PIC X(9).

       WORKING-STORAGE SECTION.
               01 Final-Table.
                   05 Final-Rows OCCURS 9 TIMES.
                       10 Final-ColsS OCCURS 9 TIMES.
                           15 cell PIC 9.

               01 WORK-VARS.
                   05 ColNum PIC 99.
                   05 RowNum PIC 99.
                   05 Scolumn PIC 99.
                   05 SRow PIC 99.
                   05 Counter PIC 99.
                   05 Chck.
                       10 CHECK-DIGIT PIC X OCCURS 9 TIMES.
                01 stat PIC X(30).
                   88 incompletes VALUE "n incomplete solution".
                   88 Valid1 VALUE " valid solution".
                   88 invalids VALUE "invalid solution".
                   88 Unkwn VALUE "X".
                   
                01 EndOfFile PIC X.
                   88 EOF VALUE "Y".


       PROCEDURE DIVISION.
       Main.
            OPEN INPUT Infile
            PERFORM Create THRU Displays UNTIL EOF
            CLOSE Infile
            STOP RUN.

       Create.
            MOVE SPACES TO Final-Table
            PERFORM VARYING RowNum FROM 1 BY 1
                   UNTIL RowNum > 9
               READ Infile
               AT END
                   SET EOF TO TRUE
               NOT AT END
                   MOVE INPUT-ROW TO Final-Rows(RowNum)
            END-PERFORM.

       Checker.
            SET Unkwn TO TRUE.
            INSPECT Final-Table TALLYING Counter FOR ALL " "
            IF Counter = 0
                PERFORM CheckC
                PERFORM CheckR
                PERFORM CheckB
                IF Unkwn
                SET Valid1 TO TRUE
                END-IF
            ELSE
                SET incompletes TO TRUE
            END-IF.

       Displays.
            DISPLAY "Puzzle:"
            DISPLAY SPACE
            PERFORM VARYING RowNum FROM 1 BY 1 UNTIL RowNum > 9
               PERFORM VARYING ColNum FROM 1 BY 1 UNTIL ColNum > 9
                   IF (ColNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8)
                          AND (RowNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8)
                      DISPLAY cell(RowNum, ColNum) " " 
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 3 OR 6)
                           AND (RowNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8)
                   DISPLAY cell (RowNum, ColNum) " | " 
				   WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8)
                           AND (RowNum = 3 OR 6)
                      DISPLAY cell (RowNum, ColNum) " "
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 3 OR 6) AND (RowNum = 3 OR 6)
                      DISPLAY cell (RowNum, ColNum) " | " 
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 9) AND (RowNum = 3 OR 6)
                      DISPLAY cell (RowNum, ColNum)
                      DISPLAY "---------------------"
                           WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 9) AND 
				   (RowNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8)
                      DISPLAY cell (RowNum, ColNum) 
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 1 OR 2 OR 4 OR 5 OR 7 OR 8) 
				   AND (RowNum = 9)
                      DISPLAY cell (RowNum, ColNum) " " 
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 3 OR 6) AND (RowNum = 9)
                      DISPLAY cell (RowNum, ColNum) " | " 
					  WITH NO ADVANCING
                   END-IF
                   IF (ColNum = 9) AND (RowNum = 9)
                      DISPLAY cell (RowNum, ColNum) WITH NO ADVANCING
                   END-IF
               END-PERFORM
               DISPLAY SPACE
            END-PERFORM

            DISPLAY SPACE
            DISPLAY "This is a" stat
            DISPLAY SPACE.
            
        CheckB.
            PERFORM VARYING RowNum FROM 1 BY 3 UNTIL RowNum > 9
               PERFORM CheckBl
                   VARYING ColNum FROM 1 BY 3 UNTIL ColNum > 9
            END-PERFORM.


       CheckR.
            PERFORM VARYING ColNum FROM 1 BY 1 UNTIL ColNum > 9
               MOVE SPACES TO Chck
            PERFORM VARYING RowNum FROM 1 BY 1 UNTIL RowNum > 9
                   MOVE "Y" TO CHECK-DIGIT(cell(RowNum, ColNum))
            END-PERFORM
               IF Chck NOT = "YYYYYYYYY"
               SET invalids TO TRUE
                   EXIT PARAGRAPH
               END-IF
            END-PERFORM.
            
         CheckBl.
            MOVE SPACES TO Chck
            PERFORM VARYING SRow FROM RowNum BY 1
             UNTIL SRow > (RowNum + 2)
            PERFORM VARYING Scolumn FROM ColNum BY 1
             UNTIL Scolumn > (ColNum + 2)
               MOVE "Y" TO CHECK-DIGIT(cell(SRow, Scolumn))
            END-PERFORM
            END-PERFORM
            IF Chck NOT = "YYYYYYYYY"
            SET invalids TO TRUE
               EXIT PARAGRAPH
            END-IF.
            
          CheckC.
            PERFORM VARYING RowNum FROM 1 BY 1 UNTIL RowNum > 9
               MOVE SPACES TO Chck
            PERFORM VARYING ColNum FROM 1 BY 1 UNTIL ColNum > 9
                   MOVE "Y" TO CHECK-DIGIT(cell(RowNum, ColNum))
            END-PERFORM
               IF Chck NOT = "YYYYYYYYY"
               SET invalids TO TRUE
                   EXIT PARAGRAPH
               END-IF
            END-PERFORM.


