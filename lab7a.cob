       
      * Tyler Zysberg 
      * Checks a file for errors and outputs the number of errors
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. lab7a.

       ENVIRONMENT DIVISION.
              INPUT-OUTPUT SECTION.
                  FILE-CONTROL.
                  SELECT MyFile ASSIGN TO 'lab7a-in.dat'
                       ORGANIZATION IS LINE SEQUENTIAL.
                       
                  SELECT TaxFile ASSIGN TO 
                      'lab7a-schooltax.dat'
                       ORGANIZATION IS LINE SEQUENTIAL.   
                       
                  SELECT OutputFile ASSIGN TO 'lab7a-out.dat'
                       ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD MyFile.
           01 INPUT-FILE-RECORD.
                  05 RegionNum PIC X(2).
                  05 RegionName PIC X(15).
                  05 DepNum PIC X(5).
                  05 DepName PIC X(30).
                  05 EmpNum PIC X(5).
                  05 EmpLName PIC X(20).
                  05 EmpFName PIC X(15).
                  05 EmpGender PIC X.
                  05 EmpAddress PIC X(20).
                  05 CityState PIC X(20).
                  05 Title PIC X(20).
                  05 DOB PIC 9(8).
                  05 DOHYear PIC 9(4).
                  05 DOHMonth PIC 9(2).
                  05 DOHDay PIC 9(2).
                  05 MaritalStatus PIC X.
                     88 Divorced VALUE "D".
                     88 Married VALUE "M".
                     88 Seperated VALUE "P".
                     88 Single VALUE "S".
                     88 Widowed VALUE "W".
                  05 NumOfDep PIC 9(2).
                  05 SchoolDistrict PIC X(3).
                  05 MedCov PIC X.
                  05 DentCov PIC X.
                  05 VisCov PIC X.
                  05 Withholding PIC V9(3).
                  05 PayCode PIC X.
                     88 SalComm VALUE "C".
                     88 Hourly VALUE "H".
                     88 Sal VALUE "S".
                  05 Pay PIC 9(7)V99.
                  05 ExpHoursPerWeek PIC 9(2)V99.
                  05 CommRate PIC V9(3).
                  05 ActualSales PIC 9(7)V99.
                  
                   
           FD TaxFile.
           01 DisTaxCodes.
              05 DisCode PIC X(3).
              05 TaxRate PIC 9V999.


           FD OutputFile.
           01 OUTPUT-FILE-RECORD PIC X(142).

           WORKING-STORAGE SECTION.
           01 EndOfFileIndicator PIC X.
               88 EOF VALUE "Y".
               
           78 TableLimit value is 500.
           01 DistrictTable.
              05 DisEntry Occurs TableLimit Times Indexed by Indx.
                 10 DisCodeVal PIC X(3).
                 10 TaxRateVal PIC 9v999.
                 
           01 TableSize PIC 999 value 0.
           01 EOFDIS PIC X VALUE "n".
           01 taxAmt PIC 9(7)v99.
           
           01 EXPECTED-MONTHLY-SALES   PIC 9(7)V99 VALUE 45000.
           01  Report-Fields.
               05 PageNumber   Pic 99 Value 0.
               05 LinesPerPage Pic 99 Value 35.
               05 LineNumber   Pic 99 Value 0.

           01 DETAIL-LINE.
                  05 EmpNum PIC ZZZZ9.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 EmpLName PIC X(20).
                  05 FILLER PIC X VALUE SPACES.
                  05 EmpFName PIC X.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 MaritalStatus PIC X.
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 NumOfDep PIC Z9.
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 DTL-INS-M PIC X.
                  05 DTL-INS-D PIC X.
                  05 DTL-INS-V PIC X.
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 DTL-EXPECT-PAY PIC $$,$$$,$$9.99.
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 DTL-COMMISSION PIC $$,$$$,$$9.99.
                  05 DTL-COMM-SPACES
                       REDEFINES DTL-COMMISSION PIC X(13).
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 DTL-401K PIC $$$,$$9.99.
                  05 DTL-401K-SPACES
                       REDEFINES DTL-401K PIC X(10).
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 DTL-FED PIC $$$,$$9.99.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 DTL-STATE PIC $$$,$$9.99.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 DTL-CALC-INSURANCE PIC $$$,$$9.99.
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 DTL-TAX PIC 9(7)v99.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 DTL-NETPAY PIC $$,$$$,$$9.99.

           01 CURRENT-DATE.
                  05 YYYY PIC 9999.
                  05 MM PIC 99.
                  05 DD PIC 99.

           01 CURRENT-TIME.
                  05 HH PIC 99.
                  05 MM PIC 99.
                  05 SS PIC 99.
                  05 CC PIC 99.

           01 HEADER-1.
                  05 MM PIC Z9.
                  05 FILLER PIC X VALUE "/".
                  05 DD PIC Z9.
                  05 FILLER PIC X VALUE "/".
                  05 YYYY PIC 99.
                  05 FILLER PIC X(49) VALUE SPACES.
                  05 FILLER PIC X(27)
                       VALUE "Stomper & Wombat's Emporium".
                  05 FILLER PIC X(47) VALUE SPACES.
                  05 FILLER PIC X(6) VALUE "Page: ".
                  05 PAGE-COUNT PIC ZZ9.

           01 HEADER-2.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 HH PIC Z9.
                  05 FILLER PIC X VALUE ":".
                  05 MM PIC 99.
                  05 FILLER PIC X VALUE SPACES.
                  05 AM-PM PIC X(2).
                  05 FILLER PIC X(39) VALUE SPACES.
                05 FILLER PIC X(50) VALUE "Monthly Payroll Register" &
                    " - Salary and Sales ".
      
           01 HEADER-3.
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 FILLER PIC X(12) VALUE "Department: ".
                  05 DepNum PIC ZZZZ9.

           01 HEADER-4.
                  05 FILLER PIC X(14) VALUE SPACES.
                  05 DepName PIC X(30).

           01 COLUMN-HEADER.
                  05 FILLER PIC X(5) VALUE "Emp #".
                  05 FILLER PIC X(10) VALUE SPACES.
                  05 FILLER PIC X(8) VALUE "Employee".
                  05 FILLER PIC X(8) VALUE SPACES.
                  05 FILLER PIC X(1) VALUE "M".
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 FILLER PIC X(4) VALUE "Deps".
                  05 FILLER PIC X(2) VALUE SPACES.
                  05 FILLER PIC X(3) VALUE "Ins".
                  05 FILLER PIC X(5) VALUE SPACES.
                  05 FILLER PIC X(9) VALUE "Gross Pay".
                  05 FILLER PIC X(6) VALUE SPACES.
                  05 FILLER PIC X(10) VALUE "Commission".
                  05 FILLER PIC X(7) VALUE SPACES.
                  05 FILLER PIC X(4) VALUE "401k".
                  05 FILLER PIC X(8) VALUE SPACES.
                  05 FILLER PIC X(3) VALUE "Fed".
                  05 FILLER PIC X(8) VALUE SPACES.
                  05 FILLER PIC X(5) VALUE "State".
                  05 FILLER PIC X(5) VALUE SPACES.
                  05 FILLER PIC X(9) VALUE "Insurance".
                  05 FILLER PIC X(7) VALUE SPACES.
                  05 FILLER PIC X(10) VALUE "School Tax".
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 FILLER PIC X(7) VALUE "Net Pay".

           01 BLANK-LINE.
                  05 FILLER PIC X VALUE SPACES.

           01 Calc-Value.
                  05 CalcMonthlyPay PIC 9(7)V99.
                  05 CalcCommission PIC 9(7)V99.
                  05 Calc401k PIC 9(5)V99.
                  05 CalcFed PIC 9(5)V99.
                  05 CalcState PIC 9(5)V99.
                  05 CalcInsurance PIC 9(5)V99.
                  05 CalcNetPay PIC 9(7)V99.
                  05 CalcTotalDept PIC 9(9)V99.
                  05 AmountAfterFed PIC 9(5)V99.
                  05 CalcTotalComp PIC 9(7)V99.
                  05 CalcTaxAmt PIC 9(7)V99.
                  05 CURRENT-FILE-LINE PIC 999 VALUE 1.
                  05 NewPage PIC 99.

           01 DETERMINE-DEPT.
                  05 CURRENT-DEPT PIC X(5).
                  05 PREV-DEPT PIC X(5) VALUE "AAAAA".

           01 DISPLAY-DEPT-FOOTER.
                  05 FILLER PIC X(96) VALUE SPACES.
                  05 FILLER PIC X(4) VALUE "Dept".
                  05 FILLER PIC X VALUE SPACES.
                  05 DepNum PIC ZZZZ9.
                  05 FILLER PIC X VALUE SPACES.
                  05 FILLER PIC X(14) VALUE "Total Payroll:".
                  05 FILLER PIC X(3) VALUE SPACES.
                  05 DISPLAY-TOT-DEPT PIC $$$$,$$$,$$9.99.

           01 DISPLAY-COMP-FOOTER.
                 05 FILLER PIC X(88) VALUE SPACES.
                 05 FILLER PIC X(13) VALUE "Total Payroll".
                 05 FILLER PIC X VALUE SPACES.
                 05 FILLER PIC X(19) VALUE "(Salary and Sales):".
                 05 FILLER PIC X(3) VALUE SPACES.
                 05 DISPLAY-TOT-COMP PIC $$$$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

               PERFORM PROCESS-INIT
               PERFORM LOAD-TABLE
               PERFORM PROCESS-FILE
               PERFORM WRAP-UP
               PERFORM SEARCH-TABLE
               STOP RUN.
               
       LOAD-TABLE.
               PERFORM until EOFDIS = "Y"
                  READ TaxFile
                     AT END 
                        SET EOFDIS TO "Y"
                     NOT AT END
                        ADD 1 TO TableSize
                        MOVE DisTaxCodes to DisEntry (TableSize)
                       END-READ
               END-PERFORM.
               
       SEARCH-TABLE.
             MOVE 0 TO taxAmt
             IF SchoolDistrict equals spaces
                MOVE 0 TO TaxAmt
                MOVE CalcTaxAmt to DTL-TAX
             ELSE 
                PERFORM VARYING indx FROM 1 BY 1 UNTIL indx > TableSize
                   IF DisCodeVal (indx) = SchoolDistrict
                      COMPUTE taxAmt ROUNDED = TaxRateVal (indx) * pay
                      MOVE taxAmt to CalcTaxAmt
                      MOVE CalcTaxAmt to DTL-Tax
                  EXIT PERFORM
                END-IF
            END-PERFORM
            END-IF
           *> IF TaxAmt = 0 AND SchoolDistrict <> SPACES
             *>  MOVE "**********" TO DTL-TAX
            *> END-IF
            
            COMPUTE CalcNetPay = CalcNetPay - taxAmt.
            
       PROCESS-INIT.
               MOVE 1 TO PAGE-COUNT
               ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
               MOVE CORRESPONDING CURRENT-DATE TO HEADER-1
               ACCEPT CURRENT-TIME FROM TIME
               IF HH IN CURRENT-TIME > 11
                   MOVE "PM" TO AM-PM IN HEADER-2
                   IF HH IN CURRENT-TIME > 12
                       SUBTRACT 12 FROM HH IN CURRENT-TIME
                   END-IF
               ELSE
                   MOVE "AM" TO AM-PM IN HEADER-2
                   IF HH IN CURRENT-TIME = 00
                       MOVE 12 TO HH IN CURRENT-TIME
                   END-IF
               END-IF
               MOVE CORRESPONDING CURRENT-TIME TO HEADER-2
               OPEN INPUT MyFile
                    INPUT TaxFile
               OPEN OUTPUT OutputFile.

       WRAP-UP.
               CLOSE MyFile
               CLOSE OutputFile
               CLOSE TaxFile.
              

       PROCESS-FILE.
               PERFORM UNTIL EOF
                   READ MyFile
                   AT END
                       MOVE "AAAA" TO DepNum IN INPUT-FILE-RECORD
                       PERFORM DEPT-BREAK-FOOTER
                       PERFORM COMP-BREAK-FOOTER
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM DEPT-BREAK-FOOTER
                       PERFORM DEPT-BREAK-HEADER
                       PERFORM PROCESS-LINE
                       PERFORM MOVE-TO-ZERO
                       ADD 1 TO CURRENT-FILE-LINE
               END-PERFORM.

       PROCESS-LINE.
               IF LineNumber = LinesPerPage
                   PERFORM DISPLAY-HEADERS
                   WRITE OUTPUT-FILE-RECORD FROM COLUMN-HEADER
                   WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                   ADD 2 TO LineNumber
               END-IF
               MOVE CORRESPONDING INPUT-FILE-RECORD TO DETAIL-LINE
               MOVE SPACES TO DTL-INS-M
               MOVE SPACES TO DTL-INS-D
               MOVE SPACES TO DTL-INS-V
               PERFORM CALC-MONTHLY-PAY
               PERFORM GET-INSURANCE
               PERFORM CALC-COMMISSION
               PERFORM CALC-401K
               PERFORM CALC-FEDERAL
               PERFORM CALC-STATE
               PERFORM CALC-INSURANCE
               PERFORM CALC-NET-PAY
               PERFORM CALC-TOTAL-PAYROLL
               PERFORM CALC-COMPANY-PAYROLL
               WRITE OUTPUT-FILE-RECORD FROM DETAIL-LINE
               ADD 1 TO LineNumber.

       CALC-MONTHLY-PAY.
               COMPUTE CalcMonthlyPay ROUNDED = Pay / 12
               MOVE CalcMonthlyPay TO DTL-EXPECT-PAY.

           GET-INSURANCE.
               IF MedCov = "Y"
                   MOVE "M" TO DTL-INS-M
               END-IF
               IF DentCov = "Y"
                   MOVE "D" TO DTL-INS-D
               END-IF
               IF VisCov = "Y"
                   MOVE "V" TO DTL-INS-V
               END-IF.

       CALC-COMMISSION.
               IF SalComm
                   COMPUTE CalcCommission ROUNDED =
                       CommRate * ActualSales
                   MOVE CalcCommission TO DTL-COMMISSION
                   ADD CalcCommission TO CalcMonthlyPay
               ELSE
                   MOVE 0 TO CalcCommission
                   MOVE SPACES TO DTL-COMM-SPACES
               END-IF.

       CALC-401K.
               IF Withholding = 0
                   MOVE 0 TO Calc401k
                   MOVE SPACES TO DTL-401K-SPACES
               ELSE
                   COMPUTE Calc401k ROUNDED
                       = Withholding * CalcMonthlyPay
                   MOVE Calc401k TO DTL-401K
                   COMPUTE CalcMonthlyPay = CalcMonthlyPay - Calc401k
               END-IF.

       CALC-FEDERAL.
               IF MaritalStatus IN INPUT-FILE-RECORD = "M" OR "P"
                   COMPUTE CalcFed ROUNDED = CalcMonthlyPay * 0.28
               ELSE
                   COMPUTE CalcFed ROUNDED = CalcMonthlyPay * 0.33
               END-IF
               COMPUTE CalcMonthlyPay = CalcMonthlyPay - CalcFed
               MOVE CalcFed TO DTL-FED.

       CALC-STATE.
               COMPUTE CalcState ROUNDED = CalcMonthlyPay * .0475
               COMPUTE CalcMonthlyPay = CalcMonthlyPay - CalcState
               
               MOVE CalcState TO DTL-STATE.

       CALC-INSURANCE.
               IF NumOfDep IN INPUT-FILE-RECORD >= 2
                   IF MedCov EQUALS "Y"
                       ADD 100 TO CalcInsurance
                   END-IF
                   IF DentCov EQUALS "Y"
                       ADD 40 TO CalcInsurance
                   END-IF
                   IF VisCov EQUALS "Y"
                       ADD 7.5 TO CalcInsurance
                   END-IF
               ELSE
                   IF MedCov EQUALS "Y"
                       ADD 75 TO CalcInsurance
                   END-IF
                   IF DentCov EQUALS "Y"
                       ADD 25 TO CalcInsurance
                   END-IF
                   IF VisCov EQUALS "Y"
                       ADD 5 TO CalcInsurance
                   END-IF
               END-IF
               COMPUTE CalcMonthlyPay = CalcMonthlyPay - CalcInsurance
               MOVE CalcInsurance TO DTL-CALC-INSURANCE.

       CALC-NET-PAY.
               MOVE CalcMonthlyPay TO CalcNetPay
               MOVE CalcNetPay TO DTL-NETPAY.

       CALC-TOTAL-PAYROLL.
               ADD CalcNetPay TO CalcTotalDept.

       CALC-COMPANY-PAYROLL.
               ADD CalcNetPay TO CalcTotalComp.

       MOVE-TO-ZERO.
               MOVE 0 TO CalcMonthlyPay
               MOVE 0 TO CalcCommission
               MOVE 0 TO Calc401k
               MOVE 0 TO CalcFed
               MOVE 0 TO CalcState
               MOVE 0 TO CalcInsurance
               MOVE 0 TO CalcNetPay
               MOVE 0 TO AmountAfterFed.

       DISPLAY-HEADERS.
               ADD 1 TO PageNumber
               MOVE PageNumber TO PAGE-COUNT
               MOVE 0 TO LineNumber
               WRITE OUTPUT-FILE-RECORD FROM HEADER-1
               WRITE OUTPUT-FILE-RECORD FROM HEADER-2
               WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
               MOVE 3 TO LineNumber.

       DEPT-BREAK-FOOTER.
               IF DepNum IN INPUT-FILE-RECORD NOT = PREV-DEPT
                       AND CURRENT-FILE-LINE > 1
                   IF LineNumber >= (LinesPerPage - 2)
                       ADD 1 TO NewPage
                       PERFORM DISPLAY-HEADERS
                       MOVE CalcTotalDept TO DISPLAY-TOT-DEPT
                       MOVE 0 TO CalcTotalDept
                       MOVE PREV-DEPT TO DepNum IN DISPLAY-DEPT-FOOTER
                       WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                       WRITE OUTPUT-FILE-RECORD FROM DISPLAY-DEPT-FOOTER
                       ADD 2 TO LineNumber
                   ELSE
                       MOVE CalcTotalDept TO DISPLAY-TOT-DEPT
                       MOVE 0 TO CalcTotalDept
                       MOVE PREV-DEPT TO DepNum IN DISPLAY-DEPT-FOOTER
                       WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                       WRITE OUTPUT-FILE-RECORD FROM DISPLAY-DEPT-FOOTER
                       ADD 2 TO LineNumber
                   END-IF
               END-IF.


       DEPT-BREAK-HEADER.
               IF DepNum IN INPUT-FILE-RECORD NOT = PREV-DEPT
                   IF LineNumber >= (LinesPerPage - 3)
                    OR PageNumber  = 0
                      PERFORM DISPLAY-HEADERS
                      ADD 1 TO NewPage
                   END-IF
                   IF NewPage = 1
                      MOVE 0 TO NewPage
                      MOVE CORRESPONDING INPUT-FILE-RECORD TO HEADER-3
                      MOVE CORRESPONDING INPUT-FILE-RECORD TO HEADER-4
                      WRITE OUTPUT-FILE-RECORD FROM COLUMN-HEADER
                      WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                      WRITE OUTPUT-FILE-RECORD FROM HEADER-3
                      WRITE OUTPUT-FILE-RECORD FROM HEADER-4
                      WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                      ADD 5 TO LineNumber
                   ELSE
                       MOVE CORRESPONDING INPUT-FILE-RECORD TO HEADER-3
                       MOVE CORRESPONDING INPUT-FILE-RECORD TO HEADER-4
                       WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                       WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                       WRITE OUTPUT-FILE-RECORD FROM HEADER-3
                       WRITE OUTPUT-FILE-RECORD FROM HEADER-4
                       WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
                       ADD 5 TO LineNumber
                   END-IF
               END-IF
               MOVE DepNum IN INPUT-FILE-RECORD TO PREV-DEPT.

       COMP-BREAK-FOOTER.
               MOVE CalcTotalComp TO DISPLAY-TOT-COMP
               WRITE OUTPUT-FILE-RECORD FROM BLANK-LINE
               WRITE OUTPUT-FILE-RECORD FROM DISPLAY-COMP-FOOTER.
