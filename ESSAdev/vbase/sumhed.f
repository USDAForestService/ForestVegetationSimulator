      SUBROUTINE SUMHED
      IMPLICIT NONE
C----------
C VBASE $Id$
C----------
C  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
C  SCREEN.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'SCREEN.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      CHARACTER*7 FMT
      CHARACTER REV*10
C
      CHARACTER*2 CRMT(5)
C
      INTEGER I1,I2,ISTFNB,ISTLNB
C
C----------
C  DEFINITION OF LOCAL VARIABLES:
C----------
C     CRMT -- CENTRAL ROCKIES ALPHABETIC MODEL TYPE DESIGNATOR
C----------
C  DATA STATEMENTS
C----------
      DATA CRMT/'SM','SP','BP','SF','LP'/
C----------
      IF (.NOT.LSCRN) GOTO 1000
C----------
C  GET VARIANT REVISION DATE.
C----------
      CALL REVISE (VARACD,REV)
C
      SELECT CASE (VARACD)
C
      CASE ('CR')
        WRITE(JOSCRN,1) CRMT(IMODTY),REV
    1   FORMAT(/T19,'CR-',A2,' FVS VARIANT -- RV:',A10/)
C
      CASE DEFAULT
        WRITE(JOSCRN,2) VARACD,REV
    2   FORMAT(/T19,A2,' FVS VARIANT -- RV:',A10/)
C
      END SELECT
C
      WRITE(JOSCRN,5) NPLT,MGMID
    5 FORMAT(/T9,'STAND = ',A26,'  MANAGEMENT CODE = ',A4/)
      I1=ISTFNB(ITITLE)
      IF (I1.GT.0) THEN
         I2=ISTLNB(ITITLE)
         WRITE (FMT,'(''(T'',I2.2,'',A)'')') (81-I2+I1)/2
         WRITE (JOSCRN,FMT) ITITLE(I1:I2)
      ENDIF
C
      SELECT CASE (VARACD)
C----------
C  EASTERN VARIANTS
C----------
      CASE ('CS','LS','NE','SN')
        WRITE (JOSCRN,10)
   10   FORMAT (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'
     >  /,76('-')/
     >  T8,'START OF SIMULATION PERIOD    REMOVALS/ACRE',4X,
     >  'AFTER TREATMENT GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'),
     >  ' CU FT',/, T6,'TREES',9X,'TOP',6X,'MERCH TREES MERCH SAWLG',
     >  9X,'TOP',6X,'PER YR',/,'YEAR /ACRE  BA SDI  HT  QMD ',
     >  'CU FT /ACRE CU FT BD FT  BA SDI  HT  QMD ACC MOR',/,
     >  '---- ----- --- --- --- ---- ----- ----- ----- ----- ',
     >  '--- --- --- ---- --- ---')
C----------
C  WESTERN AND SOUTHERN VARIANTS
C----------
      CASE DEFAULT
        WRITE (JOSCRN,20)
   20   FORMAT (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'
     >  /,76('-')/
     >  T8,'START OF SIMULATION PERIOD    REMOVALS/ACRE',4X,
     >  'AFTER TREATMENT GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'),
     >  ' CU FT',/, T6,'TREES',9X,'TOP',6X,'TOTAL TREES TOTAL MERCH',
     >  9X,'TOP',6X,'PER YR',/,'YEAR /ACRE  BA SDI  HT  QMD ',
     >  'CU FT /ACRE CU FT BD FT  BA SDI  HT  QMD ACC MOR',/,
     >  '---- ----- --- --- --- ---- ----- ----- ----- ----- ',
     >  '--- --- --- ---- --- ---')
C
      END SELECT
C
 1000 CONTINUE
C
      RETURN
      END
