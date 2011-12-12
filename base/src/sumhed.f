      SUBROUTINE SUMHED
      IMPLICIT NONE
C----------
C  **SUMHED--BS  DATE OF LAST REVISION:  07/23/08
C----------
C  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
C  SCREEN.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'SCREEN.F77'
C
C
COMMONS
C----------
      INTEGER I1,I2,ISTFNB,ISTLNB
      CHARACTER*7 FMT
      CHARACTER VVER*7,REV*10
      IF (.NOT.LSCRN) GOTO 1000
C----------
C  GET VARIANT NAME AND REVISION DATE.
C----------
      CALL VARVER (VVER)
      CALL REVISE (VVER,REV)
      IF(VVER(:2).EQ.'SM' .OR. VVER(:2).EQ.'SP' .OR. VVER(:2).EQ.'BP'
     & .OR. VVER(:2).EQ.'SF' .OR. VVER(:2).EQ.'LP')THEN
        WRITE(JOSCRN,1) VVER(:2),REV
    1   FORMAT(/T20,'CR-',A2,' FVS VARIANT -- RV:',A10/)
      ELSE
        WRITE(JOSCRN,2) VVER(:2),REV
    2   FORMAT(/T20,A2,' FVS VARIANT -- RV:',A10/)
      ENDIF
C
      WRITE(JOSCRN,5) NPLT,MGMID
    5 FORMAT(/T10,'STAND = ',A26,'  MANAGEMENT CODE = ',A4/)
      I1=ISTFNB(ITITLE)
      IF (I1.GT.0) THEN
         I2=ISTLNB(ITITLE)
         WRITE (FMT,'(''(T'',I2.2,'',A)'')') (81-I2+I1)/2
         WRITE (JOSCRN,FMT) ITITLE(I1:I2)
      ENDIF
      WRITE (JOSCRN,10)
   10 FORMAT (/T17,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'
     >  /1X,76('-')/
     >  T8,'START OF SIMULATION PERIOD    REMOVALS/ACRE',4X,
     >  'AFTER TREATMENT GROWTH',/,T7,28('-'),1X,17('-'),1X,16('-'),
     >  ' CU FT',/, T7,'TREES',9X,'TOP',6X,'TOTAL TREES TOTAL MERCH',
     >  9X,'TOP',6X,'PER YR',/,1X,'YEAR /ACRE  BA SDI  HT  QMD ',
     >  'CU FT /ACRE CU FT BD FT  BA SDI  HT  QMD ACC MOR',/,1X,
     >  '---- ----- --- --- --- ---- ----- ----- ----- ----- ',
     >  '--- --- --- ---- --- ---')
 1000 CONTINUE
      RETURN
      END
