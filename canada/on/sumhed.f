      SUBROUTINE SUMHED
      IMPLICIT NONE
C----------
C  **SUMHED--LS/M DATE OF LAST REVISION:  07/11/08
C----------
C  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
C  SCREEN.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'SCREEN.F77'
C
COMMONS
C----------
      INTEGER I1,I2,ISTFNB,ISTLNB
      CHARACTER*7 FMT
      CHARACTER REV*10
      IF (.NOT.LSCRN) GOTO 1000
C----------
C  GET VARIANT NAME AND REVISION DATE.
C----------
      CALL REVISE (VARACD,REV)
        WRITE(JOSCRN,2) VARACD,REV
    2   FORMAT(/T20,A2,' FVS-ONTARIO VARIANT -- RV:',A10/)
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
     >  T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',4X,
     >  'AFTER TREATMENT GROWTH',/,T7,28('-'),1X,17('-'),1X,16('-'),
     >  ' CU  M',/, T7,'TREES',9X,'TOP',6X,' GTV  TREES  GTV   GMV ',
     >  9X,'TOP',6X,'PER YR',/,1X,'YEAR  /HA   BA SDI  HT  QMD ',
     >  'CU  M  /HA  CU  M CU  M  BA SDI  HT  QMD ACC MOR',/,1X,
     >  '---- ----- --- --- --- ---- ----- ----- ----- ----- ',
     >  '--- --- --- ---- --- ---')
 1000 CONTINUE
      RETURN
      END
