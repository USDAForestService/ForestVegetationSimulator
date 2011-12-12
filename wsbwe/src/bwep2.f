
      SUBROUTINE BWEP2(IYRCUR)
      IMPLICIT NONE
C-----------
C **BWEP2      LAST REVISED: 2/25/97
C-----------
C
C  PRINT ANNUAL OUTPUT RE: BUDWORM MORTALITY & FEEDING
C
C  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR
C
C   CALLED FROM: BWELIT
C
C   SUBROUTINES AND FUNCTIONS CALLED:	NONE
C
C   PARAMETERS:
C
C   ITITLB - TITLE FOR BW MODEL
C   IYRCUR - CURRENT YEAR
C   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
C   OUT2(IH,ISIZE,I) -- VARIABLES FOR I: 1= INITIAL NO. OF BW EGGS;
C     2= TOTAL % MORTALITY, 3= NO. OF EGGS PRODUCED, 4= % NEW DEFOL.,
C     5= % TOTAL DEFOL.
C
C  Revision History:
C   25-FEB-97 
C      Noted last revision date.
C   02-JUN-2009 Lance R. David (FMSC)
C      Added Stand ID and comma delimiter to output tables, some header
C      and column labels modified.
C      ****************
C      ***** NOTE ***** Modified this subroutine before realizing it
C      **************** is not actually called by any other routine.
C      This subroutine is currently just hanging around and not in use.
C---------------------------------------------------------------------
C
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'

      LOGICAL HEADER
      CHARACTER*3 TRESP(6)
      CHARACTER*5 TRESIZ(3)
      INTEGER I, IH, IS, IYRCUR, NPLT 
      REAL DEFNEW, DEFTOT, PMORT, SAVOUT(7), SAVTRE(7), TOTFOL

      DATA HEADER/.TRUE./
      DATA TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/,
     *     TRESIZ/'SMALL','MED. ','LARGE'/
C
      DO 10 I=1,7
   10 SAVOUT(I)=0.0
C
C IF THIS IS THE FIRST CALL, PRINT THE HEADER
C
      IF (HEADER) THEN
        WRITE (JOBWP2,15) MGMIDB,ITITLB
   15   FORMAT (2X,A4,' -- ',A72/)
        WRITE (JOBWP2,20) 
   20 FORMAT (49X,'INITIAL     FINAL       %         % DEFOL.',/
     *   'STAND ID,',18X,'YEAR, SPECIES, SIZE,  # EGGS,   # EGGS,',
     *   '    MORT.,    NEW,     TOTAL,',/
     *   26(','),', ----, -------, ----,  ------,   ------,',
     *   '   ------,   -----,    -----,') 
         HEADER=.FALSE.
      ELSE
         WRITE (JOBWP2,80)
   80    FORMAT (1X,6('----------'))
      ENDIF
C
C  PRINT OUT THE WHOLE SHEBANG!
C
      DO 200 IH=1,6
      TOTFOL=OUT2(IH,1,7)+OUT2(IH,2,7)+OUT2(IH,3,7)
      IF (TOTFOL.LT.1.0) GOTO 180
C
      DO 85 I=1,7
   85 SAVTRE(I)=0.0
      WRITE (JOBWP2,90)
   90 FORMAT (1X)
      DO 120 IS=1,3
      IF (OUT2(IH,IS,7).LT.1.0) GOTO 120
C
C  CALC. % BW MORTALITY AND % DEFOLIATION
C
      PMORT=-1.0
      IF (OUT2(IH,IS,1).GT.0.0) PMORT=100.0*(1.0-(OUT2(IH,IS,8)/
     *   OUT2(IH,IS,1)))
      DEFNEW=-1.0
      IF (OUT2(IH,IS,6).GT.0.0) DEFNEW=100.0*(1.0-(OUT2(IH,IS,4)/
     *   OUT2(IH,IS,6)))
      DEFTOT=-1.0
      IF (OUT2(IH,IS,7).GT.0.0) DEFTOT=100.0*(1.0-(OUT2(IH,IS,5)/
     *   OUT2(IH,IS,7)))
      WRITE (JOBWP2,100) NPLT,IYRCUR,TRESP(IH),TRESIZ(IS),
     *    OUT2(IH,IS,1),OUT2(IH,IS,2),PMORT,DEFNEW,DEFTOT
  100 FORMAT (A26,', 'I4,',  ',A3,',  ',A5,', ',F8.0,', ',F8.0,
     *        ', ',F8.1,',  ',F7.0,', ',F7.0)
C
      SAVOUT(1)=SAVOUT(1)+OUT2(IH,IS,1)
      SAVOUT(2)=SAVOUT(2)+OUT2(IH,IS,2)
      SAVOUT(3)=SAVOUT(3)+OUT2(IH,IS,8)
      SAVOUT(4)=SAVOUT(4)+OUT2(IH,IS,4)
      SAVOUT(5)=SAVOUT(5)+OUT2(IH,IS,5)
      SAVOUT(6)=SAVOUT(6)+OUT2(IH,IS,6)
      SAVOUT(7)=SAVOUT(7)+OUT2(IH,IS,7)  
      SAVTRE(1)=SAVTRE(1)+OUT2(IH,IS,1)
      SAVTRE(2)=SAVTRE(2)+OUT2(IH,IS,2)
      SAVTRE(3)=SAVTRE(3)+OUT2(IH,IS,8)
      SAVTRE(4)=SAVTRE(4)+OUT2(IH,IS,4)
      SAVTRE(5)=SAVTRE(5)+OUT2(IH,IS,5)
      SAVTRE(6)=SAVTRE(6)+OUT2(IH,IS,6)
      SAVTRE(7)=SAVTRE(7)+OUT2(IH,IS,7)  
C
  120 CONTINUE
C
C  PRINT SUMMARY FOR THIS TREE SPECIES
C
      PMORT=-1.0
      IF (SAVTRE(1).GT.0.0) PMORT=100.0*(1.0-(SAVTRE(3)/SAVTRE(1)))
      DEFNEW=-1.0
      IF (SAVTRE(6).GT.0.0) DEFNEW=100.0*(1.0-(SAVTRE(4)/SAVTRE(6)))
      DEFTOT=-1.0
      IF (SAVTRE(7).GT.0.0) DEFTOT=100.0*(1.0-(SAVTRE(5)/SAVTRE(7)))
      WRITE (JOBWP2,150) NPLT,IYRCUR,TRESP(IH),SAVTRE(1),SAVTRE(2),
     *   PMORT,DEFNEW,DEFTOT
  150 FORMAT (A26,', ',I4,',  ',A3,',  ','ALL  , ',2(F8.0,', '),
     *        F8.1,',  ',F7.0,', ',F7.0)
C
C  RESET THE ARRAY TO ZERO
C
  180 DO 190 I=1,8
      OUT2(IH,1,I)=0.0
      OUT2(IH,2,I)=0.0
      OUT2(IH,3,I)=0.0
  190 CONTINUE
  200 CONTINUE
C
C  PRINT OUT THE SUMMARY FOR THE WHOLE STAND
C
      PMORT=-1.0
      IF (SAVOUT(1).GT.0.0) PMORT=100.0*(1.0-(SAVOUT(3)/SAVOUT(1)))
      DEFNEW=-1.0
      IF (SAVOUT(6).GT.0.0) DEFNEW=100.0*(1.0-(SAVOUT(4)/
     *   SAVOUT(6)))
      DEFTOT=-1.0
      IF (SAVOUT(7).GT.0.0) DEFTOT=100.0*(1.0-(SAVOUT(5)/SAVOUT(7)))
      WRITE (JOBWP2,300) NPLT,IYRCUR,
     *    SAVOUT(1),SAVOUT(2),PMORT,DEFNEW,DEFTOT
  300 FORMAT(/A26,', ',I4,',  ','ALL  ,  ','ALL  ,  ',F8.0,', ',
     *       F8.0,', ',F8.1,',  ',F7.0,', ',F7.0,', ')
      
C
      RETURN
      END
