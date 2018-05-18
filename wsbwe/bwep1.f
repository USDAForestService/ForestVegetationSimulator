      SUBROUTINE BWEP1(ISTAGE,IYRCUR)
      IMPLICIT NONE
C----------
C **BWEP1                   DATE OF LAST REVISION:  06/18/13
C----------
C
C  PRINT DETAILED OUTPUT RE: BUDWORM MORTALITY & FEEDING
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
C   OUT1(IC,IH,I) -- VARIABLES FOR I: 1= INITIAL NO. OF BW;
C      2= NET BW DISPERSING IN; 3-9= % MORTALITY CAUSED BY DISPERSAL
C      (3), STARVATION (4), ANTS (5), BIRDS (6), PARASITES (7),
C      OTHERS (8), OR TOTAL (9); 10=KG NEW FOLIAGE, 11=KG OLD FOLIAGE,
C      12-14= % DEFOLIATION FOR NEW (12), OLD (13), OR TOTAL (14)
C      FOLIAGE; FOR OLDER LARVAE ONLY, 15-17 = % OF OLDER LARVAE THAT
C      FEED ON NEW FOLIAGE (15), OLDER FOLIAGE (16), OR PUPATE EARLY (17).
C      
C  Revision History:
C    14-JUL-2010 Lance R. David (FMSC)
C       Previous noted revision 2/25/97.
C       Added IMPLICIT NONE and declared variables as needed.
C----------
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'

      LOGICAL HEADER
      CHARACTER*3 TRESP(6),CROWN(3)
      CHARACTER*5 TRESIZ(3),STAGE(3)
      INTEGER DEFTOT, I, IC, ICROWN, IH, ISIZE, ISTAGE, IYRCUR,
     &        J, K, LSIZE, LSPEC, LSTAGE, LYRCUR
      REAL TEMP(7)

      DATA HEADER/.TRUE./
      DATA TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/,
     &     TRESIZ/'SMALL','MED. ','LARGE'/,
     &     CROWN/'TOP','MID','BOT'/,
     &     STAGE/'L2-L4','L4-L6','PUPAE'/
C
C IF THIS IS THE FIRST CALL, PRINT THE HEADER
C
      IF (HEADER) THEN
      WRITE (JOBWP1,15) MGMIDB,ITITLB
   15 FORMAT (A4,' -- ',A72/)
      WRITE (JOBWP1,20)  
   20 FORMAT (45X,'PERCENT MORTALITY CAUSED BY:',17X,'FOLIAGE',
     &  ' (G)',11X,'  OLDER LARV FEED.'/
     &  45X,5('-----'),'---  TOTAL',2X,6('-----'),2X,
     &  3('-----'),/,5X,'TREE  TREE CROWN  BW   ',
     &  'INITIAL    NET ',34X,'%      AMT.   AMT.      % DEFOL.',6X,
     &  '%    %    %',/'YEAR SPEC. SIZE THIRD STAGE  NUMBER ',
     &  '  DISP.  DISP STAR ANTS BIRD PARA OTH. MORT.    NEW', 
     &  '    OLD    NEW  OLD TOTAL',
     &  '  NEW  OLD EARLY'/'---- ----- ---- ----- -----  ------',
     &  ' -------  ---- ---- ---- ---- ---- ---- -----  ------',
     &  ' ------  ---- ---- ----- ---- ---- ----')
      LYRCUR=IYRCUR
      LSIZE=0
      LSPEC=0
      ENDIF
C
      DO 200 IC=1,9
C
C  TRANSLATE CROWN LEVEL INTO TREE SIZE AND CROWN THIRD
C
      IF (IC.LE.3) THEN
         ISIZE=1
      ELSEIF (IC.LE.6) THEN
         ISIZE=2
      ELSE
         ISIZE=3
      ENDIF
      ICROWN=MOD(IC,3)
      IF (ICROWN.EQ.0) ICROWN=3
C
      DO 200 IH=1,6
      IF (OUT1(IC,IH,1).LT.1.0) GOTO 200
C
C CALC. % MORT. FROM ACTUAL #'S KILLED
C
      OUT1(IC,IH,9)=0.0
      DO 40 I=3,8
      OUT1(IC,IH,9)=OUT1(IC,IH,9)+OUT1(IC,IH,I)
   40 CONTINUE
      DO 60 J=1,7
      TEMP(J)=100.0*OUT1(IC,IH,J+2)/(OUT1(IC,IH,1)+OUT1(IC,IH,2))
   60 CONTINUE
C
C CALC % DEFOL FOR TOTAL FOLIAGE
C
      IF (ISTAGE.NE.3) THEN
         DEFTOT=((OUT1(IC,IH,10)*OUT1(IC,IH,12))+(OUT1(IC,IH,11)*
     &   OUT1(IC,IH,13)))/(OUT1(IC,IH,10)+OUT1(IC,IH,11))
      ENDIF
C
C  PRINT OUT THE WHOLE SHEBANG!
C
      IF (HEADER) THEN
         HEADER=.FALSE.
         LYRCUR=IYRCUR
         LSTAGE=ISTAGE
         LSPEC=IH
         LSIZE=ISIZE
         WRITE (JOBWP1,95)
      ELSEIF (LYRCUR.NE.IYRCUR) THEN
         WRITE (JOBWP1,80)
   80    FORMAT (10X,11('----------'))
         WRITE (JOBWP1,90)
   90    FORMAT (15X,11('*********'))
         WRITE (JOBWP1,80)
         LYRCUR=IYRCUR
         LSTAGE=ISTAGE
         LSPEC=IH
         LSIZE=ISIZE
      ELSEIF (LSTAGE.NE.ISTAGE) THEN
         WRITE (JOBWP1,80)
         LSTAGE=ISTAGE
         LSPEC=IH
         LSIZE=ISIZE
      ELSEIF (LSPEC.NE.IH) THEN
         WRITE (JOBWP1,95)
   95    FORMAT (1X)
         LSPEC=IH
         LSIZE=ISIZE
      ELSEIF (LSIZE.NE.ISIZE) THEN
         WRITE (JOBWP1,95)
         LSIZE=ISIZE
      ENDIF
      IF (ISTAGE.EQ.1) THEN
        WRITE (JOBWP1,100) IYRCUR,TRESP(IH),TRESIZ(ISIZE),
     &     CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),
     &     (TEMP(I),I=1,7),(OUT1(IC,IH,J),J=10,13),DEFTOT
  100   FORMAT (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,
     &     2F8.0,1X,F6.0,2F5.0,2X,' ---  ---  ---')
      ELSEIF (ISTAGE.EQ.2) THEN
        WRITE (JOBWP1,120) IYRCUR,TRESP(IH),TRESIZ(ISIZE),
     &     CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),
     &     (TEMP(I),I=1,7),(OUT1(IC,IH,J),J=10,13),DEFTOT,
     &     (OUT1(IC,IH,K),K=15,17)
  120   FORMAT (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,
     &     2F8.0,1X,F6.0,2F5.0,1X,3F5.0)
      ELSE
        WRITE (JOBWP1,140) IYRCUR,TRESP(IH),TRESIZ(ISIZE),
     &     CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),
     &     (TEMP(I),I=1,7)
  140   FORMAT (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,
     &     2X,'  ---     ---',3X,3('  ---'),3X,'---  ---  ---')
      ENDIF
C
C  RESET THE ARRAY TO ZERO
C
      DO 170 I=1,17
      OUT1(IC,IH,I)=0.0
  170 CONTINUE
C
  200 CONTINUE
C
      RETURN
      END
