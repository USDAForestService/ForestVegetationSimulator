      SUBROUTINE BWEP4(ICALL)
      IMPLICIT NONE
C----------
C **BWEP4                   DATE OF LAST REVISION:  07/14/10
C----------
C
C  PRINT PARAMETER SUMMARY AND SPECIAL EVENTS TABLE
C
C  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR
C
C   CALLED FROM: BWELIT
C                BWEIN
C
C   SUBROUTINES AND FUNCTIONS CALLED: NONE
C
C   PARAMETERS:
C
C   EVENT  - STORES CODES THAT ARE REPORTED IN THIS TABLE
C   IEVENT(250,4) - BW SPECIAL EVENTS SUMMARY ARRAY
C   ITITLB - TITLE FOR BW MODEL
C   IYRCUR - CURRENT YEAR
C   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
C   NEVENT - NUMBER OF BW SPECIAL EVENTS TO DATE
C
C Revision History:
C   04-MAY-00 Lance David (FHTET)
C     .Added FVS common files for keyword file name and stand id in
C      summary heading.
C     .Corrected write statements and formats to use weater and outbreak
C      random number seed variables WSEEDR and OBSEER.
C   16-MAY-00 Lance David (FHTET)
C     .Replaced temp variable TEMPS1(1) with variable ILOBYR.
C   06-DEC-00 Lance David (FHTET)
C     .Condition .OR.ISPRAY.EQ.3 removed from ISPRAY.EQ.1 and ISPRAY.EQ.2
C      statements because there is no third spray option.
C   28-DEC-2005 Lance R. David (FHTET)
C     .Increased length of weather file name.
C   10-AUG-2006 Lance R. David
C     .Removed variables MOPT and OPTION from printing of summary output
C      because there is now only one source of options and that is the
C      FVS keyword file.
C   30-AUG-2006 Lance R. David (FHTET)
C      Changed array orientation of IEVENT from (4,250) to (250,4).
C   02-JUN-2009 Lance R. David (FMSC)
C      Added Stand ID and comma delimiter to output tables, some header
C      and column labels modified.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C
C----------------------------------------------------------------------

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
      CHARACTER*15 VARY
      CHARACTER*6 STAGE(3)
      CHARACTER*5 TRESIZ(4)
      CHARACTER*3 TRESP(7),CROWN(4)
      INTEGER I, ICALL, ICHECK, ICROWN, IS, ISIZE, J, KSP, N 

      DATA TRESP/' WF',' DF',' GF','SAF',' ES',' WL','all'/,
     *     TRESIZ/'small','med. ','large',' all '/,
     *     CROWN/'top','mid','bot','all'/,
     *     STAGE/'fourth','fifth','sixth'/
C
C IF THIS IS THE FIRST CALL, PRINT THE SUMMARY INFORMATION
C
      IF (ICALL.EQ.1) THEN
         WRITE (JOBWP4,15) MGMIDB,ITITLB
   15    FORMAT (2X,A4,' -- ',A72/)
         WRITE (JOBWP4,30) KWDFIL(1:40),NPLT
   30    FORMAT(' FVS keyword file: ',A40/' Stand ID: ',A26/)
         IS=ITEMP(4)
         ISTN=ITEMP(5)
         VARY='variable'
         IF (IWOPT .EQ. 2) VARY='constant'
         IF (IWSRC.EQ.1) THEN         
           WRITE (JOBWP4,50) STATES(IS,1),WSLOOK(ISTN,IS),
     *        VARY, WSEEDR
   50      FORMAT (' Budworm Defoliation Model Weather Options: ',
     *        ' state= ',A10,';  station= ',A16,';'/
     *        18X,'between years, parameters are: ',A15/
     *        18X,'random number seed for weather: ',F10.0/)
         ELSEIF (IWSRC.EQ.2) THEN
           WRITE (JOBWP4,55) WFNAME,VARY,WSEEDR
   55      FORMAT (' Budworm Defoliation Model Weather Options: ',/
     *        ' summary file = ',A,/
     *        18X,'between years, parameters are: ',A15/
     *        18X,'random number seed for weather: ',F10.0/)
         ELSE
            WRITE (JOBWP4,60) WFNAME
   60       FORMAT (' Budworm Defoliation Model Weather Options:',/
     *        ' annual values stored in file = ',A,/
     *        17X,' no variation applied!'/) 
         ENDIF
C
         VARY='was not'
         IF (LTEMP1(1)) VARY='* was *'
         IS=ITEMP(2)
         WRITE (JOBWP4,70) ILOBYR,VARY,TEMPS2(IS),OBSEER
   70    FORMAT (' Outbreak Status: year that last outbreak began= ',
     *       I4,/,18X,'outbreak ',A7,' active at the',
     *       ' start of the simulation',
     *          /,18X,'freq. / duration source: ',A20/,
     *          18X, 'random number seed for outbreaks:',F10.0/)
         IF (IOBOPT.EQ.3) THEN
          WRITE (JOBWP4,73) 
   73       FORMAT (' Outbreak Scheduling Option:  user has selected',
     *        ' starting and ending years')
            DO 78 N=1,NOBSCH
              WRITE (JOBWP4,75) N,IOBSCH(N,1),IOBSCH(N,2)
   75         FORMAT ('   outbreak no. ',i1,':  start= ',i4,
     *           ', end= ',i4)
   78       CONTINUE
         ENDIF
         IF (IOBOPT.EQ.2) WRITE (JOBWP4,72)
   72    FORMAT (' Outbreak Scheduling Option:  both starting and ',
     *      'ending dates generated'/10x,'from frequency and durat',
     *      'ion statistics')
         IF (IOBOPT.EQ.1) WRITE (JOBWP4,71)
   71    FORMAT (' Outbreak Scheduling Option:  starting dates gen',
     *      'erated from frequency'/10x,'statistics; outbreaks end ',
     *      'when % new defoliation is < 10% for'/10x,'3 consecutive',
     *      ' years (minimum duration = 5 years)')
         VARY=' increase  '
         IF (.NOT.LTEMP1(2)) VARY='are constant'
         ICHECK=0
         DO 80 I=1,4
         DO 80 J=1,3
         IF (NEMULT(I,J).NE.1.0) ICHECK=1
   80    CONTINUE
         IF (ICHECK.EQ.0) THEN
            WRITE (JOBWP4,90) VARY
   90       FORMAT (/' Natural Enemy Options: parasitism rates ',A12,
     *        ' during the outbreak'/24X,'all multipliers set to 1.0'/)
         ELSE
            WRITE (JOBWP4,95) VARY,((NEMULT(I,J),J=1,3),I=1,4)
   95       FORMAT (' Natural Enemy Options: parasitism rates ',A12,
     *        ' during the outbreak'/8X,'multipliers: birds= ',3F6.3,
     *        ';   ants= ',3F6.3/21X,'paras= ',3F6.3,';  other= ',
     *        3F6.3/)
         ENDIF
         VARY=' increase  '
         IF (.NOT.LTEMP1(3)) VARY='are constant'
         WRITE (JOBWP4,110) VARY
  110    FORMAT (' Foliage Quality Effects: larval development times ',
     *      A12,' during the outbreak')
         VARY=' decrease  '
         IF (.NOT.LTEMP1(4)) VARY='are constant'
         WRITE (JOBWP4,120) VARY
  120    FORMAT (26X,'pupal weights ',A12,' during the outbreak'/)
C
         IF (ISPRAY.EQ.1) THEN 
            DO 135 KSP=1,NSPRAY
               WRITE (JOBWP4,130) ISPYR(KSP),STAGE(INSTSP),SPEFF
  130          FORMAT(' Insecticide applied in year ',I4,/
     *         '   at peak ',A6,
     *         ' instar, resulting in ',F5.1,' % mortality.'/)
  135       CONTINUE
         ELSEIF (ISPRAY.EQ.2) THEN 
            WRITE (JOBWP4,140) ISPVAR,STAGE(INSTSP),SPEFF
  140       FORMAT(" When last year's % defoliation is >",I4,'%, an ',
     *      'insecticide is applied',/,'   at peak ',A6,
     *      ' instar, resulting in ',F5.1,' % mortality.')
            IF (LIMITS.EQ.1) THEN 
              WRITE (JOBWP4,145)
  145         FORMAT ('   Only one application is allowed per',
     *           ' outbreak.')
            ELSE
              WRITE (JOBWP4,147)
  147         FORMAT ('  No limit on the number of applications per ',
     *           'outbreak.')
            ENDIF
         ENDIF   
C
         WRITE (JOBWP4,150)
  150    FORMAT (//,  
     *      10X,'**********',5X,'Special Events Table',5X,'**********',
     *      //'Stand ID,',18X,' Year, Tree,  Size, Third, Event,',/
     *      26('-'),',',' ----, ----, -----, -----, ',50('-'),',')
C
C  OTHERWISE, PRINT THE SPECIAL EVENTS TABLE
C
      ELSE
      IF (NEVENT.LE.0) RETURN
      DO 300 N=1,NEVENT
C
C FIGURE OUT THE TREE SIZE CLASS AND CROWN THIRD
C
      IF (IEVENT(N,3).EQ.0) THEN
        ISIZE=4
        ICROWN=4
      ELSE
        IF (IEVENT(N,3).LE.3) THEN
           ISIZE=1
        ELSEIF (IEVENT(N,3).LE.6) THEN
           ISIZE=2
        ELSE
           ISIZE=3
        ENDIF
        ICROWN=MOD(IEVENT(N,3),3)
        IF (ICROWN.EQ.0) ICROWN=3
      ENDIF
C
      WRITE (JOBWP4,250) NPLT,IEVENT(N,1),TRESP(IEVENT(N,2)),
     *    TRESIZ(ISIZE),CROWN(ICROWN),EVENT(IEVENT(N,4))
  250 FORMAT (A26,', ',I4,', ',A3,',  ',A5,',  ',A3,',  ',A50,',')
C
  300 CONTINUE
C
      ENDIF
C
      RETURN
      END
