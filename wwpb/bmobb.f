      SUBROUTINE BMOBB(ISTD,IYR)
C----------
C WWPB $Id$
C----------
C
C  BARK BEETLE FUNCTIONAL TYPE 3, EXCEPT THAT THE BEETLES DON'T ACTUALLY KILL TREES; THEY
C  ONLY SET THEM UP TO BE ATTACKED BY THE MAIN BEETLE SPECIES.  THE MODEL REMAINS ON ONLY
C  FOR THE DURATION THAT THE USER SPECIFIED.
C
C  CALLED BY :
C     BMDRV
C
C  CALLS     : 
C     GPGET2
C     GPADD
C
C  PARAMETERS :
C     MINSIZ: Minimum size class for killing
C     THRESH: Threshold for outbreak to occur
C     ATTKRT: Tree attack rate
C     GRFMAX: Level above which tree is too healthy (GRF too high)
C******************************************************************************
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.
      
      LOGICAL LOK
      INTEGER ISIZ, MINSIZ
      INTEGER DUM(1)
      REAL    GRFMAX
      REAL    ATTKRT
      REAL    THRESH
      REAL    CLSUMI
      DIMENSION PRMS(7)

      SAVE

      CLSUMI = 0.0

C.... Check for debug.

      IF (LBMDEB) WRITE (JBMBPR,3) IYR, ISTD
    3 FORMAT (' Begin BMOBB:  Year=',I5, ' Stand= ',I6)

      DO 117 ISIZ=1,NSCL
         OTHATT(ISTD,ISIZ) = 0.0
  117 CONTINUE

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
      IF (ICNT .GE. BMEND) ICNT = 0      
      ICNT = ICNT + 1
      
      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 5

C     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
C     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
C     SET OF PARAMETERS.
         
        CALL GPGET2 (323, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN
  
          IYR2= IYR1 + IFIX(PRMS(1)) - 1

          MINSIZ = INT(PRMS(2))
          THRESH = PRMS(3)
          ATTKRT = PRMS(4)
          GRFMAX = PRMS(5)

          IF (LBMDEB) WRITE (JBMBPR,101) IYR1,MINSIZ,THRESH,
     &                                   ATTKRT,GRFMAX     
  101     FORMAT (/' IN BMOBB: IYR1=',I5,' MINSIZ=',
     >    I5,' THRESHOLD=',F7.4,' ATTACK RATE= ',F6.4,' MAX RV=',F4.2)
  
          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 323, NPRMS, PRMS, 1, DUM) 
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,103) PRMS(1),IYR2
  103       FORMAT (/' IN BMOBB: OTHER BARK BEETLES ARE SCHEDULED FOR ',
     >        'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,
     >        '  NEW IYR2=',I5)     
          ENDIF
        ELSE  
C         Zero out the attact rate to signify that bark beetles are not active this year.
          IF (IYR .GT. IYR2) ATTKRT = 0.0
        ENDIF 
      ENDIF
      
C     IF THE ATTACK RATE IS ZERO THEN ASSUME THE MODEL IS NOT ACTIVE THIS YEAR.      
      IF (ATTKRT .LE. 0.0) RETURN
C
C     IF NO DESIRED SPECIES PRESENT THEN RETURN.
C
      IF (BAH(ISTD,NSCL+1) .LE. 0.0) GOTO 777
C
C     LOOP OVER TREE LIST AND CALCULATE THE
C     NUMBER OF TARGET STEMS, DEFINED AS THOSE
C     HAVING A LOWER GRF THAN A USER SPECIFIED LIMIT
c     (NONHOSTS HAVE NO GRF SO ALL TREES ARE ELIGIBLE)
C

      DO 6 ISIZ= MINSIZ, NSCL
         IF (GRF(ISTD,ISIZ) .GT. GRFMAX) GOTO 6
         CLSUMI = CLSUMI + TREE(ISTD,ISIZ,1)
    6 CONTINUE

C
C     COMPARE DENSITY OF STEMS TO THRESHOLD SPECIFIED BY USER 
C
      IF (CLSUMI .LT. THRESH) GOTO 777


C     SUFFICIENT STEMS FOR OUTBREAK, NOW APPLY SPECIFIC ATTACK RATE.  
      
      DO 20 ISIZ= MINSIZ,NSCL

         IF (GRF(ISTD,ISIZ) .GT. GRFMAX) GOTO 20
         
         OTHATT(ISTD,ISIZ) = OTHATT(ISTD,ISIZ) + ATTKRT
         OTHATT(ISTD,ISIZ) = AMIN1(OTHATT(ISTD,ISIZ),1.0)
         
   20 CONTINUE

  777 CONTINUE

      IF (LBMDEB) WRITE (JBMBPR,999) IYR, ISTD
  999 FORMAT (' End BMOBB :  Year=',I5, ' Stand= ',I6)

      RETURN
      END
