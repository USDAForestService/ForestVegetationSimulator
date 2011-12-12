      SUBROUTINE RDOAGM
C----------
C  **RDOAGM      LAST REVISION: 08/07/97 
C----------
C
C  Purpose :
C     This subroutine calls "OTHER AGENT" sub-models. 
C
C  Called By :
C     RDTREG  [ROOT DISEASE]
C
C  Calls :
C     RDOWI   (SUBROUTINE)   [ROOT DISEASE]
C     RDOWIN  (SUBROUTINE)   [ROOT DISEASE]
C     RDBB1   (SUBROUTINE)   [ROOT DISEASE]
C     RDBB2   (SUBROUTINE)   [ROOT DISEASE]
C     RDBB3   (SUBROUTINE)   [ROOT DISEASE]
C     RDBB4   (SUBROUTINE)   [ROOT DISEASE]
C
C  Common Block Variables Used :
C     NUMBB  - Number of Bark Beetles eligible to act in this time
C              period, which cannot exceed 3*MAXSP
C     FRINGE(ITOTRR) - The area in the stand that is within an average
C              root radius of a root rot infection center, for each
C              root rot type.  This area may experience greater
C              mortality due to bark beetles.
C     HOST(3*MAXSP) - Host tree species on which each beetle acts
C     MINDBH(3*MAXSP) - MINimum host DBH at which trees become
C              susceptible to each BB.
C     MININF(3*MAXSP) - Minimum proportion of roots infected at which
C              IIRATE applies.
C     ORATE(3*MAXSP) - Mortality rate outside infection centers (and
C              any associated 'fringe') caused by each BB.
C     OFRATE(3*MAXSP) - Mortality rate outside infection centers in the
C              'Fringe' area that may experience greater mortality.
C     IURATE(3*MAXSP) - Mortality rate inside infection centers of
C              uninfected trees, for each BB.
C     IIRATE(3*MAXSP) Mortality rate inside infection centers of
C              infected trees, for each BB.
C
C  Revision History :
C     08/07/97 - Matthew K. Thompson
C                Removed section of code that modified PROBI due to      
C                windthrow.  Decided that windthrow mortality needed
C                to be removed from PROBI in the subroutine RDOWIN.
C

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77' 
      INCLUDE 'RDARRY.F77'


      IF (ICYC .EQ. 1) CALL RDOWI

C.... Update FFPROB:  If this is the first time cycle, set FFPROB(I,1)
C.... to FPROB(I).  Otherwise, set it to the lower of FFPROB(I,2) or
C.... FPROB(I).  Set FFPROB(I,2) to the current FPROB(I). For numerical
C.... reasons, calculate DIFF, which will be negative if FPROB < FFPROB

      DO 10 I=1,ITRN
         
         DIFF = FPROB(I) - FFPROB(I,2)

         IF ((DIFF .LE. 1E-4) .OR. (ICYC .EQ. 1)) THEN
            FFPROB(I,1) = FPROB(I)
         ELSE
            FFPROB(I,1) = FFPROB(I,2)
         ENDIF
         
         FFPROB(I,2) = FPROB(I)
   10 CONTINUE
  
C.... Copy dead standing array OAKL() from previous timestep to
C.... PROAKL(), then zero out OAKL(), BBKILL() AND RROBNK() for use in
C.... current iteration.

      DO 200 J=1,ITRN
         PRANKL(J) = RDKILL(J) 
         
         DO 100 I=1,3
            PROAKL(I,J) = OAKL(I,J)
            OAKL(I,J) = 0.0
  100    CONTINUE
  
         DO 133 I=1,4         
            BBKILL(I,J) = 0.0
  133    CONTINUE
  200 CONTINUE
  
      DO 233 I = 1,MAXSP
         RROBNK(I) = 0.0
  233 CONTINUE

C.... Cycle processing.

      DO 500 I=1,MAXSP
         DO 400 J=1,4
            WINDSP(I,J) = 0
  400    CONTINUE
  500 CONTINUE

C.... Call windthrow. 

      CALL RDOWIN
      
C.... Calls to bark beetle models.  First, determine which bark beetles
C.... are eligible to act.  Then apply the appropriate mortality to
C.... each tree record.  Note that if more than 3*MAXSP beetles are
C.... eligible to act, only the first (3*MAXSP - 1) plus the last one
C.... will actually be done.

      NUMBB = 0 

      DO 750 IRRSP=MINRR,MAXRR
         FRINGE(IRRSP) = 0.0
  750 CONTINUE      

      CALL RDBB1
      CALL RDBB2
      CALL RDBB3
      CALL RDBB4

      IF (NUMBB .GT. 0) CALL RDBBDO
      IF (IBBOUT .GT. 0) CALL RDBOUT
      
      DO 700 I = 1,ITRN                     

C....    Check to see that FPROB is not negative.

         IF (FFPROB(I,2) .LT. 0.0) FFPROB(I,2) = 0.0
  700 CONTINUE
                                                                        
      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

      RETURN
      END
