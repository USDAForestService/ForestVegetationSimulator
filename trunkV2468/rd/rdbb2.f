      SUBROUTINE RDBB2
      IMPLICIT NONE
C----------
C  **RDBB2       LAST REVISION:  08/26/14
C----------                              
C
C  Purpose :
C     Determines how many races of bark beetle type 2 are eligible to
C     act in this growth cycle under current stand conditions.  The
C     mortality rates etc. specified for each beetle race that is
C     active are reported back to RDOAGM. By default, each race that
C     was scheduled for this time period is rescheduled for the next
C     time period (whether it was active this time or not, unless the
C     user specifies otherwise).
C
C     Keyword:  BBTYPE2
C     Inputs:  1 = first outbreak year    2 = host tree species
C              3 = DBH limit              4 = threshold density of
C                                             lg. enough windfallen host 
C              5 = "KILL Factor"          6 = 0 for reschedule,
C                                             1 for don't
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Calls :
C     OPFIND  (SUBROUTINE)   [FVS]
C     OPGET   (SUBROUTINE)   [FVS]
C     OPDONE  (SUBROUTINE)   [FVS]
C     OPREDT  (SUBROUTINE)   [FVS]
C     OPCOPY  (SUBROUTINE)   [FVS]
C     OPINCR  (SUBROUTINE)   [FVS]
C
C REVISION HISTORY:
C    06-AUG-01 Lance R. David (FHTET)
C       Change MYACT to IACTK in call to OPCOPY because of array to scalar
C       mismatch reported by LF95.
C       (previous date of last revision was 12/7/93)
C    10-DEC-07 Lance R. David (FHTET)
C       Update to argument list in call to OPCOPY, added variable
C       DONE for debug/tracking (from Don Robinson).
C   08/26/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C....................................................................

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.
      
      INTEGER   I, J, I1, I2, ISPI, ONLY1X, NOCOPY, RACE, DONE, IACTK
      INTEGER   KDT, KODE, MYACT(1), NCOPYS, NPS, NTODO
      REAL      KILLF, CLSUML, CLSUMI, SPSUMI, SPSUML, CLCNT, CLCNTA
      
      REAL      DBHLIM, THRESH, MORT, TSTEM, TDENS, CLKILL
      REAL      PRMS(5)
      
      IF (ITRN .LE. 0) RETURN
      NOCOPY = 0
      DONE = 0

C.... Ask OPFIND how many races of this beetle type are scheduled for
C.... this time period.

      DATA MYACT /2416/
      CALL OPFIND (1,MYACT,NTODO)
      IF (NTODO .LE. 0) RETURN
      
C.... For each race, first get the parameter values, and determine the
C.... first (I1) and last (I2) records of the host species.  Check that
C.... there is at least one record of the host species, and that some
C.... were windthrown.  NOTE that in this case PRMS(4) is NOT the
C.... mortality rate applied to eligible trees in a beetle outbreak. 
C.... Rather, it is the number of standing trees that will die from 
C.... bark beetles FOR EACH TREE KILLED BY WINDTHROW.  The default
C.... is that, for each tree killed by windthrow, 0.875 trees will
C.... die from beetles (from McNamee et al.1991).  

      DO 999 RACE = 1, NTODO
         CALL OPGET (RACE,5,KDT,IACTK,NPS,PRMS)
 
         ISPI = INT(PRMS(1))
         DBHLIM = PRMS(2)
         THRESH = PRMS(3)
         KILLF = PRMS(4)
         ONLY1X = INT(PRMS(5))
        
         I1 = ISCT(ISPI,1)
         I2 = ISCT(ISPI,2)
        
         IF (ISCT(ISPI,1) .EQ. 0) GOTO 888
         IF (WINDSP(ISPI,3) .LE. 0.0) GOTO 888
        
C....    Second, go through the host tree records and determine the
C....    number of windfallen stems larger than the user's size
C....    criteria.  This is done by some conversion of the proportion
C....    of the total crown area that is due to trees of this size. 
C....    The 'L' subscript means "Clean", not "Live".
        
C....    Zero out class & species accumulators.  
C....    'class' is defined as greater than specified DBH.

         CLSUML = 0
         CLSUMI = 0
         SPSUML = 0
         SPSUMI = 0

C     GET PROP'NS OF FALLEN STEMS IN SIZE CLASS
C     FROM SUM OF WEIGHT COEFFS FOR ALL FALLEN IN
C     PROP TO THOSE WHICH QUALIFY
C     NB. WEIGHT COEFFICIENTS ARE THOSE USED TO CALCULATE
C     SUSCEPTIBLITY TO WINDFALL IN SUBROUTINE RROWIN

         DO 10 J = 1,ILEND
            I = ISTEML(J)
            IF (ISP(I) .NE. ISPI) GOTO 10
            SPSUML = SPSUML + WINDWL(I)
            IF (DBH(I) .LT. DBHLIM) GOTO 10
            CLSUML = CLSUML + WINDWL(I)
   10    CONTINUE

C....    Infected stems.

         DO 20 J = 1,IIEND
            I = ISTEMI(J)
            IF (ISP(I) .NE. ISPI) GOTO 20
            SPSUMI = SPSUMI + WINDWI(I)
            IF (DBH(I) .LT. DBHLIM) GOTO 20
            CLSUMI = CLSUMI + WINDWI(I)
   20    CONTINUE

C....    Number of fallen stems in size class.

         IF (SPSUML .LT. 1E-3) SPSUML = 1E-3
         IF (SPSUMI .LT. 1E-3) SPSUMI = 1E-3
         CLCNT = (WINDSP(ISPI,1) * CLSUML / SPSUML) +
     &           (WINDSP(ISPI,2) * CLSUMI / SPSUMI)
         CLCNTA = CLCNT / SAREA
      

C Compare CLCNTA, the density of windfallen stems that met the user's size criteria, to
C the user's threshold for an active beetle outbreak.

         IF (CLCNTA .LT. THRESH) GOTO 888
      
C If the beetle is active, call OPDONE to signal the outbreak, increment NUMBB and 
C store the specified parameter values for later use in killing trees.  Calculate the 
C mortality rate needed from KILLF and the number of windfallen trees.      

         CALL OPDONE (RACE,IY(ICYC))
         DONE = 1
      
         NUMBB = NUMBB + 1
         IF (NUMBB .GT. 3*MAXSP) NUMBB = 3*MAXSP
         HOST(NUMBB) = ISPI
         MINDBH(NUMBB) = DBHLIM
         MININF(NUMBB) = 0.0
 
C....    target number of trees to kill

         CLKILL = CLCNT * KILLF
      
C....    number of live trees (note:  this is total before windthrow occurred)

         TDENS = 0.0

         DO 30 J = I1,I2
            I = IND1(J)
            IF (DBH(I) .LT. DBHLIM) GOTO 30
            TDENS = TDENS + PROBL(I)
   30    CONTINUE

         TSTEM = TDENS * SAREA
 
C....    mortality rate = number to kill / number living

         IF (CLKILL .LT. TSTEM) THEN
            MORT = CLKILL / TSTEM
         ELSE
            MORT = 1.0
         ENDIF
        
         ORATE(NUMBB) = MORT
         OFRATE(NUMBB) = MORT
         IURATE(NUMBB) = MORT
         IIRATE(NUMBB) = MORT
         
C Check whether the Type 2 beetle races are to remain potentially active after the
C current growth cycle.

  888    CONTINUE

         IF (ONLY1X .EQ. 1) NOCOPY = 1

  999 CONTINUE
  
C If the Type 2 beetle races are to remain active, then copy them to the next growth
C cycle.

      IF (NOCOPY .EQ. 1) GOTO 1000
         CALL OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)     
         CALL OPINCR (IY,ICYC,NCYC)
      
 1000 CONTINUE       

      RETURN
      END
