      SUBROUTINE RDCSD(ISPI,DBHLIM,THRESH,CRIT)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C    CALCULATES THE CRITICAL STAND DENSITY THAT DETERMINES WHETHER OR 
C    NOT AN OUTBREAK OF BARK BEETLE 1 OCCURS.
C    Depending on the arguments supplied to the keyword DNSCALC, the user
C    can select from the following to define the critical density:
C
C    METHOD: STEM or SDI
C    SPACE:  ALL or OUTSDE
C    DTYPE:  LIVE or ALL
C
C    where:
C
C    STEM    is stems/acre greater than the DBHLIM supplied
C    SDI     is Reinecke's Stand Density Index.  This method sums host +
C            non-host trees if SPACE ALL is selected.  If SPACE OUTSDE is
C            selected, only BB host trees outside root rot centers are
C            included.  In neither case is DBHLIM used to screen out trees.
C    ALL     for SPACE, includes the whole stand area, both inside and
C             outside of root rot infection centers.
C            for DTYPE, includes all live trees (whether infected or not), 
C             AND all trees that died during the last two years (whether
C             infected or not, and without regard to the cause of death). 
C    OUTSDE  includes only areas outside root rot-infected areas
C    LIVE    includes only living trees (both infected and not infected).
C
C    The meaning of THRESH depends upon the METHOD; it can be either 
C    stems/acre or the SDI value.
C
C  CALLED BY :
C     RDBB1  [ROOT DISEASE]
C
C  CALLS     :
C     DBCHK  (SUBROUTINE)   [PROGNOSIS]
C
C  PARAMETERS :
C     ISPI - the species targetted by the beetle      (INTEGER)
C     DBHLIM - the dbh limit when the METHOD is STEM  (REAL)
C     THRESH - the threshold stems/acre or SDI value  (REAL)
C     CRIT - exceeds critical value for outbreak      (LOGICAL)
C
C  VARIABLES
C     TOTDEN = the TOTal DENsity over the whole stand of trees that
C              meet the user's criteria
C     NSUM   = Number SUM - a sum of numbers of trees that meet the
C              user's criteria
C     DOSUM  = Density Outside SUM - a sum of the density outside centers
C              of trees that meet the user's criteria
C     BASUM  = Basal Area SUM - the total basal area in the stand of trees
C              that meet the user's criteria
C     PIBY4  = PI divided BY 4, used to save work in iterative computation
C     PIABY4 = PI * stand Area divided BY 4, used to save work
C
C  Revision History :
C     06/12/96  Matthew K. Thompson
C               Moved the declaration of DSO, DSII, and DSIU to the
C               parameter include file RDPARM.
C     21-MAR-00 Lance David (FHTET)
C               Changed local variable "SLOPE" to "REISLP" because SLOPE
C               is a FVS variable in common PLOT.F77. 
C     15-JUL-02 Lance David (FHTET)
C               Added debug.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'PLOT.F77'
C
COMMONS
C
      INTEGER  ALLX, DTYPE, I, I1, I2, ISPI, J, LIVE, METHOD,
     &         OUTSDE, RRTYPE, SDI, SPACE, STEM

      REAL     ASD, BASUM, DBHLIM, DOSUM, NSUM, PIBY4, PIABY4,
     &         REISLP, RRFREE, SDIVAL, TEMP, THRESH, TOTDEN

      LOGICAL  CRIT, DEBUG

C
C    Index conventions for arrays and logic
C
      DATA ALLX     /0 /
      DATA OUTSDE   /1/

      DATA LIVE     /1/

      DATA STEM     /1/
      DATA SDI      /2/
C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDCSD',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDCSD: ISPI=',ISPI,
     &   ' DBHLIM=', DBHLIM,'THRESH=',THRESH,' CRIT=',CRIT


C REINEK(1) triggers the METHOD. If REINEK(1) is zero, then the METHOD
C is STEMS; otherwise it is SDI. REINEK(2) triggers SPACE. If
C REINEK(2) is zero, SPACE is ALL; otherwise it is OUTSDE. REINEK(3)
C triggers the DTYPE. If REINEK(2) is zero, DTYPE is LIVE and does
C not include recently dead trees in the density calculation; otherwise, it
C is ALL and trees that died in the last cycle are included.
C REINEK(4) optionally triggers a change in the SLOPE (Error check in
C subroutine 'rrin').

C Default options for critical density calculations.

      METHOD = STEM
      SPACE = ALLX
      DTYPE = LIVE

      IF (REINEK(1) .EQ. 0.0) GOTO 4
      METHOD = SDI

    4 CONTINUE
      IF (REINEK(2) .EQ. 0.0) GOTO 5
      SPACE = OUTSDE

    5 CONTINUE
      IF (REINEK(3) .EQ. 0.0) GOTO 6
      DTYPE = ALLX

    6 CONTINUE
      REISLP = REINEK(4)
      
      CRIT = .FALSE.
      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: METHOD=',METHOD,' SPACE=',
     &           SPACE,' DTYPE=',DTYPE,' REISLP=',REISLP
     
C Find out what kind of fungus infects the host species
      RRTYPE = MAXRR
      IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: RRTYPE=',RRTYPE,
     &   ' PAREA=',PAREA(RRTYPE),' SAREA=',SAREA

      IF ((SPACE .EQ. OUTSDE) .AND. (PAREA(RRTYPE) .GE. SAREA)) GOTO 778

C Branch to 200 if the Reineke Stand Density Index is being used.

      IF (METHOD .EQ. SDI) GOTO 200

C *******************************************************************
C  METHOD 1:
C  Use the "Eligible Stems/Acre" method for determining if the outbreak
C  threshold density has been passed.
C *******************************************************************
      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: CALC METHOD 1 - STEMS'

C Find the first and last tree records of the host species.
      I1 = ISCT(ISPI,1)
      I2 = ISCT(ISPI,2)

C Look at every record of the species, and, for those that exceed
C the dbh limit, count the portions of the record that meet the user's
C criteria.

      NSUM = 0.0
      DOSUM = 0.0

      DO 100 J=I1,I2
         I = IND1(J)
         IF (DBH(I) .LT. DBHLIM) GOTO 100
         
C        count live stems outside
         DOSUM = DOSUM + FPROB(I)
         
C        add recent dead stems outside 
         IF (DTYPE .EQ. ALLX) NSUM = NSUM + ((2/FINT) * PROAKL(DSO,I))
         
         IF (SPACE .EQ. ALLX) THEN
C           add live stems inside
            NSUM = NSUM + PROBIT(I) + PROBIU(I)
            
C           add recent dead stems inside
            IF (DTYPE .EQ. ALLX) NSUM = NSUM + ((2/FINT)
     &                  * (PROAKL(DSII,I) + PROAKL(DSIU,I) + PRANKL(I)))
         END IF
  100 CONTINUE

C Calculate TOTDEN, compare it to the user's threshold, and use CRIT to signal
C whether the threshold was surpassed.
      IF (SPACE .EQ. ALLX) THEN
         TOTDEN = (NSUM + DOSUM*(SAREA - PAREA(RRTYPE))) / SAREA
      ELSE
         TOTDEN = DOSUM + NSUM / (SAREA - PAREA(RRTYPE))
      END IF
      
      IF (TOTDEN .GE. THRESH) CRIT = .TRUE.

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: DOSUM=',DOSUM,
     &   ' NSUM=',NSUM,' TOTDEN=',TOTDEN,' CRIT=',CRIT

      GOTO 778
       

  200 CONTINUE 
  
C ************************************************************************
C  METHOD 2:
C  Calculate threshold densities by Reinecke's SDI method.
C ************************************************************************
C
C The model needs the Average Stand Diameter (ASD) of all trees that
C meet the user's criteria (live vs. dead, host-outside-centers vs.
C all-species-everywhere). The first step is to find the total basal
C area of these trees in the stand.
C Basal Area of each tree = pi * (DBH/2)^2.
C PIBY4 and PIABY4 are used to save work in the iterative computations.
C NOTE:  THERE IS SOME ERROR involved in using DBH(I) as the dbh of both
C live and dead trees in the record.

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: CALC METHOD 2 - SDI'

      NSUM = 0.0
      BASUM = 0.0
      PIBY4 = 0.785398163
      PIABY4 = PIBY4 * SAREA
      
      IF (SPACE .EQ. OUTSDE) GOTO 301
      
C If SPACE = ALL, consider all tree species everywhere:

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: SPACE = ALL'

      DO 300 I = 1,ITRN
      
C        start with live stems
         NSUM = NSUM + PROBL(I) * SAREA
         BASUM = BASUM + PROBL(I) * PIABY4 * DBH(I)**2
         
C        add recent dead stems
         IF (DTYPE .EQ. ALLX) THEN
            TEMP = (2/FINT) * 
     &           (PROAKL(DSO,I)+PROAKL(DSII,I)+PROAKL(DSIU,I)+PRANKL(I))
            NSUM = NSUM + TEMP
            BASUM = BASUM + TEMP * PIBY4 * DBH(I)**2
         END IF
  300 CONTINUE
      TOTDEN = NSUM / SAREA
      GOTO 401
      
C If SPACE = OUTSDE, consider only trees outside the disease centers of either root
C rot type.  This procedure assumes that there is no overlap in the two types of disease
C centers, but this assumption does not create any error since the end result is a
C density of trees (not a number).  Note that the model assumes that root rot centers do
C not affect the density of non-host trees.

  301 CONTINUE

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: SPACE = OUTSIDE'

      RRFREE = SAREA
      DO 340 RRTYPE=MINRR,MAXRR
         RRFREE = RRFREE - PAREA(RRTYPE)
  340 CONTINUE       
      
      DO 400 I = 1,ITRN
         
C        start with live stems outside
         TEMP = FPROB(I) * RRFREE
         NSUM = NSUM + TEMP
         BASUM = BASUM + (TEMP * PIBY4 * DBH(I)**2)
         
C        add recent dead stems outside 
         IF (DTYPE .EQ. ALLX) THEN
            RRTYPE = MAXRR
            IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISP(I)))
            TEMP = (2/FINT) * PROAKL(DSO,I) 
     &                      * RRFREE / (SAREA - PAREA(RRTYPE)) 
            NSUM = NSUM + TEMP
            BASUM = BASUM + TEMP * PIBY4 * DBH(I)**2
         END IF
  400 CONTINUE
      TOTDEN = NSUM / RRFREE
  
  401 CONTINUE
  
C Calculate the ASD of trees that met the user's criteria from the total basal area
C of these trees:  total BA = #trees * pi * (ASD/2)^2.  Use ASD and TOTDEN to find 
C the SDIVALue of the stand from a standard equation.  Compare SDIVAL to the threshold
C and use CRIT to signal whether the conditions for an outbreak have been met.
      
      IF (NSUM .GT. 0.0) THEN
        ASD = SQRT(BASUM / (NSUM * PIBY4))
        SDIVAL = 10.0**(LOG10(TOTDEN) - (REISLP * LOG10(ASD)) + REISLP)
      
        IF (SDIVAL .GE. THRESH) CRIT = .TRUE.
      END IF

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDCSD: NSUM=',NSUM,' ASD=',ASD,
     &           ' SDIVAL=',SDIVAL,' CRIT=',CRIT

  778 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT: RDCSD'

      RETURN
      END
