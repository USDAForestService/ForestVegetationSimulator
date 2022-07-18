      SUBROUTINE RDSPL1(RCROSS,STNEW,IDI,DIAM,RTD)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  SUBROUTINE TO CREATE NEW STUMPS AND DISEASE CENTERS FROM
C  SPORE INFECTED STUMPS THAT HAVE CHANGED DISEASE TYPE
C
C  CALLED BY :
C     RDSPOR  [ROOT DISEASE]
C
C  CALLS     :
C     RDRANN  (FUNCTION)  [ROOT DISEASE]
C
C  PARAMETERS :
C     RCROSS -
C     STNEW  -
C     IDI    -
C     DIAM   -
C     RTD    -
C
C  Revision History :
C   01/01/91 - Last revision date.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'RDPARM.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'RDCOM.F77'

      INCLUDE 'RDADD.F77'
C
COMMONS
C
      INTEGER  II, IDI, JJ, NCROSS
      REAL     DIAM, R, R1, R2, R3, RCROSS, RDRANN, REMCRO, RMX,
     &         RTD, STNEW, XC, YC, XDIF, YDIF, DIS
      
      STNEW = 0.0
      IF (DIAM .LT. SPDBH(IDI)) GOTO 300

      DIMEN = SQRT(SAREA) * 208.7

C
C     GET STUMPS THAT CHANGE DISEASE TYPE
C
      NCROSS = IFIX(RCROSS)
      REMCRO = RCROSS - NCROSS
      R = RDRANN(0)
      IF (R .LT. REMCRO) NCROSS = NCROSS + 1

      IF (NCROSS .LT. 1) RETURN

      DO 350 II=1,NCROSS
C
C        CHECK TO SEE IF STUMP IS INSIDE CENTERS
C
         XC = RDRANN(0) * DIMEN
         YC = RDRANN(0) * DIMEN
         JJ = 0

  400    CONTINUE
         JJ = JJ + 1
         IF (JJ .GT. NCENTS(IDI)) GOTO 600

         XDIF = PCENTS(IDI,JJ,1) - XC
         YDIF = PCENTS(IDI,JJ,2) - YC
         DIS = SQRT((XDIF * XDIF) + (YDIF * YDIF))

         IF (DIS .GT. PCENTS(IDI,JJ,3)) GOTO 400

C
C        STUMP IS INSIDE OF OTHER TYPE'S CENTERS.
C        SO IGNORE IT
C
         GOTO 350

  600    CONTINUE
C
C        STUMP IS OUTSIDE OF OTHER TYPE'S CENTERS 
C        SO IT BECOMES A NEW CENTER

         STNEW = STNEW + 1.0

C        SO ADD IT TO THE PATCH LIST.
C
C        Perhaps IDI should be ID2 (the actual disease type instead of the other) --Sarah

         IF (NCENTS(IDI) .GE. 100) GOTO 550

         NCENTS(IDI) = NCENTS(IDI) + 1
         ICENSP(IDI,NCENTS(IDI)) = 1
         PCENTS(IDI,NCENTS(IDI),1) = XC
         PCENTS(IDI,NCENTS(IDI),2) = YC

C
C        PATCH SIZE (RADIUS) IS 50% OF STUMP ROOT DIAMETER
C
         PCENTS(IDI,NCENTS(IDI),3) = 0.25 * RTD

         GOTO 350

  550    CONTINUE
C
C        ADD PATCH AREA TO LAST PATCH
C
         R1 = PCENTS(IDI,NCENTS(IDI),3)
         R2 = (0.25 * RTD)
         R3 = SQRT((R1 * R1) + (R2 * R2))   

C        Should the following line be .5 * DIMEN instead (half of one side of a square stand)?
C           (If so, also change line in RDSPL2) -- Sarah

         RMX = 2*DIMEN
         IF (R3 .LT. RMX) PCENTS(IDI,NCENTS(IDI),3) = R3
  350 CONTINUE

  300 CONTINUE

      RETURN
      END
