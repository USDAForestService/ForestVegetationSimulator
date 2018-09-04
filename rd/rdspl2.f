      SUBROUTINE RDSPL2(RCEN,IDI,DIAM,RTD)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  CREATES NEW DISEASE CENTERS FOR SPORE INFECTED
C  STUMPS THAT ARE OUTSIDE OF EXISTING CENTERS AND DON'T CHANGE
C  DISEASE TYPE.
C
C  CALLED BY :
C     RDSPOR  [ROOT DISEASE]
C
C  CALLS     :
C     RDRANN  (FUNCTION)  [ROOT DISEASE]
C
C  PARAMETERS :
C     RCEN   -
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

      INTEGER IDI, II, JJ, NCEN, NTRY
      REAL    DIAM, DIS, R, R1, R2, R3, RCEN, RDRANN, REMCEN,
     &        RMX, RTD, XC, XDIF, YC, YDIF
      
      IF (DIAM .LT. SPDBH(IDI)) GOTO 300

      DIMEN = SQRT(SAREA) * 208.7

C
C     GET NUMBER OF CENTERS TO CREATE.
C
      NCEN = IFIX(RCEN)
      REMCEN = RCEN - NCEN
      R = RDRANN(0)
      IF (R .LT. REMCEN) NCEN = NCEN + 1

      IF (NCEN .LT. 1) RETURN

      DO 350 II=1,NCEN
C
C        KEEP TRYING UNTIL THE CENTER IS LOCATED OUTSIDE.
C
         NTRY = 0

  380    CONTINUE
         NTRY = NTRY + 1

         IF (NTRY .GT. 20) GOTO 550

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

         GOTO 380

  600    CONTINUE
C
C        STUMP IS OUTSIDE OF OWN TYPE'S CENTERS
C        SO ADD IT TO THE PATCH LIST.
C
         IF (NCENTS(IDI) .GE. 100) GOTO 550

         NCENTS(IDI) = NCENTS(IDI) + 1
         ICENSP(IDI,NCENTS(IDI)) = 1 
         JCENSP(IDI,NCENTS(IDI)) = IYEAR
         PCENTS(IDI,NCENTS(IDI),1) = XC
         PCENTS(IDI,NCENTS(IDI),2) = YC

C
C        PATCH SIZE (RADIUS) IS 50% OF STUMP ROOT RADIUS
C
         PCENTS(IDI,NCENTS(IDI),3) = 0.5 * RTD

         GOTO 350

  550    CONTINUE
C
C        ADD PATCH AREA TO LAST PATCH IF THERE ARE ALREADY 100 CENTERS
C
         R1 = PCENTS(IDI,NCENTS(IDI),3)
         R2 = (0.5 * RTD)
         R3 = SQRT((R1 * R1) + (R2 * R2))
         RMX = 2 * DIMEN
         IF (R3 .LT. RMX) PCENTS(IDI,NCENTS(IDI),3) = R3
  350 CONTINUE

  300 CONTINUE

      RETURN
      END
