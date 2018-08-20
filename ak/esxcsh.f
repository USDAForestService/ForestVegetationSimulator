      SUBROUTINE ESXCSH (TIME,II,HHT,WMAX)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESHAP.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER II
C
      REAL BB,HHT,TIME,WMAX,X
C
C----------
      SELECT CASE (II)
C----------
C  HEIGHT OF TALLEST EXCESS WHITE PINE.
C----------
        CASE (1)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS WESTERN RED CEDAR.
C----------
        CASE (2)
          BB = -0.26203 + 0.44249*TIME
   21     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 21
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 21
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 21
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
C----------
C  HEIGHT OF TALLEST EXCESS PACIFIC SILVER FIR.
C----------
        CASE (3)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS MOUNTAIN HEMLOCK.
C----------
        CASE (4)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS WESTERN HEMLOCK.
C----------
        CASE (5)
          BB = -0.26203 + 0.44249*TIME
   51     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 51
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 51
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 51
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
C----------
C  HEIGHT OF TALLEST EXCESS ALASKA CEDAR.
C----------
        CASE (6)
          BB = -0.26203 + 0.44249*TIME
   61     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 61
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 61
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 61
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
C----------
C  HEIGHT OF TALLEST EXCESS LODGEPOLE PINE.
C----------
        CASE (7)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS SITKA SPRUCE.
C----------
        CASE (8)
          BB = -0.26203 + 0.44249*TIME
   81     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 81
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 81
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 81
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB
C----------
C  HEIGHT OF TALLEST EXCESS SUBALPINE FIR.
C----------
        CASE (9)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS HARDWOOD.
C----------
        CASE (10,11,12)
          HHT = 1.0
C----------
C  HEIGHT OF TALLEST EXCESS OTHER SPECIES.
C----------
        CASE (13)
          HHT = 1.0
C
      END SELECT
C----------
C  HEIGHTS TO TALL, TEMPORARY FIX, 11-30-93  GD
C----------
      HHT=HHT*0.25
      IF(HHT.LT.1.0)HHT=1.0
C
      RETURN
      END
