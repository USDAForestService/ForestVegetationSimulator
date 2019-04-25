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
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ESPARM.F77'

      INCLUDE 'ESHAP.F77'
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------

      INTEGER II

      REAL BB,HHT,TIME,WMAX,X

C----------
C ASSIGN HEIGHTS OF TALLEST EXCESS TREES BASED ON SPECIES INDEX/NUMBER
C----------

      SELECT CASE (II)

        CASE (1)
C         ----------
C          1     SF   Pacific silver fir
C         ----------
          HHT = 1.0

        CASE (2)
C         ----------
C          2     AF   subalpine fir
C         ----------
          HHT = 1.0

        CASE (3)
C         ----------
C          3     YC   Alaska cedar
C         ----------
          BB = -0.26203 + 0.44249*TIME
   31     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 31
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 61
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 61
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB

        CASE (4:7)
C         ----------
C          4     TA   tamarack
C          5     WS   white spruce
C          6     LS   Lutz’s spruce
C          7     BE   black spruce
C         ----------
          HHT = 1.0

        CASE (8)
C         ----------
C          8     SS   Sitka spruce
C         ----------
          BB = -0.26203 + 0.44249*TIME
   81     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 81
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 81
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 81
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB

        CASE (9)
C         ----------
C          9     LP   lodgepole pine
C         ----------
          HHT = 1.0

        CASE (10)
C         ----------
C         10     RC   western redcedar
C         ----------
          BB = -0.26203 + 0.44249*TIME
  101     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 101
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 21
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 21
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB

        CASE (11)
C         ----------
C         11     WH   western hemlock
C         ----------
          BB = -0.26203 + 0.44249*TIME
  111     CALL ESRANN(X)
          IF(X .GT. WMAX) GO TO 111
          IF(NTALLY.EQ.1 .AND. X.LT.0.8) GO TO 51
          IF(NTALLY.EQ.2 .AND. X.GE.0.5) GO TO 51
          HHT = ((-(ALOG(1.0-X)))**(1.0/1.195))*BB

        CASE (12)
C         ----------
C         12     MH   mountain hemlock
C         ----------
          HHT = 1.0

        CASE (13)
C         ----------
C         13     OS   other softwoods
C         ----------
          HHT = 1.0

        CASE (14:23)
C         ----------
C         14     AD   alder species
C         15     RA   red alder
C         16     PB   paper birch
C         17     AB   Alaska birch
C         18     BA   balsam poplar
C         19     AS   quaking aspen
C         20     CW   black cottonwood
C         21     WI   willow species
C         22     SU   Scouler’s willow
C         23     OH   other hardwoods
C         ----------
          HHT = 1.0

      END SELECT
C----------
C  HEIGHTS TO TALL, TEMPORARY FIX, 11-30-93  GD
C----------
      HHT=HHT*0.25
      IF(HHT.LT.1.0)HHT=1.0
C
      RETURN
      END
