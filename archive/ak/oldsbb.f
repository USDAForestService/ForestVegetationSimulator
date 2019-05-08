      SUBROUTINE SBB(ISPC,D,H,Z,IFLAG)
      IMPLICIT NONE
C----------
C AK $Id: sbb.f 2472 2018-08-20 21:22:34Z gedixon $
C----------
C
C THIS ROUTINE LOOKS UP A Z GIVEN SPECIES, DBH, AND HT.
C IT ALSO LOOKS UP A HEIGHT GIVEN SPECIES, DBH, AND Z.
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER IFLAG,ISPC,JSPC
C
      REAL D,FBY1,FBY2,H,PSI,XI1,XI2,Y1,Y2,Z
C
      REAL COF(9,11)
C
C----------
C  DATA STATEMENTS:
C
C  SPECIES ORDER
C  1    2    3    4    5    6    7    8    9   10   11  12  13
C WS  WRC  PSF   MH   WH  AYC   LP   SS   SAF  RA   CW  OH  OS
C----------
      DATA COF/
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
C
C WESTERN HEMLOCK
C
     &      70.0,     200.0,   1.53035,    .77779,   1.17256,   1.07719,
     &    .87280,   1.67853,    .95008,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
C
C SITKA SPRUCE
C
     &      70.0,     220.0,   1.10228,    .72391,    .99652,    .93681,
     &    .86387,   1.27598,    .91893,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073,
     &  80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,
     &    .94891,   1.88756,   1.00073/
C
C----------
      JSPC=ISPC
      IF(ISPC .EQ. 12) JSPC=10
      IF(ISPC .EQ. 13) JSPC=11
C
      XI2=4.5
      XI1=0.1
      IF(IFLAG .EQ. 1)THEN
         IF (H.LE. 4.5 .OR. D .LE. .1) RETURN
         IF((XI1 + COF(1,JSPC)) .LE. D) GO TO 180
         IF((XI2 + COF(2,JSPC)) .LE. H) GO TO 180
        GO TO 190
  180    CONTINUE
        Z=0.0
        RETURN
  190    CONTINUE
        Y1=(D - XI1)/COF(1,JSPC)
        Y2=(H - XI2)/COF(2,JSPC)
        FBY1=ALOG(Y1/(1.0 - Y1))
        FBY2= ALOG(Y2/(1.0 - Y2))
        Z=( COF(4,JSPC) + COF(6,JSPC)*FBY2 - COF(7,JSPC)*( COF(3,JSPC) +
     +     COF(5,JSPC)*FBY1))*(1.0 - COF(7,JSPC)**2)**(-0.5)
          RETURN
      ELSE
        IF(D .LE. 0.1)RETURN
         IF((XI1 + COF(1,JSPC)) .LE. D)  THEN
                    H=COF(2,JSPC)
                    RETURN
          END IF
C----------
C  CALCULATE HEIGHT AFTER 10 YEARS
C----------
      PSI= COF(8,JSPC)*((D-XI1)/(XI1+ COF(1,JSPC) - D))**COF(9,JSPC)
     +     * (EXP(Z*((1.0 - COF(7,JSPC)**2  ))**0.5/COF(6,JSPC)))
        H= ((PSI/(1.0 + PSI))* COF(2,JSPC)) + XI2
C
        RETURN
      END IF
      END
