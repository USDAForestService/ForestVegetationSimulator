      SUBROUTINE BCVTS(SP, DBH, HT, VOL)
C
      REAL HT,DBH,VOL
      CHARACTER*4 SP
C----------------------------------------------------------------------
C
C Calculates cubic foot volume (bole, top and stump) for given DBH and
C HT in english units.
C
C----------------------------------------------------------------------
C
C SET COEFFICIENTS BY SPECIES. DEFAULT TO DF.
      IF (SP.EQ.'PP') THEN
         b0=-2.35820
         b1=1.87254
         b2=0.89396
      ELSE IF(SP.EQ.'DF') THEN
         b0=-2.40258
         b1=1.82212
         b2=0.95883
      ELSE IF (SP.EQ.'WL') THEN
         b0=-2.39117
         b1=1.95789
         b2=0.86407
      ELSE IF (SP.EQ.'LP') THEN
         b0=-2.53758
         b1=1.87765
         b2=1.03312
      ELSE
         b0=-2.40258
         b1=1.82212
         b2=0.95883
      END IF
C Calculate volume of bole in cubic feet
      VOL=(10**b0)*(DBH**b1)*(HT**b2)
      RETURN
      END
