      SUBROUTINE VolEq_Johnson(dbh, ht, FCLASS, VOL)

      IMPLICIT NONE
      
      INTEGER ispc, FCLASS
      REAL dbh, ht, formclass, LogLength, MinCUFTDBH, 
     & MinCUFTTop, strVolEq, CalcCUFTVolume, VOL(15)

      REAL UPPER, BUTT, C, C3, B, A

      REAL FC16
      REAL DIB16, MerchHt

C     SPECIFICATION STATEMENTS
      REAL DIB, BD79
      DOUBLE PRECISION BD80
      REAL D, RHT, SEG

C     CALCULATE CU FT VOL WITH JOHNSON'S FORM CLASS FORMULA

C     Note: there used to be a long set of code to calculate Form
C     Class.  This needs to be calculated BEFORE coming to this 
C     subroutine
      MinCUFTDBH = 1.0
      IF(FCLASS.LT.10)THEN
        formclass = 0.0
      ELSE
        formclass = REAL(FCLASS)/100.0
      ENDIF
      FC16 = formclass

      CalcCUFTVolume = 0
      MerchHt = 0
      DIB16 = 0
      FC16 = 0
      IF (formclass.EQ.0) formclass = 0.72

      DIB16 = dbh * formclass
      IF (DIB16.GT.dbh) DIB16 = dbh

      CalcCUFTVolume = 0.0
      SEG = 0.0
      DIB = DIB16
      BD79 = 0
      BD80 = 0
      RHT = 0

      A = 0
      B = 0
      C3 = 0
      C = 0
      D = 0
      BUTT = 0.0
      UPPER = 0.0
      CalcCUFTVolume = 0.0
      IF (dbh.LT.MinCUFTDBH) THEN
          CalcCUFTVolume = 0
          RETURN
      ELSEIF (dbh.LT.6) THEN
          BUTT = REAL(((dbh - 4.0) * 0.5) * (-4.68329 
     &             + (18.7668 * formclass) 
     &             - (12.18 * (formclass**2))))
      ELSE
          BUTT = REAL(8.436 - 2.608 * dbh + 0.070242 * (dbh**2) 
     &            + 3.1278 * DIB16 - 12.18 * (formclass**2))
      ENDIF
      IF (DIB16.LT.4.0 .OR. ht.LE.17) THEN
          CalcCUFTVolume = BUTT
      ELSE
          A = REAL((ht - 17.0) * (DIB16**2) * (0.022716))
          B = REAL((DIB16 - 4.0) / (DIB16 - 1.96))
          C3 = REAL(((DIB16 - 1.96)**2) / (DIB16**2))
          C = REAL(LOG(C3))
          D = REAL((0.51 * (DIB16 - 4.0)) / (DIB16))
          UPPER = REAL((A) * (B - 1.401656 - (1.040816 * C)
     &             + D))
          CalcCUFTVolume = BUTT + UPPER
      ENDIF
      VOL(4) = CalcCUFTVolume
      END