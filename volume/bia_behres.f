      SUBROUTINE BIA_Behres_Hyperobla(dbh, ht, FCLASS,
     & MinBDFTTop, VOL)
C     The source code is from BIA with little modification to work with NVEL
C     2019/04/04 YW Assigned equation number I16BEHW000 for the equation
C     It only work for 16 feet log.     
      IMPLICIT NONE
      REAL dbh, ht, formclass, LogLength, MinBDFTTop,
     & CalcBDFTVolume
      
      REAL MerchHt, VOL(15)

      INTEGER ISEG, numsegs, FCLASS, ILOGLEN
      REAL BD1, BD2, DIB16, FC16
      REAL D, DIB, BD79, RHT, BHT, SEG, XL, CDIB
      REAL tht
      DOUBLE PRECISION BD80

C     --- SPECIFICATION STATEMENTS
      IF(FCLASS.LT.10)THEN
        formclass = 0.0
      ELSE
        formclass = REAL(FCLASS)/100.0
      ENDIF
      LogLength = 16.0
      tht = ht
      FC16 = formclass

C     Note: there used to be a long set of code to calculate
C     Form Class.  This needs to be calculated BEFORE coming
C     to this subroutine

      CalcBDFTVolume = 0.0
      MerchHt = 0.0
      DIB16 = 0.0
      FC16 = 0.0
      IF (formclass.EQ.0.0) formclass = 0.7

      DIB16 = dbh * formclass
      IF (DIB16.GT.dbh) DIB16 = dbh

      CalcBDFTVolume = 0.0
      SEG = 0.0

      DIB = DIB16
      BD79 = 0.0
      BD80 = 0.0
      RHT = 0.0

      IF (DIB.LT.MinBDFTTop) THEN
          RETURN
      ENDIF

C     --- CALCULATE VOLUME OF BUTT LOG
      IF (DIB.LT.8.0) THEN
          BD79 = REAL((-0.083714 + 0.018569 * DIB + 0.059009
     &           * (DIB**2) - 0.003894 * (DIB**3)) * LogLength)
          CalcBDFTVolume = REAL(BD79)
      ELSE
          BD80 = (-0.26875 - 0.12375 * DIB + 0.049375 *
     &           (DIB**2)) * LogLength
          CalcBDFTVolume = REAL(BD80)
      ENDIF

      MerchHt = 17.3

C     --- CALCULATE BHT (HEIGHT FROM TOP OF 1ST LOG TO TOP)
      BHT = REAL(tht - 17.3)
      SEG = 0
      numsegs = NINT(BHT / 16.3)
      DO 10 ISEG = 1, numsegs
          SEG = REAL(SEG + 16.3)
          BD80 = 0
          BD79 = 0
          RHT = BHT - SEG
          IF (RHT.GT.0.0) THEN
              XL = RHT / BHT
              D = REAL(XL / (0.49 * XL + 0.51))

C             ' --- CALCULATE DIB OF CURRENT LOG
              CDIB = DIB * D
              If (CDIB.GE.MinBDFTTop) THEN
                  If (CDIB.LE.8) THEN
                      BD79 = REAL((-0.083714 + 0.018569 * CDIB
     &                       + 0.059009 * (CDIB**2) - 0.003894
     &                       * (CDIB**3)) * LogLength)
                      CalcBDFTVolume = CalcBDFTVolume + BD79
                      BD1 = BD79
                  ELSE
                      BD80 = (-0.26875 - 0.12375 * CDIB +
     &                       0.049375 * (CDIB**2)) * LogLength
                      CalcBDFTVolume = REAL(CalcBDFTVolume
     &                       + BD80)
                      BD2 = REAL(BD80)
                  ENDIF
                  MerchHt = REAL(MerchHt + (ISEG * 16.3))
              ELSE
C                  Segment top diameter less than minimum
C                  add length back on to the log and exit loop
C                                 Print #6, "FULLSEG "; Iseg;
C                                 Print #6, Space(15) & CDIB &
C                                 Print #6, "Segment Skipped"
                  RHT = REAL(RHT + 16.3)
                  EXIT
              ENDIF
          ENDIF
   10 CONTINUE

C     --- CYCLE UP LAST SEGMENT IN 1' JUMPS
      SEG = 1.01875
      LogLength = 0
      BD79 = 0
      BD80 = 0

      numsegs = NINT(RHT / SEG)
      DO 20 ISEG = 1, numsegs
          RHT = RHT - SEG
          LogLength = LogLength + 1.0
          XL = RHT / BHT
          D = REAL(XL / (0.49 * XL + 0.51))
          CDIB = DIB * D
          IF (CDIB.LT.MinBDFTTop) THEN
              IF (CDIB.LT.8.0) THEN
                  BD79 = REAL((-0.083714 + 0.018569 * CDIB
     &                    + 0.059009 * (CDIB**2) - 0.003894
     &                    * (CDIB**3)) * LogLength)
                  CalcBDFTVolume = CalcBDFTVolume + BD79
              ELSEIF (CDIB.GE.8.0) THEN
                  BD80 = (-0.26875 - 0.12375 * CDIB + 0.049375
     &                    * (CDIB**2)) * LogLength
                  CalcBDFTVolume = REAL(CalcBDFTVolume + BD80)
              ENDIF
              MerchHt = MerchHt + (ISEG * SEG)
              EXIT
          ENDIF
  20  CONTINUE
      VOL(2) = CalcBDFTVolume
      END