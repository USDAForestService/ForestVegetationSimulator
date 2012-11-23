      SUBROUTINE ONSTHG(HTG, ISPC, D, HT, SI, BAL)
      IMPLICIT NONE
C----------
C  **ONSTHG--ON    DATE OF LAST REVISION:  24-Sep-2011
C----------
C  THIS SUBROUTINE COMPUTES ANNUAL HEIGHT INCREMENTS FOR
C  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS INTNDED FOR TREES
C  THAT ARE LESS THAN 12 CM DBH
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'METRIC.F77'

C     OUTPUT:
C     HTG  = REAL annual height growth (ft) 
C
C     INPUT:
C     ISPC = INT Species # (MAXSP)
C     D    = REAL Current diameter (in)
C     HT   = REAL Current height (ft)
C     SI   = REAL Site Index (ft)
C     BAL  = REAL Basal area of trees larger than "me" (ft**2/ac)

C     NUMBER OF SMALL TREE HEIGHT GROWTH EQUATIONS

      INTEGER   MAXEQ
      PARAMETER (MAXEQ=28)

      INTEGER  ISPC
      REAL     HTG, D, HT, SI, BAL

      INTEGER  KSP
      REAL     BACON, HM, LHM, SIM, BALM

	INTEGER  OSPMAP(MAXSP)
	REAL     B00(MAXEQ),B01(MAXEQ),B02(MAXEQ),BSI(MAXEQ)
      REAL     BBAL(MAXEQ),B95(MAXEQ)

      DATA BACON  /0.005454154/

C     MAPPING OF FVS SPECIES INTO ONTARIO GROWTH SPECIES.

      DATA OSPMAP/
     >   5,  5,  3,  4,  1,  7,  8, 12,  9, 14,  !10
     >  13, 11, 28, 28, 26, 26, 25, 27, 16, 23,  !20
     >  26, 26, 26, 17, 26, 15, 27, 21, 26, 26,  !30
     >  26, 26, 26, 19, 26, 26, 25, 25, 25, 25,  !40
     >  24, 25, 18, 26, 26, 26, 22, 25, 26, 27,  !50
     >  27, 27, 22, 26, 27, 27, 27, 26, 26, 26,  !60
     >  26, 26, 26, 25, 25, 25, 26, 26,  6,  2,  !70
     >   8, 10 /

      DATA B00 /
     >  -1.9993, -1.3753, -2.0527, -1.3663, -1.4402,
     >  -1.4816, -1.5004, -1.3797, -1.8352, -1.6827,
     >  -2.7735, -2.1128, -2.7253, -2.4774, -2.9713,
     >  -1.289,   0.0,    -1.1884,  0.0,     0.0,
     >  -3.7302, -6.1703, -3.4377, -2.1056, -2.1276,
     >  -0.7405, -2.9186, -2.5740 /

      DATA B01 /
     >   0.6348,  0.6348,  0.8363,  0.8363,  0.4319,
     >   0.4319,  0.7043,  0.7043,  0.6551,  0.6551,
     >   1.0825,  1.1889,  2.6865,  0.9273,  0.9087,
     >   0.0482,  0.0,     0.7150,  0.0,     0.0,
     >   3.4475,  6.0601,  3.5856,  0.8751,  0.9495,
     >   0.6671,  0.8679,  0.6756 /

      DATA B02 /
     >  -0.1190, -0.1190, -0.1793, -0.1793, -0.1252,
     >  -0.1252, -0.1630, -0.1630, -0.1920, -0.1920,
     >  -0.1382, -0.1941, -0.6667, -0.1098, -0.0982,
     >   0.0,    -0.0581, -0.1922, -0.0720, -0.0720,
     >  -0.4647, -0.9504, -0.5959, -0.1106, -0.1240,
     >  -0.1839, -0.0901, -0.1927 /

      DATA BBAL /
     >  -0.0106, -0.0106, -0.0101, -0.0101, -0.0346,
     >  -0.0346, -0.0604, -0.0604, -0.0303, -0.0303,
     >   0.0,    -0.0142, -0.0140,  0.0,    -0.0655,
     >  -0.0324, -0.0994, -0.0227, -0.1127, -0.1127,
     >  -0.0717,  0.0,     0.0,    -0.0526, -0.0722,
     >  -0.0474, -0.0633, -0.0742 /

      DATA BSI /
     >   0.0033,  0.0033,  0.0212,  0.0212,  0.0379,
     >   0.0379,  0.0164,  0.0164,  0.0372,  0.0372,
     >   0.0,     0.0,     0.0,     0.0,     0.0495,
     >   0.0,     0.0,     0.0,     0.0,     0.0,
     >   0.0,     0.0,     0.0,     0.0152,  0.0148,
     >   0.0,     0.0463,  0.0916 /

      DATA B95 /
     >   0.5200,  0.5200,  0.7315,  0.7315,  0.5725,
     >   0.5725,  0.5100,  0.5100,  0.3340,  0.3340,
     >   0.1800,  0.4886,  0.3189,  0.5500,  0.5300,
     >   0.5080,  0.6000,  0.5320,  0.4260,  0.4260,
     >   0.4600,  0.6620,  0.8300,  0.6860,  0.6860,
     >   0.6000,  0.5300,  0.3340 /

      ! need SI  BAL

      HM   = MAX(0.05, HT * FTtoM)
      LHM  = LOG(HM)
	SIM  = SI * FTtoM
      BALM = BAL * FT2pACRtoM2pHA

      KSP = OSPMAP(ISPC)

      HTG = 
     >  +  B00(KSP)
     >  + (B01(KSP)  * LHM)
     >  + (B02(KSP)  * HM)
     >  + (BSI(KSP)  * SIM)
     >  + (BBAL(KSP) * BALM)

C     JUST TO BE SAFE, CONSTRAIN PREDICTED DG TO +/- 5 BEFORE
C     EXPONENTIATION; THEN CONSTRAIN THE ANNUAL GROWTH TO
C     0.001 - 95TH% FROM B95()

      HTG = MIN(MAX(HTG,-5.0),5.0)
      HTG = EXP(HTG)
      HTG = MIN(MAX(HTG,0.0001),B95(KSP))
      HTG = HTG * MtoFT

      RETURN
      END
