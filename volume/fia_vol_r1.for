! This is from FIA package code NIMS_VOL_R1
! Created by YW 2018/08/30
! ---------------------------------------------------------------------
! Faurot, J.L. 1977 Estimating merchantable volume and stem residue in four timber species:
! Ponderosa pine, Lodgepole pine, Western larch, Douglas-fir
! USDA Forest service Research Paper INT-196, November 1977
! CU000014 CVTS FUNCTION FAUROT_CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000015 TIP2 FUNCTION FAUROT_2CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000016 TIP3 FUNCTION FAUROT_3CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000017 TIP4 FUNCTION FAUROT_4CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000018 TIP5 FUNCTION FAUROT_5CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000019 TIP6 FUNCTION FAUROT_6CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000020 TIP7 FUNCTION FAUROT_7CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000021 TIP8 FUNCTION FAUROT_8CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! VALID SPECIES CODE: 073, 108, 122, 202
! NVEL EQUATION NUMBER:
! R01FAU0073, R01FAU0108, R01FAU0122, R01FAU0202, 
      SUBROUTINE FAUROT_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,MTOPP,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,BFMIND,MTOPP,VOL(15)
      INTEGER ERRFLG,SPN,IDX,I,J
      REAL PPCOEF(7,4),DFCOEF(7,4),WLCOEF(7,4),LPCOEF(7,4),COEF(7,4)
      REAL CVTS,CVT,TIP4,TIPV
      REAL STUMPD,STUMPV,DBH,THT,A,B,C,D,E,BH,TOPD
      DATA ((PPCOEF(J,I), I=1,4), J=1,7) /
     & 2, -.460586, -2.09688, .733094,
     & 3, 0.299133, -2.22255, .699746,
     & 4, 0.842466, -2.13641, .590700,
     & 5, 1.264030, -2.20809, .589800,
     & 6, 1.805850, -2.68641, .719723,
     & 7, 2.295140, -3.06128, .800789,
     & 8, 2.592260, -3.26371, .865846/
      DATA ((WLCOEF(J,I), I=1,4), J=1,7) /
     & 2, -.218386, -2.61784, .900808,
     & 3, 0.574917, -3.05340, 1.00746,
     & 4, 1.20417, -3.35498, 1.06157,
     & 5, 1.71647, -3.44297, 1.02285,
     & 6, 2.11511, -3.43175, 0.958133,
     & 7, 2.51253, -3.67288, 1.01315,
     & 8, 2.89616, -4.02373, 1.11914/
      DATA ((DFCOEF(J,I), I=1,4), J=1,7) /
     & 2, -.57482, -1.79704, .63400,
     & 3, 0.22840, -2.13544, .68846,
     & 4, 0.77690, -2.19153, .64755,
     & 5, 1.27970, -2.37428, .66145,
     & 6, 1.69009, -2.58148, .70505,
     & 7, 2.02163, -2.68436, .71282,
     & 8, 2.45281, -3.08509, .82531/
      DATA ((LPCOEF(J,I), I=1,4), J=1,7) /
     & 2, -.365736, -2.46962, .86752,
     & 3, .376276, -2.49626, .779565,
     & 4, .860507, -2.48286, .741235,
     & 5, 1.29058, -2.52686, .720985,
     & 6, 1.868380, -2.97158, .810267,
     & 7, 2.362290, -3.45108, 0.943681,
     & 8, 2.709090, -3.50204, 0.905578/
      V1(DBH,THT,A,B,C,D)=10**(A +B*LOG10(DBH)+C*THT+D*THT*LOG10(DBH))
      V2(BH,A,E)=10**(A + E*LOG10(BH))
      TIP(DBH,THT,A,B,C)=10**(A+B*LOG10(DBH)+C*LOG10(DBH)*LOG10(THT))
      ERRFLG = 0
      VOL = 0.0
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(MTOPP.LT.0.1) MTOPP = 4.0
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      TOPD = NINT(MTOPP)
      DBH = DBHOB
      THT = HTTOT
      IF(SPN.EQ.122)THEN
        A = -1.52198
        B = 2.12327
        C = 0.0138002
        D = -0.00561155
        CVTS = V1(DBH,THT,A,B,C,D)
        STUMPD = (DBH+0.186432)/1.00729
        COEF = PPCOEF
      ELSEIF(SPN.EQ.202)THEN
        A = -1.48037
        B = 2.11615
        C = 0.0137202
        D = -.00563672
        CVTS = V1(DBH,THT,A,B,C,D)
        STUMPD = (DBH+0.126425)/0.965427
        COEF = DFCOEF
      ELSEIF(SPN.EQ.73)THEN
        A = -.342917
        E = 0.948258
        BH = 3.141592/144.0*0.25*DBH*DBH*THT
        CVTS = V2(BH,A,E)
        STUMPD = (DBH+0.253227)/1.03641
        COEF = WLCOEF
      ELSEIF(SPN.EQ.108)THEN
        A = -.2771
        E = 0.963354
        BH = 3.141592/144.0*0.25*DBH*DBH*THT
        CVTS = V2(BH,A,E)
        STUMPD = (DBH+0.292117)/0.873569
        COEF = LPCOEF
      ELSE
        ERRFLG = 6
        RETURN
      ENDIF
!     STUMP VOL 
      STUMPV = 0.005454*STUMPD**2     
      VOL(14) = STUMPV
      VOL(1) = CVTS - STUMPV
!     CALC CV4
      IDX = 3
      A = COEF(IDX,2)
      B = COEF(IDX,3)
      C = COEF(IDX,4)
      TIP4 = TIP(DBH,THT,A,B,C)
      VOL(15) = TIP4
      VOL(4) = VOL(1) - TIP4     
      IF(TOPD.GT.8) TOP = 8
      IF(TOPD.GE.2.AND.TOPD.LE.8.AND.TOPD.NE.4)THEN
        IDX = TOPD - 1
        A = COEF(IDX,2)
        B = COEF(IDX,3)
        C = COEF(IDX,4)
        TIPV = TIP(DBH,THT,A,B,C)
        VOL(15) = TIPV
        VOL(4) = VOL(1) - TIPV
        IF(TOPD.GT.4.AND.DBH.GE.BFMIND)THEN
          VOL(4) = VOL(1) - TIPV
          VOL(7) = TIPV - TIP4
          VOL(15) = VOL(1)-VOL(4)-VOL(7)
        ENDIF
      ENDIF;
      RETURN
      END SUBROUTINE FAUROT_VOL
!----------------------------------------------------------------------
! Moisen 1989
! From unpublished memo dated October 4, 1989
! CU000057 CVT FUNCTION MOISEN2_CU(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000058 CV4 FUNCTION MOISEN2_MCU(SPN IN NUMBER,  DBH IN NUMBER, THT IN NUMBER, TOP IN NUMBER := 4)
! CU000112 CV6 FUNCTION MOISEN2_SCU( SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER, TOP IN NUMBER :=4)
! VALID SPECIES CODE: 017, 073, 108, 119, 122, 202
! NVEL EQUATION NUMBER:
! R00MOI0017, R00MOI0073, R00MOI0108, R00MOI0119, R00MOI0122, R00MOI0202, 
! CU000040 CVT FUNCTION MOISEN2_CU(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000040 has slightly different coef than CU000057 (no document say why, added this equation as is)
! NVEL EQUATION NUMBER for CU000040
! R00MOI1017, R00MOI1073, R00MOI1108, R00MOI1119, R00MOI1122, R00MOI1202, 
      SUBROUTINE MOISEN_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      INTEGER SPN,ERRFLG,I,J,IDX,CNT,VOLSP(6)
      REAL DBHOB,HTTOT,MTOPP,BFMIND,VOL(15),DBH,THT,RATIO
      REAL A0,A1,A2,B0,B1,B2,F,COEF(6,8),CVT,CV4,CV6,COEF1(6,4)
      DATA VOLSP/017, 073, 108, 119, 122, 202/
      DATA ((COEF(J,I), I=1,8), J=1,6) /
!      SPN  A0       A1     A2      B0    B1     B2     F      
     & 017, .001255, 1.662, 1.328,  .592, 3.595, 3.329, 1.093,
     & 073, .000964, 1.756, 1.283, 1.133, 3.561, 3.418, 1.175,
     & 108, .002057, 1.862,  1.12,  .688,  3.58, 3.405, 1.032,
     & 119, .001592, 1.661, 1.256,   .62, 3.358, 3.137, 1.037,
     & 122, .00172,  1.876, 1.089, 1.047,  3.45,  3.29, 1.128,
     & 202, .001655, 1.703, 1.217,  .709, 3.475, 3.229, 1.153/
      DATA ((COEF1(J,I), I=1,4), J=1,6) /
!      SPN  A0        A1     A2            
     & 017, 0.001138, 1.666, 1.345,  
     & 073, 0.000931, 1.756, 1.290, 
     & 108, 0.001948, 1.859, 1.133, 
     & 119, 0.001514, 1.648, 1.274, 
     & 122, 0.001742, 1.882, 1.080, 
     & 202, 0.001633, 1.713, 1.213/
      CV(DBH,THT,A0,A1,A2)=A0*(DBH**A1)*(THT**A2)
      TOPRATIO(DBH,MTOPP,B0,B1,B2)=B0*(MTOPP**B1/DBH**B2)
      ERRFLG = 0
      VOL = 0.0
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(MTOPP.LT.0.1) MTOPP = 4.0
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      IDX = 0
      CNT = 6
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      DBH = DBHOB
      THT = HTTOT
      IF(VOLEQ(7:7).EQ.'1')THEN
        IF(COEF1(IDX,1).EQ.SPN)THEN
          A0 = COEF1(IDX,2)
          A1 = COEF1(IDX,3)
          A2 = COEF1(IDX,4)
          CVT = CV(DBH,THT,A0,A1,A2)
          VOL(1) = CVT
        ENDIF
        RETURN
      ENDIF
      IF(COEF(IDX,1).EQ.SPN)THEN
        A0 = COEF(IDX,2)
        A1 = COEF(IDX,3)
        A2 = COEF(IDX,4)
        B0 = COEF(IDX,5)
        B1 = COEF(IDX,6)
        B2 = COEF(IDX,7)
        F = COEF(IDX,8)
        CVT = CV(DBH,THT,A0,A1,A2)
        !The original code convert TOPDOB to TOPDIB
        !TOPDIB is used for ratio calculation
        !Since MTOPP is already TOPDIB, I removed the conversion TOPDIB = TOPDOB/F
        RATIO = TOPRATIO(DBH,4.0,B0,B1,B2)
        CV4 = CVT*(1.0-RATIO)
        VOL(1) = CVT
        VOL(4) = CV4
        IF(DBH.GE.BFMIND)THEN
          CALL SAWLOGFACTOR(SPN,DBH,SPF,ERRFLG)
          CV6 = CV4*SPF
          VOL(4) = CV6
          VOL(7) = CV4 - CV6
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE MOISEN_VOL
!----------------------------------------------------------------------
! Allen, G.M., Adams, D.L., and Prausa, C.R. 1974. Preliminary volume tables for small trees in northern Idaho. 
! University of idaho, Forestry, wildlife, and Range Experiment Station Note No 21.
! Total board foot volume above a 1 foot stump using Scribner type diagraming but not true scribner rules.
! BD000006 - SV  FUNCTION ALLEN21_BD(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! Total stem volume above a 1 foot stump
! CU000006 - CVT FUNCTION ALLEN21_CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! VALID SPECIES CODE: 017, 073, 108, 202, 299
! NVEL EQUATION NUMBER:
! RIDALN0017, RIDALN0073, RIDALN0108, RIDALN0202, RIDALN0299, 
! Allen, G.M., Adams, D.L., Houck, G.L., and Hatch, C.R. 1976. Volume tables for small trees in northern Idaho. 
! University of idaho, Forestry, wildlife, and Range Experiment Station Note No 27.
! BD000007 - SV  FUNCTION ALLEN21_BD(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! CU000007 - CVT FUNCTION ALLEN21_CU(SPC IN VARCHAR2, DBH IN NUMBER, THT IN NUMBER)
! VALID SPECIES CODE: 017, 073, 108, 202, 299
! NVEL EQUATION NUMBER:
! R02ALN1017, R02ALN1073, R02ALN1108, R02ALN1202, R02ALN1299, 
      SUBROUTINE ALLEN_SMALLTREE(VOLEQ,DBHOB,HTTOT,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      INTEGER SPN,ERRFLG
      REAL DBHOB,HTTOT,VOL(15),DBH,THT,D2H,SV,CVT
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      THT = HTTOT
      VOL = 0.0
      ERRFLG = 0
      IF(DBH.LT.2.0) RETURN
      D2H = DBH*DBH*THT
      IF(VOLEQ(7:7).EQ.'0')THEN
!     STSTION NOTE NO 21
        IF(SPN.EQ.17)THEN
          SV = .009523*D2H
          CVT = .400+.002159*D2H
        ELSEIF(SPN.EQ.73)THEN
          SV = .008421*D2H
          CVT = .179+.001960*D2H
        ELSEIF(SPN.EQ.108)THEN
          SV = .01031*D2H
          CVT = .838+.00201*D2H
        ELSEIF(SPN.EQ.202)THEN
          SV = .008423*D2H
          CVT = .141+.00194*D2H
        ELSE
          SV = .009248*D2H
          CVT = .171+.002145*D2H
        ENDIF
      ELSE
!     STSTION NOTE NO 27
        IF(SPN.EQ.17)THEN
          SV = -1.84883+.009881*D2H
          CVT = -.00167+.002153*D2H
        ELSEIF(SPN.EQ.73)THEN
          SV = -1.26871+.009042*D2H
          CVT = .09023+.001922*D2H
        ELSEIF(SPN.EQ.108)THEN
          SV = -1.00612+.011104*D2H
          CVT = .14528+.002306*D2H
        ELSEIF(SPN.EQ.202)THEN
          SV = -.32308+.008395*D2H
          CVT = .16949+.001795*D2H
        ELSE
          SV = -1.488+.010601*D2H
          CVT = .03117+.002217*D2H
        ENDIF
      ENDIF
      VOL(1) = CVT
      VOL(2) = SV
      RETURN
      END SUBROUTINE ALLEN_SMALLTREE      
