! This is from FIA package code NIMS_VOL_KEMP
! Created by YW 2018/08/29
! FIA implemented Kemp equation slightly different than the one in NVEL.
! So I added the FIA equations here.
!--Kemp equation used by RMRS-FIA
!--Cubic foot to a 4 inch top
!--Scribner board feet to an 8 inch top
!--International board foot volume
!-- Total stem wood fiber cubic foot volume. Clendenen,  Memo dated 10/17/77
!-- Used by RMRS-FIA cottonwoods
!-- Converts from Kemp cubic volume to 4 inch top to total stem volume
!--To compute cubic sawlog volume, the SPF will be applied to merchantable cubic foot volume
!
! CU000065 CV4 FUNCTION FIA_MCU(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! BD000046 SV  FUNCTION FIA_BD(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! BD000047 IV  FUNCTION FIA_IBD(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000066 CVT FUNCTION CLEND_TCU(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000110 CV6 FUNCTION FIA_SCU(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
!
! CU202005 CV4 FUNCTION PSME_KEMP_MCU(DBH IN NUMBER, THT IN NUMBER)
! Valid species code: 
! NVEL EQUATION NUMBER:
! R01KEM0017, R01KEM0019, R01KEM0073, R01KEM0093, R01KEM0108, R01KEM0119, 
! R01KEM0122, R01KEM0202, R01KEM0242, R01KEM0263, R01KEM0740, R01KEM0746, 
      SUBROUTINE KEMP(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,BFMIND,VOL(15)
      INTEGER ERRFLG,SPN,I,J,IDX,CNT,VOLSP(12)
      REAL CV4COEF(12,6),IVCOEF(12,6),SVCOEF(12,6),CVT2CV4R(12,4)
      REAL C1S,C2S,C1L,C2L,CBP,I1S,I2S,I1L,I2L,IBP,S1S,S2S,S1L,S2L,SBP
      REAL R1,R2,R3
      REAL CVT,CV4,SV,IV,DBH,THT,D2H,TH,RATIO,TMPCVT,TMPCV4,SPF
      DATA VOLSP/17,19,73,93,108,119,122,202,242,263,740,746/
!--Coefficients for cubic volume to 4 inch top
! Column 2 and 3 for small tree c1 and c2; column 4 and 5 for large tree
      DATA ((CV4COEF(J,I), I=1,6), J=1,12) /
     &  17, -0.563, 0.00219,  9.969, 0.00197, 47900, 
     &  19,  1.449, 0.00183, 26.222, 0.00117, 37500, 
     &  73, -0.056,  0.0017, 19.409, 0.00132, 51200, 
     &  93, 0.48, 0.00214, 19.041, 0.00174, 46400, 
     & 108, 1.052, 0.00221, 5.369, 0.00197, 18000, 
     & 119, 0.166, 0.00206, 4.508, 0.00194, 36200, 
     & 122, -1.656, 0.00203, -9.637, 0.00218, 53200, 
     & 202, 0.437, 0.00178, 7.702, 0.00165, 55900, 
     & 242, 1.141, 0.00174, 8.931, 0.00146, 27800, 
     & 263, -0.991, 0.00209, 2.544, 0.0021, 0, 
     & 740, -0.749, 0.00204, 4.285, 0.00194, 50300, 
     & 746, -0.343, 0.00224, 1.071, 0.00217, 20200/
!--Coefficients for international board foot volume
      DATA ((IVCOEF(J,I), I=1,6), J=1,12) /
     & 17, -25.764, 0.01423, 46.951, 0.01299, 58600, 
     & 19, -1.484, 0.01112, 159.286, 0.00738, 43000, 
     & 73, -5.836, 0.0108, 124.606, 0.0089, 68700, 
     & 93, -2.363, 0.0128, 48.715, 0.01225, 92900, 
     & 108, 3.548, 0.01319, 24.579, 0.01205, 18400, 
     & 119, -15.602, 0.01302, 18.828, 0.01243, 58400, 
     & 122, -46.452, 0.0137, -271.093, 0.01691, 70000, 
     & 202, -18.15, 0.01116, 25.891, 0.01071, 97900, 
     & 242, -3.099, 0.01014, 38.72, 0.00878, 30700, 
     & 263, -31.897, 0.01345, -8.618, 0.01379, 0, 
     & 740, -9.24, 0.01157, -24.975, 0.012, 36600, 
     & 746, -9.547, 0.01309, -12.441, 0.01325, 18100/
!--Coefficients for scribner board foot volume
      DATA ((SVCOEF(J,I), I=1,6), J=1,12) /
     & 17, -34.127, 0.01293, 10.603, 0.01218, 59600, 
     & 19, -11.403, 0.01011, 124.425, 0.00694, 42800, 
     & 73, -29.79, 0.00997, 85.15, 0.00841, 73700, 
     & 93, -11.851, 0.01149, 1.62, 0.01158, 0, 
     & 108, -8.085, 0.01208, 14.111, 0.01103, 21100, 
     & 119, -26.729, 0.01189, -32.516, 0.01181, 0, 
     & 122, -50.34, 0.01201, -298.784, 0.01595, 63100, 
     & 202, -25.332, 0.01003, -9.522, 0.01011, 0, 
     & 242, -10.742, 0.00878, -4.064, 0.00799, 8500, 
     & 263, -37.314, 0.01203, -50.68, 0.01306, 13000, 
     & 740, -15.966, 0.01046, -46.735, 0.0114, 32700, 
     & 746, -18.544, 0.01197, -21.309, 0.01216, 14600/
!--Coefficients for computing ratio between total cubic and cubic to 4 inch top
      DATA ((CVT2CV4R(J,I), I=1,4), J=1,12) /
     & 17,	0.87614,-1.48268,0.60654,
     & 19,	0.97449,-1.42305,0.44856,
     & 73,	0.87614,-1.48268,0.60654,
     & 93,	0.97449,-1.42305,0.44856,
     & 108,	0.99471,-1.30771,0.313,
     & 119,	0.96272,-1.37551,0.41279,
     & 122,	0.90178,-1.28594,0.38416,
     & 202,	0.87614,-1.48268,0.60654,
     & 242,	1.03508,-2.07016,1.03508,
     & 263,	0.9806,	-1.41272,0.43212,
     & 740,	0.9615,	-1.58271,0.62121,
     & 746,	0.95806,-1.33682,0.37877/
      READ(VOLEQ(8:10),'(I3)')SPN
      CNT = 12
      IDX = 0
      ERRFLG = 0
      VOL = 0.0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      IF(DBHOB.LT.5.0) RETURN
      DBH = DBHOB
      THT = HTTOT
      D2H = DBH*DBH*THT
      IF(IDX.GT.0.AND.SPN.EQ.CV4COEF(IDX,1)) THEN
        C1S = CV4COEF(IDX,2)
        C2S = CV4COEF(IDX,3)
        C1L = CV4COEF(IDX,4)
        C2L = CV4COEF(IDX,5)
        CBP = CV4COEF(IDX,6)
        R1 = CVT2CV4R(IDX,2)
        R2 = CVT2CV4R(IDX,3)
        R3 = CVT2CV4R(IDX,4)
        IF(DBH.GE.21 .AND. CBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
          CV4 = C1L + C2L*D2H
        ELSEIF(DBH.LT.21 .AND. CBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
          CV4 = C1S + C2S*D2H
        ELSEIF(D2H.LE.CBP) THEN               ! -- Small trees
          CV4= C1S + C2S*D2H
        ELSE                                     !-- Large trees
          CV4= C1L + C2L*D2H
        ENDIF
        IF(CV4.LT.0.0) CV4 = 0.1
        VOL(4) = CV4
!-- Total stem wood fiber cubic foot volume     
!-- Converts from Kemp cubic volume to 4 inch top to total stem volume   
        TH = (-R2*THT-SQRT((R2*THT)**2-4.0*R3*(R1*THT**2-
     &       (16.0*THT**2/DBH**2))))/2.0*R3
        TMPCVT = .005454*DBH**2*THT*(R1*(1.0-(1.0/THT))+R2/2.0*(1.0-
     &        (1.0/THT)**2)+(R3/3.0)*(1.0-(1.0/THT)**3))
        TMPCV4 = .005454*DBH**2*THT*(R1*(TH/THT-(1.0/THT))+R2/2.0*
     &  ((TH/THT)**2-(1.0/THT)**2)+(R3/3.0)*((TH/THT)**3-(1.0/THT)**3))
        RATIO = TMPCVT/TMPCV4
        CVT = RATIO*CV4
        IF(CVT.LT.0.0) CVT = 0.0
        VOL(1) = CVT
! Calculate volume for saw timber      
        IF(DBH.GE.BFMIND)THEN
!--Scribner board feet to an 8 inch top
          S1S = SVCOEF(IDX,2)
          S2S = SVCOEF(IDX,3)
          S1L = SVCOEF(IDX,4)
          S2L = SVCOEF(IDX,5)
          SBP = SVCOEF(IDX,6)   
          IF(DBH.GE.21 .AND. SBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
            SV = S1L + S2L*D2H
          ELSEIF(DBH.LT.21 .AND. SBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
            SV = S1S + S2S*D2H
          ELSEIF(D2H.LE.SBP) THEN                !-- Small trees
            SV = S1S + S2S*D2H
          ELSE                                     !-- Large trees
            SV = S1L + S2L*D2H
          ENDIF
          IF(SV.LT.1) SV = 1.0
          VOL(2) = SV
!--International board foot volume          
          I1S = IVCOEF(IDX,2)
          I2S = IVCOEF(IDX,3)
          I1L = IVCOEF(IDX,4)
          I2L = IVCOEF(IDX,5)
          IBP = IVCOEF(IDX,6)   
          IF(DBH.GE.21 .AND. IBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
            IV = I1L + I2L*D2H
          ELSEIF(DBH.LT.21 .AND. IBP.EQ.0) THEN          !-- Large trees - Hemlock break on DBH
            IV = I1S + I2S*D2H
          ELSEIF(D2H.LE.IBP) THEN                !-- Small trees
            IV = I1S + I2S*D2H
          ELSE                                     !-- Large trees
            IV = I1L + I2L*D2H
          ENDIF
          IF(IV.LT.1) IV = 1.0
          VOL(10) = IV
!--To compute cubic sawlog volume, the SPF will be applied to merchantable cubic foot volume
          CALL SAWLOGFACTOR(SPN,DBH,SPF,ERRFLG)
          IF(ERRFLG.EQ.0)THEN
            CV6 = CV4*SPF
            VOL(4) = CV6
            VOL(7) = CV4 - CV6
            IF(VOL(7).LT.0.0) VOL(7) = 0.0
          ENDIF
        ENDIF
        VOL(15) = VOL(1)-VOL(4)-VOL(7)
      ENDIF
      RETURN
      END SUBROUTINE KEMP
!----------------------------------------------------------------------
      SUBROUTINE SAWLOGFACTOR(SPN,DBH,SPF,ERRFLG)
      INTEGER SPN,ERRFLG,VOLSP(12),I,J,IDX,CNT
      REAL DBH,CSAW,ACSP,BCSP,FACN,SAWFAC(12,5),SPF   
      DATA VOLSP/17,19,73,93,108,119,122,202,242,263,740,746/
! --Coefficients for factors for sawlog portion factor;
      DATA ((SAWFAC(J,I), I=1,5), J=1,12) /
     & 17,	0.98,	0.96,	0.16,	4,
     & 19,	0.98,	0.96,	0.16,	4,
     & 73,	0.98,	0.96,	0.16,	4,
     & 93,	0.98,	0.96,	0.15,	3,
     & 108,	0.95,	0.98,	0.1,	4,
     & 119,	0.95,	0.98,	0.13,	5,
     & 122,	0.93,	0.95,	0.26,	7,
     & 202,	0.98,	0.96,	0.19,	5,
     & 242,	0.92,	0.93,	0.21,	2.5,
     & 263,	0.98,	0.96,	0.21,	2,
     & 740,	0.96,	0.92,	0.22,	1.4,
     & 746,	0.96,	0.92,	0.22,	1.4/
      ERRFLG = 0
      IDX = 0
      CNT = 12
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      CSAW = SAWFAC(IDX,2)
      ACSP = SAWFAC(IDX,3)
      BCSP = SAWFAC(IDX,4)
      FACN = SAWFAC(IDX,5)
      IF(DBH.LE.42.0) THEN
        SPF=ACSP - (BCSP * ((42.-DBH)/33.)**FACN)
      ELSE
        SPF = CSAW
      ENDIF
      IF(SPF.LT.0.0) SPF = 1.0
      RETURN
      END SUBROUTINE SAWLOGFACTOR