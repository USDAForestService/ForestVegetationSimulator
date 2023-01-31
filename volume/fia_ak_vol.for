! This is from NIMS_AK_VOL package
! Created by YW 2018/08/15
!--Gregory Robert A. and Paul M. Haack
!--Tables and Equations for Estimating Cubic Foot Volumes of Interior Alaska Tree species
!--NOR-6, May 1964
! CU000107 FUNCTION GREGORY_CVTS(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000108 FUNCTION GREGORY_CV4(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000113 FUNCTION GREGORY_CV6(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! CU000114 FUNCTION GREGORY_CV8(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
! Valid species code: 094, 375, 746, 747
! NVEL Equation Number:
! P01GRE0094, P01GRE0375, P01GRE0746, P01GRE0747, 
      SUBROUTINE GREGORY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,MTOPP,BFMIND,VOL(15)
      INTEGER ERRFLG,SPN
      REAL CV4,CV6,CV8,DBH,THT,CVTS,TOP
      VOL = 0.0
      DBH = DBHOB
      THT = HTTOT

C Added initialization of variables 01/2023 DW
      CV4  = 0
      CV6  = 0
      CV8  = 0
      CVTS = 0

      IF(DBH.LT.1.0.OR.THT.LT.0.0) RETURN
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      IF(MTOPP.LT.0.1) MTOPP = 6.0
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(SPN.EQ.94) THEN
        CVTS = -1.1843+0.205*DBH+.01639*THT+.00187*DBH**2*THT
        CV4 = -2.0555+.2982*DBH+.00181*DBH**2*THT
      ELSEIF(SPN.EQ.746) THEN
        CVTS = -0.011*DBH**2+.00662*THT+.00235*DBH**2*THT
        CV4 = -.5553-.02216*DBH**2+.00246*DBH**2*THT
      ELSEIF(SPN.EQ.375) THEN
        CVTS = -.01408*DBH**2+.00815*THT+.00227*DBH**2*THT;
        CV4=-2.5767+.9524*DBH-.10446*DBH**2-.03303*THT+.00282*DBH**2*THT
      ELSEIF(SPN.EQ.747) THEN
        CVTS = .00806*THT+.00175*DBH**2*THT
        CV4=-3.2187+.828*DBH-.05908*DBH**2-.01985*THT+.00199*DBH**2*THT
      ENDIF;
      VOL(1) = CVTS
      VOL(14) = 0.0001
      IF(CV4.LT.0.0)THEN
        TOP = 4.0
        CALL CONE_CV(DBH,THT,TOP,CV4)
      ENDIF
      VOL(4) = CV4
      IF(DBH.GE.BFMIND)THEN
        IF(MTOPP.EQ.6.0)THEN
          CALL GREGORY_CV6(SPN, DBH, THT, CV6)
          VOL(7) = CV4 - CV6
          VOL(4) = CV6
        ELSEIF(MTOPP.EQ.8.0)THEN  
          IF(SPN.EQ.94) THEN
            CV8 = -16.7852+4.6690*DBH-.25937*DBH*DBH-.28088*THT
     &       +.00313*DBH*DBH*THT
          ELSEIF(SPN.EQ.746) THEN
            IF(DBH.GT.16.5 .AND. THT.GT.87.5) THEN
              CALL GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
            ELSE
              CV8 = -10.0346+2.9603*DBH-.20966*DBH**2-.25373*THT
     &         +.00375*DBH*DBH*THT
            ENDIF;
          ELSEIF(SPN.EQ.375) THEN
            IF((DBH.GT.12.5 .AND. THT.LT.47.5) .OR.
     &         (DBH.GT.13.5 .AND. THT.LT.52.5) .OR.
     &         (DBH.GT.14.5 .AND. THT.LT.62.5) .OR.
     &         (DBH.GT.15.5 .AND. THT.LT.72.5) .OR.
     &         (DBH.GT.16.5 .AND. THT.LT.77.5) .OR.
     &         (DBH.GT.17.5 .AND. THT.LT.87.5) .OR.
     &         (DBH.GT.18.5)) THEN
              CALL GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
            ELSE
              CV8 = -.16588*THT+.00249*DBH**2*THT
            ENDIF;
          ELSEIF(SPN.EQ.747) THEN
            IF((DBH.GT.12.5 .AND. THT.LT.47.5) .OR.
     &         (DBH.GT.13.5 .AND. THT.LT.52.5) .OR.
     &         (DBH.GT.14.5 .AND. THT.LT.62.5) .OR.
     &         (DBH.GT.15.5 .AND. THT.LT.72.5) .OR.
     &         (DBH.GT.16.5 .AND. THT.LT.77.5) .OR.
     &         (DBH.GT.17.5 .AND. THT.LT.87.5) .OR.
     &         (DBH.GT.18.5)) THEN
              CALL GREGORY_CV6(SPN, DBH, THT, CV6)
              CV8 = CV6 - .005454*DBH**2
            ELSE
              CV8 = -14.9026+.9689*DBH+.00140*DBH*DBH*THT
            ENDIF;
          ENDIF;
          IF(CV8.GT.0.0)THEN
C         THE PUBLICATION IS FROM 2 FOOT STUMP TO 8 INCH TOP     
C I added a 1 foot cylinder the size of DBH outside bark to approximate volume to 1 foot stump. KRC    
             CV8 = CV8+.005454*DBH**2
          ENDIF
          IF(CV8.LT.0.0)THEN
            TOP = 8.0
            CALL CONE_CV(DBH,THT,TOP,CV8)
          ENDIF
          VOL(7) = CV4 - CV8
          VOL(4) = CV8
        ENDIF  
      ENDIF
      RETURN
      END SUBROUTINE GREGORY_VOL
! ---------------------------------------------------------------------
      SUBROUTINE GREGORY_CV6(SPN, DBH, THT, CV6)   
      INTEGER SPN
      REAL DBH,THT,CV6
          IF(SPN.EQ.94) THEN
            IF((THT.GT.112.5 .AND. DBH.GT.14.5 .AND. DBH.LT.15.5) .OR.
     &       (THT.GT.107.5 .AND. DBH.GE.15.5 .AND. DBH.LT.18.5) .OR.
     &       (THT.GT.112.5 .AND. DBH.GE.18.5 .AND. DBH.LT.20.5) .OR.
     &       (THT.GT.117.5 .AND. DBH.GE.20.5 .AND. DBH.LT.21.5)) THEN
              CV6 = -2.0555+.2982*DBH+.00181*DBH**2*THT
            ELSE
             CV6 = -10.8448+2.6242*DBH-.14074*DBH*DBH-.09628*THT
     &              +.00246*DBH*DBH*THT
            ENDIF;
          ELSEIF(SPN.EQ.746) THEN
            IF((DBH.GT.14.5 .AND. THT.LT.52.5) .OR.
     &         (DBH.GT.15.5 .AND. THT.LT.62.5) .OR.
     &         (DBH.GT.16.5 .AND. THT.LT.67.5) .OR.
     &         (DBH.GT.17.5 .AND. THT.LT.77.5) .OR.
     &         (DBH.GT.18.5 .AND. THT.LT.82.5) .OR.
     &         (DBH.GT.19.5 .AND. THT.LT.87.5) .OR.
     &         (DBH.GT.20.5 .AND. THT.LT.92.5) .OR.
     &         (DBH.GT.22.5)) THEN
              CV6 = -.5553-.02216*DBH**2+.00246*DBH**2*THT
            ELSE
              CV6 = -3.5054+.00229*DBH**2*THT
            ENDIF;
          ELSEIF(SPN.EQ.375) THEN
            IF((DBH.GT.10.5 .AND. THT.LT.47.5) .OR.
     &         (DBH.GT.11.5 .AND. THT.LT.57.5) .OR.
     &         (DBH.GT.12.5 .AND. THT.LT.67.5) .OR.
     &         (DBH.GT.13.5 .AND. THT.LT.77.5) .OR.
     &         (DBH.GT.14.5 .AND. THT.LT.87.5) .OR.
     &         (DBH.GT.15.5)) THEN
              CV6 = -2.5767+.9524*DBH-.10446*DBH**2-.03303*THT
     &         +.00282*DBH**2*THT
            ELSE
              CV6 = -.07842*THT+.00242*DBH**2*THT
            ENDIF;
          ELSEIF(SPN.EQ.747) THEN
            IF((DBH.GT.11.5 .AND. THT.LT.37.5) .OR.
     &         (DBH.GT.12.5 .AND. THT.LT.42.5) .OR.
     &         (DBH.GT.13.5 .AND. THT.LT.62.5) .OR.
     &         (DBH.GT.14.5 .AND. THT.LT.67.5) .OR.
     &         (DBH.GT.15.5 .AND. THT.LT.72.5) .OR.
     &         (DBH.GT.16.5 .AND. THT.LT.82.5) .OR.
     &         (DBH.GT.18.5 .AND. THT.LT.87.5) .OR.
     &         (DBH.GT.18.5 .AND. THT.LT.92.5) .OR.
     &         (DBH.GT.21.5 .AND. THT.LT.97.5) .OR.
     &         (DBH.GT.26.5 .AND. THT.LT.102.5) .OR.
     &         (DBH.GT.29.5 .AND. THT.LT.105.5) .OR.
     &         (DBH.GT.34.5)) THEN
              CV6 = -3.2187+.828*DBH-.05908*DBH**2-.01985*THT
     &         +.00199*DBH**2*THT;
            ELSE
              CV6 = -7.7571+.6546*DBH+.00151*DBH**2*THT
            ENDIF;
          ENDIF;
          IF(CV6.LT.0.0)THEN
            TOP = 6.0
            CALL CONE_CV(DBH,THT,TOP,CV6)
          ENDIF
      RETURN
      END SUBROUTINE GREGORY_CV6
! ---------------------------------------------------------------------
!--Embry, Robert S. and Paul M. Haack
!--Volume Tables and Equations for Young-Growth Western Hemlock and Sitka Spruce in Southeast Alaska
!--Northern Forest Experiment Station, U. S. Forest Service Research Note NOR-12,
!--NOR-12, December 1965.
! CU000117 FUNCTION EMBRY_CV4(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! CU000118 FUNCTION EMBRY_CV6(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! BD000092 FUNCTION EMBRY_SV(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! BD000093 FUNCTION EMBRY_IV(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! Valid species code: 098, 263
! NVEL Equation Number:
! P01EMB0098, P01EMB0263,  
      SUBROUTINE EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,MTOPP,BFMIND,VOL(15)
      INTEGER ERRFLG,SPN,I,J
      REAL CV4,CV6,IV,DBH,HT,TOP
      VOL = 0.0
      DBH = DBHOB
      HT = HTTOT

C Added initialization of variables 01/2023 DW
      CV4 = 0
      CV6 = 0
      IV  = 0
      
      IF(DBH.LT.1.0.OR.HT.LT.0.0) RETURN
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      IF(MTOPP.LT.0.1) MTOPP = 6.0
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(SPN.EQ.263) THEN
        CV4 = -2.85632 + 0.0633 * HT + 0.00202 * DBH**2 * HT
      ELSEIF(SPN.EQ.98) THEN
        CV4 = 0.0022 * DBH**2 * HT - 7.32683 / DBH**2
      ENDIF;
      IF(CV4.LT.0.0)THEN
        TOP = 4.0
        CALL CONE_CV(DBH,HT,TOP,CV4)
      ENDIF
      VOL(4) = CV4
      IF(DBH.GE.BFMIND)THEN
          IF(SPN.EQ.263) THEN
            IF(DBH.LT.19.5) THEN
              CV6 = -3.70342 + 0.02856 * HT + 0.00213 * DBH**2 * HT
            ELSE
              CV6 = -2.85632 + 0.0633 * HT + 0.00202 * DBH**2 * HT
            ENDIF;
            SV = -0.53887*DBH**2 + 0.01614*DBH**2*HT
            IV = -0.63535*DBH**2 + 0.32091*HT + 0.01741*DBH**2*HT
          ELSEIF(SPN.EQ.98) THEN
            CV6 = 0.00216 * DBH**2 * HT - 154.15834 / DBH**2
            SV = -4.60417*DBH +  0.01427*DBH**2*HT
            IV =  -0.43761 * DBH**2 + 0.01733 * DBH**2 * HT
          ENDIF;
          IF(CV6.LT.0.0)THEN
            TOP = 6.0
            CALL CONE_CV(DBH,HT,TOP,CV6)
          ENDIF
          VOL(7) = CV4 - CV6
          VOL(4) = CV6
          VOL(2) = SV
          VOL(10) = IV
      ENDIF
      RETURN
      END SUBROUTINE EMBRY_VOL
! ---------------------------------------------------------------------
! Cap to prevent negative volumes  KRC - 4/18/09
      SUBROUTINE CONE_CV(DBH,THT,TOP,CV)
      REAL DBH,THT,TOP,CV
      REAL HT_TOP,CONE_STEM,CONE_TOP,CYLINDER_TO_DBH,CONE
        HT_TOP = TOP*(THT-4.5)/DBH
        CONE_STEM = (3.14*((DBH/24)*(DBH/24))*(THT-3.5))/3
        CONE_TOP = 0.029088*HT_TOP
        CYLINDER_TO_DBH = (3.14*((DBH/24)*(DBH/24))*(3.5))
        CONE = CYLINDER_TO_DBH+CONE_STEM-CONE_TOP
        IF (CONE.GT.CV) THEN
          CV = CONE
        END IF
      RETURN
      END SUBROUTINE CONE_CV
! ---------------------------------------------------------------------
! =====================================================================
! The following code is from NIMS_VOL_AK_DEM
! FIA implement Demars Taper Equation slightly different than R10
! ---------------------------------------------------------------------
!--compute gross volume from top of stump(HT_STUMP) to given top diameter inside bark (TOP)
!--for individual tree using translation of Dave Bruce/Don Demars program as provided for AFSL use (DBVOL3.FOR)
! CU000116 FUNCTION DEMARS_VOL_CU
!--Computes Scribner Volume using Demars Taper equation as it is done by Alaska FIA
!--Tree divided into 16.6 foot logs above stump and each log computed with regression formula
!--Last partial log is computed as if whole log with 6 inch top and then porpotioned
!--by ratio of length/16.3
! BD000090  FUNCTION DEMARS_SV
!--Computes International 1/4 Volume using Demars Taper equation as it is done by Alaska FIA
!--Tree divided into 16.6 foot logs above stump and each log computed with regression formula
!--Last partial log is computed as if whole log with 6 inch top and then porpotioned
!--by ratio of length/16.3
! BD000091 FUNCTION DEMARS_IV
! Valid species code: 098, 263, 042, 242
! NVEL Equation Number:
! P01DEM0042, P01DEM0098, P01DEM0242, P01DEM0263, 
! For Afognak Island trees <= 110 ft
! P01DEMA098, P01DEMA263 
! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft
      SUBROUTINE DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      INTEGER SPN,ERRFLG
      CHARACTER*1 GEOSUB
      REAL DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15)
      REAL DBH,HT,BRSQ,MHT,CVT,TIP,VOLCYLINDER,TOP,VR,CV4
      REAL MLOGS,RELHT,RELD,DIB,HTUP,CL,LOGSV,LOGIV,SV6,IV6,CV6
      
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(SPN.NE.42.AND.SPN.NE.98.AND.SPN.NE.242.AND.SPN.NE.263)THEN
        ERRFLG = 6
        RETURN
      ENDIF
      DBH = DBHOB
      HT = HTTOT
      TOP = 4.0
      GEOSUB = VOLEQ(7:7)
      IF(STUMP.LT.0.1) STUMP = 1.0
      IF(MTOPP.LT.0.1) MTOPP = 6.0
      VOL = 0.0
!--  ratio of diameter inside bark to diameter outside bark
      CALL BRSQ_DEMARS(SPN, DBH, HT, GEOSUB, BRSQ)
!--  volume of cylinder with diameter equal to diameter at breast height inside bark and length equal to bole
!--  length above breast height
      VOLCYLINDER = 0.005454154 * DBH ** 2 * (HT - 4.5) * BRSQ
!--  height to merchantable top (top diameter point), in feet
      CALL MERCH_HT(SPN,DBH,HT,TOP,BRSQ, GEOSUB,MHT)
!--  volume stump to tip
!--  FF function returns total volume inside bark for a given taper function relative to volume
!--  of cylinder (FV) with diameter = D and length =  total length above breast height
      RELHT = (HT - STUMP) / (HT - 4.5)
      CALL VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
      CVT = VR * VOLCYLINDER
!--  volume top diameter point to tip
      RELHT = (HT - MHT) / (HT - 4.5)
      CALL VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
      TIP = VR * VOLCYLINDER
      CV4 = CVT - TIP   !-- cubic volume, stump to 4 inch top dib
      VOL(1) = CVT
      VOL(4) = CV4
      VOL(15) = TIP
      IF(DBH.GE.BFMIND)THEN
        TOP = MTOPP
        MHT = 0.0
        SV6 = 0.0
        IV6 = 0.0
        CALL MERCH_HT(SPN, DBH, HT, TOP, BRSQ, GEOSUB, MHT)
!--  volume top diameter point to tip
        RELHT = (HT - MHT) / (HT - 4.5)
        CALL VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
        TIP = VR * VOLCYLINDER
        CV6 = CVT - TIP   
        VOL(7) = CV4 - CV6
        VOL(4) = CV6     
!--  Number of whole logs in merch portion
        MLOGS = INT((MHT-STUMP)/16.3)
!--  Compute and accumulate whole log volumes
        HTUP = STUMP
        DO 10 I = 1,INT(MLOGS)
          HTUP = HTUP+16.3
          RELHT = (HT - HTUP) / (HT - 4.5)
          CALL RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
          DIB = (RELD*BRSQ)**0.5*DBH
          LOGSV = 0.79*DIB**2-2.0*DIB-4.0
          SV6 = SV6+LOGSV
          LOGIV = 0.796*DIB**2-1.374*DIB-1.23
          IV6 = IV6 + LOGIV
10      CONTINUE
!--  Compute and accumulate last chunk volumes
        CL = MHT-16.3*MLOGS-STUMP
        RELHT = (HT - MHT) / (HT - 4.5)
        CALL RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
        DIB = (RELD*BRSQ)**0.5*DBH
        LOGSV = (0.79*DIB**2-2.0*DIB-4.0)*CL/16.3
        SV6 = SV6+LOGSV
        VOL(2) = SV6
        IF (CL.GT.0.0 .AND. CL.LT.4.075) THEN
          LOGIV = CL / 4.075 * (0.199 * DIB**2 - 0.642 * DIB)
        ELSEIF (CL.LT.8.15) THEN
          LOGIV = CL / 8.15 * (0.398 * DIB**2 - 1.086 * DIB - 0.271)
        ELSEIF (CL.LT.12.225) THEN
          LOGIV = CL / 12.225 * (0.597 * DIB**2 - 1.33 * DIB - 0.715)
        ELSEIF (CL.LT.16.3) THEN
          LOGIV = CL / 16.3 * (0.796 * DIB**2 - 1.375 * DIB - 1.23)
        ENDIF
        IV6 = IV6+LOGIV
        VOL(10) = IV6
      ENDIF
      RETURN
      END SUBROUTINE DEMARS_VOL
! ---------------------------------------------------------------------
!--Switches between Demars and Embry/Haack equations depending on dbh and ht
!--Only the Sitka Spruce Embry equation is used by Alaska FIA regardless of species
!--CU000119 FUNCTION DEMARS_EMBRY_CV4
!--CU000120 FUNCTION DEMARS_EMBRY_CV6
! Valid species code: 098, 263, 042, 242
! NVEL Equation Number:
! P01DEE0098, P01DEE0263, P01DEE0042, P01DEE0242, 
! P01DEEA098, P01DEEA263, P01DEEA042, P01DEEA242, 
! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft
      SUBROUTINE DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ, VOLEQTEM
      INTEGER SPN,ERRFLG
      REAL DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15)
      VOL = 0.0
      IF(DBHOB.LT.9.0.OR.HTTOT.LT.40.0)THEN
        VOLEQTEM = VOLEQ(1:7)//'098'
        CALL EMBRY_VOL(VOLEQTEM,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      ELSE
        CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     & ERRFLG)
      ENDIF
      RETURN
      END SUBROUTINE DEMARS_EMBRY
! ---------------------------------------------------------------------
!--Switches between Browne, Demars and Embry/Haack equations depending on dbh and ht
!--Only the Sitka Spruce Embry equation is used by Alaska FIA regardless of species
!--Browne, J. E. 1962, Standard Cubic Foot Volume for Commercial Tree Species of British Columbia
!--Embry, Robert S. and Paul M. Haack, NOR-12, December 1965.
!--DeMars
! CU042001 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_CV4
! CU042002 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_CV6
! BD042002 FUNCTION CHNO_BROWNE_DEMARS_EMBRY_IV
! CU242001 FUNCTION THPL_BROWNE_DEMARS_EMBRY_CV4
! CU242002 FUNCTION THPL_BROWNE_DEMARS_EMBRY_CV6
!          FUNCTION THPL_BROWNE_DEMARS_EMBRY_IV
! Valid species code: 042, 242
! NVEL Equation Number:
! P01BDE0042, P01BDE0242, P01BDEA042, P01BDEA242    
! THE 7TH CHARACTER IN THE EQUATION NUMBER: 0 = AK WIDE, A = Afognak Island trees <= 110 ft  
      SUBROUTINE BROWNE_DEMARS_EMBRY(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,
     & BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ,VOLEQTMP,VOLEQTMPI
      INTEGER SPN,ERRFLG
      REAL DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL(15),VOLTMP(15)
      READ (VOLEQ(8:10),'(I3)')SPN
      IF(SPN.NE.42.AND.SPN.NE.242)THEN
        ERRFLG = 6
        RETURN
      ENDIF
      VOL = 0.0
      VOLEQTMP = VOLEQ
      IF(SPN.EQ.42)THEN
        VOLEQTMP(4:7) = 'BRO0'
      ELSEIF(SPN.EQ.242)THEN
        VOLEQTMP(4:7) = 'BRC2'
      ENDIF
      VOLEQTMPI = VOLEQTMP
      IF(DBHOB.LT.5.0)THEN
        CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     & ERRFLG)
      ELSEIF(DBHOB.LT.9.0)THEN
        CALL BROWNE_DNR24(VOLEQTMP,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
!       Recalc international boardfoot volume using Browne no location and no age split
!       242 is not a valid species for no location and no age split, so has o use 042
        VOLEQTMPI(4:10) = 'BRO0042'
        CALL BROWNE_DNR24(VOLEQTMPI,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,
     &   ERRFLG)
        VOL(10) = VOLTMP(10)
      ELSEIF((DBHOB.LT.38.0.AND.SPN.EQ.42).OR.
     &       (DBHOB.LT.56.0.AND.SPN.EQ.242))THEN
        IF(HTTOT.LT.40.0)THEN
         CALL BROWNE_DNR24(VOLEQTMP,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
        ELSE
         CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     & ERRFLG)
        ENDIF
!       Recalc international boardfoot volume
        IF(HTTOT.LT.25.0)THEN
          VOLEQTMPI(4:10) = 'BRO0042'
          CALL BROWNE_DNR24(VOLEQTMPI,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,
     &         ERRFLG)
        ELSE
          CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,
     &         VOLTMP,ERRFLG)
        ENDIF
        VOL(10) = VOLTMP(10)
      ELSE
        VOLEQ = VOLEQ(1:7)//'098'
        IF(HTTOT.LT.40.0)THEN
          CALL EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     &         ERRFLG)
        ELSE
          CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOL,
     &         ERRFLG)
        ENDIF
!       Recalc international boardfoot volume
        IF(HTTOT.LT.25.0)THEN
          CALL EMBRY_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOLTMP,
     &         ERRFLG)
        ELSE
          CALL DEMARS_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,STUMP,BFMIND,VOLTMP,
     &         ERRFLG)
        ENDIF
        VOL(10) = VOLTMP(10)
      ENDIF
      RETURN
      END SUBROUTINE BROWNE_DEMARS_EMBRY
! ---------------------------------------------------------------------
!--Taper equation, yields ratio of squared top diameter inside bark to squared base
!--diameter outside bark given base diameter, total tree height, and ratio of total
!--tree length above breast height to length of section (relative ht) from breast height
!--to top diameter inside bark point
!--RELHT = Relative Height or (HT-HTUP/(HT-4.5)
! FUNCTION RELATIVE_DIA
      SUBROUTINE RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,RELD)
      INTEGER SPN
      REAL DBH,HT,RELHT,RELD
      CHARACTER*1 GEOSUB
      IF(SPN .EQ. 98 .OR. SPN .EQ. 263) THEN
        IF (GEOSUB .EQ. 'A' .AND. HT.LE.110) THEN  !Afognak Island trees <= 110 ft.
           RELD = RELHT ** 1.5 + (-0.0269451 * HT + 0.00023794 * HT ** 2
     &       + 0.148759 * HT / DBH) * (RELHT ** 1.5 - RELHT ** 3)
     &       + (0.0974044-0.0000145706 * HT ** 2- 0.0156971*HT/DBH)
     &       * (RELHT ** 1.5 - RELHT ** 32)
        ELSE
           RELD = RELHT**1.5 + (-0.0052554*HT + 0.000034947*HT** 2
     &       + 0.104477*HT/DBH) * (RELHT ** 1.5 - RELHT ** 3) 
     &       + (7.76807 / DBH ** 2 - 0.0000094852 * HT ** 2 -
     &          0.011351 * HT / DBH) * (RELHT ** 1.5 - RELHT ** 32)
        ENDIF;
      ELSEIF (SPN .EQ. 42) THEN
        RELD = RELHT ** 1.5 + ((RELHT ** 1.5 - RELHT ** 3) 
     &    * (-0.02834001 * DBH + 0.00007123 * HT ** 2
     &    + 0.06709114 * HT / DBH)) + ((RELHT ** 1.5 - RELHT ** 32)
     &    * (0.00282021*DBH - 0.00002277*HT**2 + 1.06064717/DBH**2
     &    - 0.00528349 * HT / DBH))
      ELSEIF (SPN .EQ. 242) THEN
        RELD = RELHT ** 1.5 + ((RELHT ** 1.5 - RELHT ** 3)
     &    * (5.17703194 / DBH ** 2 - 0.12516819 *DBH + 0.02537037 * HT
     &    - 0.00004193 * HT ** 2 + 0.00155481 * DBH ** 2)) +
     &    ((RELHT ** 1.5 - RELHT ** 32) * (-0.0000207 * HT ** 2 
     &   + 0.24125235 / DBH ** 2))
      ENDIF;
      RETURN
      END SUBROUTINE RELATIVE_DIA
! ---------------------------------------------------------------------
!--Compute integral of taper equation for tree given "relative height", dbh outside bark in
!--inches, and total tree height from ground to tip in feet
! FUNCTION VOLUME_RATIO
      SUBROUTINE VOLUME_RATIO(SPN,DBH,HT,RELHT,GEOSUB,VR)
      INTEGER SPN
      REAL DBH,HT,RELHT,VR
      CHARACTER*1 GEOSUB
      IF(SPN.EQ.98 .OR. SPN.EQ.263) THEN
        IF (GEOSUB.EQ.'A' .AND. HT.LE.110) THEN  !--Afognak Island trees <= 110 ft.
          VR = 0.4*RELHT**2.5+(-0.0269451*HT+0.00023794*HT**2 
     &      + 0.148759*HT/DBH)*(0.4*RELHT**2.5-0.25*RELHT**4) 
     &      + (0.0974044-0.0000145706*HT**2-0.0156971*HT/DBH)
     &      * (0.4 * RELHT ** 2.5 - (1.0 / 33.0) * RELHT ** 33)
        ELSE
          VR = 0.4*RELHT**2.5+(-0.0052554*HT+0.000034947*HT**2
     &      + 0.104477*HT/DBH)*(0.4*RELHT**2.5 - 0.25*RELHT**4)
     &      + (7.76807/DBH**2 - 0.0000094852*HT**2 - 0.011351*HT/DBH)
     &      * (0.4 * RELHT ** 2.5 - (1.0 / 33.0) *RELHT ** 33)
        ENDIF;
      ELSEIF (SPN.EQ.42) THEN
       VR = 0.4 * RELHT**2.5 + ((0.4*RELHT**2.5 - 0.25*RELHT**4)
     &   * (-0.02834001 *DBH + 0.00007123*HT**2 + 0.06709114*HT/DBH))
     &   + ((0.4 * RELHT ** 2.5 - 1.0 / 33.0 * RELHT ** 33) *
     & (0.00282021 * DBH - 0.00002277 * HT ** 2 + 1.06064717 / DBH**2
     & - 0.00528349 * HT / DBH))

      ELSEIF (SPN.EQ.242) THEN
       VR = 0.4 * RELHT**2.5 + ((0.4 * RELHT ** 2.5 - 0.25 * RELHT**4)
     &   * (5.17703194 / DBH **2 - 0.12516819 * DBH + 0.02537037*HT
     &  - 0.00004193 * HT ** 2 + 0.00155481 *DBH ** 2))
     &  + ((0.4 * RELHT ** 2.5 - 1.0 / 33.0 * RELHT ** 33)
     &  * (-0.0000207 * HT ** 2 + 0.24125235 / DBH ** 2))
      ENDIF;
      RETURN
      END SUBROUTINE VOLUME_RATIO
! ---------------------------------------------------------------------
!--Ratio of inside bark diameter squared to outside bark diameter squared
!  FUNCTION BRSQ_DEMARS_AK
      SUBROUTINE BRSQ_DEMARS(SPN,DBH,HT,GEOSUB,BRSQ)
      INTEGER SPN
      REAL DBH,HT,BRSQ
      CHARACTER*1 GEOSUB
      IF(SPN.EQ.98 .OR. SPN.EQ.263) THEN
        IF (GEOSUB.EQ.'A' .AND. HT.LE.110) THEN
!--      Spruce-hemlock bark thickness equation, Afognak Island trees <= 110 ft
          BRSQ = 0.773 + 0.00134 * DBH + 0.000958 * HT
        ELSE
!--      Spruce-hemlock bark thickness equation
          BRSQ = 0.8467 + 0.0009144 * DBH + 0.0003568 * HT
        END IF;
      ELSEIF (SPN.EQ.42) THEN
!--    AK yellow cedar
        BRSQ = 0.95866817 + 0.00064402 * DBH - 3.1299972 / HT
      ELSEIF (SPN.EQ.242) THEN
!--    Western red cedar
        BRSQ = 0.86031485 + 0.00059638 * HT - 0.18335961 / DBH
      END IF;
      RETURN 
      END SUBROUTINE BRSQ_DEMARS
! ---------------------------------------------------------------------
!--Computes height to a specified top diameter
! FUNCTION MERCH_HT_SPRUCE_HEMLOCK
! FUNCTION MERCH_HT_CHNO
! FUNCTION MERCH_HT_CEDAR
      SUBROUTINE MERCH_HT(SPN,DBH,HT,TOP,BRSQ,GEOSUB,MHT)
      INTEGER SPN,K,KNOL
      REAL DBH,HT,TOP,BRSQ,MHT
      CHARACTER*1 GEOSUB,SMALLD,ATLIMD
      REAL DST,XLL,RELHT,RXL,MHEST,DS,DXL,TAPER,DIB
      REAL RELD_TOLERANCE/0.0001/
      REAL DSX,TOHIGH,TOLOW,DIA,HTUP
      REAL HITOP,DSI,C,STUMP,D_TOLERANCE/0.0001/
      IF(SPN.EQ.98.OR.SPN.EQ.263) THEN
!--  initial estimate of height to merchantable top (TOP),
!--  from Bruce, 1984 (Proceedings), page 100
        MHEST = HT * (1.0 - (2.0/3.0) * (TOP/DBH))
!--  relative height based on estimated merch. height and total tree height,
!--  from Bruce, 1984, pg. 98
        RELHT = (HT - MHEST) / (HT - 4.5)
!--  relative diameter based on initial estimate of height to top diameter inside bark,
!--  measured diameter outside bark, and total tree height
        CALL RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DS)
!--  ratio of target top dib squared to dbh inside bark squared
!--  target realtive diameter
        DST = (TOP**2) / (BRSQ * DBH**2)
!--  90% of initial estimate of relative height - just to use in estimating taper near top
        RXL = 0.9 * RELHT
!--  relative diameter (dib squared / DBH ib squared) for RXL
        CALL RELATIVE_DIA(SPN, DBH, HT, RXL, GEOSUB,DXL)
!--  change in relative diameter per change in relative height, to be used in adjusting RELHT for input to
!--  taper function in process of converging on target relative diameter
        TAPER = (DS - DXL) / (0.1 * RELHT)
!--  iterate up to 10 times to converge on target top diameter inside bark
        K = 1
        DO WHILE ((K.LE.10).AND.((DS.LE.(DST - RELD_TOLERANCE)) .OR.
     &     (DS.GE.(DST + RELD_TOLERANCE)))) 
          RXL = RELHT + (DST - DS) / TAPER
          RELHT = RXL
          CALL RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DS)
          K = K + 1
        ENDDO
        MHT = HT - (RELHT * (HT - 4.5))
      ELSEIF(SPN.EQ.42.OR.SPN.EQ.242) THEN
        TOLOW = 4.5
        TOHIGH = HT
        HTUP = HT-TOP*(HT-4.5)/DBH
        RELHT = (HT - HTUP) / (HT - 4.5)                   !-- relative height for top of full log
        CALL RELATIVE_DIA(SPN,DBH,HT,RELHT, GEOSUB,DSX) !-- relative diameter at top of full log
        IF (DSX.LT.0) THEN
          DIA = 0
        ELSE
          DIA = (DSX * BRSQ) ** 0.5 * DBH               !-- diameter inside bark at top of full log
        ENDIF
        K = 0
        DO WHILE(K.LT.100.AND.(DIA.LT.(TOP-.001).OR.DIA.GT.(TOP+.001))) 
          IF(DIA.GT.TOP) THEN
            TOLOW = HTUP
          ELSE
            TOHIGH = HTUP
          ENDIF
          HTUP = (TOLOW+TOHIGH)/2.0
          RELHT = (HT - HTUP) / (HT - 4.5)                !-- relative height for top of full log
          CALL RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,DSX) !-- relative diameter at top of full log
          IF (DSX.LT.0.0) THEN
            DIA = 0
          ELSE
            DIA = (DSX * BRSQ) ** 0.5 * DBH               !-- diameter inside bark at top of full log
          ENDIF
          K = K + 1
        ENDDO
        MHT = HTUP
! THE FOLLOWING IS NOT USED IN FIA CODE        
      ELSEIF(SPN.EQ.242) THEN
        KNOL = 0                  !-- number of whole logs below top diameter inside bark limit
        HITOP = STUMP + 16.3   !-- height at top of first full log
        SMALLD = 'N'            !-- flags section top d smaller than top d limit
        K = 0
        DO WHILE ((SMALLD.EQ.'N'.AND.HITOP.LE.HT.AND.K.LT.100)) 
          K = K+1
          KNOL = KNOL + 1
          RELHT = (HT - HITOP) / (HT - 4.5)                !-- relative height for top of full log
          CALL RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB,DSX) !-- relative diameter at top of full log
          IF (DSX.LT.0.0) THEN
            DSI = 0
          ELSE
            DSI = (DSX * BRSQ) ** 0.5 * DBH               !-- diameter inside bark at top of full log
          ENDIF
          IF (DSI.GE.TOP) THEN
            SMALLD = 'Y'
          ELSE
            HITOP = HITOP + 16.3 !-- diameter at top of full log >= merch top diameter; add another full log length
          ENDIF
        ENDDO
        IF (DSI.LT.TOP) THEN
          KNOL = KNOL - 1  !-- top diameter is smaller than merch d, subtract a full log length
        ENDIF
        HITOP = HITOP - 16.3            !-- height to top of last full log
        C = 1.0                           !-- height adjustment factor initially set to 1 foot
!--  Iterate, adjusting height, to converge on specified top diameter inside bark.
!--  I capped the number of iterations at 100 because original code seemed to loop endlessly.
!--  Tried fewer iterations but could not get reasonable match with periodic data with fewer
!--  iterations. There has got to be a more efficient way to code this. KRC
        ATLIMD = 'N'
        K = 0
        DO WHILE (ATLIMD.EQ.'N'.AND.K.LT.100) 
          K = K+1
          HITOP = HITOP + C              !-- new top height: add a foot to height to top of last full log
          RELHT = (HT - HITOP) / (HT - 4.5)    !-- relative height to new top height
          CALL RELATIVE_DIA(SPN,DBH,HT,RELHT,GEOSUB,DSX)  !-- relative diameter at top of last full log
          DSI = (DSX * BRSQ) ** 0.5 * DBH   !-- diameter inside bark at top of full log
          IF ((((DSI.GT.(TOP - D_TOLERANCE)) .AND. 
     &      (DSI.LT.(TOP + D_TOLERANCE))) .OR. K.GT.100)) THEN
!--      top diameter at limit +/- .001 inch tolerance
            MHT = HITOP     !-- height to top d limit (LIMD)
            ATLIMD = 'Y'
          ELSEIF (DSI.LT.TOP) THEN
!--      top diameter smaller than limit, reduce top height and height adjustment factor, estimate next top diameter
            HITOP = HITOP - C
!            C = C * 0.1
          END IF;
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE MERCH_HT
