! This is from FIA package code NIMS_VOL_FIA_EAST_PKB
! EAST1_MCU    - CU000059  Gross cubic foot volume - to a merch top (merch definition unknown)
! EAST1_TCU    - CU000060  Gross cubic foot volume - total wood fiber
! EAST1_SCU    - CU000111  Gross cubic foot volume - to a merch top SAWLOG
! EAST1_INT_BD - BD000044 International Board foot volume to a 6 inch top
! EAST1_BD     - BD000045 Scribner Board foot volume to a 6 inch top
! REFERENCE:
! Code from RMRS program HARDTS.F AND Source document not known
! NVEL EQUATION NUMBER:
! R00RMR0*** (where *** is the species in VOLSP)
! CREATED BY YW 2018/0803
      SUBROUTINE EAST_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND
      INTEGER ERRFLG,SPN,I,J,IDX,CNT,VOLSP(14)
      REAL CUCOEF(14,7),TFCOEF(14,5),BRCOEF(14,5)
      REAL DBH,HT,TCU,MCU,SAWCU,BD,RC,RB,CV4
      REAL A1,A2,B1,B2,C1,C2,D1,D2
      DATA VOLSP/
     &  68,123,313,400,491,544,552,602,680,731,
     & 823,826,972,999/
!    Gross cubic volume stump to 4 inch top from total height
!    Sapling (column 2 & 3), pole (column 4 & 5)  and saw (column 6 & 7)   
      DATA ((CUCOEF(J,I), I=1,7), J=1,14) /
     & 68,0.052928,0.002853,-0.315878,0.002207,-0.104252,0.002145,
     & 123,0.081287,0.002219,-0.013552,0.002453,0.870587,0.002205,
     & 313,0.054564,0.002565,-0.451948,0.001921,0.518892,0.001802,
     & 400,0.04762,0.002192,-0.665929,0.001868,-0.793179,0.001884,
     & 491,0.037005,0.002729,-0.347844,0.001837,-0.202284,0.001818,
     & 544,0.064004,0.00241,-0.392822,0.001921,0.172701,0.001851,
     & 552,0.058647,0.002378,-0.347844,0.001837,-0.202284,0.001818,
     & 602,0.058647,0.002378,-0.033805,0.001656,0.42189,0.001596,
     & 680,0.058647,0.002378,-0.347844,0.001837,-0.202284,0.001818,
     & 731,0.054564,0.002565,0.229691,0.001901,2.326908,0.001649,
     & 823,0.041238,0.002603,-0.279035,0.001894,0.248363,0.001823,
     & 826,0.041238,0.002603,-0.279035,0.001894,0.248363,0.001823,
     & 972,0.032199,0.002589,-0.155066,0.001828,-0.316005,0.001851,
     & 999,0.062125,0.002494,-0.37883,0.001855,0.82352,0.00163/
!--Coefficient to convert trees from CV4 (4 inch top) to CVT (total stem)
!--   POLES (column 2 & 3) SAW (column 4 & 5)
      DATA ((TFCOEF(J,I), I=1,5), J=1,14) /
     & 68,1.176439,2.550195,1.176439,2.550195,
     & 123,1.127272,1.123936,1.127272,1.123936,
     & 313,1.145017,2.255001,1.145017,2.255001,
     & 400,1.189261,3.074669,1.189261,3.074669,
     & 491,1.186636,2.417775,1.186636,2.417775,
     & 544,1.128655,2.34928,1.128655,2.34928,
     & 552,1.186636,2.417775,1.186636,2.417775,
     & 602,1.173659,1.863595,1.173659,1.863595,
     & 680,1.186636,2.417775,1.186636,2.417775,
     & 731,1.150504,1.590116,1.150504,1.590116,
     & 823,1.218003,2.181659,1.218003,2.181659,
     & 826,1.218003,2.181659,1.218003,2.181659,
     & 972,1.159555,2.270106,1.159555,2.270106,
     & 999,1.218003,2.181659,1.218003,2.181659/
!--Coefficient to convert trees from CV4 (4 inch top) to cubic sawlog portion and boardfoot      
!--   saw cubicfoot (column 2 & 3) SAW boardfoot (column 4 & 5)
      DATA ((BRCOEF(J,I), I=1,5), J=1,14) /
     & 68,0.987563,-4.027958,-31.928229,40.082406,
     & 123,1.019967,-3.831951,-36.169514,44.588514,
     & 313,0.993648,-14.095485,-46.585716,54.641538,
     & 400,0.975054,-11.967499,-43.385922,51.122382,
     & 491,0.970888,-12.11488,-33.469593,40.320487,
     & 544,0.990354,-13.86657,-44.046785,51.632536,
     & 552,0.970888,-12.11488,-33.469593,40.320487,
     & 602,0.939211,-10.789604,-16.280751,21.457858,
     & 680,0.970888,-12.11488,-33.469593,40.320487,
     & 731,0.977294,-16.118257,-45.419797,53.050199,
     & 823,0.970577,-11.942936,-42.2359,46.599115,
     & 826,0.970577,-11.942936,-39.207446,46.599115,
     & 972,0.944758,-11.243663,-33.168491,39.961348,
     & 999,0,0,-39.207446,46.599115/
     
      ERRFLG = 0
      IF(DBHOB.LT.1.0) ERRFLG = 3
      IF(HTTOT.LT.4.5) ERRFLG = 4
      IF(ERRFLG.GT.0) RETURN
      READ(VOLEQ(8:10),'(I3)')SPN
      CNT = 14
      IDX = 0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      DBH = DBHOB
      HT = HTTOT
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      IF(IDX.GT.0.AND.SPN.EQ.CUCOEF(IDX,1)) THEN
        CV4 = 0.0
        IF(BFMIND.LT.0.1)THEN
          IF(SPN.LT.300)THEN
            BFMIND = 9.0
          ELSE
            BFMIND = 11.0
          ENDIF
        ENDIF
!       SAPLINGS        
        IF(DBH.LT.5.0)THEN
          A1 = 0.
          A2 = 0.
!       POLES          
        ELSEIF(DBH.LT.BFMIND)THEN     
          A1 = CUCOEF(IDX,4)
          A2 = CUCOEF(IDX,5)
          B1 = TFCOEF(IDX,2)
          B2 = TFCOEF(IDX,3)
!       SAW
        ELSE
          A1 = CUCOEF(IDX,6)
          A2 = CUCOEF(IDX,7)
          B1 = TFCOEF(IDX,4)
          B2 = TFCOEF(IDX,5)
          C1 = BRCOEF(IDX,2)
          C2 = BRCOEF(IDX,3)
          D1 = BRCOEF(IDX,4)
          D2 = BRCOEF(IDX,5)
        ENDIF
        CV4 = A1+A2*DBH**2*HT
        VOL(4) = CV4
        TF = B1+B2*(1.0/(DBH-3.0)**2)
        TCU = CV4*TF
        VOL(1) = TCU
        IF(DBH.GE.BFMIND)THEN
          RC = C1 + C2 * (1.0 / (DBH-5.0) ** 2)
          MCU = CV4*RC
          VOL(7) = CV4-MCU
          VOL(4) = MCU
          RB = D1+D2*(1.0-1.0/DBH)
          BD = CV4*RB
          VOL(10) = BD
          VOL(2) = BD*0.89
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE EAST_VOL
          
