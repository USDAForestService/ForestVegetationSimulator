! This is from FIA package code NIMS_VOL_FIA_NE_PKB
! SCOTT_CU - CUBIC VOLUME TO 4 INCH TOP --CU000052
! SCOTT_MCU - CUBIC SAWLOG VOLUME       --CU000053
! SCOTT_BD - BOARD FOOT VOLUME          --BD000041
! References:
! 1. Scott, C.T. 1981. Northeastern Forest Survey Revised Cubic-Foot Volume Equations.
!    USDA Forest Service Northeastern Forest Experiment Station Research Note NE-304
! 2. Scott, C.T. 1979. Northeastern Forest Survey Board-Foot Volume Equations.
!    USDA Forest Service Northeastern Forest Experiment Station Research Note NE-271
! NVEL Equation Number:
! N02SCT0012 N02SCT0094 N02SCT0129 N02SCT0131 N02SCT0241 N02SCT0261 
! N02SCT0317 N02SCT0318 N02SCT0370 N02SCT0400 N02SCT0531 N02SCT0544 
! N02SCT0621 N02SCT0762 N02SCT0832 N02SCT0833 N02SCT0850 N02SCT0999 
! Created by YW 2018/08/02
      SUBROUTINE SCOTT_VOL(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,BFMIND,
     &                     VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,VOL(15)
      INTEGER ERRFLG,SPN,VOLSP(18),I,J,IDX,CNT
      REAL DBH,UPSRATIO,CUCOEF(18,7),BDCOEF(18,7)
      REAL A1,A2,A3,A4,A5,A6,B1,B2,B3,B4,B5,B6
      REAL BFMIND,CV4,SAWCU,BD,TOPVOL
      DATA VOLSP/
     & 12,94,129,131,241,261,317,318,370,400,
     & 531,544,621,762,832,833,950,999/
!     Scot cubic foot equation coefficients     
      DATA ((CUCOEF(J,I), I=1,7), J=1,18) /
     & 12,-0.1,-0.05444,2.1194,0.04821,2.0427,0.3579,
     & 94,0.17,-0.06315,2.0654,0.05122,2.0264,0.3508,
     & 129,0.11,-0.05977,2.0498,0.04965,2.0198,0.3468,
     & 131,-0.03,-0.05604,2.0473,0.05022,2.0198,0.3242,
     & 241,0.19,-0.05904,1.9935,0.04981,2.0027,0.3214,
     & 261,0.24,-0.05895,2.0362,0.04947,2.0172,0.3366,
     & 317,-0.45,-0.00523,2.2323,0.01338,2.0093,0.6384,
     & 318,-0.19,-0.01171,1.8949,0.0134,1.9928,0.6471,
     & 370,-0.27,-0.00675,1.9738,0.01327,1.9967,0.6407,
     & 400,-0.27,-0.00466,2.1575,0.01174,2.0035,0.664,
     & 531,-0.6,-0.00711,2.2693,0.01399,2.019,0.6518,
     & 544,0.06,-0.02437,1.5419,0.01299,1.9885,0.6453,
     & 621,-0.45,-0.00523,2.2323,0.01338,2.0093,0.6384,
     & 762,-0.04,-0.01783,1.8109,0.01358,1.9905,0.6553,
     & 832,-0.26,0.00038,2,0.01068,1.998,0.6438,
     & 833,-0.13,-0.00536,1.9172,0.01131,1.9975,0.6549,
     & 950,-0.39,-0.00622,2.0066,0.0131,1.9939,0.6494,
     & 999,0.13,-0.00183,2.36,0.00944,2.0608,0.6516/
       DATA ((BDCOEF(J,I), I=1,7), J=1,18) /    
     & 12,-12.29,-0.08212,2.5641,0.1416,2.2657,0.3744,
     & 94,-13.03,-0.05197,2.5248,0.12,2.1999,0.4227,
     & 129,-12.25,-0.02418,2.6865,0.0961,2.2281,0.4222,
     & 131,-6.78,-0.00841,2.7001,0.0645,2.1938,0.4713,
     & 241,-8.89,-0.07324,2.4556,0.1216,2.2382,0.3249,
     & 261,-8.36,-0.01433,2.7878,0.0771,2.2593,0.4202,
     & 317,2.84,-0.00557,3.1808,0.0296,2.4606,0.5771,
     & 318,3.73,-0.00182,3.3766,0.0262,2.4291,0.6139,
     & 370,8.23,0.00039,3,0.0206,2.2116,0.8019,
     & 400,-1.24,-0.00385,3.1648,0.0312,2.3888,0.6067,
     & 531,-0.84,-0.01207,3.0043,0.0419,2.3951,0.5912,
     & 544,9.2,0.00052,3,0.0193,2.2165,0.8043,
     & 621,2.84,-0.00557,3.1808,0.0296,2.4606,0.5771,
     & 762,1.58,-0.00151,3.3878,0.0287,2.3875,0.6356,
     & 832,4.46,-0.00061,3.5972,0.0182,2.4804,0.5922,
     & 833,1.01,-0.00192,3.3188,0.0246,2.4268,0.6,
     & 950,2.66,-0.00313,3.278,0.0282,2.4416,0.594,
     & 999,0.03,-0.00196,3.3236,0.0263,2.4162,0.6012/
     
      READ(VOLEQ(8:10),'(I3)')SPN
      CNT = 18
      IDX = 0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN 
      ENDIF
      
      IF(IDX.GT.0.AND.SPN.EQ.CUCOEF(IDX,1)) THEN
        A1 = CUCOEF(IDX,2)
        A2 = CUCOEF(IDX,3)
        A3 = CUCOEF(IDX,4)
        A4 = CUCOEF(IDX,5)
        A5 = CUCOEF(IDX,6)
        A6 = CUCOEF(IDX,7)
        
        DBH = DBHOB
!       Height to 4" top is required for the volume from stump to 4" top        
        IF(HT2PRD.LT.0.1)THEN
          ERRFLG = 8
          RETURN
        ENDIF
        CV4 = A1+A2*DBH**A3+A4*DBH**A5*HT2PRD**A6
        VOL(4) = CV4
        IF(BFMIND.LT.0.1)THEN
          IF(SPN.LT.300)THEN
            BFMIND = 9.0
          ELSE
            BFMIND = 11.0
          ENDIF
        ENDIF
        IF(DBH.GE.BFMIND)THEN
          IF(HT1PRD.LT.0.1)THEN
            ERRFLG = 7
            RETURN
          ENDIF
          B1 = BDCOEF(IDX,2)
          B2 = BDCOEF(IDX,3)
          B3 = BDCOEF(IDX,4)
          B4 = BDCOEF(IDX,5)  
          B5 = BDCOEF(IDX,6)
          B6 = BDCOEF(IDX,7)
          IF(SPN.LT.300)THEN
            IF(DBH.GE.9.0.AND.DBH.LE.10.9)THEN
              UPSRATIO = 0.1589
            ELSEIF (DBH .GE. 11.0 .AND. DBH .LE.12.9) THEN
              UPSRATIO = 0.1300
            ELSEIF (DBH .GE. 13.0 .AND. DBH .LE.14.9) THEN
              UPSRATIO = 0.1060
            ELSEIF (DBH .GE. 15.0 .AND. DBH .LE.16.9) THEN
              UPSRATIO = 0.0900
            ELSEIF (DBH .GE. 17.0 .AND. DBH .LE.18.9) THEN
              UPSRATIO = 0.0775
            ELSEIF (DBH .GE. 19.0 .AND. DBH .LE.20.9) THEN
              UPSRATIO = 0.0700
            ELSEIF (DBH .GE. 21.0) THEN
              UPSRATIO = 0.0650
            END IF;
          ELSE
            IF (DBH .GE. 9.0 .AND. DBH .LE.10.9) THEN
              UPSRATIO = 0.0
            ELSEIF (DBH .GE. 11.0 .AND. DBH .LE.12.9) THEN
              UPSRATIO = 0.2640
            ELSEIF (DBH .GE. 13.0 .AND. DBH .LE.14.9) THEN
              UPSRATIO = 0.1900
            ELSEIF (DBH .GE. 15.0 .AND. DBH .LE.16.9) THEN
              UPSRATIO = 0.1600
            ELSEIF (DBH .GE. 17.0) THEN
              UPSRATIO = 0.1500
            END IF;
          END IF;  
          
          TOPVOL = CV4*UPSRATIO
          SAWCU = CV4 - TOPVOL
          VOL(7) = TOPVOL
          VOL(4) = SAWCU
          BD = B1+B2*DBH**B3+B4*DBH**B5*HT1PRD**B6
          VOL(10) = BD
        ENDIF
      ENDIF  
      RETURN
      END SUBROUTINE SCOTT_VOL    