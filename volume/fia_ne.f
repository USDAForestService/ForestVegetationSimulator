      SUBROUTINE FIA_NE(BEQ, DBHOB, HTTOT, VOL, BMS)
      CHARACTER(12) BEQ
      CHARACTER(3) COMP
      REAL DBHOB, HTTOT, BMS, DBH
      REAL VOL(15)
      INTEGER SPN, SPLIST(21), DONE, FIRST, LAST, ERRFLG, I
      REAL Y1(21), Y2(21)
      INTEGER MDL(21)
      REAL ABVGRD_TOT, PCT_FOL, PCT_BRA, PCT_STP_RT, PCT_MST
      REAL M3, PCT_STEM, PCT_STEM_BRA

      DATA (SPLIST(I),I=1,21)/
     + 12, 94,129,131,132,241,261,310,372,746, 
     +400,421,611,802,832,833,835,837,318,531,999/

      DATA (Y1(I),I=1,21)/
     +0.5958,0.8079,0.4080,0.8162,0.7157,
     +1.1182,0.6803,0.9392,1.1297,0.4689,
     +1.9338,1.5779,2.5883,1.2892,2.1202,
     +1.6891,2.6574,2.1457,5.2480,5.3373,3.2031/

      DATA (Y2(I),I=1,21)/
     +2.4017,2.3316,2.4490,2.2453,2.3865,
     +1.9269,2.3617,2.3804,2.3376,2.6087,
     +2.6209,2.5153,2.4253,2.7010,2.5344,
     +2.6598,2.4395,2.5030,-0.3661,-0.3257,-0.2337/

      DATA (MDL(I),I=1,21)/
     +1,1,1,1,1,1,1,1,1,1,
     +2,2,2,2,2,2,2,2,3,3,3/


      READ(BEQ(4:6),'(I3)') SPN
      COMP = BEQ(7:9)
      FIRST = 1
      LAST = 21
      DONE = 0
      DBH = DBHOB

C     First check the species for in the SPLIST
      !CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
      DO 10 I = FIRST, LAST
        IF(SPN.EQ.SPLIST(I))THEN
          DONE = I
          EXIT
        ENDIF
10    CONTINUE
      IF(DONE.GT.0) THEN
C       First calculate the total above ground dry biomass including stump, branches and foliage
C       Created from FCS_TREE_BIOM_NERS written by Carol Alerich
        IF (MDL(DONE).EQ.1) THEN
          ABVGRD_TOT = EXP(Y1(DONE) + Y2(DONE)*LOG(DBH))  
        ELSEIF(MDL(DONE).EQ.2) THEN
          ABVGRD_TOT = 10**(LOG10(Y1(DONE)) + Y2(DONE)*LOG10(DBH))
        ELSEIF(MDL(DONE).EQ.3) THEN
          IF(SPN.EQ.318) THEN
            M3 = 0.0076
          ELSEIF(SPN.EQ.531) THEN
            M3 = 0.0072
          ELSEIF(SPN.EQ.999) THEN
            M3 = 0.0061
          ENDIF
         ABVGRD_TOT=(Y1(DONE)+Y2(DONE)*DBH*25.4+M3*(DBH*25.4)**2)*2.2046
        ENDIF     
    
C       Then calculate component ratio
        IF(SPN.LT.300) THEN
          IF(DBH.GE.5.AND.DBH.LT.9) THEN
            PCT_BRA = 0.1402
            PCT_FOL = 0.1126
            PCT_STP_RT = 0.2552
            PCT_MST = 0.6503
          ELSEIF(DBH.GE.9.AND.DBH.LT.15) THEN
            PCT_BRA = 0.141
            PCT_FOL = 0.096
            PCT_STP_RT = 0.2547
            PCT_MST = 0.7695
          ELSEIF(DBH.GE.15.AND.DBH.LT.21) THEN
            PCT_BRA = 0.1427
            PCT_FOL = 0.086
            PCT_STP_RT = 0.255
            PCT_MST = 0.8053
          ELSEIF(DBH.GE.21) THEN
            PCT_BRA = 0.1457
            PCT_FOL = 0.0768
            PCT_STP_RT = 0.2563
            PCT_MST = 0.8136
          ENDIF
        ELSE
          IF(DBH.GE.5.AND.DBH.LT.11) THEN
            PCT_BRA = 0.1093
            PCT_FOL = 0.0348
            PCT_STP_RT = 0.2673
            PCT_MST = 0.7269
          ELSEIF(DBH.GE.11.AND.DBH.LT.15) THEN
            PCT_BRA = 0.0991
            PCT_FOL = 0.0268
            PCT_STP_RT = 0.252
            PCT_MST = 0.7737
          ELSEIF(DBH.GE.15.AND.DBH.LT.21) THEN
            PCT_BRA = 0.0932
            PCT_FOL = 0.0224
            PCT_STP_RT = 0.2412
            PCT_MST = 0.7852
          ELSEIF(DBH.GE.21) THEN
            PCT_BRA = 0.086
            PCT_FOL = 0.0175
            PCT_STP_RT = 0.231
            PCT_MST = 0.7884
          ENDIF
        ENDIF
        PCT_STEM = 1 - PCT_BRA - PCT_FOL
        PCT_STEM_BRA = 1 - PCT_FOL

C       calculate component biomass with ratio        

C       Return the biomass for the BEQ       
C       Total stem  
        IF(COMP.EQ.'STT')THEN
          BMS = ABVGRD_TOT*PCT_STEM
          IF(DBH.LT.5) BMS = 0
C       Merch stem total
        ELSEIF(COMP.EQ.'MST')THEN
          BMS = ABVGRD_TOT*PCT_MST
C       BRANCHES
        ELSEIF(COMP.EQ.'BRT') THEN
          BMS = ABVGRD_TOT*PCT_BRA
C       FOLIAGE
        ELSEIF(COMP.EQ.'FOT') THEN
          BMS = ABVGRD_TOT*PCT_FOL
C       STUMP+ROOT
        ELSEIF(COMP.EQ.'SMR') THEN
          BMS = ABVGRD_TOT*PCT_STP_RT
C       STEM+BRANCHES
        ELSEIF(COMP.EQ.'AWB') THEN
          IF(DBH.LT.5) THEN
            BMS = EXP(0.95595 + 2.4264*LOG(DBH))
          ELSE
            BMS = ABVGRD_TOT*PCT_STEM_BRA
          ENDIF
C       ABOVE GROUND TOTAL
        ELSEIF(COMP.EQ.'AGT') THEN
          BMS = ABVGRD_TOT
        ENDIF
      ELSE        
        ERRFLG = 15
      ENDIF
      END
