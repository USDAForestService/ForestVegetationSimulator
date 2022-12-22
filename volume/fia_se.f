      SUBROUTINE FIA_SE(BEQ, DBHOB, HTTOT, VOL, BMS)
      CHARACTER(12) BEQ
      CHARACTER(3) COMP
      CHARACTER(10) VOLEQ
      REAL DBHOB, HTTOT, BMS, DBH, D2H, BFMIND
      REAL VOL(15)
      INTEGER SPN, SPLIST(30), DONE, LAST, ERRFLG, I
      REAL A1(30), A2(30), A3(30), B1(30), B2(30), B3(30)
      INTEGER MDL(30)
      REAL GRN_TOT, GRN_MERCH, VOL_RAT

      DATA (SPLIST(I),I=1,30)/
     + 43, 90,107,110,111,121,123,129,131,132, 
     +221,316,400,491,531,540,611,621,693,740,
     +802,806,812,823,827,832,833,835,842,999/

      DATA (A1(I),I=1,30)/
     +0.35315,0.35315,0.48066,0.29,   0.48066,
     +0.48066,0.48066,0.30038,0.30038,0.48066,
     +0.35315,0.36353,0.43476,0.48009,0.48248,
     +0.33939,0.34061,0.33899,0.5291, 0.35315,
     +0.4659, 0.55167,0.55167,0.48248,0.55167,
     +0.44567,0.55167,0.45539,0.46035,0.43787/

      DATA (A2(I),I=1,30)/
     +0.89767,0.89767,0.88296,0.8939, 0.88296,
     +0.88296,0.88296,0.88366,0.88366,0.88296,
     +0.89767,0.9107, 0.87668,0.90174,0.86809,
     +0.88117,0.88255,0.8918, 0.83757,0.89767,
     +0.8613, 0.8414, 0.8414, 0.86809,0.8414,
     +0.87324,0.8414, 0.86019,0.92557,0.87427/

      DATA (A3(I),I=1,30)/
     + 1, 1, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94, 0.94,
     + 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     + 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/

      DATA (B1(I),I=1,30)/
     +49.18587, 0.30668, 0.23921,-56.89566,-25.58159,
     +-44.4188,-10.59459,0.32519,-10.59459,49.18587,
     +0.09605,0.23088,0.08827,0.12438,0.12438,
     +0.35658,0.09605,0.11221,0.09255,0.09625,
     +0.13343,0.19275,0.06632,0.13343,0.26857,
     +0.11649,0.18579,0.13343,0.46035,0.43787/

      DATA (B2(I),I=1,30)/
     +0.1821,0.93426,0.96994,0.19112,0.18734,
     +0.20297,0.17868,0.91445,0.17868,0.1821,
     +1.05684,0.96726,1.08086,1.05211,1.05211,
     +0.89812,1.05684,1.02985,1.05526,1.05014,
     +1.04502,1.00974,1.11245,1.04502,0.96981,
     +1.04876,1.00655,1.04502,0.92557,0.87427/

      DATA (B3(I),I=1,30)/
     + 1, 1, 1, 0.96, 0.96, 0.96, 0.96, 1, 0.96, 1,
     + 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     + 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /

      DATA (MDL(I),I=1,30)/
     + 2, 4, 4, 2, 2, 2, 2, 4, 2, 2,
     + 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     + 4, 4, 4, 4, 4, 4, 4, 4, 4, 4/


      READ(BEQ(4:6),'(I3)') SPN
      COMP = BEQ(7:9)
      LAST = 30
      DONE = 0
      DBH = DBHOB

C     First check the species for in the SPLIST
      CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
      IF(DONE.GT.0) THEN
        D2H = DBHOB**2*HTTOT
        IF(DBHOB.LT.5) THEN
          GRN_TOT = A1(DONE)*D2H**A2(DONE)
          GRN_MERCH = GRN_TOT*A3(DONE)
        ELSE
          IF(MDL(DONE).EQ.4) THEN
            GRN_TOT = B1(DONE)*D2H**B2(DONE)
          ELSEIF(MDL(DONE).EQ.2) THEN
            GRN_TOT = B1(DONE) + D2H*B2(DONE)
          ENDIF
C         Adjust to remove stump
          GRN_MERCH = GRN_TOT*B3(DONE)
C         Convert from total stem to 4 inch top biomass
          VOL = 0.0
          BFMIND = 0.0
          ERRFLG = 0
          VOLEQ = 'S00SRS0999'
          VOLEQ(8:10) = BEQ(4:6)
          CALL SRS_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
          IF(VOL(7).GT.0.0)THEN
            VOL(4) = VOL(4) + VOL(7)
            VOL(7) = 0.0
          ENDIF
          IF(VOL(1).GT.0.AND.VOL(4).GT.0) THEN
            VOL_RAT = VOL(4)/VOL(1)
            GRN_MERCH = GRN_TOT*VOL_RAT
          ENDIF
        ENDIF


C       Return the biomass for the BEQ       
C       Total stem wood 
        IF(COMP.EQ.'STW')THEN
          BMS = GRN_TOT
C       Merch stem wood
        ELSEIF(COMP.EQ.'MSW')THEN
          BMS = GRN_MERCH
        ENDIF
        
        IF(BEQ(12:12).EQ.'D') BMS = BMS*0.5


      ENDIF
      END
