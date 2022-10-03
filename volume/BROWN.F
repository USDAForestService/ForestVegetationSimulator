      SUBROUTINE BROWN(BIOEQ, DBHOB, HTTOT, CR, BIOMAS, ERRFLG)
      IMPLICIT NONE
      REAL DBHOB, HTTOT, CR, CFWT(7), BIOMAS
      CHARACTER*12 BIOEQ
      INTEGER SPEC, ERRFLG,SPLIST(24),LAST,DONE,I
      CHARACTER*3 CMP
C  YW 2019/05/01 changed the biomass component code to B0Q and BQ1  
C Valid species code for Brown's fraction biomasss  
C 011,015,017,019,020,021,022,041,042,073,081,093,098
C 101,108,116,117,119,121,122,202,242,260,263  
C List of Brown fraction equations:
C BRN***FOT00D, BRN***B0Q00D,BRN***BQ100D,BRN***B1300D,BRN***B3P00D,
C BRN***LCR00D,BRN***DCR00D, 
      DATA (SPLIST(I), I=1,24)/
     + 011,015,017,019,020,021,022,041,042,073,081,093,
     + 098,101,108,116,117,119,121,122,202,242,260,263/
      CMP = BIOEQ(7:9)
      READ (BIOEQ(4:6),'(I3)') SPEC
C     First check the species for in the SPLIST
      LAST = 24
      DONE = 0
      CALL SEARCH(LAST,SPLIST,SPEC,DONE,ERRFLG)
      IF(DONE.LE.0)THEN
        ERRFLG = 15
        RETURN
      ENDIF
      CALL BROWNCROWNFRACTION(SPEC, DBHOB, HTTOT,CR, CFWT)
      IF(CMP.EQ.'FOT')THEN
C     1 - NEEDLES  
        BIOMAS = CFWT(1)
      ELSEIF(CMP.EQ.'B0Q'.OR.CMP.EQ.'B04')THEN
C     2 - 0 TO 1/4"      
        BIOMAS = CFWT(2)
      ELSEIF(CMP.EQ.'BQ1'.OR.CMP.EQ.'B41')THEN
C     3 - 1/4 TO 1"      
        BIOMAS = CFWT(3)
      ELSEIF(CMP.EQ.'B13')THEN
C     4 - 1 TO 3"      
        BIOMAS = CFWT(4)
      ELSEIF(CMP.EQ.'B3P')THEN
C     5 - 3" AND UP      
        BIOMAS = CFWT(5)
      ELSEIF(CMP.EQ.'LCR')THEN
C     6 - Live Crown      
        BIOMAS = CFWT(6)
      ELSEIF(CMP.EQ.'DCR')THEN
C     7 - Dead crown      
        BIOMAS = CFWT(7)
      ELSE
        ERRFLG = 15
        RETURN
      ENDIF
      
      RETURN
      END
C **************************************************************************************      
C     THIS SUBROUTINE CALCULATE CROWN BIOMASS (DRY WEIGHT) FRACTIONS BASED ON BROWN 1976
C     THE RESULT FOR SIZE CATEGORY IN CWF AS:
C     1 - NEEDLES
C     2 - 0 TO 1/4"
C     3 - 1/4 TO 1"
C     4 - 1 TO 3"
C     5 - 3" AND UP
C     6 - Live Crown
C     7 - Dead crown
      SUBROUTINE BROWNCROWNFRACTION(SPCD, DBH, THT,CR, CFWT)
! Expose subroutine BrownCrownFraction to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCROWNFRACTION
      IMPLICIT NONE
      REAL DBH, THT, CFWT(7), CR
      INTEGER SPCD, PROP
      REAL LCW, DCW, LP1, LP2, LP3, LP4, DP1, DP2 
      REAL DST1, DST2, DST3, SW
      REAL CONST2, CONST3, CONST4
      REAL LIVEPROP, DEADPROP
C test biomass equation 2019/05/08
!      CHARACTER*12 BMEQ
!      REAL tmpD,tmpH,tmpCR
!      BMEQ = 'AFF019BRL01D'
!      tmpD = 15.0
!      tmpH = 65.0
!      tmpCR = 30.0/65.0
!      CALL CALCFROMBIOEQDB(BMEQ,tmpD,tmpH,tmpCR,DCW)
C end test biomass equation 2019/05/08            
      LCW = 0.0
      DCW = 0.0
C     ASSOCIATE SPECIES CODE WITH BROWN SPECIES CODE
      IF(SPCD.EQ.15.OR.SPCD.EQ.11.OR.SPCD.EQ.21.OR.SPCD.EQ.22)THEN
        SPCD = 17
      ELSEIF(SPCD.EQ.20)THEN
        SPCD = 19
      ELSEIF(SPCD.EQ.116.OR.SPCD.EQ.117.OR.SPCD.EQ.121)THEN
        SPCD = 108
      ELSEIF(SPCD.EQ.98)THEN
        SPCD = 93
      ELSEIF(SPCD.EQ.260)THEN
        SPCD = 263
      ELSEIF(SPCD.EQ.41.OR.SPCD.EQ.42.OR.SPCD.EQ.81)THEN
        SPCD = 242
      ENDIF
      CALL LIVECROWNWEIGHT(SPCD, DBH, THT, CR, LCW)
      
C     SMALL TREES AND BIG TREES ARE CALCULATED DIFFERENT FOR PROPORTION
      IF(DBH.LE.6)THEN
        CALL SMALLTREESTEMWEIGHT(SPCD, DBH, THT, SW)
        DST1 = LIVEPROP(SPCD, DBH, 1)
        DST2 = LIVEPROP(SPCD, DBH, 2)
        DST3 = LIVEPROP(SPCD, DBH, 3)
C       NEEDLES        
        CFWT(1) = LCW*DST1   
C       0 TO 1/4"        
        CFWT(2) = LCW*(DST2-DST1)   
C       1/4 TO 1"
        IF(DBH.GT.1.AND.DBH.LE.2)THEN
          CONST2 = 0.02
        ELSE
          CONST2 = 1.0
        ENDIF
        CFWT(3) = LCW*(DST3-DST2) + SW*CONST2
C       1 TO 3"
        IF(DBH.LE.2)THEN
          CONST3 = 0.96
        ELSEIF(DBH.LE.3)THEN
          CONST3 = 0.5
        ELSEIF(DBH.LE.4)THEN
          CONST3 = 0.1
        ELSE
          CONST3 = 0
        ENDIF
        CFWT(4) = LCW*(1-DST3) + CONST3*SW
C       3" AND UP
        IF(DBH.LE.3)THEN
          CONST4 = 0.5
        ELSEIF(DBH.LE.4)THEN
          CONST4 = 0.9
        ELSE
          CONST4 = 1.0
        ENDIF
        CFWT(5) = SW*CONST4
        IF(DBH.LT.3) CFWT(5) = 0  
      ELSE
        CALL DEADCROWNWEIGHT(SPCD, DBH, THT,CR, DCW)
        LP1 = LIVEPROP(SPCD, DBH, 1)
        LP2 = LIVEPROP(SPCD, DBH, 2)
        LP3 = LIVEPROP(SPCD, DBH, 3)
        LP4 = LIVEPROP(SPCD, DBH, 4)
        DP1 = DEADPROP(SPCD, DBH, 1)
        DP2 = DEADPROP(SPCD, DBH, 2)
        !NEEDLES
        CFWT(1) = LP1*LCW
        ! 0 TO 1/4"
        CFWT(2) = (LP2-LP1)*LCW +DP1*DCW
        ! 1/4 TO 1"
        CFWT(3) = (LP3-LP2)*LCW + (DP2-DP1)*DCW
        ! 1 TO 3" (ONLY CALCULATE FOR PP, DF, WP, GF, WC AND LP
        IF(SPCD.EQ.122.OR.SPCD.EQ.202)THEN
          CFWT(4) = (LP4-LP3)*LCW + (1-DP2)*DCW
        ELSEIF(SPCD.EQ.17 .OR. SPCD.EQ.119
     +        .OR. SPCD.EQ.108 .OR. SPCD.EQ.242)THEN
          CFWT(4) = (1-LP3)*LCW + (1-DP2)*DCW
        ELSE
          CFWT(4) = 0
        ENDIF
        ! 3" AND UP (ONLY FOR PP AND DF)
        IF(SPCD.EQ.122.OR.SPCD.EQ.202)THEN
          CFWT(5) = (1-LP4)*LCW
        ELSE
          CFWT(5) = 0
        ENDIF
      ENDIF
      CFWT(6) = LCW
      CFWT(7) = DCW
      END                          
C ---------------------------------------------------------------------
C Brown topwood dry weight = gross CUFT sedondary volume times breakage ratio times wood density
      SUBROUTINE BROWNTOPWOOD(SPN, GCUFTS, WT)
! Expose subroutine BrownTopwood to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNTOPWOOD
      IMPLICIT NONE
      REAL GCUFTS, WT, WOODDRYWEIGHT
      INTEGER SPN
!      !test===============================================================
!      CHARACTER*10 VOLEQ,FIAVTYPE,VOLEQ2,GEOSUB,FIAEQ
!      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
!      REAL DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAVOL
!      REAL CENTROID_HT,CENTROID_DIA
!      INTEGER BA,SI,ERRFLG
!      VOLEQ = 'CU108102'
!      SPN = 108
!      DBHOB = 15
!      HTTOT = 76
!      HT1PRD = 0.0
!      HT2PRD = 0.0
!      MTOPP = 6.0
!      STUMP = 1.0
!      DRCOB = 0.0
!      UPSHT1 = 0.0
!      UPSD1 = 0.0
!      UPSHT2 = 0.0
!      UPSD2 = 0.0
!      VOL = 0.0
!      BA = 85
!      SI = 65
!      GEOSUB = '0'
!      ERRFLG = 0
!      FIAVOL = 0.0
!      BFMIND = 9
!      CALL FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
!     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,
!     & FIAVTYPE,FIAVOL,BFMIND)
!      !end test==========================================================
      WT = WOODDRYWEIGHT(SPN, GCUFTS, 1)
      END
C ---------------------------------------------------------------------
C BROWN CULLLOG WEIGHT = GROSS LOG VOLUME TIMES BARK WEIGHT/STEM WEIGHT RATIO TIMES WOOD DENSITY
      SUBROUTINE BROWNCULLLOG(SPN, GCUFTS, WT)
! Expose subroutine BrownCullLog to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCULLLOG
      IMPLICIT NONE
      REAL GCUFTS, WT, WOODDRYWEIGHT
      INTEGER SPN
      WT = WOODDRYWEIGHT(SPN, GCUFTS, 0)
      END
C --------------------------------------------------------------------- 
C BROWN CULL CHUNK WEIGHT = GROSS CUFT MINUS NET CUFT VOLUME TIMES FRACTION LEFT IN THE WOOD
      SUBROUTINE BROWNCULLCHUNK(SPN, GCUFT, NCUFT, FLIW, WT)
! Expose subroutine BrownCullChunk to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::BROWNCULLCHUNK
      IMPLICIT NONE
      REAL GCUFT, NCUFT, FLIW, WT, WOODDRYWEIGHT, VOL
      INTEGER SPN
      VOL = (GCUFT - NCUFT)*FLIW
      WT = WOODDRYWEIGHT(SPN, VOL, 0);
      END
C ---------------------------------------------------------------------
      SUBROUTINE LIVECROWNWEIGHT(SPCD, DBH, THT, CR, LCW)
C     THIS SUBROUTINE CALCULATE LIVE CROWN DRY WEIGHT
      IMPLICIT NONE
      REAL DBH, THT, LCW, CR, DF34PLUSLCW
      INTEGER SPCD
      CHARACTER*12 BMEQ
      DF34PLUSLCW(DBH) = EXP(0.0168 + 1.949*LOG(DBH))
      LCW = -1
      IF(DBH.GT.50) DBH = 50
      IF(DBH.LT.1) DBH = 1
      IF(SPCD.EQ.17)THEN
c       eqn 305      
        BMEQ = 'BRN017LCR02D'
        IF(DBH.GT.40) LCW = 1.03*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.19)THEN
c       eqn 312      
        BMEQ = 'BRN019LCR01D'
      ELSEIF(SPCD.EQ.108)THEN
c       eqn 323      
        BMEQ = 'BRN108LCR01D'
      ELSEIF(SPCD.EQ.73)THEN
c       eqn 315      
        BMEQ = 'BRN073LCR01D'
        IF(DBH.GT.34) LCW = 0.58*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.93)THEN
c       eqn 317      
        BMEQ = 'BRN093LCR01D'
        IF(DBH.GT.28) LCW = 1.24*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.202)THEN
        IF(DBH.LE.16)THEN
c       eqn 337        
          BMEQ = 'BRN202LCR01D'
        ELSEIF(DBH.LE.32)THEN
c       eqn 339
          BMEQ = 'BRN202LCR03D'
        ELSE
          LCW = DF34PLUSLCW(DBH)
        ENDIF
      ELSEIF(SPCD.EQ.263)THEN
        IF(DBH.LE.32)THEN
c       eqn 350
          BMEQ = 'BRN263LCR01D'
        ELSE
c       eqn 4146
          BMEQ = 'BRN263LCR04D'
        ENDIF
      ELSEIF(SPCD.EQ.119)THEN
c       eqn 324
        BMEQ = 'BRN119LCR01D'
        IF(DBH.GT.42) LCW = 0.45*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.242)THEN
c       eqn 345
        BMEQ = 'BRN242LCR01D'
        IF(DBH.GT.36) LCW = 0.77*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.122)THEN
c       eqn 330
        BMEQ = 'BRN122LCR01D'
        IF(DBH.GT.34) LCW = 2.0*DF34PLUSLCW(DBH)
      ELSEIF(SPCD.EQ.101)THEN
        IF(DBH.LE.6)THEN
c       eqn 324
          BMEQ = 'BRN119LCR01D'
        ELSE
c       eqn 323
          BMEQ = 'BRN108LCR01D'
        ENDIF
      ENDIF
      IF(LCW.LT.0)THEN
        LCW = 0
        CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
      ENDIF
      END
C ---------------------------------------------------------------------
      SUBROUTINE DEADCROWNWEIGHT(SPCD, DBH, THT, CR, DCW)
      IMPLICIT NONE
C     THIS SUBROUTINE CALCULATE LIVE CROWN DRY WEIGHT
      REAL DBH, THT, DCW, P3, LCW, CR
      INTEGER SPCD
      CHARACTER*12 BMEQ
      P3 = 0
      DCW = -1
      IF(DBH.LE.6) RETURN
      IF(DBH.GT.50) DBH = 50
      IF(SPCD.EQ.17)THEN
        IF(DBH.LE.18)THEN
c       eqn 310
          BMEQ = 'BRN017DCR02D'
        ELSEIF(DBH.LE.20)THEN
c       eqn 309
          BMEQ = 'BRN017LCR02D'
          CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
          DCW = LCW*0.38
        ELSE
          DBH = 20
c       eqn 310
          BMEQ = 'BRN017DCR02D'
        ENDIF
      ELSEIF(SPCD.EQ.19)THEN
        IF(DBH.LE.16)THEN
c       eqn 313
          BMEQ = 'BRN019DCR01D'
        ELSE
c       eqn 312
          BMEQ = 'BRN019LCR01D'
          CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
          DCW = LCW*0.31
        ENDIF
      ELSEIF(SPCD.EQ.108)THEN
c       eqn 323
        BMEQ = 'BRN108LCR01D'
        CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
        DCW = LCW*0.235  
      ELSEIF(SPCD.EQ.73)THEN
        DCW = 0
      ELSEIF(SPCD.EQ.93)THEN
        IF(DBH.GT.22) DBH = 22
c       eqn 318
        BMEQ = 'BRN093DCR01D'
      ELSEIF(SPCD.EQ.202)THEN
        IF(DBH.GT.34) DBH = 34
c       eqn 341
        BMEQ = 'BRN202DCR02D'
      ELSEIF(SPCD.EQ.263)THEN
        IF(DBH.GT.20) DBH = 21
c       eqn 351
        BMEQ = 'BRN263DCR01D'
      ELSEIF(SPCD.EQ.119)THEN
        IF(DBH.GT.24) DBH = 25
c       eqn 326
        BMEQ = 'BRN119DCR01D'
      ELSEIF(SPCD.EQ.242)THEN
        IF(DBH.GT.26) DBH = 27
c       eqn 347
        BMEQ = 'BRN242DCR01D'
      ELSEIF(SPCD.EQ.122)THEN
        IF(DBH.GT.34) DBH = 34
c       eqn 332
        BMEQ = 'BRN122DCR01D'
      ELSEIF(SPCD.EQ.101)THEN
c       eqn 324
        BMEQ = 'BRN119LCR01D'
        CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,LCW)
        DCW = LCW*0.235
      ENDIF
      IF(DCW.LT.0) CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, CR,DCW)
      IF(DCW.LT.0) DCW = 0
      END
C ---------------------------------------------------------------------
C     DAM SMALL TREE STEM WEIGHT
      SUBROUTINE SMALLTREESTEMWEIGHT(SPN, DBH, THT, STSWT)
      IMPLICIT NONE
      REAL DBH, THT, STSWT, P3
      INTEGER SPN
      CHARACTER*12 BMEQ
      STSWT = -1
      P3 = 0
      
      IF(SPN.EQ.17)THEN
        IF(DBH.LE.4)THEN
c       eqn 304
          BMEQ = 'BRN017STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 62
        ELSEIF(DBH.LE.6)THEN
          STSWT = 106
        ENDIF
      ELSEIF(SPN.EQ.19)THEN
        IF(DBH.LE.4)THEN
c       eqn 5025
          BMEQ = 'BRN019STT11D'
        ELSE
          STSWT = 0
        ENDIF
      ELSEIF(SPN.EQ.108)THEN
        IF(DBH.LE.4)THEN
c       eqn 322
          BMEQ = 'BRN108STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 83
        ELSEIF(DBH.LE.6)THEN
          STSWT = 134
        ENDIF  
      ELSEIF(SPN.EQ.73)THEN
        IF(DBH.LE.4)THEN
c       eqn 5022
          BMEQ = 'BRN073STT11D'
        ELSE
          STSWT = 0
        ENDIF
      ELSEIF(SPN.EQ.93)THEN
        IF(DBH.LE.4)THEN
c       eqn 4148
          BMEQ = 'BRN093STT02D'
        ELSE
          STSWT = 0
        ENDIF
      ELSEIF(SPN.EQ.202)THEN
        IF(DBH.LE.4)THEN
c       eqn 327
          BMEQ = 'BRN122STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 69
        ELSEIF(DBH.LE.6)THEN
          STSWT = 113
        ENDIF
      ELSEIF(SPN.EQ.263)THEN
        IF(DBH.LE.4)THEN
c       eqn 5021
          BMEQ = 'BRN263STT11D'
        ELSE
          STSWT = 0
        ENDIF
      ELSEIF(SPN.EQ.119)THEN
        IF(DBH.LE.4)THEN
c       eqn 325
          BMEQ = 'BRN119STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 50
        ELSEIF(DBH.LE.6)THEN
          STSWT = 82
        ENDIF
      ELSEIF(SPN.EQ.242)THEN
        IF(DBH.LE.4)THEN
c       eqn 342
          BMEQ = 'BRN242STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 45
        ELSEIF(DBH.LE.6)THEN
          STSWT = 73
        ENDIF
      ELSEIF(SPN.EQ.122)THEN
        IF(DBH.LE.4)THEN
c       eqn 4147
          BMEQ = 'BRN122STT04D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 50
        ELSEIF(DBH.LE.6)THEN
          STSWT = 82
        ENDIF
      ELSEIF(SPN.EQ.101)THEN
        IF(DBH.LE.4)THEN
c       eqn 325
          BMEQ = 'BRN119STT01D'
        ELSEIF(DBH.LE.5)THEN
          STSWT = 50
        ELSEIF(DBH.LE.6)THEN
          STSWT = 82
        ENDIF
      ELSE
        STSWT = 0    
      ENDIF
      IF(STSWT.LT.0) CALL CALCFROMBIOEQDB(BMEQ, DBH, THT, P3, STSWT)
      IF(STSWT.LT.0) STSWT = 0
      RETURN
      END  
            
C ---------------------------------------------------------------------      
C     FUNCTION TO CALCULATE DOUGLAS FIR LIVE CROWN FOR DBH > 34
c      REAL FUNCTION DF34PLUSLCW(DBH) 
c      IMPLICIT NONE
c      REAL DBH
c      DF34PLUSLCW = EXP(0.0168 + 1.949*LOG(DBH))
c      RETURN
c      END
C ---------------------------------------------------------------------
      REAL FUNCTION PROPFUNC1(A, B, DBH)
      IMPLICIT NONE
      REAL A, B, DBH
      PROPFUNC1 = A + B*DBH
      RETURN
      END
C ---------------------------------------------------------------------
      REAL FUNCTION PROPFUNC2(A, B, DBH)
      IMPLICIT NONE
      REAL A, B, DBH
      PROPFUNC2 = A*EXP(B*DBH)
      RETURN
      END                 
C ---------------------------------------------------------------------
C     THE FUNCTION CALC LIVE PROP FOR LP1, LP2, LP3 AND LP4
      REAL FUNCTION LIVEPROP(SPN, DBH, PROP)     
      IMPLICIT NONE
      REAL DBH, LP, PROPFUNC1, PROPFUNC2
      INTEGER SPN, PROP
      IF(SPN.EQ.17)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.34)THEN
            LP = 1/PROPFUNC1(1.5916, 0.05294, DBH)
          ELSE
            LP = 0.286
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.34)THEN
            LP = 1/PROPFUNC1(1.1495, 0.04165, DBH)
          ELSE
            LP = 0.378
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1
          ELSEIF(DBH.LE.34)THEN
            LP = PROPFUNC1(1.0267, -0.01495, DBH)
          ELSE
            LP = 0.488
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.19)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC2(0.5966, -0.04247, DBH)
        ELSEIF(PROP.EQ.2)THEN
          LP = PROPFUNC2(0.8643, -0.03733, DBH)
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC1(1.0221, -0.01083, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.108)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC1(0.4933, -0.01167, DBH)
        ELSEIF(PROP.EQ.2)THEN
          LP = PROPFUNC1(0.7767, -0.01464, DBH)
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.3)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC1(1.0494, -0.01464, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.73)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC2(0.3468, -0.04343, DBH)
        ELSEIF(PROP.EQ.2)THEN
          LP = PROPFUNC2(0.745, -0.03622, DBH)
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC2(1.05448, -0.0213, DBH)
          ENDIF
        ELSEIF(PROP.EQ.4)THEN
          IF(DBH.LE.12)THEN
            LP = 1.0
          ELSE
            LP =0.9223 + 0.7197/DBH
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.93)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.40)THEN
            LP = PROPFUNC2(0.5783, -0.325, DBH)
          ELSE
            LP = 0.158
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.40)THEN
            LP = PROPFUNC2(0.8519, -0.02811, DBH)
          ELSE
            LP = 0.277
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2) THEN
            LP = 1.0
          ELSEIF(DBH.LE.40)THEN
            LP = PROPFUNC1(1.03781, -0.01537, DBH)
          ELSE
            LP = 0.423
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.202)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.36)THEN
            LP = PROPFUNC2(0.484, -0.02102, DBH)
          ELSE
            LP = 0.227
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.36)THEN
            LP = PROPFUNC2(0.7289, -0.02332, DBH)
          ELSE
            LP = 0.315
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSEIF(DBH.LE.36)THEN
            LP = PROPFUNC1(1.0342, -0.01584, DBH)
          ELSE
            LP = 0.465
          ENDIF
        ELSEIF(PROP.EQ.4)THEN
          IF(DBH.LE.14)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC1(1.0221, -0.001821, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.263)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.40)THEN
            LP = PROPFUNC2(0.5474, -0.03697, DBH)
          ELSE
            LP = 0.125
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.40)THEN
            LP = PROPFUNC2(0.8352, -0.03802, DBH)
          ELSE
            LP = 0.183
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSEIF(DBH.LE.40)THEN
            LP = PROPFUNC2(1.0781, -0.02735, DBH)
          ELSE
            LP = 0.361
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.119)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC2(0.5497, -0.03453, DBH)
        ELSEIF(PROP.EQ.2)THEN
          LP = PROPFUNC2(0.9138, -0.09782, DBH)
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC2(1.0564, -0.0181, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.242)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC2(0.6174, -0.02326, DBH)
        ELSEIF(PROP.EQ.2)THEN
          LP = PROPFUNC2(0.7562, -0.02411, DBH)
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.2)THEN
            LP = 1.0
          ELSE
            LP = PROPFUNC2(1.0602, -0.02226, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.122)THEN
        IF(PROP.EQ.1)THEN
          LP = PROPFUNC2(0.5578, -0.04754, DBH)
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.30)THEN
            LP = PROPFUNC2(0.6254, -0.05114, DBH)
          ELSE
            LP = PROPFUNC2(0.5578, -0.04754, DBH) + 0.01
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          LP = PROPFUNC2(0.985, -0.03102, DBH)
        ELSEIF(PROP.EQ.4)THEN
          LP = PROPFUNC1(1.083, -0.01306, DBH)
        ENDIF
      ELSEIF(SPN.EQ.101)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.6)THEN
            LP = PROPFUNC2(0.5497, -0.0345, DBH)
          ELSE
            LP = PROPFUNC1(0.4933, -0.01167, DBH)
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.6)THEN
            LP = 0.9138 - 0.0978*DBH**0.5
          ELSE
            LP = PROPFUNC1(0.7767, -0.01464, DBH)
          ENDIF
        ELSEIF(PROP.EQ.3)THEN
          IF(DBH.LE.6)THEN
            LP = PROPFUNC2(1.0564, -0.0181, DBH)
          ELSE
            LP = PROPFUNC1(1.0494, -0.01402, DBH)
          ENDIF
        ENDIF
      ENDIF
      LIVEPROP = LP
      RETURN
      END
C ---------------------------------------------------------------------
      REAL FUNCTION DEADPROP(SPN, DBH, PROP) 
      IMPLICIT NONE
      REAL DBH, DP, PROPFUNC1, PROPFUNC2
      INTEGER SPN, PROP
      DP = 0
      IF(SPN.EQ.17)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.26)THEN
            DP = PROPFUNC2(1.4336, -0.1816, DBH)
          ELSE
            DP = 0.01
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          DP = PROPFUNC2(1.2623, -0.0347, DBH)
        ENDIF
      ELSEIF(SPN.EQ.19)THEN
        IF(PROP.EQ.1) DP = 1.2105*DBH**(-0.565)
      ELSEIF(SPN.EQ.108)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.20)THEN
            DP = 1.3527*DBH**(-0.7585)
          ELSE
            DP = 0.139
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.20)THEN
            DP = PROPFUNC2(2.7979, -0.1257, DBH)
          ELSE
            DP = 0.226
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.93)THEN
        IF(PROP.EQ.1)THEN
          DP = 1.4657*DBH**(-0.6454)
        ELSEIF(PROP.EQ.2)THEN
          DP = 1/PROPFUNC1(0.847, 0.01678, DBH)
        ENDIF
      ELSEIF(SPN.EQ.202)THEN
        IF(PROP.EQ.1)THEN
          DP = 0.08355 + 1.5893/DBH
        ELSEIF(PROP.EQ.2)THEN
          DP = PROPFUNC2(1.5673, -0.05232, DBH)
        ENDIF
      ELSEIF(SPN.EQ.263)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.28)THEN
            DP = PROPFUNC2(1.9608, -0.2064, DBH)
          ELSE
            DP = 0.005
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.12)THEN
            DP = 1.0
          ELSE
            DP = 1/PROPFUNC1(0.2772, 0.06141, DBH)
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.119)THEN
        IF(PROP.EQ.1)THEN
          DP = 1.0077*DBH**(-0.4556)
        ELSEIF(PROP.EQ.2)THEN
          DP = PROPFUNC1(1.0291, -0.004964, DBH)
        ENDIF
      ELSEIF(SPN.EQ.242)THEN
        IF(PROP.EQ.1)THEN
          DP = -0.01578 + 1.4673/DBH
        ELSEIF(PROP.EQ.2)THEN
          DP = PROPFUNC2(1.4534, -0.05395, DBH)
        ENDIF
      ELSEIF(SPN.EQ.122)THEN
        IF(PROP.EQ.1)THEN
          IF(DBH.LE.30)THEN
            DP = -0.04345 + 1.4114/DBH
          ELSE
            DP = 0.0004
          ENDIF
        ELSEIF(PROP.EQ.2)THEN
          IF(DBH.LE.30)THEN
            DP = PROPFUNC1(1.0621, -0.03342, DBH)
          ELSE
            DP = 0.06
          ENDIF
        ENDIF
      ELSEIF(SPN.EQ.101)THEN
        IF(PROP.EQ.1)THEN
          DP = 1.3527*DBH**(-0.7585)
        ELSEIF(PROP.EQ.2)THEN
          DP = PROPFUNC2(2.7979, -0.1257, DBH)
        ENDIF
      ENDIF
      DEADPROP = DP
      RETURN
      END
C ---------------------------------------------------------------------
      REAL FUNCTION WOODDRYWEIGHT(SPN, GCUFTS, TWFLG)
      IMPLICIT NONE
      REAL GCUFTS, DRYWT, WD, RATIO, RATIO2
      INTEGER SPN, TWFLG
C     TWFLAG IS THE FLAG FOR: 1 = TOPWOOD, 0 = LOG / CHUNK   
      DRYWT = 0
      RATIO = 1.68
      RATIO2 = 1.188
      WD = 20
      IF(SPN.EQ.17)THEN
        WD = 23.1
        RATIO2 = 1.353
      ELSEIF(SPN.EQ.19)THEN
        RATIO2 = 1.26
      ELSEIF(SPN.EQ.108)THEN
        WD = 25.6
        RATIO2 = 1.114
      ELSEIF(SPN.EQ.73)THEN
        WD = 32.4
        RATIO2 = 1.194
      ELSEIF(SPN.EQ.93)THEN
        WD = 21.8
        RATIO2 = 1.274
      ELSEIF(SPN.EQ.202)THEN
        WD = 30
        RATIO2 = 1.188
      ELSEIF(SPN.EQ.263)THEN
        WD = 28.1
        RATIO2 = 1.207
      ELSEIF(SPN.EQ.119)THEN
        WD = 23.7
        RATIO = 1.97
        RATIO2 = 1.232
      ELSEIF(SPN.EQ.242)THEN
        RATIO2 = 1.168
      ELSEIF(SPN.EQ.122)THEN
        WD = 25
        RATIO = 1.97
        RATIO2 = 1.209
      ELSEIF(SPN.EQ.101)THEN
        WD = 23.7
        RATIO = 1.97
        RATIO2 = 1.232
      ENDIF
      IF(TWFLG.EQ.0) RATIO = RATIO2
      DRYWT = GCUFTS*RATIO*WD
      WOODDRYWEIGHT = DRYWT
      RETURN
      END   
!**********************************************************************
      SUBROUTINE CALCFROMBIOEQDB(BMEQ,DBH,THT,CR,BIOMS)
      CHARACTER*12 BMEQ
      REAL DBH,THT,CR,LCW,HT1,HT2,BIOMS,TOPD
      INTEGER ERRFLG
!C      For equations saved in the SQLite database, calculate biomass with its equation formula
      CALL BIOEQDB(BMEQ,DBH,THT,HT1,HT2,CR,TOPD,BIOMS,ERRFLG)      
      RETURN
      END   