      SUBROUTINE FIA_NW(BEQ, DBHOB, HTTOT, VOL, BMS)
!      INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
      CHARACTER(12) BEQ, BIOEQ
      CHARACTER(3) COMP
      REAL DBHOB,HTTOT,BMS,DBH,HT1PRD, HT2PRD,CR,TOPD
      REAL VOL(15),BIOMS(8)
      INTEGER SPN, SPLIST(123),DONE,LAST,ERRFLG,I,STEMS
      REAL WD_DEN(123), SG(11),WDDEN,BKDEN
      REAL STEM_WDTOT, STEM_WDMCH, BRCH_L, BRCH_D, STEM_BRCH
      REAL STEM_BK, FOL, BRCH_TOT, ABVGRD_TOT, CRWN,BRATIO
      CHARACTER(2) EQMDL

      DATA (SPLIST(I),I=1,123)/
     +  1, 11, 14, 15, 17, 19, 20, 21, 22, 41, 
     + 42, 50, 51, 52, 53, 54, 55, 56, 62, 64,
     + 65, 66, 72, 73, 81, 92, 93, 94, 95, 98,
     +101,102,103,104,108,109,113,116,117,119,
     +120,122,124,127,130,133,137,142,201,202,204,
     +211,212,231,242,251,263,264,298,299,312,
     +313,320,321,322,330,333,341,351,352,361,
     +374,375,421,431,475,492,500,510,511,540,
     +542,547,591,600,602,603,604,611,631,660,
     +661,730,731,746,747,748,756,758,760,763,
     +766,768,771,801,805,807,811,815,818,821,
     +826,839,901,920,922,926,927,981,990,997,
     +998,999/

      DATA (WD_DEN(I),I=1,123)/
     +28.08,24.96,22.46,23.09,21.84,19.34,22.46,22.46,23.09,24.34,
     +26.21,41.81,25.58,41.81,25.58,41.81,41.81,41.81,33.70,33.70,
     +33.70,33.70,29.95,29.95,21.84,21.84,20.59,20.59,20.59,23.09,
     +23.09,23.09,23.09,23.09,23.71,26.83,23.09,23.71,21.22,21.84,
     +26.83,23.71,21.84,26.83,26.83,23.09,23.09,23.09,28.70,28.70,28.7,
     +21.22,23.71,41.81,19.34,31.82,26.21,26.21,25.58,25.58,27.46,
     +19.34,23.71,23.71,29.33,23.71,23.71,18.72,23.09,23.09,43.06,
     +18.72,18.72,24.96,29.95,43.68,43.68,25.58,49.92,49.92,31.20,
     +31.2 ,31.2 ,25.58,31.82,31.82,31.82,31.82,31.82,36.19,36.19,
     +36.19,28.7 ,28.7 ,21.84,19.34,19.34,43.68,43.68,29.32,29.32,
     +29.33,29.32,29.32,49.92,49.92,37.44,37.44,37.44,34.94,37.44,
     +37.44,49.92,41.18,22.46,22.46,22.46,22.46,36.82,31.82,32.45,
     +31.82,28.08/


      READ(BEQ(4:6),'(I3)') SPN
      COMP = BEQ(7:9)
      EQMDL = BEQ(10:11)
      LAST = 123
      DONE = 0
      DBH = DBHOB

C     First check the species for in the SPLIST
      !CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)
      !IF(DONE.GT.0) THEN
C       First calculate the total stem wood and merch stem wood using cubic volume
      !  STEM_WDTOT = VOL(1)*WD_DEN(DONE)
      !  STEM_WDMCH = VOL(4)*WD_DEN(DONE)
      CALL WOODDEN(SPN, WDDEN, BKDEN)
      STEM_WDTOT = VOL(1)*WDDEN
      STEM_WDMCH = VOL(4)*WDDEN
      
        BIOEQ(4:6) = BEQ(4:6)
        BIOEQ(10:12) = '01D'

        IF(COMP.EQ.'AWB')THEN
C         CALCULATE BRANCHES AND STEM BARK USING STANDISH, SHAW, OR GHOLZ EQUATION
          IF(EQMDL.EQ.'ST')THEN
C           STANDISH EQUSTION
            BIOEQ(1:3) = 'STA'
          ELSEIF(EQMDL.EQ.'SH')THEN
C           SHAW EQUSTION
            BIOEQ(1:3) = 'SHA'
          ELSEIF(EQMDL.EQ.'G1'.OR.EQMDL.EQ.'G2')THEN
C           GHOLZ EQUSTION
            BIOEQ(1:3) = 'GHZ'
          ENDIF
!         FIA equation 000051 and 000072 use local biomass config equation for bark and branches
!         the local config equation, so here using Jenkins bark and branches for now 2019/07/26
          IF(EQMDL.EQ.'51'.OR.EQMDL.EQ.'72')THEN
            CALL JENKINS(SPN,DBH,BIOMS)
            STEM_BK = BIOMS(3)
            BRCH_L = BIOMS(6)
            IF(EQMDL.EQ.'72')THEN
              CALL MILESDATA(SPN, SG)
              STEM_WDTOT = STEM_WDTOT*(1.0+SG(8)/100.0)
              STEM_BRCH = STEM_WDTOT + BRCH_L
            ELSE
              STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L
            ENDIF
          ELSE       
            BIOEQ(7:9) = 'BRL'
            BIOEQ(10:12) = '01D'        
            CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,
     +       HT2PRD,TOPD,STEMS,VOL,BRCH_L,ERRFLG)

C         STEM BARK
            BIOEQ(7:9) = 'STB'
           CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +     STEMS,VOL,STEM_BK,ERRFLG)
           STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L
          ENDIF
          
          IF(EQMDL.EQ.'G2')THEN
C           THE STEM WOOD IS ALSO CALCULATED USING GHOLZ EQUATION
            BIOEQ(7:9) = 'BRD'
            CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +      STEMS,VOL,BRCH_D,ERRFLG)
            
            BIOEQ(7:9) = 'STW'
            CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +      STEMS,VOL,STEM_WDTOT,ERRFLG)
            
            STEM_BRCH = STEM_WDTOT + STEM_BK + BRCH_L + BRCH_D
          ENDIF

        ELSEIF(COMP.EQ.'AGT')THEN
C         CALCULATE ABOVE GROUND TOTAL USING GHOLZ EQUATION
          BIOEQ(1:3) = 'GHZ'
          BIOEQ(7:9) = 'BRL'
          BIOEQ(10:12) = '01D'        
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,BRCH_L,ERRFLG)
          
C         STEM BARK
          BIOEQ(7:9) = 'STB'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,STEM_BK,ERRFLG)
          
          BIOEQ(7:9) = 'STW'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,STEM_WDTOT,ERRFLG)
          
          BIOEQ(7:9) = 'FOT'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,FOL,ERRFLG)
          
          BIOEQ(7:9) = 'BRD'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,BRCH_D,ERRFLG)
          
          ABVGRD_TOT = STEM_WDTOT + STEM_BK + BRCH_L + BRCH_D + FOL

        ELSEIF(COMP.EQ.'BRT'.AND.EQMDL.EQ.'SN')THEN
C         CALCULATE BRANCHES USING SNALL EQUATION
          BIOEQ(1:3) = 'SNE'
          BIOEQ(7:9) = 'CRW'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,CRWN,ERRFLG)
          
          BIOEQ(7:9) = 'FOT'
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,FOL,ERRFLG)
          
          BRCH_TOT = CRWN - FOL
        ELSEIF(COMP.EQ.'MSB'.AND.EQMDL.EQ.'PI')THEN
          CALL PILLSBSTMBRKVOL(SPN,DBHOB,HTTOT, BRKVOL)
          !CALL MILESDATA(SPN, SG)
          !STEM_BK = BRKVOL*SG(5)    !SG(5) = bark weight (lb/cf)
          STEM_BK = BRKVOL*BKDEN
        ELSEIF(SPN.EQ.211.AND.COMP.EQ.'STB')THEN
C         Redwood stem bark
C         It uses Harmon for large trees and Shaw cedar for small trees.
          BIOEQ(7:9) = 'STB'
          BIOEQ(10:11) = '01'
          IF(DBHOB.GT.39.37)THEN
            BIOEQ(1:3) = 'HAN'
            BIOEQ(4:6) = '212'           
          ELSE
            BIOEQ(1:3) = 'SHA'
            BIOEQ(4:6) = '242'
          ENDIF
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD,HT2PRD,TOPD,
     +    STEMS,VOL,STEM_BK,ERRFLG)
        ELSEIF(COMP.EQ.'STB'.OR.COMP.EQ.'MSB')THEN  
          BRATIO = 0.0
          IF(SPN.EQ.312)then
            BRATIO = 0.14
          ELSEIF(SPN.EQ.351)THEN
            BRATIO = 0.18
          ELSEIF(SPN.EQ.361)THEN
            BRATIO = 0.05
          ELSEIF(SPN.EQ.431)THEN
            BRATIO = 0.22
          ELSEIF(SPN.EQ.631)THEN
            BRATIO = 0.24
          ENDIF
          IF(COMP.EQ.'STB')THEN
            STEM_BK = STEM_WDTOT*BRATIO
          ELSEIF(COMP.EQ.'MSB')THEN
            STEM_BK = STEM_WDMCH*BRATIO
          ENDIF
        ENDIF

C       Return the biomass for the BEQ       
C       Total stem wood 
        IF(COMP.EQ.'STW')THEN
          IF(VOL(1).EQ.0.0)THEN
            ERRFLG = 18
            RETURN
          ENDIF
          BMS = STEM_WDTOT
C       TOTAL STEM BARK
        ELSEIF(COMP.EQ.'STB'.OR.COMP.EQ.'MSB')THEN
          BMS = STEM_BK
C       Merch stem wood
        ELSEIF(COMP.EQ.'MSW')THEN
          IF(VOL(4).EQ.0.0)THEN
            ERRFLG = 18
            RETURN
          ENDIF
          BMS = STEM_WDMCH
C       STEM AND BRANCHES
        ELSEIF(COMP.EQ.'AWB') THEN
          BMS = STEM_BRCH
C       BRANCHES
        ELSEIF(COMP.EQ.'BRT') THEN
          BMS = BRCH_TOT
C       ABOVE GROUND TOTAL
        ELSEIF(COMP.EQ.'AGT') THEN
          BMS = ABVGRD_TOT
        ENDIF
        

      !ENDIF
      END
C ------------------------------------------------------------------------------------
C Computes DBH+DBT at breast high then feeds both DBH and DBH+DBT into Pillsbury CV4 to 
C get bark volume.
      SUBROUTINE PILLSBSTMBRKVOL(SPN, DBHOB, HTTOT, BRKVOL)
      INTEGER SPN, SPLIST(13), DONE, FIRST, LAST, I, ERRFLG
      REAL DBHOB, HTTOT, MSTMBRK
      REAL OUTDBH, CMDBH, OUTCV4, INCV4, BRKVOL
      REAL T1(13), T2(13), C1(13), C2(13), C3(13), A, B, C

      DATA (SPLIST(I), I=1,13)/
     + 312,361,431,631,801,805,807,811,815,818,821,839,981/

      DATA (T1(I), I=1,13)/
     + 0.21235, -0.03425, 0.39534, -4.1177, -1.92379, 
     + -0.48584, -0.44003, -1.99573, -0.78034, -0.68133, 
     + -0.97254, 0.12237, -0.32491/

      DATA (T2(I), I=1,13)/
     + 0.94782, 0.98155, 0.90182, 0.95354, 0.93475, 
     + 0.96147, 0.94403, 0.92472, 0.95956, 0.95767, 
     + 0.93545, 0.92953, 0.96579/

      DATA (C1(I), I=1,13)/
     + 0.003421416, 0.002561643, 0.005521294, 0.000577497, 0.002457485, 
     + 0.00316706, 0.004232407, 0.005386635, 0.002427703, 0.00367957, 
     + 0.000968436, 0.004119226, 0.001638075/

      DATA (C2(I), I=1,13)/
     + 2.35347, 1.99295, 2.07202, 2.19576, 2.53284, 
     + 2.32519, 2.53987, 2.61268, 2.25575, 2.12635, 
     + 2.39565, 2.14915, 2.0591/

      DATA (C3(I), I=1,13)/
     + 0.69586, 1.01532, 0.77467, 1.14078, 0.60764, 
     + 0.74348, 0.50591, 0.31103, 0.87108, 0.83339, 
     + 0.98878, 0.77843, 1.05293/

      LAST = 13
      DONE = 0

C     First check the species for in the SPLIST
      CALL SEARCH(LAST,SPLIST,SPN,DONE,ERRFLG)

      IF(DONE.GT.0) THEN
        CMDBH = DBHOB*2.540005
        OUTDBH = (CMDBH + T1(DONE))/T2(DONE)/2.540005
        A = C1(DONE)
        B = C2(DONE)
        C = C3(DONE)
        OUTCV4 = A*OUTDBH**B*HTTOT**C
        INCV4 = A*DBHOB**B*HTTOT**C
        BRKVOL = OUTCV4 - INCV4
      ENDIF
      RETURN
      END
    

