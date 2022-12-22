C YW 2016/03/08 Hard coded the weight factor for DF in Rogue Rive - Siskiyou NF
C YW 07/05/2017 R6 requests to remove the hardcoded weight factor 
C YW 2018/11/02 CHANGED TO NOT REMOVE TIP FROM BRANCHES TO AVOID NEGATIVE NUMBER FOR SMALL TREES
C      THIS SUBROUTINE SEARCH THE REGIONAL SPECIES DEFAULT RECORDS TO FIND THE SPECIES
C      DEFAULT WEIGHT FACTOR AND COMPONENT BIOMASS EQUATION FOR THE REGIONAL/FOREST
       SUBROUTINE CRZSPDFT(REGN, FORST, SPCD, WF, BMSEQ, REF)
       IMPLICIT NONE
       INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
       INCLUDE 'regndftdata.inc'      !'REGNDFTDATA.INC'
       INCLUDE 'beqrefinfo.inc'
       
       INTEGER REGN, IFORST, SPCD, I, J, K
       REAL WF(3), DWF, REGNWF(3), NATLWF
       CHARACTER*2 FORST
       CHARACTER*12 BMSEQ(8), BIOEQ
       CHARACTER*40 REF(8)
       CHARACTER*3 REFABRV
       INTEGER FIRST, LAST, HALF, DONE
       
       READ(FORST,'(i2)') IFORST
       DO 3, J=1,8
         BMSEQ(J) = '-'
         REF(J) = ''
3      CONTINUE
       DONE = 0
       I = 0
C      First to check if the species has a regional/forest default record  
C      Skip search regional default if REGN = 0 (YW 20190918)   
       IF(REGN.EQ.0) DONE = -1  
       DO 5, WHILE (DONE.EQ.0)
         I = I + 1
         IF(SPREGNDFTWF(I,1).EQ.REGN) THEN  
           IF((SPREGNDFTWF(I,2).EQ.IFORST .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD) .OR.
     &      (SPREGNDFTWF(I,2).EQ.0 .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD)) THEN
             DONE = I
             REGNWF(1) = SPREGNDFTWF(I,4)
             REGNWF(2) = SPREGNDFTWF(I,5)
             REGNWF(3) = SPREGNDFTWF(I,6)
             BMSEQ(1) = SPREGNDFTBEQ(I,1)
             BMSEQ(2) = SPREGNDFTBEQ(I,2)
             BMSEQ(3) = SPREGNDFTBEQ(I,3)
             BMSEQ(4) = SPREGNDFTBEQ(I,4)
             BMSEQ(7) = SPREGNDFTBEQ(I,5)
           ENDIF
         ENDIF 
         IF(I.GE.TOTDFT.AND.DONE.EQ.0) DONE = -1
5      CONTINUE
       IF(DONE.GT.0) THEN
       !Found regional species default, then get the default equation and reference   
         !SECONDARY WEIGHT FACTOR
         IF(WF(2).GT.0) THEN
            REF(6) = 'USER PROVIDED WEIGHT FACTOR (SECOND)'
         ELSEIF(REGNWF(2).GT.0.1)THEN
            WF(2) = REGNWF(2)
            REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (SECOND)'
         ELSE
           IF(WF(1).GT.0) THEN
             WF(2) = WF(1)
             REF(6) = 'USER PROVIDED WEIGHT FACTOR(PRIM)'
           ELSE
             WF(2) = REGNWF(1)
             REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (PRIM)'
           ENDIF
         ENDIF
         !PRIMARY WEIGHT FACTOR
         IF(WF(1).GT.0.1)THEN
           REF(5) = 'USER PROVIDED WEIGHT FACTOR(PRIM)'
         ELSEIF(REGNWF(1).GT.0.1)THEN
            WF(1) = REGNWF(1)
            REF(5) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (PRIM)'
         ENDIF
         !MOISTURE CONTENT
         IF(WF(3).GT.0) THEN
           REF(8) = 'USER PROVIDED MOISTURE CONTENT'
         ELSEIF(REGNWF(3).GT.0.1)THEN
           WF(3) = REGNWF(3)
           REF(8) = 'USE REGIONAL DEFAULT MOISTURE CONTENT'
         ENDIF
         !BIOMASS EQN REFERENCE
         DO K=1, 5
           REFABRV = " "
           IF(K.LE.4)THEN
             BIOEQ = BMSEQ(K)
           ELSE
             BIOEQ = BMSEQ(7)
           ENDIF
           IF(LEN_TRIM(BIOEQ).EQ.12) REFABRV = BIOEQ(1:3)

           IF(LEN_TRIM(REFABRV).EQ.3) THEN
             DO I=1,REFCNT
               IF(REFABRV.EQ.BEQREF(I)%ABRV)THEN
                 IF(K.LE.4)THEN
                   REF(K) = BEQREF(I)%AUTH
                 ELSE
                   REF(7) = BEQREF(I)%AUTH
                 ENDIF
               ENDIF
             ENDDO
           ENDIF
         ENDDO
       ENDIF
C  Region 6 Rogue river - Siskiyou NF Douglas-fir has different Weight factor based on DBH
C  DBH < 15 weight factor = 59, DBH > 15 weight factor = 51
C  The weight factor is hardcoded here (YW 2016/03/08)
       IF(SPCD.EQ.204) SPCD = 202
C  R6 requests to remove the following lines (YW 07/05/2017)       
c       IF(REGN.EQ.6.AND.IFORST.EQ.10.AND.SPCD.EQ.202)THEN
c         WF(1) = 51
c         WF(2) = 59
c       ENDIF;     
       IF(DONE.LE.0 .OR. WF(1).EQ.0 .OR. WF(3) .EQ.0) THEN
C      IF THE SPECIES DOES NOT HAVE REGION DEFAULT, SEARCH NATIONAL DEFAULT
         DONE = 0
         I = 0
         FIRST = 1
         LAST = TOTSPC
         IF(WF(1).LT.0.1 .OR. WF(3).LT.0.1) THEN
           DO 10, WHILE (DONE.EQ.0)
             HALF = (LAST - FIRST +1)/2 + FIRST
             IF(WDBKWT(HALF,1).EQ.SPCD) THEN
               DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
             ELSEIF(WDBKWT(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
             ELSE
               LAST = HALF - 1
             ENDIF
10         CONTINUE
             IF(DONE.LT.0) DONE = TOTSPC
             IF(WF(1).LT.0.1) THEN
               WF(1) = WDBKWT(DONE,9)
               REF(5) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
               IF(WF(2).LT.0.1) THEN
                 WF(2) = WF(1)
                 REF(6) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
               ENDIF
               IF(WF(3).LT.0.1) THEN
                 DWF = WDBKWT(DONE,10)
                 WF(3) = (WF(1) - DWF)/DWF*100
                 REF(8) = 'MOISTURE CONTENT FROM MILES & SMITH 2009'
               ENDIF
             ELSE
               IF(WF(2).LT.0.1) WF(2) = WF(1)
               IF(WF(3).LT.0.1) THEN
                 DWF = WDBKWT(DONE,10)
                 WF(3) = (WF(1) - DWF)/DWF*100
                 REF(8) = 'MOISTURE CONTENT FROM MILES & SMITH 2009'
               ENDIF
             ENDIF
         ENDIF 
       ENDIF    
       END
C -----------------------------------------------------------------------------------------------
C THIS SUNROUTINE IS SEARCHING THE SPECIES DEFAULT DATA(SMITH AND MILES 2009) TO GET WOOD DENSITY
       SUBROUTINE WOODDEN(SPCD, WDEN, BDEN)
       IMPLICIT NONE
       INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
       INTEGER SPCD, FIRST, LAST, HALF, DONE
       REAL WDEN, BDEN
         DONE = 0
         FIRST = 1
         LAST = TOTSPC
           DO 50, WHILE (DONE.EQ.0)
             HALF = (LAST - FIRST +1)/2 + FIRST
             IF(WDBKWT(HALF,1).EQ.SPCD) THEN
               DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
             ELSEIF(WDBKWT(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
             ELSE
               LAST = HALF - 1
             ENDIF
50         CONTINUE
         IF(DONE.LT.0) DONE = TOTSPC
         WDEN = WDBKWT(DONE,4)
         BDEN = WDBKWT(DONE,5)
       END       
C -----------------------------------------------------------------------------------------------
C THIS SUNROUTINE IS SEARCHING THE SPECIES DEFAULT DATA(SMITH AND MILES 2009) TO GET SAPLING ADJUSTMENT FACTOR
       SUBROUTINE SAPLINGADJ(SPCD, ADJ)
       IMPLICIT NONE
       INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
       INTEGER SPCD, FIRST, LAST, HALF, DONE
       REAL ADJ
         DONE = 0
         FIRST = 1
         LAST = TOTSPC
           DO 55, WHILE (DONE.EQ.0)
             HALF = (LAST - FIRST +1)/2 + FIRST
             IF(WDBKWT(HALF,1).EQ.SPCD) THEN
               DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
             ELSEIF(WDBKWT(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
             ELSE
               LAST = HALF - 1
             ENDIF
55         CONTINUE
         IF(DONE.LT.0) DONE = TOTSPC
         ADJ = WDBKWT(DONE,11)
       END       
C--------------------------------------------------------------------------------------------
       SUBROUTINE CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB, HTTOT,FCLASS,
     +  VOL,WF,BMS,ERRFLG)
       IMPLICIT NONE
       INTEGER REGN, IFORST, SPCD, ERRFLG, I,J, LENGTH, FCLASS
       REAL DBHOB, HTTOT, DBH, THT, VOL(15), WF(3), BMS(8),DRCOB
       CHARACTER*2 FORST
       CHARACTER*12 BMSEQ(8), BEQ
       CHARACTER*40 REF(8)
       REAL MERCHSTEM, P3, BIOMS, DRY2GRN, STEMGWT
       REAL JNKBMS(8), RATIO, DEN, STUMPDRY, STUMPGRN
       INTEGER stm, Pied, Quga
       REAL drcp, drcq
C      THE COMPONENTS IN BMS VARIABLES AS:
C      1 ABOVEGROUND TOTAL
C      2 BRANCHES + TOP
C      3 DEAD BRANCHES
C      4 FOLIAGE
C      5 PRIMARY PROD (WOOD + BARK)
C      6 SECONDARY (TOPWOOD) (WOOD + BARK)
C      7 STEM TIP

       READ(FORST,'(i2)') IFORST
       DO 13, J=1,8
         BMS(J) = 0.0
13     CONTINUE
C      Convert DRC to DBH (Chojnacky 1999)
       CALL DRC2DBH(SPCD,DRCOB,FCLASS,DBHOB)
       
       IF(DBHOB.EQ.0) GOTO 40
       DBH = DBHOB
       THT = HTTOT
       RATIO = 0
C      IF WEIGHT FACTOR IS NOT PROVIDED, GET IT FROM REGIONAL OR NATIONAL DEFAULT
       IF(WF(1).LT.0.1) THEN
         CALL CRZSPDFT(REGN,FORST,SPCD,WF,BMSEQ,REF)
       ENDIF
C Reset weight factor for DF based on DBH (YW 2016/03/08)      
       IF(SPCD.EQ.204) SPCD = 202
C R6 requests to remove the following lines (YW 07/05/2017)
c       IF(REGN.EQ.6.AND.IFORST.EQ.10.AND.SPCD.EQ.202)THEN
c         IF(DBHOB.LT.15) WF(1) = 59
c       ENDIF    
       IF(WF(2).LT.0.1) WF(2) = WF(1)
C      GET THE MERCH STEM MASS (WOOD AND BARK) FROM VOL AND WEIGHT FACTOR
       BMS(5) = VOL(4)*WF(1)
       BMS(6) = VOL(7)*WF(2)
       BMS(7) = VOL(15)*WF(2)
C      GET THE DRY WEIGHT FOR MERCH WOOD ONLY       
C       CALL WOODDEN(SPCD, DEN)
C       MERCHSTEM = (VOL(4) + VOL(7))*DEN
       
C      CALL JENKINS FUNCTION TO GET BIOMASS FOR TREE COMPONENT
C      NOTE: JENKINS FUNCTION RETURN DRY BIOMASS FOR THE FOLLOWING COMPONENT
C     1 ABOVE GROUND TOTAL
C     2 STEM WOOD
C     3 STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 STEM WOOD AND BARK

       CALL JENKINS(SPCD, DBH, JNKBMS)

C      CONVERT DRY BIOMASS TO GREEN BIOMASS
       DRY2GRN = 1 + WF(3)/100
       JNKBMS = JNKBMS * DRY2GRN
C      THE TOTAL STEM WEIGHT FROM STUMP TO 4' TOP       
       STEMGWT = BMS(5) + BMS(6)
C      IF NO VOLUME FOR THE MAIN STEM IS PROVIDED, USING THE BIOMASS CALCULATED FRON JENKINS EQUATION       
C       IF(DBHOB.GE.5.AND.STEMGWT.EQ.0) STEMGWT = JNKBMS(8)
C      BECAUSE IT IS BELIEVED THE VOLUME FROM VOLLIB AND LOCAL WEIGHT FACTOR HAS
C      BETTER ESTIMATE GREEN BIOMASS FOR MERCH STEM, SO A RATIO FACTOR IS GOTTEN
C      TO ADJUST THE BIOMASS FROM JENKINS EQUATION. FOR SAPLING THE ADJUSTMENT FACTOR
C      IS USING THE DEFAULT VALUE FROM THE DEFAULT VALUE
C      GET THE MASS RATIO FOR THE STEM
C       RATIO = MERCHSTEM/JNKBMS(2)
       IF(STEMGWT.GT.0.AND.JNKBMS(8).GT.0) RATIO = STEMGWT/JNKBMS(8)

C      Added check if merch vol is calculated for small trees(2017/02/21)       
       IF(DBHOB.LT.5.AND.STEMGWT.LE.0) THEN
         CALL SAPLINGADJ(SPCD, RATIO)
C        Small tree does not have stem component biomass         
c         BMS(5) = 0
c         BMS(6) = 0
         BMS(7) = 0
       ENDIF
       IF(RATIO.LE.0) RATIO = 1
       
C      ADJUST BIOMASS FROM JENKINS
       BMS(1) = JNKBMS(1)*RATIO
       BMS(2) = JNKBMS(6)*RATIO
       BMS(4) = JNKBMS(4)*RATIO
C      If tip is calculated seperately, remove it from branches
C      CHANGED TO NOT REMOVE TIP FROM BRANCHES TO AVOID NEGATIVE NUMBER FOR SMALL TREES (YW 2018/11/02)
C       IF(BMS(7).GT.0) BMS(2) = BMS(2)-BMS(7)       
C      RETURN MAINSTEM BIOMASS FROM JENKINS WHEN THERE IS NO VOLUME FOR MAINSTEM(6/13/2016)
C      For tree DBH>=5 and no merch vol calculated, stem goes to branches (03/07/2017)      
       IF(BMS(5).EQ.0.AND.BMS(6).EQ.0.AND.DBHOB.GE.5)THEN
        BMS(2) = BMS(1) - BMS(4)
       ENDIF
C      For small trees, if merch vol is calculated to topd < 4", adjust the branches total.
C      2017/02/21
       IF(JNKBMS(8).EQ.0.AND.STEMGWT.GT.0) BMS(2)=BMS(2)-BMS(5)-BMS(6)
         
C      IF BIOMASS EQUSTION IS SET IN REGIONAL DEFAULT, USE IT TO CALCULATE BIOMASS
       DO 30, J=1,5
         IF(J.LT.5)THEN
           I=J
         ELSE
           I=7
         ENDIF
         BEQ = BMSEQ(I)
         IF(BEQ.NE.''.AND.(BEQ(12:12).EQ.'D'.OR.BEQ(12:12).EQ.'G'))THEN
           CALL CalcBiomass(BEQ, DBHOB, THT, P3, BIOMS)
           BMS(I) = BIOMS
           IF(BEQ(12:12).EQ.'D') BMS(I) = BIOMS*DRY2GRN
         ENDIF
30     CONTINUE   
40     RETURN    
       END
C ------------------------------------------------------------------------------------
C Convert DRC to DBH for woodland species
C Chojnack, D. C. and Rogers, P. Converting Tree Diameter Measured at Root Collar
C to Diameter at Breast Height. WJAF 14(1), 1999
      SUBROUTINE DRC2DBH (SPCD,DRCOB,FCLASS, DBHOB)
      INTEGER SPCD, Pied, Quga, stm, FCLASS
      REAL DRCOB, DBHOB, drcp, drcq
      
       IF(DRCOB.GT.0) THEN
         Pied = 0
         Quga = 0
         stm = 1
         IF(FCLASS.GT.1) stm = 0
C        Pinyon pine species         
         IF(SPCD.EQ.106.OR.SPCD.EQ.133.OR.SPCD.EQ.134
     &      .OR.SPCD.EQ.138.OR.SPCD.EQ.140.OR.SPCD.EQ.141
     &      .OR.SPCD.EQ.143)THEN
           Pied = 1
           drcp = DRCOB
C        Gambel oak species           
         ELSEIF(SPCD.EQ.803.OR.SPCD.EQ.810.OR.SPCD.EQ.814
     &      .OR.SPCD.EQ.829.OR.SPCD.EQ.843.OR.SPCD.EQ.846
     &      .OR.SPCD.EQ.847)THEN
           Quga = 1
           drcq = DRCOB
         ENDIF
         DBHOB = -2.6843+1.0222*DRCOB+0.7433*stm
     &   +0.7469*Pied-0.0399*drcp+1.2244*Quga-0.0689*drcq
         IF(DBHOB.LT.0) DBHOB = 0
       ENDIF
      RETURN
      END