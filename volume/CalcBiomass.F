C WARNING! The codes were created by NBEL Tester program. Please do NOT modify
C Last modified on 7/17/2015
C This sunroutine is generated from NBEL Tester for selected biomass equations
       SUBROUTINE CalcBiomass(BEQ, DBH, THT, CR, BMS)
       CHARACTER*12 BEQ
       REAL DBH, THT, CR, VOL(15),BMS
       INTEGER STEMS, ERRFLG
       REAL HT1PRD, HT2PRD,TOPD
       BMS = 0

       CALL BiomassLibrary(BEQ,DBH,THT,CR,HT1PRD, 
     + HT2PRD,TOPD, STEMS, VOL, BMS, ERRFLG)

       RETURN
       END
C ------------------------------------------------------------------------------------------------------------
C This subroutine calculates biomass for equation stored in the National Biomass Estimator Library (NBEL).
C The file fiabioeq.inc lists all FIA Regional biomass equation. 
C The file bioeqinfo.inc lists published biomass equations in NBEL and the file bioeqcoef.inc contains the
C coefficients for all biomass equations.
C Input variables are:
C BIOEQ -- biomass equatiom number 
C DBHOB -- outside bark diameter at breast height
C HTTOT -- total height 
C CR    -- crown ratio (=crown length/total height)
C HT1PRD -- height for primary prod (saw timber)
C HT2PRD -- height for secondary prod (pulp)
C TOPD -- top diameter for merch part
C STEMS -- number of stems for woodland species
C VOL -- the volume calculated from volume library 
C BIOMS -- the calculated dry biomass for the given BIOEQ.
C ERRFLG -- error code returnd from calculation:
C        15 - invalid BIOEQ
C        16 - primary prod height required
C        17 - secondary prod height is required
C        18 - stem cubic volume is required for FIA biomass equation
       SUBROUTINE BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     + HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG)
       CHARACTER*12 BIOEQ,GEOSUB
       REAL DBHOB,HTTOT,HT1PRD,HT2PRD,TOPD,CR,VOL(15),BIOMS
       INTEGER STEMS,ERRFLG,SPN
       SPN = 0
       GEOSUB = '0'
       CALL BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     + HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG,SPN,GEOSUB)
       RETURN
       END
! ---------------------------------------------------------------------       
       SUBROUTINE BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     + HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLG,SPN,GEOSUB)
       INCLUDE 'fiabioeq.inc'
C       INCLUDE 'bioeqinfo.inc'
C       INCLUDE 'bioeqcoef.inc'
       CHARACTER*12 BIOEQ,BIOEQTMP,GEOSUB,NVELBEQ
       REAL DBHOB, HTTOT, TOPD, CR, VOL(15),BIOMS,BIOMSTMP
       REAL HT1PRD, HT2PRD, VOLM(15)
       CHARACTER*3 MDL,COMP
       REAL DIA, HT
       INTEGER STEMS, IDX, I, SPEC, ERRFLG,SPN
       CHARACTER*2 DiaUnit, HtUnit, BioUnit
       CHARACTER*3 DiaType, HtType
       REAL IN2CM,IN2MM,IN2M,FT2M,FT2CM,KG2LB,G2LB,MG2LB
       REAL SG(11), MC,JNKBMS(8),RATIO
       REAL DTMP,HTMP,HT1,HT2,CV4,WDEN,BDEN,CV4MS
       INTEGER EqForm,BEQNUM,stat,LEN,isint
       
       ERRFLG = 0
       ! Check if BIOEQ is FIA equation number
       CALL str2int(BIOEQ,BEQNUM,stat)
       IF(stat.EQ.0)THEN
         IF(BEQNUM.EQ.109)THEN
           BIOMS = 0.0
           RETURN
         ELSE
           CALL FIABEQ2NVELBEQ(BEQNUM,SPN,NVELBEQ,GEOSUB,ERRFLG)
         ENDIF
       ELSE
         NVELBEQ = BIOEQ
       ENDIF
       DIA = DBHOB
       HT = HTTOT
       MDL = NVELBEQ(1:3)
       COMP = NVELBEQ(7:9)
       VOLM = VOL
       BIOMS = 0.0
       IF(MDL.EQ.'CRM')THEN
!      Jenkins component ratio method
         CV4 = VOL(4)+VOL(7)
!         IF(CV4.LT.0.1)THEN
!           ERRFLG = 18
!           RETURN
!         ENDIF
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
         CALL JENKINS(SPN, DIA, JNKBMS)
         !Convert CV4 to mass
         CALL WOODDEN(SPN, WDEN, BDEN)
         CV4MS = CV4*WDEN
         IF(CV4MS.GT.0.0.AND.JNKBMS(2).GT.0.0)THEN
           RATIO = CV4MS/JNKBMS(2)
         ELSE
           IF(DIA.LT.5.0)THEN
             CALL SAPLINGADJ(SPN,RATIO)
           ELSE
             RATIO = 1.0
           ENDIF
         ENDIF
         !return results for components
         IF(COMP.EQ.'AGT')THEN
           BIOMS = JNKBMS(1)*RATIO
         ELSEIF(COMP.EQ.'MSW')THEN
           BIOMS = JNKBMS(2)*RATIO
         ELSEIF(COMP.EQ.'MSB')THEN
           BIOMS = JNKBMS(3)*RATIO
         ELSEIF(COMP.EQ.'FOT')THEN
           BIOMS = JNKBMS(4)*RATIO 
         ELSEIF(COMP.EQ.'RTT')THEN
           BIOMS = JNKBMS(5)*RATIO  
         ELSEIF(COMP.EQ.'BRT'.OR.COMP.EQ.'BRL')THEN
           BIOMS = JNKBMS(6)*RATIO
         ELSEIF(COMP.EQ.'CRW')THEN
           BIOMS = JNKBMS(7)*RATIO
         ELSEIF(COMP.EQ.'MST')THEN
           BIOMS = JNKBMS(8)*RATIO
         ENDIF
                 
       ELSEIF(MDL.EQ.'FNW'.OR.MDL.EQ.'FRM'.OR.MDL.EQ.'FNC'
     +    .OR.MDL.EQ.'FNE'.OR.MDL.EQ.'FSE'.OR.MDL.EQ.'FPI'
     &    .OR.MDL.EQ.'CAB'.OR.MDL.EQ.'CHV'.OR.MDL.EQ.'ASN')THEN
C      Check if the equation is a valid 
         IDX = 0
         ! No need to check equation number for FNW***STW01D and FNW***MSW01D
         IF(MDL.EQ.'FNW'.AND.(COMP.EQ.'STW'.OR.COMP.EQ.'MSW'))THEN
           CONTINUE
         ELSE
          DO I = 1, TOTEQ
           IF(MDL.EQ.'FSE'.AND.FIABEQ(I)(1:11).EQ.NVELBEQ(1:11))THEN
             IDX = I
             EXIT
           ELSEIF(FIABEQ(I).EQ.NVELBEQ)THEN
             IDX = I
             EXIT
           ENDIF
          END DO
          IF(IDX.EQ.0)THEN
C      Flag for invalid BIOEQ       
           ERRFLG = 15
           RETURN
          ENDIF
         ENDIF
C      Vol(4) should be the volume to 4" top
C      If top wood volume is saved in VOL(7), add it back to VOL(4)
         IF(VOLM(7).GT.0) VOLM(4) = VOLM(4)+VOLM(7)    
C         Stem cubic volume is required for FIA biomass equation
!         IF(VOLM(1).EQ.0.AND.VOLM(4).EQ.0)THEN
!           ERRFLG = 18
!           RETURN
!         ENDIF 
C      For the FIA biomass equation call FIA subroutine     
         IF(MDL.EQ.'FNW')THEN
           CALL FIA_NW(NVELBEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FRM')THEN
           CALL FIA_RM(NVELBEQ, DIA, HT, STEMS, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FNC')THEN
           CALL FIA_NC(NVELBEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FNE')THEN
           CALL FIA_NE(NVELBEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FSE')THEN
           CALL FIA_SE(NVELBEQ, DIA, HT, VOL, BIOMS)
         ELSE
         ! PACIFIC ISLANDS
           CV4 = VOLM(4)
           CALL PI_BIOMASS(SPN,NVELBEQ,DIA,HT,CV4,BIOMS,ERRFLG)
         ENDIF
       ELSEIF(MDL.EQ.'BRN'.AND.NVELBEQ(10:12).EQ.'00D')THEN
C      Brown's fraction biomasss  
C      List of Brown fraction equations:
C      BRN***FOT00D, BRN***B0Q00D,BRN***BQ100D,BRN***B1300D,
C      BRN***B3P00D, BRN***LCR00D,BRN***DCR00D
C      Valid species code:
C      011,015,017,019,020,021,022,041,042,073,081,093,
C      098,101,108,116,117,119,121,122,202,242,260,263  
         CALL BROWN(NVELBEQ, DIA, HT, CR, BIOMS,ERRFLG)
       ELSE
!C      For equations saved in the SQLite database, calculate biomass with its equation formula
!       Affleck 2019 component sum equations
         IF(MDL.EQ.'AFF'.AND.(COMP.EQ.'LCR'.OR.
     &     COMP.EQ.'MST'.OR.COMP.EQ.'AST'.OR.COMP.EQ.'B0Q'))THEN
           BIOEQTMP = NVELBEQ
           IF(COMP.EQ.'MST')THEN
!           MAIN STEM TOTAL (BARK + WOOD)
            BIOEQTMP(7:9)='MSB'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='MSW'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
           ELSEIF(COMP.EQ.'LCR')THEN
!          LIVE CROWN (LiveBranches + Foliage)
            BIOEQTMP(7:9)='BRL'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='FOT'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
           ELSEIF(COMP.EQ.'AST')THEN
!          ABOVE STUMP TOTAL (DeadBranched + LiveBranches + Foliage + StemBARK + StemWOOD)
            BIOEQTMP(7:9)='BRD'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='BRL'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='FOT'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='MSB'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
            BIOEQTMP(7:9)='MSW'
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS+BIOMSTMP  
           ELSEIF(COMP.EQ.'B0Q')THEN
!          BRANCHES < 1/4 INCHES
            BIOEQTMP(7:9)='BRL'
            DTMP = DIA
            HTMP = HT
            CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMS,ERRFLG)
            DTMP = DIA
            HTMP = HT
            BIOMSTMP = 0.0
            CALL BIOEQDB(BIOEQ,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
            BIOMS = BIOMS*BIOMSTMP
           ENDIF
         ELSEIF(MDL.EQ.'SNE'.AND.COMP.EQ.'BRL')THEN
         ! Snell live branches = live crown - foliage total
           BIOEQTMP = NVELBEQ
           BIOEQTMP(7:9)='CRW'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
           BIOMS = BIOMSTMP
           BIOEQTMP(7:9)='FOT'
           DTMP = DIA
           HTMP = HT
           BIOMSTMP = 0.0
           CALL BIOEQDB(BIOEQTMP,DTMP,HTMP,HT1,HT2,CR,TOPD,
     &       BIOMSTMP,ERRFLG)
           BIOMS = BIOMS - BIOMSTMP
         ELSE  
           CALL BIOEQDB(NVELBEQ,DIA,HT,HT1PRD,HT2PRD,CR,TOPD,
     &      BIOMS,ERRFLG)
         ENDIF   
       ENDIF
       RETURN
       END
C *********************************************************************************************************
C This subroutine the specific gravity data for a given species
C The wood and bark specific gravity data for a species are given as:
C      Column# Name
C            1 Species FIA code
C            2 soft or hard wood (0=soft and 1=hard)
C            3 Jenkins species group code
C            4 Wood dry weight (lb/cf)
C            5 bark dry weight (lb/cf)
C            6 Wood moist content(%)
C            7 Bark moist cotent (%)
C            8 bark to wood vol percent (%)
C            9 Green weight factor (lb/cf)
C           10 Dry weight factor (lb/cf)
C           11 Sapling adjustment factor
      SUBROUTINE MILESDATA(SPCD, SG)
      INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
      INTEGER SPCD, FIRST, LAST, HALF, DONE
      REAL SG(11)
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
         SG(1) = WDBKWT(DONE,1)
         SG(2) = WDBKWT(DONE,2)
         SG(3) = WDBKWT(DONE,3)
         SG(4) = WDBKWT(DONE,4)
         SG(5) = WDBKWT(DONE,5)
         SG(6) = WDBKWT(DONE,6)
         SG(7) = WDBKWT(DONE,7)
         SG(8) = WDBKWT(DONE,8)
         SG(9) = WDBKWT(DONE,9)
         SG(10) = WDBKWT(DONE,10)
         SG(11) = WDBKWT(DONE,11)
         RETURN         
      END
C **********************************************************************
      SUBROUTINE BIOEQDB(BIOEQ,DIA,HT,HT1PRD,HT2PRD,CR,TOPD,
     & BIOMS,ERRFLG)  
      INCLUDE 'bioeqinfo.inc'
      INCLUDE 'bioeqcoef.inc'
      CHARACTER*12 BIOEQ
      REAL DIA,HT,HT1PRD,HT2PRD,CR,CL,TOPD,BIOMS
      REAL a,b,c,d,e
      INTEGER STEMS, IDX, I, SPEC, ERRFLG, EqForm
      CHARACTER*2 DiaUnit, HtUnit, BioUnit
      CHARACTER*3 DiaType, HtType
      REAL IN2CM,IN2MM,IN2M,FT2M,FT2CM,KG2LB,G2LB,MG2LB
      REAL SG(11), MC
C      For equations saved in the SQLite database, calculate biomass with its equation formula
C      1. check if the BIOEQ is valid. EQNTOT is total number of biomass equations in the database
       IDX = 0
       a = 0.0
       b = 0.0
       c = 0.0
       d = 0.0
       e = 0.0
C      List of unit conversion factor       
       IN2CM = 2.54
       IN2MM = 25.4
       IN2M = 0.0254
       FT2M = 0.3048
       FT2CM = 30.48
       KG2LB = 2.20462
       G2LB = 0.00220462
       MG2LB = 2204.62
       DO I = 1, EQNTOT
         IF(BIOMSEQ(I)%BIOEQ.EQ.BIOEQ)THEN
           IDX = I
           EXIT
         ENDIF
       END DO
       IF(IDX.EQ.0)THEN
         ERRFLG = 15
         RETURN
       ENDIF
       IF(BIOEQ(1:2).EQ.'AS')THEN
         READ(BIOEQ(3:6),'(I4)') SPEC
       ELSE
         READ(BIOEQ(4:6),'(I3)') SPEC
       ENDIF

C      2. get the EQN info and coef
       DiaUnit = BIOMSEQ(IDX)%DiaUnit
       DiaType = BIOMSEQ(IDX)%DiaType
       HtType = BIOMSEQ(IDX)%HtType
       HtUnit = BIOMSEQ(IDX)%HtUnit
       IF(HtType.EQ.'HT1')THEN
         IF(HT1PRD.LE.0)THEN
C          Primary prod height is required for the BIOEQ         
           ERRFLG = 16
           RETURN
         ELSE
           HT = HT1PRD
         ENDIF
       ELSEIF(HtType.EQ.'HT2')THEN
         IF(HT2PRD.LE.0)THEN
C          Secondary prod height is required for the BIOEQ         
           ERRFLG = 17
           RETURN
         ELSE
           HT = HT2PRD
         ENDIF
       ENDIF
       
       IF(DiaUnit.EQ.'CM'.OR.DiaUnit.EQ.'cm')THEN
         DIA = DIA*IN2CM
       ELSEIF(DiaUnit.EQ.'MM'.OR.DiaUnit.EQ.'mm')THEN
         DIA = DIA*IN2MM
       ELSEIF(DiaUnit.EQ.'M'.OR.DiaUnit.EQ.'m')THEN
         DIA = DIA*IN2M
       ENDIF
       IF(HtUnit.EQ.'M'.OR.HtUnit.EQ.'m')THEN
         HT = HT*FT2M
       ELSEIF(HtUnit.EQ.'CM'.OR.HtUnit.EQ.'cm')THEN
         HT = HT*FT2CM
       ENDIF
       CL = CR*HT

       IF(DiaType.EQ.'CBH'.OR.DiaType.EQ.'CRC'.OR.DiaType.EQ.'CBA')THEN
         DIA = DIA*3.14
       ELSEIF(DiaType.EQ.'BA')THEN
         DIA = (DIA/2)**2*3.14
       ELSEIF(DiaType.EQ.'D2')THEN
         DIA = DIA*DIA
       ELSEIF(DiaType.EQ.'BAH')THEN
         DIA = DIA**2*0.005454*HT
       ELSEIF(DiaType.EQ.'D2H')THEN
         DIA = DIA*DIA*HT
       ELSEIF(DiaType.EQ.'D2R')THEN
         DIA = DIA*DIA*CR
       ENDIF
       EqForm = BEQCOEF(IDX,1)
       a = BEQCOEF(IDX,2)
       b = BEQCOEF(IDX,3)
       c = BEQCOEF(IDX,4)
       d = BEQCOEF(IDX,5)
       e = BEQCOEF(IDX,6)

C      3. call the EQN formula to calculate the biomass
       CALL BioeqFormula(EqForm,DIA,HT,CR,TOPD,a,b,c,d,e,BIOMS)
       IF(BIOEQ(12:12).EQ.'R'.OR.EqForm.EQ.41.OR.EqForm.EQ.19) RETURN
C      4. return dry biomass in lb
       BioUnit = BIOMSEQ(IDX)%BioUnit
       IF(BioUnit.EQ.'KG')THEN
         BIOMS = BIOMS*KG2LB
       ELSEIF(BioUnit.EQ.'G')THEN
         BIOMS = BIOMS*G2LB
       ELSEIF(BioUnit.EQ.'MG')THEN
         BIOMS = BIOMS*MG2LB
       ENDIF
       READ(BIOEQ(4:6),'(I3)') SPEC
C      Convert to dry biomass if the original equation is for green biomass
       IF(BIOEQ(12:12).EQ.'G')THEN
         CALL MILESDATA(SPEC, SG)
         MC = (SG(9)-SG(10))/SG(10)
         BIOMS = BIOMS/(1.0+MC)
       ENDIF      
       RETURN
      END  
C ********************************************************************
      subroutine biomasslib_r(BIOEQ,DBH_d,THT_d,CL_d,HT1_d,HT2_d,CV4_d,
     + TOPD_d,STEMS,DRYBIOMAS_d,GRNBIOMAS_d,ERRFLAG,SPN,GEOSUB)
C This subroutine is for R user to calculate biomass      !
C YW 05/09/2019
! YW 2019/08/05 Added input variables SPN and GEOSUB and also make BIOEQ
! be able to use FIA biomass equation number.

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::biomasslib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'biomasslib_r_'::biomasslib_r

      IMPLICIT NONE
      
      DOUBLE PRECISION DBH_d,THT_d,CL_d,HT1_d,HT2_d,CV4_d,TOPD_d
      DOUBLE PRECISION DRYBIOMAS_d,GRNBIOMAS_d
      CHARACTER*12 BIOEQ,GEOSUB,NVELBEQ
      INTEGER SPEC,STEMS,ERRFLAG,SPN 
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,CV4,VOL(15),TOPD,CL,CR
      REAL DRYBIOMAS,GRNBIOMAS
      REAL SG(11), MC
      
      DBHOB = REAL(DBH_d)
      HTTOT = REAL(THT_d)
      HT1PRD = REAL(HT1_d)
      HT2PRD = REAL(HT2_d)
      CV4 = REAL(CV4_d)
      TOPD = REAL(TOPD_d)
      VOL(4) = CV4
      CL = REAL(CL_d)
      ERRFLAG = 0
      DRYBIOMAS = 0.0
      GRNBIOMAS = 0.0
      IF(HTTOT.GT.1.0.AND.CL.GT.1.0) CR = CL/HTTOT
      CALL BiomassLibrary2(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     + HT2PRD,TOPD,STEMS,VOL,DRYBIOMAS, ERRFLAG,SPN,GEOSUB)
      IF(SPN.EQ.0)THEN
        READ(BIOEQ(4:6),'(I3)') SPEC
      ELSE
        SPEC = SPN
      ENDIF
      CALL MILESDATA(SPEC, SG)
      MC = (SG(9)-SG(10))/SG(10)
      GRNBIOMAS = DRYBIOMAS*(1.0+MC)
      DRYBIOMAS_d = DBLE(DRYBIOMAS)
      GRNBIOMAS_d = DBLE(GRNBIOMAS)
      RETURN
      END
C ********************************************************************
C This subroutine return the species default weight factor for R
      subroutine getwtfactor_r(REGN,FORST,SPEC,GRNWF_d,DRYWF_d)
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::getwtfactor_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'getwtfactor_r_'::getwtfactor_r

      IMPLICIT NONE
      
      DOUBLE PRECISION GRNWF_d,DRYWF_d
      REAL WF(3),DRYWF
      INTEGER REGN,SPEC
      CHARACTER*2 FORST
      CHARACTER*12 BMSEQ(8), BIOEQ
      CHARACTER*48 REF(8)
      WF = 0.0
      CALL CRZSPDFT(REGN,FORST,SPEC,WF,BMSEQ,REF)
      
      DRYWF = WF(1)/(1.0+WF(3)/100.0)
      GRNWF_d = DBLE(WF(1))
      DRYWF_d = DBLE(DRYWF)
      RETURN
      END
      
      