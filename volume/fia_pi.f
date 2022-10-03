! Asner etal 2011 High-resolution carbon mapping on the million-hectare island of Hawaii.
! Frontiers in Ecology and the Environment 9:434-439.
! Above Ground Dry Biomass in pounds
      SUBROUTINE ASNER_AGT(SPN, DBH, THT, AGT)
      INTEGER SPN
      REAL DBH,THT,AGT,P,DBHCM,THTM,WDEN,SG(11)
      ! THE ORIGINAL EQUATION DBH IN CM, THT IN M AND AGT IN KG, p in g/cm3
      DBHCM = DBH*2.54
      THTM = THT*0.3048
      CALL MILESDATA(SPN, SG)
      P = SG(4)/62.4
      AGT = 3.14*(DBHCM/2.0)**2*THTM*P/10.0
      ! RETURN AGT IN LB
      AGT = AGT*2.2046
      RETURN
      END
!======================================================================
      SUBROUTINE CHAVE_AGT(SPN,BEQ,DBH,THT,AGT)
      INTEGER SPN
      REAL DBH,THT,AGT
      CHARACTER*12 BEQ
      REAL DBHCM,THTM,P,SG(11)
      CHARACTER*2 EQTYPE
      AGT = 0.0
      DBHCM = DBH*2.54
      THTM = THT*0.3084
      CALL MILESDATA(SPN, SG)
      P = SG(4)/62.4
      EQTYPE = BEQ(10:11)
      IF(BEQ(1:3).EQ.'CHV'.OR.EQTYPE.EQ.'04')THEN
      ! Chave J et al. 2014. Improved allometric models to estimate the aboveground
      ! biomass of tropical trees. Glob Change Biol, 20: 3177.3190.
        AGT = 0.0673*(P*DBHCM**2*THTM)**0.976
      ELSE
      ! Chave J, Andalo C, Brown S, et al. 2005. Tree allometry and improved estimation
      ! of carbon stocks and balance in tropical forests. Oecologia 145: 87-99.
      ! the first digit number in the eqtype as: 1 = dry, 2 = moist, 3 = wet, 0 = all
        IF(EQTYPE.EQ.'01')THEN
          AGT = -2.801+2.115*LOG(DBHCM)+0.78*LOG(THTM)+0.809*LOG(P)
        ELSEIF(EQTYPE.EQ.'11')THEN
          AGT = -2.68+1.805*LOG(DBHCM)+1.038*LOG(THTM)+0.377*LOG(P)
        ELSEIF(EQTYPE.EQ.'21')THEN
          AGT = -2.994+2.135*LOG(DBHCM)+0.824*LOG(THTM)+0.809*LOG(P)
        ELSEIF(EQTYPE.EQ.'31')THEN
          AGT = -2.408+2.04*LOG(DBHCM)+0.659*LOG(THTM)+0.746*LOG(P)
        ELSEIF(EQTYPE.EQ.'02')THEN
          AGT = -2.922 + 0.99*LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'12')THEN
          AGT = -2.235 + 0.916*LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'22')THEN
          AGT = -3.08 + 1.007*LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'32')THEN
          AGT = -2.605 + 0.94*LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'03')THEN
          AGT = -2.994 + LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'13')THEN
          AGT = -2.843 + LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'23')THEN
          AGT = -3.027 + LOG(DBHCM**2*THTM*P)
        ELSEIF(EQTYPE.EQ.'33')THEN
          AGT = -3.024 + LOG(DBHCM**2*THTM*P)
        ENDIF
        AGT = EXP(AGT)
      ENDIF
      ! CONVERT TO LB
      AGT = AGT*2.2046
      RETURN
      END
! =====================================================================
      SUBROUTINE PI_AGT(SPN,BEQ,DBH,THT,AGT)
      INTEGER SPN,ERRFLG
      REAL DBH,THT,AGT
      CHARACTER*12 BEQ
      REAL HT1PRD,HT2PRD,CR,TOPD,BIOMS
      CR = 0.5
      HT1PRD = 0.0
      HT2PRD = 0.0
      TOPD = 4.0
      ERRFLG = 0
      IF(SPN.GE.6545.AND.SPN.LE.6549)THEN
        CALL ASNER_AGT(SPN, DBH, THT, AGT)
      ELSEIF((SPN.EQ.6001.OR.SPN.EQ.303).AND.DBH.LT.12.0)THEN
        BEQ = "AS6006AGT01D"
        CALL BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,
     &      BIOMS,ERRFLG) 
        AGT = BIOMS
      ELSEIF((SPN.EQ.7782.OR.SPN.EQ.7783).AND.DBH.LT.12.0)THEN
        BEQ = "AS7783AGT01D"
        CALL BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,
     &      BIOMS,ERRFLG) 
        AGT = BIOMS
      ELSEIF((SPN.EQ.8355).AND.DBH.LT.8.0)THEN
        BEQ = "AS8355AGT01D"
        CALL BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,
     &      BIOMS,ERRFLG) 
        AGT = BIOMS
      ELSE
        CALL CHAVE_AGT(SPN,BEQ,DBH,THT,AGT)
      ENDIF
      RETURN
      END
! =====================================================================
      SUBROUTINE PI_CRM_MSW(SPN,BEQ,DBH,THT,MSW)
      INTEGER SPN
      REAL DBH,THT,MSW,AGT
      CHARACTER*12 BEQ
      REAL BIOMS(8),JK_WDRATIO
      MSW = 0.0
      CALL JENKINS(SPN, DBH, BIOMS)
      JK_WDRATIO = BIOMS(2)/BIOMS(1)
      IF(DBH.GT.5.0)THEN
        CALL PI_AGT(SPN,BEQ,DBH,THT,AGT)
        MSW = AGT*JK_WDRATIO
      ENDIF
      RETURN
      END
! =====================================================================
      SUBROUTINE PI_BIOMASS(SPN,BEQ,DBH,THT,CV4,BIOMS,ERRFLG)
      INTEGER SPN,ERRFLG,REGN
      REAL DBH,THT,CV4,BIOMS,CR
      CHARACTER*12 BEQ
      CHARACTER*10 VOLEQ
      REAL TOPD,VOL(15),MHT,DBHOB,HTTOT,WDEN,BDEN,HT1PRD,HT2PRD
      CR = 0.5
      ERRFLG = 0
      HT1PRD = 0.0
      HT2PRD = 0.0
      IF(BEQ.EQ.'FPI999MST01D')THEN
        ! biomass equation num 000120
        IF(CV4.LE.0.0)THEN
          VOLEQ = 'CU000144'
          REGN = 5
          TOPD = 4.0
          VOL = 0.0
          CALL VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT, ERRFLG)
          CV4 = VOL(4)
        ENDIF
        CALL WOODDEN(SPN,WDEN,BDEN)
        BIOMS = CV4*WDEN
      ELSEIF(BEQ(1:9).EQ.'FPI999AGT')THEN
        ! biomass equation num 000121, 000123
        CALL PI_AGT(SPN,BEQ,DBH,THT,BIOMS)
      ELSEIF(BEQ(1:9).EQ.'FPI999MSW')THEN
        ! biomass equation num 000122
        CALL PI_CRM_MSW(SPN,BEQ,DBH,THT,BIOMS)
      ELSEIF(BEQ(1:3).EQ.'CAB'.OR.BEQ(1:3).EQ.'CHV')THEN
        ! biomass equation num 000124, 000125
        CALL CHAVE_AGT(SPN,BEQ,DBH,THT,BIOMS)
      ELSEIF(BEQ(1:3).EQ.'ASN')THEN 
        ! biomass equation num 000126
        CALL ASNER_AGT(SPN, DBH, THT, BIOMS) 
      ELSEIF(BEQ.EQ.'AS6006AGT01D'.OR.BEQ.EQ.'AS7783AGT01D'
     &       .OR.BEQ.EQ.'AS8355AGT01D')THEN
        ! biomass equation num 000127, 000128, 000129
        CALL BIOEQDB(BEQ,DBH,THT,HT1PRD,HT2PRD,CR,TOPD,
     &      BIOMS,ERRFLG)
      ENDIF
      RETURN
      END
     
      