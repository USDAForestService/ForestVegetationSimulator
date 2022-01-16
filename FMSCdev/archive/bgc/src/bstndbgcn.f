C  STAND-BGC MODEL -- A physiological, entity level model derived from Forest-BGC
C  (Running 1986) by Kelsey Milner and Dean Coble.
C
C DATE OF LAST REVISION:
C
C REVISED 6/6/03 Amended format for WRITE(66,994)--"EXPAND" now written to 2
C                decimal places.  AJM
C                Changed the WRITE(74,898) so that reported grass height is 
C                in feet instead of meters.  all other heights in this output
C                file are in feet).
C
C...Numerous changes made 10/16/00 ajm.  Added LA_SH and associated variables (sumla_, trans_,
C daytrans_, sumtrans_; see entity.f77).  Changed the three WRITE statements pertaining to file 70
C (daystand.out) that report leaf areas.  Added LA_SH (time zero) and SUMLA_SH/AREA for year end, and
C headers.  Three "WRITES" affected: lines 76, 86, 341.
C
C Added FVS_CYC as a passed argument from here to: (1) BGROWN (for its passing to RESPIRE) and (2) 
C BZCOMP.  In both RESPIRE and BZCOMP two calculations were heretofore done (needlessly) at the
C beginning of each cycle instead of only once per simulation.  Now they are done only when FVS_CYC
C is equal to 1.
C
C  Adding FVS_CYC as a passed variable to BSTRUCTN. This addition works hand-
C  in-hand with the new subroutine called from BSTRUCTN.  That new subroutine 
C ("REDOLAI") is only called during first year of first cycle (it only 
C  affects how entities are initialized [but as an aside, it needs to be called
C  late, as it is,--what it does cannot be done in or from BINITIAL]); hence
C  we need to know if we are in cycle one. ajm 6/03
C
C Added 3 new common block variables for accumulating maintResp by entity by tissue pool
C accumulated in RESPIRE: YRMRLF(i),YRMRST(i),YRMRRT(i).
C Adding them to YRENTY output along with the already-existing
C growth maintResp variables ETYLFGR(I),ETYSTGR(I), and ETYFRGR(I) 8/1/03 ajm
C Also (7/25/03) added SUMRT vars (concurrent w/changes related to making water buckets 
C  a function of root biomass instead of leaf area)
C
C 1/26/04 this version "3" identical to version "2R" except I'm changing the writing-out 
C  of the "dayentity" file for an NAU-specific analysis.  This is a huge output file---
C  one line per entity per day.  For my current analysis, it will generate output for one
C year only, (2003) and will generate approx 204*365=74460 lines. ajm
C REVISING FOR CVS POSTING VERSION2.
C COMMENTING OUT A WRITE AJM 7/7/05
C
      SUBROUTINE BSTNDBGCN(IYRS,FVS_CYC)
C
C CALLED FROM BGCGROW
C
      INTEGER YEAR, FVS_CYC
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
      INCLUDE 'BGCCOM.F77'
CC------------------------------------------------------------
C BRANCH AROUND INITIALIZATION IF USING BGC INCREMENTS AND
C BGC WAS ALREADY INITIALIZED
C------------------------------------------------------------
C     IF(IBGC.EQ.1 .AND. FVS_CYC.GT.1) GOTO 1
C------------------------------------------------------------
C  OPEN BGC OUTPUT FILES
C------------------------------------------------------------
      CALL BIOPEN
C------------------------------------------------------------
C------------------------------------------------------------
C CALL INITIALIZATION SUBROUTINES
C------------------------------------------------------------
      CALL BINITIAL
C------------------------------------------------------------
C BEGIN THE YEARLY LOOP
C------------------------------------------------------------
    1 CONTINUE
      WRITE(*,*)
      WRITE(*,*) 'BGC IS RUNNING -- THIS WILL TAKE A FEW MINUTES...'
      WRITE(*,*)
      WRITE(*,*) 'NUMBER OF ENTITIES: ',NB
      WRITE(*,*)
      YRFLAG=1                    !set year 1 flag for ZCOMP subroutine
      DO 5 J=1,IYRS             !year loop
C Calculate LAI for lifeforms
        SITELAI=SUMLA/AREA
        LA_GR=SUMLA_GR/AREA		!*!
        LA_SH=SUMLA_SH/AREA               !ajm 10/00
        LA_SM=SUMLA_SM/AREA		!*!
        LA_LG=SUMLA_LG/AREA		!*!
C
        CALL BSTRUCTN(FVS_CYC)
        YEAR=INT(Z1(1) + 1)
        WRITE(*,*)
        WRITE(*,*) 'SITE TOTAL LAI IS ',SITELAI
        WRITE(*,*) 'PROCESSING YEAR: ',YEAR,' ;CYCLE:',FVS_CYC
C Write out initial values for entities (end of year=0)
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
      IF(J.EQ.1) THEN
        ZERO=0.0
        DO 7 L=1,NB
          IF(L.EQ.1) WRITE(66,992)
  992     FORMAT(/,T2,'FVS',T7,'YR',T11,'ETY',T20,'TREE',T26,'SP',T33,
     +   'PSN',T41,'TRANSP',T53,'MRESP',T64,'GRESP',T76,'LEAF',T87,
     +   'STEM',T98,'ROOT',T106,'ABSRAD',T115,'TURNOVR',4X,'TPH',/,
     +   T2,'CYC',T31,'(kgC)',T43,'(m3)',T53,'(kgC)',T64,'(kgC)',T75,
     +   '(kgC)',T86,'(kgC)',T97,'(kgC)',T108,'(GJ)',T117,'(kgC)',/,
     +'=================================================================
     +================================================================')
          WRITE(66,994) FVS_CYC,ZERO,L,TREENO(L),SPP(L),ZERO,ZERO,ZERO,
     +    ZERO,LEAF(L),STEM(L),ROOT(L),ZERO,ZERO,EXPAND(L)
          BOLECARB=BOLECARB+STEM(L)*EXPAND(L)
    7   CONTINUE
C
C write out stand level data, labels and year 0, to yrstand.out
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
        WRITE(70,680)
  680   FORMAT(/T2,'FVS',T7,'YR',T16,'YRPSN',T25,'YRTRANS',T36,
     +  'YRMRESP',T47,'YRGRESP',T59,'LAI_GR',T70,'LAI_SH',T81,'LAI_SM',
     +  T92'LAI_LG',
     +  T102,'SITELAI',T115,'TSTEM',T125,'CCF',T130,'ABSRAD'/,T2,'CYC'
     +     ,T13,'(kgC/ha)',T25,'(m3/ha)',T35,'(kgC/ha)',
     +     T46,'(kgC/ha)',T58,'(m2/m2)',T69,'(m2/m2)',T80,'(m2/m2)',
     +     T91,'(m2/m2)',T102,'(m2/m2)',T112,'(kgC/ha)',T129,'(GJ/ha)'/,
     +'=================================================================
     +==================================================================
     +=====')

	WRITE(70,490) FVS_CYC, INT(ZERO), ZERO, ZERO, ZERO, ZERO,LA_GR,
     +              LA_SH, LA_SM, LA_LG, SITELAI, BOLECARB, SUMCCF,ZERO
  490   FORMAT(2I4,1X,10(1X,F10.2),1X,F7.1,1X,F7.0)
      ENDIF
C
C------------------------------------------------------------
C BEGIN DAILY LOOP
C------------------------------------------------------------
        DO 10 K=1,ND             !day loop
        IF(K.EQ.1) WRITE(*,*) 'PROCESSING DAY:  ',K
 1990   IF(IEOF.LT.0) REWIND 60
        IF(MOD(K,60).EQ.0) WRITE(*,*) 'PROCESSING DAY:  ',K
C Read in data from MTCLIM
          READ(60,2000,END=1990,IOSTAT=IEOF) JD,TMAX,TMIN,RH,XRAD,PPT,TR
 2000     FORMAT(1X,I3,2X,3(F8.2),F10.2,F8.2,F8.5)
C          WRITE(*,*)'JD= ',JD,'TMAX= ',TMAX,'TMIN= ',TMIN,'RH= ',RH,
C     &              'XRAD= ',XRAD,'PPT= ',PPT,'TR= ',TR
C!!!THE INTERVAL ALGORITHM NOT YET IMPLEMENTED--1/5/94,DWC
C         IF(JD.EQ.365) FLAG=1  !update pools & dimensions yearly
C         IF(JD.EQ.INTERVAL(IB,1)) THEN
C           FLAG=1
C           INTERVAL(IB,1)=INTERVAL(IB,1) + JD
C         ELSE
C           FLAG=0
C         ENDIF
C------------------------------------------------------------
C Call daily timestep subroutines
C------------------------------------------------------------
          CALL BABSRAD           !calculate absorbed radiation
          CALL BZCOMP(FVS_CYC)   !calculate climatic variables--(FVS_CYC) added 2/01 ajm
          CALL BWATER            !calculate site water balance
          DO 20 I=1,NB          !entity loop
            CALL SETLEAFN(I)     !set LEAF_ON & FROOT_ON
            CALL SITOCN(I)
            CALL BGROWN(I,FVS_CYC)  !Added FVS_CYC for BGROWN's call to RESPIRE.  ajm 2/16/01
C accumulate across entities daily and expand to unit/area basis
            SUMPSN=SUMPSN + XPSN * EXPAND(I)
            SUMTRANS=SUMTRANS + TRANS * EXPAND(I)
            SUMZQ1=SUMZQ1 + ETZQ1(I)*EXPAND(I)
C--- modified -----------------------------------------------------------------------
            SUMTRANS_GR=SUMTRANS_GR + TRANS_GR * EXPAND(I)   !*! added 10/97 -epv
            SUMTRANS_SH=SUMTRANS_SH + TRANS_SH * EXPAND(I)     ! Added 10/00 ajm
            SUMTRANS_SM=SUMTRANS_SM + TRANS_SM * EXPAND(I)   !*! added 10/97 -epv
            SUMTRANS_LG=SUMTRANS_LG + TRANS_LG * EXPAND(I)   !*! added 10/97 -epv
            SUMTRANS_LG1=SUMTRANS_LG1 + TRANS_LG1 * EXPAND(I)   !*!
            SUMTRANS_LG2=SUMTRANS_LG2 + TRANS_LG2 * EXPAND(I)   !*!
C------------------------------------------------------------------------------------
            SUMGRESP=SUMGRESP + TOTGRESP * EXPAND(I)
            SUMMRESP=SUMMRESP + TOTMRESP * EXPAND(I)
C accumulate each entity daily for allocation fraction subroutine
            AFPSN(I)=AFPSN(I) + XPSN
            AFTRANS(I)=AFTRANS(I) + TRANS
            AFGRESP(I)=AFGRESP(I) + TOTGRESP
            AFMRESP(I)=AFMRESP(I) + TOTMRESP
C accumulate for each entity for update subroutine
            ETYZQ1(I)=ETYZQ1(I) + ETZQ1(I)
            ETYPSN(I)=ETYPSN(I) + XPSN
            ETYTRANS(I)=ETYTRANS(I) + TRANS
            ETYGRESP(I)=ETYGRESP(I) + TOTGRESP
            ETYMRESP(I)=ETYMRESP(I) + TOTMRESP
            ETYLFGR(I)=ETYLFGR(I) + GRLEAF
            ETYSTGR(I)=ETYSTGR(I) + GRSTEM
            ETYFRGR(I)=ETYFRGR(I) + GRFROOTS
C call update subroutine then zero the accumulators

      IF (XPSN.LT.TOTMRESP) THEN
c         STRESSDAYS(I)=STRESSDAYS(I)+1		!*!  # days of stress an entity experiences in a year
c         write (*,*) i,jd,xpsn,totmresp,stressdays(i)
      ENDIF
C***************************************************************************
C write out growth loop variables
            !output entity-level daily variables
C Commenting out the writing of dayentity output file for release version
C ajm 11/02
C Adding back for nau project testing
C BYPASSING FOR RELEASE AJM JULY 05
      GOTO 12
C****************************************************************************
            IF(Z1(1).EQ.1 .AND. JD.EQ.1 .AND. I.EQ.1) WRITE(65,995)
  995         FORMAT(/,'CYC',T4,'YEAR',T11,'JD',T17,'TRENO',T27,'LA',
     +             T34,'OCPNCY',T44,'BUCKET1',T52,'BUCKET2',
     +             T61,'SWP1',T71,'SWP2',
     +             T79,'RADABS',T90,'TRAN',T100,'PSN',
     +             T108,'MRESP',T117,'GRESP',/,
     +      T27,'(m2)',T34,'(m2)',T44,'(m3)',T52,'(m3)',T61,'(-MPa)',
     +      T71,'(-MPa)',T79,'(Kj)',T90,'(l)',T100,'(gC)',T108,'(gC)',
     +      T117,'(gC)',/,
     +'=================================================================
     +===============================================================')
c            IF (MOD(JD,30).eq.0)
            IF (FVS_CYC.EQ.1 .AND.YEAR .EQ.5)
     +        WRITE(65,996) FVS_CYC,Z1(1),JD,TREENO(I),LA(I),OCPNCY(I),
     +             BUCKET(1),BUCKET(2),SWP(1),SWP(2),
     +             ETZQ1(I),TRANS*1000,XPSN*1000,TOTMRESP*1000,
     +             TOTGRESP*1000
  996         FORMAT(I2,2X,F4.0,I4,1X,I8,6(1X,F8.2),1X,F9.0,4(1X,F8.2))
C***************************************************************************
C           WRITE(*,*) JD,I,AFPSN(I),AFTRANS(I),AFMRESP(I),AFGRESP(I)
C           IF(JD.EQ.365) THEN
C             WRITE(*,*) JD, YRPSN, YRTRANS, YRMRESP, YRGRESP
C             WRITE(*,*) JD, I, LEAF(I), STEM(I), ROOT(I)
C             WRITE(*,*) I, D(I), H(I), LEAF(I), STEM(I)
C             WRITE(*,*) JD, I, LEAFCF(I),STEMCF(I),FROOTCF(I)
C           ENDIF
C zero out accumulators. BUCKET is zeroed based on the assumption that
C the entity's water bucket will recharge at night from the surrounding
C soil profile, if water is available. See GROW.FOR for more--12/9/93,DWC
c !*!            BUCKET=0.0
C
C*******************************************************************************
   12 CONTINUE

            DO 15 SL=1,NSL		!*!
               BUCKET(SL)=0.0		!*! modified 12/97 -epv
   15       CONTINUE			!*!

            XPSN=0.0
            SUBCL=0.0
            TRANS=0.0
            TRANS_GR=0.0		!*!
            TRANS_SH=0.0                  !added 10/99  ajm
            TRANS_SM=0.0		!*! modified 12/97 -epv
            TRANS_LG=0.0		!*!
            TRANS_LG1=0.0		!*!
            TRANS_LG2=0.0		!*!
C------------------------------------------------------------------------------------
            ETZQ1(I)=0.0
            ETZQ2(I)=0.0
   20     CONTINUE			! end entity loop
C accumulate across days for annual total
          YRPSN=YRPSN + SUMPSN
          YRTRANS=YRTRANS + SUMTRANS
          YRZQ1=YRZQ1 + SUMZQ1
          YRMRESP=YRMRESP + SUMMRESP
          YRGRESP=YRGRESP + SUMGRESP
C write out results
C         WRITE(*,998) JD, SUMPSN,SUMTRANS,SUMGRESP,SUMMRESP, SITEWP
C         WRITE(*,998) JD, YRPSN, YRTRANS, YRMRESP, YRGRESP, SITEWP
C output stand-level daily variables for year 1
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
          IF(Z1(1).EQ.1 .AND. JD.EQ.1) WRITE(64,997)
  997     FORMAT(/,T2,'FVS',T6,'YR',T11,'JD',T19,'PPT',T27,'TMAX',T36,
     +   'TMIN',T46,'PSN',T53,'TRANS',T62,'MRESP',T72,'SWP1',T81,
     +   'SWP2',T88,'RADTOP',T97,'RADBOT',T105,'SITELAI',/,T2,'CYC',
     +      T18,'(mm)',T28,'(C)',T37,'(C)',T42,'(kgC/ha)',T52,'(m3/ha)',
     +      T60,'(kgC/ha)',T70,'(-MPa)',T79,'(-MPa)',T90,'(kJ)',T99,
     +      '(kJ)',T105,'(m2/m2)',/,
     +'=================================================================
     +===============================================')
C          IF(Z1(1).EQ.1)   !commented out 10/00 ajm.  Now write for all years.
         WRITE(64,998) FVS_CYC,Z1(1),JD,PPT,Z1(4),Z1(5),SUMPSN,
     +      SUMTRANS,SUMMRESP,SWP(1),SWP(2),RADTOP, RADBOT, SITELAI
  998       FORMAT(I4,F4.0,I4,11(1X,F8.2))
C zero-out daily accumulators for the next day.
          DAYTRANS=SUMTRANS   !Carry daily transp to WATER subroutine
          DAYTRANS_GR=SUMTRANS_GR 	!*!
          DAYTRANS_SH=SUMTRANS_SH         !added 10/00 ajm
          DAYTRANS_SM=SUMTRANS_SM	!*! modified 12/97 -epv
          DAYTRANS_LG=SUMTRANS_LG	!*!
          DAYTRANS_LG1=SUMTRANS_LG1	!*!
          DAYTRANS_LG2=SUMTRANS_LG2	!*!
          SUMPSN=0.0
          SUMTRANS=0.0
          SUMZQ1=0.0
          SUMTRANS_GR=0.0		!*!
          SUMTRANS_SH=0.0                 !added 10/00 ajm
          SUMTRANS_SM=0.0		!*! modified 12/97 -epv
          SUMTRANS_LG=0.0		!*!
          SUMTRANS_LG1=0.0		!*!
          SUMTRANS_LG2=0.0		!*!
          SUMGRESP=0.0
          SUMMRESP=0.0
   10   CONTINUE			! end day loop
C
C-------------------------------------------------------------
C BEGIN YEAR-END CALCULATONS
        SUMLA=0.0
        SUMLA_GR=0.0	!*!  LA for grass only
        SUMLA_SH=0.0    !*!  LA for shrubs (added 10/00, ajm)
        SUMLA_SM=0.0	!*!  LA for small trees  modified 12/97 -epv
        SUMLA_LG=0.0	!*!  LA for large trees
C
C NEW VARIABLES FOR NEW METHOD OF DETERMINING WATER BUCKETS.
C USE ROOT CARBON INSTEAD OF LEAF AREA. ajm 7/03
C
        SUMRT=0.0
        SUMRT_GR=0.0
        SUMRT_SH=0.0
        SUMRT_SM=0.0
        SUMRT_LG=0.0

C        stressdays=0.0  !*!  sets array to 0
      DO 30 I=1,NB         !entity loop for yearly calculations
C  year-end allocation fraction subroutine
         CALL BUPDATE(I,YEAR)
         CALL BALLOCA(I)
         !sum updated stem carbon pools across tree entities
         IF(ID(I).EQ.'T') TSTEM=TSTEM+STEM(I)*EXPAND(I)
C Call year-end turnover subroutine (doesn't include total plant trnovr).
C Note: structure is updated at the start of the next year to avoid
C   calling STRUCT twice.
         CALL BTRNOVR(I)
C Accumulate total stand turnover (per hectar).
         YRSTNDTO=YRSTNDTO + TOTALTO(I)*EXPAND(I)
        !output entity-level annual variables
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
C       WRITE(66,994) FVS_CYC,Z1(1),I, TREENO(I),SPP(I), ETYPSN(I),
C     +    ETYTRANS(I), ETYMRESP(I), ETYGRESP(I),LEAF(I), STEM(I),
C     +    ROOT(I), ETYZQ1(I)/1000000.,TOTALTO(I),EXPAND(I)
C
C  994  FORMAT(I4,F4.0,I4,3X,I8,T26,A2,1X,F7.3,6(1X,F10.2),2X,F8.3,1X,
C     +        F8.3,1X,F6.2)
C
C Redoing table: adding maintResp and Growth resp by tissue pool.
C ajm 8/03
       WRITE(66,994) FVS_CYC,Z1(1),I, TREENO(I),SPP(I), ETYPSN(I),
     +    ETYTRANS(I), ETYMRESP(I), ETYGRESP(I),LEAF(I), STEM(I),
     +    ROOT(I), ETYZQ1(I)/1000000.,TOTALTO(I),EXPAND(I),
     +    YRMRLF(I),YRMRST(I),YRMRRT(I),ETYLFGR(I),ETYSTGR(I),
     +    ETYFRGR(I)

  994  FORMAT(I4,F4.0,I4,3X,I8,T26,A2,1X,F7.3,6(1X,F10.2),2X,F8.3,1X,
     +        F8.3,1X,F6.2,6(1X,F10.3))
C zero out entity accumulators
         ETYZQ1(I)=0.0
         ETYPSN(I)=0.0
         ETYTRANS(I)=0.0
         ETYGRESP(I)=0.0
         ETYMRESP(I)=0.0
         ETYLFGR(I)=0.0
         ETYSTGR(I)=0.0
         ETYFRGR(I)=0.0
         TOTALTO(I)=0.0
C zero the yearly entity allocation fraction accumulators for the next yr.
         AFPSN(I)=0.0
         AFTRANS(I)=0.0
         AFGRESP(I)=0.0
         AFMRESP(I)=0.0
         TLEAFON(I)=0.0
         SUMPSI(I)=0.0
C zero out new MaintResp accumulators created 8/03 ajm
         YRMRLF(I)=0.0
         YRMRST(I)=0.0
         YRMRRT(I)=0.0
C-------------------------------------------------------
C Accumulate entity increments for passing to FVS and
C write out entity list to file ENTYLIST.OUT (device 74)
C-------------------------------------------------------
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
c Also changed units in heading for 12th column to % (from %/100)
C Added ICOV (beginning-of-year % cover for understory to output
C  (Heretofore only cover increment reported)  ajm 10/26/00
         IF(I.EQ.1) WRITE(74,896)
  896    FORMAT(/,T2,'FVS',T7,'YR',T10,'INDX',T16,'ID',T22,'TRENO',T33,
     &  'IHT',T41,'IDBH',T51,'ICR',T60,'ICW',T68,'ICOV',
     &   T79,'HG',T88,'DG',T94,
     &  'CRINC',T103,'CUMHG',T112,'CUMDG',T121,'COVINC',T129,'SPECIES',
     &   /,T2,'CYC',T31,'(ft.)',T40,'(in.)',T48,'(%/100)',T58,'(ft.)',
     &   T69,'(%)'
     &   T76,'(ft.)',T85,'(in.)',T95,'(%)',T103,'(ft.)',T112'(in.)',
     &   T123,'(%)',/,
     & '================================================================
     &==================================================================
     &=====')
         IF(ID(I).EQ.'T') THEN
C Set expansion factor to zero for trees killed in BTRNOVR and pass to FVS.
C          IF(IDEADFLG(I).EQ.1) EXPAND(I)=0.0
           DGIB=(D(I)-O_D(I))/1.074  ! Converts to dib
           DGIB=DGIB/2.54            ! Converts to inches
           HTINC=(H(I)-O_H(I))*3.28  ! Converts to feet
           FVSHT=O_H(I)*3.28
           FVSDBH=O_D(I)/2.54
           FVSCW=O_CW(I)*3.28
           CRINC=(CR(I)-O_CR(I))*100 ! Converts to %
           SUMDG(I)=SUMDG(I)+DGIB
           SUMHG(I)=SUMHG(I)+HTINC
           PASSDG(I)=SUMDG(I)
           PASSHG(I)=SUMHG(I)
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
c           WRITE(74,897)FVS_CYC,YEAR,I,ID(I),TREENO(I),FVSHT,FVSDBH,
c     &     O_CR(I),FVSCW,HTINC,DGIB,CRINC,SUMHG(I),SUMDG(I),SPP(I)
c  897      FORMAT(I4,2(I4,1X),2X,A1,1X,I8,4(1X,F8.2),9X,5(1X,F8.2)
c     &            T131,A2)
c adding radial growth in mm, printing to %cover column for now
c (which is blank for trees anyway.)ajm 2/10/04
           WRITE(74,897)FVS_CYC,YEAR,I,ID(I),TREENO(I),FVSHT,FVSDBH,
     &     O_CR(I),FVSCW,DGIB*12.7,HTINC,DGIB,CRINC,SUMHG(I),SUMDG(I),
     &     SPP(I)
  897      FORMAT(I4,2(I4,1X),2X,A1,1X,I8,4(1X,F8.2),2x,f5.3,2x,
     &            5(1X,F8.2),T131,A2)
         ELSE IF ((ID(I).EQ.'G').OR.(ID(I).EQ.'S')) THEN
C Define and write out increments in height and cover for grass and shrubs
           HTINC=(H(I)-O_H(I))*3.28  ! Converts to feet
           COVINC=(PCOVER(I)-O_PCOVER(I))
           WRITE(74,898) FVS_CYC,YEAR,I,ID(I),O_H(I)*3.28,O_PCOVER(I),
     &     HTINC,COVINC,SPP(I)     ! added initial cover ajm 10/00
  898      FORMAT(I4,2(I4,1X),2x,A1,T28,F8.2,T67,F5.2,T76,F5.2,T121,F5.1,
     &            T131,A2)
         ENDIF
   30 CONTINUE	! end entity loop for yearly calculations
C---------------------------------------------------
C
C

C write out stand level data for the year to yrstand.out
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
       WRITE(70,690) FVS_CYC,YEAR, YRPSN, YRTRANS, YRMRESP, YRGRESP,
     +    SUMLA_GR/AREA, SUMLA_SH/AREA, SUMLA_SM/AREA, SUMLA_LG/AREA,
     +            SUMLA/AREA, TSTEM, SUMCCF, YRZQ1/1000000.
  690  FORMAT(I4,I4,1X,10(1X,F10.2),1X,F7.1,1X,F7.0)
          SUMCCF=0.0
          SITELAI=0.0
          LA_GR=0.0	!*!
          LA_SH=0.0     ! added 10/00 ajm
          LA_SM=0.0	!*! modified 12/97 -epv
          LA_LG=0.0	!*!
          TSTEM=0.0

C Call year-end mortality subroutine (total plant death only)
        CALL BKILL(FVS_CYC)
C zero the yearly accumulators for the next yr.
        YRPSN=0.0
        YRTRANS=0.0
        YRZQ1=0.0
        YRMRESP=0.0
        YRGRESP=0.0
        SUMWP=0.0
        YRSTNDTO=0.0
C zero the BASE(I,J), LLA(I,J), & LAI(I,J) arrays
        DO 40 M=1,SIZE
          DO 50 N=1,SIZE*2
            LAI(M,N)=0.0
            LLA(M,N)=0.0
            BASE(M,N)=0.0
            ZONELA(N)=0.0
   50     CONTINUE
   40   CONTINUE
    5 CONTINUE         		!end yearly loop
C ZERO OUT THE INCREMENT ACCUMULATORS
      DO 900 I=1,SIZE
        SUMDG(I)=0.0
        SUMHG(I)=0.0
  900 CONTINUE
      BOLECARB=0.0
C RE-SET Z(1) TO ZERO
      Z1(1)=0.
      WRITE(*,*) 'STAND-BGC IS FINISHED'
      RETURN
      END

