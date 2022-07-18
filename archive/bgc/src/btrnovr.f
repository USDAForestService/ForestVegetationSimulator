      SUBROUTINE BTRNOVR(I)

C-------------------------------------------------------------------
C   THIS SUBROUTINE CALCULATES THE YEARLY LEAF, STEM, AND ROOT 
C   TURNOVER, & INDIVIDUAL TREE MORTALITY. -- DWC, 12/3/93.
C
C!!!ADDED SUBROUTINE TO UPDATE CROWN RATIOS -- DWC,5/13/94
C!!!ADDED WRITE STATEMENTS TO STRUCT.OUT SO UPDATED D,H,CR,etc.
C!!!  WILL MATCH THE UPDATED L,S,R POOLS,etc. -- DWC,10/17/94.
C
C   INDIVIDUAL SHRUB & GRASS MORTALITY OCCURS BY ATTRITION.
C   WHEN PSN=0 FOR SHRUBS AND GRASSES, TURNOVER AND RESPIRATION ARE
C   SUBTRACTED FROM THE L,S,R CARBON POOLS. EVENTUALLY, THE CARBON
C   POOLS REACH ZERO, AND SO THE SHRUB AND GRASS COMPONENTS DIE.
C
C   FOR TREES, USE YEAR-END L,S,R (POST-TURNOVER) TO BEGIN NEXT YEAR.
C   TURNOVER DOES NOT AFFECT DIMEMSIONS, SO DIMENSIONS ARE NOT UPDATED,
C   EXCEPT FOR CROWN RATIO, SINCE LEAF POOL CAN CHANGE.
C
C   FOR SHRUBS & GRASSES, USE YEAR-END ROOT CARBON (POST-TURNOVER) &
C   LEAF CARBON TURNOVER FOR NEXT YEAR'S BEGINNING CARBON POOLS. SINCE 
C   + AND - %COVER INCREMENT IS POSSIBLE, THEY CAN EXPAND & CONTRACT 
C   THEIR SITE COVERAGE AS THEIR CARBON POOLS GROW & SHRINK. SHRUB
C   HEIGHTS CANNOT DECREASE BASED ON THE ASSUMPTION THAT DYING SHRUBS
C   SEEM TO REDUCE THEIR %COVER, NOT THEIR HEIGHT.
C
C  !AMMENDED THE 'BACKCALCULATION' OF % COVER (FROM LEAF C) FOR UNDERSTORY
C   SO THAT %COVER IS ZERO WHEN LEAF C IS ZERO.  EQUATIONS
C   FOR A NEW LINE ARE PRESENTED THAT TAKE EFFECT ONLY WHEN LEAF C IS SUCH
C   THAT % COVER (AS CALCULATED BY ORIGINAL ALGORITHM) IS 0.5% OR LESS --AJM 10/26/00
C-------------------------------------------------------------------

      REAL NEW_CR
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

      IB=IBLB(I)
C-------------------------------------------------------------------
C calculate the yearly leaf, stem, & root turnover of healthy plants
      TOLEAF(I)=LEAF(I) * (B1(9,IB)/100.)
      TOSTEM(I)=STEM(I) * (B1(10,IB)/100.)
      TOFROOT(I)=ROOT(I) * (B1(11,IB)/100.)
      TOTALTO(I)=TOLEAF(I) + TOSTEM(I) + TOFROOT(I)
C-------------------------------------------------------------------
C PUT TURNOVER INTO DEAD POOLS
C!!!ADD DEAD_CARBON POOLS HERE

C-------------------------------------------------------------------
C UPDATE THE LEAF, STEM, & ROOT CARBON POOLS, & LEAF AREA, AND
C PREPARE FOR THE NEXT YEAR.

C update carbon pools
      IF(ID(I).EQ.'T') THEN
        LEAF(I)=LEAF(I) - TOLEAF(I)
        STEM(I)=STEM(I) - TOSTEM(I)
        ROOT(I)=ROOT(I) - TOFROOT(I)
        LA(I)=LEAF(I) * B1(13,1)
C!!!ADDED SUBROUTINE TO UPDATE CROWN RATIOS -- DWC,5/13/94
        !update crown ratios
        CALL BCROWN(I,NEW_CR)
        CR(I)=NEW_CR     !replaced right-side w/ NEW_CR--DWC,6/2/94
        !output tree-level variables using MOD variable
C        IF(Z1(1).EQ.1 .AND. I.EQ.1) WRITE(63,993)
C  993     FORMAT('YEAR',T7,'ETY',T15,'TREE',T24,'DBH(cm)',T33,
C     +           'HT(m)',T43,'CR(.)',T51,'CW(m)',T57,'EXPFAC(tph)',
C     +           T75,'SPP')
C        IF(Z1(1).EQ.1 .OR. MOD(Z1(1),1).EQ.0. .OR. Z1(1).EQ.4 .OR.
C     +     Z1(1).EQ.6)
C     +    WRITE(63,995) Z1(1),I,TREENO(I),D(I),H(I),CR(I),
C     +                 CW(I),EXPAND(I),SPP(I)
C  995     FORMAT(F4.0,I4,2X,I8,1X,5(1X,F8.2),T75,A2)
      ELSE IF(ID(I).EQ.'S') THEN
C*****************************************************************
c        LEAF(I)=TOLEAF(I) * .7   !Commented out 10/16/00 AJM
C  I do not understand above logic.  Why should leaf biomass equal
C  a fraction of what was turned over? If we want to replace some of turned-
C  over biomass, how about LEAF=LEAF-([TRNOVERate*LEAF]*FRACTION).  Perhaps it would
C  be better to subtract turnover BEFORE growth is added in line 84 BUPDATE?
c
C  Adding line below: LEAF=LEAF-TRNOVER.  Unlike grasses, I am putting this
C  before calculation of %cover.  Note that turnover is post-growth turnover
C  rates.  100% leaf turnover will kill an entity!
        LEAF(I)=LEAF(I)-TOLEAF(I) !Same as trees (above) added 10/16/00 ajm
        !note: PCOVER solved by inverting biomass eqn. in INITIAL.FOR
        !note: pcover should never exceed 100%
C  Changing 1/0.5 to 1/0.3.  ajm 10/16/00. This correction makes this eqn the
C  correct "inverse" of the biomass eqn in BINITIAL.
C        PCOVER(I)=((LEAF(I)*(1./B2(18)) * 500./AREA * (1/0.5))
C*****************************************************************
C  Adding new condition (like for grasses below) whereby if leaf C mass
C  is low enought such that % cover will be 0.5% or less, then use different
C  equation for calculating % cover.  This equation is also linear; it sets
C  % cover to zero when Leaf C mass = 0, and goes through the same point
C  at 0.5% cover as the original equation.  AJM 10/26/00.
C  Also, added condition (like that for grasses below) that % cover cannot
C   exceed 100%.
C
        IF((1./ H(I))*(0.02625*LEAF(I) / B2(18) - 0.26162).LT.0.5) THEN
        PCOVER(I)=(0.013125 / (B2(18)*(0.5 * H(I) + 0.26162))) !slope of line
     &             * LEAF(I)
        ELSE
        PCOVER(I)=((LEAF(I)*(1./B2(18)) * 500./AREA * (1/0.3))
     +             - 1.66075) * (1./(100.*H(I))) * (1/0.06348)
        ENDIF
C
        PCOVER(I) = MIN(100.0,PCOVER(I)) !ADDED 10/00 AJM
C
        STEM(I)=STEM(I) - TOSTEM(I)
        ROOT(I)=ROOT(I) - TOFROOT(I)
        LA(I)=LEAF(I) * B1(13,2)
        !output shrub structure variables
        !use same format statement as grass
C        IF(Z1(1).EQ.1 .OR. MOD(Z1(1),1).EQ.0.)
C     +    WRITE(63,997) Z1(1),I,ID(I),H(I),PCOVER(I),EXPAND(I),SPP(I)
      ELSE IF(ID(I).EQ.'G') THEN
C OLD WAY        LEAF(I)=TOLEAF(I)  !!may multiply by a fraction
C        !note: PCOVER solved by inverting biomass eqn. in INITIAL.FOR
C        !note: pcover should never exceed 100%
C**********************************************************************
C   Adding new condition on this function to prevent low leaf biomass
C   from causing negative % covers.  Now, if leaf biomass is below the value
C   resulting in 0.5 %cover (using original function), then percent cover will
C   be calculated using a different linear function, one that intersects these points:
C   (9.092 kg leaf C, 0.5% cover [assuming B2(18)=0.5]) and (0 kg leaf C, 0% cover)
C   This will prevent shrubs and grasses from dying.  Mortality of grass
C   now should only occur if leaf turnover rate is set to 100%. AJM 10/26/00
C
        IF(LEAF(I)/B2(18).LT.18.18432) THEN
        PCOVER(I)=(0.5 / (18.18432 * B2(18))) * LEAF(I)
        ELSE
        PCOVER(I)=((LEAF(I)*(1./B2(18)) *500./AREA) - 0.78009)
     +            * (1./0.25822)
        ENDIF
       PCOVER(I)=MIN(100.0,PCOVER(I)) !I put this back in.  It was commented out.  ajm 10/00
C
C       LEAF(I)=(.78009+.25822*PCOVER(I))*(1./500.)*AREA*B2(18)
        LEAF(I)=LEAF(I) - TOLEAF(I)
        STEM(I)=STEM(I) - TOSTEM(I)
        ROOT(I)=ROOT(I) - TOFROOT(I)
        LA(I)=LEAF(I) * B1(13,3)
        !use MOD variable to select output years
C        IF(Z1(1).EQ.1 .OR. MOD(Z1(1),1).EQ.0.)
C     +    WRITE(63,997) Z1(1),I,ID(I),H(I),PCOVER(I),EXPAND(I),SPP(I)
C  997     FORMAT(F4.0,I4,T17,A1,T33,F5.2,T42,F5.2,T60,F5.2,T75,A2)
        ENDIF

      SUMLA=SUMLA + LA(I)*EXPAND(I)
      SUMRT =SUMRT + ROOT(I)*EXPAND(I)
C--- modified (epv) -----------------------------------------------------------------
C ADDING IN SUMRT VARIABLES FOR NEW WATER BUCKET CALCULATION METHOD AJM 7/03

      IF (ID(I).EQ.'G') THEN					!*!
          SUMLA_GR=SUMLA_GR + LA(I)*EXPAND(I)			!*
          SUMRT_GR=SUMRT_GR + ROOT(I)*EXPAND(I)
      ELSE IF (ID(I).EQ.'S') THEN 				!*! Added 10/00 ajm
          SUMLA_SH=SUMLA_SH + LA(I)*EXPAND(I)			!*!
          SUMRT_SH=SUMRT_SH + ROOT(I)*EXPAND(I)
      ELSE IF ((ID(I).EQ.'T') .AND. (H(I).LT.HTLIMIT)) THEN 	!*!
          SUMLA_SM=SUMLA_SM + LA(I)*EXPAND(I)			!*!
          SUMRT_SM=SUMRT_SM + ROOT(I)*EXPAND(I)
      ELSE							!*!  added 12/97
          SUMLA_LG=SUMLA_LG + LA(I)*EXPAND(I)			!*!
          SUMRT_LG=SUMRT_LG + ROOT(I)*EXPAND(I)
      ENDIF							!*!
C        IF(Z1(1).EQ.1 .OR. MOD(Z1(1),1).EQ.0.)
C     +    WRITE(63,998) Z1(1), sumla_gr,sumla_sm,sumla_lg
C  998     FORMAT(F4.0,T55,3(F8.2)) !!NOTE.  If you reactivate this, note that I have added new
C                                            variable sumla_sh.  AJM 10/00
C      WRITE (*,*)  SUMLA_GR,SUMLA_SM,SUMLA_LG 			!*!
C------------------------------------------------------------------------------------
C thin the trees & treat the grasses by reducing the expansion factor
      IF((Z1(1)+1.).EQ.THINYR .AND. PTREMOVE.NE.1.0 .AND. ID(I).EQ.'T')
     +  EXPAND(I)=EXPAND(I) * (1.0 - PTREMOVE)
      IF((Z1(1)+1.).EQ.THINYR .AND. PSREMOVE.NE.1.0 .AND. ID(I).EQ.'S')
     +  LEAF(I)=LEAF(I) * (1.0 - PSREMOVE)
      IF((Z1(1)+1.).EQ.THINYR .AND. PGREMOVE.NE.1.0 .AND. ID(I).EQ.'G')
     + THEN
        LEAF(I)=LEAF(I) * (1.0 - PGREMOVE)
        PCOVER(I)=PCOVER(I)*(1.0-PGREMOVE)
        STEM(I)=STEM(I)*(1.0-PGREMOVE)
        ROOT(I)=ROOT(I)*(1.0-PGREMOVE)
        LA(I)=LEAF(I) * B1(13,3)
      ENDIF
C-------------------------------------------------------------------
C flag dead trees (defined as trees with CR < 0), dead shrubs, & dead 
C grasses (both defined as grass with PCOVER < 0). 
C Dead plants are eliminated in the KILL.FOR subroutine called in GSV.
C---------------------------------------------------------------      
      IF(ID(I).EQ.'T') THEN
C----------------------------------------------------------------      
C  Declining vigor method
C-----------------------------------------------------------------
C      IF(O_LEAF(I).GT.LEAF(I)) THEN
C          STAT(I)=STAT(I)+1.0
C        ELSE
C          STAT(I)=0.0
C        ENDIF
C        IF(STAT(I).GE.3.0) THEN 
C          IDEADFLG(I)=1
C        ELSE 
C          IDEADFLG(I)=0
C        ENDIF
C---------------------------------------------------------------
C  Crown ratio method
C---------------------------------------------------------------
C        IF(CR(I).LE.0.0) THEN
C        IDEADFLG(I)=1
C        ELSE
C         IDEADFLG(I)=0
C        ENDIF
C----------------------------------------------------------------
C  Kill if MRESP(I) greater than PSN two years running or if
C  CR less than 0.     
C----------------------------------------------------------------
       IF(AFMRESP(I).GT.AFPSN(I)) THEN
         STAT(I)=STAT(I)+1.0
       ELSE
         STAT(I)=0.0
       ENDIF
C       IF(STAT(I).GE.10.0 .OR. CR(I).LE.0.0) THEN    !Changed 10/24/00 AJM
       IF(STAT(I).GE.10.0 .OR. CR(I).LT.0.02) THEN
         IDEADFLG(I)=1
       ELSE 
         IDEADFLG(I)=0
       ENDIF
C-----------------------------------------------------------------
      ELSE IF(ID(I).EQ.'S') THEN
        IF(PCOVER(I).LT.0.0) THEN
          IDEADFLG(I)=1
        ELSE
          IDEADFLG(I)=0
        ENDIF
      ELSE IF(ID(I).EQ.'G') THEN
        IF(PCOVER(I).LE.0.0) THEN
          IDEADFLG(I)=1
        ELSE
          IDEADFLG(I)=0
        ENDIF
      ENDIF
C-------------------------------------------------------------------
C SAVE YEAR END VALUES FOR LATER USE
C-------------------------------------------------------------------
        S_LEAF(I)=LEAF(I)
        S_STEM(I)=STEM(I)
        S_ROOT(I)=ROOT(I)
        S_CR(I)=CR(I)
        S_D(I)=D(I)
        S_H(I)=H(I)
        S_BD(I)=BD(I)
        S_TPH(I)=TPH(I)
C-------------------------------------------------------------------
C     WRITE(*,998) JD,I,TOLEAF(I),TOSTEM(I),TOFROOT(I)
C 998 FORMAT(2I4,3(1X,F8.2))
      RETURN
      END

