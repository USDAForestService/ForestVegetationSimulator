      SUBROUTINE BINITIAL

      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

      
C      CALL BETAS    ! NOW INITIALIZED IN BGCINIT.F
C      CALL SOILH2O  ! now initialized in bgcinit.f
      print *,'in binitial'
      CALL HABTYPE   !Commented out 10/00 ajm.  Variables set in HABTYPE
C      used in BHTGROWTH, which is no longer used!
      CALL BIOMASS
      CALL SP2INT
      RETURN
      END


      SUBROUTINE BIOMASS
C----------------------------------------------------------------------
C THIS SUBROUTINE CONVERTS TREE DIMENSIONS TO STEM VOLUME, THEN TO 
C DRY BIOMASS USING CONVERSION OF 1 M3 WOOD=500 KG BIOMASS (1 CUFT=14.16
C KG BIOMASS). BIOMASS IN LEAVES, BRANCHES, AND ROOTS ARE ESTIMATED
C BY APPLYING RATIOS OF LEAF/STEM, BRANCHES/STEM, AND ROOT/STEM DERIVED
C FROM PUBLISHED BIOMASS EQUATIONS.
C
C GRASSES AND SHRUBS USE PUBLISHED BIOMASS EQUATIONS
C
C BIOMASS IS CONVERTED TO CARBON USING A CONVERSION =.5 KG C/1 KG BIOMASS 
C
C VOLUME EQUATIONS
C SOURCES: UNPUBLISHED CHAMPION INTERNATIONAL WESTERN OPERATION
C
C BIOMASS EQUATIONS  
C SOURCES: DF-GOWER,VOGT,GRIER. 1992. metric units
C          WL-GOWER,GRIER,VOGT,VOGT. 1987. metric units
C          PP-GHOLZ,GRIER,CAMPBELL,BROWN. 1979. metric
C          LP-AS ABOVE
C          SHRUB-OLSON,MARTIN. 1981. (Pachistima). metric
C          GRASS-AS ABOVE (pine grass)
C
C           SLA or B1(13,x)-RUNNING,HUNT. 1993.
C----------------------------------------------------------------------
C   Revised 11/12/02.  Removing index ISTND, and removing PPE common "includes"
C                      (PPEPRM, PPCNTL, & PRGPRM).  AJM
C           These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
C           BGCIN, and BGCCOM.f77--remove all PPE funtionality.
C           The FVS-BGC code is now, once again, a single stand model.
C----------------------------------------------------------------------
C
      REAL STEMB, STEMW, BRANCH, TWIG, VOL, ENGD, ENGH
      CHARACTER*4 SPEC
      INCLUDE 'BGCCOM.F77'
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
C      INCLUDE 'PPCNTL.F77'  !Added for index ISTND in IBCYC (now dimensioned
C                            by MXSTND).  ajm 8/29/00. !! REMOVED 11/02 ajm
C      INCLUDE 'PRGPRM.F77'                             ! DITTO.
C
      HTLIMIT=1.3       !*!  1.3m = 4.5 ft (the pt at which sm trees become lg trees)

C
C
C IF STAND-BGC HAS ALREADY BEEN INITIALIZED, SKIP THIS SUBROUTINE UNLESS NEW
C ENTITIES HAVE BEEN ADDED BY FVS - FROM THE REGEN MODEL FOR EXAMPLE.
C IBCYC IS SET IN BGCGROW, NB IS SET IN BENTYLOAD, BENTYUPDT AND BKILL.
C NLF IS SET IN BKILL.
C
C***********************************************
C      IF(IBCYC(ISTND).EQ.1 .AND. NB.GT.NLF) THEN    !removed 11/02 ajm
      IF(IBCYC.EQ.1 .AND. NB.GT.NLF) THEN
         I1=NLF+1
         I2=NB
C      ELSE IF(IBCYC(ISTND).EQ.0) THEN               !removed 11/02 ajm
      ELSE IF(IBCYC.EQ.0) THEN
         I1=1
         I2=NB
         stressdays=0.0                                         !*!
         sumla=0.0                                              !*!
         sumla_gr=0.0                                           !*!
         sumla_sm=0.0                                           !*!
         sumla_lg=0.0                                           !*!
         sumla_sh=0.0   !added 10/00 ajm
C NEW VARIABLES FOR NEW METHOD OF WATER BUCKET CALCULATION.
C NOW, USE ROOT BIOMASS INSTEAD OF LEAF AREA
        SUMRT=0.0
        SUMRT_GR=0.0
        SUMRT_SH=0.0
        SUMRT_SM=0.0
        SUMRT_LG=0.0

      ELSE
         GOTO 20
      END IF
C
C
      DO 10 I=I1,I2
       IF (ID(I).EQ.'T') THEN
C
C For small trees(dbh < 2.0 inches), use formula for a parabaloid to calculate
C volume and then convert to carbon. Set root and foliar carbon
C approximately=stem C. Basal area for small trees is calculated
C from basal diameter.
C
          IF(D(I).LT.5.08) THEN
            V=BA(I)*H(I)/2.0      ! parabolic volume in cubic meters inside bark.
C            BOLE=V*500.            ! VOLUME TO BIOMASS
C            DOB=5.08                ! REFERENCE DIAMETER (CM) FOR BIOMASS RATIOS
            V2=V*500.*B2(18)      ! 500 converts volume (m3) to biomass (KgC). B2(18) to kgC.
            STEM(I)=V2*(1+B2(22)) ! Increase by 25% for branches, twigs, and bark.
C For trees with height less than or equal to 4.5 feet (1.3m) assign leaf carbon based
C on estimated proportion. Otherwise use Melinda Moeur's foliar biomass equation. Convert
C lbs to Kg using .4535923.
              IF(H(I).LE.1.3) THEN
                 LEAF(I)=(.36/.28)*STEM(I)
              ELSE
                 LEAF(I)=EXP( -2.05828 + .4035*LOG(H(I)*3.3) +
     +              1.25837*LOG(H(I)*3.3*CR(I)) -
     +              .12975*LOG(TPH(I)/2.47) ) * .4535923 * B2(18)
              END IF
C
C  Implementing new model.  Initializing root biomass to 2*leaf biomass (as inferred
C  from Koch 1987). AJM 1/12/01  This also for some--not all--large trees, below.
C
C              ROOT(I)=LEAF(I)
              ROOT(I)=2.0 * LEAF(I)
C
C For trees with DBH >= 2 inches use CIC equations to get stem volume, then
C estimate leaf, branch and root biomass using ratios from biomass equations.
C Convert DBH from cm to inches and HT to feet for the CIC equations. Convert
C calculated bole volume in cubic feet to biomass using 14.16 Kg per cuft.
C Volume equation defaults to DF.
          ELSE IF(D(I).GE.5.08) THEN
            ENGD=D(I)/2.54
            ENGH=H(I)*3.28
C            SPEC='LP'     !SPP(I) commented out 10/16/00 ajm
            CALL BCVTS(SPEC,ENGD,ENGH,VOL)
            BOLE=VOL*14.16
            DOB=D(I)
C          END IF
C
C============== DOUGLAS-FIR ===========================================
             IF(SPEC.EQ.'DF') THEN
C            IF (SPP(I).EQ.'DF') THEN
C Compute biomass ratios
             STEMW=10**(-1.024 + 2.416*LOG10(DOB))                    ! Stemwood biomass eq
             RLF2S=10**(-2.347 + 2.478*LOG10(DOB)) / STEMW            ! Leaf/Stemwood biomass
             RBR2S=10**(-2.336 + 2.650*LOG10(DOB)) / STEMW            ! Branch/stemwood biomass
             RTW2S=10**(-4.108 + 2.749*LOG10(DOB)) / STEMW            ! Twig/stemwood biomass
             RBK2S=10**(-1.234 + 2.128*LOG10(DOB)) / STEMW            ! Bark/stemwood biomass
C Compute biomass in plant parts and convert to carbon using B2(18)
             BARK=BOLE*RBK2S
             BRANCH=BOLE*RBR2S
             TWIG=BOLE*RTW2S
C Calculate leaf carbon
             LEAF(I)=BOLE*RLF2S*B2(18)
C Calculate total stem carbon
             STEM(I)=(BOLE+BARK+BRANCH+TWIG)*B2(18)
C Set root carbon = leaf carbon
             ROOT(I)=LEAF(I)
C=============== WESTERN LARCH ========================================
            ELSE IF(SPEC.EQ.'WL') THEN
C           ELSE IF (SPP(I).EQ.'WL') THEN
C Compute biomass ratios
             STEMW=10**(-1.158 + 2.460*LOG10(DOB))                    ! Stemwood biomass eq
             RLF2S=10**(-2.779 + 2.499*LOG10(DOB)) / STEMW            ! Leaf/Stemwood biomass
             RBR2S=10**(-2.484 + 2.648*LOG10(DOB)) / STEMW            ! Branch/stemwood biomass
             RTW2S=10**(-3.754 + 2.166*LOG10(DOB)) / STEMW            ! Twig/stemwood biomass
             RBK2S=10**(-1.322 + 2.020*LOG10(DOB)) / STEMW            ! Bark/stemwood biomass
C Compute biomass in plant parts and convert to carbon using B2(18)
             BARK=BOLE*RBK2S
             BRANCH=BOLE*RBR2S
             TWIG=BOLE*RTW2S
C Calculate leaf carbon
             LEAF(I)=BOLE*RLF2S*B2(18)
C Calculate total stem carbon
             STEM(I)=(BOLE+BARK+BRANCH+TWIG)*B2(18)
C Set root carbon = leaf carbon
             ROOT(I)=LEAF(I)
C=============== PONDEROSA PINE =======================================
            ELSE IF(SPEC.EQ.'PP') THEN
C           ELSE IF (SPP(I).EQ.'PP') THEN
C Compute biomass ratios
             STEMW=EXP(-4.4907 + 2.7587*LOG(DOB))                     ! Stemwood biomass eq
             RLF2S=EXP(-4.2612 + 2.0967*LOG(DOB)) / STEMW             ! Leaf/Stemwood biomass
             RBR2S=EXP(-5.3855 + 2.7185*LOG(DOB)) / STEMW             ! Branch/stemwood biomass
             RBK2S=EXP(-4.2063 + 2.2312*LOG(DOB)) / STEMW             ! Bark/stemwood biomass
C Compute biomass in plant parts and convert to carbon using B2(18)
             BARK=BOLE*RBK2S
             BRANCH=BOLE*RBR2S
C Calculate leaf carbon
             LEAF(I)=BOLE*RLF2S*B2(18)
C Calculate total stem carbon
             STEM(I)=(BOLE+BARK+BRANCH)*B2(18)
C Set root carbon = leaf carbon
C *****Changing to 2:1.  AJM 1/12/01******
C	     ROOT(I)=LEAF(I)
            ROOT(I) = 2.0 * LEAF(I)
C============== LODGEPOLE PINE ========================================
            ELSE IF(SPEC.EQ.'LP') THEN
C           ELSE IF (SPP(I).EQ.'LP') THEN
C Compute biomass ratios
             STEMW=EXP(-2.9849 + 2.4287*LOG(DOB))                     ! Stemwood biomass eq
             STEMDF=10**(-1.024 + 2.416*LOG10(DOB))                   ! DF Stemwood biomass eq
             RLF2S=EXP(-3.6187 + 1.8362*LOG(DOB)) / STEMW             ! Leaf/Stemwood biomass
             RBR2S=EXP(-4.6004 + 2.3533*LOG(DOB)) / STEMW             ! Branch/stemwood biomass
             RBK2S=10**(-1.234 + 2.128*LOG10(DOB)) / STEMDF           ! Bark/stemwood biomass from DF
C Compute biomass in plant parts and convert to carbon using B2(18)
             BRANCH=BOLE*RBR2S
             BARK=BOLE*RBK2S
C Calculate leaf carbon
             LEAF(I)=BOLE*RLF2S*B2(18)
C Calculate total stem carbon
             STEM(I)=(BOLE+BRANCH+BARK)*B2(18)
C Set root carbon = leaf carbon
C *****Changing to 2:1.  AJM 1/12/01******
C	     ROOT(I)=LEAF(I)
            ROOT(I) = 2.0 * LEAF(I)
C============== ALL OTHER SPECIES ======================================
           ELSE    !use DF biomass for all other species
C Compute biomass ratios
             STEMW=10**(-1.024 + 2.416*LOG10(DOB))                    ! Stemwood biomass eq
             RLF2S=10**(-2.347 + 2.478*LOG10(DOB)) / STEMW            ! Leaf/Stemwood biomass
             RBR2S=10**(-2.336 + 2.650*LOG10(DOB)) / STEMW            ! Branch/stemwood biomass
             RTW2S=10**(-4.108 + 2.749*LOG10(DOB)) / STEMW            ! Twig/stemwood biomass
             RBK2S=10**(-1.234 + 2.128*LOG10(DOB)) / STEMW            ! Bark/stemwood biomass
C Compute biomass in plant parts and convert to carbon using B2(18)
             BARK=BOLE*RBK2S
             BRANCH=BOLE*RBR2S
             TWIG=BOLE*RTW2S
C Calculate leaf carbon
             LEAF(I)=BOLE*RLF2S*B2(18)
C Calculate total stem carbon
             STEM(I)=(BOLE+BARK+BRANCH+TWIG)*B2(18)
C Set root carbon = leaf carbon
C *****Changing to 2:1.  AJM 1/12/01******
C	     ROOT(I)=LEAF(I)
            ROOT(I) = 2.0 * LEAF(I)
           ENDIF
         END IF
C         EXPAND(I)=TPH(I)
         LA(I)=LEAF(I) * B1(13,1)
       ELSE IF (ID(I).EQ.'S') THEN
C  original (in Olson & Martin, 1981) H in cm, PCOVER in %, and
C  biomass in g/.5m2. H is converted to m & biomass to kgC/ha.
C  Note: the 1/500 & AREA convert g/.5m2 to kg/ha.
C      : H(I)*100 converts height in meters to cm.
C      : the 0.? divides above-ground biomass into leaf & stem (??:??)
            STEM(I)=(1.66075 + 0.06348*PCOVER(I)*(H(I)*100.))
     +              * (1./500.) * AREA * 0.7 * B2(18)
            LEAF(I)=(1.66075 + 0.06348*PCOVER(I)*(H(I)*100.))
     +              * (1./500.) * AREA * 0.3 * B2(18)
            ROOT(I)=LEAF(I)
            EXPAND(I)=1.
            LA(I)=LEAF(I) * B1(13,2)
       ELSE IF (ID(I).EQ.'G') THEN
            LEAF(I)=(0.78009 + 0.25822*PCOVER(I))
     +               * (1./500.) * AREA * B2(18)
            ROOT(I)=LEAF(I)
            STEM(I)=0.0
            EXPAND(I)=1.
            LA(I)=LEAF(I) * B1(13,3)
       ENDIF
C
C  added line for shrubs below.  ajm 10/00
       SUMLA=SUMLA + LA(I)*EXPAND(I)
       SUMRT=SUMRT + ROOT(I)*EXPAND(I)
       IF (ID(I).EQ.'G') THEN                                   !*!
           SUMLA_GR=SUMLA_GR + LA(I)*EXPAND(I)                  !*!
           SUMRT_GR=SUMRT_GR + ROOT(I)*EXPAND(I)
       ELSE IF (ID(I).EQ.'S') THEN                                ! added 10/00 ajm
           SUMLA_SH = SUMLA_SH + LA(I)*EXPAND(I)
           SUMRT_SH=SUMRT_SH + ROOT(I)*EXPAND(I)
       ELSE IF ((ID(I).EQ.'T') .AND. (H(I).LT.HTLIMIT)) THEN    !*!
           SUMLA_SM=SUMLA_SM + LA(I)*EXPAND(I)                  !*!
           SUMRT_SM=SUMRT_SM + ROOT(I)*EXPAND(I)
       ELSE                                                     !*!  added 12/97
           SUMLA_LG=SUMLA_LG + LA(I)*EXPAND(I)                  !*!
           SUMRT_LG=SUMRT_LG + ROOT(I)*EXPAND(I)
       ENDIF                                                    !*!
   10 CONTINUE
   20 RETURN
      END

      
      
      SUBROUTINE SP2INT
C------------------------------------------------------------
C THIS SUBROUTINE CONVERTS THE CHARACTER VARIABLE, "SPP", TO
C AN INTEGER VARIABLE, "ISP". ALSO, IT CONVERTS THE CHARACTER
C VARIABLE "ID" TO AN INTEGER VARIABLE "IB".
C------------------------------------------------------------

      INCLUDE 'ENTITY.F77'
      print *, 'in sp2int'      
      DO 10 I=1,NB
        IF (SPP(I).EQ.'PP') THEN
          BGCISP(I)=1
        ELSE IF (SPP(I).EQ.'DF') THEN
          BGCISP(I)=2
        ELSE IF (SPP(I).EQ.'WL') THEN
          BGCISP(I)=3
        ELSE IF (SPP(I).EQ.'ES') THEN
          BGCISP(I)=4
        ELSE IF (SPP(I).EQ.'LP') THEN
          BGCISP(I)=5
        ELSE IF (SPP(I).EQ.'AF') THEN
          BGCISP(I)=6
        ELSE IF (SPP(I).EQ.'GF') THEN
          BGCISP(I)=7
        ELSE IF (SPP(I).EQ.'BP') THEN
          BGCISP(I)=8
        ELSE IF (SPP(I).EQ.'WP') THEN
          BGCISP(I)=9
        ELSE IF (SPP(I).EQ.'RC') THEN
          BGCISP(I)=10
        ELSE IF (SPP(I).EQ.'WH') THEN
          BGCISP(I)=11
        ELSE IF (SPP(I).EQ.'SH') THEN   !If BGC gets compiled with variants having shasta red fir
          BGCISP(I)=12                     !this will need to be changed. (Its code is 'SH') ajm 10/00
        ELSE IF (SPP(I).EQ.'GR') THEN
          BGCISP(I)=13
C************************************************
C...The following are temporay "fixes" for CR-variant species.  Adding species
C   Equating white and corkbark fir with grand fir; blue spruce w/ ES
C   mtn hemlock with western hemlock; bristlecone, liber, pinyon, sw white,
C   and juniper  w/ whitebark pine.  The BGCISP codes (in BGC) used only for crown
C   width and CCF calculations.
C   Now, all gymnosperms are included from ALL western variants,
C   EXCEPT: SO, NC, CA, AK, PN, WS, WC
C   (That is, all conifers in the following are accounted for:
C   BM, CI, CR, EC, EM, KT, NI, TT, UT)
C************************************************
        ELSE IF (SPP(I) .EQ. 'BS') THEN
          BGCISP(I)=4
        ELSE IF (SPP(I) .EQ. 'CB') THEN
          BGCISP(I)=7
        ELSE IF (SPP(I) .EQ. 'WF') THEN
          BGCISP(I)=7
        ELSE IF (SPP(I) .EQ. 'MH') THEN
          BGCISP(I)=11
        ELSE IF (SPP(I) .EQ. 'BC') THEN
          BGCISP(I)=8
        ELSE IF (SPP(I) .EQ. 'WB') THEN   !Same as BP, above!
          BGCISP(I)=8
        ELSE IF (SPP(I) .EQ. 'LM') THEN
          BGCISP(I)=8
        ELSE IF (SPP(I) .EQ. 'PI') THEN
          BGCISP(I)=8
        ELSE IF (SPP(I) .EQ. 'JU') THEN
          BGCISP(I)=8
        ELSE IF (SPP(I) .EQ. 'WS') THEN
          BGCISP(I)=4
        ELSE IF (SPP(I) .EQ. 'SF') THEN
          BGCISP(I)=7
        ELSE IF (SPP(I) .EQ. 'L') THEN   ! Same as WL
          BGCISP(I)=3
        ELSE IF (SPP(I) .EQ. 'S') THEN   ! Same as ES
          BGCISP(I)=4
        ELSE IF (SPP(I) .EQ. 'C') THEN   ! Same as RC
          BGCISP(I)=10
C
C************************************************
        ENDIF
C----------
        IF (ID(I).EQ.'T') THEN
          IBLB(I)=1
        ELSE IF (ID(I).EQ.'S') THEN
          IBLB(I)=2
        ELSE IF (ID(I).EQ.'G') THEN
          IBLB(I)=3
        ENDIF
   10 CONTINUE
      RETURN
      END


C     SUBROUTINE SOILH2O ! moved to bgcinit.f
C----------------------------
C  calculates maximum volumetric soil water content from sand, silt
C  and clay fractions.  Maximum volumetric content can also be read
C  in input. Called once for a site.
C    PSAND=percent sand.     S(12) in BIOME-BGC
C    PSILT=percent silt.     S(13) in BIOME-BGC (NOT USED)
C    PCLAY=percent clay.     S(14) in BIOME-BGC
C    VOLMAX=maximum volumetric water content. S(3) in BIOME=BGC
C    AA and BB are texture coefficients
C----------------------------
C      REAL PSAND, PCLAY
C      INCLUDE 'SITE.F77'
C      
C      PSAND=S(12)
C      PCLAY=S(14)
C      VOLMAX=S(3)
C  calculate texture coefficients
C      AA=100.*EXP(-4.396-0.0715*PCLAY-4.880E-4*PSAND**2.
C     &           -4.285E-5*PSAND**2.*PCLAY)
C      BB=-3.14-0.00222*PCLAY**2.-3.484E-5*PSAND**2.*PCLAY
C  calculate maximum volumetric water content if not given in input.
C
C      IF(VOLMAX.EQ.0.0) VOLMAX=EXP((LOG(33.)-AA)/BB)
C      WRITE(*,*) 'WATER: ',AA,BB,VOLMAX
C      RETURN
C      END


C     SUBROUTINE BETAS 
C------------------------------------------------
C This subroutine reads the B() & S() variables
C    The B1() variables are lifeform specific.
C    The B2() variables cover all lifeforms.
C    The S() variables represent site characteristics.
C------------------------------------------------
C
C      INCLUDE 'SITE.F77'
C      OPEN(61,FILE='BETA.DAT',STATUS='OLD')
C      OPEN(62,FILE='SITE.DAT',STATUS='OLD')
C      READ(61,10) ((B1(I,J),J=1,3),I=1,13)
C   10 FORMAT(3(F8.4,1X))
C      READ(61,'(F8.0)') (B2(K),K=1,22)
C      READ(62,'(F8.0)') (S(L),L=1,15)
C 
C     DO 20 K=1,22
C        WRITE(*,100) (B2(K),K=1,22)
C  100   FORMAT (F8.4)   
C  20 CONTINUE
C      RETURN
C     END

      
      SUBROUTINE HABTYPE
C-------------------------------------------------------------------
C   THIS SUBROUTINE SETS HABITAT TYPE AND NATIONAL FOREST LOCATION
C   FLAGS FOR THE PROGNOSIS-BASED HEIGHT GROWTH SUBROUTINE.
C      -- DWC, 1/24/94.
C-------------------------------------------------------------------

      INCLUDE 'SITE.F77'
      print *, 'in habtype'
      IF(HTYPE.EQ.250 .OR. HTYPE.EQ.260 .OR. HTYPE.EQ.280 .OR.
     +   HTYPE.EQ.290 .OR. HTYPE.EQ.310 .OR. HTYPE.EQ.320 .OR. 
     +   HTYPE.EQ.330) THEN 
        IHAB1=1
        IHAB2=3
      ELSE IF(HTYPE.EQ.690 .OR. HTYPE.EQ.710 .OR. 
     +        HTYPE.EQ.720) THEN 
        IHAB1=2
        IHAB2=3
      ELSE IF(HTYPE.EQ.130 .OR. HTYPE.EQ.170 .OR. HTYPE.EQ.660 .OR.
     +        HTYPE.EQ.730 .OR. HTYPE.EQ.830 .OR. HTYPE.EQ.850 .OR. 
     +        HTYPE.EQ.999) THEN 
        IHAB1=3
        IHAB2=3
      ELSE IF(HTYPE.EQ.420 .OR. HTYPE.EQ.470) THEN
        IHAB1=4
        IHAB2=3
      ELSE IF(HTYPE.EQ.510 .OR. HTYPE.EQ.620 .OR. HTYPE.EQ.640 .OR. 
     +        HTYPE.EQ.670 .OR. HTYPE.EQ.680) THEN 
        IHAB1=5
        IHAB2=3
      ELSE IF(HTYPE.EQ.520) THEN
        IHAB1=6
        IHAB2=3
      ELSE IF(HTYPE.EQ.530) THEN
        IHAB1=7
        IHAB2=1
      ELSE IF(HTYPE.EQ.550 .OR. HTYPE.EQ.570 .OR. HTYPE.EQ.610) THEN
        IHAB1=8
        IHAB2=2
      ELSE IF(HTYPE.EQ.540) THEN
        IHAB1=8
        IHAB2=3
      ENDIF

      IF(NFLOC.EQ.5 .OR. NFLOC.EQ.17) THEN 
        ILOC=1 
      ELSE IF(NFLOC.EQ.6 .OR. NFLOC.EQ.18) THEN 
        ILOC=2  
      ELSE
        ILOC=3  
      ENDIF 
      
      RETURN
      END
      
