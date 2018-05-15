      SUBROUTINE BALLOCA(I)

C-------------------------------------------------------------------
C   THIS SUBROUTINE CALCULATES THE LEAF, STEM, AND ROOT ALLOCATION  
C   FRACTIONS -- DWC, 8/11/93;12/1/93.  CALLED BY ENTITY
C   NOTE: The fractions are initialized in the RESPIRE subroutine for year 1.
C         The dynamic allocation fraction algorithm has been disabled
C         because it does not perform well; static allocation fractions
C         are being used.
C
C   Changing leaf allocation fractions for grass back to static "defaults";
C   For now, hard-coded at 50% each to leaf and root.
C   ALlocation before this change seemed to dis-favor leaf, hence the grasses 
C   always dying.  AJM 11/03
C   I think there is a conceptual model problem.  If PSN is zero, all three 
C   allocation fractions get set to zero.  This would not be a problem if the
C   allocation fractions were used in this year (of zero PSN).  However, the 
C   fractions calculated here are used NEXT YEAR.  PSN might not be zero.  Yet
C   since the fractions are set here to zero, even if PSN is net positive
C   nedxt year, nothing will get allocated to leaf,stem,root, because in 
C   UPDATE, the PSN is multiplied by allocation fraction to get net increment
C   to pool.
C   As initial temporary fix, am setting allocation fractions to 63%root,&
C   37% leaf if PSN is zero. ajm 1/22/04
C-------------------------------------------------------------------

      REAL SWFINDX1,SWFINDX, NO2INDX, LRATIO, CLIMIT, WLIMIT
      REAL PSN, FINALLC,CLIMIT1
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
      
      
      IB=IBLB(I)
C-----------------------------------------------------------
C CALCULATE ALLOCATION FRACTIONS FOR LEAF, STEM, & ROOTS
C Subtract maintenance respiration from photosynthesis
      PSN=AFPSN(I) - AFMRESP(I)
C Zero-out PSN and growth respiration if PSN <= 0
      IF(PSN.LE.0.0) THEN 
       AFGRESP(I)=0.0
       PSN=0.0
      ENDIF
C Annual soil water fractional average index. SUMWP sums daily soil
C water potential. SUMPSI(I) summed leafwp over layers (in CONDUCTANCE)
C and thus overestimated cummulative stress. SWFINDX1 approaches zero 
C when there are many days of high water stress, and approaches one if
C there are few days with water stress. B2(3) is water potential at 
C stomatal closure. SWFINDX is constrained to be no greater than 0.66.
C     SWFINDX1=1.0 - SUMPSI(I) / (1.01*TLEAFON(I)*B2(3))

C Note! 11/03.  I am not changing this here and now.  But the code below does 
C not work for grass!  SWFINDX1 gets calculated as a negative number--I think
C because SUMWP is derived for ALL days, while denominator is for only those
C days that grass leaf is ON.  For current algorithm to work, SUMWP(1) will
C need to be re-calculated separately for grasses shrubs and trees.  Note that
C this error only presents errors if the SWFINDX that SHOULD be calculated
C is greater than 0.66, because if it is less than 0.66 it gets readjusted TO
C 0.66 further below.  So now, with the error, grasses typically get assigned
C SWFINDX values of 0.66 regardless of the water status, because of this
C mathematical error resulting in negative SWFINDX1 values. ajm
C
      IF (ID(I).EQ.'G') THEN
         SWFINDX1=1.0 - SUMWP(1) / (1.01*TLEAFON(I)*B2(3))	!*!  for grass
C         SWFINDX1=1.0 - SUMWP(1) / (1.01*365*B2(3))	!*!  for grass
      ELSE IF ((ID(I).EQ.'S').OR.                               !added shrubs ajm 10/99
     + ((ID(I).EQ.'T').AND.(H(I).LT.HTLIMIT))) THEN
         SWFINDX1=1.0 - SUMWP(1) / (1.01*TLEAFON(I)*B2(3))	!*!for small trees [and shrub (ajm)]
      ELSE
         SWFINDX1=1.0 - SUMWP(2) / (1.01*TLEAFON(I)*B2(3))	!*!  for large trees
      ENDIF
C
      SWFINDX=MAX(SWFINDX1,0.66)
C Nitrogen availability index
C!!!note: nitrogen use has NOT been incorporated yet.DWC,1/94.
      NO2INDX=0.0
      NO2INDX=MIN(NO2INDX,1.0)
C Leaf/(Leaf+Root) allocation ratio. B2(19) is the maximum ratio(.66) allowed.
C If a year has little water stress, SWFINDX is close to one, and 
C LRATIO approaches B2(19). In dry years, LRATIO will be smaller, thus
C more allocation to roots.
C      LRATIO=(B2(19) / 2.) * (SWFINDX + NO2INDX)
       LRATIO=B2(19) * SWFINDX  !!! nitrogen not operative
C Carbon, nitrogen, and water limits for leaf area.
C======================================================================
C Carbon limit. Calculation based on net PSN minus growth respiration.      
C If all carbon went to either leaves or roots, the maximum that could
C go to leaves would be determined by LRATIO.
      CLIMIT1=PSN * LRATIO*(1-B2(7))
      CLIMIT=MAX(CLIMIT1,0.0)
C======================================================================
C nitrogen limit
C     NLIMIT=1.0
C======================================================================
C water limit, uses Myers' water stress integral. B1(9,IB) is leaf
C turnover %. B2(20) appears to be a fudge factor Hunt threw in - called the 
C water stress integral fraction. B2(7) is the growth respiration 
C fraction. If there is no water stress, SWFINDX is 1, and WLIMIT 
C gives the carbon needed to replace leaf turnover plus growth 
C respiration costs, plus the fudge factor. As water stress increases
C WLIMIT is reduced by SWFINDX. 
      WLIMIT=SWFINDX*((B1(9,IB)/100.)+B2(20)) * LEAF(I)/(1.0-B2(7))
C     WLIMIT=((B1(9,IB)/100.)+B2(20)) * LEAF(I)/(1.0-B2(7))
C======================================================================      
C final leaf carbon limit
C     FINALLC=MIN(CLIMIT,NLIMIT)  !DON'T NEED NEXT 2 LINES UNTIL N IS USED
C     FINALLC=MIN(WLIMIT,FINALLC)
      FINALLC=MIN(CLIMIT,WLIMIT)
C======================================================================
C leaf carbon fraction of total psn
C test new algorithm.  reduce PSN by (1-grwoth respir fraction for leaf)
C in the calculation of LEAFCF.  ajm 10.03
      IF(PSN.GT.0.0) THEN
         LEAFCF(I)=FINALLC/(PSN * (1.0-B2(7)))
C         LEAFCF(I)=FINALLC/PSN ! commented out ajm 10/03)
C        LEAFCF(I)=B2(11)
      ELSE
C        LEAFCF(I)=MIN((LEAF(I)*(B1(9,IB)/100.)) 
C    +                  / (PSN + 0.000001),0.0)
C         LEAFCF(I)=0.0  !! COMMENT OUT 1/04 AJM
         LEAFCF(I)=0.37
C        LEAFCF(I)=B2(11)
      ENDIF
C======================================================================
C Fine root fraction of total psn. The first term in the MIN
C expression solves for what the root allocation fraction would
C have to be for LRATIO to be maintained. The second term in the MIN
C expression assumes stems get nothing. 
      FROOTCF(I)=MIN(LEAFCF(I)*(1./LRATIO-1.),1.-LEAFCF(I))
C
C Calculate based on RLIMIT for root turnover as with WLIMIT for leaves, except
C increase according to SWFINDX. Change=(1+(1-SWFINDX)). 
C      RLIMIT=(2.0-SWFINDX)*(B1(11,IB)/100.)*ROOT(I)/(1.0-B2(10))
C      RFRACT=RLIMIT/PSN
C      FROOTCF(I)=MIN(RFRACT,1.0-LEAFCF(I))      
C      FROOTCF(I)=B2(14)
C=======================================================================
C stem carbon  !!!B2(21) REPLACES 1 IF KEEPING TRACK OF CROOT.
C     X1=B2(21)
      X1=1.0
      IF(PSN.GT.0.0) THEN
        STEMCF(I)=X1 * (1. - LEAFCF(I) - FROOTCF(I))
      ELSE 
        STEMCF(I)=0.0
      ENDIF
C     STEMCF(I)=B2(12)
C=======================================================================
C coarse root carbon
C     CROOTCF= 1. - LEAFCF - FROOTCF - STEMCF
C=======================================================================
C assign stem allocation fraction for grass to be 0.0. MODIFIED 11/03 AJM
      IF(ID(I).EQ.'G') THEN
         LEAFCF(I) = 0.5
         FROOTCF(I)=1-LEAFCF(I)
         STEMCF(I)=0.0
C        LEAFCF(I)=B2(11)
C        FROOTCF(I)=B2(14)
C        STEMCF(I)=B2(12)
      ENDIF
C original below: ADDED ABOVE 11/03 AJM**************************************
C      IF(ID(I).EQ.'G') THEN
C         FROOTCF(I)=1-LEAFCF(I)
C         STEMCF(I)=0.0
CC        LEAFCF(I)=B2(11)
CC        FROOTCF(I)=B2(14)
CC        STEMCF(I)=B2(12)
C      ENDIF
C***************************************************************************
C Commenting out the writing of "FRACTIONS.out" for release version. ajm 11/02
C	  WRITE(75,*) 'LIMITS',I,PSN,CLIMIT,WLIMIT,FINALLC,SWFINDX
       WRITE(75,100)I,ID(I),SWFINDX1,SWFINDX,CLIMIT1,CLIMIT,WLIMIT,
     &              FINALLC,LEAFCF(I),STEMCF(I),FROOTCF(I),PSN
  100  FORMAT(I3, A2, 6(1X,F8.4),3(1X,F6.4),1X,F7.3)
C       WRITE(*,*) 'WATER STRESS',I,SUMWP,TLEAFON(I),SWFINDX1
C       WRITE(*,*) 'WLIMIT',I,SWFINDX,LEAF(I),B1(9,IB)
      RETURN
      END

