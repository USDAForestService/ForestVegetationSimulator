      SUBROUTINE BENTYLOAD(FVS_CYC,CV_THIN,FVS_TRN,FVS_DBH,FVS_HT,
     $                    FVS_CR,FVS_PROB,FVS_ISP,FVS_JSP,FVS_IDTREE,
     $                    CV_CLOW,CV_CMED,CV_CTALL,MAXCY1)
C----------
C  **BENTYLOAD   DATE OF LAST REVISION JULY 2003, AJM.  INSERTING NEW METHOD
C FOR CACLULATING BD.  ADDING NEW VARIALBE "A_"; THIS IS THE PARAMETER "a" IN
C THE EQUATION FOR A PARABOLA : X^2 = 4ay.  EACH TREE HAS ITS OWN "a".
C
C              BGC--DATE OF LAST REVISION: 10/16/99
C----------
C
C     LOADS FVS TREE ENTITY ATTRIBUTES INTO BGC ARRAYS. UNDERSTORY VEGETATION
C     IS FROM COVER MODEL OR FROM KEYWORD.
C
C     CALLED FROM: BGCGROW
C
COMMONS
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
COMMONS
      LOGICAL CV_THIN
      INTEGER FVS_ISP(*),FVS_CR(*),FVS_IDTREE(*),FVS_CYC
      INTEGER FVS_TRN,MAXCY1
      CHARACTER *4 FVS_JSP(*), SPEC
      DIMENSION FVS_PROB(*),FVS_DBH(*),FVS_HT(*),CV_THIN(*),
     $          CV_CLOW(MAXCY1,2),CV_CMED(MAXCY1,2),CV_CTALL(MAXCY1,2)


C
C-----------------------------------------------------------------------
C LOAD TREE ARRAYS. CONVERT TO METRIC. 
C-----------------------------------------------------------------------
         DO 50 I=1,FVS_TRN
            ID(I)='T'
C      print *,'in bentyload, I=',I,' ID(I)',ID(I)
            TREENO(I)=FVS_IDTREE(I)
            D(I)=2.54*FVS_DBH(I)
            H(I)=FVS_HT(I)/3.28
            TPH(I)=2.47*FVS_PROB(I)
            EXPAND(I)=TPH(I)
            IK=FVS_ISP(I)
            SPP(I)=FVS_JSP(IK)
            CR(I)=FVS_CR(I)/100.          ! BGC wants decimal value
            SPEC=SPP(I)
            DOB=FVS_DBH(I)
C FOR SMALL TREES WITH HT<1.3M, SET DBH=0
            IF(H(I).LE.1.3) D(I)=0.0
C CALCULATE BD FOR TREES > 2" FVS_DBH BY ADDING 2*BARK THICKNESS
            IF(FVS_DBH(I).GE.2.0) THEN
               CALL BARK(SPEC,DOB,BRK)
               BD(I)=(FVS_DBH(I)+2*BRK)*2.54
            ELSE
C
C***************************************************************************
C COMMENTING OUT THIS METHOD. AJM 7/03
c NEW METHOD FOR CALCULATION OF BASAL DIAMETER (BD)
C 1) ASSUME PARABOLIC SHAPE
C 2) KNOWING HT AND DBH, GET PARAMETER "a" IN EQN FOR PARABOLA X^2 = 4aY
C 3) ONCE "a" IS KNOWN, WE CAN SOLVE FOR ANY X AT ANY Y (A DIAMETER AT ANY HT)
C
C***************************************************************************
C    *****  OLD WAY   *****

C CALCULATE FROM FVS_HT FOR TREES WITH FVS_DBH < 2.0". FIRST GET HEIGHT
C AT REFERENCE DBH FROM H:D CURVE. ASSUME BD FOR REFERENCE DBH IS
C DBH + 2*BRK. STRAIGHT LINE INTERPOLATION.
C               REFD=2.0
C               CALL BARK(SPEC,REFD,BRK)
C               CALL HTDIAM(SPEC,REFD,REFHT)
C               BD(I)=(FVS_HT(I)*(REFD+2*BRK)/REFHT)*2.54
C      print *,'in bentyload, DB(I)=',BD(I),' REFHT=',REFHT,' BRK=',BRK
C      PAUSE
C
C     *****  NEW WAY  *****  (ASSUMES TREE HAS A DBH!)
C
              A_(I)=(FVS_DBH(I)/2.)**2/(4.*(FVS_HT(I)-4.5))
              BD(I)=(4.*SQRT(A_(I)*FVS_HT(I)))*2.54 !METRIC
C     *****  *****  *****  *****
            END IF
C CALCULATE TREE BASAL AREA. USE BD FOR TREES WITH DBH < 5.08cm.
            IF(D(I).LT.5.08) THEN
C               DIB=BD(I)-2*BRK*2.54 ! NOT NEEDED (IN ANY CASE) AJM 7/03
               BA(I)=((BD(I)/1.074)**2.)*0.00007854
            ELSE
               BA(I)=D(I)**2. * 0.00007854
            END IF
C accumulate basal area per hectare
            TOTBA=TOTBA+BA(I)*TPH(I)
   50    CONTINUE
            NB=FVS_TRN
      print *,'in bentyload, fvs_trn=',fvs_trn,'NB=',NB
C
C==========================================================================
C LOAD UNDERSTORY VEGETATION ACCORDING TO OPTION (1=COVER MODEL, 2=KEYWORD)
C==========================================================================
       IF(ISOURCE.EQ.1) THEN 
C      print *,'in bentyload, isource= ', ISOURCE
C------------------------------------------------------------------------
C LOAD SHRUBS BY LAYERS FROM COVER/SHRUB MODEL
C------------------------------------------------------------------------
C SEE IF STAND HAS BEEN THINNED
      ITHN=1
      IF(CV_THIN(FVS_CYC)) ITHN=2
C      print *,'in bentyload, ithn= ', ITHN
C-------------- LOW SHRUBS ----------------------------------------------
      NB=NB+1
C      print *,'in bentyload, nb=',nb
      SPP(NB)='LS'
C      print *,'in bentyload, spp(nb)=',spp(nb)
      ID(NB)='S'
      PCOVER(NB)=CV_CLOW(FVS_CYC,ITHN)
      H(NB)=0.5  !meters
      EXPAND(NB)=1.0
C      print *,'in bentyload, low shrub ht= ', H(NB)
C--------------- MED SHRUBS ----------------------------------------------
      NB=NB+1
C      print *,'in bentyload, nb=',nb
      SPP(NB)='MS'
C      print *,'in bentyload, spp(nb)=',spp(nb)
      ID(NB)='S'
      PCOVER(NB)=CV_CMED(FVS_CYC,ITHN)
      H(NB)=1.6   !meters
      EXPAND(NB)=1.0
C      print *,'in bentyload, med shrub ht= ', H(NB)
C--------------- TALL SHRUBS ----------------------------------------------
      NB=NB+1
C      print *,'in bentyload, nb=',nb
      SPP(NB)='TS'
C      print *,'in bentyload, spp(nb)=',spp(nb)
      ID(NB)='S'
      PCOVER(NB)=CV_CTALL(FVS_CYC,ITHN)
      H(NB)=2.0    !meters
      EXPAND(NB)=1.0
C      print *,'in bentyload, tall shrub ht= ', H(NB)
C      PAUSE
C========================================================================
      ELSE IF(ISOURCE.EQ.2) THEN
C------------------------------------------------------------------------
C LOAD VEG DATA, BY ENTITY, AS SPECIFIED BY USER ON KEYWORD 'UNDERVEG'
C------------------------------------------------------------------------
      DO 60 I=1,NVE
         NB=NB+1
         SPP(NB)=VSP(I)
         ID(NB)=VID(I)
         PCOVER(NB)=VCOV(I)   !%
         H(NB)=VHT(I)/3.28    !meters
         EXPAND(NB)=1.0
   60 CONTINUE
      END IF
      RETURN
      END

