      SUBROUTINE GRINCR (DEBUG,IPMODI,LTMGO,LMPBGO,LDFBGO,
     1     LBWEGO,LCVATV,LBGCGO)

      IMPLICIT NONE
C----------
C     **GRINCR--BASE  DATE OF LAST REVISION: 03/05/10
C----------
C     
C     COMPUTES GROWTH AND MORTALITY ON EACH TREE RECORD.
C----------
C  SECTIONS THAT ARE SPECIFIC TO THE ORGANON MODEL
C  ARE DENOTED/DELINITATED WITH THE "ORGANON" 
C  COMMENT BLOCK. 
C  DEVEOPMENT NOTES FOR FUTURE EXAMINATION ARE
C  DENOTED WITH A TODO: TAG
C----------
C     
C     CALLED FROM: PPMAIN AND TREGRO.
C     
COMMONS
C     

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PDEN.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'STDSTK.F77'
      INCLUDE 'ESHAP.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'ORGANON.F77'

COMMONS
C     

      REAL PRM(6)
      INTEGER MYACTS(3)
      INTEGER IPMODI,NTODO,ITODO,NP,IACTK,IDATE,I,IS,IGRP,IULIM,IG,IGSP
      INTEGER IPH,IC,ICDF,IBDF,ISPCC,ITFN,J,JJ,K
      REAL STAGEA,STAGEB
      LOGICAL LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,LBGCGO,DEBUG,LINCL
      INTEGER IBA,ISTOPRES,IRTNCD
      CHARACTER*10 KARD

      INTEGER*4 ORG_VERSION

      logical status
      LOGICAL ALLOW_THINNING_OPTION

      DATA MYACTS/98,99,120/
      DATA KARD/'          '/
C     

C----------
C     ORGANON: ADDED THE ORGANON HEADER FILE TO INCLUDE GLOBAL VARIABLES
C     THAT MIGHT BE REQUIRED BY ORGANON DLL FUNCTION CALLS.
C     DEFINITION TO THE ORGANON EXECUTE FUNCTION FROM DOCS.
C	POINTER (QORG,EXECUTE) 
C	THIS IS THE BEST METHOD TO INTEGRATE THE ORGANON DLL.
C	USING THE DLL_IMPORT METHOD DOESN'T ALLOW COMPILER INDEPENDENCE,
C	DECLARE AN INTERFACE BLOCK FOR THE EXECUTE ORGANON SUBROUTINE
C	MATCH THE ARGUMENTS EXACTLY
C----------

c$$$	INTERFACE
c$$$      SUBROUTINE EXECUTE(CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO,
c$$$     1                   PTNO,SPECIES,USER,INDS,DBH1,HT1,CR1,SCR1,
c$$$     2                   EXPAN1,MGEXP,RVARS,ACALIB,PN,YSF,BABT,BART,YST,
c$$$     3                   NPR,PRAGE,PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,
c$$$     4                   BRHT,BRDIA,JCORE,SERROR,TERROR,SWARNING,
c$$$     5                   TWARNING,IERROR,DGRO,HGRO,CRCHNG,SCRCHNG,
c$$$     6                   MORTEXP,NTREES2,DBH2,HT2,CR2,SCR2,EXPAN2,STOR)
c$$$      !DEC$ ATTRIBUTES DLLIMPORT:: EXECUTE
c$$$	integer*4	CYCLG
c$$$	integer*4	VERSION
c$$$	integer*4	NPTS
c$$$	integer*4	NTREES1
c$$$	integer*4	STAGE
c$$$	integer*4	BHAGE
c$$$	integer*4	TREENO(2000)
c$$$	integer*4	PTNO(2000)
c$$$	integer*4	SPECIES(2000)
c$$$	integer*4	USER(2000)
c$$$	integer*4	INDS(30)
c$$$	real*4		DBH1(2000)
c$$$	real*4		HT1(2000)
c$$$	real*4		CR1(2000)
c$$$	real*4		SCR1(2000)
c$$$	real*4		EXPAN1(2000)
c$$$	real*4		MGEXP(2000)
c$$$	real*4		RVARS(30)
c$$$	real*4		ACALIB(3,13)
c$$$	real*4		PN(5)
c$$$	real*4		YSF(5) ! years since fertilization
c$$$	real*4		BABT(5)
c$$$	real*4		BART(5)
c$$$	real*4		YST(5)
c$$$	integer*4	NPR(2000)
c$$$	integer*4	PRAGE(3,2000)
c$$$	real*4		PRLH(3,2000)
c$$$	real*4		PRDBH(3,2000)
c$$$	real*4		PRHT(3,2000)
c$$$	real*4		PRCR(3,2000)
c$$$	real*4		PREXP(3,2000)
c$$$	integer*4	BRCNT(3,2000)
c$$$	integer*4	BRHT(40,2000)
c$$$	integer*4	BRDIA(40,2000)
c$$$	integer*4	JCORE(40,2000)
c$$$	integer*4	SERROR(35)
c$$$	integer*4	TERROR(6,2000)
c$$$	integer*4	SWARNING(9)
c$$$	integer*4	TWARNING(2000)
c$$$	integer*4	IERROR
c$$$	real*4		DGRO(2000)
c$$$	real*4		HGRO(2000)
c$$$	real*4		CRCHNG(2000)
c$$$	real*4		SCRCHNG(2000)
c$$$	real*4		MORTEXP(2000)
c$$$	integer*4	NTREES2
c$$$	real*4		DBH2(2000)
c$$$	real*4		HT2(2000)
c$$$	real*4		CR2(2000)
c$$$	real*4		SCR2(2000)
c$$$	real*4		EXPAN2(2000)
c$$$	real*4		STOR(30)
c$$$	end subroutine
c$$$	end interface

C     
      IF (DEBUG) WRITE (JOSTND,2) ICYC,NPLT
    2 FORMAT (/' IN GRINCR, ICYC=',I3,'; NPLT=',A26)

C     
C     SET THE CYCLE LENGTH AND OPTION POINTERS FOR THE CYCLE.
C     
      IF(ICYC .GE. 2) THEN
         OLDFNT = IY(ICYC)-IY(ICYC-1)
      ELSE
         OLDFNT = FINT
      ENDIF
      IFINT=IY(ICYC+1)-IY(ICYC)
      FINT=IFINT

C     
C     SET UP THE OPTION POINTERS FOR THE CYCLE.
C     
      CALL OPCSET (ICYC)
C     
C     SET CYCLE DEPENDENT SWITCHES
C     
      LTRIP=(ICYC.LE.ICL4.AND.ITRN.LE.(MAXTRE/3).AND..NOT.NOTRIP)
C
C     GET THE RESTART CODE AND BRANCH ACCORDINGLY
C
      CALL fvsGetRestartCode (ISTOPRES)
      IF (DEBUG) WRITE (JOSTND,5) ICYC,ISTOPRES,NPLT
    5 FORMAT (/' IN GRINCR, ICYC=',I3,'; ISTOPRES=',I3,'; NPLT=',A)
      GOTO (1,16,17,71,72), ISTOPRES+1

C     STOP HAS NOT OCCURED

    1 CONTINUE
C----------
C     PROCESS THE SETSITE OPTIONS.
C----------
      CALL OPFIND (1,MYACTS(3),NTODO)
      IF(NTODO.GT.0) THEN
         DO 9 ITODO=1,NTODO
            CALL OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
            IF (IACTK.LT.0) GOTO 9
            CALL OPDONE(ITODO,IY(ICYC))
C     
            IF(PRM(1) .GT. 0.)THEN
               KODTYP=IFIX(PRM(1))
               KARD='         '
               CPVREF= '          '
               CALL HABTYP (KARD,PRM(1))
               IF(KODTYP .GT. 0)ICL5=KODTYP
               DO 4 I=1,MAXPLT
                  IPHAB(I)=ITYPE
 4             CONTINUE
          IF(DEBUG)WRITE(JOSTND,*)' IN GRINCR PROCESSING SETSITE HABITA'
     &              ,'T TYPE: KODTYP,ICL5,ITYPE= ',KODTYP,ICL5,ITYPE
            ENDIF
C     
            IF(PRM(2) .GT. 0)THEN
               BAMAX=PRM(2)
               LBAMAX=.TRUE.
               IF(DEBUG)WRITE(JOSTND,*)' IN GRINCR PROCESSING SETSITE',
     &              ' BASAL AREA MAXIMUM = ',BAMAX,'  LBAMAX = ',LBAMAX     
            ENDIF
C     
            IS=IFIX(PRM(3))
C----------
C     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP
C     IF SITE SPECIES HAS NOT BEEN SET, SET IT TO THE FIRST SPECIES
C     IN THE GROUP.
C----------
            IF(IS .LT. 0) THEN
               IGRP = -IS
               IULIM = ISPGRP(IGRP,1)+1
               IF (ISISP .EQ. 0) ISISP=ISPGRP(IGRP,2)
               LSITE=.TRUE.
               DO 6 IG=2,IULIM
                  IGSP = ISPGRP(IGRP,IG)
                  IF(PRM(5) .EQ. 0)THEN
                     IF(PRM(4).GT.0.)SITEAR(IGSP)=PRM(4)
                  ELSE
               IF(PRM(4).NE.0.)SITEAR(IGSP)=SITEAR(IGSP)+SITEAR(IGSP)*
     &                    PRM(4)/100.
                  ENDIF
                  IF(SITEAR(IGSP).LT.1.)SITEAR(IGSP)=1.
                  IF(PRM(6) .GT. 0.) THEN
                     SDIDEF(IGSP)=PRM(6)
                     MAXSDI(IGSP)=1
                  ENDIF
 6             CONTINUE
C----------
C     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:
C----------
            ELSEIF(IS .EQ. 0)THEN
               DO 7 I=1,MAXSP
                  IF(PRM(5) .EQ. 0)THEN
                     IF(PRM(4).GT.0.)SITEAR(I)=PRM(4)
                  ELSE
              IF(PRM(4).NE.0.)SITEAR(I)=SITEAR(I)+SITEAR(I)*PRM(4)/100.
                  ENDIF
                  IF(SITEAR(I).LT.1.)SITEAR(I)=1.
                  IF(PRM(6) .GT. 0.) THEN
                     SDIDEF(I)=PRM(6)
                     MAXSDI(I)=1
                  ENDIF
 7             CONTINUE
C----------
C     SPECIES CODE IS SPECIFIED (IS>0).
C----------
            ELSE
               IF(PRM(5) .EQ. 0)THEN
                  IF(PRM(4).GT.0.)SITEAR(IS)=PRM(4)
               ELSE
            IF(PRM(4).NE.0.)SITEAR(IS)=SITEAR(IS)+SITEAR(IS)*PRM(4)/100.
               ENDIF
               IF(SITEAR(IS).LT.1.)SITEAR(IS)=1.
               IF(PRM(6) .GT. 0.) THEN
                  SDIDEF(IS)=PRM(6)
                  MAXSDI(IS)=1
               ENDIF
            ENDIF
C     
            IF(DEBUG .AND. (PRM(4).GT.0. .OR. PRM(6).GT.0.))THEN
            WRITE(JOSTND,*)' IN GRINCR PROCESSING SETSITE SITE INDEX ',
     &              'AND/OR SDIMAX:'
C     
               DO 992 I=1,15
                  J=(I-1)*10 + 1
                  JJ=J+9
                  IF(JJ.GT.MAXSP)JJ=MAXSP
                  WRITE(JOSTND,990)(NSP(K,1)(1:2),K=J,JJ)
 990              FORMAT(/'    SPECIES ',5X,10(A2,6X))
                  WRITE(JOSTND,994)(SITEAR(K),K=J,JJ )
 994              FORMAT(' SITE INDEX ',   10F8.0)
                  WRITE(JOSTND,991)(SDIDEF(K),K=J,JJ )
 991              FORMAT('    SDI MAX ',   10F8.0)
                  IF(JJ .EQ. MAXSP)GO TO 993
 992           CONTINUE
 993           CONTINUE
            ENDIF
 9       CONTINUE
         IF(ISISP.GT.0 .AND. ISISP.LE.MAXSP)THEN
            STNDSI=SITEAR(ISISP)
         ELSE
            STNDSI=0.
         ENDIF
         CALL RCON
      ENDIF
C     
C     WESTERN ROOT DISEASE VER. 3.0 MODEL PROJECTION SETUP.
C     
      CALL RDMN2 (OLDFNT)
      CALL RDTRP (LTRIP)
C     
C     FIRE MODEL PROJECTION SETUP
C     
      CALL FMSDIT
C     
C     IF THE PROGRAM IS LINKED TO PPE, AND IF PROCESSING A BRANCH
C     CREATED BY A PHASE 2 EVENT, THEN: SKIP CUTS AND BUG MODEL CALLS.
C     THE EXPPE ROUTINE RETURNS IPH=0
C     
      CALL PPBRPH(IPH)
      IF (DEBUG) WRITE (JOSTND,'('' IN GRINCR, IPH='',I3)') IPH
C     
      IF (IPH.EQ.2) GOTO 120
C     
C     CALL SILFTY (ENTRY IN SDICAL) TO COMPUTE SILVAH FOREST TYPE
C     BEFORE THINNING
C     
      CALL SILFTY
C     
      IF (DEBUG) WRITE(JOSTND,10)BTSDIX,SDIBC,SDIBC2,ICYC
 10   FORMAT(' BEFORE SDICAL, BTSDIX, SDIBC, SDIBC2, CYCLE=',
     &     F9.1,2F9.1,I2)
C     
C     CALL SDICAL TO GET CYCLE 0 BEFORE CUT SDI MAX.
C     
      CALL SDICAL(BTSDIX)
      CALL SDICLS(0,0.,999.,1,SDIBC,SDIBC2,STAGEA,STAGEB,0)
C     
      IF (DEBUG) WRITE(JOSTND,15) BTSDIX, SDIBC, SDIBC2, ICYC
 15   FORMAT(' AFTER SDICAL, BTSDIX, SDIBC, SDIBC2, CYCLE=',
     &     F9.1,2F9.1,I2) 
C     
      IBA = 1
      CALL SSTAGE (IBA,ICYC,.FALSE.)
C
C     IS THIS A STOPPING POINT?
C
      CALL fvsStopPoint (1,ISTOPRES)
      IF (ISTOPRES.NE.0) RETURN
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
   16 CONTINUE
C
C     IF PROCESSING A PHASE 1 SKIP CALL TO EVENT MONITOR.
C     
      IF (IPH.NE.1) CALL EVMON (1,1)
C
C     IS THIS A STOPPING POINT?
C
      CALL fvsStopPoint (2,ISTOPRES)
      IF (ISTOPRES.NE.0) RETURN
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
   17 CONTINUE
C
C     EVALUATE ECON EXTENSTION STATUS
C     0 MEANS THIS CALL TO ECSTATUS.F IS BEFORE THE CALL TO CUTS.F
C     
      CALL ECSTATUS(ICYC, NCYC, IY, 0)
C     
C     PRINT THE VISULIZATION FOR BEGINING CYCLE. SKIP IN FIRST CYCLE.
C     
      IF (ICYC.GT.1) CALL SVOUT(IY(ICYC),1,'Beginning of cycle')
C     
C     SAVE LAST CYCLE DENSITY STATS.
C     
      OLDTPA=TPROB
      OLDAVH=AVH
      OLDBA=BA
      RELDM1=RELDEN
      ORMSQD=RMSQD
C     
C     CALL **CUTS** TO INVOKE THINNING.
C     
      IF (DEBUG) WRITE(JOSTND,20) ICYC
 20   FORMAT(' CALLING CUTS, CYCLE=',I2)
      CALL CUTS

C     
C     STORE THE REMOVED TPA IN WK4 FOR USE IN THE SECOND CALL TO THE
C     EVENT MONITOR (SPMCDBH INVOLVING CUT TREES).
C     
      DO 21 IC=1,MAXTRE
         WK4(IC)=WK3(IC)
 21   CONTINUE
C     
C     CALL **CVGO** TO DETERMINE IF THE COVER EXTENSION IS ACTIVE.
C     
      CALL CVGO (LCVATV)
C     
C     CALL **DENSE** TO UPDATE STAND DENSITY STATISTICS IF THINNING
C     OCCURRED.
C     
      IF (ONTREM(7).GT.0.0) THEN
         IF (DEBUG) WRITE(JOSTND,40) ICYC
 40      FORMAT (' CALLING DENSE (POST THIN), CYCLE=',I3)
         CALL DENSE
      ENDIF

C     THESE SEEM TO WRITE OVER THE EXISTING VARIABLES, 
C     BUT I NEED TO CONFIRM THESE ARE CORRECT.

C     
C     SAVE POST-THIN DENSITY.
C     
      ATAVD=RMSQD
      ATAVH=AVH
      ATBA=BA
      ATCCF=RELDEN
      ATTPA=TPROB
      CALL SDICAL(ATSDIX)
      CALL SDICLS(0,0.,999.,1,SDIAC,SDIAC2,STAGEA,STAGEB,0)

C     
C     CALL SILFTY (ENTRY IN SDICAL) TO COMPUTE SILVAH FOREST TYPE
C     AFTER THINNING
C     
      CALL SILFTY

C     
C     CALL CVBROW & CVCNOP TO COMPUTE POST-THIN STAND SHRUB AND COVER
C     STATISTICS.
C     
      IF (ONTREM(7).GT.0.0 .AND. LCVATV) THEN
         IF (DEBUG) WRITE(JOSTND,60) ICYC
 60      FORMAT (' CALLING CVBROW, CYCLE =',I2)
         CALL CVBROW (.TRUE.)
         IF (DEBUG) WRITE(JOSTND,70) ICYC
 70      FORMAT (' CALLING CVCNOP, CYCLE =',I2)
         CALL CVCNOP (.TRUE.)
      ENDIF
C     
C     COMPUTE THE STRUCTURAL STAGE OF THE STAND AFTER TO CUTS.
C     
      IBA = 2
      CALL SSTAGE (IBA,ICYC,.FALSE.)
C
C     IS THIS A STOPPING POINT?
C
      CALL fvsStopPoint (3,ISTOPRES)
      IF (ISTOPRES.NE.0) RETURN
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
   71 CONTINUE
C
C     CALL EVENT MONITOR FOR PHASE II CHECKING.
C     
      CALL EVMON (2,1)
C
C     IS THIS A STOPPING POINT?
C
      CALL fvsStopPoint (4,ISTOPRES)
      IF (ISTOPRES.NE.0) RETURN
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
   72 CONTINUE
C
C     EVALUATE ECON EXTENSTION STATUS
C     1 MEANS THIS CALL TO ECSTATUS.F IS AFTER THE CALL TO CUTS.F 
C     
      CALL ECSTATUS(ICYC, NCYC, IY, 1)
C----------
C     SET PAST VOLUME VARIABLES FOR USE NEXT CYCLE
C----------
      DO 215 I=1,ITRN
         PTOCFV(I)=CFV(I)
         PMRCFV(I)=WK1(I)
         PMRBFV(I)=BFV(I)
         PDBH(I)=DBH(I)
         PHT(I)=HT(I)
         ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
         IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
         NCFDEF(I)=ICDF
         NBFDEF(I)=IBDF
 215  CONTINUE

C     
C     CALL COMCUP TO COMPRESS THE TREE LIST.
C     
      IF (DEBUG) WRITE (JOSTND,30) ICYC
 30   FORMAT (' CALLING COMCUP, CYCLE=',I3)
      CALL COMCUP
C     IS THIS THEN ACTIVE?

C     
C     IF IPMODI IS EQUAL TO 2, SKIP THESE BUG MODELS
C     
      IF (IPMODI.EQ.2) GOTO 120
C     
C     CALL DFTMGO, MPBGO, AND DFBGO, TO DETERMINE IF INSECT
C     OUTBREAK WILL OCCUR THIS CYCLE.
C     
      IF(DEBUG) WRITE(JOSTND,80) ICYC
 80   FORMAT(' CALLING DFTMGO, CYCLE=',I2)
      CALL DFTMGO(LTMGO)
      IF(DEBUG) WRITE(JOSTND,90) ICYC
 90   FORMAT(' CALLING MPBGO, CYCLE=',I2)
      CALL MPBGO(LMPBGO)
      IF (DEBUG) WRITE(JOSTND,95) ICYC
 95   FORMAT(' CALLING DFBGO, CYCLE=',I2)
      CALL DFBGO(LDFBGO)
C     
C     CALL BGCGO TO SEE IF STAND-BGC IS ACTIVE
C     
      CALL BGCGO(LBGCGO)
C----------
C     ADAPTATION FOR BUDLITE MODEL
C----------
      IF(DEBUG) WRITE(JOSTND,105) ICYC
 105  FORMAT(' CALLING BWEGO, CYCLE=',I2)
      CALL BWEGO(LBWEGO)
C     
C     BRANCH TO STATEMENT 40 IF NO TUSSOCK MOTH OUTBREAK THIS CYCLE.
C     
      IF (LTMGO) THEN
C     
C     THERE IS A TUSSOCK MOTH OUTBREAK; COMPUTE BIOMASS.
C     
         IF (DEBUG) WRITE(JOSTND,110) ICYC
 110     FORMAT(' CALLING TMBMAS, CYCLE=',I2)
         CALL TMBMAS
      ENDIF
C     
C     BRANCH TO THIS LOCATION IF PROCESSING A BRANCH WHICH WAS CREATED
C     BY A PHASE II EVENT OR IF IPMODI IS EQUAL TO 2 (PPE CODE).
C     
 120  CONTINUE

C----------
C     ORGANON 
C----------

      IF (DEBUG) WRITE(JOSTND,121) ICYC
 121  FORMAT(' LOADING ORGANON.DLL, CYCLE=',I2 )

C     MOVE THE ORGANON STAND LEVEL DATA INTO THE APPROPRIATE PLACE
C     IN THE FVS STAND LEVEL DATA. 
      
C     THE NUMBER OF CYCLES GROWN IN ORGANON = FVS CYCLES - 1
      CYCLG = ICYC - 1	

C     CONVERT THE FVS_STANDINIT!MODEL_TYPE TO ORGANON_VERSION
C     WHEN IT'S READ IN, IT'S ASSIGNED TO THE IMODTY VARIABLE.
C      VERSION=IMODTY

C      NPTS=IPTINV               ! THE NUMBER OF POINTS IN THE SAMPLE
C     SINCE FVS ALREADY PROCESSES THE TREE LIST BY DIVIDING THE NUMBER
C     OF POINTS, AND ORGANON WILL ALSO CALL EDIT AS PART OF IT'S PROCESSING
C     THERE IS NO NEED TO DIVIDE BY THE NUMBER OF POINTS AGAIN
      NPTS=1               ! THE NUMBER OF POINTS IN THE SAMPLE


      NTREES1=ITRN              ! THE TOTAL NUMBER OF STEMS IN THE TREE LIST

      
C     SET USING KEYWORD FILE VIA SUPPOSE FOR EACH STAND
C     INDS(4)		= 1 ! 1=STAND IS EVEN AGED, 0=UNEVEN AGED (INTIIALIZED IN KEYWORD FILE)
C     IAGE -- ORIGINAL STAND AGE -- SHOULD NEVER BE UPDATED.
C     ICAGE -- ORIGINAL STAND AGE COMPUTED FROM SIZE CLASS & TREE HT - CHANGES

C     Total stand age (even aged only)
      IF( INDS(4) .EQ. 1 ) THEN
        STAGE     = IAGE + ( CYCLG * 5 )         
        BHAGE     = STAGE - 5   ! breast height age
      ELSE
        STAGE     = 0
        BHAGE     = 0           ! breast height age
      ENDIF

      INDS(5)		= 0     ! TRIPPLING / LTRIP ! FVS VERSION OF TRIPPLING (NOTRIPLE)
      INDS(6)		= 0     ! STAND HAS BEEN PRUNED
C$$$      INDS(7)		= 0     ! STAND HAS BEEN THINNED
C$$$      INDS(8)		= 0     ! STAND HAS BEEN FERTILIZED      
C     SET USING KEYWORD FILE VIA SUPPOSE FOR EACH STAND
C     INDS(9)		= 1 ! USE LIMIT OF MAX SDI - SET ALREADY      
      INDS(10)	= 0             ! WOOD QUALITY VARIABLES ARE BEING COMPUTED.
      INDS(11)	= 0             ! INGROWTH WAS ADDED AT START OF GROWTH CYCLE
      INDS(12)	= 0             ! OVERSTORY TREES WERE REMOVED AT START OF CURRENT CYCLE
      INDS(13)	= 0             ! MAJOR CONIFER TREES WERE CUT AT BEINNING OF CYCLE
      INDS(14)	= 0             ! PLANTED WITH GENETIC IMPROVED STOCK
      INDS(15)	= 0             ! IS INFECTED WITH SWISS NEEDLE CAST
      
C     ASSIGN THE SPECIES SPECIFIC VALUES FOR THE STAND LEVEL VARIABLES
C     SO THAT ORGANON WILL IMPUTE THE VALUES WHEN IT CALLS PREPARE 
      



C     THIS IS A TEMP DEBUGGING HACK TO MAKE SURE THE SITE INDEX VALUES ARE THE SAME GOING INTO THE DLL
C      SITEAR( 16 ) = 129.0   ! THE DOUGLAS-FIR SITE INDEX
C      SITEAR( 15 ) = 125.0   ! THE PP SITE INDEX


C     ORGANON VARIANT: SWO
      IF( IMODTY .EQ. 1 ) THEN
         RVARS(1)    = SITEAR( 16 ) ! THE DOUGLAS-FIR SITE INDEX
         RVARS(2)    = SITEAR( 15 ) ! THE PP SITE INDEX

         RVARS(3)    = SDIDEF( 16 ) ! MAX SDI FOR DF IN SWO, NWO, AND SMC
         RVARS(4)    = SDIDEF(  2 ) ! MAX SDI FOR WF/GF IN SWO (WF), NWO/SMC (GF).
         RVARS(5)    = SDIDEF( 15 ) ! MAX SDI FOR PP/WH IN SWO (PP), NWO/SMC (WH).
      END IF
      
C     ORGANON VARIANT: NWO
      IF( IMODTY .EQ. 2 ) THEN
         RVARS(1)    = SITEAR( 16 ) ! THE DOUGLAS-FIR SITE INDEX
         RVARS(2)    = SITEAR( 19 ) ! WESTERN HEMLOCK SITE INDEX

         RVARS(3)    = SDIDEF( 16 ) ! MAX SDI FOR DF IN SWO, NWO, AND SMC
         RVARS(4)    = SDIDEF(  3 ) ! MAX SDI FOR WF/GF IN SWO (WF), NWO/SMC (GF).
         RVARS(5)    = SDIDEF( 19 ) ! MAX SDI FOR PP/WH IN SWO (PP), NWO/SMC (WH).
      END IF
      
C     ORGANON VARIANT: SMC
      IF( IMODTY .EQ. 3 ) THEN
         RVARS(1)    = SITEAR( 16 ) ! THE DOUGLAS-FIR SITE INDEX
         RVARS(2)    = SITEAR( 19 ) ! THE WESTERN HEMLOCK SITE INDEX

         RVARS(3)    = SDIDEF( 16 ) ! MAX SDI FOR DF IN SWO, NWO, AND SMC
         RVARS(4)    = SDIDEF(  3 ) ! MAX SDI FOR WF/GF IN SWO (WF), NWO/SMC (GF).
         RVARS(5)    = SDIDEF( 19 ) ! MAX SDI FOR PP/WH IN SWO (PP), NWO/SMC (WH).
      END IF

C     TODO: HOLD OFF CONVERTING THESE UNTIL FIRST CODE REVIEW.
!RVARS(6)	= DF GENETIC WORTH VALUE FOR DIAMETER GROWTH
!RVARS(7)	= DF GENETIC WORTH VALUE FOR HEIGHT GROWTH

C     TODO: ASSIGN THE SWISS NEEDLE CAST PARAMETERS
!RVARS(8)	= N YRS FOLIAGE RETENTION FOR DF (SNC)
!RVARS(9)	= PLANTING DENSITY OF ALDER RAP VERSION

C     
!RVARS(10-30)	= UNUSED. 
      
C-----------
C     MOVE THE DATA FROM THE EXISTING FVS TREE ARRAYS 
C     INTO THE FORMAT THAT ORGANON REQUIRES
C     ASSIGNMENT OF FVS-VARIABLES TO ORGANON-VARIABLES
C-----------
      DO I = 1, ITRN         
         TREENO(I)	= I     ! TREE NUMBER (ON PLOT?)
         PTNO(I)	= ITRE(I) ! POINT NUMBER OF THE TREE RECORD
         READ( FIAJSP( ISP( I ) ), '(I4)'  ) SPECIES( I )
         DBH1(I)	= DBH(I) ! DBH 
         HT1OR(I)	= HT(I) ! TOTAL HEIGHT AT BEGINNING OF PERIOD
         CR1(I)		= REAL(ICR(I)) / 100.0 ! MEASURED/PREDICTED CROWN RATIO
         SCR1B(I)	= 0.0   ! SHADOW CROWN RATIO
         EXPAN1(I)	= PROB(I) ! EXPANSION FACTOR
         MGEXP(I)	= 0.0   ! MANAGEMENT EXPANSION FACTOR - REMOVED.
         USER(I)	= ISPECL(I) ! USER CODE AT BEGINNING OF PERIOD
         WK2(I)  = 0.0   ! CLEAR OUT THE WK2 ARRAY FOR THIS CYCLE
                         ! IT WILL BE ASSIGNED THE MORTALITY          


         CR2(I) = 0.0       ! RESET THE ENDING CROWN RATIO
         DG(I)  =   0.0     ! ASSIGN THE DIAMETER GROWTH
         HTG(I) =   0.0     ! ASSIGN THE HEIGHT GROWTH
         ICR(I) =   0 ! INTEGER - CROWN RATIO

      ENDDO
      
C-----------
C     INITIALIZE THE WARNING AND ERROR VARIABLES
C     BEFORE THE CALL THE EXECUTE FUNCTION 
C     IN THE ORGANON DLL
C-----------
      IERROR = 0
      
      DO I = 1, 35
         SERROR(I) = 0
      end do
      
      DO I = 1, 9
         SWARNING(I) = 0
      end do
      
      DO I = 1, 2000
         TWARNING(I) = 0
         DO J = 1, 6
            TERROR(I,J) = 0
         end do
      end do

C-----------
C  MANAGE THE THINNING VARIABLES
C-----------
      IF( OLDBA .NE. BA ) THEN

C     MOVE THE VARIABLES FROM THE PREVIOUS THINNINGS, IF THERE ARE ANY
        DO I=5,2,-1
          YST(I)  = YST(I-1)
          BART(I) = BART(I-1)
	  END DO

C     THE YST VECTOR CONTAINS AN ARRAY OF LENGTH FIVE 
C     FOR THE NUMBER OF YEARS THAT HAVE PASSED SINCE 
C     THE LAST THINNING. SINCE THIS HAPPENS BEFORE THE 
C     CURRENT GROWTH CYCLE, THE OLDBA IS THE BASAL AREA
C     BEFORE THINNING.
      YST(1)  = FLOAT( ( CYCLG - 1 ) * 5 )
      BART(1) = OLDBA - ATBA
      BABT    = OLDBA
      
C     SET THE "STAND HAS BEEN THINNED" VARIABLE TO TRUE.
	INDS(7)		= 1 ! stand has been thinned

      END IF

	IF( INDS(7) .EQ. 1 ) THEN

C     WRITE OUT THE VARIABLES TO THE DEBUG/OUTPUT FILE FOR REVIEW LATER.
        IF (DEBUG) THEN
          
          WRITE(JOSTND,123) ICYC, BABT
  123     FORMAT(' STAND HAS BEEN THINNED BEFORE CALLING ',
     &    ' ORGANON.DLL, CYCLE=',I2, ' BABT= ', F9.3 )

C     WRITE OUT THE YEARS SINCE THINNING ARRAY
          WRITE(JOSTND,1240) ICYC,
     &      YST(1), 
     &      YST(2), 
     &      YST(3), 
     &      YST(4), 
     &      YST(5) 
 1240     FORMAT(
     &    ' ORGANON.DLL, CYCLE=',I2, 
     &    ', YST(1)=', F6.2,
     &    ', YST(2)=', F6.2,
     &    ', YST(3)=', F6.2,
     &    ', YST(4)=', F6.2,
     &    ', YST(5)=', F6.2 )

C     WRITE OUT THE THINNING BASAL AREA REMOVED AT THINNING ARRAY
          WRITE(JOSTND,1250) ICYC,
     &      BART(1), 
     &      BART(2), 
     &      BART(3), 
     &      BART(4), 
     &      BART(5) 
 1250     FORMAT(
     &    ' ORGANON.DLL, CYCLE=',I2, 
     &    ', BART(1)=', F6.2,
     &    ', BART(2)=', F6.2,
     &    ', BART(3)=', F6.2,
     &    ', BART(4)=', F6.2,
     &    ', BART(5)=', F6.2 )
     
        END IF

	END IF

C      IF (DEBUG) WRITE(JOSTND,124) ICYC, VERSION
      IF (DEBUG) WRITE(JOSTND,124) ICYC, IMODTY
 124  FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &     ', ORGANON::VARIANT= ',I2 )

      ORG_VERSION = IMODTY      ! MODEL_TYPE == ORGANON VERSION

C     FOR SOME REASON, LF95 DOESN'T REQUIRE EXPLICIT LOADING 
C     AND ASSIGNING OF THE POINTER TO THE FUNCTION/SUBROUTINE.
C     WHICH IS REALLY STRANGE, SINCE YOU HAVE TO BUILD THE DLL YOURSELF
      call EXECUTE( CYCLG,ORG_VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO,
     1     PTNO,SPECIES,USER,INDS,DBH1,HT1OR,CR1,SCR1B,
     2     EXPAN1,MGEXP,RVARS,ACALIB,PN,YSF,BABT,BART,YST,
     3     NPR,PRAGE,PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,
     4     BRHT,BRDIA,JCORE,SERROR,TERROR,SWARNING,
     5     TWARNING,IERROR,DGRO,HGRO,CRCHNG,SCRCHNG,
     6     MORTEXP,NTREES2,DBH2,HT2OR,CR2,SCR2B,EXPAN2,
     7     STOR)

c$$$      END IF

C     IF THE DLL THREW AN ERROR, REPORT THE ERROR TO THE OUTPUT FILE.
      if( IERROR .EQ. 1 ) then

         IF (DEBUG) WRITE(JOSTND,125) ICYC, status, IERROR
 125     FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &        ' STATUS=',I2,
     &        ' IERROR=',I2 )

         DO I = 1, 9
            if( SWARNING(I) .EQ. 1 ) then
               IF (DEBUG) WRITE(JOSTND,127) ICYC, I, SWARNING(I)
 127           FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &              ' IDX=',I2, ' SWARNING=',I2 )
            end if
         end do


         DO I = 1, 35
            if( SERROR(I) .EQ. 1 ) then
           
C     IGNORE THE FOLLOWING ERRORS:
C     6 -- BHAGE has been set to 0 for an uneven-aged stand
C     7 -- BHAGE > 0 for an uneven-aged stand
C     8 -- STAGE is too small for the BHAGE
                if( ( I .EQ. 6  ) .OR. 
     &               ( I .EQ. 7 ) .OR. 
     &               ( I .EQ. 8 ) .OR.
     &               ( I .EQ. 9 ) .OR.
     &               ( I .EQ. 11 ) ) then

               IF (DEBUG) WRITE(JOSTND,136) ICYC, I, SERROR(I)
 136             FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &   ' IDX=',I2, ' SERROR=',I2, ' ERROR IGNORED.'
     &   ' PLEASE REFER TO: ', 
     &   'http://www.cof.orst.edu/cof/fr/research/organon/downld.htm' )

                else

               IF (DEBUG) WRITE(JOSTND,126) ICYC, I, SERROR(I)
 126           FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &              ' IDX=',I2, ' SERROR=',I2 )
                end if
            end if
         end do


         DO I = 1, 2000
            if( TWARNING(I) .EQ. 1 ) then
               IF (DEBUG) WRITE(JOSTND,128) ICYC, I, TWARNING(I)
 128           FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &              ' IDX=',I2, ' TWARNING=',I2 )
            end if

            DO J = 1, 6
               if( TERROR(I,J) .EQ. 1 ) then
                  IF (DEBUG) WRITE(JOSTND,129) ICYC, TERROR(I,J), I, J
 129              FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',
     &                 I2,', TERROR(I,J)=',I2,
     &                 ', TREE NUMBER=',I4, 
     &                 ', ERROR NUMBER=', I4 )
               end if

            end do
         end do

      end if                    ! end of the error check for the growth cycle

            DO J = 1, 30
               IF (DEBUG) WRITE(JOSTND,135) ICYC, J, STOR(J)
 135          FORMAT(' CALLING ORGANON.DLL::EXECUTE::BETA, CYCLE=',I2,
     &               ', J=',I2,
     &               ', STOR(J)=',F10.4 )
            end do

C-----------
C     CONVERT THE DATA PROJECTED USING ORGANON TO 
C     TO THE FVS-VARIABLES
C-----------

C     For each of the trees in the tree list
C     SET THE FVS INCREMENTS FROM THE ORGANON INCREMENTS.
C     FVS VARIABLE = ORGANON VARIABLE
      DO I = 1, ITRN           
         DG(I)  =   DGRO(I)     ! ASSIGN THE DIAMETER GROWTH
         HTG(I) =   HGRO(I)     ! ASSIGN THE HEIGHT GROWTH
!         ICR(I) =   IFIX(-CR2(I)*100.0) ! INTEGER - CROWN RATIO


!     ROUND TO THE NEAREST 100-THS PLACE.
!         ICR(I) =   IFIX( (CR2(I)*1000.0+0.5)/1000.0 ) ! INTEGER - CROWN RATIO
         ICR(I) =   ANINT( -CR2(I) * 100.0 ) ! INTEGER - CROWN RATIO


!         ICR(I) =   -1*IFIX( ( CR2(I) * 1000.0 + 0.5 ) / 1000.0 ) ! INTEGER - CROWN RATIO
!         ICR(I) =   IFIX(((CR2(I)*1000.0+0.5)/1000.0) ) ! INTEGER - CROWN RATIO

!         ICR(I) =   FLOAT( INT( A * 1000.0 + 0.5 ) ) / 1000.0

      ENDDO
      

C----------
C     PROCESS THE FIXDG AND FIXHTG OPTIONS.
C----------
      CALL OPFIND (1,MYACTS(1),NTODO)
      IF (NTODO.GT.0) THEN
         DO 300 ITODO=1,NTODO
            CALL OPGET (ITODO,4,IDATE,IACTK,NP,PRM)
            IF (IACTK.LT.0) GOTO 300
            CALL OPDONE(ITODO,IY(ICYC))
            ISPCC=IFIX(PRM(1))
            IF(PRM(2) .LT. 0.) PRM(2)=0.
            IF(PRM(3) .LT. 0.) PRM(3)=0.
            IF(PRM(4) .LE. 0.) PRM(4)=999.
            IF (ITRN.GT.0) THEN
               DO 290 I=1,ITRN
                  LINCL = .FALSE.
                  IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                     LINCL = .TRUE.
                  ELSEIF(ISPCC.LT.0)THEN
                     IGRP = -ISPCC
                     IULIM = ISPGRP(IGRP,1)+1
                     DO 400 IG=2,IULIM
                        IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                           LINCL = .TRUE.
                           GO TO 401
                        ENDIF
 400                 CONTINUE
                  ENDIF
 401              CONTINUE
                  IF (LINCL .AND.
     >                 (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                     DG(I)=DG(I)*PRM(2)
                     IF(LTRIP)THEN
                        ITFN=ITRN+2*I-1
                        DG(ITFN)=DG(ITFN)*PRM(2)
                        DG(ITFN+1)=DG(ITFN+1)*PRM(2)
                     ENDIF
                  ENDIF
 290           CONTINUE
            ENDIF
 300     CONTINUE
      ENDIF
      
      
!! study this says nick.
!! calls up the options processor - what to do this cycle.
!! myacts = how many things you've got to do. 
      CALL OPFIND (1,MYACTS(2),NTODO)
      IF (NTODO.GT.0) THEN
         DO 302 ITODO=1,NTODO
            CALL OPGET (ITODO,4,IDATE,IACTK,NP,PRM)
            IF (IACTK.LT.0) GOTO 302
            CALL OPDONE(ITODO,IY(ICYC))
            ISPCC=IFIX(PRM(1))
            IF (PRM(2) .LT. 0.)  PRM(2)=0.
            IF (ITRN.GT.0) THEN
               DO 301 I=1,ITRN
                  LINCL = .FALSE.
                  IF(ISPCC.EQ.0 .OR. ISPCC.EQ.ISP(I))THEN
                     LINCL = .TRUE.
                  ELSEIF(ISPCC.LT.0)THEN
                     IGRP = -ISPCC
                     IULIM = ISPGRP(IGRP,1)+1
                     DO 402 IG=2,IULIM
                        IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                           LINCL = .TRUE.
                           GO TO 403
                        ENDIF
 402                 CONTINUE
                  ENDIF
 403              CONTINUE
                  IF (LINCL .AND.
     >                 (PRM(3).LE.DBH(I) .AND. DBH(I).LT.PRM(4))) THEN
                     HTG(I)=HTG(I)*PRM(2)
                     
!! if we're trippling, then assign the scale to the triples too.
                     IF(LTRIP)THEN
                        ITFN=ITRN+2*I-1
                        HTG(ITFN)=HTG(ITFN)*PRM(2) !! triple #1
                        HTG(ITFN+1)=HTG(ITFN+1)*PRM(2) !! triple #2
                     ENDIF
                  ENDIF
 301           CONTINUE
            ENDIF
 302     CONTINUE
      ENDIF


C     
C     CALL **MORTS** TO COMPUTE TREE MORTALITY.
C     
      IF (DEBUG) WRITE(JOSTND,160) ICYC
  160 FORMAT(' CALLING MORTS, CYCLE=',I2)
      CALL MORTS

C----------
C     NOW TRIPLE RECORDS AND REALLIGN POINTERS IF TRIPLING OPTION IS SET.
C----------
C     
      IF (LTRIP.AND.ITRN.GT.0) THEN
         IF (DEBUG) WRITE(JOSTND,170) ICYC
 170     FORMAT(' CALLING TRIPLE, CYCLE=',I2)
         CALL TRIPLE
C     
C     INFLATE THE TREE RECORD COUNT.
C     
         ITRN=ITRN*3
C     
C     REALLIGN POINTER VECTORS.
C     
         IF (DEBUG) WRITE(JOSTND,180) ICYC
 180     FORMAT(' CALLING REASS, CYCLE=',I2)
         CALL REASS
C     
C     INFLATE IREC1.
C     
         IREC1=IREC1*3
      ENDIF

C     
C     CALL **FFERT** TO COMPUTE THE EFFECTS OF APPLYING FERTILIZER.
C     
      IF (DEBUG) WRITE(JOSTND,190) ICYC
 190  FORMAT(' CALLING FFERT, CYCLE=',I2)
      CALL FFERT
C     
C     CALL **BGCGROW** TO COMPUTE BGC GROWTH ESTIMATES.
C     
      IF(LBGCGO)CALL BGCGRO(IY(ICYC),IY(ICYC+1))
C     

      RETURN
      END

