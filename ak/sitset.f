      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
C  SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD, LOADS
C  THE SDIDEF ARRAY WITH SDI MAXIMUMS FOR EACH SPECIES WHICH WAS NOT
C  ASSIGNED AN SDI MAXIMUM USING THE SDIMAX KEYWORD, AND LOADS VOLUME 
C  MERCHANTABILITY SPECIFICATIONS AND VOLUME EQUATIONS.
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz's spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TN
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TB
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
COMMONS
C
C----------
C  DEFINITIONS
C
C     DIST --  
C    FORST --  
C     PROD --  
C      VAR --  
C    VOLEQ --  
C  ERRFLAG --  
C        I -- INDEX USED FOR LOOPING ACROSS SPECIES   
C   IFIASP --  
C   INTFOR --  
C    IREGN --  
C     ISPC -- INDEX USED FOR LOOPING ACROSS SPECIES
C        J -- INDEX USED FOR PRINTING REPORTS 
C       JJ -- INDEX USED FOR PRINTING REPORTS
C        K -- INDEX USED FOR PRINTING REPORTS
C MERCHCDS --  
C MERCHCAT --  
C      TEM -- DEFAULT SITE INDEX, SET TO 70.
C   SDICON -- ARRAY CONTAINING SDI MAXIMUM FOR EACH SPECIES 
C               (BASED ON SHAW AND LONG ANALYSIS)
C      SLO -- ARRAY CONTAINING MINIMUM SITE INDEX VALUES FOR EACH SPECIES
C             ALL SPECIES OTHER THAN WH, MH, AND SS LT 200 YEARS OLD:
C               HEGYI, F., JELINEK, J., VISZLAI, J.,CARPENTER, D.,
C               BRITNEFF, A. 1981.SITE INDEX EQUATIONS AND CURVES 
C               FOR THE MAJOR TREE SPECIES IN BRITISH COLUMBIA.
C               TABLE 4 (100 YR BASE AGE)
C             WH, MH, AND SS LESS THAN 200 YEARS OF AGE: PAYANDEH, B.
C               1974. NONLINEAR SITE INDEX EQUATIONS FOR SEVERAL MAJOR 
C               CANADIAN TIMBER SPECIES. THE FORESTRY CHRONICLE. 194-
C               196.
C      SHI -- ARRAY CONTAINING MAXIMUM SITE INDEX VALUES FOR EACH 
C               SPECIES. CITATIONS ARE THE SAME AS SLO.
C NOTE: CAP SLO and SHI FOR AD, WI, SU, AND OH BASED ON REDUCTIONS MADE
C       FOR HEIGHT DUBBING, LARGE TREE DIAMETER GROWTH, AND LARGE TREE
C       HEIGHT GROWTH.
C         AD, WI, OH: ASSUME MULTIPLIER OF 0.45
C         SU: ASSUME MULTIPLIER OF 0.65
C NOTE -- BASE AGE 100 USED IN SPRTHT (ESSPRT.F) TO DETERMINE HEIGHT 
C         OF SPROUT RECORDS. 
 
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      CHARACTER DIST*2,FORST*2,PROD*2,VAR*2,VOLEQ*10
C
      INTEGER ERRFLAG,I,IFIASP,INTFOR,IREGN,ISPC,J,JJ,K,
     &        MERCHCDS(2,17),MERCHCAT
C
      REAL TEM
C
      REAL SDICON(MAXSP),SLO(MAXSP),SHI(MAXSP)
C
C----------
C  DATA STATEMENTS:
C----------
      DATA SDICON /
     & 790., 602., 592., 387., 412., 412., 500., 654., 679., 762.,
     & 682., 687., 412., 441., 441., 466., 466., 384., 562., 452.,
     & 447., 447., 452./

      DATA SLO /
     &  35., 20., 25., 25., 30., 30., 20., 40., 15., 30.,
     &  35., 20., 30., 20., 75., 35., 45., 45., 45., 45.,
     &  20., 30., 20./

      DATA SHI /
     & 105.,  65.,  75.,  55.,  85.,  85.,  55., 125.,  55.,  85.,
     & 105.,  65.,  85., 50., 135., 75., 90., 105.,  85., 105.,
     &  50.,  70.,  50./
C
C----------
C  LOAD VOLUME DEFAULT MERCH. SPECS.
C----------  
C
C  MERCHCDS CONTAINS MERCH SPEC CATEOGORIES (MERCHCAT) FOR EACH 
C  LOCATION CODE
      DATA MERCHCDS/
     &  713,  1,
     &  720,  2,
     &  7400, 2,
     &  7401, 2,
     &  7402, 2,
     &  7403, 2,
     &  7404, 2,
     &  7405, 2,
     &  7406, 2,
     &  7407, 2,
     &  7408, 2,
     &  703,  3,
     &  1005, 3,
     &  8134, 3,
     &  8135, 3,
     &  8112, 3,
     &  1004, 4/
C----------
C IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
C WITH DEFAULT SITE VALUES. 
C----------
      TEM = 70.
      IF(ISISP .GT. 0) THEN
        IF (SITEAR(ISISP) .GT. 0.0) TEM=SITEAR(ISISP)
      ENDIF
      IF(ISISP .EQ. 0) ISISP = 11
C----------
C TRANSLATE SITE SPECIES SITE INDEX TO OTHER SPECIES
C----------
      DO 10 I=1,MAXSP
        IF(TEM .LT. SLO(ISISP))TEM=SLO(ISISP)
        IF(SITEAR(I) .LE. 0.0) SITEAR(I) = SLO(I) +
     &  (TEM-SLO(ISISP))/(SHI(ISISP)-SLO(ISISP))*
     &  (SHI(I)-SLO(I))

   10 CONTINUE

C----------
C LOAD THE SDIDEF ARRAY.
C----------
      DO 40 I=1,MAXSP
        IF(SDIDEF(I) .GT. 0.0) GO TO 40
        IF(BAMAX .GT. 0.) THEN
          SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
        ELSE
          SDIDEF(I) = SDICON(I)
        ENDIF
   40 CONTINUE
C
      DO 92 I=1,15
      J=(I-1)*10 + 1
      JJ=J+9
      IF(JJ.GT.MAXSP)JJ=MAXSP
      WRITE(JOSTND,90)(NSP(K,1)(1:2),K=J,JJ)
   90 FORMAT(/'SPECIES ',5X,10(A2,6X))
      WRITE(JOSTND,91)(SDIDEF(K),K=J,JJ )
   91 FORMAT('SDI MAX ',   10F8.0)
      IF(JJ .EQ. MAXSP)GO TO 93
   92 CONTINUE
   93 CONTINUE
C
C----------
C  LOOP THROUGH MERCHCDS AND SELECT MERCHCAT BASED ON
C  LOCATION CODE (KODFOR - 17 OPTIONS)
C----------
      DO I=1, 17 
        IF(KODFOR .EQ. MERCHCDS(1,I)) THEN 
          MERCHCAT= MERCHCDS(2,I)
         EXIT
C  DEFAULT TO TONGASS IF LOCATION CODE IS NOT FOUND IN
C  MERCHCDS 
        ELSE 
          MERCHCAT= 3
        ENDIF
      ENDDO
C----------
C  DETERMINE DBHMIN, BFMIND, BFTOPD, AND TOPD BASED ON MERCHCAT
C----------
C  SET DBHMIN DEFAULTS
      DO ISPC=1,MAXSP
        IF(DBHMIN(ISPC) .LE. 0) THEN
          SELECT CASE(MERCHCAT)
           CASE(1)
            DBHMIN(ISPC)= 6
           CASE(2)
            DBHMIN(ISPC)= 5
           CASE(3, 4)
            DBHMIN(ISPC)= 9
           CASE DEFAULT
            DBHMIN(ISPC)= 9
          END SELECT
        ENDIF
C  SET BFMIND DEFAULTS
        IF(BFMIND(ISPC) .LE. 0) THEN
          SELECT CASE(MERCHCAT)
           CASE(1, 2, 3, 4)
            BFMIND(ISPC)= 9
           CASE DEFAULT
            BFMIND(ISPC)= 9
          END SELECT
        ENDIF
C  SET TOPD DEFAULTS
        IF(TOPD(ISPC) .LE. 0) THEN
          SELECT CASE(MERCHCAT)
           CASE(1, 2)
            TOPD(ISPC)= 4
           CASE(3)
            TOPD(ISPC)= 7
           CASE(4)
            TOPD(ISPC)= 6
           CASE DEFAULT
            TOPD(ISPC)= 7
          END SELECT
        ENDIF
C  SET BFTOPD DEFAULTS
        IF(BFTOPD(ISPC) .LE. 0) THEN
          SELECT CASE(MERCHCAT)
           CASE(1,2,4)
            BFTOPD(ISPC)= 6
           CASE(3)
            BFTOPD(ISPC)= 7
           CASE DEFAULT
            BFTOPD(ISPC) = 7
          END SELECT
        ENDIF
      ENDDO
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
C     CHANGED CONDITION FROM .LT. TO .NE. IN ORDER TO DEAL WITH NEW 
C     LOCATION CODES FOR STATE, BLM, TCC, AND TRIBAL LANDS      
      IF(INTFOR.NE.10)FORST(1:1)='0'
      IREGN = KODFOR/100
      DIST='  '
      PROD='  '
      VAR='AK'
C----------
C    BRITISH COLUMBIA, MAKAH INDIAN RESERVATION AND IREGN
C    81 ARE MAPPED TO THE TONGASS
C    
C    LOCATION CODES 713, 720 AND IREGN 81 ARE MAPPED TO 
C    THE CHUGACH
C----------
      IF((KODFOR.EQ.713).OR.(KODFOR.EQ.720).OR.(IREGN.EQ.74))THEN
        IREGN=10
        FORST='04'
      ELSE IF((KODFOR.EQ.703).OR.(IREGN.EQ.81)) THEN
        IREGN=10
        FORST='05'
      ENDIF
C LOAD VOLUME ARRAYS USING VOLEQDEF
      DO ISPC=1,MAXSP
        READ(FIAJSP(ISPC),'(I4)')IFIASP
!       Determine default cubic volume equation
        IF(VEQNNC(ISPC).EQ.'          ') THEN
          IF(METHC(ISPC).EQ.6 .OR. METHC(ISPC).EQ.9) THEN
            CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,
     +                    ERRFLAG)
          ELSEIF(METHC(ISPC).EQ.10) THEN
            CALL NVB_DefaultEq(IREGN,FORST,DIST,IFIASP,VOLEQ)
          END IF
          VEQNNC(ISPC)=VOLEQ
        END IF
!       Determine default board foot volume equation
        IF(VEQNNB(ISPC).EQ.'          ') THEN
          IF(METHB(ISPC).EQ.6 .OR. METHB(ISPC).EQ.9) THEN
            CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,
     +                    ERRFLAG)
          ELSEIF(METHB(ISPC).EQ.10) THEN
            CALL NVB_DefaultEq(IREGN,FORST,DIST,IFIASP,VOLEQ)
          END IF
          VEQNNB(ISPC)=VOLEQ
        END IF
      END DO 
C----------
C  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE
C---------
      IF(LFIA) THEN
        CALL FIAHEAD(JOSTND)
        WRITE(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I=1,MAXSP)
 211    FORMAT ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
      ENDIF
C----------
C  WRITE VOLUME EQUATION NUMBER TABLE
C----------
      CALL VOLEQHEAD(JOSTND)
      WRITE(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J=1,MAXSP)
 230  FORMAT(4(2X,A2,4X,A10,1X,A10,1X))
C
      RETURN
      END
