      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C  **SITSET-- CS  DATE OF LAST REVISION:  10/03/17
C----------
C THIS SUBROUTINE LOADS THE SITELG ARRAY WITH A SITE INDEX FOR EACH
C SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.
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
C
      LOGICAL DEBUG
      REAL BAMAXA(MAXSP),ASITE(MAXSP),BSITE(MAXSP)
      INTEGER I,J,JJ,K
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*10
      INTEGER IFIASP,ERRFLAG,ISPC,IREGN,KFORST
C----------
C  LOAD BA MAXIMUM VALUES --- R9 APPROVED BAMAX.
C----------
      DATA BAMAXA /
     & 150.,   150.,   210.,   150.,   210.,   150.,   240.,
     & 160.,   150.,   140.,   140.,   140.,   140.,   160.,
     & 160.,   160.,   160.,   160.,   160.,   160.,   160.,
     & 160.,   160.,   150.,   150.,   150.,   150.,   130.,
     & 150.,   150.,   150.,   200.,   150.,   150.,   150.,
     & 150.,   150.,   150.,   150.,   150.,   180.,   150.,
     & 150.,   150.,   150.,   150.,   160.,   160.,   160.,
     & 160.,   160.,   130.,   160.,   160.,   160.,   160.,
     & 130.,   160.,   160.,   160.,   130.,   160.,   160.,
     & 160.,   160.,   160.,   160.,   150.,   150.,   150.,
     & 150.,   150.,   150.,   150.,   130.,   130.,   150.,
     & 150.,   150.,   160.,   150.,   140.,   150.,   150.,
     & 150.,   150.,   150.,   150.,   170.,   150.,   150.,
     & 180.,   150.,   150.,   150.,   150.     /
C---------
C LOAD SITE INDEX COEFF
C----------
      DATA ASITE /
     &      0,      0,-5.1489,      0,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,  18.19,-35.098,
     &-35.098,-35.098,      0,      0,      0,
     &      0,      0,-26.067,-26.067,-26.067,
     &-26.067,-26.067,-26.067,-26.067,-26.067,
     &  -7.01,  18.19,      0,-36.805,-36.805,
     &-36.805,      0, -1.334, -1.334, -2.706,
     &  1.097,      0,      0,      0,      0,
     &      0,      0,      0,  2.656,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,  3.543,  3.543,
     &  3.543,      0,      0,      0,      0,
     &  7.521,  7.521,  7.521,  7.521,      0,
     &      0,      0,      0,      0,      0,
     &      0,      0,      0,      0,      0,
     &      0/
      DATA BSITE /
     &    0.8,    0.8,  1.062,   1.25,   1.25,
     &   1.25,   1.35,   1.19,   1.19,   1.15,
     &   1.15,   1.15,   1.15,   1.08,   1.08,
     &   1.08,   1.08,   1.08,    1.1,    1.1,
     &    1.1,    1.1,    1.1, 0.7695,  1.729,
     &  1.729,  1.729,    1.3,   1.32,   1.32,
     &   1.32,   1.25,1.51754,1.51754,1.51754,
     &1.51754,1.51754,1.51754,1.51754,1.51754,
     &  1.203, 0.7695,   1.22, 1.6748, 1.6748,
     & 1.6748,      1,  1.082,  1.082,  1.106,
     &  1.063,   0.96,   0.89,   0.89,   0.89,
     &   0.89,   0.83,   0.83,  0.965,    1.2,
     &    1.2,    1.2,    1.2,    1.2,    1.2,
     &    1.2,    1.2,   1.35,   1.35,   1.35,
     &   1.35,   1.35,   1.35, 1.2407, 1.2407,
     & 1.2407,    1.2,   1.35,   1.35,   1.35,
     &  1.074,  1.074,  1.074,  1.074,    0.8,
     &    0.8,    0.8,    0.8,    0.8,    0.9,
     &    0.9,    0.9,    0.9,    0.9,    0.9,
     &    0.9/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SITSET',6,ICYC)
C----------
C  DEFAULT MERCH LIMITS ARE SET IN LS/VOLS
C----------
C  SET DEFAULT SITE SPECIES OF WHITE OAK WITH INDEX OF 65
C  IF MISSING
C----------
      IF(ISISP .LE. 0) ISISP=47
      IF(SITEAR(ISISP) .LE. 0.0) SITEAR(ISISP)=65.
C----------
C  BEGIN BY CALCULATING WO SI BASED ON SITE SPECIES 
C----------
      IF(ISISP .NE. 47) SITEAR(47)= (-1*ASITE(ISISP)/BSITE(ISISP))
     &     +((1/BSITE(ISISP))*SITEAR(ISISP))
C----------                                              
C  SET SITE INDEX FOR ALL SPECIES           
C----------                                                   
      DO 5 I=1,MAXSP
        IF(SITEAR(I) .LT. .0001) SITEAR(I) = ASITE(I)
     &     +BSITE(I)*(SITEAR(47))
    5 CONTINUE
C----------                                              
C  SET SDIDEF VALUES IF NOT SET BY KEYWORD.            
C----------
      DO 15 I=1,MAXSP                                   
      IF(SDIDEF(I) .LE. 0.) THEN
        IF(BAMAX .GT. 0.)THEN
          SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
        ELSE
          SDIDEF(I)=BAMAXA(I)/(0.5454154*(PMSDIU/100.))
        ENDIF
      ENDIF
   15 CONTINUE
C----------
C  LOAD VOLUME DEFAULT MERCH. SPECS.
C----------
      DO ISPC=1,MAXSP
      IF(DBHMIN(ISPC).LE.0.)THEN          !SET **DBHMIN** DEFAULT
        IF(ISPC.LE.7)THEN                 !SOFTWOODS
          DBHMIN(ISPC)=5.
        ELSE                              !HARDWOODS
          SELECT CASE(IFOR)
          CASE(1)
            DBHMIN(ISPC)=5.
          CASE DEFAULT
            DBHMIN(ISPC)=6.
          END SELECT
        ENDIF
      ENDIF
      IF(TOPD(ISPC).LE.0.)THEN            !SET **TOPD** DEFAULT
        IF(ISPC.LE.7)THEN                 !SOFTWOODS
          TOPD(ISPC)=4.
        ELSE                              !HARDWOODS
          SELECT CASE (IFOR)
          CASE(2)
            TOPD(ISPC)=5.
          CASE DEFAULT
            TOPD(ISPC)=4.
          END SELECT
        ENDIF
      ENDIF
      IF(BFMIND(ISPC).LE.0.)THEN           !SET **BFMIND** DEFAULT
        IF(ISPC.LE.7)THEN                 !SOFTWOODS
          SELECT CASE(IFOR)
          CASE(1)
            BFMIND(ISPC)=9.
            IF(ISPC.EQ.1)BFMIND(ISPC)=6.
          CASE DEFAULT
            BFMIND(ISPC)=9.
          END SELECT
        ELSE                              !HARDWOODS
          SELECT CASE(IFOR)
          CASE(1)
            BFMIND(ISPC)=9.
          CASE DEFAULT
            BFMIND(ISPC)=11.
          END SELECT
        ENDIF
      ENDIF
      IF(BFTOPD(ISPC).LE.0.)THEN          !SET **BFTOPD** DEFAULT
        IF(ISPC.LE.7)THEN                 !SOFTWOOD
          SELECT CASE(IFOR)
          CASE(1)
            BFTOPD(ISPC)=7.6
            IF(ISPC.EQ.1)BFTOPD(ISPC)=5.
          CASE DEFAULT
            BFTOPD(ISPC)=7.6
          END SELECT
        ELSE                              !HARDWOODS
          SELECT CASE(IFOR)
          CASE(1)
            BFTOPD(ISPC)=7.6
          CASE DEFAULT
            BFTOPD(ISPC)=9.6
          END SELECT
        ENDIF
      ENDIF
      ENDDO
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      KFORST = KODFOR-900
      IREGN=9
      WRITE(FORST,'(I2)')KFORST
      IF(KFORST.LT.10)FORST(1:1)='0'
      DIST='  '
      PROD='  '
      VAR='CS'
C
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9).OR.
     &    (METHC(ISPC).EQ.5)).AND.(VEQNNC(ISPC).EQ.'          '))THEN
        IF(METHC(ISPC).EQ.5)THEN
          VOLEQ(1:7)='900DVEE'
        ELSE
          VOLEQ(1:7)='900CLKE'
        ENDIF        
        PROD='02'
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ
C      WRITE(16,*)'PROD,IFIASP,ISPC,VEQNNC(ISPC)= ',PROD,IFIASP,ISPC,
C     &VEQNNC(ISPC)
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9).OR.
     &    (METHB(ISPC).EQ.5)).AND.(VEQNNB(ISPC).EQ.'          '))THEN
        IF(METHB(ISPC).EQ.5)THEN
          VOLEQ(1:7)='900DVEE'
        ELSE
          VOLEQ(1:7)='900CLKE'
        ENDIF        
        PROD='01'
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ENDIF
      ENDDO
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
