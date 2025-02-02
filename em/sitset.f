      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C EM $Id$
C----------
C
C THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
C SPECIES THAT WAS NOT ASSIGNED BY KEYWORD, AND LOADS THE SDIDEF
C ARRAY FOR EACH SPECIES WHICH WAS NOT ASSIGNED AN SDI MAXIMUM
C BY KEYWORD.
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
      INCLUDE 'EMCOM.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
COMMONS
C----------
      LOGICAL DEBUG
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*11
      INTEGER IFIASP,ERRFLAG,JJ,K
      REAL SDICON(9)
      REAL BAMAXA(30)
      INTEGER MAPSDI(122),I,ISDI,INTFOR,IREGN,ISPC,J
      INTEGER MAPSIT(30,11),MAPSS(30)
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
C
C  SPECIES EXPANSION
C  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
C  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
C  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
C  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
C  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)
C----------
C
      DATA BAMAXA/
     &  140.,220.,250.,310.,240.,270.,310.,310.,200.,310.,
     &  290.,330.,380.,440.,500.,500.,390.,390.,440.,180.,
     &  290.,400.,350.,390.,260.,300.,220.,220.,160.,300./
C
      DATA MAPSDI/9*2,13*5,5*1,4,3*3,4*5,4*4,3,1,3*3,2,1,10*2,
     &2*7,3,2*7,2*9,2*4,15*7,4*4,2*8,3*5,2*7,5,3*6,7,8,14*9,3*7,
     &4,6,9,5*5/
C
      DATA SDICON /467.,634.,696.,768.,775.,751.,707.,661.,635./
C
      DATA MAPSIT/
     & 25,29,35,35,32,34,35,32,27,39,37,32,36,41,41,41,!LM,LL
     &    43,38,39,32,36,34,34,32,36,36,36,26,22,26,
     &  6, 8,10,10, 9,10,10, 9, 7,11, 9,11,12,12,12,12,!RM
     &    13,11,11, 9,10,10,10, 9,10,10,10, 7, 6, 7,
     & 36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,!AS
     &    60,54,55,46,49,52,52,46,52,52,52,38,33,38,
     & 44,57,76,62,72,72,71,67,55,86,67,80,95,93,93,93,!GA,CW,BA,PW,NC,OH
     &    98,84,86,67,73,80,80,67,80,80,80,49,36,49,
     & 36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,!PB
     &    60,54,55,46,49,52,52,46,52,52,52,38,33,38,
     & 35,51,57,60,48,55,45,38,44,60,64,47,47,47,47,47,!WB,WL,OS
     &    47,47,47,53,50,57,49,57,49,49,47,41,26,60,
     & 46,68,76,80,64,73,67,67,59,80,85,62,62,62,62,62,!PP
     &    62,62,62,70,67,76,65,76,65,65,62,55,35,80,
     & 26,44,49,51,41,47,39,33,38,51,42,40,40,40,40,40,!DF
     &    40,40,40,46,43,49,42,42,42,42,30,36,23,51,
     & 46,68,76,80,75,73,72,70,63,80,80,62,62,62,62,62,!LP
     &    62,62,62,74,76,76,68,68,68,65,35,51,35,80,
     & 46,68,76,78,64,73,61,51,59,80,85,68,68,68,68,68,!ES
     &    68,68,68,75,68,78,80,80,80,67,64,53,45,78,
     & 46,68,76,80,64,73,61,51,59,80,85,62,62,62,62,62,!AF
     &    62,62,62,70,67,76,76,76,76,65,62,55,35,80/
C
      DATA MAPSS/2*10,7*3,2*8,2*3,3*3,3,5*9,3,9,3,3*9,3,3/
C
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SITSET',6,ICYC)
C----------
C IF SITEAR() HAS NOT BEEN SET WITH SITECODE THEN CALCULATE VALUE HERE.
C SITE SPECIES DEFAULT WAS ALWAYS DOUGLAS-FIR (3) IN OLD EM. SETTING IT
C BY MAPPED HABITAT TYPE SEEMED LIKE A BETTER IDEA. INDEX ITYPE IS THE
C CORRECT INDEX POINTING TO ONE OF THE ORIGINAL 30 NI HABITAT TYPES.
C GD 2-9-09.
C
C ADDED MAPPING OF ALL SPECIES SITE INDICES BASED ON AVERAGE SITE INDEX
C FOUND IN PFISTER 1977 ADJUSTED FOR BASE AGE DIFFERENCES USING EQUATION
C BELOW
C   SPECIES   BASE YR   REF
C     WB       100      ALEXANDER TACKLE AND DAHMS REDUCED TO .75
C     DF        50      MONSERUD
C     LP       100      ALEXANDER TACKLE AND DAHMS
C     AF       100      ALEXANDER
C     ES       100      ALEXANDER
C     PP       100      MEYER  BULL 630
C
C PFISTER 1977 BASE AGE CONVERSION EQUATION
C 50YR BASE = 1.0096 + .6279*100YR BASE
C----------
      IF(ISISP.EQ.0)ISISP = MAPSS(ITYPE)
      DO 30 I=1,MAXSP
      IF(SITEAR(I) .EQ. 0.0) THEN                       !IF SPECIES SI NOT SET
        IF(I .EQ. 4 .OR. I.EQ. 5)THEN
          SITEAR(I)=MAPSIT(ITYPE,1)
        ELSEIF(I .EQ. 6)THEN
          SITEAR(I)=MAPSIT(ITYPE,2)
        ELSEIF(I .EQ. 12)THEN
          SITEAR(I)=MAPSIT(ITYPE,3)
        ELSEIF(I.EQ.11 .OR. (I.GE.13 .AND. I.LE.16) .OR.
     &         I.EQ.19)THEN
          SITEAR(I)=MAPSIT(ITYPE,4)
        ELSEIF(I .EQ. 17)THEN
          SITEAR(I)=MAPSIT(ITYPE,5)
        ELSEIF(I .EQ. 1 .OR. I .EQ. 2 .OR. I .EQ. 18)THEN
          SITEAR(I)=MAPSIT(ITYPE,6)          
        ELSEIF(I .EQ. 10)THEN
          SITEAR(I)=MAPSIT(ITYPE,7)
        ELSEIF(I .EQ. 3)THEN
          SITEAR(I)=MAPSIT(ITYPE,8)
        ELSEIF(I .EQ. 7)THEN
          SITEAR(I)=MAPSIT(ITYPE,9)
        ELSEIF(I .EQ. 8)THEN
          SITEAR(I)=MAPSIT(ITYPE,10)
        ELSEIF(I .EQ. 9)THEN
          SITEAR(I)=MAPSIT(ITYPE,11)
        ELSE
          SITEAR(I)=70.
        ENDIF
      ENDIF
   30 CONTINUE
C----------
C LOAD BAMAX AND THE SDIDEF ARRAY
C----------
      ISDI=5
      IF(IEMTYP .GT. 0) ISDI=MAPSDI(IEMTYP)
      DO 40 I=1,MAXSP
        IF(SDIDEF(I) .GT. 0.0) GO TO 40
        IF(BAMAX .GT. 0.) THEN
          SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
        ELSE
          SDIDEF(I) = SDICON(ISDI)
        ENDIF
   40 CONTINUE
      IF(BAMAX.LE.0.) BAMAX=BAMAXA(ITYPE)
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
C  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
C  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
C  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.
C----------
      DO 50 ISPC=1,MAXSP
      IF(METHB(ISPC).EQ.999)METHB(ISPC)=6
      IF(METHC(ISPC).EQ.999)METHC(ISPC)=6
   50 CONTINUE
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      IREGN = KODFOR/100
      DIST='  '
      PROD='  '
      VAR='EM'
C
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9)).AND.
     &     (VEQNNC(ISPC).EQ.'           '))THEN
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9)).AND.
     &     (VEQNNB(ISPC).EQ.'           '))THEN
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ENDIF
      ENDDO
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
