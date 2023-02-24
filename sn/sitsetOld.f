      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C SN $Id$
C----------
C  THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
C  SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD. LOGIC
C  DEVELOPED BY DONNELLY, FMSC. WRITES FIA SPECIES CODE/FVS SPECIES
C  CODE TRANSLATION TABLE
C  THIS SUBROUTINE FOLLOWS SE-TWIGS SUBROUTINES BLOCK1, CONV1, AND
C  CONV2.
C
C Note: This version of sitset.f has been replaced Q2 2023 to update
C       Merchantability specifications for National Forests in NC, 
C       per FSH 2409.18 Ch 50 Supplement R8_NC_2409.18-2022-1
C       ** DELETE THIS FILE ONCE ALL TESTING HAS BEEN COMPLETE **
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
      INCLUDE 'SNCOM.F77'
C
C
COMMONS
C
C
      INTEGER IFIASP, ERRFLAG
      INTEGER I,IMAPSP,IGRP,IMGSP,IXTMP
      INTEGER IREGN,IFORST,INTDIST,ISPC,J
      REAL RSISP,A,B,C,D,XTMP
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*10
      LOGICAL DEBUG
      REAL SIMAX(MAXSP), SIMIN(MAXSP), SDICON(MAXSP)
      INTEGER ISNSIS(43),ISNGRP(43),MAPSI(MAXSP), MGSISP(9)
      REAL MGRSI(9), MGSI, MGSPIX, MGSION
C----------
C  LOAD SDI MAXIMUM VALUES 
C----------
      DATA SDICON /
     &  655., 354., 412., 499., 490., 385., 490., 332., 398., 398., 
     &  310., 529., 480., 499., 692., 623., 518., 371., 344., 421., 
     &  590., 371., 371., 400., 350., 375., 276., 492., 420., 422., 
     &  257., 147., 364., 414., 408., 423., 414., 338., 492., 430., 
     &  155., 283., 283., 430., 478., 492., 415., 492., 492., 492., 
     &  422., 277., 726., 430., 704., 304., 164., 492., 499., 648., 
     &  520., 384., 361., 315., 342., 405., 326., 387., 384., 326., 
     &  417., 336., 365., 417., 414., 342., 311., 370., 410., 343., 
     &  447., 492., 526., 282., 263., 282., 227., 354., 492., 421./
C----------
C  THIS ARRAY CONTAINS OFFICIAL SITE SPECIES USED IN FIA DATA AND
C  IS CONTAINED IN THE SN SPECIES LIST
C----------
      DATA ISNSIS/
     &   5,   6,  11,  17,  64,  35,  47,  75,  78,  15,
     &  16,  44,  59,  76,  45,   8,  12,  13,   2,   1,
     &   3,   7,   4,  14,  34,  61,  65,  87,  74,  10,
     &  20,  22,  24,  25,  33,  60,  62,  63,  66,  69,
     &  71,  73,  83/
C----------
C  THIS ARRAY CONTAINS A MAP FROM THE OFFICIAL SITE SPECIES TO AN
C  INDEX GROUP.
C----------
      DATA ISNGRP/
     &   1,   1,   1,   1,   2,   2,   2,   2,   2,   3,
     &   3,   3,   3,   3,   3,   4,   4,   4,   5,   5,
     &   5,   5,   5,   5,   6,   6,   6,   6,   7,   8,
     &   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
     &   9,   9,   9/
C----------
C  THIS ARRAY CONTAINS A MAP TO ESTIMATE INDIVIDUAL SITE INDICIES.
C----------
      DATA MAPSI/
     &   5,   5,   5,   5,   1,   1,   5,   4,   9,   8,
     &   1,   4,   4,   5,   3,   3,   1,   9,   9,   9,
     &   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
     &   9,   9,   9,   6,   2,   9,   9,   9,   9,   9,
     &   9,   9,   9,   3,   3,   9,   2,   9,   9,   9,
     &   9,   9,   9,   9,   9,   9,   9,   9,   3,   9,
     &   6,   9,   9,   2,   6,   9,   9,   9,   9,   9,
     &   9,   9,   9,   7,   2,   3,   9,   2,   9,   9,
     &   9,   9,   9,   9,   9,   9,   6,   5,   9,   9/
C----------
C  ARRAY TO IDENTIFY THE "MASTER GROUP" SPECIES WHICH HAVE PUBLISHED
C  REFERENCES TO ALLOW DIRECT TRANSFORMATION OF SITE INDEX BETWEEN
C  SPECIES (DOOLITTLE 1958).  SITE INDEX COMPARISONS...SOUTHERN
C  APPALACHIANS, SOIL SCIENCE OF AMERICA PROC. 22(5):455-458;
C  USDA FS(R8) 1992.  SILVICULTURAL...PRESCRIPTION FIELD BOOK, P.19)
C----------
C--MASTER GROUP NO.1   2   3   4   5   6   7   8   9
      DATA MGSISP /5, 64, 45, 12, 14, 65, 74, 10, 63/
C----------
C  ARRAYS CONTAINING MAX AND MIN SITE INDEX BY SPECIES AND
C  MODIFIED FOR USE IN HEIGHT GROWTH SITE INDEX SCALING.
C  THIS PROCESS IS ESPECIALLY IMPORTANT FOR THE SHORTER TREE SPECIES.
C----------
      DATA SIMIN/
     &  15,   15,   15,   35,   35,   35,   45,   45,   35,   25,
     &  35,   40,   40,   35,   30,   30,   35,   35,   35,   35,
     &  30,   35,   25,   35,   35,   15,   25,   30,   15,   15,
     &  15,   15,   35,   35,   35,   35,   35,   25,   15,   15,
     &  35,   35,   35,   30,   30,   35,   25,   35,   15,   35,
     &  15,   15,   30,   35,   35,   15,   15,   15,   30,   40,
     &  30,   35,   25,   25,   25,   30,   25,   25,   35,   25,
     &  35,   35,   30,   25,   25,   15,   25,   25,   30,   25,
     &  15,   15,   35,   35,   35,   35,   35,   15,   15,   15/
C
      DATA SIMAX/
     & 100,   70,   80,  100,  105,  105,   90,  125,   70,   95,
     & 105,  135,  125,   95,  120,  120,   90,   70,   70,   85,
     & 105,  100,   90,   85,   70,   40,   85,   90,   90,   40,
     &  45,   70,   85,  105,   95,   85,  105,  120,   50,   65,
     &  70,   85,   85,  125,  135,  125,  115,  125,   75,  125,
     &  40,   55,  105,  105,   95,   40,   70,   60,  120,  125,
     &  90,  105,  115,  115,  115,  125,   65,   65,   95,   65,
     &  95,   75,  115,  115,  115,  125,   85,  115,   65,   95,
     & 110,   80,   90,   90,   90,   90,   90,   55,   55,   55/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'SITSET',6,ICYC)
C----------
C  SET DEFAULT SITE INDEX.
C  IF SITE INDEX SPECIES IS BLANK, USE WHITE OAK.  IF NOT BLANK, CHECK
C  TO SEE IF IT IS A RECOGNIZED SN SITE SPECIES.  IF NOT, SET TO WHITE OAK.
C  FVS SEQUENCE No. 63 AND SITE INDEX OF 70
C----------
      IF(ISISP .LE. 0) ISISP=63
      DO 100 I=1,43
        IF(ISISP .EQ. ISNSIS(I))THEN
          IMAPSP=I
          GO TO 101
        ENDIF
  100 CONTINUE
      ISISP=63
      IMAPSP=38
  101 CONTINUE
      IF(SITEAR(ISISP) .LE. 0.0) SITEAR(ISISP)=70.
C----------
C  BOUND INPUT SITE INDEX HERE
C  (VALUES FROM DONNELLY, 02/2000)
C----------
      IF (SITEAR(ISISP) .LE. SIMIN(ISISP)) THEN
        SITEAR(ISISP) = SIMIN(ISISP)
        WRITE (JOSTND,110) JSP(ISISP), SITEAR(ISISP)
      ENDIF
      IF (SITEAR(ISISP) .GE. SIMAX(ISISP)) THEN
        SITEAR(ISISP) = SIMAX(ISISP)
        WRITE (JOSTND,110) JSP(ISISP), SITEAR(ISISP)
      ENDIF
  110 FORMAT('***** WARNING - THE SITE SPECIES (',A4,') SITE INDEX VALU
     1E WAS OUTSIDE OF THE ALLOWABLE RANGE, THE VALUE USED WAS ',F5.1)
C----------
C  COMPUTE RELATIVE SITE INDEX FOR SITE SPECIES
C----------
      RSISP = (SITEAR(ISISP)-SIMIN(ISISP))/(SIMAX(ISISP)-SIMIN(ISISP))
C----------
C  FOR MASTER GROUP SPECIES, FIND INDEX EQUATION COEFFICIENTS FOR
C  X-AXIS VALUE CORRESPONDING TO SITE SPECIES SI.  FIRST, FIND
C  MASTER GROUP NUMBER OF SITE INDEX SPECIES.  THEN, DETERMINE
C  COEFFICIENTS TO BE USED IN EQUATION TO COMPUTE TRANSFORMATION
C  INDEX.
C----------
      IGRP=ISNGRP(IMAPSP)
      IF(IGRP .EQ. 1) THEN
        IF (PCOM(1:1) .EQ. 'M') THEN
C----------
C   MOUNTAIN
C----------
          A = -7.1837
          B =  0.1633
        ELSE
C----------
C   OTHER
C----------
          A = -10.0
          B =  0.2
        ENDIF
      ELSEIF (IGRP.EQ.2) THEN
        IF (PCOM(1:1) .EQ. 'M') THEN
          A = -8.6809
          B =  0.1702
        ELSE
          A = -12.0
          B =  0.2
          IF(ISISP .EQ. 78) THEN
            A = -16.0
            B = 0.2667
          ENDIF
        ENDIF
      ELSEIF (IGRP.EQ.3) THEN
        A = -4.0
        B =  0.1
      ELSEIF (IGRP.EQ.4) THEN
        A = -9.4118
        B =  0.1569
      ELSEIF (IGRP.EQ.5) THEN
        A = -9.3913
        B =  0.1739
      ELSEIF (IGRP.EQ.6) THEN
        A = -10.0
        B =  0.2
      ELSEIF (IGRP .EQ. 7) THEN
        A = -8.6809
        B =  0.1702
      ELSEIF (IGRP.EQ.8) THEN
        A = -7.1837
        B =  0.1633
      ELSEIF (IGRP.EQ.9) THEN
        IF (PCOM(1:1) .EQ. 'M') THEN
          A = -8.7442
          B =  0.186
        ELSE
          A = -10.0
          B =  0.2
        ENDIF
      ENDIF
C----------
C  USING A, B COEFFS FROM ABOVE, COMPUTE SITE TRANSFORM INDEX:
C  FIRST, FIND MASTER GROUP SPECIES FROM MASTER GROUP SITE INDEX ARRAY,
C         GIVEN THE GROUP NUMBER OF THE ACTUAL SITE INDEX SPECIES
C  SECOND, COMPUTE MASTER GROUP SPECIES OWN SITE INDEX
C  THIRD, GIVEN MASTER GROUP SPECIES OWN SITE INDEX, COMPUTE MASTER
C         GROUP SITE SPECIES TRANSFORM INDEX
C----------
      IMGSP  = MGSISP(IGRP)
      MGSION = RSISP * ( SIMAX(IMGSP) - SIMIN(IMGSP) ) + SIMIN(IMGSP)
      MGSPIX = A + B*MGSION
C----------
C    FIRST, FIND THE COEFFICIENTS NEEDED TO ESTIMATE THE SITE INDEX FOR
C    EACH OF THE MASTER GROUP SPECIES.  THEN, USING THE COEFFS C AND D,
C    COMPUTE THE SITE INDEX FOR EACH MASTER GROUP SPECIES (MGSI).
C    THEN, COMPUTE THE RELATIVE SITE INDEX FOR EACH MASTER GROUP
C    SPECIES (MGRSI) BASED ON ITS SITE INDEX POSITION IN ITS RANGE
C    FROM ITS SIMIN TO ITS SIMAX.
C----------
      DO 130 I=1, 9
        IF (I.EQ.1) THEN
          IF (PCOM(1:1) .EQ. 'M') THEN
            C = 44.
            D = 6.13
          ELSE
            C = 50.
            D = 5.
          ENDIF
        ELSEIF (I.EQ.2) THEN
          IF (PCOM(1:1) .EQ. 'M') THEN
            C = 51.
            D = 5.88
          ELSE
            C = 60.
            D = 5.
            IF(ISISP .EQ. 78) THEN
              C = 60.
              D = 3.75
            ENDIF
          ENDIF
        ELSEIF (I.EQ.3) THEN
          C = 40.
          D = 10.
        ELSEIF (I.EQ.4) THEN
          C = 60.
          D = 6.38
        ELSEIF (I.EQ.5) THEN
          C = 54.
          D = 5.75
        ELSEIF (I.EQ.6) THEN
          C = 50.
          D = 5.
        ELSEIF (I.EQ.7) THEN
          C = 51.
          D = 5.88
        ELSEIF (I .EQ. 8) THEN
          C = 44.
          D = 6.13
        ELSEIF( I .EQ. 9.) THEN
          IF (PCOM(1:1) .EQ. 'M') THEN
            C = 47.
            D = 5.38
          ELSE
            C = 50.
            D = 5.
          ENDIF
        ENDIF
        MGSI = C + D * MGSPIX
        MGRSI(I) = ( MGSI - SIMIN(MGSISP(I))) /
     &             ( SIMAX(MGSISP(I)) - SIMIN(MGSISP(I)))
  130 CONTINUE
C----------
C    GIVEN THE TRANSFORMED RELATIVE SITE INDEX VALUES FOR EACH OF THE
C    MASTER GROUP SPECIES, COMPUTE AN ESTIMATED SITE INDEX VALUE FOR
C    EACH SPECIES POSSIBLE IN THE VARIANT.  EACH SPECIES IS
C    ASSOCIATED WITH ONE OF THE MASTER GROUP SPECIES.  THE RELATIVE
C    SITE INDEX THEN SCALES THE POTENTIAL RANGE OF SITE INDEX VALUES
C    USED IN THE HEIGHT GROWTH EQUATION (CARMEAN, NC-128) ASSIGNED
C    TO EACH SPECIES.  ALSO, EACH SITE INDEX VALUE IS CHECKED TO BE
C    SURE IT IS WITHIN MIN AND MAX BOUNDS.
C----------
      DO 150 I = 1, MAXSP
        IF ( SITEAR(I) .EQ. 0. ) THEN
          SITEAR(I) = MGRSI(MAPSI(I))*(SIMAX(I) - SIMIN(I)) + SIMIN(I)
          IF (DEBUG) WRITE(JOSTND,*)'I, SITEAR= ',I,'  ',SITEAR(I)
          IF (SITEAR(I) .LE. SIMIN(I)) THEN
            IF(DEBUG) WRITE(JOSTND,155) JSP(I), SITEAR(I),SIMIN(I)
            SITEAR(I) = SIMIN(I)
          ENDIF
          IF (SITEAR(I) .GE. SIMAX(I)) THEN
            IF(DEBUG) WRITE(JOSTND,155) JSP(I), SITEAR(I),SIMAX(I)
            SITEAR(I) = SIMAX(I)
          ENDIF
        ENDIF
  155   FORMAT('*** WARNING - THE SITE SPECIES (',A4,f5.1,')SITE INDEX
     1 WAS OUTSIDE OF THE ALLOWABLE RANGE, THE VALUE USED WAS ',F5.1)
  150 CONTINUE
C----------
C  END OF SITE DUBBING LOGIC
C----------                                              
C  SET SDIDEF VALUES IF NOT SET BY KEYWORD.            
C----------
      DO 15 I=1,MAXSP                                   
      IF(SDIDEF(I) .LE. 0.) THEN
        IF(BAMAX .GT. 0.)THEN
          SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
        ELSE
          SDIDEF(I) = SDICON(I)
        ENDIF
      ENDIF
   15 CONTINUE
C  START DIGESTION OF FOREST TYPE VARIABLE
C----------
      IF(IFORTP .GT. 999) THEN
C----------
C  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
C  CHARACTER INDICATE THAT THE USER SETS THE FOREST TYPE TO BE
C  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
C  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE
C----------
        XTMP= FLOAT(IFORTP)
        XTMP= XTMP/1000. + 0.00001
        IXTMP= INT(XTMP)
        IFORTP= INT((XTMP-REAL(IXTMP))*1000.)
        LFLAGV= .TRUE.
      ENDIF
C----------
C  LOAD VOLUME DEFAULT MERCH. SPECS.
C----------
      IF(ISEFOR.NE.0)THEN
        IREGN = KODFOR/10000
        IFORST=KODFOR/100-IREGN*100
        WRITE(FORST,'(I2)')IFORST
        IF(IFORST.LT.10)FORST(1:1)='0'
        INTDIST = KODFOR - (KODFOR/100)*100
        WRITE(DIST,'(I2)')INTDIST
        IF(INTDIST.LT.10)DIST(1:1)='0'
      ELSE
        IF(KODFOR .GT. 1000)THEN
          IFORST = KODFOR/100-900
        ELSE
          IFORST = KODFOR-900
        ENDIF
        DIST=' '
        IREGN=9
        VOLEQ(1:7)='900CLKE'
        WRITE(FORST,'(I2)')IFORST
        IF(IFORST.LT.10)FORST(1:1)='0'
      ENDIF
C
      DO ISPC=1,MAXSP
      IF(IREGN.EQ.8)THEN
        IF(DBHMIN(ISPC).LE.0.)THEN                 !SET **DBHMIN** DEFAULT
          IF((ISPC.LE.17).OR.(ISPC.EQ.88))THEN   !SOFTWOODS
            IF((ISPC.EQ.7).OR.(ISPC.EQ.13))THEN
              DBHMIN(ISPC)=6.
            ELSE
              DBHMIN(ISPC)=4.
            ENDIF
          ELSE                                   !HARDWOODS
            IF((ISPC.EQ.39).OR.(ISPC.EQ.43).OR.(ISPC.EQ.44).OR.
     &         (ISPC.EQ.52).OR.(ISPC.EQ.53).OR.(ISPC.EQ.55).OR.
     &         (ISPC.EQ.63))THEN
              DBHMIN(ISPC)=6.
            ELSE
              DBHMIN(ISPC)=4.
            ENDIF
          ENDIF
        ENDIF
        IF(TOPD(ISPC).LE.0.)TOPD(ISPC)=4.        !SET **TOPD** DEFAULT
        IF(BFMIND(ISPC).LE.0.)THEN                 !SET **BFMIND** DEFAULT
          IF((ISPC.LE.17).OR.(ISPC.EQ.88))THEN   !SOFTWOODS
            SELECT CASE(IFOR)
            CASE(10)
              IF(ISPC.EQ.2)THEN
                BFMIND(ISPC)=9.
              ELSE
                BFMIND(ISPC)=10.
              ENDIF
            CASE DEFAULT
              BFMIND(ISPC)=10.
            END SELECT
          ELSE                                   !HARDWOODS
            BFMIND(ISPC)=12.
          ENDIF
        ENDIF
        IF(BFTOPD(ISPC).LE.0.)THEN                 !SET **BFTOPD** DEFAULT
          IF((ISPC.LE.17).OR.(JSP(ISPC)(1:2).EQ.'OS'))THEN
            BFTOPD(ISPC)=7.
          ELSE
            BFTOPD(ISPC)=9.
          ENDIF
        ENDIF
      ELSE                                     !REGION 9
        IF(DBHMIN(ISPC).LE.0.)THEN          !SET **DBHMIN** DEFAULT
          IF((ISPC.LE.17).OR.(ISPC.EQ.88))THEN !SOFTWOODS
            DBHMIN(ISPC)=5.
          ELSE                              !HARDWOODS
            SELECT CASE(IFOR)
            CASE(5)
              DBHMIN(ISPC)=5.
            CASE DEFAULT
              DBHMIN(ISPC)=6.
            END SELECT
          ENDIF
        ENDIF
        IF(TOPD(ISPC).LE.0.)THEN            !SET **TOPD** DEFAULT
          IF(ISPC.LE.17)THEN                 !SOFTWOODS
            TOPD(ISPC)=4.
          ELSE                              !HARDWOODS
            SELECT CASE (IFOR)
            CASE(8)
              TOPD(ISPC)=5.
            CASE DEFAULT
              TOPD(ISPC)=4.
            END SELECT
          ENDIF
        ENDIF
        IF(BFMIND(ISPC).LE.0.)THEN           !SET **BFMIND** DEFAULT
          IF(ISPC.LE.17)THEN                 !SOFTWOODS
            SELECT CASE(IFOR)
            CASE(5)
              BFMIND(ISPC)=9.
              IF(ISPC.EQ.2)BFMIND(ISPC)=6.
            CASE DEFAULT
              BFMIND(ISPC)=9.
            END SELECT
          ELSE                              !HARDWOODS
            SELECT CASE(IFOR)
            CASE(5)
              BFMIND(ISPC)=9.
            CASE DEFAULT
              BFMIND(ISPC)=11.
            END SELECT
          ENDIF
        ENDIF
        IF(BFTOPD(ISPC).LE.0.)THEN          !SET **BFTOPD** DEFAULT
          IF(ISPC.LE.17)THEN                 !SOFTWOOD
            SELECT CASE(IFOR)
            CASE(5)
              BFTOPD(ISPC)=7.6
              IF(ISPC.EQ.2)BFTOPD(ISPC)=5.
            CASE DEFAULT
              BFTOPD(ISPC)=7.6
            END SELECT
          ELSE                              !HARDWOODS
            SELECT CASE(IFOR)
            CASE(5)
              BFTOPD(ISPC)=7.6
            CASE DEFAULT
              BFTOPD(ISPC)=9.6
            END SELECT
          ENDIF
        ENDIF
      ENDIF
      ENDDO
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      PROD='  '
      VAR='SN'
C
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9)).AND.
     &     (VEQNNC(ISPC).EQ.'          '))THEN

        PROD='02'
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ

      IF(DEBUG)WRITE(16,*)'VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,',
     & 'ERRFLAG= ',VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9)).AND.
     &     (VEQNNB(ISPC).EQ.'          '))THEN
        PROD='01'
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ENDIF
      ENDDO
C----------
C  HT-DBH COEFFICIENTS
C----------
      HT1(1)=  4.5084
      HT1(2)=  4.0374
      HT1(3)=  4.5084
      HT1(4)=  4.2899
      HT1(5)=  4.6271
      HT1(6)=  4.6561
      HT1(7)=  4.7258
      HT1(8)=  4.5991
      HT1(9)=  4.2139
      HT1(10)= 4.3898
      HT1(11)= 4.5457
      HT1(12)= 4.6090
      HT1(13)= 4.6897
      HT1(14)= 4.4718
      HT1(15)= 4.6171
      HT1(16)= 4.4603
      HT1(17)= 4.5084
      HT1(18)= 4.3164
      HT1(19)= 4.2378
      HT1(20)= 4.3379
      HT1(21)= 4.5991
      HT1(22)= 4.4834
      HT1(23)= 4.5697
      HT1(24)= 4.4388
      HT1(25)= 4.4522
      HT1(26)= 3.8550
      HT1(27)= 4.5128
      HT1(28)= 4.9396
      HT1(29)= 4.4207
      HT1(30)= 3.7512
      HT1(31)= 3.7301
      HT1(32)= 4.4091
      HT1(33)= 4.4772
      HT1(34)= 4.4819
      HT1(35)= 4.5959
      HT1(36)= 4.6155
      HT1(37)= 4.6155
      HT1(38)= 4.3734
      HT1(39)= 4.4009
      HT1(40)= 4.4931
      HT1(41)= 4.0151
      HT1(42)= 4.5018
      HT1(43)= 4.5018
      HT1(44)= 4.5920
      HT1(45)= 4.6892
      HT1(46)= 4.4004
      HT1(47)= 4.6067
      HT1(48)= 4.4004
      HT1(49)= 4.3609
      HT1(50)= 4.4004
      HT1(51)= 3.9678
      HT1(52)= 3.9613
      HT1(53)= 4.4330
      HT1(54)= 4.3802
      HT1(55)= 4.4334
      HT1(56)= 4.0322
      HT1(57)= 4.1352
      HT1(58)= 4.0965
      HT1(59)= 4.6355
      HT1(60)= 4.9396
      HT1(61)= 4.9396
      HT1(62)= 4.3286
      HT1(63)= 4.5463
      HT1(64)= 4.5225
      HT1(65)= 4.5142
      HT1(66)= 4.7342
      HT1(67)= 3.9365
      HT1(68)= 4.4375
      HT1(69)= 4.5710
      HT1(70)= 3.9191
      HT1(71)= 4.6135
      HT1(72)= 4.3420
      HT1(73)= 4.5577
      HT1(74)= 4.4618
      HT1(75)= 4.5202
      HT1(76)= 4.6106
      HT1(77)= 4.2496
      HT1(78)= 4.4747
      HT1(79)= 4.2959
      HT1(80)= 4.4299
      HT1(81)= 4.4911
      HT1(82)= 4.3383
      HT1(83)= 4.5820
      HT1(84)= 4.3744
      HT1(85)= 4.5992
      HT1(86)= 4.6008
      HT1(87)= 4.6238
      HT1(88)= 4.3898
      HT1(89)= 3.9392
      HT1(90)= 3.9089
C
      HT2(1)=  -6.0116
      HT2(2)=  -4.2964
      HT2(3)=  -6.0116
      HT2(4)=  -4.1019
      HT2(5)=  -6.4095
      HT2(6)=  -6.2258
      HT2(7)=  -6.7703
      HT2(8)=  -5.9111
      HT2(9)=  -4.5419
      HT2(10)= -5.7183
      HT2(11)= -6.8000
      HT2(12)= -6.1896
      HT2(13)= -6.8801
      HT2(14)= -5.0078
      HT2(15)= -6.2684
      HT2(16)= -5.0577
      HT2(17)= -6.0116
      HT2(18)= -4.0582
      HT2(19)= -4.1080
      HT2(20)= -3.8214
      HT2(21)= -6.6706
      HT2(22)= -4.5431
      HT2(23)= -5.7172
      HT2(24)= -4.0872
      HT2(25)= -4.5758
      HT2(26)= -2.6623
      HT2(27)= -4.9918
      HT2(28)= -8.1838
      HT2(29)= -5.1435
      HT2(30)= -2.5539
      HT2(31)= -2.7758
      HT2(32)= -4.8464
      HT2(33)= -4.7206
      HT2(34)= -4.5314
      HT2(35)= -6.4497
      HT2(36)= -6.2945
      HT2(37)= -6.2945
      HT2(38)= -5.3135
      HT2(39)= -5.0560
      HT2(40)= -4.6501
      HT2(41)= -4.3314
      HT2(42)= -5.6123
      HT2(43)= -5.6123
      HT2(44)= -5.1719
      HT2(45)= -4.9605
      HT2(46)= -4.7519
      HT2(47)= -5.2030
      HT2(48)= -4.7519
      HT2(49)= -4.1423
      HT2(50)= -4.7519
      HT2(51)= -3.2510
      HT2(52)= -3.1993
      HT2(53)= -4.5383
      HT2(54)= -4.7903
      HT2(55)= -4.5709
      HT2(56)= -3.0833
      HT2(57)= -3.7450
      HT2(58)= -3.9250
      HT2(59)= -5.2776
      HT2(60)= -8.1838
      HT2(61)= -8.1838
      HT2(62)= -4.0922
      HT2(63)= -5.2287
      HT2(64)= -4.9401
      HT2(65)= -5.2205
      HT2(66)= -6.2674
      HT2(67)= -4.4599
      HT2(68)= -4.6654
      HT2(69)= -6.0922
      HT2(70)= -4.3503
      HT2(71)= -5.7613
      HT2(72)= -5.1193
      HT2(73)= -4.9595
      HT2(74)= -4.8786
      HT2(75)= -4.8896
      HT2(76)= -5.4380
      HT2(77)= -4.8061
      HT2(78)= -4.8698
      HT2(79)= -5.3332
      HT2(80)= -4.9920
      HT2(81)= -5.7928
      HT2(82)= -4.5018
      HT2(83)= -5.0903
      HT2(84)= -4.5257
      HT2(85)= -7.7428
      HT2(86)= -7.2732
      HT2(87)= -7.4847
      HT2(88)= -5.7183
      HT2(89)= -3.4279
      HT2(90)= -3.0149
C----------
C  LOAD FORT BRAGG HT-DBH CONSTANTS
C----------
      IF(IFOR .EQ. 20)THEN
        ITYPE=176
        PCOM='232BQ'
        HT1(5) = 4.705
        HT2(5) = -7.904
        HT1(6) = 4.787
        HT2(6) = -8.015
        HT1(8) = 4.562
        HT2(8) = -7.314
        HT1(11) = 4.806
        HT2(11) = -9.573
        HT1(13) = 4.79
        HT2(13) = -8.5
        WRITE(JOSTND,310)
  310   FORMAT(/,T12,'ECOLOGICAL UNIT CODE CHANGED TO 232BQ FOR ',
     &  'FURTHER PROCESSING OF FORT BRAGG LOCATION.')
      ENDIF
C----------
C  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE
C---------
      IF(LFIA) THEN
        CALL FIAHEAD(JOSTND)
        WRITE(JOSTND,311) (NSP(I,1)(1:2),FIAJSP(I),I=1,MAXSP)
 311    FORMAT ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
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
