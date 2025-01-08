      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C METRIC-CR $Id$
C----------
C  THIS SUBROUTINE IS USED TO SET ALL VARIANT SPECIFIC VARIABLES,
C  AND VARIABLES DEPENDENT ON MODEL TYPE.
C
C  IT IS ALSO USED TO SET SITE INDEX VALUES FOR SPECIES
C  THAT HAVE NOT BEEN SET USING THE SITECODE KEYWORD, AND DEFAULT SDI
C  MAXIMUM VALUES FOR SPECIES THAT HAVE NOT BEEN SET USING THE SDIDEF
C  KEYWORD.
C
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
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C
C
      INCLUDE 'GGCOM.F77'
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
      INCLUDE 'METRIC.F77'
COMMONS
C----------
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*11
      INTEGER IFIASP,ERRFLAG,I,J,JJ,K,INTFOR,IREGN,ISPC
      REAL SDICON(MAXSP),DEFMT(23),TEM
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C
      DATA SDICON/
     & 735., 735., 560., 735., 735., 735., 770., 650., 470., 470.,
     & 675., 415., 529., 470., 645., 415., 735., 735., 735., 725.,
     & 470., 470., 470., 470., 470., 470., 470., 725., 415., 415.,
     & 415., 415., 415., 415., 415., 529., 470., 470./
C
      DATA DEFMT/ 5, 3, 4, 5, 3, 4, 5, 5, 4, 4, 5, 4,
     &            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
C----------
      CHARACTER LABEL*26
C----------
C  FINISH PROCESSING THE MODTYPE KEYWORD
C----------
      WRITE(JOSTND,10)
   10 FORMAT(/'MODTYPE    KEYWORD NOW BEING PROCESSED')
C----------
C  DECODE MODEL TYPE
C----------
      IF(IMODTY .EQ. 1) LABEL='SOUTHWEST MIXED CONIFERS  '
      IF(IMODTY .EQ. 2) LABEL='SOUTHWEST PONDEROSA PINE  '
      IF(IMODTY .EQ. 3) LABEL='BLACK HILLS PONDEROSA PINE'
      IF(IMODTY .EQ. 4) LABEL='SPRUCE-FIR                '
      IF(IMODTY .EQ. 5) LABEL='LODGEPOLE PINE            '
      IF(IMODTY.GT.0 .AND. IMODTY.LE.5) THEN
        WRITE (JOSTND,200) IMODTY,LABEL
 200    FORMAT(T12,'MODEL TYPE FOR THIS PROJECTION IS ',I4,'. ',A26)
      ELSE
        IMODTY=DEFMT(IFOR)
        IF(IMODTY .EQ. 1) LABEL='SOUTHWEST MIXED CONIFERS  '
        IF(IMODTY .EQ. 2) LABEL='SOUTHWEST PONDEROSA PINE  '
        IF(IMODTY .EQ. 3) LABEL='BLACK HILLS PONDEROSA PINE'
        IF(IMODTY .EQ. 4) LABEL='SPRUCE-FIR                '
        IF(IMODTY .EQ. 5) LABEL='LODGEPOLE PINE            '
        WRITE(JOSTND,14)IMODTY,LABEL
   14   FORMAT(' ',T12,'MODEL TYPE NOT RECOGNIZED. BEING SET TO',I4,
     &  '= ',A26,'  FOR FURTHER PROCESSING')
      ENDIF
C----------
C THE FOLLOWING APPLY TO ONE OR MORE MODEL TYPES, AND MAY BE
C CHANGED IN SPECIFIC MODEL TYPE SECTIONS BELOW.
C----------
C
C  FOR THIS VARIANT, SIGMAR WAS SET TO 0.2 JUST TO GET A LITTLE
C  VARIATION IN THE SYSTEM. CARL'S SE VALUES LOOKED REALLY SUSPECT.
C  EQNS FROM OTHER VARIANTS HAVE SIGMAR VALUES
C
      DO 50 I=1,MAXSP
      SIGMAR(I) = 0.2
   50 CONTINUE
      SIGMAR( 4) = 0.260
      SIGMAR( 6) = 0.3433
      SIGMAR( 7) = 0.4125
      SIGMAR( 8) = 0.4243
      SIGMAR(10) = 0.4671
      SIGMAR(14) = 0.11645
C
      BARK1( 1) = 0.890
      BARK1( 2) = 0.890
      BARK1( 3) = 0.867
      BARK1( 4) = 0.890
      BARK1( 5) = 0.890
      BARK1( 6) = 0.9497
      BARK1( 7) = 0.9497
      BARK1( 8) = 0.87407
      BARK1( 9) = 0.9625
      BARK1(10) = 0.9625
      BARK1(11) = 0.9625
      BARK1(12) = 0.8967
      BARK1(13) = 0.8967
      BARK1(14) = 0.9625
      BARK1(15) = 0.9643
      BARK1(16) = 0.8967
      BARK1(17) = 0.9502
      BARK1(18) = 0.9502
      BARK1(19) = 0.9502
      BARK1(20) = 0.950
      BARK1(21) = 0.892
      BARK1(22) = 0.892
      BARK1(23) = 0.93789
      BARK1(24) = 0.93789
      BARK1(25) = 0.93789
      BARK1(26) = 0.93789
      BARK1(27) = 0.93789
      BARK1(28) = 0.950
      BARK1(29) = 0.8967
      BARK1(30) = 0.8967
      BARK1(31) = 0.8967
      BARK1(32) = 0.8967
      BARK1(33) = 0.8967
      BARK1(34) = 0.8967
      BARK1(35) = 0.8967
      BARK1(36) = 0.8967
      BARK1(37) = 0.8967
      BARK1(38) = 0.892
C
      BARK2( 1) =  0.
      BARK2( 2) =  0.
      BARK2( 3) =  0.
      BARK2( 4) =  0.
      BARK2( 5) =  0.
      BARK2( 6) =  0.
      BARK2( 7) =  0.
      BARK2( 8) = -0.185
      BARK2( 9) = -0.1141
      BARK2(10) = -0.1141
      BARK2(11) = -0.1141
      BARK2(12) = -0.4448
      BARK2(13) = -0.4448
      BARK2(14) = -0.1141
      BARK2(15) =  0.
      BARK2(16) = -0.4448
      BARK2(17) = -0.2528
      BARK2(18) = -0.2528
      BARK2(19) = -0.2528
      BARK2(20) =  0.
      BARK2(21) = -0.086
      BARK2(22) = -0.086
      BARK2(23) = -0.24096
      BARK2(24) = -0.24096
      BARK2(25) = -0.24096
      BARK2(26) = -0.24096
      BARK2(27) = -0.24096
      BARK2(28) =  0.
      BARK2(29) = -0.4448
      BARK2(30) = -0.4448
      BARK2(31) = -0.4448
      BARK2(32) = -0.4448
      BARK2(33) = -0.4448
      BARK2(34) = -0.4448
      BARK2(35) = -0.4448
      BARK2(36) = -0.4448
      BARK2(37) = -0.4448
      BARK2(38) = -0.086
C
      DBHMAX( 1) = 36.
      DBHMAX( 2) = 36.
      DBHMAX( 3) = 50.
      DBHMAX( 4) = 20.
      DBHMAX( 5) = 40.
      DBHMAX( 6) = 20.
      DBHMAX( 7) = 20.
      DBHMAX( 8) = 20.
      DBHMAX( 9) = 36.
      DBHMAX(10) = 36.
      DBHMAX(11) = 20.
      DBHMAX(12) = 30.
      DBHMAX(13) = 50.
      DBHMAX(14) = 20.
      DBHMAX(15) = 36.
      DBHMAX(16) = 50.
      DBHMAX(17) = 40.
      DBHMAX(18) = 46.
      DBHMAX(19) = 20.
      DBHMAX(20) = 30.
      DBHMAX(21) = 36.
      DBHMAX(22) = 36.
      DBHMAX(23) = 20.
      DBHMAX(24) = 20.
      DBHMAX(25) = 20.
      DBHMAX(26) = 20.
      DBHMAX(27) = 20.
      DBHMAX(28) = 30.
      DBHMAX(29) = 50.
      DBHMAX(30) = 50.
      DBHMAX(31) = 50.
      DBHMAX(32) = 50.
      DBHMAX(33) = 30.
      DBHMAX(34) = 30.
      DBHMAX(35) = 30.
      DBHMAX(36) = 50.
      DBHMAX(37) = 20.
      DBHMAX(38) = 20.
C**********************************************************
C  DEFAULT MERCH SPECS ARE SET IN NI/VOLS
C**********************************************************
C
C DEFAULT MODEL TYPE IS 1 --- SOUTHWEST MIXED CONFIERS.  VARIANT
C SPECIFIC VARIABLES FOR THIS MODEL TYPE ARE SET IN BLKDAT AND GRINIT.
C
C******************************************************
C MODEL TYPE 1 --- SOUTHWEST MIXED CONIFERS          **
C******************************************************
C
      IF(IMODTY .EQ. 1) THEN
C----------
C LOAD GRINIT VARIABLES FOR MODEL 2
C----------
        IF(ELEV .LE. 0.) ELEV=88.
C
      ENDIF
C
C*******************************************************
C MODEL TYPE 2 -- SOUTHWEST PONDEROSA PINE.           **
C*******************************************************
C
      IF(IMODTY .EQ. 2) THEN
C----------
C LOAD BLKDAT VARIABLES FOR MODEL 2
C----------
        SIGMAR(23) = 0.043
        SIGMAR(24) = 0.043
        SIGMAR(25) = 0.043
        SIGMAR(26) = 0.043
        SIGMAR(27) = 0.043
        SIGMAR(12) = 0.181
        SIGMAR(33) = 0.181
        SIGMAR(34) = 0.181
        SIGMAR(35) = 0.181
C----------
C LOAD GRINIT VARIABLES FOR MODEL 2
C----------
        IF(ELEV .LE. 0.) ELEV=88.
C
C**************************************************
C MODEL TYPE 3 -- BLACK HILLS PONDEROSA PINE     **
C**************************************************
C
      ELSEIF(IMODTY .EQ. 3) THEN
C----------
C LOAD BLKDAT VARIABLES FOR MODEL 3
C----------
        DO 1225 I=1,MAXSP
        SIGMAR(I) = 0.346
 1225   CONTINUE
        SIGMAR(17) = 0.468
        SIGMAR(18) = 0.468
        SIGMAR(19) = 0.468
        BARK1(12) = 0.
        BARK2(12) = 0.
        BARK1(13) = 0.
        BARK2(13) = 0.
        BARK1(16) = 0.
        BARK2(16) = 0.
        BARK1(29) = 0.
        BARK2(29) = 0.
        BARK1(30) = 0.
        BARK2(30) = 0.
        BARK1(31) = 0.
        BARK2(31) = 0.
        BARK1(32) = 0.
        BARK2(32) = 0.
        BARK1(33) = 0.
        BARK2(33) = 0.
        BARK1(34) = 0.
        BARK2(34) = 0.
        BARK1(35) = 0.
        BARK2(35) = 0.
        BARK1(36) = 0.
        BARK2(36) = 0.
        BARK1(37) = 0.
        BARK2(37) = 0.
C----------
C LOAD GRINIT VARIABLES FOR MODEL 3
C----------
        TLAT=44.0
        IF(ELEV .LE. 0.) ELEV = 55.
C----------
C LOAD GGCOM VARIABLES FOR MODEL 3
C----------
        DBHMAX(1) = 20.
        DBHMAX(2) = 20.
        DBHMAX(3) = 20.
        DBHMAX(5) = 20.
        DBHMAX(9) = 20.
        DBHMAX(10) = 20.
        DBHMAX(11) = 24.
        DBHMAX(12) = 20.
        DBHMAX(13) = 32.
        DBHMAX(14) = 30.
        DBHMAX(15) = 20.
        DBHMAX(16) = 24.
        DBHMAX(17) = 24.
        DBHMAX(18) = 24.
        DBHMAX(19) = 30.
        DBHMAX(20) = 24.
        DBHMAX(21) = 48.
        DBHMAX(22) = 48.
        DBHMAX(28) = 24.
        DBHMAX(29) = 24.
        DBHMAX(30) = 24.
        DBHMAX(31) = 24.
        DBHMAX(32) = 24.
        DBHMAX(33) = 20.
        DBHMAX(34) = 20.
        DBHMAX(35) = 20.
        DBHMAX(36) = 32.
C*************************************************
C MODEL TYPE 4 -- SPRUCE-FIR                    **
C*************************************************
C
      ELSEIF(IMODTY .EQ. 4) THEN
C----------
C LOAD BLKDAT VARIABLES FOR MODEL 4
C----------
        BARK1(12) = 0.
        BARK2(12) = 0.
        BARK1(13) = 0.
        BARK2(13) = 0.
        BARK1(16) = 0.
        BARK2(16) = 0.
        BARK1(29) = 0.
        BARK2(29) = 0.
        BARK1(30) = 0.
        BARK2(30) = 0.
        BARK1(31) = 0.
        BARK2(31) = 0.
        BARK1(32) = 0.
        BARK2(32) = 0.
        BARK1(33) = 0.
        BARK2(33) = 0.
        BARK1(34) = 0.
        BARK2(34) = 0.
        BARK1(35) = 0.
        BARK2(35) = 0.
        BARK1(36) = 0.
        BARK2(36) = 0.
        BARK1(37) = 0.
        BARK2(37) = 0.
C----------
C LOAD GRINIT VARIABLES FOR MODEL 4
C----------
        TLAT=40.0
        IF(ELEV .LE. 0.) ELEV = 90.
C----------
C LOAD GGCOM VARIABLES FOR MODEL 4
C----------
        DBHMAX(1) = 28.
        DBHMAX(2) = 28.
        DBHMAX(3) = 42.
        DBHMAX(4) = 36.
        DBHMAX(5) = 30.
        DBHMAX(7) = 40.
        DBHMAX(8) = 36.
        DBHMAX(9) = 20.
        DBHMAX(11)= 36.
        DBHMAX(12)= 20.
        DBHMAX(13)= 32.
        DBHMAX(14)= 30.
        DBHMAX(17)= 36.
        DBHMAX(19)= 36.
        DBHMAX(21)= 24.
        DBHMAX(22)= 24.
        DBHMAX(33)= 20.
        DBHMAX(34)= 20.
        DBHMAX(35)= 20.
        DBHMAX(36)= 32.
C
C*************************************************
C MODEL TYPE 5 -- LODGEPOLE PINE                **
C*************************************************
C
      ELSE IF(IMODTY .EQ. 5) THEN
C----------
C LOAD BLKDAT VARIABLES FOR MODEL 5
C----------
        BARK1(12) = 0.
        BARK2(12) = 0.
        BARK1(13) = 0.
        BARK2(13) = 0.
        BARK1(16) = 0.
        BARK2(16) = 0.
        BARK1(29) = 0.
        BARK2(29) = 0.
        BARK1(30) = 0.
        BARK2(30) = 0.
        BARK1(31) = 0.
        BARK2(31) = 0.
        BARK1(32) = 0.
        BARK2(32) = 0.
        BARK1(33) = 0.
        BARK2(33) = 0.
        BARK1(34) = 0.
        BARK2(34) = 0.
        BARK1(35) = 0.
        BARK2(35) = 0.
        BARK1(36) = 0.
        BARK2(36) = 0.
        BARK1(37) = 0.
        BARK2(37) = 0.
C----------
C LOAD GRINIT VARIABLES FOR MODEL 5
C----------
        TLAT=40.
        IF(ELEV .LE. 0.) ELEV = 90.
C----------
C LOAD GGCOM VARIABLES FOR MODEL 5
C----------
        DBHMAX(1) = 28.
        DBHMAX(2) = 28.
        DBHMAX(3) = 42.
        DBHMAX(4) = 36.
        DBHMAX(5) = 30.
        DBHMAX(7) = 40.
        DBHMAX(8) = 36.
        DBHMAX(9) = 20.
        DBHMAX(11)= 36.
        DBHMAX(12)= 20.
        DBHMAX(13)= 32.
        DBHMAX(14)= 30.
        DBHMAX(17)= 36.
        DBHMAX(19)= 36.
        DBHMAX(21)= 24.
        DBHMAX(22)= 24.
        DBHMAX(33)= 20.
        DBHMAX(34)= 20.
        DBHMAX(35)= 20.
        DBHMAX(36)= 32.
      ENDIF
C----------
C IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
C WITH DEFAULT SITE VALUES.
C----------
      TEM = 70.
      IF(IMODTY .EQ. 3) TEM = 57.
      IF(IMODTY .EQ. 4) TEM = 75.
      IF(IMODTY .EQ. 5) TEM = 65.
      IF(ISISP .GT. 0 .AND. SITEAR(ISISP) .GT. 0.0) TEM=SITEAR(ISISP)
      IF(IMODTY .EQ. 1 .AND. ISISP .EQ. 0) ISISP = 3
      IF(IMODTY .EQ. 2 .AND. ISISP .EQ. 0) ISISP = 13
      IF(IMODTY .EQ. 3 .AND. ISISP .EQ. 0) ISISP = 13
      IF(IMODTY .EQ. 4 .AND. ISISP .EQ. 0) ISISP = 18
      IF(IMODTY .EQ. 5 .AND. ISISP .EQ. 0) ISISP = 11
      DO 100 I=1,MAXSP
      IF(TEM .LT. SITELO(ISISP))TEM=SITELO(ISISP)
      IF(SITEAR(I) .LE. 0.0) SITEAR(I) = SITELO(I) +
     & (TEM-SITELO(ISISP))/(SITEHI(ISISP)-SITELO(ISISP))
     & *(SITEHI(I)-SITELO(I))
  100 CONTINUE
C----------
C LOAD THE SDIDEF ARRAY
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
      WRITE(JOSTND,91)(SDIDEF(K)/ACRtoHA,K=J,JJ )
   91 FORMAT('SDI MAX ',   10F8.0)
      IF(JJ .EQ. MAXSP)GO TO 93
   92 CONTINUE
   93 CONTINUE
C----------
C  LOAD VOLUME DEFAULT MERCH. SPECS.
C----------
      IF(IMODTY.EQ.3)THEN
        DO I=1,MAXSP
        IF(DBHMIN(I) .LE. 0.) DBHMIN(I) = 9.0
        IF(TOPD(I) .LE. 0.) TOPD(I) = 6.0
        IF(BFTOPD(I) .LE. 0.0) BFTOPD(I) = 6.0
        IF(BFMIND(I) .LE. 0.0) BFMIND(I) = 9.0
        ENDDO
      ELSEIF((IMODTY.EQ.1).OR.(IMODTY.EQ.2).OR.
     &       (IMODTY.EQ.4).OR.(IMODTY.EQ.5))THEN
        DO I=1,MAXSP
        IF(DBHMIN(I) .LE. 0.) DBHMIN(I) = 5.0
        IF(TOPD(I) .LE. 0.) TOPD(I) = 4.0
        IF(BFTOPD(I) .LE. 0.0) BFTOPD(I) = 6.0
        IF(BFMIND(I) .LE. 0.0) THEN
          IF(IFOR .LT. IGFOR) THEN
            BFMIND(I) = 7.0
          ELSE
            BFMIND(I) = 9.0
          ENDIF
        ENDIF
        ENDDO
      ENDIF
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      IREGN = KODFOR/100
      DIST='  '
      PROD='  '
      VAR='CR'
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
 230  FORMAT(4(3X,A2,4X,A10,1X,A10))
C
      RETURN
      END

