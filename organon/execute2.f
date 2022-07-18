C----------
C ORGANON $Id$
C----------
C      SUBROUTINE EXECUTE(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,TREENO,
C     1                   PTNO,SPECIES,USER,INDS,DBH1,HT1,CR1,SCR1,
C     2                   EXPAN1,MGEXP,SITE_1,SITE_2,ACALIB,DFMSDI,
C     3                   TFMSDI,OCMSDI,PN,YSF,BABT,BART,YST,NPR,PRAGE,
C     4                   PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,BRHT,BRDIA,
C     5                   JCORE,SERROR,TERROR,SWARNING,TWARNING,IERROR,
C     6                   DGRO,HGRO,CRCHNG,SCRCHNG,MORTEXP,STOR)
C
C     REDIMENSIONED INDS(30) AND STOR(30) AND ADDED RVARS(30) WHICH INCLUDES
C     SITE_1, SITE_2, DFMSDI, TFMSDI, OCMSDI, PDEN
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE CKAGE TO CKAGE_RUN
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - EDIT TO EDIT_RUN
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_RUN
C
      SUBROUTINE EXECUTE(CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO,
     1                   PTNO,SPECIES,USER,INDS,DBH1,HT1,CR1,SCR1,
     2                   EXPAN1,MGEXP,RVARS,ACALIB,PN,YSF,BABT,BART,YST,
     3                   NPR,PRAGE,PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,
     4                   BRHT,BRDIA,JCORE,SERROR,TERROR,SWARNING,
     5                   TWARNING,IERROR,DGRO,HGRO,CRCHNG,SCRCHNG,
     6                   MORTEXP,NTREES2,DBH2,HT2,CR2,SCR2,EXPAN2,STOR,
     7                   OCC2,OAHT2)
      IMPLICIT NONE
      INTEGER*4 CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,
     1          TREENO(2000),PTNO(2000),SPECIES(2000),USER(2000),
     2          PRAGE(2000,3),BRCNT(2000,3),BRHT(2000,40),
     3          BRDIA(2000,40),JCORE(2000,40),NPR(2000),SERROR(35),
     4          TERROR(2000,6),SWARNING(9),TWARNING(2000)
      INTEGER*4 NXT,BNXT,ONXT,IB,BIG6,OTHER,NSPN,TDATAI(2000,3),
     1          SPGRP(2000),TCYCLE,FCYCLE
      INTEGER*4 IERROR,YCYCLG,NTREES
      INTEGER*4 INDS(30)
C*****START NEW*****
      INTEGER*4 NTREES2
C*****END NEW*****
      REAL*4 DBH1(2000),HT1(2000),CR1(2000),EXPAN1(2000),
     1       SCR1(2000),SITE_1,SITE_2,ACALIB(3,18),MSDI_1,MSDI_2,MSDI_3,
     2       PDEN,PN(5),YSF(5),BABT,BART(5),YST(5),PRLH(2000,3),
     3       PRDBH(2000,3),PRHT(2000,3),PRCR(2000,3),PREXP(2000,3),OLD,
     4       MGEXP(2000),DGRO(2000),HGRO(2000),MORTEXP(2000),
     5       CRCHNG(2000),SCRCHNG(2000),NO,RD0,STOR(30),RVARS(30)
      REAL*4 TDATAR(2000,8),SBA1,BALL1(51),BAL1(500),GROWTH(2000,4),
     1       YF(5),YT(5),SCR(2000,3),VOLTR(2000,4),DEADEXP(2000),
     2       SYTVOL(2000,2),CCH(41),A1,A2,A1MAX,PA1MAX,RAAGE,SI_1,SI_2,
     3       RASI,CCFLL1(51),CCFL1(500),SBA2,CCFLL2(51),CCFL2(500),
     4       BALL2(51),BAL2(500),TPA2,SCCF2,CALIB(6,18),CC,X,MAXRAH,
     5       CON_RASI,SCCF1,TPA1,CON,QMD1,RD1,HT40,CCMAX,AHCB,SHCB
C*****START NEW*****
      REAL*4 DBH2(2000),HT2(2000),CR2(2000),EXPAN2(2000),SCR2(2000)
      REAL*4 GWDG,GWHG,FR,OCC2,OAHT2
C*****END NEW*****
      LOGICAL*2 CALH,CALC,CALD,EVEN,TRIPLE,POST,PRUNE,THIN,FERT,MORT,
     1          ERROR
      LOGICAL*2 WOODQ,TRIAL
      LOGICAL*2 OSTORY,INGRO,B6THIN
C*****START NEW*****
      LOGICAL*2 GENETICS,SWISSNC
C*****END NEW*****
      INTEGER*4 I,II,J,IDXAGE,IDXCYC
C      INTEGER*4 IANS,IYN
C
C     SET THE MAXIMUM TREE LEVEL ARRAY SIZES
C
      NTREES=NTREES1
      YCYCLG=5*CYCLG
      CALH =   .FALSE.
      CALC =   .FALSE.
      CALD =   .FALSE.
      EVEN =   .FALSE.
      TRIPLE = .FALSE.
      PRUNE =  .FALSE.
      THIN =   .FALSE.
      FERT =   .FALSE.
      MORT =   .FALSE.
      POST =   .FALSE.
      OSTORY = .FALSE.
      INGRO =  .FALSE.
      B6THIN = .FALSE.
C*****START NEW*****
      GENETICS = .FALSE.
      SWISSNC =  .FALSE.
C*****END NEW******
      IF(INDS(1) .EQ. 1) THEN
         CALH = .TRUE.
      ENDIF
      IF(INDS(2) .EQ. 1) THEN
         CALC = .TRUE.
      ENDIF
      IF(INDS(3) .EQ. 1) THEN
         CALD = .TRUE.
      ENDIF
      IF(INDS(4) .EQ. 1) THEN
         EVEN = .TRUE.
      ENDIF
      IF(INDS(5) .EQ. 1) THEN
         TRIPLE = .TRUE.
      ENDIF
      IF(INDS(6) .EQ. 1) THEN
         PRUNE = .TRUE.
      ENDIF
      IF(INDS(7) .EQ. 1) THEN
         THIN = .TRUE.
      ENDIF
      IF(INDS(8) .EQ. 1) THEN
         FERT = .TRUE.
      ENDIF
      IF(INDS(9) .EQ. 1) THEN
         MORT = .TRUE.
      ENDIF
      IF(INDS(11) .EQ. 1) THEN
         OSTORY = .TRUE.
      ENDIF
      IF(INDS(12) .EQ. 1) THEN
         INGRO = .TRUE.
      ENDIF
      IF(INDS(13) .EQ. 1) THEN
         B6THIN = .TRUE.
      ENDIF
C*****START NEW*****
      IF(INDS(14) .EQ. 1) THEN
         GENETICS = .TRUE.
      ENDIF
      IF(INDS(15) .EQ. 1) THEN
         SWISSNC = .TRUE.
      ENDIF
C*****END NEW******
      SITE_1=RVARS(1)
      SITE_2=RVARS(2)
      MSDI_1=RVARS(3)
      MSDI_2=RVARS(4)
      MSDI_3=RVARS(5)
C*****START NEW*****
      GWDG=RVARS(6)
      GWHG=RVARS(7)
      FR=RVARS(8)
C*****END NEW******
      PDEN=RVARS(9)
      NO=STOR(1)
      RD0=STOR(2)
      A1=STOR(3)
      A2=STOR(4)
      A1MAX=STOR(5)
      PA1MAX=STOR(6)
C      CALL EDIT(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
C     1          CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,DBH1,HT1,CR1,
C     2          EXPAN1,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,ACALIB,
C     3          PN,YSF,BABT,BART,YST,SCR1,MGEXP,IB,NSPN,BIG6,OTHER,BNXT,
C     4          ONXT,SPGRP,SERROR,TERROR,SWARNING,TWARNING,ERROR)
C*****START NEW*****
      CALL EDIT_RUN(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,CALH,
     1       CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,GENETICS,SWISSNC,DBH1,
     2          HT1,CR1,EXPAN1,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,
     3          ACALIB,PN,YSF,BABT,BART,YST,SCR1,MGEXP,GWDG,GWHG,FR,IB,
     4          NSPN,BIG6,OTHER,BNXT,ONXT,SPGRP,SERROR,TERROR,SWARNING,
     5          TWARNING,ERROR)
C*****END NEW******
      IERROR =   0
      IF(ERROR) THEN
         IERROR = 1
         RETURN
      ENDIF
      NXT=BNXT+ONXT
      IF(VERSION .EQ. 1) THEN
         IDXAGE=500
         IDXCYC=100
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3) THEN
         IDXAGE=120
         IDXCYC=24
      ELSE
         IDXAGE=30
         IDXCYC=30
      ENDIF
      FCYCLE = 0
      TCYCLE = 0
      IF(FERT .AND. YSF(1) .EQ. FLOAT(YCYCLG)) FCYCLE = 1
      IF(THIN .AND. YST(1) .EQ. FLOAT(YCYCLG)) THEN
         TCYCLE = 1
         POST=.TRUE.
      ENDIF
      WOODQ=.FALSE.
      IF(INDS(10) .EQ. 1) THEN
         WOODQ=.TRUE.
      ENDIF
      TRIAL=.FALSE.
      DO I=1,2000
        GROWTH(I,3)=0.0
        GROWTH(I,4)=0.0
        TDATAI(I,1)=0
        TDATAI(I,2)=0
        TDATAI(I,3)=0
        TDATAR(I,1)=0.0
        TDATAR(I,2)=0.0
        TDATAR(I,3)=0.0
        TDATAR(I,4)=0.0
        SCR(I,1)=0.0
        SCR(I,2)=0.0
        SCR(I,3)=0.0
        DEADEXP(I)=0.0
        DBH2(I)=0.0
        HT2(I)=0.0
        CR2(I)=0.0
        EXPAN2(I)=0.0
        SCR2(I)=0.0
        DO II=1,2
           SYTVOL(I,II)=0.0
        ENDDO
        DO II=1,4
           VOLTR(I,II)=0.0
        ENDDO
      ENDDO
      DO I=1,NTREES
        TDATAI(I,1)=SPECIES(I)
        TDATAI(I,2)=SPGRP(I)
C        TDATAI(I,2)=1
        TDATAI(I,3)=USER(I)
        TDATAR(I,1)=DBH1(I)
        TDATAR(I,2)=HT1(I)
        TDATAR(I,3)=CR1(I)
        TDATAR(I,4)=EXPAN1(I)
        SCR(I,1)=SCR1(I)
      ENDDO
      DO I=1,18
         CALIB(4,I)=ACALIB(1,I)
         CALIB(5,I)=ACALIB(2,I)
         CALIB(6,I)=ACALIB(3,I)
         IF(CALH)THEN
            CALIB(1,I)=(1.0+CALIB(4,I))/2.0 + (0.5**(0.5*(CYCLG)))
     1                 *((CALIB(4,I)-1.0)/2.0)
         ELSE
            CALIB(1,I)=1.0
         ENDIF
         IF(CALC)THEN
            CALIB(2,I)=(1.0+CALIB(5,I))/2.0 + (0.5**(0.5*(CYCLG)))
     1                 *((CALIB(5,I)-1.0)/2.0)
         ELSE
            CALIB(2,I)=1.0
         ENDIF
         IF(CALD)THEN
            CALIB(3,I)=(1.0+CALIB(6,I))/2.0 + (0.5**(0.5*(CYCLG)))
     1                 *((CALIB(6,I)-1.0)/2.0)
         ELSE
            CALIB(3,I)=1.0
         ENDIF
      ENDDO
      DO I=1,5
         YF(I)=YSF(I)
         YT(I)=YST(I)
      ENDDO
CC
CC     CALCULATE SPECIES GROUP
CC
C      CALL SPGROUP(VERSION,NSPN,NTREES,TDATAI)
      IF(VERSION .EQ. 1)THEN
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_1=1.062934*SITE_2
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_2=0.940792*SITE_1
        ENDIF
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)THEN
C  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_1=0.480 +( 1.110 * SITE_2)
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_2=-0.432 +( 0.899 * SITE_1)
        ENDIF
      ELSE
        IF(SITE_2 .LE. 0.0) THEN
          SITE_2=4.776377*SITE_1**0.763530587
        ENDIF
      ENDIF
C
C     CHANGE TO EXPANSION FACTOR FOR STAND--NOT SAMPLE
C
      DO J=1,NTREES
        TDATAR(J,4)=TDATAR(J,4)/FLOAT(NPTS)
        MGEXP(J)=MGEXP(J)/FLOAT(NPTS)
        TDATAR(J,5)=TDATAR(J,4)
        IF(PRUNE) THEN
          TDATAR(J,6)=SCR(J,1)
        ELSE
          TDATAR(J,6)=TDATAR(J,3)
        ENDIF
        IF(SCR(J,1) .GT. 0.0) THEN
          SCR(J,2)=SCR(J,1)
        ENDIF
      ENDDO
C
C     CALCULATE RED ALDER SITE INDEX FOR TREES IN NATURAL STANDS
C
      MAXRAH=0.0
      IF(VERSION .LE. 3) THEN
         DO J=1,NTREES
           IF(TDATAI(J,1) .EQ. 351 .AND. TDATAR(J,2) .GT. MAXRAH) THEN
             MAXRAH=TDATAR(J,2)
           ENDIF
         ENDDO
         RASI=CON_RASI(SITE_1)
      ENDIF
C
C     CALCULATE RED ALDER AGE FOR NATURAL STANDS
C
      RAAGE=0.0
      IF(MAXRAH .GT. 0.0) THEN
         CALL RAGEA(MAXRAH,RASI,RAAGE)
         IF(RAAGE .LE. 0.0) THEN
            RAAGE=55.0
            CALL RASITE(MAXRAH,RAAGE,RASI)
         ENDIF
         IF ( RAAGE .GT. 55.0 ) THEN
            RAAGE = 55.0
         ENDIF
      ENDIF
      SI_1=SITE_1-4.5
      SI_2=SITE_2-4.5
C
C     INITIALIZE A1 AND A2
C
      IF(CYCLG .LE. 0 .OR. A1 .EQ. 0.0 .OR. A2 .EQ. 0.0
     &  .OR. OSTORY .OR. INGRO .OR. B6THIN) THEN
         CALL SUBMAX(TRIAL,VERSION,NTREES,TDATAI,TDATAR,MGEXP,MSDI_1,
     1               MSDI_2,MSDI_3,A1,A2)
      ENDIF
C
C     CALCULATE DENSITY VARIABLES AT SOG
C
      CALL SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA1,TPA1,SCCF1,
     1            BAL1,BALL1,CCFL1,CCFLL1)
      CON=0.005454154
      QMD1=SQRT(SBA1/(CON*TPA1))
      RD1=TPA1/EXP(A1/A2-LOG(QMD1)/A2)
C
C     CALCULATE H40 AND MAXIMUM CROWN CLOSURE AT SOG
C
      CALL HTFORTY(0.0,VERSION,IB,NTREES,TDATAI,TDATAR,MGEXP,HT40)
      IF(HT40 .LT. 140.0) THEN
         CCMAX=100.
      ELSE
         CCMAX=121.0-0.15*HT40
      ENDIF
      IF(RD1. LT. 0.4) THEN
         CCMAX=CCMAX*0.6
      ELSE IF(RD1 .GE. 0.4 .AND. RD1 .LT. 0.8) THEN
         CCMAX=CCMAX*(0.2+RD1)
      ENDIF
C
C     CALCULATE CCH AND CROWN CLOSURE AT SOG
C
      CALL CRNCLO(0,0.0,VERSION,NTREES,TDATAI,TDATAR,SCR,MGEXP,
     1            CCH,CC)
C
C     SET VARIABLES NEEDED BY THE EVENT MONITOR: LOCALLY OCC2 AND OAHT2
C
      OCC2 = 100.0 * (1.0 - EXP(-0.01*CC)) 
      OAHT2 = HT40
C
      OLD=0.0
      DO I=1,NTREES
        TDATAR(I,8)=TDATAR(I,4)
        IF(PRUNE) THEN
          TDATAR(I,7)=SCR(I,1)
        ELSE
          TDATAR(I,7)=TDATAR(I,3)
        ENDIF
        IF(SCR(I,1) .GT. 0.0) THEN
          SCR(I,3)=SCR(I,1)
        ENDIF
      ENDDO
C      CALL GROW(VERSION,CYCLG,NTREES,NXT,BNXT,ONXT,IB,BIG6,
C     1          OTHER,NSPN,STAGE,BHAGE,PTNO,TREENO,TDATAI,PRAGE,BRCNT,
C     2          BRHT,BRDIA,JCORE,NPR,TCYCLE,FCYCLE,TRIPLE,WOODQ,POST,
C     3          MORT,TDATAR,SI_1,SI_2,SBA1,BALL1,BAL1,CALIB,PN,YF,
C     4          BABT,BART,YT,GROWTH,PRLH,PRDBH,PRHT,PRCR,PREXP,SCR,
C     5          VOLTR,SYTVOL,CCH,OLD,MGEXP,DEADEXP,A1,A2,A1MAX,PA1MAX,
C     6          NO,RD0,RAAGE,RASI,CCFLL1,CCFL1,SBA2,CCFLL2,CCFL2,
C     7          BALL2,BAL2,TPA2,SCCF2)
      CALL GROW(VERSION,CYCLG,NTREES,IB,BIG6,OTHER,NSPN,STAGE,BHAGE,
     1          PTNO,TREENO,TDATAI,PRAGE,BRCNT,BRHT,BRDIA,JCORE,NPR,
     2          TCYCLE,FCYCLE,TRIPLE,WOODQ,POST,MORT,GENETICS,SWISSNC,
     3          TDATAR,SI_1,SI_2,SBA1,BALL1,BAL1,CALIB,PN,YF,BABT,BART,
     4          YT,GROWTH,PRLH,PRDBH,PRHT,PRCR,PREXP,SCR,VOLTR,SYTVOL,
     5          CCH,OLD,MGEXP,DEADEXP,A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,
     6          RASI,CCFLL1,CCFL1,SBA2,CCFLL2,CCFL2,BALL2,BAL2,TPA2,
     7          SCCF2,GWDG,GWHG,FR,PDEN)
      NTREES2=NTREES
      POST=.FALSE.
C      IF(.NOT. EVEN) THEN
C         BHAGE=0
C         STAGE=0
C      ENDIF
      IF(INDS(4) .EQ. 0) THEN
         BHAGE=0
         STAGE=0
      ENDIF
      X=100.0*(OLD/(BIG6-BNXT))
      IF(X .GE. 50.0)THEN
         SWARNING(7) = 1
      ENDIF
      IF(VERSION .EQ. 1) THEN
         IF(EVEN .AND. BHAGE .GT. 500.)THEN
            SWARNING(7) = 1
         ENDIF
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)THEN
         IF(EVEN .AND. BHAGE .GT. 120.)THEN
            SWARNING(7) = 1
         ENDIF
      ELSE
         IF(EVEN .AND. STAGE .GT. 30.)THEN
            SWARNING(7) = 1
         ENDIF
      ENDIF
      DO I=1,NTREES2
        SPECIES(I)=TDATAI(I,1)
        USER(I)=TDATAI(I,3)
        MGEXP(I)=0.0
        DBH2(I)=TDATAR(I,1)
        HT2(I)=TDATAR(I,2)
        CR2(I)=TDATAR(I,3)
        EXPAN2(I)=TDATAR(I,4)*FLOAT(NPTS)
        SCR2(I)=SCR(I,1)
        HGRO(I)=GROWTH(I,1)
        DGRO(I)=GROWTH(I,2)
        MORTEXP(I)=DEADEXP(I)*FLOAT(NPTS)
        AHCB=(1.0-TDATAR(I,3))*TDATAR(I,2)
        SHCB=(1.0-SCR(I,1))*TDATAR(I,2)
        IF(AHCB .GT. SHCB) THEN
           CRCHNG(I)=0.0
           SCRCHNG(I)=SCR(I,1)-TDATAR(I,7)
        ELSE
           CRCHNG(I)=TDATAR(I,3)-TDATAR(I,7)
           SCRCHNG(I)=0.0
        ENDIF
      ENDDO
      STOR(1)=NO
      STOR(2)=RD0
      STOR(3)=A1
      STOR(4)=A2
      STOR(5)=A1MAX
      STOR(6)=PA1MAX
      RETURN
      END
C***********************************************************************
C      SUBROUTINE EDIT(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,
C     1                SPECIES,CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,
C     2                DBH,HT,CR,EXPAN,SITE_1,SITE_2,MSDI_1,MSDI_2,
C     3                MSDI_3,PDEN,ACALIB,PN,YSF,BABT,BART,YST,SCR,MGEXP,
C     4                IB,NSPN,BIG6,OTHER,BNXT,ONXT,SPGRP,SERROR,TERROR,
C     5                SWARNING,TWARNING,ERROR)
C*****START NEW*****
      SUBROUTINE EDIT_RUN(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
     1                CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,GENETICS,
     2                SWISSNC,DBH,HT,CR,EXPAN,SITE_1,SITE_2,MSDI_1,
     3                MSDI_2,MSDI_3,PDEN,ACALIB,PN,YSF,BABT,BART,YST,
     4                SCR,MGEXP,GWDG,GWHG,FR,IB,NSPN,BIG6,OTHER,BNXT,
     5                ONXT,SPGRP,SERROR,TERROR,SWARNING,TWARNING,ERROR)
C*****END NEW******
      IMPLICIT NONE
      INTEGER*4 CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,
     1          SPECIES(2000),IB,NSPN,BIG6,OTHER,BNXT,ONXT,IIB
      INTEGER*4 I,J,SPGRP(2000),SERROR(35),TERROR(2000,6),SWARNING(9),
     1          TWARNING(2000),EXCAGE,YCYCLG
      REAL*4 DBH(2000),HT(2000),CR(2000),EXPAN(2000),SITE_1,SITE_2,
     1       MSDI_1,MSDI_2,MSDI_3,PDEN,ACALIB(3,18),PN(5),YSF(5),BABT,
     2       BART(5),YST(5),SCR(2000),MGEXP(2000),MAXDF,MAXGF,MAXWH,
     3       MAXPP,MAXIC,MAXHT,MAXRA,ICSI
      REAL*4 BA,SBA,B6SBA,HWSBA,B0,B1,PHT,OLD,Z50,X,PRA
C*****START NEW*****
      REAL*4 GWDG,GWHG,FR
C*****END NEW*****
      LOGICAL*2 CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,ERROR
C*****START NEW*****
      LOGICAL*2 GENETICS,SWISSNC
C*****END NEW*****
      LOGICAL*2 LDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      LDANUW = CALC
      LDANUW = CALD
      LDANUW = CALH
      LDANUW = MORT
      LDANUW = PRUNE
C
      ERROR = .FALSE.
      YCYCLG=5*CYCLG
      BIG6=0
      OTHER=0
      BNXT=0
      ONXT=0
      MAXGF = 0.0
      MAXDF = 0.0
      MAXWH = 0.0
      MAXPP = 0.0
      MAXIC = 0.0
      MAXRA = 0.0
      DO I=1,2000
         DO J=1,6
            TERROR(I,J)=0
         ENDDO
         TWARNING(I)=0
      ENDDO
      DO I=1,35
         SERROR(I)=0
      ENDDO
      DO I=1,9
         SWARNING(I)=0
      ENDDO
      IF(NTREES .LT. 1 .OR. NTREES .GT. 2000) THEN
         SERROR(1) = 1
      ENDIF
      IF(VERSION .LT. 1 .OR. VERSION .GT. 4) THEN
         SERROR(2) = 1
      ENDIF
      IF(NPTS .LE. 0) THEN
         SERROR(3) = 1
      ENDIF
      IF(SITE_1 .LE. 0.0 .AND. SITE_2 .LE. 0.0)THEN
          SERROR(4) = 1
      ENDIF
      IF(EVEN .AND. BHAGE .LE. 0) THEN
         SERROR(6) = 1
      ENDIF
      IF(.NOT. EVEN .AND. BHAGE .GT. 0) THEN
         SERROR(7) = 1
      ENDIF
      IF(EVEN .AND. (STAGE-BHAGE) .LT. 1) THEN
         SERROR(8) = 1
      ENDIF
      IF(.NOT. EVEN .AND. FERT) THEN
         SERROR(9) = 1
      ENDIF
      DO I=1,5
         IF(.NOT. FERT .AND. (YSF(I) .NE. 0 .OR. PN(I) .NE. 0)) THEN
            SERROR(10) = 1
         ENDIF
         IF(FERT) THEN
               IF(YSF(I). GT. STAGE .OR. YSF(I) .GT. 70.0) THEN
                  SERROR(11) = 1
               ENDIF
               IF(I .EQ. 1) THEN
                  IF(PN(I) .LE. 0.0 .OR. PN(I) .GT. 400.0) THEN
                     SERROR(12) = 1
                  ENDIF
               ELSE
                  IF(PN(I) .GT. 400.0) THEN
                     SERROR(12) = 1
                  ENDIF
               ENDIF
         ENDIF
      ENDDO
      IF(THIN .AND. BART(1) .GE. BABT) THEN
         SERROR(13) = 1
      ENDIF
      DO I=1,5
         IF(.NOT. THIN .AND. (YST(I) .NE. 0 .OR. BART(I) .NE. 0)) THEN
            SERROR(14) = 1
         ENDIF
         IF(THIN) THEN
               IF(EVEN .AND. YST(I). GT. STAGE) THEN
                  SERROR(15) = 1
               ENDIF
               IF(I .GT.1) THEN
C                  IF(YST(I) .GT. 0 .AND. BART(I) .LE. 0.0) THEN
                  IF(YST(I) .NE. 0 .AND. BART(I) .LE. 0.0) THEN
                     SERROR(16) = 1
                  ENDIF
               ENDIF
               IF(BABT .LE. 0.0) THEN
                  SERROR(17) = 1
               ENDIF
         ENDIF
      ENDDO
C      IF(THIN .AND. YST(1) .EQ. 0.0) THEN
      IF(THIN .AND. YST(1) .EQ. YCYCLG) THEN
         SERROR(18) = 1
         DO I=1,NTREES
            IF(MGEXP(I) .GT. 0.0) SERROR(18) = 0
         ENDDO
      ENDIF
      IF(CYCLG .LT. 0) THEN
         SERROR(19) = 1
      ENDIF
      DO I=1,3
         DO J=1,18
            IF(ACALIB(I,J) .GT. 2.0 .OR. ACALIB(I,J) .LT. 0.5) THEN
               SERROR(20) = 1
            ENDIF
         ENDDO
      ENDDO
      IF(MSDI_1 .GT. 1000.0 .OR. MSDI_2 .GT. 1000.0 .OR. MSDI_3 .GT.
     1   1000.0) THEN
         SERROR(21) = 1
      ENDIF

C*****START NEW*****
      IF(GENETICS) THEN
         IF(.NOT. EVEN) THEN
            SERROR(22) = 1
         ENDIF
         IF(GWDG .LT. 0.0 .OR. GWHG .LT. 0.0) THEN
            SERROR(23) = 1
         ENDIF
         IF(GWDG .GT. 20.0 .OR. GWHG .GT. 20.0) THEN
            SERROR(24) = 1
         ENDIF
      ELSE
         IF(GWDG .GT. 0.0 .OR. GWHG .GT. 0.0) THEN
            SERROR(25) = 1
         ENDIF
      ENDIF
      IF(SWISSNC) THEN
         IF(VERSION .EQ. 1 .OR. VERSION .EQ. 4) THEN
            SERROR(26) = 1
         ENDIF
         IF(.NOT. EVEN) THEN
            SERROR(27) = 1
         ENDIF
         IF(FR .LT. 0.85) THEN
            SERROR(28) = 1
         ENDIF
         IF(FR .GT. 7.0) THEN
            SERROR(29) = 1
         ENDIF
         IF(FERT .AND. FR .LT. 3.0) THEN
            SERROR(30) = 1
         ENDIF
      ELSE
         IF(FR .GT. 0.0) THEN
            SERROR(31) = 1
         ENDIF
      ENDIF
C*****END NEW*****

      IF(VERSION .GE. 4 .AND. SITE_1 .LE. 0.0)THEN
          SERROR(32) = 1
      ENDIF
      IF(VERSION .GE. 4 .AND. PDEN .LE. 0.0)THEN
          SERROR(33) = 1
      ENDIF
      IF(.NOT. EVEN .AND. VERSION .GE. 4) THEN
         SERROR(34) = 1
      ENDIF
      DO I=1,34
         IF(SERROR(I) .EQ. 1) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDDO
      SELECT CASE(VERSION)
        CASE(1)
          IB=5
          NSPN=18
        CASE(2,3)
          IB=3
          NSPN=11
        CASE(4)
          IB=3
          NSPN=7
      ENDSELECT
C
C     EDIT TREE RECORDS FOR ERRORS
C
      DO I=1,NTREES
         CALL CKSP_RUN(VERSION,I,SPECIES,TERROR)
         IF(DBH(I) .LE. 0.09)THEN
            TERROR(I,2) = 1
         ENDIF
         IF(HT(I) .LE. 4.5)THEN
            TERROR(I,3) = 1
         ENDIF
         IF(CR(I) .LE. 0.0 .OR. CR(I) .GT. 1.0)THEN
            TERROR(I,4) = 1
         ENDIF
         IF(EXPAN(I) .LT. 0.0)THEN
            TERROR(I,5) = 1
         ENDIF
         IF(SCR(I) .LT. 0.0 .OR. SCR(I) .GT. 1.0)THEN
            TERROR(I,6) = 1
         ENDIF
      ENDDO
      DO I=1,NTREES
         DO J=1,6
            IF(TERROR(I,J) .EQ. 1) THEN
               ERROR = .TRUE.
               RETURN
            ENDIF
         ENDDO
      ENDDO
      DO I=1,NTREES
         SELECT CASE(VERSION)
           CASE(1)           !  SWO  BIG SIX
             IF(SPECIES(I) .EQ. 122 .AND. HT(I) .GT. MAXPP)THEN
               MAXPP=HT(I)
             ELSEIF(SPECIES(I) .EQ. 81 .AND. HT(I) .GT. MAXIC)THEN
               MAXIC=HT(I)
             ELSEIF(SPECIES(I) .EQ. 202 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 15 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 17 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 117 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ENDIF
           CASE(2,3)
             IF(SPECIES(I) .EQ. 17 .AND. HT(I) .GT. MAXGF)THEN
               MAXGF=HT(I)
             ELSEIF(SPECIES(I).EQ.202 .AND. HT(I) .GT. MAXDF)THEN
               MAXDF=HT(I)
             ELSEIF(SPECIES(I) .EQ. 263 .AND. HT(I) .GT. MAXWH)THEN
               MAXWH=HT(I)
             ENDIF
           CASE(4)
             IF(SPECIES(I) .EQ. 351 .AND. HT(I) .GT. MAXRA)THEN
               MAXRA=HT(I)
             ENDIF
         ENDSELECT
         CALL SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
         IIB=IB
         IF(VERSION .GE. 4) IIB=1
         IF(SPGRP(I).LE.IIB) THEN
            BIG6=BIG6+1
            IF(EXPAN(I) .LE. 0.0) THEN
               BNXT=BNXT+1
            ENDIF
         ELSE
            OTHER=OTHER+1
            IF(EXPAN(I) .LE. 0.0) THEN
               ONXT=ONXT+1
            ENDIF
         ENDIF
      ENDDO
C
C     DETERMINE IF SPECIES MIX CORRECT FOR STAND AGE
C
      SBA=0.0
      B6SBA=0.0
      HWSBA=0.0
      DO I=1,NTREES
        IF(EXPAN(I) .LE. 0.0) CYCLE
        BA=DBH(I)**2*EXPAN(I)
        SBA=SBA+BA
        IF(SPGRP(I) .LE. IIB)B6SBA=B6SBA+BA
        IF(VERSION .EQ. 1) THEN
           IF(SPECIES(I) .EQ. 361 .OR. SPECIES(I) .EQ. 431 .OR.
     1        SPECIES(I) .EQ. 818) THEN
              HWSBA=HWSBA+BA
           ENDIF
        ENDIF
      ENDDO
      SBA=SBA*.005454154/FLOAT(NPTS)
      B6SBA=B6SBA*.005454154/FLOAT(NPTS)
      IF(B6SBA .LE. 0) THEN
         SERROR(5) = 1
         ERROR = .TRUE.
         RETURN
      ENDIF
      IF(VERSION .GE. 4) THEN
         IF(SBA .GT. 0.0) THEN
            PRA=B6SBA/SBA
         ELSE
            PRA=0.0
         ENDIF
         IF(PRA .LT. 0.9) THEN
            SERROR(35) = 1
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDIF
C
C     DETERMINE WARNINGS (IF ANY)
C
      SELECT CASE (VERSION)
         CASE(1)
            IF((SITE_1 .GT. 0.0) .AND. (SITE_1 .LT. 40.0 .OR. SITE_1
     1          .GT. 150.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SITE_2 .GT. 0.0) .AND. (SITE_2 .LT. 50.0 .OR. SITE_2
     1          .GT. 140.0))THEN
               SWARNING(2) = 1
            ENDIF
         CASE(2,3)
            IF((SITE_1 .GT. 0.0) .AND. (SITE_1 .LT. 90.0 .OR. SITE_1
     1          .GT. 142.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SITE_2 .GT. 0.0) .AND. (SITE_2 .LT. 90.0 .OR. SITE_2
     1          .GT. 142.0))THEN
               SWARNING(2) = 1
            ENDIF
         CASE(4)
            IF(SITE_1 .LT. 20.0 .OR. SITE_1 .GT. 125.0)THEN
               SWARNING(1) = 1
            ENDIF
            IF((SITE_2 .GT. 0.0) .AND. (SITE_2 .LT. 90.0 .OR. SITE_2
     1          .GT. 142.0))THEN
               SWARNING(2) = 1
            ENDIF
      ENDSELECT
      SELECT CASE(VERSION)
        CASE(1)
          IF(MAXPP .GT. 0.0)THEN
             MAXHT=(SITE_2-4.5)*(1.0/(1.-EXP(-0.164985*(SITE_2-4.5)
     1              **0.288169)))+4.5
             IF(MAXPP .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXIC .GT. 0.0)THEN
             ICSI=(0.66*SITE_1)-4.5
             MAXHT=ICSI*(1.0/(1.-EXP(-0.174929*ICSI**0.281176)))+4.5
             IF(MAXIC .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXDF .GT. 0.0) THEN
             MAXHT=(SITE_1-4.5)*(1.0/(1.0 -
     1             EXP(-0.174929*(SITE_1-4.5)**0.281176)))+4.5
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
        CASE(2,3)
          IF(MAXDF .GT. 0.0)THEN
             Z50=2500.0/(SITE_1-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXGF .GT. 0.0)THEN
             Z50=2500.0/(SITE_1-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXGF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXWH .GT. 0.0)THEN
             Z50=2500.0/(SITE_2-4.5)
             MAXHT=4.5+1.0/(0.00192+0.00007*Z50)
             IF(MAXWH .GT. MAXHT) SWARNING(3) = 1
          ENDIF
        CASE(4)
          IF(MAXRA .GT. 0.0)THEN
             CALL WHHLB_H40_RUN(SITE_1,20.0,150.0,MAXHT)
             IF(MAXRA .GT. MAXHT) SWARNING(3) = 1
          ENDIF
      ENDSELECT
      IF(EVEN .AND. VERSION .LE. 3 .AND. BHAGE .LT. 10)THEN
         SWARNING(4) = 1
      ENDIF
      IF((VERSION .EQ. 1 .AND. (B6SBA + HWSBA) .LT. SBA*.2).OR.
     1   (VERSION .EQ. 2 .AND. (B6SBA + HWSBA) .LT. SBA*.5).OR.
     2   (VERSION .EQ. 3 .AND. (B6SBA + HWSBA) .LT. SBA*.5).OR.
     3   (VERSION .EQ. 4 .AND. (B6SBA + HWSBA) .LT. SBA*.8))THEN
         SWARNING(5) = 1
      ENDIF
      IF(NTREES .LT. 50)THEN
         SWARNING(6) = 1
      ENDIF
      CALL CKAGE(VERSION,NTREES,IB,SPGRP,PDEN,SITE_1,SITE_2,HT,OLD)
      X=100.0*(OLD/(BIG6-BNXT))
      IF(X .GE. 50.0) SWARNING(7) = 1
      IF(VERSION .EQ. 1) THEN
         IF(EVEN .AND. BHAGE .GT. 500.) SWARNING(8) = 1
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3) THEN
         IF(EVEN .AND. BHAGE .GT. 120.) SWARNING(8) = 1
      ELSE
         IF(EVEN .AND. STAGE .GT. 30.) SWARNING(8) = 1
      ENDIF
      IF(EVEN) THEN
         SELECT CASE(VERSION)
            CASE(1)
               EXCAGE=500-STAGE-5
            CASE(2,3)
               EXCAGE=120-STAGE-5
            CASE(4)
               EXCAGE=30-STAGE-1
         ENDSELECT
      ELSE
         SELECT CASE(VERSION)
            CASE(1)
               EXCAGE=500-(CYCLG+1)*5
            CASE(2,3)
               EXCAGE=120-(CYCLG+1)*5
            CASE(4)
               EXCAGE=30-(CYCLG+1)*1
         ENDSELECT
      ENDIF
      IF(EXCAGE .LT. 0) THEN
         SWARNING(9) = 1
      ENDIF
      B1=-0.04484724
      DO I=1,NTREES
         SELECT CASE(SPECIES(I))
           CASE(202)
              B0=19.04942539
           CASE(263)
              IF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)B0=19.04942539
              IF(VERSION .EQ. 4)B0=19.04942539
           CASE(17,15)
              B0=16.26279948
           CASE(122)
              B0=17.11482201
           CASE(117)
              B0=14.29011403
           CASE DEFAULT
              B0=15.80319194
         ENDSELECT
         PHT=4.5+B0*DBH(I)/(1.0-B1*DBH(I))
         IF(HT(I) .GT. PHT) THEN
            TWARNING(I) = 1
         ENDIF
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),SPGRP(2000),ISX,J
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4  SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      INTEGER*4   SCODE3(7)/
     1           351,202,263,242,312,492,920/
      ISX = -9999
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES(I).EQ. SCODE1(J))THEN
                ISX = J
                IF(ISX .GT.2) ISX=ISX-1
                EXIT
             ENDIF
           ENDDO
        CASE(2,3)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE2(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
        CASE(4)
           DO J = 1, 7
              IF(SPECIES(I).EQ. SCODE3(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
      ENDSELECT
      SPGRP(I)=ISX
      RETURN
      END
C***********************************************************************
      SUBROUTINE CKSP_RUN(VERSION,I,SPECIES,TERROR)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),J,
     1          TERROR(2000,6)
      LOGICAL*2 BAD
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4   SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      INTEGER*4   SCODE3(7)/
     1           351,202,263,242,312,492,920/
      BAD=.TRUE.
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES(I).EQ. SCODE1(J))THEN
                BAD =.FALSE.
                EXIT
             ENDIF
           ENDDO
        CASE(2,3)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE2(J))THEN
                 BAD =.FALSE.
             ENDIF
           ENDDO
        CASE(4)
           DO J = 1, 7
              IF(SPECIES(I).EQ. SCODE3(J))THEN
                 BAD =.FALSE.
             ENDIF
           ENDDO
      ENDSELECT
      IF(BAD) THEN
         TERROR(I,1) = 1
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE CKAGE(VERSION,NTREES,IB,SPGRP,PDEN,SITE_1,SITE_2,HT,
     1                 OLD)
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,IB,SPGRP(2000),K,ISISP
      REAL*4 PDEN,SITE,SITE_1,SITE_2,HT(2000),OLD,GEAGE,PHTGRO,IDXAGE,
     1       SI_UC,GP
      OLD=0.0
      DO K=1,NTREES
         IF(HT(K) .LE. 4.5) CYCLE
         SELECT CASE(VERSION)
         CASE(1)
C
C        GROWTH EFFECTIVE AGE FROM HANN AND SCRIVANI'S (1987) DOMINANT
C        HEIGHT GROWTH EQUATION
C
            IF(SPGRP(K) .EQ. 3)THEN
               SITE=SITE_2-4.5
               ISISP=2
            ELSE
               SITE=SITE_1-4.5
               IF(SPGRP(K) .EQ. 5) SITE=SITE_1*0.66-4.5
               ISISP=1
            ENDIF
            CALL HS_HG(ISISP,SITE,HT(K),GEAGE,PHTGRO)
            IDXAGE=500.0
         CASE(2)
            GP=5.0
            IF(SPGRP(K) .EQ. 3)THEN
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
C           DOMINANT HEIGHT GROWTH EQUATION
C
               SITE = SITE_2
               CALL F_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
            ELSE
C
C        GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
C        GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR
C
               SITE=SITE_1
               CALL B_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
            ENDIF
            IDXAGE=120.0
         CASE(3)
            GP=5.0
            IF(SPGRP(K) .EQ. 3)THEN
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
C           DOMINANT HEIGHT GROWTH EQUATION
C
                SITE = SITE_2
                CALL F_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
            ELSE
C
C           GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
C           GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR
C
               SITE=SITE_1
               CALL B_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
             ENDIF
             IDXAGE=120.0
         CASE(4)
            GP=1.0
            IF(SPGRP(K) .EQ. 1)THEN
C
C           GROWTH EFFECTIVE AGE FROM WEISKITTEL ET AL.'S (2009) RED ALDER
C           DOMINANT HEIGHT GROWTH EQUATION
C
                SITE = SITE_1
                CALL WHHLB_SI_UC_RUN(SITE,PDEN,SI_UC)
                CALL WHHLB_GEA_RUN(HT(K),SI_UC,GEAGE)
            ENDIF
            IDXAGE=30.0
         ENDSELECT
         IF(SPGRP(K).LE.IB.AND.GEAGE.GT.IDXAGE)THEN
            OLD=OLD+1.0
         ENDIF
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE GET_ORGRUN_EDITION(EDITION)
      IMPLICIT NONE
      REAL*4 EDITION
      EDITION=9.1
      RETURN
      END
C***********************************************************************
      SUBROUTINE CROWN_CLOSURE(VERSION,NTREES,NPTS,SPECIES,DBH,HT,
     1                         CR,SCR,EXPAN,CC)
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,NPTS,SPECIES(2000),L,I,
     1          ISPGRP(2000)
      REAL*4    DBH(2000),HT(2000),CR(2000),SCR(2000),EXPAN(2000),
     1          CCH(41),CC,CL,HCB,MCW,LCW,HLCW,EXPFAC
C
      DO I=1,NTREES
         CALL SPGROUP_RUN(VERSION,I,SPECIES,ISPGRP)
      ENDDO
      DO L=1,40
         CCH(L)=0.
      ENDDO
      CCH(41)=HT(1)
      DO I=2,NTREES
         IF(HT(I).GT.CCH(41)) CCH(41)=HT(I)
      ENDDO
      DO I=1,NTREES
         CL=CR(I)*HT(I)
         HCB=HT(I)-CL
         EXPFAC=EXPAN(I)/FLOAT(NPTS)
         SELECT CASE(VERSION)
            CASE(1)
               CALL MCW_SWO(ISPGRP(I),DBH(I),HT(I),MCW)
               CALL LCW_SWO(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
               CALL HLCW_SWO(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
               CALL CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,
     1                      EXPFAC,CCH)
            CASE(2)
               CALL MCW_NWO(ISPGRP(I),DBH(I),HT(I),MCW)
               CALL LCW_NWO(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
               CALL HLCW_NWO(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
               CALL CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,
     1                      EXPFAC,CCH)
            CASE(3)
               CALL MCW_SMC(ISPGRP(I),DBH(I),HT(I),MCW)
               CALL LCW_SMC(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
               CALL HLCW_SMC(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
               CALL CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,
     1                      EXPFAC,CCH)
            CASE(4)
               CALL MCW_RAP(ISPGRP(I),DBH(I),HT(I),MCW)
               CALL LCW_RAP(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
               CALL HLCW_RAP(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
               CALL CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,
     1                      EXPFAC,CCH)
         ENDSELECT
      ENDDO
      CC=CCH(1)
      RETURN
      END
**********************************************************************
      SUBROUTINE INGRO_FILL(VERSION,NTREES,NINGRO,SPECIES,SITE_1,
     1                      SITE_2,ACALIB,DBH,HT,CR,EXPAN)
C     ROUTINE TO CALCULATE MISSING CROWN RATIOS
C
C     NINGRO = NUMBER OF TREES ADDED
C
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,NINGRO,SPECIES(2000)
      INTEGER*4 IB,SPGRP(2000),TDATAI(2000,3),I
      REAL*4 SITE_1,SITE_2,ACALIB(3,18),DBH(2000),HT(2000),CR(2000),
     1       EXPAN(2000)
      REAL*4 SI_1,SI_2,TDATAR(2000,8),GROWTH(2000,4),DEADEXP(2000),
     1       RHT,SBA,TPA,SCCF,BAL(500),BALL(51),CCFL(500),CCFLL(51),OG,
     2       SCCFL,HCB
      IF(VERSION .EQ. 1)THEN
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_1=1.062934*SITE_2
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_2=0.940792*SITE_1
        ENDIF
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)THEN
        IF(SITE_1 .LE. 0.0 .AND. SITE_2 .GT. 0.0)THEN
          SITE_1=0.480 + (1.110 * SITE_2)
        ELSEIF(SITE_2 .LE. 0.0)THEN
          SITE_2=-0.432 + (0.899 * SITE_1)
        ENDIF
      ELSE
        IF(SITE_2 .LE. 0.0) THEN
          SITE_2=4.776377*SITE_1**0.763530587
        ENDIF
      ENDIF
      SELECT CASE(VERSION)
        CASE(1)
          IB=5
        CASE(2,3)
          IB=3
        CASE(4)
          IB=3
      ENDSELECT
      SI_1=SITE_1-4.5
      SI_2=SITE_2-4.5
      DO I=NTREES-NINGRO,NTREES
         IF(HT(I) .NE. 0.0) CYCLE
         CALL SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
C
C        CALCULATE HCB
C
         SELECT CASE(VERSION)
            CASE(1)
               CALL HD_SWO(SPGRP(I),DBH(I),RHT)
            CASE(2)
               CALL HD_NWO(SPGRP(I),DBH(I),RHT)
            CASE(3)
               CALL HD_SMC(SPGRP(I),DBH(I),RHT)
            CASE(4)
               CALL HD_RAP(SPGRP(I),DBH(I),RHT)
         ENDSELECT
         HT(I)=4.5+ACALIB(1,SPGRP(I))*(RHT-4.5)
      ENDDO
      DO I=1,2000
        GROWTH(I,3)=0.0
        GROWTH(I,4)=0.0
        TDATAI(I,1)=0
        TDATAI(I,2)=0
        TDATAI(I,3)=0
        TDATAR(I,1)=0.0
        TDATAR(I,2)=0.0
        TDATAR(I,3)=0.0
        TDATAR(I,4)=0.0
        DEADEXP(I)=0.0
      ENDDO
      DO I=1,NTREES
        TDATAI(I,1)=SPECIES(I)
        TDATAI(I,2)=SPGRP(I)
        TDATAR(I,1)=DBH(I)
        TDATAR(I,2)=HT(I)
        TDATAR(I,3)=CR(I)
        TDATAR(I,4)=EXPAN(I)
      ENDDO
      CALL OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG)
      CALL SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA,TPA,SCCF,BAL,
     1            BALL,CCFL,CCFLL)
      DO I=NTREES-NINGRO,NTREES
         IF(CR(I) .NE. 0.0) CYCLE
C
C        CALCULATE HCB
C
         CALL GET_CCFL(DBH(I),CCFLL,CCFL,SCCFL)
         SELECT CASE(VERSION)
            CASE(1)
               CALL HCB_SWO(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,
     1                      OG,HCB)
            CASE(2)
               CALL HCB_NWO(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,
     1                      OG,HCB)
            CASE(3)
               CALL HCB_SMC(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,
     1                      OG,HCB)
            CASE(4)
               CALL HCB_RAP(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,
     1                      OG,HCB)
         ENDSELECT
         IF(HCB .LT. 0.) HCB=0.0
         IF(HCB .GT. 0.95*HT(I)) HCB=0.95*HT(I)
         CR(I)=(1.-(HCB/HT(I)))*ACALIB(2,SPGRP(I))
      ENDDO
      RETURN
      END
C**********************************************************************
C      SUBROUTINE NCH40(H40,AGE,SI)
CC
CC     NIGH AND COURTIN (1998) RED ALDER
CC
C      IMPLICIT NONE
C      REAL*4 SI,AGE,H40,MSI,MH40
C      MSI=SI/3.28
C      MH40=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
C      H40=3.28*MH40
C      RETURN
C      END
**********************************************************************


