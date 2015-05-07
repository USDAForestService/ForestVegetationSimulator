C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C        PREPARE
C        EDIT
C        SPGROUP
C        CKSP
C        CKAGE
C        HSHG
C        BHG
C        FHG
CC        NCHG
C        WHHLB_GEA
C        GET_ORGEDIT_EDITION
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE B_HG TO B_HG_EDIT
C  CHANGED THE NAME OF SUBROUTINE CKAGE TO CKAGE_EDIT
C  CHANGED THE NAME OF SUBROUTINE F_HG TO F_HG_EDIT
C  CHANGED THE NAME OF SUBROUTINE HS_HG TO HS_HG_EDIT
C  CHANGED THE NAME OF SUBROUTINE CALL SITECV_F TO SITECV_F_EDIT
C  CHANGED THE NAME OF SUBROUTINE CALL SPGROUP TO SPGROUP_EDIT
C
**********************************************************************
      SUBROUTINE PREPARE(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
     1                   USER,IEVEN,DBH,HT,CR,EXPAN,RADGRO,RVARS,
     2                   SERROR,TERROR,SWARNING,TWARNING,IERROR,IRAD,
     3                   GROWTH,ACALIB)
C      SUBROUTINE PREPARE(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
C     1                   USER,IEVEN,DBH,HT,CR,EXPAN,RADGRO,SITE_1,
C     2                   SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,SERROR,TERROR,
C     3                   SWARNING,TWARNING,IERROR,IRAD,GROWTH,ACALIB)
      IMPLICIT NONE
      INTEGER*4 VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),
     1          USER(2000),SERROR(13),TERROR(2000,6),SWARNING(8),
     2          TWARNING(2000)
      INTEGER*4 IB,NSPN,TDATAI(2000,3),SPGRP(2000),RADIN(18),
     1          ENTDBH(18),ENTHT(18),ENTCR(18),ENT
      INTEGER*4 IEVEN,IERROR,IRAD
      REAL*4 DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),
     1       RVARS(30)
      REAL*4 SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN
      REAL*4 TDATAR(2000,4),SBA,BALL(51),BAL(500),GROWTH(2000),
     1       PDG(2000),PTRHT(2000),PCR(2000),CCFLL(51),
     2       CCFL(500),STDATAR(2000,4),OG,ACALIB(3,18),HT40,D40,PDF,
     3       PTF,PPP,PWH,PRA
      LOGICAL*2 EVEN,ERROR,MISSHT,MISSCR,RAD
      INTEGER*4 I,II,J,K,KK,IDXAGE,IDXCYC
C      INTEGER*4 IANS,IYN
      EVEN =   .FALSE.
      IF(IEVEN .EQ. 1) THEN
         EVEN = .TRUE.
      ENDIF
      SITE_1=RVARS(1)
      SITE_2=RVARS(2)
      MSDI_1=RVARS(3)
      MSDI_2=RVARS(4)
      MSDI_3=RVARS(5)
      PDEN=RVARS(9)
      CALL EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,EVEN,DBH,
     1          HT,CR,EXPAN,RADGRO,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,
     2          PDEN,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,SWARNING,
     3          TWARNING,ERROR,MISSHT,MISSCR,RAD)
      IERROR =   0
      IF(ERROR) THEN
         IERROR = 1
         RETURN
      ENDIF
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
      DO I=1,2000
        TDATAI(I,1)=0.0
        TDATAI(I,2)=0.0
        TDATAI(I,3)=0.0
        TDATAR(I,1)=0.0
        TDATAR(I,2)=0.0
        TDATAR(I,3)=0.0
        TDATAR(I,4)=0.0
        STDATAR(I,1)=0.0
        STDATAR(I,2)=0.0
        STDATAR(I,3)=0.0
        STDATAR(I,4)=0.0
        PCR(I)=0.0
        PTRHT(I)=0.0
        GROWTH(I)=0.0
      ENDDO
      DO I=1,NTREES
        TDATAI(I,1)=SPECIES(I)
        TDATAI(I,2)=SPGRP(I)
        TDATAI(I,3)=USER(I)
        TDATAR(I,1)=DBH(I)
        TDATAR(I,2)=HT(I)
        TDATAR(I,3)=CR(I)
        TDATAR(I,4)=EXPAN(I)
      ENDDO
      DO I=1,18
         ACALIB(1,I)=0.0
         ACALIB(2,I)=0.0
         ACALIB(3,I)=0.0
         ENTDBH(I)=0
         ENTHT(I)=0
         ENTCR(I)=0
      ENDDO
      CALL HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,STAGE,BHAGE,TDATAI,EVEN,
     1     SITE_1,SITE_2,PDEN,TDATAR,D40,HT40,PDF,PTF,PPP,PWH,PRA,ENTHT,
     2     ENTDBH,PTRHT,ACALIB)
      IF(MISSHT) THEN
         CALL PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40,PDF,PTF,
     1        PPP,PWH,PRA,ACALIB,PTRHT)
      ENDIF
      CALL CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,ENTDBH,
     1     SITE_1,SITE_2,TDATAR,STDATAR,BAL,BALL,CCFL,CCFLL,SBA,OG,
     2     ENTCR,PCR,ACALIB)
      IF(MISSCR) THEN
         CALL PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,SITE_1,
     1              SITE_2,TDATAR,CCFL,CCFLL,SBA,OG,ENT,PCR,ACALIB)
      ENDIF
      IF(RAD) THEN
         CALL DGCALIB(VERSION,IB,NSPN,NPTS,NTREES,RADIN,TDATAI,
     1                RAD,BAL,BALL,OG,TDATAR,GROWTH,SITE_1,SITE_2,
     2                RADGRO,SBA,STDATAR,PDG,ACALIB)
      ELSE
         DO I=1,18
            ACALIB(3,I)=1.0
         ENDDO
      ENDIF
      IRAD=0
      IF(RAD) THEN
         IRAD=1
      ENDIF
      DO I=1,NTREES
        SPECIES(I)=TDATAI(I,1)
        USER(I)=TDATAI(I,3)
        DBH(I)=TDATAR(I,1)
        HT(I)=TDATAR(I,2)
        CR(I)=TDATAR(I,3)
        EXPAN(I)=TDATAR(I,4)
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
     1           EVEN,DBH,HT,CR,EXPAN,RADGRO,SI_1,SI_2,MSDI_1,MSDI_2,
     2           MSDI_3,PDEN,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,SWARNING,
     3           TWARNING,ERROR,MISSHT,MISSCR,RAD)

      IMPLICIT NONE
      INTEGER*4 VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),IB,
     1          NSPN,IIB
      INTEGER*4 I,J,SPGRP(2000),RADIN(18),BIG6,BNXT,SERROR(13),
     1          TERROR(2000,6),SWARNING(8),TWARNING(2000),TEMPERR
      REAL*4 DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),
     1       SI_1,SI_2,MSDI_1,MSDI_2,MSDI_3,MAXDF,MAXGF,MAXWH,MAXPP,
     2       MAXIC,MAXRA,MAXHT,ICSI,PDEN,PRA
      REAL*4 BA,SBA,B6SBA,HWSBA,B0,B1,PHT,OLD,Z50,X,XSI50
      LOGICAL*2 EVEN,ERROR,MISSHT,MISSCR,RAD
      ERROR = .FALSE.
      RAD = .FALSE.
      MISSHT = .FALSE.
      MISSCR = .FALSE.
      BIG6=0
      BNXT=0
      MAXGF = 0.0
      MAXDF = 0.0
      MAXWH = 0.0
      MAXPP = 0.0
      MAXIC = 0.0
      MAXRA = 0.0
      DO I=1,18
         RADIN(I)=0
      ENDDO
      DO I=1,2000
         DO J=1,6
            TERROR(I,J)=0
         ENDDO
         TWARNING(I)=0
      ENDDO
      DO I=1,13
         SERROR(I)=0
      ENDDO
      DO I=1,8
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
      IF(SI_1 .LE. 0.0 .AND. SI_2 .LE. 0.0)THEN
          SERROR(4) = 1
      ENDIF
      IF(EVEN .AND. BHAGE .LE. 0) THEN
         SERROR(6) = 1
      ENDIF
      IF(.NOT. EVEN .AND. BHAGE .GT. 0) THEN
         SERROR(7) = 1
      ENDIF
      IF(EVEN .AND. (STAGE-BHAGE) .LE. 1) THEN
         SERROR(8) = 1
      ENDIF
      IF(MSDI_1 .GT. 1000.0 .OR. MSDI_2 .GT. 1000.0 .OR. MSDI_3 .GT.
     1   1000.0) THEN
         SERROR(9) = 1
      ENDIF
      IF(VERSION .GE. 4 .AND. SI_1 .LE. 0.0)THEN
          SERROR(10) = 1
      ENDIF
      IF(VERSION .GE. 4 .AND. PDEN .LE. 0.0)THEN
          SERROR(11) = 1
      ENDIF
      IF(.NOT. EVEN .AND. VERSION .GE. 4) THEN
         SERROR(12) = 1
      ENDIF
      DO I=1,13
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
         CALL CKSP_EDIT(VERSION,I,SPECIES,TERROR)
         IF(DBH(I) .LE. 0.09)THEN
            TERROR(I,2) = 1
         ENDIF
         IF(HT(I) .LE. 0.0)THEN
            MISSHT = .TRUE.
         ELSEIF(HT(I) .LE. 4.5)THEN
            TERROR(I,3) = 1
         ENDIF
         IF(CR(I) .LE. 0.0)THEN
            MISSCR = .TRUE.
         ELSEIF(CR(I) .GT. 1.0)THEN
            TERROR(I,4) = 1
         ENDIF
         IF(EXPAN(I) .LT. 0.0)THEN
            TERROR(I,5) = 1
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
         CALL SPGROUP_EDIT(VERSION,I,SPECIES,SPGRP)
         IIB=IB
         IF(VERSION .GE. 4) IIB=1
         IF(SPGRP(I).LE.IIB) THEN
            BIG6=BIG6+1
            IF(EXPAN(I) .LE. 0.0) THEN
               BNXT=BNXT+1
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
        IF(SPGRP(I) .LE. IIB) B6SBA=B6SBA+BA
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
            SERROR(13) = 1
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDIF
      IF(VERSION .EQ. 1)THEN
        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
          SI_1=1.062934*SI_2
        ELSEIF(SI_2 .LE. 0.0)THEN
          SI_2=0.940792*SI_1
        ENDIF
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)THEN
C  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
          SI_1=0.480 +( 1.110 * SI_2)
        ELSEIF(SI_2 .LE. 0.0)THEN
          SI_2=-0.432 +( 0.899 * SI_1)
        ENDIF
      ELSE
        IF(SI_2 .LE. 0.0) THEN
           SI_2=4.776377*SI_1**0.763530587
        ENDIF
      ENDIF
C
C     DETERMINE WARNINGS (IF ANY)
C
      SELECT CASE (VERSION)
         CASE(1)
            IF((SI_1.GT.0.0).AND.(SI_1.LT.40.0 .OR. SI_1.GT. 150.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SI_2.GT.0.0).AND.(SI_2.LT.50.0 .OR. SI_2.GT. 140.0))THEN
               SWARNING(2) = 1
            ENDIF
         CASE(2,3)
            IF((SI_1.GT.0.0).AND.(SI_1.LT.90.0 .OR. SI_1.GT. 142.0))THEN
               SWARNING(1) = 1
            ENDIF
            IF((SI_2.GT.0.0).AND.(SI_2.LT.90.0 .OR. SI_2.GT. 142.0))THEN
               SWARNING(2) = 1
            ENDIF
         CASE(4)
            IF(SI_1.LT.20.0 .OR. SI_1.GT. 90.0)THEN
               SWARNING(1) = 1
            ENDIF
            IF((SI_2.GT.0.0).AND.(SI_2.LT.70.0 .OR. SI_2.GT. 160.0))THEN
               SWARNING(2) = 1
            ENDIF
      ENDSELECT
      SELECT CASE(VERSION)
        CASE(1)
          IF(MAXPP .GT. 0.0)THEN
             MAXHT=(SI_2-4.5)*(1.0/(1.-EXP(-0.164985*(SI_2-4.5)
     1              **0.288169)))+4.5
             IF(MAXPP .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXIC .GT. 0.0)THEN
             ICSI=(0.66*SI_1)-4.5
             MAXHT=ICSI*(1.0/(1.-EXP(-0.174929*ICSI**0.281176)))+4.5
             IF(MAXIC .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXDF .GT. 0.0) THEN
             MAXHT=(SI_1-4.5)*(1.0/(1.0 -
     1             EXP(-0.174929*(SI_1-4.5)**0.281176)))+4.5
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
        CASE(2,3)
          IF(MAXDF .GT. 0.0)THEN
             Z50=2500.0/(SI_1-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXDF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXGF .GT. 0.0)THEN
             Z50=2500.0/(SI_1-4.5)
             MAXHT=4.5+1.0/(-0.000733819+0.000197693*Z50)
             IF(MAXGF .GT. MAXHT) SWARNING(3) = 1
          ENDIF
          IF(MAXWH .GT. 0.0)THEN
             Z50=2500.0/(SI_2-4.5)
             MAXHT=4.5+1.0/(0.00192+0.00007*Z50)
             IF(MAXWH .GT. MAXHT) SWARNING(3) = 1
          ENDIF
        CASE(4)
          IF(MAXRA .GT. 0.0)THEN
             CALL WHHLB_H40_EDIT(SI_1,20.0,150.0,MAXHT)
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
      CALL CKAGE_EDIT(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,PDEN,HT,OLD)
      X=100.0*(OLD/(BIG6-BNXT))
      IF(X .GE. 50.0) SWARNING(7) = 1
      IF(VERSION .EQ. 1) THEN
         IF(EVEN .AND. BHAGE .GT. 500.) SWARNING(8) = 1
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3) THEN
         IF(EVEN .AND. BHAGE .GT. 120.) SWARNING(8) = 1
      ELSE
         IF(EVEN .AND. STAGE .GT. 30.) SWARNING(8) = 1
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
         IF(HT(I) .GT. 4.5) THEN
            PHT=4.5+B0*DBH(I)/(1.0-B1*DBH(I))
            IF(HT(I) .GT. PHT) THEN
               TWARNING(I) = 1
            ENDIF
         ENDIF
         IF(RADGRO(I) .GT. 0.0) THEN
            RAD = .TRUE.
            RADIN(SPGRP(I))=RADIN(SPGRP(I))+1
         ENDIF
      ENDDO
      RETURN
      END
C***********************************************************************
      SUBROUTINE SPGROUP_EDIT(VERSION,I,SPECIES,SPGRP)
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
      INTEGER*4   SCODE2(11)/
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
      SUBROUTINE CKSP_EDIT(VERSION,I,SPECIES,TERROR)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),SPGRP(2000),J,
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
      SUBROUTINE CKAGE_EDIT(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,
     &PDEN,HT,OLD) 
      IMPLICIT NONE
      INTEGER*4 VERSION,NTREES,IB,SPGRP(2000),K,ISISP
      REAL*4 SITE,SI_1,SI_2,HT(2000),OLD,GEAGE,PHTGRO,IDXAGE,SI_UC,PDEN
      OLD=0.0
      DO K=1,NTREES
         IF(HT(K) .LE. 4.5) CYCLE
         IF(SPGRP(K).GT.IB) CYCLE
         SELECT CASE(VERSION)
         CASE(1)
C
C        GROWTH EFFECTIVE AGE FROM HANN AND SCRIVANI'S (1987) DOMINANT
C        HEIGHT GROWTH EQUATION
C
            IF(SPGRP(K) .EQ. 3)THEN
               SITE=SI_2-4.5
               ISISP=2
            ELSE
               SITE=SI_1-4.5
C               IF(SPGRP(K) .EQ. 5) SITE=SI_1*0.66-4.5
               IF(SPGRP(K) .EQ. 5) SITE=SI_1*0.66-4.5
               ISISP=1
            ENDIF
            CALL HS_HG_EDIT(ISISP,SITE,HT(K),GEAGE,PHTGRO)
            IDXAGE=500.0
         CASE(2)
            IF(SPGRP(K) .EQ. 3)THEN
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
C           DOMINANT HEIGHT GROWTH EQUATION
C
               SITE = SI_2
               CALL F_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
            ELSE
C
C        GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
C        GROWTH EQUATION FOR DOUGLAS-FIRAND GRAND FIR
C
               SITE=SI_1
               CALL B_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
            ENDIF
            IDXAGE=120.0
         CASE(3)
            IF(SPGRP(K) .EQ. 3)THEN
C
C           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
C           DOMINANT HEIGHT GROWTH EQUATION
C
                SITE = SI_2
                CALL F_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
            ELSE
C
C           GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
C           GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR
C
               SITE=SI_1
               CALL B_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
             ENDIF
             IDXAGE=120.0
         CASE(4)
            IF(SPGRP(K) .EQ. 1)THEN
C
C           GROWTH EFFECTIVE AGE FROM WEISKITTEL ET AL. (2009) RED ALDER
C           DOMINANT HEIGHT GROWTH EQUATION
C
                SITE = SI_1
                CALL WHHLB_SI_UC_EDIT(SITE,PDEN,SI_UC)
                CALL WHHLB_GEA_EDIT(HT(K),SI_UC,GEAGE)
            ENDIF
            IDXAGE=30.0
         ENDSELECT
         IF(GEAGE.GT.IDXAGE)THEN
            OLD=OLD+1.0
         ENDIF
      ENDDO
      RETURN
      END
**********************************************************************
      SUBROUTINE HS_HG_EDIT(ISP,SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
      INTEGER*4 ISP
      REAL*4 SI,GEAGE,HT,PHTGRO,B0,B1,B2,BBC,X50,A1A,XAI,XAI5
      IF(ISP .EQ. 1)THEN
         B0=-6.21693
         B1=0.281176
         B2=1.14354
      ELSE
         B0=-6.54707
         B1=0.288169
         B2=1.21297
      ENDIF
      BBC=B0+B1*LOG(SI)
      X50=1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
      A1A=1.0-(HT-4.5)*(X50/SI)
      IF(A1A .LE. 0.)THEN
         GEAGE=500.
         PHTGRO=0.
      ELSE
         GEAGE=((-1.0*LOG(A1A))/(EXP(B0)*SI**B1))**(1.0/B2)
         XAI=1.0-EXP(-1.0*EXP(BBC+B2*LOG(GEAGE)))
         XAI5=1.0-EXP((-1.)*EXP(BBC+B2*LOG(GEAGE+5.)))
         PHTGRO=(4.5+(HT-4.5)*(XAI5/XAI))-HT
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE B_HG_EDIT(SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
      REAL*4 SI,HT,X1,X2,B1,B2,GEAGE,PHT,XX1,PHTGRO
      X1=13.25-SI/20.0
      X2=63.25-SI/20.0
      B2=-0.447762-0.894427*SI/100.0+0.793548*(SI/100.0)**2
     1   -0.171666*(SI/100.0)**3
      B1=ALOG(4.5/SI)/(X1**B2-X2**B2)
      XX1=ALOG(HT/SI)/B1+X2**B2
      IF(XX1 .GT. 0.0) THEN
         GEAGE=XX1**(1.0/B2)-X1
      ELSE
         GEAGE=500.0
      ENDIF
      PHT=SI*EXP(B1*((GEAGE+5.0+X1)**B2-X2**B2))
      PHTGRO=PHT-HT
      RETURN
      END
**********************************************************************
      SUBROUTINE F_HG_EDIT(SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
C     For Western Hemlock compute Growth Effective Age and 5-year potential
C     height growth using the western hemlock top height curves of
C     Flewelling.  These subroutines are required:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C       Note: Flewelling's curves are metric.
C             Site Index is not adjusted for stump height.
C
      REAL*4 SI,GEAGE,HT,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
      INTEGER*4 I
      SIM = SI * 0.3048
      HTM = HT * 0.3048
C
C     Compute growth effective age
C
      AGE = 1.0
      DO I=1,4
    5     AGE = AGE + 100./10.**I
          IF(AGE .GT. 500.0) THEN
             GEAGE = 500.0
             CALL SITECV_F_EDIT(SIM,GEAGE,XHTOP1)
             CALL SITECV_F_EDIT(SIM,GEAGE+5,XHTOP2)
             PHTGRO = 3.2808*(XHTOP2-XHTOP1)
             RETURN
          ENDIF
          CALL SITECV_F_EDIT(SIM,AGE,HTOP)
          IF (HTOP .LT. HTM) GO TO 5
          AGE = AGE - 100./10.**I
      ENDDO
      GEAGE = AGE
C
C     Compute top height and potential height growth
C
      CALL SITECV_F_EDIT(SIM,GEAGE+5,HTOP)
      PHT = HTOP*3.2808
      PHTGRO = PHT - HT
      RETURN
      END
C**********************************************************************
C      SUBROUTINE NCHG(SI,HT,GEAGE,PHTGRO)
CC
CC     NIGH AND COURTIN (1998) RED ALDER
CC
C      IMPLICIT NONE
C      REAL*4 SI,HT,GEAGE,PHTGRO,MSI,MHT,X1,X2,AGE,FMHT
C      MSI=SI/3.28
C      MHT=HT/3.28
C      X1=1.693*(MSI-1.3)/(MHT-1.3)
C      IF(X1 .LT. 1.0001) X1=1.0001
C      X2=(3.600-ALOG(X1-1.0))/1.240
C      GEAGE=EXP(X2)+0.5
C      IF(GEAGE .GT. 150.0) GEAGE=150.0
C      AGE=GEAGE+1.0
C      FMHT=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
C      PHTGRO=3.28*(FMHT-MHT)
C      RETURN
C      END
C***********************************************************************
      SUBROUTINE WHHLB_GEA_EDIT(H,SI_UC,GEA)
C
C     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
C         THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH
C         EQUATION
C
      IMPLICIT NONE
      REAL*4 H,SI_UC,GEA,B1,B2,X
      B1=-4.481266
      B2=-0.658884
      X=(1.0/B1)*ALOG(H/SI_UC)+20.0**B2
      IF(X .LT. 0.03) X=0.03
      GEA=X**(1.0/B2)
      RETURN
      END
C***********************************************************************
      SUBROUTINE GET_ORGEDIT_EDITION(EDITION)
      IMPLICIT NONE
      REAL*4 EDITION
      EDITION=9.1
      RETURN
      END
