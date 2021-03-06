C----------
C ORGANON $Id$
C----------
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE DIAMGRO TO DIAMGRO_RUN IN THE
C               DIAGRO.FOR FILE, SO IN THIS FILE WE EDITED
C               THE CALL TO DIAMGRO TO DIAMGRO_RUN
C  CHANGED THE NAME OF SUBROUTINE CALL - MORTAL TO MORTAL_RUN
C
      SUBROUTINE GROW(VERSION,CYCLG,NTREES,IB,BIG6,OTHER,NSPN,STAGE,
     1                BHAGE,POINT,TREENO,TDATAI,PRAGE,BRCNT,BRHT,BRDIA,
     2                JCORE,NPR,TCYCLE,FCYCLE,TRIPLE,WOODQ,POST,MORT,
     3                GENETICS,SWISSNC,TDATAR,SI_1,SI_2,SBA1,BALL1,BAL1,
     4                CALIB,PN,YF,BABT,BART,YT,GROWTH,PRLH,PRDBH,PRHT,
     5                PRCR,PREXP,SCR,VOLTR,SYTVOL,CCH,OLD,MGEXP,DEADEXP,
     6                A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,RASI,CCFLL1,CCFL1,
     7                SBA2,CCFLL2,CCFL2,BALL2,BAL2,TPA2,SCCF2,GWDG,GWHG,
     8                FR,PDEN)
      IMPLICIT NONE
      INTEGER*4 VERSION,CYCLG,NTREES,IB,BIG6,OTHER,
     1          NSPN,STAGE,BHAGE,POINT(2000),TREENO(2000),
     2          TDATAI(2000,3),PRAGE(2000,3),BRCNT(2000,3),
     3          BRHT(2000,40),BRDIA(2000,40),JCORE(2000,40),
     4          NPR(2000),TCYCLE,FCYCLE,J,NTCAL1,NTCAL2,NTCAL3,NTCAL4,
     5          NTCAL5,NTCAL6,M,ON,IWQ,I,II,NZERO,BZERO,OZERO
      REAL*4 TDATAR(2000,8),SI_1,SI_2,SBA1,BALL1(51),BAL1(500),
     1       CALIB(6,18),PN(5),YF(5),BABT,BART(5),YT(5),GROWTH(2000,4),
     2       PRLH(2000,3),PRDBH(2000,3),PRHT(2000,3),PRCR(2000,3),
     3       PREXP(2000,3),SCR(2000,3),VOLTR(2000,4),SYTVOL(2000,2),
     4       CCH(41),OLD,MGEXP(2000),DEADEXP(2000),A1,A2,A1MAX,PA1MAX,
     5       NO,RD0,RAAGE,RASI,CCFLL1(51),CCFL1(500),SBA2,CCFLL2(51),
     6       CCFL2(500),BALL2(51),BAL2(500),TPA2,SCCF2,MCALIB,PDEN
      REAL*4 TAGE,GWDG,GWHG,FR,DGMOD_GG,HGMOD_GG,DGMOD_SNC,HGMOD_SNC
      LOGICAL*2 TRIPLE,WOODQ,POST,MORT
      LOGICAL*2 GENETICS,SWISSNC
C
C---------------------------------------------------------
C     M=0 NO TRIPLING
C     M=1 TRIPLE EVERY TREE
C     M=2 TRIPLE EVERY OTHER TREE
C     M=3 RANDOM ERROR
C---------------------------------------------------------
      DGMOD_GG=1.0
      HGMOD_GG=1.0
      DGMOD_SNC=1.0
      HGMOD_SNC=1.0
      IF(STAGE .GT. 0 .AND. GENETICS) THEN
         TAGE=FLOAT(STAGE)
         CALL GG_MODS(TAGE,GWDG,GWHG,DGMOD_GG,HGMOD_GG)
      ENDIF
      IF(SWISSNC .AND. (VERSION .EQ. 2 .OR. VERSION .EQ. 3)) THEN
         CALL SNC_MODS(FR,DGMOD_SNC,HGMOD_SNC)
      ENDIF
C     GROWTH 1
C     CALCULATE DIAMGRO
C
      NZERO=0
      BZERO=0
      OZERO=0
      DO I=1,NTREES
         IF(TDATAR(I,4) .LE. 0.0) THEN
            NZERO=NZERO+1
               IF(TDATAI(I,2) .LE. IB)THEN
                  BZERO=BZERO+1
               ELSE
                  OZERO=OZERO+1
               ENDIF
         ENDIF
      ENDDO
      J=NTREES+1
      NTCAL1=(NTREES-NZERO)*3
      NTCAL2=NZERO+NTCAL1
      NTCAL3=(NTREES-NZERO)*2
      NTCAL4=NZERO+NTCAL3
      NTCAL5=BIG6+2*(BIG6-BZERO)
      NTCAL6=BIG6+(BIG6-BZERO)
      M = 0
      IF(TRIPLE)THEN
         IF(NTCAL1 .LE. 2000 .AND. NTCAL2 .LE. 2000)THEN
            M=1
            IF(WOODQ .AND. NTCAL5 .GT. 2000)M=0
         ELSE IF(NTCAL3 .LE. 2000 .AND. NTCAL4 .LE. 2000)THEN
            M=2
            IF(WOODQ .AND. NTCAL6 .GT. 2000)M=0
         ENDIF
      ENDIF
      ON=0
      IWQ=0
      DO I=1,NTREES
        IF(WOODQ.AND.TDATAI(I,2).LE. IB) IWQ=IWQ+1
        IF(TDATAR(I,4) .LE. 0.)THEN
           GROWTH(I,2)=0.
        ELSE
           CALL DIAMGRO_RUN(VERSION,I,CYCLG,TDATAI,TDATAR,SI_1,SI_2,
     1                  SBA1,BALL1,BAL1,CALIB,PN,YF,BABT,BART,YT,GROWTH)
           IF(TDATAI(I,1) .EQ. 202) THEN
              GROWTH(I,2)=GROWTH(I,2)*DGMOD_GG*DGMOD_SNC
           ENDIF
           CALL XTRIP(I,J,M,ON,IWQ,WOODQ,IB,BIG6,POINT,TREENO,
     1                TDATAI,PRAGE,BRCNT,BRHT,BRDIA,JCORE,NPR,PRLH,
     2                PRDBH,PRHT,PRCR,PREXP,SCR,VOLTR,SYTVOL)
           CALL DGTRIP(I,J,M,ON,VERSION,IB,BIG6,OTHER,TDATAI,
     1                 TDATAR,GROWTH,MGEXP,DEADEXP)
        ENDIF
      ENDDO
      IF(TRIPLE)NTREES=J-1
C---------------------------------------------------------
C     GROWTH 2
C     CALCULATE HTGRO FOR BIG6
C
      NTCAL1=(BIG6-BZERO)*3+(OTHER-OZERO)
      NTCAL2=BZERO+OZERO+NTCAL1
      NTCAL3=(BIG6-BZERO)*2+(OTHER-OZERO)
      NTCAL4=BZERO+OZERO+NTCAL3
      NTCAL5=BIG6+2*(BIG6-BZERO)
      NTCAL6=BIG6+(BIG6-BZERO)
      M=0
      IF(TRIPLE)THEN
         IF(NTCAL1 .LE. 2000 .AND. NTCAL2 .LE. 2000)THEN
            M=1
            IF(WOODQ .AND. NTCAL5 .GT. 2000)M=0
         ELSE IF(NTCAL3 .LE. 2000 .AND. NTCAL4 .LE. 2000)THEN
            M=2
            IF(WOODQ .AND. NTCAL6 .GT. 2000)M=0
         ENDIF
      ENDIF
      ON=0
      IWQ=0
      DO I=1,NTREES
        IF(TDATAI(I,2) .LE. IB)THEN
          IF(WOODQ) IWQ=IWQ+1
          IF(TDATAR(I,4) .LE. 0.)THEN
              GROWTH(I,1)=0.
          ELSE
              CALL HTGRO1(I,M,ON,VERSION,CYCLG,IB,TDATAI,TDATAR,SI_1,
     1                    SI_2,CCH,CALIB,PN,YF,BABT,BART,YT,OLD,PDEN,
     2                    GROWTH)
              IF(TDATAI(I,1) .EQ. 202) THEN
                 GROWTH(I,1)=GROWTH(I,1)*HGMOD_GG*HGMOD_SNC
              ENDIF
              CALL XTRIP(I,J,M,ON,IWQ,WOODQ,IB,BIG6,POINT,TREENO,
     1                   TDATAI,PRAGE,BRCNT,BRHT,BRDIA,JCORE,NPR,PRLH,
     2                   PRDBH,PRHT,PRCR,PREXP,SCR,VOLTR,SYTVOL)
              CALL HGTRIP(I,J,M,ON,VERSION,BIG6,OTHER,TDATAI,
     1                    TDATAR,GROWTH,MGEXP,DEADEXP)
          ENDIF
        ENDIF
      ENDDO
      IF(TRIPLE)NTREES=J-1
C
C          DETERMINE MORTALITY, IF REQUIRED
C
      CALL MORTAL_RUN(VERSION,CYCLG,NTREES,IB,TDATAI,POST,MORT,TDATAR,
     1            SCR,GROWTH,MGEXP,DEADEXP,BALL1,BAL1,SI_1,SI_2,PN,YF,
     2            A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,PDEN)
C
C     UPDATE DIAMETERS
C
      DO I=1,NTREES
        TDATAR(I,1)=TDATAR(I,1)+GROWTH(I,2)
      ENDDO
C
C     CALC EOG SBA, CCF/TREE, CCF IN LARGER TREES AND STAND CCF
C
      CALL SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA2,TPA2,SCCF2,
     1            BAL2,BALL2,CCFL2,CCFLL2)
C
C     CALCULATE HTGRO FOR 'OTHER' & CROWN ALL SPECIES
C
      IWQ=0
      DO I=1,NTREES
         IF(TDATAI(I,2) .GT. IB)THEN
            IF(TDATAR(I,4) .LE. 0.)THEN
              GROWTH(I,1)=0.
           ELSE
              CALL HTGRO2(I,VERSION,IB,TDATAI,TDATAR,RASI,CALIB,
     1                    GROWTH)
           ENDIF
        ENDIF
        TDATAR(I,2)=TDATAR(I,2)+GROWTH(I,1)
      ENDDO
C
C     CALC CROWN GROWTH
C
      CALL CROWGRO(VERSION,CYCLG,NTREES,IB,TDATAI,TDATAR,SCR,
     1             GROWTH,MGEXP,DEADEXP,CCFLL1,CCFL1,CCFLL2,CCFL2,SBA1,
     2             SBA2,SI_1,SI_2,CALIB,CCH)
C
C     UPDATE STAND VARIABLES
C
      IF(VERSION .LE. 3)THEN
         STAGE=STAGE+5
         BHAGE=BHAGE+5
      ELSE
         STAGE=STAGE+1
         BHAGE=BHAGE+1
      ENDIF
      CYCLG=CYCLG+1
      IF(FCYCLE.GT.2) THEN
         FCYCLE=0
      ELSE IF(FCYCLE.GT.0) THEN
        FCYCLE=FCYCLE+1
      ENDIF
      IF(TCYCLE.GT.0) TCYCLE=TCYCLE+1
C     REDUCE CALIBRATION RATIOS
      DO I=1,3
         DO II=1, NSPN
            IF(CALIB(I,II) .LT. 1.0 .OR. CALIB(I,II) .GT. 1.0)THEN
               MCALIB=(1.0+CALIB(I+3,II))/2.0
               CALIB(I,II)=MCALIB+SQRT(0.5)*(CALIB(I,II)-MCALIB)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END

