      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C   **FMCFMD FIRE-SO-DATE OF LAST REVISION: 02/28/08
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
*     THE CLOSEST FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE
*     BY THE DYNAMIC FUEL MODEL
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*  LOCAL VARIABLE DEFINITIONS:
*     DSTLG:   YEARS SINCE LAST DISTURBANCE: FIRE OR HARVESTING
*     IPASO:   SORNEC PLANT ASSOCIATION GROUP -> OREGON PLANT ASSOC'N
*              'SUPER-GROUP' (EG DRY LODGEPOLE PINE)
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
*     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
*
***********************************************************************

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'SSTGMC.F77'

C     LOCAL VARIABLE DECLARATIONS

      INTEGER ICLSS

      PARAMETER(ICLSS = 14)

      INTEGER  FMD
      INTEGER  IPTR(ICLSS), ITYP(ICLSS)
      INTEGER  IPASO(92)
      REAL     XPTS(ICLSS,2), EQWT(ICLSS)
      REAL     WD(MAXSP),WT1(2),WT2(2),WT3(2),X(2),Y(2), X3
      LOGICAL  DEBUG
      REAL     XSNG,XM,XMLP,XA,AFWT
      INTEGER  IYR,J,I,IPAG, IFMST
      REAL     DSTLG
      REAL     ALGSLP
C
C     THIS TABLE IS THE BASIS FOR THE IPASO VARIABLE. IT IS BASED ON
C     THE 92 PLANT ASSOCIATIONS FROM NICK CROOKSTON, AND ANNOTATED BY
C     ELIZABETH WITH THE 8 PLANT GROUPS USED BY THE OREGON MODEL
C
C  1:{CDS612 PSME-ABCO/SYAL/LIBO}      DRY MC
C  2:{CDS613 PSME-ABCO/SYAL/FORB}      DRY MC
C  3:{CDS614 PSME-ABCO/SYAL/CARU}      DRY MC
C  4:{CEM111 PIEN/CAEU}                WET MC
C  5:{CEM221 PIEN/EQAR-STRO}           WET MC
C  6:{CEM222 PIEN/CLUN}                WET MC
C  7:{CEM311 PIEN/VAOC2-FORB}          WET MC
C  8:{CEM312 PIEN/VAOC2/CAEU}          WET MC
C  9:{CLC111 PICO-PIAL/PELA}           DRY LP
C 10:{CLC112 PICO-PIAL/ARCO2}          DRY LP
C 11:{CLF111 PICO/FORB}                WET LP
C 12:{CLG311 PICO/STOC-BASIN}          DRY LP
C 13:{CLG313 PICO/STOC-LUCA-LINU}      DRY LP
C 14:{CLG314 PICO/STOC-LUCA-PUM}       DRY LP
C 15:{CLG315 PICO/FRVI/FEID}           DRY LP
C 16:{CLG411 PICO/CAPE-LUCA-PUM}       DRY LP
C 17:{CLG412 PICO/CAPE-LUCA-PEEU}      DRY LP
C 18:{CLG413 PICO/CAPE-STOC-BASIN}     DRY LP
C 19:{CLG415 PICO/SIHY-CAPE}           DRY LP
C 20:{CLH111 PICO/POTR/FRVI}           WET LP
C 21:{CLM111 PICO/CANE-ELGL-WET}       WET LP
C 22:{CLM112 PICO/POPR}                WET LP
C 23:{CLM113 PICO/CAEU}                WET LP
C 24:{CLM114 PICO/CAAQ}                WET LP
C 25:{CLM211 PICO/ARUV-PUM}            WET LP
C 26:{CLM311 PICO/VAOC2-PUM}           WET LP
C 27:{CLM312 PICO/VAOC2/CAEU}          WET LP
C 28:{CLM313 PICO/SPDO-FORB}           WET LP
C 29:{CLM314 PICO/SPDO/CAEU}           WET LP
C 30:{CLM411 PICO/XETE-PUM}            DRY LP
C 31:{CLM911 PICO/PIEN/ELPA2}          WET LP
C 32:{CLS112 PICO/ARTR-RHYO}           DRY LP
C 33:{CLS211 PICO/PUTR/STOC-PUM}       DRY LP
C 34:{CLS212 PICO/PUTR/CAPE-PUM}       DRY LP
C 35:{CLS213 PICO/PUTR/FORB-PUM}       DRY LP
C 36:{CLS214 PICO/PUTR/FEID-PUM}       DRY LP
C 37:{CLS215 PICO/RICE-PUTR/STOC-PUM}  DRY LP
C 38:{CLS216 PICO/PUTR-RHYO}           DRY LP
C 39:{CLS311 PICO/ARNE}                DRY LP
C 40:{CLS412 PICO/VASC-PUM}            DRY LP
C 41:{CLS413 PICO/VASC-FORB}           DRY LP
C 42:{CLS414 PICO/VASC/CAPE}           DRY LP
C 43:{CLS911 PICO/CEVE-ARPA-PUM}       DRY LP
C 44:{CMS111 TSME/VASC-DES}            HEM
C 45:{CPC211 PIPO-JUOC/CELE/FEID}      DRY PP
C 46:{CPF111 PIPO/WYMO}                WET PP
C 47:{CPG212 PIPO/CAPE-FEID-LALA2}     WET PP
C 48:{CPH311 PIPO-POTR/PONE}           WET PP
C 49:{CPS111 PIPO/PUTR-ARTR/FEID}      DRY PP
C 50:{CPS112 PIPO/PUTR-ARTR/SIHY}      DRY PP
C 51:{CPS121 PIPO/ARTR/PONE}           DRY PP
C 52:{CPS211 PIPO/PUTR/FEID-PUM}       DRY PP
C 53:{CPS212 PIPO/PUTR/STOC-PUM}       DRY PP
C 54:{CPS213 PIPO/PUTR-ARPA/STOC-PUM}  DRY PP
C 55:{CPS214 PIPO/PUTR-ARPA/CAPE-PUM}  DRY PP
C 56:{CPS215 PIPO/PUTR/CAPE-PUM}       DRY PP
C 57:{CPS216 PIPO/PUTR/FEID-AGSP-PUM}  DRY PP
C 58:{CPS217 PIPO/PUTR-ARPA/FEID-PUM}  DRY PP
C 59:{CPS218 PIPO/PUTR/SIHY-RHYO}      DRY PP
C 60:{CPS311 PIPO/PUTR-CEVE/STOC-PUM}  DRY PP
C 61:{CPS312 PIPO/PUTR-CEVE/CAPE-PUM}  DRY PP
C 62:{CPS314 PIPO/PUTR-CEVE/FEID}      DRY PP
C 63:{CPS511 PIPO/SYAL-FLOOD}          WET PP
C 64:{CRG111 ABMAS/CAPE}               DRY MC
C 65:{CRS111 ABMAS/ARNE}               DRY MC
C 66:{CRS112 ABMAS-TSME/ARNE/CAPE}     DRY MC
C 67:{CRS311 ABMAS/CACH/CHUM-CAPE}     DRY MC
C 68:{CWC111 ABCO-PIPO-CADE/AMAL}      DRY MC
C 69:{CWC211 ABCO/CEVE-CACH/PTAQ}      DRY MC
C 70:{CWC212 ABCO/CEVE-CACH/CARU}      DRY MC
C 71:{CWC213 ABCO/CEVE/CAPE-PTAQ}      DRY MC
C 72:{CWC215 ABCO-PSME-CEVE/ARUV}      DRY MC
C 73:{CWC311 ABCO-PICO/STOC-CAPE}      DRY MC
C 74:{CWC411 ABCO-PIPO-PILA/RIVI}      DRY MC
C 75:{CWC412 ABCO-PIPO-PILA/ARPA}      DRY MC
C 76:{CWC911 PIEN-BOTTOMS}             WET LP
C 77:{CWF431 ABCO/CLUN}                WET MC
C 78:{CWH111 ABCO/CEVE-CACH}           DRY MC
C 79:{CWH112 ABCO/CACH-PAMY/CHUM}      DRY MC
C 80:{CWH211 ABCO-PIPO-POTR/CAPE}      DRY MC
C 81:{CWM111 ABCO/ALTE}                WET MC
C 82:{CWS112 ABCO/CEVE-ARPA-PUM}       DRY MC
C 83:{CWS113 ABCO/CEVE-ARPA/CAPE-PEEU} DRY MC
C 84:{CWS114 ABCO/CEVE-PUM}            DRY MC
C 85:{CWS115 ABCO/CEVE/CAPE}           DRY MC
C 86:{CWS116 ABCO/CEVE/CEPR-FRVI}      DRY MC
C 87:{CWS117 ABCO-PIPO/ARPA/BERE}      DRY MC
C 88:{CWS312 ABCO/SYAL/FRVI}           DRY MC
C 89:{CWS313 ABCO-PIPO/SYAL/STJA}      DRY MC
C 90:{HQM121 POTR/ELGL}                WET LP
C 91:{HQM411 POTR-PICO/SPDO/CAEU}      WET LP
C 92:{HQS221 POTR/SYAL/ELGL}           WET LP
C
      DATA IPASO /
     > 3,  3,  3,  4,  4,  4,  4,  4,  6,  6,
     > 7,  6,  6,  6,  6,  6,  6,  6,  6,  7,
     > 7,  7,  7,  7,  7,  7,  7,  7,  7,  6,
     > 7,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     > 6,  6,  6,  5,  1,  2,  2,  2,  1,  1,
     > 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     > 1,  1,  2,  3,  3,  3,  3,  3,  3,  3,
     > 3,  3,  3,  3,  3,  7,  4,  3,  3,  3,
     > 4,  3,  3,  3,  3,  3,  3,  3,  3,  7,
     > 7,  7 /
C
C     FIXED VALUES FOR INTERPOLATION FUNCTION
C
      DATA     Y / 0.0, 1.0 /
C
C     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
C     CLASS. THEY ARE RETURNED WITH THE WEIGHT
C
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /
C
C     THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
C     VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
C     AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
C     SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
C     SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.
C
      DATA ITYP / ICLSS * 0 /
C
C     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C     WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C     COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C     SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C
      DATA ((XPTS(I,J), J=1,2), I=1,ICLSS) /
     >   5., 15.,   ! FMD   1
     >   5., 15.,   ! FMD   2
     >   5., 15.,   ! FMD   3
     >   5., 15.,   ! FMD   4
     >   5., 15.,   ! FMD   5
     >   5., 15.,   ! FMD   6
     >   5., 15.,   ! FMD   7
     >   5., 15.,   ! FMD   8
     >   5., 15.,   ! FMD   9
     >  15., 30.,   ! FMD  10 ! shares with 11
     >  15., 30.,   ! FMD  11
     >  30., 60.,   ! FMD  12 ! shares with 14
     >  45.,100.,   ! FMD  13
     >  30., 60./   ! FMD  14
C
C     Initialize some of the other variables
C
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C
C     BEGIN ROUTINE
C
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

      IF (LUSRFM) RETURN

      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C
C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C
      IF (LUSRFM) RETURN
C
C     THE FOLLOWING FOREST CODES ARE USED
C     CALIFORNIA = REGION 5
C       505 KLAMATH NATIONAL FOREST
C       506 LASSEN
C       509 MODOC
C       511 PLUMAS
C     OREGON = REGION 6
C       601 DESCHUTES NATIONAL FOREST
C       602 FREMONT
C       620 WINEMA
C     INDUSTRY
C       701 ASSUMED TO BE CALIFORNIA (GARY DIXON)

      IF ((KODFOR .GE. 500 .AND. KODFOR .LT. 600)
     >    .OR. KODFOR .GE. 700) THEN   ! CALIFORNIA
C
C       COMPUTE WEIGHTS FOR DOMINANT SPECIES
C
        CALL SOSPDM(DBH,FMPROB,ISP,ITRN,WD)

        DO I = 1,MAXSP

          IF (WD(I) .GT. 0.0) THEN
C
C         DOMINANT: PINYON JUNIPER, JEFFREY PINE,
C         INCENSE CEDAR (WHITE PINE, SUGAR PINE)
C
            SELECT CASE (I)

              CASE (1,2,6,11)
C
C             RULE WAS <30=2, >30=9
C             CHANGED TO <25=2; >35=9; INTERPOLATE BETWEEN THE TWO
C
                X(1) = 25.0
                X(2) = 35.0
                J = 2

                WT1(J)   = ALGSLP(PERCOV,X,Y,2)
                WT1(J-1) = 1.0 - WT1(J)

                IF (WT1(1) .GT. 0.0) THEN
                  EQWT(2)  = EQWT(2) + WT1(1) * WD(I)
                ENDIF

                IF (WT1(2) .GT. 0.0) THEN
                  EQWT(9)  = EQWT(9) + WT1(2) * WD(I)
                ENDIF
C
C         PONDEROSA PINE, WHITE FIR, DOUGLAS-FIR,
C         MOUNTAIN HEMLOCK, ENGELMANN SPRUCE
C
C         THIS MODEL SELECTION IS DERIVED FROM AN AMBIGUOUS SET OF RULES
C         IN THE SORNEC DOCUMENT. AMBIGUOUS=NON-DICHOTOMOUS.
C
              CASE (10,4,3,5,8)

                XSNG = 0.0
                DO J = 1,NSNAG
                  IF (DBHS(J) .GT. RMSQD)
     >                XSNG = XSNG + DENIH(J) + DENIS(J)
                ENDDO

                ! WT1: AVERAGE HEIGHT (AVH FROM **PLOT.F77**)
                X(1) = 30.0
                X(2) = 40.0
                J = 2
                WT1(J)   = ALGSLP(AVH,X,Y,2)
                WT1(J-1) = 1.0 - WT1(J)

                ! WT2: PERCOV
                X(1) = 55.0
                X(2) = 65.0
                J = 2
                WT2(J)   = ALGSLP(PERCOV,X,Y,2)
                WT2(J-1) = 1.0 - WT2(J)

                ! WT3: SNAG DENSITY
                X(1) = 3.5
                X(2) = 4.5
                J = 2
                WT3(J)   = ALGSLP(XSNG,X,Y,2)
                WT3(J-1) = 1.0 - WT3(J)

                IF (WT1(2) .GT. 0.0 .OR. WT2(2) .GT. 0.0) THEN
                  IF (WT3(1) .GT. 0.0) THEN
                    EQWT(9) = EQWT(9) +
     >                         WD(I) * WT1(2) * WT2(1) * WT3(1)
                    EQWT(9) = EQWT(9) +
     >                         WD(I) * WT1(2) * WT2(2) * WT3(1)
                    EQWT(9) = EQWT(9) +
     >                         WD(I) * WT1(1) * WT2(2) * WT3(1)
                  ENDIF
                  IF (WT3(2) .GT. 0.0) THEN
                    EQWT(10) = EQWT(10) +
     >                         WD(I) * WT1(2) * WT2(1) * WT3(2)
                    EQWT(10) = EQWT(10) +
     >                         WD(I) * WT1(2) * WT2(2) * WT3(2)
                    EQWT(10) = EQWT(10) +
     >                         WD(I) * WT1(1) * WT2(2) * WT3(2)
                  ENDIF
                ENDIF
                IF (WT1(1) .GT. 0.0 .AND. WT2(1) .GT. 0.0) THEN
                  EQWT(5) = EQWT(5) + WD(I) * WT1(1) * WT2(1)
                ENDIF
C
C         LODGEPOLE PINE, RED FIR (SUBALPINE FIR)
C
C         USE SPECIES_CCF/STAND_CCF TO FIND IF THE STAND IS PURE PL OR PURE RED FIR
C         PURE = >95% OF CCF. OTHERWISE USE WK2 (PREDICTED MORTALITY) TO FIND OUT
C         IF LP MORTALITY (BY BASAL AREA) IS >50% OF TOTAL MORTALITY
C
              CASE (7,9)

                XA = 1.0
                IF (RELDEN .GT. 1.0E-6)
     >            XA = MAX(RELDSP(7),RELDSP(9))/RELDEN

                ! WT1: RELATIVE DENSITY PL OR RF
                X(1) = 0.85
                X(2) = 0.95
                J = 2
                WT1(J)   = ALGSLP(XA,X,Y,2)
                WT1(J-1) = 1.0 - WT1(J)

                IF (WT1(2) .GT. 0.0) THEN
                  EQWT(8) = EQWT(8) + WD(I) * WT1(2)
                ENDIF

                IF (WT1(1) .GT. 0.0) THEN

                  XM   = 0.0
                  XMLP = 0.0
                  DO J = 1,ITRN
                    XA = WK2(J) * DBH(J)**2
                    IF (ISP(I) .EQ. 7) XMLP = XMLP + XA
                    XM = XM + XA
                  ENDDO

                  XA = 0.0
                  IF (XM .GT. 1.0E-6) XA = XMLP/XM

                  ! WT2: RELATIVE PL MORTALITY
                  X(1) = 0.45
                  X(2) = 0.55
                  J = 2
                  WT2(J)   = ALGSLP(XA,X,Y,2)
                  WT2(J-1) = 1.0 - WT2(J)

                  IF (WT2(1) .GT. 0.0)
     >              EQWT(8)  = EQWT(8)  + WD(I) * WT1(1) * WT2(1)
                  IF (WT2(2) .GT. 0.0)
     >              EQWT(10) = EQWT(10) + WD(I) * WT1(1) * WT2(2)

                ENDIF
               
              CASE DEFAULT
             ! fm 8 is the default for R5 when none of the above
             ! species is dominant
                EQWT(8) = EQWT(8) + WD(I)
                
            END SELECT
          ENDIF
        ENDDO
C
      ELSE      ! OREGON
C
C       FIND ONE OF THE FOLLOWING GROUPS FROM AMONG THE 92 SORNEC PLANT
C       ASSOCIATION GROUPS:
C
C         DRY PONDEROSA PINE (1)
C         WET PONDEROSA PINE (2)
C         DRY MIXED CONIFER  (3)
C         WET MIXED CONIFER  (4)
C         HEMLOCK            (5)
C         DRY LODGEPOLE PINE (6)
C         WET LODGEPOLE PINE (7)
C         JUNIPER SHRUBLAND  (8)
C
        IPAG  = IPASO(ITYPE)
        DSTLG = REAL(MIN((IYR-HARVYR), (IYR-BURNYR)))

C     DETERMINE STRUCTURE CLASS IF IN LODGEPOLE PINE OR JUNIPER TYPES
        IF ((IPAG .EQ. 6) .OR. (IPAG .EQ. 7) .OR. (IPAG .EQ. 8)) THEN
          CALL FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,
     &                  IFMST,X3,FMPROB,FMICR)  
        ENDIF

        SELECT CASE (IPAG)

          CASE (1)              ! DRY PONDEROSA PINE

            ! WT1: PERCOV
            X(1) = 35.0
            X(2) = 45.0
            J = 2
            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(2) .GT. 0.0) THEN
              EQWT(9) = EQWT(9) + WT1(2)
            ENDIF
            IF (WT1(1) .GT. 0.0) THEN
C
C             REMOVE RULES THAT WERE ONLY IN EFFECT FOR 1 YEAR; IE FMD=8
C             IN YR 1 AFTER FIRE.- SARAH AND ELIZABETH (APR 2000)
C
              ! WT2: DSTLG
              X(1) = 5.0
              X(2) = 7.0
              J = 2
              WT2(J)   = ALGSLP(DSTLG,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              IF (WT2(1) .GT. 0.0) THEN
                EQWT(2) = EQWT(2) + WT1(1) * WT2(1)
              ENDIF
              IF (WT2(2) .GT. 0.0) THEN
                EQWT(6) = EQWT(6) + WT1(1) * WT2(2)
              ENDIF

            ENDIF

          CASE (2)  ! WET PONDEROSA PINE
C
C           ORIGINALLY >70=9, <70=5; CHANGE TO >80=9, <60=5
C
            ! WT1: PERCOV
            X(1) = 60.0
            X(2) = 80.0
            J = 2
            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              EQWT(5) = EQWT(5) + WT1(1)
            ENDIF
            IF (WT1(2) .GT. 0.0) THEN
              EQWT(9) = EQWT(9) + WT1(2)
            ENDIF
C
C     PLANT ASSOCIATION INDEX ITYPE IS USED TO IDENTIFY BITTERBRUSH (PUTR)
C     CEANOTHUS (CEVE) AND MANZANITA (ARPA,ARNE) IN WET PP
C     THESE RULES NEVER APPLY, SINCE BITTERBRUSH, CEANOTHUS/MANZANITA
C     PLANT ASSOCIATIONS NEVER OCCUR IN WET PPINE GROUPS.
C
C           IF (KODFOR .EQ. 602) THEN ! 'FREMONT'
C           ! IF BITTERBRUSH
C           IF (DISTLG .LE. 10) FMD = 8
C           ! IF CEANOTHUS/MANZANITA
C           IF (DISTLG .LE. 2) FMD = 5
C             ELSE
C           IF (DSTLG .LE. 2) THEN
C           ! IF CEANOTHUS/MANZANITA
C             FMD = 5
C           ! IF BITTERBRUSH
C             FMD = 6
C           ENDIF
C              ENDIF

          CASE (3)  ! DRY MIXED CONIFER
C
C           ORIGINALLY >50=8, <50=5; CHANGE TO >60=8, <40=5
C
            ! WT1: PERCOV
            X(1) = 40.0
            X(2) = 60.0
            J = 2
            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              EQWT(5) = EQWT(5) + WT1(1)
            ENDIF
            IF (WT1(2) .GT. 0.0) THEN
              EQWT(8) = EQWT(8) + WT1(2)
            ENDIF

          CASE (4)  ! WET MIXED CONIFER
C
C           ORIGINALLY >50=8, <50=5; CHANGE TO >60=8, <40=5
C
            ! WT1: PERCOV
            X(1) = 40.0
            X(2) = 60.0
            J = 2
            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              EQWT(5) = EQWT(5) + WT1(1)
            ENDIF
            IF (WT1(2) .GT. 0.0) THEN
              EQWT(8) = EQWT(8) + WT1(2)
            ENDIF

          CASE (5)  ! HEMLOCK

             EQWT(8) = 1.0

          CASE (6)  ! DRY LODGEPOLE PINE

            ! WT1: ACTBH - ACTIVE CROWN BASE HEIGHT
            X(1) = 2.0
            X(2) = 4.0
            J = 2
            WT1(J)   = ALGSLP(REAL(ACTCBH),X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              IF (IFMST .LE. 1) THEN
                EQWT(5) = EQWT(5) + WT1(1)
              ELSEIF (IFMST .GE. 2) THEN
                EQWT(10) = EQWT(10) + WT1(1)
              ENDIF
            ENDIF

            IF (WT1(2) .GT. 0.0) THEN
              EQWT(8) = EQWT(8) + WT1(2)
            ENDIF

          CASE (7)  ! WET LODGEPOLE PINE

            IF (IFMST .LE. 1) THEN

              ! WT1: ACTBH - ACTIVE CROWN BASE HEIGHT
              X(1) = 2.0
              X(2) = 4.0
              J = 2
              WT1(J)   = ALGSLP(REAL(ACTCBH),X,Y,2)
              WT1(J-1) = 1.0 - WT1(J)

              IF (WT1(1) .GT. 0.0) THEN
                EQWT(5) = EQWT(5) + WT1(1)
              ENDIF
              IF (WT1(2) .GT. 0.0) THEN
                EQWT(8) = EQWT(8) + WT1(2)
              ENDIF

            ELSEIF (IFMST .GT. 1 .AND. IFMST .LT. 5) THEN

              EQWT(8) = 1.0

            ELSE

              ! WT1: PERCOV
              X(1) = 40.0
              X(2) = 60.0
              J = 2
              WT1(J)   = ALGSLP(PERCOV,X,Y,2)
              WT1(J-1) = 1.0 - WT1(J)

              IF (WT1(1) .GT. 0.0) THEN
                EQWT(3) = EQWT(3) + WT1(1)
              ENDIF
              IF (WT1(2) .GT. 0.0) THEN
                EQWT(8) = EQWT(8) + WT1(2)
              ENDIF

            ENDIF

           CASE (8)  ! JUNIPER SHRUBLAND
C
C           ORIGINALLY WAS >70=8, <70=1 OR 6, DEPENDING ON STRUCTURAL STAGE
C           CHANGE TO >80=8, <60=1 OR 6
C
            ! WT1: PERCOV
            X(1) = 60.0
            X(2) = 80.0
            J = 2
            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              IF(IFMST .LE. 1) THEN
                EQWT(1) = EQWT(1) + WT1(1)
              ELSE
                EQWT(6) = EQWT(6) + WT1(1)
              ENDIF
            ENDIF
            IF (WT1(2) .GT. 0.0) THEN
              EQWT(8) = EQWT(8) + WT1(2)
            ENDIF

        END SELECT

      ENDIF  ! END OF OREGON RULES
C
C     END OF DETAILED LOW FUEL MODEL SELECTION
C
C     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 AND 14 ARE
C     CANDIDATE MODELS, SHARING WITH 10 AND 12 RESPECTIVELY. THE
C     WEIGHT OF THE SHARED RELATIONSHIP DECLINES FROM PURE 11 INITIALLY,
C     TO PURE 10 AFTER THE PERIOD EXPIRES. SIMILARLY, COMPUTE WEIGHT FOR
C     MODEL 14 ACTIVITY FUELS, SHARED WITH CURRENT MODEL 12. THE
C     RELATIONSHIP CHANGES IN THE SAME WAS AS THE 10/11 FUELS.
C
      AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
      IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
        LATFUEL  = .TRUE.
        EQWT(11) = AFWT
        EQWT(14) = AFWT
        IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
      ENDIF
      IF (.NOT. LATFUEL) AFWT = 0.0
C
C     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
C
      EQWT(10) = 1.0 - AFWT
      EQWT(12) = 1.0 - AFWT
      EQWT(13) = 1.0
C
C     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C     FROM THE HIGHEST TO LOWEST, SET FMD (UGING THE HIGHEST WEIGHT)
C
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)

      RETURN
      END

C--------------------------------------------------------------------
C
C     COMPUTE THE DOMINANT SPECIES WTIH WEIGHTS
C
C     D  = REAL VECTOR OF DBH
C     P  = REAL VECTOR OF STEMS/ACRE
C     SP = INTEGER VECTOR OF SPECIES
C     N  = INTEGER LENGTH OF D(), P() AND SP VECTORS
C     WDOM = WEIGHTED DOMINANT CATEGORY [ RETURNED ]
C
      SUBROUTINE SOSPDM(D,P,SP,N,WDOM)
      IMPLICIT NONE

      INCLUDE 'PRGPRM.F77'

      REAL    D,P,WDOM
      INTEGER SP,N
      DIMENSION D(N),P(N),SP(N),WDOM(MAXSP)

      INTEGER I,J,K,PASS,BASP
      REAL    DT,XRAN,JITTER
      REAL    XBA(MAXSP)
      DOUBLE PRECISION SAVESO

      DATA PASS /50/, JITTER /0.2/
C/
C     ZERO BINS TO WEIGHTED DOMINANT CATEGORY AND COMPLEXITY INDEX
C
      DO I = 1,MAXSP
        WDOM(I) = 0.0
      ENDDO
C
C     POPULATE BASIC SIZE CATEGORIES USING UNIFORM RANDOM JITTER.
C     THE IDEA HERE IS TO SAMPLE DIAMETERS JITTERED
C     BY 20%, THEN RECOMPUTE A WEIGHTED DOMINANT CATEGORY.
C
      CALL RANNGET(SAVESO) 
C 
      DO J = 1,PASS
C
C     ZERO BINS TO HOLD BA-BASED WEIGHTS FOR THE SPECIES CATEGORIES
C
        DO I = 1,MAXSP
          XBA(I) = 0.0
        ENDDO

        DO I = 1,N
          CALL RANN(XRAN)
          DT = D(I) * (1.0 + JITTER * ((XRAN * 2.0) - 1.0))
          K = SP(I)
          XBA(K) = XBA(K) + (DT * DT * P(I))
        ENDDO

        BASP = 7  ! LP IS DEFAULT FOR NO BA
        DO K = 1,MAXSP
          IF (XBA(K) .GT. XBA(BASP)) BASP = K
        ENDDO

        WDOM(BASP) = WDOM(BASP) + 1.0
      ENDDO

      CALL RANNPUT(SAVESO)

      DO I = 1,MAXSP
        WDOM(I) = WDOM(I)/REAL(PASS)
      ENDDO

      RETURN
      END
