      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C FIRE-CI $Id$
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
*     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
*     FUEL MODEL
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
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
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'SSTGMC.F77'

C     LOCAL VARIABLE DECLARATIONS
C
C     NUMBER OF POTENTIAL FIRE MODELS
C
      INTEGER   ICLSS
      PARAMETER(ICLSS = 14)
C
      INTEGER   IYR, FMD

      INTEGER   I,J,K,II,ICT
      INTEGER   IPTR(ICLSS),ITYP(ICLSS)
      REAL      XPTS(ICLSS,2),EQWT(ICLSS),WT1(4),WT2(4)
      REAL      STNDBA,X(2),Y(2),AFWT
      REAL      ALGSLP
      REAL      PRLONG,CCGF,CRGF,FMTPA
      REAL      CAREA,CWIDTH,TOTCRA
      LOGICAL   DEBUG,LCRGF
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
C     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C     MUST SUM TO 1, WRT EACH OTHER
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
C
C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C
      IF (LUSRFM) RETURN

      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C
C     COMPUTE TOTAL STAND BASAL AREA AND PROPORTION IN LONG-NEEDLE
C     SPECIES: WHITE PINE AND PONDEROSA PINE
C
      STNDBA = 0.0
      PRLONG = 0.0
      DO I = 1, MAXSP
        STNDBA = STNDBA + FMTBA(I)
        SELECT CASE (I)
          CASE (1,10)
            PRLONG = PRLONG + FMTBA(I)
        END SELECT
      ENDDO

      IF (PRLONG .GT. 0.01 .AND. STNDBA .GT. 0.01) THEN
        PRLONG = PRLONG / STNDBA
      ELSE
        PRLONG = 0.0
      ENDIF
C
C     3 - RESOLVE DYNAMIC FMD
C
      DO I = 1,4
        WT1(I) = 0.0
      ENDDO
C
C     FIND POTENTIAL VEGETATION GROUP (PVG) **FMCMBA**
C     **NOTE** % SHORT VS %LONG: 100% SHORT -> 8; 100% LONG -> 9
C     IS FREQUENTLY USED TO WEIGHT THE SELECTION OF MODEL 8/9
C
      CALL CIPVG(ICT)
      SELECT CASE (ICT)
C
C       1 = DRY PONDEROSA PINE - XERIC DOUGLAS-FIR,
C       2 = WARM/DRY DOUGLAS-FIR - MOIST PONDEROSA PINE,
C       3 = COOL MOIST DOUGLAS-FIR
C       4 = COOL DRY DOUGLAS-FIR
C
        CASE (1:4)

          SELECT CASE (ICT)
            CASE (1)
              K = 1
            CASE (2)
              CALL CIS9B(I)  ! IS THIS NINEBARK OR SNOWBERRY? **FMCBA**
              IF (I .EQ. 1) THEN
                K = 5
              ELSE
                K = 2
              ENDIF
            CASE (3)
              K = 5
            CASE (4)
              K = 2
          END SELECT

          X(1) =  30.0
          X(2) =  50.0
          J = 2

          WT1(J)   = ALGSLP(PERCOV,X,Y,2)
          WT1(J-1) = 1.0 - WT1(J)

          IF (WT1(1) .GT. 0.0) THEN
            EQWT(K) = EQWT(K) + WT1(1)
          ENDIF

          IF (WT1(2) .GT. 0.0) THEN
            EQWT(9) = EQWT(9) + WT1(2) * PRLONG
            EQWT(8) = EQWT(8) + WT1(2) * (1.0 - PRLONG)
          ENDIF
C
C       5 = DRY GRAND FIR
C       6 = WET GRAND FIR
C
        CASE (5:6)

          SELECT CASE (ICT)
            CASE (5)
              K = 2
            CASE (6)
              K = 5
          END SELECT
C
C         COMPUTE DETAILS ON GF < 3" DBH: DO THEY HAVE, ON
C         AVERAGE, >75% CROWN AND WHAT %CC DO THEY FORM?
C
          LCRGF  = .FALSE.
          CCGF   = 0.0
          IF (ISCT(4,1) .GT. 0) THEN
            CRGF   = 0.0
            FMTPA  = 0.0
            TOTCRA = 0.0
            DO II = ISCT(4,1),ISCT(4,2)
              I = IND1(II)
              IF (DBH(I) .LE. 3.0) THEN
                CRGF   = CRGF + (FMPROB(I) * ICR(I))
                FMTPA  = FMTPA + FMPROB(I)
                CWIDTH=CRWDTH(I)
                CAREA  = 3.1415927 * CWIDTH * CWIDTH / 4.0
                TOTCRA = TOTCRA + CAREA * FMPROB(I)
              ENDIF
            ENDDO
            IF(FMTPA .LE. 0.)THEN
              CRGF=0.
            ELSE
              CRGF  = CRGF / FMTPA
            ENDIF
            LCRGF = (CRGF .GE. 75.0)
            CCGF  = 100.0 * (1.0 - EXP(-TOTCRA/43560.))
          ENDIF

          IF (LCRGF) THEN

            X(1) =  50.0
            X(2) =  70.0
            J = 2

            WT1(J)   = ALGSLP(CCGF,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN

              X(1) =  40.0
              X(2) =  60.0
              J = 2

              WT2(J)   = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              IF (WT2(1) .GT. 0.0) THEN
                EQWT(K) = EQWT(K) + WT1(1) * WT2(1)
              ENDIF

              IF (WT2(2) .GT. 0.0) THEN
                EQWT(9) = EQWT(9) + WT1(1) * WT2(2) * PRLONG
                EQWT(8) = EQWT(8) + WT1(1) * WT2(2) * (1.0 - PRLONG)
              ENDIF

            ENDIF
C
C           NOTE DISCONTINUITY: MODEL SWITCHES COMPLETELY TO 5 WHEN WT1(2) = 1.0
C           WHILE ITS APPROACH TO THIS LIMIT IS ALONG THE 9/8 TRAJECTORY. PLUS
C           CA CHANGE.
C
            IF (WT1(2) .GT. 0.0) THEN

              IF (WT1(2) .EQ. 1.0) THEN

                EQWT(5) = EQWT(5) + WT1(2)

              ELSE

                X(1) =  40.0
                X(2) =  60.0
                J = 2

                WT2(J)   = ALGSLP(PERCOV,X,Y,2)
                WT2(J-1) = 1.0 - WT2(J)

                IF (WT2(1) .GT. 0.0) THEN
                  EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN
                  EQWT(9) = EQWT(9) + WT1(2) * WT2(2) * PRLONG
                  EQWT(8) = EQWT(8) + WT1(2) * WT2(2) * (1.0 - PRLONG)
                ENDIF

              ENDIF
            ENDIF

          ELSE  ! NO SIG LONG-CROWN GF UNDERSTOREY

            X(1) =  40.0
            X(2) =  60.0
            J = 2

            WT1(J)   = ALGSLP(PERCOV,X,Y,2)
            WT1(J-1) = 1.0 - WT1(J)

            IF (WT1(1) .GT. 0.0) THEN
              EQWT(K) = EQWT(K) + WT1(1)
            ENDIF

            IF (WT1(2) .GT. 0.0) THEN
              EQWT(9) = EQWT(9) + WT1(2) * PRLONG
              EQWT(8) = EQWT(8) + WT1(2) * (1.0 - PRLONG)
            ENDIF

          ENDIF
C
C       7  = WARM DRY SUBALPINE FIR
C       8  = WET SUBALPINE FIR
C       9  = HIGH WATER TABLE SUBALPINE FIR
C       10 = PERSISTENT LODGEPOLE PINE
C       11 = HIGH ELEVATION SUBALPINE FIR W/ WHITEBARK PINE
C
        CASE (7:11)

          EQWT(8) = 1.0

      END SELECT
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
        LATFUEL = .TRUE.
        EQWT(11)  = AFWT
        EQWT(14)  = AFWT
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
C     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)

      RETURN
      END

