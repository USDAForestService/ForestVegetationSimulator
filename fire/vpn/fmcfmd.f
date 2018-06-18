      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE

C   **FMCFMD FIRE-PN-DATE OF LAST REVISION: 03/10/05

*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
*     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
*     FUEL MODEL
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
*     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
*
***********************************************************************

C     PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'SSTGMC.F77'

C     LOCAL VARIABLE DECLARATIONS

C     NUMBER OF HABITAT TYPES IN **HABTYP*

      INTEGER    MXR6CODE
      PARAMETER (MXR6CODE=75)

C     NUMBER OF POTENTIAL FIRE MODELS

      INTEGER   ICLSS
      PARAMETER(ICLSS = 13)

C     COVER METAGROUP ENUMS

      INTEGER SFCT,DFCT,MHCT,RACT,LPCT,WOCT,UBCT

      PARAMETER(
     &  SFCT   =  1, ! SF,WH,SS,RC
     &  DFCT   =  2, ! DF,GF,WP
     &  MHCT   =  3, ! MH,AF,WB
     &  RACT   =  4, ! RA
     &  LPCT   =  5, ! LP
     &  WOCT   =  6, ! WO,GC
     &  UBCT   =  6) ! UPPER BOUND

      INTEGER   FMD
      INTEGER   IYR,I,J,K,ICT(2),ICOV,IWET
      INTEGER   IPTR(ICLSS), ITYP(ICLSS),INDX(UBCT),IXS(MAXTRE)
      REAL      XPTS(ICLSS,2), EQWT(ICLSS), WT1(2), WT2(4), WT3(3)
      REAL      ICTWT(2),TARGBA,SUMBA,LTREES,TREEBA,SNG1
      REAL      CTBA(UBCT), STNDBA, X(2), Y(2), X1, QMD80, AFWT
      REAL      ALGSLP
      LOGICAL   DEBUG, LQMD, LGRASS, LFORB, LSHRUB, LWET

C     FIXED VALUES FOR INTERPOLATION FUNCTION

      DATA     Y / 0.0, 1.0 /

C     These are the integer tags associated with each fire model
C     class. They are returned with the weight

      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13 /

c     These are 0 for regular lines, -1 for horizontal and 1 for
c     vertical lines. If any of the lines defined by xpts() are of
c     an unusual variety, this must be entered here so that
c     special logic can be invoked.  In this case, all the line
c     segments have a |slope| that is > 0 and less than inif.

      DATA ITYP / ICLSS * 0 /

C     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C       WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C       COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C       SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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
     >  30., 60.,   ! FMD  12
     >  45.,100./   ! FMD  13

C     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C     MUST SUM TO 1, WRT EACH OTHER

      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO

C     BEGIN ROUTINE

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

C     DETERMINE WHETHER HABITAT IS FORB, GRASS OR SHRUB; INVALID MAPPING
C     DEFAULTS TO GRASS

      ICOV = 0
      CALL PNFGS(ICOV)
      LGRASS = .FALSE.
      LFORB  = .FALSE.
      LSHRUB = .FALSE.
      SELECT CASE (ICOV)
        CASE (1)
          LFORB  = .TRUE.
        CASE (0,2)
          LGRASS = .TRUE.
        CASE (3)
          LSHRUB = .TRUE.
      END SELECT

C     DETERMINE WHETHER HABITAT IS MOIST, AFFECTS LPCT RULES
C     DEFAULTS TO .FALSE. ENTRY POINT IN **FMCBA**

      CALL PNWET(IWET)
      LWET = .FALSE.
      IF (IWET .EQ. 2) LWET = .TRUE.

C     ZERO OUT THE COVER GROUPS AND COMPUTE TOTAL STAND BASAL AREA

      DO I = 1,UBCT
        CTBA(I) = 0.0
      ENDDO
      STNDBA = 0.0
      DO I = 1,MAXSP
        STNDBA = STNDBA + FMTBA(I)
      ENDDO

C     FIND BA IN EACH COVER TYPE GROUP; ANYTHING NOT SPECIFIED IS
C     PUT WITH DOUGLAS-FIR; IN THE WC-FFE SITKA SPRUCE IS POOLED WITH
C     ENGELMANN = 10. CASE-STATEMENT VALUES CAN'T BE VARIABLES, SO
C     THE WHOLE THING IS DUPLICATED, CHANGING THE 10 AND 6 FOR THE
C     FIRST CASE...

      IF (VARACD .EQ. 'WC') THEN
        DO I = 1,MAXSP
          SELECT CASE (I)
            CASE (1,19,10,18)
              CTBA(SFCT) = CTBA(SFCT) + FMTBA(I)
            CASE (20,4,31)
              CTBA(MHCT) = CTBA(MHCT) + FMTBA(I)
            CASE (22)
              CTBA(RACT) = CTBA(RACT) + FMTBA(22)
            CASE (11)
              CTBA(LPCT) = FMTBA(11)
            CASE (28,25)
              CTBA(WOCT) = FMTBA(28) + FMTBA(25)
            CASE DEFAULT  ! 16,3,14 + minor
              CTBA(DFCT) = CTBA(DFCT) + FMTBA(I)
          END SELECT
        ENDDO
      ELSE
        DO I = 1,MAXSP
          SELECT CASE (I)
            CASE (1,19,6,18)
              CTBA(SFCT) = CTBA(SFCT) + FMTBA(I)
            CASE (20,4,31)
              CTBA(MHCT) = CTBA(MHCT) + FMTBA(I)
            CASE (22)
              CTBA(RACT) = CTBA(RACT) + FMTBA(22)
            CASE (11)
              CTBA(LPCT) = FMTBA(11)
            CASE (28,25)
              CTBA(WOCT) = FMTBA(28) + FMTBA(25)
            CASE DEFAULT  ! 16,3,14 + minor
              CTBA(DFCT) = CTBA(DFCT) + FMTBA(I)
          END SELECT
        ENDDO
      ENDIF

C     2 - FIND AND SAVE THE TWO DOMINANT GROUPS; RESCALING
C     SO THEY SUM TO 1.0

      IF ((ITRN .GT. 0) .AND. (STNDBA.GT.0.001)) THEN
        CALL RDPSRT (UBCT,CTBA,INDX,.TRUE.)
        X1 = 0.0
        DO I = 1,2
          ICT(I)   = INDX(I)
          ICTWT(I) = CTBA(INDX(I))
          X1       = X1 + ICTWT(I)
        ENDDO
        DO I = 1,2
          ICTWT(I) = ICTWT(I)/X1
        ENDDO
      ELSE
        ICT(1)   = OLDICT
        ICT(2)   = OLDICT2
        ICTWT(1) = OLDICTWT(1)
        ICTWT(2) = OLDICTWT(2)
      ENDIF
      OLDICT      = ICT(1)
      OLDICT2     = ICT(2)
      OLDICTWT(1) = ICTWT(1)
      OLDICTWT(2) = ICTWT(2)

C     COMPUTE QMD OF LOWER 80% OF BA TREES IF EITHER SFCT OR DFCT IS A
C     COVER TYPE (THIS ALGORITHM BASED ON WS-FFE **CWHR**)

      LQMD = .FALSE.
      DO I = 1,2
        IF ((ICT(I) .EQ. SFCT) .OR. (ICT(I) .EQ. DFCT)) THEN
          LQMD = .TRUE.
        ENDIF
      ENDDO
      QMD80 = 0.0
      IF (LQMD) THEN
        TARGBA = STNDBA * 0.80
        SUMBA  = 0.0
        LTREES = 0.0
        IF (ITRN .GT. 0) CALL RDPSRT(ITRN,DBH,IXS,.TRUE.)
        DO J = ITRN,1,-1
          I = IXS(J)
          TREEBA = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          IF ((SUMBA + TREEBA) .LT. TARGBA) THEN
            SUMBA  = SUMBA + TREEBA
            QMD80  = QMD80 + (FMPROB(I) * DBH(I) * DBH(I))
            LTREES = LTREES + FMPROB(I)
          ELSE
            IF (TREEBA .GT. 0) THEN
            	SNG1 = (TARGBA - SUMBA) / TREEBA
            ELSE
            	SNG1 = 0
            ENDIF
            LTREES = LTREES + (FMPROB(I) * SNG1)
            QMD80  = QMD80 + (DBH(I) * DBH(I) * FMPROB(I) * SNG1)
            GOTO 100
          ENDIF
        ENDDO
  100 CONTINUE
      IF (LTREES .GT. 0.0) QMD80 = SQRT(QMD80/LTREES)
      ENDIF

C     3 - RESOLVE DYNAMIC FMD
C     RULES ARE BASED ON DIAGRAMS DEVELOPED AT PN/WC FFE WORKSHOP. TRANSITION
C     CUTPOINTS  WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE
C     ADJUSTED SO THAT THEY BEGIN TO CHANGE -10% FROM THE CUTPOINT AND FINISH
C     CHANGING +10% ABOVE THE CUTPOINT. SIZE [ QMD ] CUTPOINTS ARE ADJUSTED
C     USING -1" AND +1" ABOUT THE CUTPOINT. THESE TRANSITIONS INSURE THAT THE
C     MODEL BEHAVIOUR DOES NOT (USUALLY) EXHIBIT EXTREME JUMPS DURING STAND
C     DEVELOPMENT

C     WT1 - WEIGHT ASSIGNED TO THE COVER TYPE (ONLY TOP 2)
      DO I = 1,2
        WT1(I) = ICTWT(I)
      ENDDO

      DO I = 1,2
        DO J = 1,UBCT
          IF (ICT(I) .EQ. J .AND. WT1(I) .GT. 0.0) THEN

            SELECT CASE (J)

              ! SF,WH,SS,RC
              CASE (SFCT)

                X(1) =  3.0
                X(2) =  5.0
                K = 2
                WT2(K)   = ALGSLP(QMD80,X,Y,2)
                WT2(K-1) = 1.0 - WT2(K)

                IF (WT2(1) .GT. 0.0) THEN
                  EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN

                  DO K = 1,3
                    WT3(K) = 0.0
                  ENDDO

                  IF (PERCOV .LT. 60.0) THEN
                    X(1) = 40.0
                    X(2) = 60.0
                    K = 2
                  ELSEIF (PERCOV .GE. 60.0 ) THEN
                    X(1) = 70.0
                    X(2) = 90.0
                    K = 3
                  ENDIF
                  WT3(K)   = ALGSLP(PERCOV,X,Y,2)
                  WT3(K-1) = 1.0 - WT3(K)

                  IF (WT3(1) .GT. 0.0) THEN
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(1)
                  ENDIF

                  IF (WT3(2) .GT. 0.0) THEN
                    IF (LSHRUB) THEN
                      EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(2)
                    ELSE
                      EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(2)
                    ENDIF
                  ENDIF

                  IF (WT3(3) .GT. 0.0) THEN
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(3)
                  ENDIF

                ENDIF

              ! DF,GF,WP
              CASE (DFCT)

                X(1) =  3.0
                X(2) =  5.0
                K = 2
                WT2(K)   = ALGSLP(QMD80,X,Y,2)
                WT2(K-1) = 1.0 - WT2(K)

                IF (WT2(1) .GT. 0.0) THEN
                  EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN

                  DO K = 1,3
                    WT3(K) = 0.0
                  ENDDO

                  X(1) = 70.0
                  X(2) = 90.0
                  K = 2
                  WT3(K)   = ALGSLP(PERCOV,X,Y,2)
                  WT3(K-1) = 1.0 - WT3(K)

                  IF (WT3(1) .GT. 0.0) THEN
                    IF (LGRASS) THEN
                      EQWT(2) = EQWT(2) + WT1(I) * WT2(2) * WT3(1)
                    ELSEIF (LSHRUB) THEN
                      EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(1)
                    ELSEIF (LFORB) THEN
                      EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(1)
                    ENDIF
                  ENDIF

                  IF (WT3(2) .GT. 0.0) THEN
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(2)
                  ENDIF

                ENDIF

               ! MH,AF,WB
              CASE (MHCT)

                DO K = 1,2
                  WT2(K) = 0.0
                ENDDO

                X(1) = 70.0
                X(2) = 90.0
                K = 2
                WT2(K)   = ALGSLP(PERCOV,X,Y,2)
                WT2(K-1) = 1.0 - WT2(K)

                IF (WT2(1) .GT. 0.0) THEN
                  IF (LSHRUB) THEN
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                  ELSE
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(1)
                  ENDIF
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN
                  EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
                ENDIF

              ! RA
              CASE (RACT)

                DO K = 1,2
                  WT2(K) = 0.0
                ENDDO

                X(1) = 40.0
                X(2) = 60.0
                K = 2
                WT2(K)   = ALGSLP(PERCOV,X,Y,2)
                WT2(K-1) = 1.0 - WT2(K)

                IF (WT2(1) .GT. 0.0) THEN
                  EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN
                  EQWT(9) = EQWT(9) + WT1(I) * WT2(2)
                ENDIF

              ! LP
              CASE (LPCT)

                IF (LWET) THEN
                  DO K = 1,2
                    WT2(K) = 0.0
                  ENDDO

                  X(1) = 40.0
                  X(2) = 60.0
                  K = 2
                  WT2(K)   = ALGSLP(PERCOV,X,Y,2)
                  WT2(K-1) = 1.0 - WT2(K)

                  IF (WT2(1) .GT. 0.0) THEN
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                  ENDIF

                  IF (WT2(2) .GT. 0.0) THEN
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
                  ENDIF

                ELSE
                  EQWT(5) = EQWT(5) + WT1(I)
                ENDIF

              ! WO,GC
              CASE (WOCT)

                DO K = 1,2
                  WT2(K) = 0.0
                ENDDO

                X(1) = 40.0
                X(2) = 60.0
                K = 2
                WT2(K)   = ALGSLP(PERCOV,X,Y,2)
                WT2(K-1) = 1.0 - WT2(K)

                IF (WT2(1) .GT. 0.0) THEN
                  IF (LSHRUB) THEN
                    EQWT(2) = EQWT(2) + WT1(I) * WT2(1)
                  ELSE
                    EQWT(1) = EQWT(1) + WT1(I) * WT2(1)
                  ENDIF
                ENDIF

                IF (WT2(2) .GT. 0.0) THEN
                  IF (LSHRUB) THEN
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(2)
                  ELSEIF (LGRASS) THEN
                    EQWT(2) = EQWT(2) + WT1(I) * WT2(2)
                  ELSEIF (LFORB) THEN
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
                  ENDIF
                ENDIF

            END SELECT

          ENDIF  ! NONZERO WTS
        ENDDO  ! LOOP OVER COVER TYPES
      ENDDO ! LOOP OVER TWO MAJOR COVER TYPES

C     END OF DETAILED LOW FUEL MODEL SELECTION

C     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
C     CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
C     RELATIONSHIP DECLINES FROM PURE 11 INITIALLY TO PURE 10 AFTER
C     THE PERIOD EXPIRES.

      AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
      IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
        LATFUEL  = .TRUE.
        EQWT(11) = AFWT
        IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
      ENDIF
      IF (.NOT. LATFUEL) AFWT = 0.0

C     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC

      EQWT(10) = 1.0 - AFWT
      EQWT(12) = 1.0
      EQWT(13) = 1.0

C     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)

      RETURN
      END
