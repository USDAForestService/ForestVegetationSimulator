      SUBROUTINE FMSHRUB (IYR,FMD)
      IMPLICIT NONE
C----------
C FIRE-WS $Id$
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMCFMD IN CA VARIANTS (WS/NC/IC)
C
C     PURPOSE:
C
C     A POOR MAN'S SHRUB MODEL:
C
C     THIS ROUTINE IS CALLED AFTER THE COMPUTATIONS DONE BY **FMDYN**, AND
C     DETECTS A SWITCH FROM FM8/9 TO FM5/25 IN A YEAR IN WHICH A MANAGEMENT OR
C     FIRE DISTURBANCE OCCURS. THE SWITCH TO A BRUSH MODEL IS DELAYED AS FOLLOWS
C
C     FM8/9 -> FM5  -- SPREAD OVER 5 YRS
C     FM8/9 -> FM26 -- SPREAD OVER 15 YRS; GOING TO FM5 AT 5 YRS AND THEN
C                      TO FM26 AT 15 YRS
C
C     RATIO OF 8/9 IS STORED AND USED FOR SHIFTING PROPORTIONALLY WHEN
C     BOTH ARE PRESENT. THE ACTUAL AMOUNT TO REARRANGE CHANGES WITH THE
C     PREDICTED AMOUNT OF THE SHRUB MODEL (5 AND/OR 26). SINCE FUEL
C     MODEL WEIGHT AND NUMBER MAY CHANGE WHEN THIS ROUTINE IS CALLED
C     FWT(), FMOD() & NFMODS MUST ALL BE RE-COMPUTED. A CUT/PASTE FROM
C     **FMDYN** IS USED TO DO THIS
C
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     IYR:     CURRENT YEAR
*     FMD:     FUEL MODEL NUMBER
*
***********************************************************************

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'

      LOGICAL  L89,L5,L26,L526
      LOGICAL  FMCHKFWT
      INTEGER  IYR,FMD
      INTEGER  I,J,K,JYR
      INTEGER  I5,I8,I9,I26
      INTEGER  FMOD2(MXFMOD+3),INDX(MXFMOD+3)
      REAL     R8,R9,FWT5,FWT8,FWT9,X
      REAL     XV1(2),XV2(2),YV(2),FWT2(MXFMOD+3)
      REAL     ALGSLP

      DATA     XV1 / 0.0, 5.0 /
      DATA     XV2 / 5.0,15.0 /
      DATA     YV  / 1.0, 0.0 /

      L89  = .FALSE.
      L5   = .FALSE.
      L26  = .FALSE.
      L526 = .FALSE.

      I5   = 0
      I8   = 0
      I9   = 0
      I26  = 0

      R8   = 0.0
      R9   = 0.0

      DO I=1,MXFMOD+3
        FMOD2(I) = 0
        FWT2(I)  = 0.0
      ENDDO

      DO I = 1,MXFMOD
        IF (FMOD(I) .EQ.  5) THEN
          L5   = .TRUE.
          L526 = .TRUE.
          I5   = I
        ENDIF
        IF (FMOD(I) .EQ.  8) THEN
          L89  = .TRUE.
          I8   = I
        ENDIF
        IF (FMOD(I) .EQ.  9) THEN
          L89  = .TRUE.
          I9   = I
        ENDIF
        IF (FMOD(I) .EQ. 26) THEN
          L26  = .TRUE.
          L526 = .TRUE.
          I26  = I
        ENDIF
      ENDDO
C
C     SET YEAR IN WHICH FM CHANGE BEGINS: MUST HAVE 5/26 IN
C     CURRENT YR AND HAVE BEEN FM8/9 WITH NO 5/26 IN PREV YR;
C     AT LEAST 10% CHANGE IN %CC MUST ALSO HAVE TAKEN PLACE
C
      IF ((CCCHNG .GE. CCCRIT) .AND. LPRV89 .AND. L526) THEN
        FM89YR  = IYR
        LATSHRB = .TRUE.
      ENDIF
C
C     RECORD FM8 AND FM9 WEIGHTS
C
      LPRV89 = L89 .AND. (.NOT. L526)
      IF (LPRV89) THEN
        IF (I8 .GT. 0) PRV8 = FWT(I8)
        IF (I9 .GT. 0) PRV9 = FWT(I9)
      ENDIF
C
C     SHIFT MODELS IN "ACTIVE SHRUB" PERIOD
C
      IF (LATSHRB) THEN

        FWT5  = 0.0             ! TEMP STORAGE FOR NEW WTS
        FWT8  = 0.0
        FWT9  = 0.0

        R8   = PRV8/(PRV8+PRV9) ! PROPN IN FM8
        R9   = 1.0 - R8         ! PROPN IN FM9

        JYR = IYR-FM89YR

        IF (L5) THEN
          X          = ALGSLP(AMAX0(MIN(JYR, 5),0),XV1,YV,2)
          FWT8       = FWT8 + (FWT(I5) * X * R8)
          FWT9       = FWT9 + (FWT(I5) * X * R9)
          FWT(I5)    = FWT(I5) - (FWT(I5) *  X)
        ENDIF
        IF (L26) THEN
          IF (JYR .LE. 5) THEN
            X        = ALGSLP(AMAX0(MIN(JYR, 5),0),XV1,YV,2)
            FWT8     = FWT8 + (FWT(I26) * X * R8)
            FWT9     = FWT9 + (FWT(I26) * X * R9)
            FWT5     = FWT5 + (FWT(I26) * (1.0 - X))
            FWT(I26) = 0.0
          ELSE
            X        = ALGSLP(AMAX0(MIN(JYR,15),0),XV2,YV,2)
            FWT5     = FWT5 + (FWT(I26) * X)
            FWT(I26) = FWT(I26) - (FWT(I26) * X)
          ENDIF
        ENDIF
C
C       RECONFIGURE THE FMOD AND FWT ARRAYS: SOME SLOTS
C       PREVIOUSLY OCCUPIED ARE NOW EMPTY, AND V/VERSA. BEGIN BY
C       MOVING WEIGHT TO ANY MODELS ALREADY IN THE FMOD LIST
C
        IF (I5 .NE. 0) THEN
          FWT(I5) = FWT(I5) + FWT5
          FWT5 = 0.0
        ENDIF
        IF (I8 .NE. 0) THEN
          FWT(I8) = FWT(I8) + FWT8
          FWT8 = 0.0
        ENDIF
        IF (I9 .NE. 0) THEN
          FWT(I9) = FWT(I9) + FWT9
          FWT9 = 0.0
        ENDIF
C
C       MOVE ALL NON-ZERO FWT/FMOD, LEAVING ALL OPEN SPOTS AT THE
C       BOTTOM OF THE ARRAY
C
        K = 0
        DO 60 I = 1,MXFMOD
          IF (FMOD(I) .EQ. 0 .OR. FWT(I) .EQ. 0.0) GOTO 60
          K = K + 1
          IF (I .NE. K .AND. FMCHKFWT(K)) THEN
            FMOD(K) = FMOD(I)
            FWT(K)  = FWT(I)
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDIF
   60   CONTINUE
C
C       COPY CURRENT MODELS TO TOP OF TMP ARRAY;
C       THEN NEW MODELS AT END
C
        DO I = 1,MXFMOD
          FMOD2(I) = FMOD(I)
          FWT2(I)  = FWT(I)
        ENDDO
        IF (FWT5 .GT. 0.0) THEN
          I = MXFMOD+1
          FMOD2(I) = 5
          FWT2(I)  = FWT5
        ENDIF
        IF (FWT8 .GT. 0.0) THEN
          I = MXFMOD+2
          FMOD2(I) = 8
          FWT2(I)  = FWT8
        ENDIF
        IF (FWT9 .GT. 0.0) THEN
          I = MXFMOD+3
          FMOD2(I) = 9
          FWT2(I)  = FWT9
        ENDIF
C
C       SORT BY WEIGHT; LAST 3 ARE DROPPED
C
        CALL RDPSRT (MXFMOD+3,FWT2,INDX,.TRUE.)
        DO I = 1,MXFMOD
          J= INDX(I)
          FMOD(I) = FMOD2(J)
          FWT(I)  = FWT2(J)
        ENDDO
C
C       BEGINNING OF COPY FROM **FMDYN**
C
C       TRUNCATE NUMBER OF FUEL MODELS TO 4 AND REWEIGHT,
C       IN THE RARE CASE THAT >4 MODELS HAVE BEEN SELECTED
C       THIS SOMETIMES HAPPENS IN THE EC, WITH THE 5TH MODEL
C       WEIGHT AT 2-3% OF THE TOTAL.
C       (CONVERSATION WITH NICK; 3 DECEMBER 2002)
C
        X = 0.0
        DO I = 1,4
          X = X + FWT(I)
        ENDDO
        IF (X .GT. 1.0E-6) THEN
          DO I = 1,4
            FWT(I) = FWT(I) / X
          ENDDO
          DO I = 5,MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
        ENDIF
C
C       SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
C       `FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING
C
        FMD = -1
        IF (FWT(1) .GT. 1.0E-6) FMD = FMOD(1)
        IF (FMD .LT. 0) THEN
          FMOD(1) = 8
          FWT(1)  = 1.0
          NFMODS  = 1
          DO I = 2,MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
          CALL RCDSET(2, .TRUE.)
        ENDIF
C
C       COMPUTE THE NUMBER OF ACTIVE MODELS
C
        DO I = 1,MXFMOD
          IF (FWT(I) .LE. 1.0E-6) THEN
            NFMODS = I-1
            EXIT
          ENDIF
        ENDDO
        NFMODS = MIN(NFMODS,4)
C
C       IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS
C
        IF (.NOT. LDYNFM) THEN
          FMOD(1) = FMD
          FWT(1)  = 1.0
          NFMODS  = 1
          DO I = 2, MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
        ENDIF
C
C       END OF COPY FROM **FMDYN**
C
C       TURN OFF SHRUB MODEL WHEN THERE ARE NO MORE SPECIAL
C       WEIGHTS TO PROCESS
C
        IF (LATSHRB .AND. ((FWT5+FWT8+FWT9) .LE. 0.0)) THEN
          LATSHRB = .FALSE.
        ENDIF

      ENDIF

      RETURN
      END
