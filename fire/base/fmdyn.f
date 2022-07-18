      SUBROUTINE FMDYN (SM,LG,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYN,FMD)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
C     CALLED FROM: FMCFMD
C
C  PURPOSE:
C     THIS SUBROUTINE RETURNS THE SET OF UP TO 4, (TYPICALLY 2) FUEL
C     MODELS THAT WOULD BE USED IF THE DYNAMIC FUEL MODEL IS SELECTED
C
C************DYNAMIC FUEL MODEL CALCULATIONS***************
C
C     THIS ROUTINE FINDS A GROUP OF FIRE MODELS MOST APPROPRIATE TO
C     THE INPUT COMBINATION OF LARGE AND SMALL FUEL COMPONENTS.
C
C     THIS ROUTINE TAKES AS INPUT VARIABLES SMALL AND LARGE; A POINT
C     IN A 2-SPACE, REPRESENTING SMALL AND LARGE FUEL. THE POINT IS
C     COMPARED TO THE FUEL MODELS, EACH OF WHICH IS REPRESENTED BY A
C     STRAIGHT LINE ENCODED WITH XPTS(). THE NEAREST NEIGHORING LINES
C     ARE FOUND AND THE INVERSE OF THE DISTANCE TO THOSE LINES IS
C     USED AS THE WEIGHT WHICH IS GIVEN FOR THE FUEL MODEL ASSOCIATED
C     WITH THE LINE.
C
C     USUALLY THERE WILL BE TWO NEIGHBORING LINES (ONE TO THE LEFT AND
C     ONE TO THE RIGHT OF THE POINT). IN CASES WHERE THE POINT IS BEYOND
C     THE UPPER/LOWER-MOST LINE, THERE WILL BE ONLY ONE NEIGHBOR, AND IN
C     CASES WHERE HORIZONTAL, VERTICAL AND SLOPING LINES CROSS, THERE
C     MAY BE AS MANY AS FOUR NEIGHBOURS. THE LAST CASE IS UNUSUAL AND
C     WILL ONLY OCCUR IF THE MODEL WRITER KNOWINGLY MIXES IN THESE
C     TYPES OF LINES.
C
C     THE VECTORS FWT(5) AND FMOD(5) ARE PRODUCED AS OUTPUT. FOR EACH
C     NON-ZERO FMOD(I) THERE WILL BE A NON-ZERO WEIGHT GIVEN TO THE
C     CORRESPONDING FMOD() MODEL. WEIGHTS SUM TO 1.0
C
C----------------------------------------------------------------------
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'

      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C     LOCAL VARIABLE DEFINITIONS:
C
C     SM:         SMALL FUEL LOAD
C     LG:         LARGE FUEL LOAD
C     LOK:        LOGICAL: TRUE IF MODEL IS A CANDIDATE;OTHERWISE FALSE
C     ITYP:       0 FOR SLANTED LINES; -1 FOR HORIZ; 1 FOR VERTICAL
C     XPTS(-,1):  FOR EACH FUEL MODEL, X-INTERCEPT (VALUE OF SMALL WHEN LARGE=0)
C     XPTS(-,2):  FOR EACH FUEL MODEL, Y-INTERCEPT (VALUE OF LARGE WHEN SMALL=0)
C     EQMOD:      INDEX OF ANY COLLINEAR FUEL MODEL (OTHERWISE 0)
C     EQWT:       WT OF ANY COLLINEAR FUEL MODEL (OTHERWISE 0)
C     IPTR:       LABEL/TAG FOR EACH FIRE MODEL CLASS
C     ICLSS:      LENGTH OF NUMEROUS VECTORS (IPTR,ITYP,EQMOD,EQWT,XPTS)
C     LDYN:       IS DYNAMIC MODELLING NEEDED OR NOT
C     FMD:        DOMINANT FUEL MODEL (ASSIGNED ON RETURN)
C
C**********************************************************************
C
      INTEGER   ICLSS
      INTEGER   I, II, J, K, K2
      INTEGER   IPTR, ITYP, FMD
      INTEGER   FMOD2, NBR(4), INDX(MXFMOD)
      INTEGER   EQMOD(ICLSS)
      REAL      SM, LG
      REAL      XPTS, XD, YD
      REAL      PT(2), NPT(2), M1, M2, B1, B2, XP, YP, XWT
      REAL      EQWT, FWT2
      REAL      PRV(4), WT(4)
      LOGICAL   DEBUG, LOK, FMCHKFWT, LDYN

      DIMENSION IPTR(ICLSS), ITYP(ICLSS), EQWT(ICLSS)
      DIMENSION XPTS(ICLSS,2), XD(ICLSS), YD(ICLSS), LOK(ICLSS)
      DIMENSION FWT2(MXFMOD), FMOD2(MXFMOD)

      CALL DBCHK (DEBUG,'FMDYN',5,ICYC)

C     INITIALIZE WEIGHTS OF DYNAMIC FIRE MODELS

      DO I= 1,4
        NBR(I) = 0
      ENDDO

      DO I = 1,MXFMOD
        FMOD(I)  = 0
        FWT(I)   = 0.
        FMOD2(I) = 0
        FWT2(I)  = 0.
      ENDDO

      DO I = 1,ICLSS
        EQMOD(I) = 0
        LOK(I)   = .FALSE.
      ENDDO

      PT(1) = SM
      PT(2) = LG
C
C     EMPTY-HANDED IF EITHER SMALL OR LARGE IS NEGATIVE
C
      IF (PT(1) .LT. 0.0 .OR. PT(2) .LT. 0.0) GOTO 999
C
C     SET LOK() VECTOR
C
      DO I = 1,ICLSS
        IF (EQWT(I) .GT. 0.0) LOK(I) = .TRUE.
      ENDDO
C
C     UNSET LINES THAT ARE LOK, BUT WITH INVALID COEFFICIENTS
C
      DO I = 1, ICLSS
        IF (LOK(I)) THEN
          J = ITYP(I)
          IF ((J .EQ. 0 .AND.
     &        (XPTS(I,1) .EQ. 0..OR. XPTS(I,2) .EQ. 0.0)) .OR.
     &        (J .EQ. -1 .AND. XPTS(I,2) .EQ. 0) .OR.
     &        (J .EQ.  1. AND. XPTS(I,1) .EQ. 0)) THEN
            LOK(I)  = .FALSE.
            EQWT(I) = 0.0
          ENDIF
        ENDIF
      ENDDO
C
C     SET EQMOD() VECTOR
C
      DO I = 1,ICLSS
        EQMOD(I) = 0
      ENDDO
      DO I = 1,ICLSS
        IF (LOK(I)) THEN
          DO J = I,ICLSS
            IF (EQMOD(J) .EQ. 0 .AND.
     >        LOK(J) .AND.
     >        XPTS(I,1) .EQ. XPTS(J,1) .AND.
     >        XPTS(I,2) .EQ. XPTS(J,2)) EQMOD(J) = I
          ENDDO
        ENDIF
      ENDDO
C
C     RESCALE SO THAT THE COLINEAR EQWT ELEMENTS SUM TO 1.0
C
      DO I = 1,ICLSS
        IF (LOK(I)) THEN
          XWT = 0.0
          DO J = I,ICLSS
            IF (LOK(J) .AND. EQMOD(J) .EQ. I) XWT= XWT+ EQWT(J)
          ENDDO
          IF (XWT .GT. 1.0E-6) THEN
            DO J = I,ICLSS
              IF (LOK(J) .AND. EQMOD(J) .EQ. I) EQWT(J) = EQWT(J)/XWT
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C     COMPUTE VECTOR XD() CONTAINING DISTANCE TO THE LEFT (<0) AND RIGHT
C     (>0) LINES RELATIVE TO THE SAMPLE POINT. DO THE SAME WITH YD() FOR
C     UPPER (>0) AND LOWER (<0) SAMPLE POINTS. IN WELL-BEHAVED SITUATIONS
C     THEY WILL BE THE SAME, BUT TO HANDLE THE HORIZONTAL AND VERTICAL
C     CASES, THE TWO SEARCHES ARE REQUIRED. THERE ARE CASES (WITH ISOLINES
C     THAT CROSS IN A COMPLICATED WAY) THAT WILL NOT BE DONE PROPERLY:
C     THESE WOULD REQUIRE A SMARTER NEIGHBOR SEARCH. IN SOME CASES THE
C     SAMPLE POINT WILL BE THE LARGEST OR SMALLEST; THESE CORRESPOND TO
C     BEING ABOVE OR BELOW THE GREATEST OR SMALLEST ISOLINE.
C
C     XD() HOLDS DISTANCE ON X-AXIS FROM POINT TO WHERE IT CROSSES EACH LINE
C     IF <0, THEN TO THE LEFT; >0 THEN TO THE RIGHT; HORIZONTAL/VERTICAL CASES
C     ARE GIVEN AN ARBITRARY LARGE NUMBER
C
      DO 20 I = 1, ICLSS
        IF (.NOT. LOK(I)) GOTO 20
        J = ITYP(I)
        XD(I) = 0.
        YD(I) = 0.
        IF (J .EQ. 0) THEN
          M1 = XPTS(I,2) / (-XPTS(I,1))
          B1 = XPTS(I,2)
          XP = (PT(2) - B1 ) / M1
          YP = M1 * PT(1) + B1
          XD(I) = XP - PT(1)
          YD(I) = YP - PT(2)
        ELSEIF (J .EQ. 1) THEN ! VERTICAL
          XD(I) = XPTS(I,1) - PT(1)
        ELSEIF (J .EQ. -1) THEN ! HORIZONTAL
          YD(I) = XPTS(I,2) - PT(2)
        ENDIF
   20 CONTINUE
C
C     FIND CLOSEST NEIGHBORS IN LEFT/RIGHT AND UP/DOWN DIRECTION
C     AND STORE INDICES IN NBR() ARRAY. DUPLICATES ARE OK
C
      PRV(1) = -9.99E+30
      PRV(2) =  9.99E+30
      PRV(3) = -9.99E+30
      PRV(4) =  9.99E+30

      DO 22 I = 1,ICLSS
        IF (.NOT. LOK(I)) GOTO 22
        J = ITYP(I)
        IF (J .EQ. 0 .OR. J .EQ. 1) THEN ! NORMAL OR VERTICAL
          IF (XD(I) .LT. 0. .AND. XD(I) .GT. PRV(1)) THEN
            PRV(1) = XD(I)
            NBR(1) = I
          ELSEIF (XD(I) .GE. 0. .AND. XD(I) .LT. PRV(2)) THEN
            PRV(2) = XD(I)
            NBR(2) = I
          ENDIF
        ENDIF
        IF (J .EQ. 0 .OR. J .EQ. -1) THEN ! NORMAL OR HORIZONTAL
          IF (YD(I) .LT. 0. .AND. YD(I) .GT. PRV(3)) THEN
            PRV(3) = YD(I)
            NBR(3) = I
          ELSEIF (YD(I) .GE. 0. .AND. YD(I) .LT. PRV(4)) THEN
            PRV(4) = YD(I)
            NBR(4) = I
          ENDIF
        ENDIF
   22 CONTINUE
C
C     FIND DISTANCE FROM TRIAL POINT TO EACH DEFINED LINE;
C     FIND THE NEAREST TWO LINES IF WITHIN THE DOMAIN; OTHERWISE
C     FIND THE NEAREST ONE LINE IF OUTSIDE THE DOMAIN
C
C     M1 = SLOPE OF ISOLINE
C     B1 = INTERCEPT OF "ISOLINE"
C     M2 = SLOPE OF LINE NORMAL ISOLINE
C     B2 = INTERCEPT OF LINE NORMAL TO "ISOLINE"
C
C     DISTANCE IS PLACE IN WT() VECTOR
C
      DO 30 K = 1,4
        I = NBR(K)
        IF (I .EQ. 0) GOTO 30
        IF (.NOT. LOK(I)) GOTO 30
        J = ITYP(I)
        IF (J .EQ. 0) THEN
          M1 = XPTS(I,2) / (-XPTS(I,1))
          B1 = XPTS(I,2)
          M2 = -(1./M1)
          B2 = PT(2) - (M2 * PT(1))
          NPT(1) = (B2 - B1) / (M1 - M2)
          NPT(2) = M2 * NPT(1) + B2
          WT(K) = SQRT((PT(2) - NPT(2))**2 + (PT(1) - NPT(1))**2)
        ELSEIF (J .EQ. 1) THEN ! VERTICAL
          WT(K) = ABS(XPTS(I,1) - PT(1))
        ELSEIF (J .EQ. -1) THEN ! HORIZONTAL
          WT(K) = ABS(XPTS(I,2) - PT(2))
        ENDIF
   30 CONTINUE
C
C     MERGE DUPLICATE INDICES; WHEN FINISHED, K IS THE NEXT
C     OPEN POSITION IN THE FWT/FMOD ARRAYS
C
      K2 = 0
      DO 40 I = 1,4
        IF (NBR(I) .EQ. 0) GOTO 40
        K2 = K2 + 1
        DO 41 J = 1, I
          IF (FMOD(J) .EQ. NBR(I)) THEN
            K = J
            GOTO 42
          ENDIF
   41   CONTINUE
        K = K2
        IF (FMCHKFWT(K)) FMOD(K) = NBR(I)
   42   IF (FMCHKFWT(K)) FWT(K)  = FWT(K) + WT(I)
   40 CONTINUE
C
C     REWEIGHT BY INVERSE OF DISTANCE
C
      XWT = 0.
      DO 50 I = 1,MXFMOD
        IF (FMOD(I) .EQ. 0) GOTO 50
        FWT(I) = 1.0/(FWT(I) + 1.0E-6)
        XWT = XWT + FWT(I)
   50 CONTINUE
      DO 55 I = 1, MXFMOD
        IF (FMOD(I) .EQ. 0) GOTO 55
        FWT(I) = FWT(I) / XWT
   55 CONTINUE
C
C     MOVE ALL NON-ZERO FWT/FMOD, LEAVING ALL OPEN SPOTS
C     AT THE BOTTOM OF THE ARRAY
C
      K = 0
      DO 60 I = 1,MXFMOD
        IF (FMOD(I) .EQ. 0) GOTO 60
        K = K + 1
        IF (I .NE. K .AND. FMCHKFWT(K)) THEN
          FMOD(K) = FMOD(I)
          FWT(K)  = FWT(I)
          FMOD(I) = 0
          FWT(I)  = 0.0
        ENDIF
   60 CONTINUE
C
C     WALK THROUGH LIST OF MODELS AND SAVE THE WEIGHT TO BE SHARED
C     AMONG THE OVERLAPPING MODELS
C
      DO I = 1,MXFMOD
        FMOD2(I)  = 0
        FWT2(I)   = 0.
      ENDDO

      K=1
      DO I = 1,MXFMOD
        IF (FMOD(I) .NE. 0) THEN
          II  = FMOD(I)
          XWT = FWT(I)
          IF (EQMOD(II) .EQ. 0 .AND. FMCHKFWT(K)) THEN
            FWT2(K)  = FWT(I)
            FMOD2(K) = FMOD(I)
            K        = K + 1
          ELSE
            DO J = 1,ICLSS
              IF (EQMOD(J) .EQ. EQMOD(II) .AND.
     &            EQWT(J) .GT. 0.0) THEN
                IF (FMCHKFWT(K)) THEN
                  FWT2(K)  = FWT2(K) + EQWT(J) * XWT
                  FMOD2(K) = J
                  K        = K + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C     MERGE IN ANY DUPLICATES THAT MAY HAVE ARISEN FROM
C     THE ADDITION OF COLINEAR FUEL MODELS (IMPOSSIBLE UNDER
C     CURRENT FUEL MODEL DEFINITIONS)
C
  999 K2 = 0
      DO I = 1,MXFMOD
        FMOD(I)  = 0
        FWT(I)   = 0.
      ENDDO

      DO 70 I = 1,MXFMOD
        IF (FMOD2(I) .EQ. 0) GOTO 70
        K2 = K2 + 1
        DO J = 1, I
          IF (FMOD(J) .EQ. FMOD2(I)) THEN
            K = J
            GOTO 72
          ENDIF
        ENDDO
        K = K2
        IF (FMCHKFWT(K)) FMOD(K) = FMOD2(I)
   72   IF (FMCHKFWT(K)) FWT(K)  = FWT(K) + FWT2(I)
   70 CONTINUE

      DO 75 I = 1,MXFMOD
        IF (FMOD(I) .EQ. 0) GOTO 75
        FMOD(I) = IPTR(FMOD(I))
   75 CONTINUE
C
C     SORT WEIGHTS SUCH THAT HIGHEST WEIGHT IS ALWAYS FIRST
C     WHAT ABOUT TIES?
C
      CALL RDPSRT (MXFMOD,FWT,INDX,.TRUE.)
      DO I = 1,MXFMOD
        J= INDX(I)
        FMOD2(I) = FMOD(J)
        FWT2(I)  = FWT(J)
      ENDDO
      DO I = 1,MXFMOD
        FMOD(I) = FMOD2(I)
        FWT(I)  = FWT2(I)
      ENDDO
C
C     TRUNCATE NUMBER OF FUEL MODELS TO 4 AND REWEIGHT,
C     IN THE RARE CASE THAT >4 MODELS HAVE BEEN SELECTED
C     THIS SOMETIMES HAPPENS IN THE EC, WITH THE 5TH MODEL
C     WEIGHT AT 2-3% OF THE TOTAL.
C     (CONVERSATION WITH NICK; 3 DECEMBER 2002)
C
      XWT = 0.0
      DO I = 1,4
        XWT = XWT + FWT(I)
      ENDDO
      IF (XWT .GT. 1.0E-6) THEN
        DO I = 1,4
          FWT(I) = FWT(I) / XWT
        ENDDO
        DO I = 5,MXFMOD
          FMOD(I) = 0
          FWT(I)  = 0.0
        ENDDO
      ENDIF
C
C     SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
C     FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING
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
C     COMPUTE THE NUMBER OF ACTIVE MODELS
C
      DO I = 1,MXFMOD
        IF (FWT(I) .LE. 1.0E-6) THEN
          NFMODS = I-1
          EXIT
        ENDIF
      ENDDO
      NFMODS = MIN(NFMODS,4)
C
C     IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS
C
      IF (.NOT. LDYN) THEN
        FMOD(1) = FMD
        FWT(1)  = 1.0
        NFMODS  = 1
        DO I = 2, MXFMOD
          FMOD(I) = 0
          FWT(I)  = 0.0
        ENDDO
      ENDIF

      IF (DEBUG) WRITE (JOSTND,80) FMD, FMOD,FWT
   80 FORMAT (' FMDYN, FMD=',I4,' FMOD=',5I4,' FWT=',5F7.2)

      RETURN
      END
