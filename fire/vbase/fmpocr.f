      SUBROUTINE FMPOCR(IYR,ICALL)
      IMPLICIT NONE
C----------
C FIRE-VBASE $Id$
C----------
C     CALLED FROM: FMBURN
C
C  PURPOSE:
C     THIS SUBROUTINE FINDS THE BOTTOM OF THE LIVE CROWN (ACTCBH)
C     THE CROWN BULK DENSITY (CBD).
C     TCLOAD IS THE TOTAL CANOPY FUEL LOAD (LBS/SQFT).
C
C  THIS ROUTINE WAS REWRITTEN BY N.CROOKSTON, 04/07/99. I CHANGED
C  THE RUNNING AVERAGE COMPUTATIONS SO THAT ZEROS ALONG THE BOLE
C  WHERE NO CROWN EXISTS ON ANY TREES ARE NOT COUNTED IN THE AVERAGE
C  THIS CAUSES THE CALCULATION OF THE BASE OF THE LIVE CROWN TO BE
C  MORE RESPONSIVE TO THINNINGS AND PRUNINGS.
C
C  JANUARY 2003: HARDWOODS (LSW(I) .FALSE.) DO NOT CONTRIBUTE TO
C  THE CALCULATIONS
C
C     PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     COMMON INCLUDE FILES.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C     VARIABLE DECLARATIONS.

      LOGICAL DEBUG, LBHPP
      INTEGER J,I,I1,I2,J1,J2,MXJ,IYR,K,ICALL
      REAL    CRBOT, CRFILL(400), ABOT, ABOTMX
      REAL    ADCRWN,A,ADJ
      REAL MSDI, MRD, WTRADJ, WEIBB, WEIBC, TSCL, SECINT
      REAL ADCRN(400), SECBND, WPROP(400), LCR, DCM, CRBIO

C
C     CHECK FOR DEBUG.
C
      CALL DBCHK (DEBUG,'FMPOCR',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMPOCR CYCLE = ',I2,' ITRN=',I5)

      DO J = 1,400
        CRFILL(J) = 0.0
      ENDDO

C----------
C SET VARIABLES AND INSERT CODE TO COMPUTE RELATIVE DENSITY FOR NEW 
C WEIBULL FUNCTION FOR BLACK HILLS PONDEROSA PINE.  
C SEE KEYSER AND SMITH, FOREST SCIENCE 56(2) 2010
C RELATIVE DENSITY IS IN METRIC UNITS.
C----------
      MSDI = 0
      MRD = 0
      WEIBB = 0
      WEIBC = 0
      WTRADJ = 0
      TSCL = 0
      SECINT = 0
      DO I = 1,ITRN 
        DCM = DBH(I)*2.54   
        MSDI = MSDI + ((FMPROB(I)*2.47)*(DCM/25.4)**1.6)
      ENDDO
      MRD = MSDI / 1111.97
      IF(MRD.GT.1.0) MRD=1.0

      DO I = 1,ITRN

        IF ((LSW(ISP(I)) .OR. (ICANSP .EQ. 1)) .AND.
     >      FMICR(I) .GT. 0 .AND.
     >      HT(I)    .GT. CANMHT) THEN

          LBHPP = .FALSE.
          SELECT CASE (VARACD)
          CASE ('CR')
            IF ((KODFOR .EQ. 203 .OR. KODFOR .EQ. 207) .AND. 
     &          (ISP(I) .EQ. 13)) LBHPP = .TRUE.                    
          CASE ('IE','EM','KT')
            IF (ISP(I) .EQ. 10) LBHPP = .TRUE.                      
          END SELECT
        
          IF (.NOT.(LBHPP)) THEN
 
            CRBOT = HT(I) * (1.0 - (FLOAT(FMICR(I)) * 0.01))
            IF (CRBOT.LT.0.) CRBOT=0.0
            
            ADCRWN = (CROWNW(I,0) + CROWNW(I,1) * 0.5) *
     >        FMPROB(I) / (HT(I) - CRBOT)
            
            I1 = INT(CRBOT) + 1
            I2 = INT(HT(I)) + 1
            
            IF (DEBUG) WRITE (JOSTND,10) I,ADCRWN,HT(I),FMICR(I),
     >        CRBOT,I1,I2,CROWNW(I,0),CROWNW(I,1)
   10       FORMAT (' FMPOCR, I=',I4,' ADCRWN=',F9.4,' HT=',F7.2,
     >        ' FMICR=',I3,' CRBOT=',F7.2,' I1,I2=',2I5,
     >        ' CROWNW(0&1)=',2F7.2)
            
            IF (I1 .GT. 400) I1 = 400
            IF (I2 .GT. 400) I2 = 400
            
C----------
C ADD THE TREES CANOPY FUELS MATERIAL TO CRFILL.  ADJUST THE TOP AND BOTTOM
C INTERVALS SINCE THE CROWN MIGHT NOT FULLY COVER THAT 1-FT INTERVAL.  SAR 3/2015
C----------
            IF (I1.LE.I2 .AND. ADCRWN .GT. 0.0) THEN
              DO J = I1,I2
                IF (J .EQ. I1) THEN
                   ADJ = MAX(0.,MIN(1.,I1 - CRBOT))
                   CRFILL(J) = CRFILL(J) + ADCRWN*ADJ                
                ELSEIF (J .EQ. I2) THEN
                   ADJ = MAX(0.,MIN(1.,I2 - HT(I)))
                   CRFILL(J) = CRFILL(J) + ADCRWN*(1-ADJ)                 
                ELSE
                  CRFILL(J) = CRFILL(J) + ADCRWN              
                ENDIF
              ENDDO
            ENDIF
            
          ELSE  ! Black Hill PP uses special shape distribution
            LCR = (FLOAT(FMICR(I)) * 0.01)
            CRBIO = (CROWNW(I,0) + CROWNW(I,1) * 0.5) * FMPROB(I)
C----------
C COMPUTE HEIGHT TO BASE OF CROWN (I1) AND TOP OF CROWN (I2)
C----------
            CRBOT = HT(I) * (1.0 - (FLOAT(FMICR(I)) * 0.01))
C            WRITE(20,*) 'LCR,CRBIO,CRBOT,'
C            WRITE(20,*) LCR,',',CRBIO,',',CRBOT
C
            IF (CRBOT.LT.0.) CRBOT=0.0
C          
            I1 = INT(CRBOT) + 1
            I2 = INT(HT(I)) + 1
            IF ((I2 - HT(I)) .GE. 1) I2 = I2 - 1
C
            IF (I1 .GT. 400) I1 = 400
            IF (I2 .GT. 400) I2 = 400
C            WRITE(20,*) 'I1 ,', I1
C            WRITE(20,*) 'I2 ,', I2
C
            IF (I1.LE.I2 .AND. CRBIO .GT. 0.0) THEN
C----------
C THE NEW WEIBUL STUFF
C ESTIMATE THE WEIBULL PARAMETERS B AND C FOR THE TREE
C B = SHAPE PARAMTER AND C = SCALE PARAMETER
C MRD = SDI RELATIVE DENSITY AND LCR = LIVE CROWN RATIO 
C----------
              WEIBB = 7.1386 - (0.0608 * (HT(I) / 3.28))
              WEIBC = 3.3126 - (0.0214 * (HT(I) / 3.28)) - 
     >          (1.1622 * MRD)
C----------
C DETERMINE THE ADJUSTMENT FOR THE WEIBULL TRUNCATION 
C----------
              WTRADJ = 1 - EXP(-((10/WEIBB)**WEIBC))
C              WRITE(20,*) 'WEIBB ,', WEIBB
C              WRITE(20,*) 'WEIBC ,', WEIBC
C              WRITE(20,*) 'WTRADJ ,', WTRADJ
C----------
C DETERMINE THE AMOUNT OF FOLIAGE IN EACH 1 FOOT SECTION OF CROWN.
C SCALE FEET TO A CONTIOUS X VARIABLE FROM 1 TO 10 FOR WEIBULL.
C DETERMINE THE PROPORTION OF CROWN MASS WITHIN EACH SECTION BY 
C SUBTRACTION. MULTIPLY THE PROPORTION BY TOTAL CROWN MASS
C   TSCL = CROWN LENGTH
C   SECINT = INTERVAL FOR 1 FOOT SECTION BOUNDARIES SCALED TO 10
C   SECBND = 1 FOOT SECTION BOUNDARIES SCALED TO 10
C   WPROP = PROPORTION OF TOTAL FUEL MASS IN A 1 FOOT SECTION 
C   K = COUNTER (NOT USED BUT HELPS INTERPRET OUTPUT 
C----------
C SET VARIABLES          
C----------
              TSCL = I2-I1
              SECINT =  10.0 / (TSCL + 1) 
              DO J = 1,400
                WPROP(J) = 0.0
                ADCRN(J) = 0.0
              ENDDO
              ADCRWN = 0
              SECBND=0
              K=1
C              WRITE(20,*) 'TSCL ,', TSCL
C              WRITE(20,*) 'SECINT ,', SECINT
C----------
C COMPUTE BIOMASS FOR EACH FOOT OF TREE CROWN         
C----------
              DO J = I2,I1,-1
                SECBND = SECBND + SECINT
                IF (J.EQ.I2) THEN
                  WPROP(J) = (1 - EXP(-((SECBND/WEIBB)**WEIBC)))
                ELSE 
                  WPROP(J) = (1 - EXP(-((SECBND/WEIBB)**WEIBC))) - 
     >              (1 - EXP(-(((SECBND-SECINT)/WEIBB)**WEIBC)))
                ENDIF
C                WRITE(20,*) 'K ,', K
                K  = K + 1
                ADCRN(J) = (((CROWNW(I,0) + CROWNW(I,1) * 0.5) * 
     >            FMPROB(I)) * WPROP(J)) / WTRADJ
                ADCRWN = ADCRWN + ADCRN(J)
C                WRITE(20,*) 'J ,', J
C                WRITE(20,*) 'SECBND ,', SECBND
C                WRITE(20,*) 'WPROP ,', WPROP(J)
C                WRITE(20,*) 'ADCRN ,', ADCRN(J)
              ENDDO
C                WRITE(20,*) 'ADCRWN,', ADCRWN
C----------
C ADD TREE CROWN/FOOT TO STAND TOTAL
C----------
              IF (I1.LE.I2 .AND. ADCRWN .GT. 0.0) THEN
                DO J = I1,I2
                  CRFILL(J) = CRFILL(J) + ADCRN(J)
                ENDDO
              ENDIF
            ENDIF
          ENDIF 
        ELSE
          IF (DEBUG) WRITE (JOSTND,11) I,HT(I),FMICR(I)
   11     FORMAT (' FMPOCR, I=',I4,' HT=',F7.2,' FMICR=',I3)
        ENDIF
      ENDDO
C
C     SUM UP THE TOTAL CANOPY FUEL LOAD
C
      TCLOAD = 0.0  ! THIS SUM WILL BE IN LBS/ACRE
      DO I = 1,400
        TCLOAD = TCLOAD + CRFILL(I)
      ENDDO
      TCLOAD = TCLOAD / 43560.0   ! CONVERT LBS/ACRE TO LBS/SQFT
      IF (DEBUG) WRITE (JOSTND,*) 'TCLOAD = ', TCLOAD
          
C     PASS CANOPY PROFILE INFORMATION TO DATABASE TABLE, IF REQUESTED.
C     ONLY PRINT IT FOR POST-ACTIVITY VALUES.

      IF (IYR .LT. ICFPB .OR. IYR .GT. ICFPE) GOTO 14
   
      IF (ICALL .EQ. 2) CALL DBSFMCANPR(IYR, CRFILL, NPLT)
   14 CONTINUE
   
C     CALCULATE THE 13-FT RUNNING AVERAGE. THE MAX VALUE OF THE MEAN
C     IS THE CROWN BULK DENSITY. THE LOWEST HEIGHT AT WHICH
C     MEAN > 30 LBS/ACRE-FT (CBHCUT) IS THE CROWN BASE HEIGHT

      CBD    =  0.0
      ACTCBH = -1
      MXJ    = -1
      ABOTMX =  0.0

C     FIRST, FIND THE HEIGHT WERE THE CROWN "STARTS".  THIS IS THE
C     LOWEST POINT WERE AT LEAST 5 LBS/ACRE-FT EXIST.  WE WILL START
C     START THE RUNNING AVERAGE AT THAT POINT. ALSO FIND THE POINT WHERE
C     THE CROWN "ENDS".  THIS IS A COMPLIMENT TO THE STARTING RULE.

      J1 = 0
      DO J = 1,400
        IF (CRFILL(J) .GT. 5.0) THEN
          J1 = J
          EXIT
        ENDIF
      ENDDO
C
C     IF J1 IS STILL 0, THEN NO 1-FOOT INTERVAL HAS MORE THAN 5 LBS.  ,
C     THEREFORE THE BOTTOM OF THE LIVE CROWN CAN NOT BE COMPUTED.
C
      IF (J1 .EQ. 0) GOTO 80
C
C     FIND THE EFFECTIVE TOP OF THE CANOPY.
C
      J2 = 201
      DO J = 400,1,-1
        IF (CRFILL(J) .GT. 5.0) THEN
          J2 = J
          EXIT
        ENDIF
      ENDDO
C
C     BOTTOM AND TOP ARE THE SAME PLACE.  SET THE BOTTOM AND CBD.
C
      IF (J1 .EQ. J2) THEN
        CBD = CRFILL(J1)
        IF (CRFILL(J1) .GE. 5.0) ACTCBH = J1
        GOTO 80
      ENDIF
C
C     START THE CODE TO COMPUTE THE RUNNING AVERAGE, 6 ABOVE AND 6 BELOW.
C
      DO J = J1,J2

        I1 = J-6
        IF (I1 .LT. J1) I1 = J1
        I2 = J+6
        IF (I2 .GT. J2) I2 = J2

        A = 0.0
        DO I = I1,I2
          A = A + CRFILL(I)
        ENDDO
        A = A / FLOAT(I2-I1+1)
C
C       FIND THE MAXIMUM CBD.
C
        IF (A .GT. CBD) CBD = A
C
C       FIND THE BOTTOM OF THE LIVE CROWN...COMPUTE A 3FT RUNNING
C       AVERAGE...AND SAVE A POINTER TO THE MAX VALUE.
C
        I1 = J-1
        IF (I1 .LT. J1) I1 = J1
        I2 = J+1
        IF (I2 .GT. J2) I2 = J2

         ABOT = 0.0
         DO I = I1,I2
           ABOT = ABOT + CRFILL(I)
         ENDDO
         ABOT = ABOT / FLOAT(I2-I1+1)
C
C        DON'T INCREMENT THE CROWN BASE UNLESS THE UPPER RUNNING
C        AVERAGE IS 'SLIGHTLY' MORE THAN THE LOWER RUNNING AVERAGE.
C        (THIS KEEPS THE CROWN BASE SET AT THE LOWEST VALUE IN THE
C         RARE CASE THAT THE CROWN PROFILE IS UNIFORM OVER A RANGE
C         OF HEIGHTS).
C
C         IF (ABOT .GT. ABOTMX) THEN
         IF (ABOT .GT. (ABOTMX+0.1)) THEN
           ABOTMX = ABOT
           MXJ = J
         ENDIF

         IF (ABOT .GE. CBHCUT .AND. ACTCBH .EQ. -1) ACTCBH = J

         IF (DEBUG) WRITE (JOSTND,20) J,CRFILL(J),A,CBD,ACTCBH,ABOT,MXJ
   20    FORMAT (' FMPOCR, J=',I3,' CRFILL=',F9.3,' A=',F9.3,
     >           ' CBD=',F9.3,' ACTCBH=',I4,' ABOT=',F9.3,
     >           ' MXJ=',I3)

      ENDDO
C
C     IF THE CROWN IS STILL NOT COMPUTED, PICK THE LOCATION OF THE MAX
C     RUNNING AVERAGE SO LONG AS IT IS OVER 5.
C
      IF (ACTCBH .EQ. -1 .AND. ABOTMX .GT. 5.0) ACTCBH = MXJ

   80 CONTINUE
C
C     CHANGE THE CBD TO KG/M3:
C     MULTIPLY BY LBTOKG AND DIVIDE BY (ACRETOM2 * FTTOM)
C
      CBD = CBD * 0.45359237 / (4046.856422 * 0.3048)
      
C     CAP CBD AT 0.35 - S. REBAIN - SEPT 2005
      IF (CBD .GT. 0.35) CBD = 0.35

      IF (DEBUG) WRITE (JOSTND,90) CBD,ACTCBH
   90 FORMAT (' FMPOCR, CBD=',F8.4,' ACTCBH=',I4)

      RETURN
      END
