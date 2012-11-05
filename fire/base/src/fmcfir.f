      SUBROUTINE FMCFIR (IYR,FMOIS,WMULT,SWIND,CFTMP,OINIT1,OACT1,HPA)
      IMPLICIT NONE
C----------
C  **FMCFIR FIRE-DATE OF LAST REVISION:  04/19/10
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMPOFL
*                  FMBURN
*----------------------------------------------------------------------
*  PURPOSE:  CALCULATES THE CROWN FIRE INFORMATION
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMOIS    FIRE TYPE BEING CALCULATED
*     WMULT    ACTUAL WIND MODIFIER
*     SWIND    WINDSPEED BEFORE MODIFIER
*     CFTMP    LABEL FOR TYPE OF FIRE
*     OINIT1   TORCHING INDEX
*     OACT1    CROWNING INDEX
*
*  LOCAL VARIABLE DEFINITIONS:
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     ACTCBH   ACTUAL CROWN BASE HEIGHT (FT) (-1 means that there isn't one)
*     CBD      CROWN BULK DENSITY (METRIC UNITS)
*
***********************************************************************

C     PARAMETER STATEMENTS.

C     PARAMETER INCLUDE FILES.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     COMMON INCLUDE FILES.
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'

C     VARIABLE DECLARATIONS.

      INTEGER IYR,FMOIS
      INTEGER SWIND
      INTEGER OLDND, OLDNL, IRTNCD
      REAL    OLDFWG(2,7), OLDMEX(3), OLDMPS(2,3), OLDEPT
      REAL    HPA, HPA2, HPA3, B, DIFF, BOUNDL, BOUNDU
      REAL    INIT1, RACT, RACT1
      REAL    OINIT1(3), OACT1(3)
      CHARACTER*8 CFTMP
      LOGICAL DEBUG
      INTEGER I,J
      REAL    WMULT,SAVWND,FLAME,BYRAM,RINIT1,CFB

      CALL DBCHK (DEBUG,'FMCFIR',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,FMOIS,CBD
    7 FORMAT(' ENTERING FMCFIR CYCLE=',I3,' FMOIS=',I2,' CBD=',F7.3)
C
C     IF THE CROWN IS VERY SPARSE, THEN CAN HAVE NO CROWN FIRE, SO IGNORE
C     IF (ACTCBH .LE. 0 .OR. CBD .LE. 0.0) THEN
C
      IF (CBD .LE. 0.0) THEN
        CFTMP = 'SURFACE'
        CRBURN = 0.0
        OINIT1(FMOIS) = -1.0
        OACT1(FMOIS) = -1.0
        RETURN
      ENDIF

      INIT1 = 0.0
C      IF (ACTCBH .NE. -1) INIT1 = (4.078 * ACTCBH)**(3.0/2.0)  ! original line
C      IF (ACTCBH .NE. -1) INIT1 = (4.065 * ACTCBH)**(3.0/2.0)  ! new coefficient
C      IF (ACTCBH .NE. -1) THEN
C        INIT1 = (0.3048*(460+25.9*FOLMC))/100*(0.2891)**(2/3)  ! with FMC
C        INIT1 = (INIT1 * ACTCBH)**(3.0/2.0)  
C      ENDIF
      IF (ACTCBH .NE. -1) THEN
        INIT1 = ((460+25.9*FOLMC))*.001333  ! with FMC simplified
        INIT1 = (INIT1 * ACTCBH)**(3.0/2.0)  
      ENDIF
C
C     GET THE FUEL MODEL 10 INFORMATION (AND BACKUP THE EXISTING INFO)
C     (COPIED FROM FMCFMD)
C
      OLDND = ND
      OLDNL = NL
      OLDEPT = DEPTH
      OLDFWG(1,7) = FWG(1,7)
      OLDFWG(2,7) = FWG(2,7)
      DO I = 1,3
        OLDMPS(1,I) = MPS(1,I)
        OLDMPS(2,I) = MPS(2,I)
        OLDFWG(1,I) = FWG(1,I)
        OLDFWG(2,I) = FWG(2,I)
        J = I + 3
        OLDFWG(1,J) = FWG(1,J)
        OLDFWG(2,J) = FWG(2,J)
        OLDMEX(I) = MEXT(I)
      ENDDO
      MPS(1, 1) = 2000
      MPS(1, 2) = 109
      MPS(1, 3) = 30
      MPS(2, 1) = 1500
      ND = 3
      NL = 1
      FWG(1, 1) = .138
      FWG(1, 2) = .092
      FWG(1, 3) = .23
      FWG(2, 1) = .092
      DEPTH = 1.0
      MEXT(1) = .25
C
C     CHANGE THE DEFAULT WIND AND MOISTURE CONDITIONS, IF NECESSARY
C     SAVE THE CANOPY-CORRECTED WIND SPEED SO THAT IT IS THE REPORTED VALUE
C
      SAVWND = FWIND
      FWIND  = SWIND * 0.4
C
C     CALL FMFINT TO GET THE INTERMEDIATE VALUES THAT WE NEED FOR THE CALC.
C
      CALL FMFINT(IYR, BYRAM, FLAME, 2, HPA2, 1)
      CALL getfvsRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN
      
      FWIND = SAVWND
C
C     DO THE CROWN CALCULATIONS
C
      B = 0.02526 * (SSIGMA(FMOIS)**0.54)

CC     C = 7.47 * EXP(-0.133 * (SSIGMA(FMOIS)**0.55))
CC     E = 0.715 * EXP(-3.59 * (10**(-4.0)) * SSIGMA(FMOIS))

C
C     CROWNING INDEX (O'ACTIVE)
C
      IF (SIRXI(2) .LT. 00001) THEN
      OACT1(FMOIS) = -1
      ELSE
        OACT1(FMOIS) = ((2.95 * SRHOBQ(2) / (SIRXI(2) * CBD))
     &    - SPHIS(2) - 1.0) / 0.001612
        IF (OACT1(FMOIS) .GT. 0.0) THEN
          OACT1(FMOIS) = (OACT1(FMOIS)**0.7) * 0.01137 / 0.4
        ELSE
          OACT1(FMOIS) = 0.0
        ENDIF
      ENDIF
C
C     GET MORE VALUES FOR CALC THE CROWN TYPE VALUES
C
      RACT = 3.34 * SFRATE(2)
      RINIT1 = 0
      IF (HPA .GT. 0) RINIT1 = 60.0*INIT1 / HPA


C     RESET THESE VALUES BACK SINCE THEY WERE RESET FOR USE OF FM 10.

      ND       = OLDND
      NL       = OLDNL
      DEPTH    = OLDEPT
      FWG(1,7) = OLDFWG(1,7)
      FWG(2,7) = OLDFWG(2,7)

      DO I = 1,3
        MPS(1,I) = OLDMPS(1,I)
        MPS(2,I) = OLDMPS(2,I)
        FWG(1,I) = OLDFWG(1,I)
        FWG(2,I) = OLDFWG(2,I)
        J = I + 3
        FWG(1,J) = OLDFWG(1,J)
        FWG(2,J) = OLDFWG(2,J)
        MEXT(I)  = OLDMEX(I)
      ENDDO

C     NEW TORCHING INDEX CALCULATION
C     CALL FMFINT AND ITERATE WITH DIFFERENT WIND SPEEDS UNTIL THE 
C     PREDICTED SPREAD RATE VALUE EQUALS THE CRITICAL VALUE FOR INITIATION.
C     USE THE OLD CALCULATION AS A STARTING POINT.
C     THIS FIXES THE PROBLEM WHERE THE FIRE TYPE BASED ON TI IS
C     INCONSISTENT WITH THE FIRE TYPE BASED ON SPREAD RATES (I.E. THE
C     NONLINEARITY PROBLEM THAT SOMETIMES CROPPED UP WHEN INTERPOLATING
C     BETWEEN FUEL MODELS.)
C
      SAVWND = FWIND
      
      IF ((ACTCBH .NE. -1) .AND. (HPA .GT. 0)) THEN
         OINIT1(FMOIS) = ((60.*INIT1*SRHOBQ(FMOIS) / (HPA*SIRXI(FMOIS)))
     &       - SPHIS(FMOIS) - 1.0) / SCBE(FMOIS)
         IF (OINIT1(FMOIS) .GT. 0.0) THEN
           OINIT1(FMOIS) = (OINIT1(FMOIS)**(1/B)) * 0.01137 / WMULT
         ELSE
           OINIT1(FMOIS) = 0.0
         ENDIF

         FWIND  = 999 * WMULT    
         CALL FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
         CALL getfvsRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN

         IF (SFRATE(2) .LT. RINIT1) THEN
           OINIT1(FMOIS) = 999
           GOTO 205
         ENDIF

         IF (OINIT1(FMOIS) .GE. 999) OINIT1(FMOIS) = 999

         FWIND  = OINIT1(FMOIS) * WMULT    
         CALL FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
         CALL getfvsRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN

         DIFF = SFRATE(2) - RINIT1
         IF ((DIFF .LE. .001) .AND. (DIFF .GE. -.001)) GOTO 205
         
         DO 200 I=1,1000
           IF (I .EQ. 1) THEN
             IF (DIFF .GT. .001) THEN
               BOUNDL = 0
               BOUNDU = OINIT1(FMOIS)
             ELSE
               BOUNDL = OINIT1(FMOIS)
               BOUNDU = 999
             ENDIF
           ENDIF
           OINIT1(FMOIS) = (BOUNDU + BOUNDL)/2
           FWIND  = OINIT1(FMOIS) * WMULT    
           CALL FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
           CALL getfvsRtnCode(IRTNCD)
           IF (IRTNCD.NE.0) RETURN

           DIFF = SFRATE(2) - RINIT1          
           IF (DEBUG) WRITE(JOSTND,*) 'ITER NUMBER = ', I
           IF (DEBUG) WRITE(JOSTND,*) 'BOUNDL = ', BOUNDL 
           IF (DEBUG) WRITE(JOSTND,*) 'BOUNDU = ', BOUNDU 
           IF ((DIFF .LE. .001) .AND. (DIFF .GE. -.001)) GOTO 205
           IF (DIFF .GT. .001) BOUNDU = OINIT1(FMOIS)
           IF (DIFF .LT. -.001) BOUNDL = OINIT1(FMOIS)                       
           IF (BOUNDU .LE. 0) GOTO 205
           IF ((BOUNDU .GT. 0) .AND. (BOUNDU .LT. .00000000001)) THEN
             OINIT1(FMOIS) = 0.0
             GOTO 205
           ENDIF
  200   CONTINUE
  205   CONTINUE 
         IF (DEBUG) WRITE(JOSTND,*) 'ITER NUMBER = ', I
         IF (DEBUG) WRITE(JOSTND,*) 'DIFF = ', DIFF      
         IF (DEBUG) WRITE(JOSTND,*) 'SFRATE(2) = ',SFRATE(2)
C        SET 999 AS THE MAX TORCHING INDEX
         OINIT1(FMOIS) = MIN(OINIT1(FMOIS), 999.)     
      ELSE

C     THE FIRE DOESN'T REALLY BURN OR THERE WAS TOO LITTLE CROWN, SO SET
C     A -1 TO INDICATE A NULL VALUE

        OINIT1(FMOIS) = -1.0
      ENDIF

      FWIND = SAVWND

C     FIRE TYPE
C
C      RACT1 = 9.84249/CBD ! critical spread rate for active crown fire (ft/min)
C
C      IF (RACT .GE. RACT1) THEN
C        IF (SFRATE(FMOIS) .GE. RINIT1) THEN
C          CFTMP = 'ACTIVE'
C          CRBURN = 1.0
C          FIRTYPE = 1
C          RFINAL = RACT         
C        ELSE
C          CFTMP = 'COND_CRN'
C          CRBURN = 1.0
C          FIRTYPE = 1
C          RFINAL = RACT       
C        ENDIF
C      ELSEIF (SFRATE(FMOIS) .GE. RINIT1) THEN
C        CFTMP = 'PASSIVE'
C        FIRTYPE = 2      
C      ELSE
C        CFTMP = 'SURFACE'
C        CRBURN = 0.0
C        FIRTYPE = 3
C        RFINAL = SFRATE(FMOIS)      
C      ENDIF 

      IF (OINIT1(FMOIS) .GT. SWIND) THEN
        IF (OACT1(FMOIS) .GT. SWIND) THEN
          CFTMP = 'SURFACE'
          CRBURN = 0.0
          FIRTYPE = 3
          RFINAL = SFRATE(FMOIS)        
        ELSE
          CFTMP = 'COND_CRN'
          CRBURN = 1.0
          FIRTYPE = 1
          RFINAL = RACT
        ENDIF
      ELSEIF (OACT1(FMOIS) .GT. SWIND) THEN
        CFTMP = 'PASSIVE'
        FIRTYPE = 2
      ELSE
        CFTMP = 'ACTIVE'
        CRBURN = 1.0
        FIRTYPE = 1
        RFINAL = RACT
      ENDIF
      IF ((OINIT1(FMOIS) .EQ. -1) .OR. (OACT1(FMOIS) .EQ. -1)) THEN
        CFTMP = 'SURFACE'
        CRBURN = 0.0
        FIRTYPE = 3
        RFINAL = SFRATE(FMOIS)       
      ENDIF

C      FOR PASSIVE FIRES, CALCULATE THE CROWN FRACTION BURNED USING
C      THE STRAIGHT LINE FORMULA, LIKE IN NEXUS
C      (SCOTT AND REINHARDT 2001 PG 41)
C
C     CALL FMFINT WITH THE CROWNING INDEX AS THE WINDSPEED TO GET R'SA
C
      IF (CFTMP .EQ. 'PASSIVE') THEN
        SAVWND = FWIND
        FWIND  = OACT1(FMOIS) * WMULT
        CALL FMFINT(IYR, BYRAM, FLAME, 2, HPA2, 2)
        CALL getfvsRtnCode(IRTNCD)
        IF (IRTNCD.NE.0) RETURN

        FWIND = SAVWND	
        CFB = (SFRATE(FMOIS) - RINIT1)/(SFRATE(2) - RINIT1)
        RFINAL = SFRATE(FMOIS) + CFB * (RACT - SFRATE(FMOIS))
        CRBURN = CFB
        IF (CRBURN .GT. 1.0) CRBURN = 1.0        
      ENDIF

      IF (DEBUG) WRITE(JOSTND,*) 'SFRATE(FMOIS) = ',SFRATE(FMOIS)
      IF (DEBUG) WRITE(JOSTND,*) 'SFRATE(2) = ',SFRATE(2)
      IF (DEBUG) WRITE(JOSTND,*) 'RACT = ',RACT
      IF (DEBUG) WRITE(JOSTND,*) 'RINIT1 = ',RINIT1
      IF (DEBUG) WRITE(JOSTND,*) 'CRBURN = ',CRBURN
      
      IF (DEBUG) WRITE(JOSTND,8) FMOIS,CFTMP,OINIT1(FMOIS),
     >                           OACT1(FMOIS), RFINAL
    8 FORMAT(' FMCFIR FMOIS=',I2,' CFTMP=',A,' OINIT1=',F13.3,
     >       ' OACT1=',F13.3, ' RFINAL=',F13.3)

      RETURN
      END
