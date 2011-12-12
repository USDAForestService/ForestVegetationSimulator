      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-EM-DATE OF LAST REVISION:  01/04/11
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN
C
C  Purpose:
C     Find the dominant species (by basal area). Set the initial live
C     and dead fuel values as well. The dead fuels are only initialized
C     in the first year, but COVTYP and the live fuels must be done
C     each year.
C
C  Local variable definitions:
C     BAMOST:  The highest basal area in a single species
C     CAREA:   The area covered by the crown at its widest point (sqft)
C     CRL:     Crown length
C     CWIDTH:  The maximum width of the crowns (ft)
C     FUINIE:  The initial fuel loadings for established stands (from JBrown)
C     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
C     FULIVE:  The herb/shrub for established stands (from JBrown)
C     FULIVI:  The herb/shrub for initiating stands (from JBrown)
C     ISWTCH:  =1 if called by SVSTART
C              =0 if called by any other subroutine (FMMAIN, FMPPHV)
C     TOTBA:   The total basal area in the stand (used in the fuels calcs)
C     TOTCRA:  The sum of the area of the crowns, per acre (sqft)
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'EMCOM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
Cppe      INCLUDE 'PPEPRM.F77'
Cppe      INCLUDE 'PPCNTL.F77'
COMMONS
C----------
C  LOCAL VARIABLES DECLARATIONS
C----------
      INTEGER MD1(122), MD2(122), M1, M2
      REAL    TBA(MAXSP)
      REAL    BAMOST, BA1, TOTCRA, CWIDTH
      REAL    FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL    FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL    STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL    PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG
      INTEGER MYACT(3)
      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
C----------
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C
C                  herbs, shrubs
C----------
      DATA FULIVE /  0.2,   0.05,   !WB USE LP
     &               0.2,    0.1,   !WL
     &               0.2,    0.1,   !DF
     &               0.2,   0.25,   !LM USE IE LM
     &              0.15,    0.2,   !LL USE IE LL
     &               0.2,   0.25,   !RM USE IE RM
     &               0.2,   0.05,   !LP
     &              0.15,   0.10,   !ES
     &              0.15,   0.10,   !AF
     &               0.2,  0.125,   !PP
     &               0.2,    0.2,   !GA USE IE CO
     &               0.2,    0.2,   !AS USE IE AS
     &               0.2,    0.2,   !CW USE IE CO
     &               0.2,    0.2,   !BA USE IE CO
     &               0.2,    0.2,   !PW USE IE CO
     &               0.2,    0.2,   !NC USE IE CO
     &               0.2,    0.2,   !PB USE IE PB
     &              0.14,   0.35,   !OS (JUNIPER) (Ottmar, Volume I) 
     &               0.2,    0.2/   !OH USE IE CO
C----------
C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C
C                  herbs, shrubs
C----------
      DATA FULIVI /  0.4,   0.5,   !WB USE LP
     &               0.4,   1.0,   !WL
     &               0.4,   1.0,   !DF
     &              0.25,   0.1,   !LM USE IE LM
     &               0.3,   2.0,   !LL USE IE LL
     &              0.25,   0.1,   !RM USE IE RM
     &               0.4,   0.5,   !LP
     &               0.3,   1.0,   !ES
     &               0.3,   1.0,   !AF
     &              0.25,  0.05,   !PP
     &               0.4,   2.0,   !GA USE IE CO
     &               0.4,   2.0,   !AS USE IE AS
     &               0.4,   2.0,   !CW USE IE CO
     &               0.4,   2.0,   !BA USE IE CO
     &               0.4,   2.0,   !PW USE IE CO
     &               0.4,   2.0,   !NC USE IE CO
     &               0.4,   2.0,   !PB USE IE PB
     &               0.1,  2.06,   !OS (JUNIPER) (Ottmar, Volume I)
     &               0.4,   2.0/   !OH USE IE CO
C----------
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
C----------
      DATA FUINIE /0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !WB USE LP
     &             0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !WL
     &             0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !DF
     &             0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !LM USE IE LM
     &             1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !LL USE IE LL
     &             0.7 ,0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !RM USE IE RM
     &             0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !LP
     &             1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !ES
     &             1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !AF
     &             0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !PP
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !GA USE IE CO
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !AS USE IE AS
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !CW USE IE CO
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !BA USE IE CO
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !PW USE IE CO
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !NC USE IE CO
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !PB USE IE PB
     &             0.1, 0.2, 0.4,  0.5, 0.8, 1.0, 0.0,0.0,0.0,0.1, 0.0, !OS (juniper - Ottmar, Volume I)
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0/ !OH USE IE CO
C----------
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
C----------
      DATA FUINII /0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !WB USE LP
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !WL
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !DF
     &             0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !LM USE IE LM
     &             0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !LL USE IE LL
     &             0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !RM USE IE RM
     &             0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !LP
     &             0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !ES
     &             0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !AF
     &             0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !PP
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !GA USE IE CO
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !AS USE IE AS
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !CW USE IE CO
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !BA USE IE CO
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !PW USE IE CO
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !NC USE IE CO
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !PB USE IE PB
     &             0.2, 0.4, 0.2,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.2, 0.0, !OS (juniper - Ottmar, Volume I)
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0/ !OH USE IE CO
C----------
C     FIRE MODEL TO USE WHEN COVER < 20%
C     USES ITYPE INDEX TO JTYPE() IN **BLKDAT**
C----------
      DATA (MD1(I), I=   1,  50) /
     &  8,  1,  2,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,
     &  2,  2,  2,  1,  1,  1,  1,  2,  2,  2,
     &  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
     &  2,  2,  2,  2,  2,  2,  2,  2,  2,  2/
      DATA (MD1(I), I=  51, 100) /
     &  2,  2,  2,  2,  2,  2,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5/
      DATA (MD1(I), I= 101, 122) /
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5*8/
C----------
C     FIRE MODEL TO USE WHEN COVER > 0%
C     USES ITYPE INDEX TO JTYPE() IN **BLKDAT**
C----------
      DATA (MD2(I), I=   1,  50) /
     &  8,  2,  6,  2,  2,  2,  2,  2,  2,  2,
     &  2,  2,  2,  2,  2,  9,  9,  9,  9,  6,
     &  6,  6,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8/
      DATA (MD2(I), I=  51, 100) /
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8/
      DATA (MD2(I), I= 101, 122) /
     &  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     &  8,  8,  8,  8,  8,  8,  8,  5*8/
C
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C----------
C  BEGIN ROUTINE.
C
C  ZERO OUT THE CUMMULATIVE VARIABLES
C----------
      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA  = 0.0
C----------
C  LOOP THROUGH THE TREE LIST
C----------
      IF (ITRN.GT.0) THEN
C----------
C  ZERO OUT THE CUMMULATIVE VARIABLES
C----------
         BAMOST = 0.0
         TOTCRA = 0.0
C
         DO KSP=1,MAXSP
            TBA(KSP) = 0.0
         ENDDO
C
         DO I=1,ITRN
            IF (FMPROB(I) .GT. 0.0) THEN
C
               KSP = ISP(I)
C
               BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
               TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)
C
               IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
C----------
C  CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
C  ENCOMPASSED BY ALL TREES
C----------
               CWIDTH=CRWDTH(I)
               CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
               TOTCRA = TOTCRA + CAREA*FMPROB(I)
            ENDIF
C----------
C  USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.
C----------
            CURKIL(I) = 0.0
         ENDDO
C----------
C  DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C  -> THAT WILL BE THE COVER TYPE
C----------
         DO KSP=1,MAXSP
            IF (TBA(KSP) .GT. BAMOST) THEN
               BAMOST = TBA(KSP)
               COVTYP = KSP
            ENDIF
            TOTBA = TOTBA + TBA(KSP)
         ENDDO
C----------
C  USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
C  OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
C  CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:
C
C  PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))
C----------
         PERCOV = 1.0 - EXP(-TOTCRA/43560.)
         PERCOV = PERCOV * 100.0
C
      ENDIF
C----------
C  IF THERE ARE NO TREES (COVTYP=0) THEN USE THE HABITAT TYPE
C  (INDEX=ITYPE) TO DETERMINE A SURROGATE
C
C        DEFINE COVTYP (IT IS A SPECIES CODE) AS A FUNCTION OF THE HABITAT
C        TYPE ENTERED FOR THE RUN. WE COMPUTE COVTYP AS A FUNCTION OF FMKOD
C----------
      IF (COVTYP.EQ.0) THEN
         IF (FMKOD.LT.200) THEN
            COVTYP = 10         ! PP
         ELSEIF (FMKOD.LT.400) THEN
            COVTYP = 3          ! DF
         ELSEIF (FMKOD.LT.500) THEN
            COVTYP = 8          ! ES
         ELSEIF (FMKOD.LT.900) THEN
            COVTYP = 9          ! AF
         ELSE
            COVTYP = 7          ! LP
         ENDIF
      ENDIF
C----------
C     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C----------
      XCOV(1)=10.
      XCOV(2)=60.
      DO I=1,2
         YLOAD(1)=FULIVI(I,COVTYP)
         YLOAD(2)=FULIVE(I,COVTYP)
         FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO
C
      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >       2F6.3)
C----------
C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION
C----------
      IF (IYR .EQ. IY(1)) THEN
C----------
C  ASSUME THE FUELS ARE UNPILED AND HARD. [**THIS WILL CHANGE LATER]
C
C  LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C  STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C----------
        XCOV(1)=10.
        XCOV(2)=60.
        DO ISZ = 1,MXFLCL
          YLOAD(1)=FUINII(ISZ,COVTYP)
          YLOAD(2)=FUINIE(ISZ,COVTYP)
          STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
          STFUEL(ISZ,1) = 0
        ENDDO
C----------        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
C----------
        CALL OPFIND(1,MYACT(2),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,2,JYR,IACTK,NPRM,PRMS)
          IF ((PRMS(1) .GE. 0) .AND. (PRMS(2) .GE. 0)) THEN
            CALL FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL, 
     >                      FOTOVALS)
            DO I = 1, MXFLCL
              IF (FOTOVAL(I) .GE. 0) STFUEL(I,2) = FOTOVAL(I)
              IF (I .LE. 9) STFUEL(I,1) = FOTOVALS(I)
            ENDDO                 
C----------
C  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
C----------
C  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
C----------
         CALL OPFIND(1,MYACT(1),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(2) .GE. 0) STFUEL(3,2) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(4,2) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(5,2) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(6,2) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(1,2) = PRMS(8)          
          IF (PRMS(9) .GE. 0) STFUEL(2,2) = PRMS(9)           
          IF (PRMS(1) .GE. 0) THEN
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
            ENDIF                 
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .GE. 0)) THEN
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
            ENDIF  
            IF ((PRMS(8) .GE. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
            ENDIF  
          ENDIF                
          IF (PRMS(10) .GE. 0) STFUEL(7,2) = PRMS(10) 
          IF (PRMS(11) .GE. 0) STFUEL(8,2) = PRMS(11) 
          IF (PRMS(12) .GE. 0) STFUEL(9,2) = PRMS(12)       
C----------
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)
        ENDIF

        CALL OPFIND(1,MYACT(3),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,9,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(1) .GE. 0) STFUEL(1,1) = PRMS(1)
          IF (PRMS(2) .GE. 0) STFUEL(2,1) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(3,1) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(4,1) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(5,1) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(6,1) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(7,1) = PRMS(7)          
          IF (PRMS(8) .GE. 0) STFUEL(8,1) = PRMS(8)                           
          IF (PRMS(9) .GE. 0) STFUEL(9,1) = PRMS(9)          

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

C----------
C  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C  OF BASAL AREA IN THE STAND.
C----------
        DO ISZ = 1,MXFLCL
          IF (TOTBA .GT. 0.0) THEN
            DO KSP = 1,MAXSP
              IF (TBA(KSP) .GT. 0.0) THEN
                DO J = 1,2
                  PRCL = TBA(KSP) / TOTBA
                  IDC = DKRCLS(KSP)
                  ADD = PRCL * STFUEL(ISZ,J)
                  CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                ENDDO
              ENDIF
            ENDDO
          ELSE
            IDC = DKRCLS(COVTYP)
            DO J = 1,2
              CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
            ENDDO
          ENDIF
        ENDDO
C
      ENDIF
      RETURN
C
C----------
C     HOOK TO ALLOW THE MD1() AND MD2() ARRAYS TO BE READ BY **FMCFMD*
C----------
      ENTRY EMMD(M1,M2)
      M1 = MD1(IEMTYP)
      M2 = MD2(IEMTYP)
      RETURN
C
C----------
C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      ENTRY SNGCOE
      RETURN
C
      END
