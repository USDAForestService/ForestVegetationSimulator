      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-BM $Id$
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN
C
*  Purpose:
*     Find the dominant species (by basal area). Set the initial live
*     and dead fuel values as well. The dead fuels are only initialized
*     in the first year, but COVTYP and the live fuels must be done
*     each year.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     BAMOST:  The highest basal area in a single species
*     CAREA:   The area covered by the crown at its widest point (sqft)
*     COVINI:  The seral cover type to be used for initiating fuels in
*              bare stands
*     CRL:     Crown length
*     CWIDTH:  The maximum width of the crowns (ft)
*     FUINIE:  The initial fuel loadings for established stands (from JBrown)
*     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
*     FULIVE:  The herb/shrub/regen for established stands (from JBrown)
*     FULIVI:  The herb/shrub/regen for initiating stands (from JBrown)
*     ISWTCH:  =1 if called by SVSTART
*              =0 if called by any other subroutine (FMMAIN, FMPPHV)
*     TOTBA:   The total basal area in the stand (used in the fuels calcs)
*     TOTCRA:  The sum of the area of the crowns, per acre (sqft)
*
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
Cppe      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
Cppe      INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
C.... Variable declarations.
C
C     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
C     DIMENSION OF THE ICITYP ARRAY IN BM **BLKDAT**
C
      INTEGER MXVCODE
      PARAMETER (MXVCODE = 92)

      INTEGER*1 COVINI(MXVCODE)
      REAL BAMOST, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG

      INTEGER MYACT(3)

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
      INTEGER BMHMC(92),BMWMD(92), TEMP, MOIST, K
      REAL DKRADJ(3,3,3)

C     EACH BM HABITAT CODE MAPS TO EITHER HOT (1), MODERATE (2)
C     OR COLD (3).  (FROM FMR6SDCY)

      DATA (BMHMC(I), I=   1,  50) /
     & 3, 3, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 2, 3, 3, 2, 3, 2, 2, 2, 3, 3, 
     & 3, 2, 2, 2, 2, 2, 3, 3, 1, 1/
      DATA (BMHMC(I), I=  51,  92) /
     & 1, 1, 2, 2, 2, 1, 2, 2, 1, 2, 
     & 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     & 2, 1/

C     EACH BM HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).  (FROM FMR6SDCY)

      DATA (BMWMD(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 
     & 3, 2, 3, 2, 1, 2, 3, 1, 1, 2, 
     & 1, 1, 2, 2, 2, 2, 2, 3, 2, 3,
     & 2, 3, 3, 1, 1, 1, 2, 1, 2, 3, 
     & 3, 3, 2, 2, 2, 2, 3, 3, 3, 3/
      DATA (BMWMD(I), I=  51,  92) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
     & 3, 3, 3, 3, 3, 3, 3, 1, 1, 2,
     & 2, 2, 2, 1, 1, 1, 3, 3, 3, 2, 
     & 2, 2, 3, 3, 2, 1, 3, 2, 1, 2,
     & 2, 2/

      DATA (((DKRADJ(I,J,K), K=1,3), J=1,3), I=1,3) /           
     &  1.7,    2,  1.7, 1.49, 1.91, 1.49,  0.75, 0.85,  0.75,
     & 1.35, 1.85, 1.35,    1,  1.7,    1, 0.875,  1.2, 0.875,
     & 1.21, 1.79, 1.21, 1.14, 1.76, 1.14,  0.75, 0.85,  0.75/

C
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     (SPP 6 LEFT POPULATED TO PREVENT ZERO-TROUBLES)
C
C                  herbs, shrubs
      DATA FULIVE /0.15,  0.1, !WP
     &              0.2,  0.2, !WL
     &              0.2,  0.2, !DF
     &             0.15,  0.1, !GF
     &              0.2,  0.2, !MH
     &             0.14, 0.35, !WJ (WJ - Ottmar vol 1)
     &              0.2,  0.1, !LP
     &             0.15,  0.2, !ES
     &             0.15,  0.2, !AF
     &              0.2, 0.25, !PP
     &              0.2,  0.1, !WB (USE BM LP)
     &              0.2,  0.1, !LM (USE BM LP)
     &              0.2,  0.2, !PY (USE BM DF)
     &              0.2,  0.2, !YC (USE WC RC)
     &             0.25, 0.25, !AS (USE UT AS; Ottmar 2000?)
     &             0.25, 0.25, !CW (USE UT AS)
     &              0.2, 0.25, !OS (USE BM PP VALUES)
     &             0.25, 0.25/ !OH (USE UT AS)
C
C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C     (SPP 6 LEFT POPULATED TO PREVENT ZERO-TROUBLES)
C
C                  herbs, shrubs
      DATA FULIVI /0.30,  2.0, !WP
     &              0.4,  2.0, !WL
     &              0.4,  2.0, !DF
     &             0.30,  2.0, !GF
     &              0.4,  2.0, !MH
     &             0.10, 2.06, !WJ (WJ - Ottmar vol 1)
     &              0.4,  1.0, !LP
     &             0.30,  2.0, !ES
     &             0.30,  2.0, !AF
     &             0.25, 0.10, !PP
     &              0.4,  1.0, !WB (USE BM LP)
     &              0.4,  1.0, !LM (USE BM LP)
     &              0.4,  2.0, !PY (USE BM DF)
     &              0.4,  2.0, !YC (USE WC RC)
     &             0.18, 1.32, !AS (USE UT AS)
     &             0.18, 1.32, !CW (USE UT AS)
     &             0.25, 0.10, !OS (USE BM PP VALUES)
     &             0.18, 1.32/ !OH (USE UT AS)
C
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD (SPP 6 LEFT
C     POPULATED TO PREVENT ZERO-TROUBLES)
C
C                  <.25 to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINIE /1.0, 1.0, 1.6, 10.0,10.0,10.0, 0.0,0.0,0.0,0.8,30.0, !WP
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !WL
     &             0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !DF
     &             0.7 ,0.7, 3.0,  7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, !GF
     &             2.2, 2.2, 5.2, 15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !MH
     &             0.1, 0.2, 0.4,  0.5, 0.8, 1.0, 0.0,0.0,0.0,0.1, 0.0, !WJ (Ottmar and others vol 1 1998)
     &             0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !LP
     &             1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !ES
     &             1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !AF
     &             0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !PP
     &             0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !WB (USE BM LP)
     &             0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !LM (USE BM LP)
     &             0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !PY (USE BM DF)
     &             2.2, 2.2, 5.2, 15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !YC (USE WC RC)
     &             0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !AS (USE UT AS)
     &             0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !CW (USE UT AS)
     &             0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !OS (USE BM PP VALUES)
     &             0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8/ !OH (USE UT AS)
C
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD (SPP 6 LEFT
C     POPULATED TO PREVENT ZERO-TROUBLES)
C
C                  <.25 to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINII /0.6, 0.6, 0.8,  6.0, 6.0, 6.0, 0.0,0.0,0.0,0.4,12.0, !WP
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !WL
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !DF
     &             0.5, 0.5, 2.0,  2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, !GF
     &             1.6, 1.6, 3.6,  6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !WH
     &             0.2, 0.4, 0.2,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.2, 0.0, !WJ (Ottmar and others vol 1 1998)
     &             0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !LP
     &             0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !ES
     &             0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !AF
     &             0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !PP
     &             0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !WB (USE BM LP)
     &             0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !LM (USE BM LP)
     &             0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !PY (USE BM DF)
     &             1.1, 1.1, 3.6,  6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !YC (USE WC RC)
     &             0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !AS (USE UT AS)
     &             0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !CW (USE UT AS)
     &             0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !OS (USE BM PP VALUES)
     &             0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6/ !OH (USE UT AS)
C
C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM VEG CODES IN PCOML ARRAY IN **HABTYP**
C
      DATA (COVINI(I), I=   1,  50) /
     &  9,  9,  3,  3,  3,  3,  3,  3,  3,  3,  
     &  3,  3,  3,  9,  9,  9,  9,  8,  8,  8,
     &  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7, 
     &  7,  7,  7,  7,  7,  7,  5,  5, 10, 10/
      DATA (COVINI(I), I=  51,  92) /
     & 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
     & 10, 10, 10, 10, 10, 10, 10,  4,  4,  4,
     &  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, 
     &  4,  4,  4,  4,  4,  4,  4,  4,  4,  7,
     &  7,  7/
C
C
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)

C.... Begin routine.

C     Zero out the cumulative variables

      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA  = 0.0

C     Loop through the tree list

      IF (ITRN.GT.0) THEN

C        Zero out the cummulative variables
        BAMOST = 0.0
        TOTCRA = 0.0

        DO KSP=1,MAXSP
          FMTBA(KSP) = 0.0
        ENDDO


        DO I=1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN

            KSP = ISP(I)

            FMTBA(KSP) = FMTBA(KSP) +
     &           FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)

C             Calculate the crown width of the tree and total the area
C             encompassed by all trees

            CWIDTH=CRWDTH(I)
            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF

C         Use this loop to zero this variable, for lack of a better place.
          CURKIL(I) = 0.0
        ENDDO

C        Determine which species has the most basal area
C        -> that will be the cover type

        DO KSP=1,MAXSP
          IF (FMTBA(KSP) .GT. BAMOST) THEN
            BAMOST = FMTBA(KSP)
            COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO

C        Use the crown width information to determine the percent cover
C        of the stand. Use the equation sent by Nick which assumes that
C        crowns are randomly distrubuted in the stand:
C
C        PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

        PERCOV = 100.0 * (1.0 - EXP(-TOTCRA/43560.))

      ENDIF
C
C     If there are no trees (COVTYP=0) in cycle 1,
C     use the optional ITYPE if it is valid. Otherwise
C     issue a warning and use a default LP cover. After the
C     first cycle, use the previous cover type if no cover is
C     present.
C
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
          IF (ITYPE .GE. 1 .AND. ITYPE .LE. MXVCODE)
     &      COVTYP = COVINI(ITYPE)
          IF (COVTYP .EQ. 0) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',
     &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 3
          ENDIF
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      ENDIF

      OLDCOVTYP = COVTYP

C
C     Load live fuels as a function of PERCOV...assume that the initiating
C     stands correspond to about 10% cover and established are 60% or more.
C
      XCOV(1)=10.
      XCOV(2)=60.
      DO I=1,2
         YLOAD(1)=FULIVI(I,COVTYP)
         YLOAD(2)=FULIVE(I,COVTYP)
         FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO

      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >       2F6.3)
C
C     Initialize the dead fuels only for the first year of the simulation
C
      IF (IYR .EQ. IY(1)) THEN

        TEMP = BMHMC(ITYPE)
        MOIST = BMWMD(ITYPE)
        
        DO I = 1,9
          DO J = 1,4
            IF (I .LE. 3) THEN
              K = 1
            ELSEIF (I .LE. 5) THEN 
              K = 2
            ELSE 
              K = 3
            ENDIF
C       adjust the decay rates only if the user hasn't reset them with FuelDcay
C       also, adjust the decay rates if smaller wood is decaying more slowly than larger wood.
C       in this case, bump up the decay rate of the smaller wood to that of the larger wood.
            IF ((SETDECAY(I,J) .LT. 0) .AND. (ISWTCH .NE. 1)) THEN
              DKR(I,J) = DKR(I,J)*DKRADJ(TEMP,MOIST,K)
              IF (DKR(I,J) .GT. 1.0) DKR(I,J) = 1.0
              TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)              
            ENDIF
          ENDDO
        ENDDO

        DO I = 9,2,-1
          DO J = 1,4
            IF (((DKR(I,J)-DKR(I-1,J)) .GT. 0).AND.(ISWTCH.NE.1)) THEN 
              IF (SETDECAY(I-1,J) .LT. 0) THEN
                DKR(I-1,J) = DKR(I,J)
                TODUFF(I-1,J) = DKR(I-1,J) * PRDUFF(I-1,J)                
              ENDIF             
            ENDIF
          ENDDO
        ENDDO

C     
Csng      IF (IYR .EQ. IY(1)) THEN
Cppe      IF (IYR .EQ. MIY(1)) THEN
C
C        Load dead fuels as a function of PERCOV...assume that the initiating
C        stands correspond to about 10% cover and established are 60% or more.
C
         XCOV(1)=10.
         XCOV(2)=60.
         DO ISZ = 1,MXFLCL
            YLOAD(1)=FUINII(ISZ,COVTYP)
            YLOAD(2)=FUINIE(ISZ,COVTYP)
            STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
            STFUEL(ISZ,1) = 0
         ENDDO
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

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

C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
        
C        CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C        FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

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

C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

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


C        DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C        OF BASAL AREA IN THE STAND.

        DO ISZ = 1,MXFLCL
          IF (TOTBA .GT. 0.0) THEN
            DO KSP = 1,MAXSP
              IF (FMTBA(KSP) .GT. 0.0) THEN
                DO J = 1,2
                  PRCL = FMTBA(KSP) / TOTBA
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

      ENDIF
      
      ENTRY SNGCOE

C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

      RETURN
      END
