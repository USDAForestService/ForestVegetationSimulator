      SUBROUTINE BMFIRE (ISTD, IYR)
C----------
C WWPB $Id$
C----------
C     CALLED FROM BMDRV
C     CALLS   BMFLD
C             BMFMRT
C             BMFINT
C             GPGET2
C             GPADD
***********************************************************************
*  **BMFIRE  Date of last revision:  June 14, 1994
*----------------------------------------------------------------------
*  Purpose:
*     This subroutine is the driving part of the fire model. It first
*     determines the environmental variables, then selects the appropriate
*     fuel model. Next, all the other subroutines are called to determine
*     prob of mort. This is based on information from Jim Brown
*     and Elizabeth Weinhardt.
*----------------------------------------------------------------------
*
*  Local variable definitions:   
*     FDONE:  Logical indicating if any fire occurred this year
*     FMOIS:  Fuel/moisture codes
*     SH:     Scorch height (in feet) 
*     STLAST: Last stand that could be called
*
*  Common block variables and parameters:
*     DWPHOS: From BMCOM; Array to hold volumes of downed host tree volume for
*             Ips (stratified by standing/dead and size class)
*     MXDWSZ: From BMPRM; Maximum number of Dead Woody Pool size classes
*
C     Keywords: FIRED yrstart, period, firetype (fmois)    (use defaults)
C        or     FIRES yrstart, period, MOIS(), ATEMP, FWIND
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.
      INCLUDE 'BMCOM.F77' 
      INCLUDE 'BMFCOM.F77'

C.... Variable declarations.
      
      INTEGER I, IPC, IAGE, FMOIS, FMD 
      INTEGER DUM(1)
      LOGICAL FDONE, LOK
      REAL    SMBURN, LGBURN
      REAL    SH       

      DIMENSION PRMS(2)  

      SAVE

C.... Check for debug.
      IF (LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT (' Begin BMFIRE: Year= ',I5, 'Stand= ', I6)

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
      IF (ICNT .GE. BMEND) ICNT = 0      
      ICNT = ICNT + 1
      
C     CHECK ACTIVITY SCHEDULER.

      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 2 
        FMOIS = 0
        FDONE = .FALSE.

C     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
C     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
C     SET OF PARAMETERS.
         
        CALL GPGET2 (313, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN
  
C         (iyr2 is the next year that fire could occur, assuming it occurred
c           this year - prms(1) is the fire period, in years)
          IYR2= IYR1 + IFIX(PRMS(1))

          FMOIS = INT(PRMS(2))

          IF (LBMDEB) WRITE (JBMBPR,101) IYR1,FMOIS
  101     FORMAT (/' IN BMFIRE: IYR1=',I5,' FMOIS=',I4)
  
        ENDIF 
      ENDIF    
      
C     Zero the scorch array
      DO 203 ISIZ= 1,NSCL
         SCORCH(ISTD,ISIZ)= 0.0
  203 CONTINUE   
      
C     If the fuel model type is 0 then fire is not active this year.

      IF (FMOIS .LE. 0) RETURN


C.... If fire model contains different number of classes, ND and NL will be
C     changed there.
      
C     1hr, 10hrs, 100hrs
      ND = 3

C     live
      NL = 0

C.... Begin Routine
      
      
C.... Select environmental variables: fuel mositure, air temp, wind speed
      
      IF (FMOIS .EQ. 1) THEN

C         "very low moisture" or wildfire case

          MOIS(1, 1) = .04
          MOIS(1, 2) = .05
          MOIS(1, 3) = .05
          MOIS(2, 1) = .7
          ATEMP = 90.0
          FWIND = 20.0 * .3

      ELSEIF (FMOIS .EQ. 2) THEN

C         "low moisture" or precribed fire-low mc

          MOIS(1, 1) = .08
          MOIS(1, 2) = .08
          MOIS(1, 3) = .1
          MOIS(2, 1) = 1.1
          ATEMP = 70.0
          FWIND = 10.0 * .3

      ELSEIF (FMOIS .EQ. 3) THEN

C         "moderate moisture" or prescribed fire-mod mc

          MOIS(1, 1) = .12
          MOIS(1, 2) = .12
          MOIS(1, 3) = .14
          MOIS(2, 1) = 1.5
          ATEMP = 70.0
          FWIND = 4.0 * .3

      ELSEIF (FMOIS .EQ. 4) THEN

C         "high moisture"

          MOIS(1, 1) = .16
          MOIS(1, 2) = .16
          MOIS(1, 3) = .18
          MOIS(2, 1) = 1.5
          ATEMP = 70.0
          FWIND = 6.0 * .3

      ENDIF
      
C.... Calculate fuel loadings which will modify fuel model selection
      
C      CALL BMFLD(SMALL, LARGE) 

C     These calculations ignore any crownfall from standing dead trees
      DO 100 IAG=1,MXDWAG
        SMALL = SMALL + DDWP(ISTD,1,IAG)
        LARGE = LARGE + DDWP(ISTD,2,IAG)
  100 CONTINUE

      
C.... select appropriate fire model
C     based (somewhat) on habitat type and fuel loading
      IF (HABTYP(ISTD) .LE. 0) THEN
         FMD = 8
      ELSEIF (HABTYP(ISTD) .LT. 400) THEN
         FMD = 9
      ELSE 
         FMD = 8
      ENDIF

c     the amount of small and large fuel present can modify the choice of model.
      IF ((SMALL .LE. 6) .AND. (LARGE .GT. 15)) FMD = 10
      IF ((SMALL .GT. 6) .AND. (SMALL .LE. 15)
     &                          .AND. (LARGE .LE. 20)) FMD = 10
      IF ((SMALL .GT. 6) .AND. (SMALL .LE. 15)
     &                          .AND. (LARGE .GT. 20)) FMD = 12
      IF ((SMALL .GT. 15) .AND. (SMALL .LE. 30)
     &                          .AND. (LARGE .LE. 25)) FMD = 12
      IF ((SMALL .GT. 15) .AND. (SMALL .LE. 30)
     &                          .AND. (LARGE .GT. 25)) FMD = 13
      IF ((SMALL .GT. 30) .AND. (LARGE .GE. 30)) FMD = 13
      
C.... from letter from Jim Brown
      IF (FMD .EQ. 1) THEN        

C        Short grass

         MPS(1, 1) = 3500
         FWG(1, 1) = .034
         DEPTH = 1.0
         MEXT(1) = .12
         ND = 1

      ELSEIF (FMD .EQ. 2) THEN

C        Timber (grass & understory)

          MPS(1, 1) = 3000
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          MPS(2, 1) = 1500
          FWG(1, 1) = .092
          FWG(1, 2) = .046
          FWG(1, 3) = .023
          FWG(2, 1) = .023
          DEPTH = 1.0
          MEXT(1) = .15
          NL = 1
       ELSEIF (FMD .EQ.8) THEN

C         Closed timber litter

          MPS(1, 1) = 2000
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          FWG(1, 1) = .069
          FWG(1, 2) = .046
          FWG(1, 3) = .115
          DEPTH = .2
          MEXT(1) = .3

       ELSEIF (FMD .EQ. 9) THEN

C         Hardwood litter

          MPS(1, 1) = 2500
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          FWG(1, 1) = .134 
          FWG(1, 2) = .019
          FWG(1, 3) = .007
          DEPTH = .2
          MEXT(1) = .25
    
      ELSEIF (FMD .EQ. 10) THEN

C         Timber (litter & understory)

          MPS(1, 1) = 2000
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          MPS(2, 1) = 1500
          FWG(1, 1) = .138
          FWG(1, 2) = .092
          FWG(1, 3) = .23
          MPS(2, 1) = .092
          DEPTH = 1.0
          MEXT(1) = .25

C         living class is present

          NL = 1

       ELSEIF (FMD .EQ. 12) THEN

C         Medium logging slash

          MPS(1, 1) = 1500
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          FWG(1, 1) = .184
          FWG(1, 2) = .644
          FWG(1, 3) = .759
          DEPTH = 2.3
          MEXT(1) = .2

       ELSEIF (FMD .EQ.13) THEN

C         Heavy logging slash

          MPS(1, 1) = 1500
          MPS(1, 2) = 109
          MPS(1, 3) = 30
          FWG(1, 1) = .322
          FWG(1, 2) = 1.058
          FWG(1, 3) = 1.288
          DEPTH = 3
          MEXT(1) = .25
      ENDIF
      
C.... Compute Byram's Fireline Intensity
      
      CALL BMFINT(BYRAM, FLAME, ISTD)
      
C.... Calculate scorch height (in feet)
C     Convert byram to BTU/ft/second (rather than min)
      
      BYRAM = BYRAM / 60.0
      SH = (63.0 / (140.0 - ATEMP)) * (BYRAM ** (7.0 / 6.0)
     &                  / (BYRAM + FWIND ** 3.0) ** 0.5)
      
C.... If fire occurs then reduce fuel loadings: 'SMALL' is assumed to
C      comprise 'S/DDWP( )' size category 1 (<3") & LARGE is
C      everything above that
      
      IF (FLAG(1) .EQ. 0) THEN

c        FIRE OCCURRED
         FDONE = .TRUE.

         IF ((FMD .EQ. 10) .OR. (FMD .EQ. 12)
     &            .OR. (FMD .EQ. 13)) THEN
            SMBURN = .2
         ELSE
            SMBURN = .35
         ENDIF
      
         LGBURN = .5
      
         DO 1000 IAGE=1,5  
            DDWP(ISTD,1,IAGE) = DDWP(ISTD,1,IAGE) * SMBURN

            DO 1100 I=2,MXDWSZ
                  DDWP(ISTD,I,IAGE) = DDWP(ISTD,I,IAGE) * LGBURN
 1100       CONTINUE

            DO 1110 IPC=1,3
      
               SDWP(ISTD,IPC,1,IAGE) = SDWP(ISTD,IPC,1,IAGE) * SMBURN
      
               DO 1115 I=2,MXDWHZ + 1
                  SDWP(ISTD,IPC,I,IAGE) = SDWP(ISTD,IPC,I,IAGE) * LGBURN
 1115          CONTINUE
      
 1110       CONTINUE
 1000    CONTINUE
      
C....    Don't forget to burn new slash too
      
         DO 2000 IPC=1,2
            DO 2200 I=1,MXDWHZ
               DWPHOS(ISTD,IPC,I) = DWPHOS(ISTD,IPC,I) * LGBURN
 2200       CONTINUE
 2000    CONTINUE
         
C....    Calculate probability of tree mortality
      
         CALL BMFMRT(ISTD, IYR, SH, FMD) 

      ENDIF

      IF (ICNT .GE. BMEND .AND. FDONE) THEN
      
C        SINCE FIRE OCCURRED IN SOME STAND THIS YEAR, RESCHEDULE FIRE 
C        TO OCCUR AFTER PERIOD NUMBER OF YEARS (IYR2)
         
         CALL GPADD (KODE, IYR2, 313, NPRMS, PRMS, 1, DUM) 
         IF (LBMDEB) WRITE (JBMBPR,902) IYR2
  902    FORMAT (/' IN BMFIRE: FIRE OCCURRED AND IS RESCHEDULED ',
     >     'FOR ',I5)
      
      ELSEIF (ICNT .GE. BMEND) THEN
      
C     FIRE DID NOT OCCUR IN ANY STAND, SO FIRE SHOULD BE RESET TO 
C         OCCUR POSSIBLY NEXT YEAR.      
                    
          CALL GPADD (KODE, (IYR+1), 313, NPRMS, PRMS, 1, DUM) 
          IF (LBMDEB) WRITE (JBMBPR,903) IYR+1
  903     FORMAT (/' IN BMFIRE: FIRE DID NOT OCCUR AND IS RESCHEDULED ',
     &       'FOR NEXT YEAR:',I5)
      ENDIF
                         
C.... Common Return

 9999 CONTINUE
     
      IF (LBMDEB) WRITE (JBMBPR,999) IYR, ISTD
  999 FORMAT (' End BMFIRE :  Year=',I5, ' Stand= ',I6)

      RETURN
      END
