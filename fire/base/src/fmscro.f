      SUBROUTINE FMSCRO (I,SP,DEADYR,DSNAGS,ICALL)
      IMPLICIT NONE
C----------
C  $Id$
C  $Id$
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMSADD
C
*  Purpose:
*     This subroutine divides the crowns of newly created snags into
*     pools that will become down debris at specified years in the
*     future.  The weight of crown material that each snag has in each
*     size class at the time of death is taken from CROWNW.  The time by
*     which all of the material of a given size will have fallen from
*     the dead snag is extrapolated from data in (**Al's reference to
*     the old Division of Forest Economics report (1961).  The model
*     assumes that a constant proportion of the material will fall in
*     each future year up to that time.
*     NOTE:  this routine is called after CWD2B has been added to the
*     down debris pools and an FVS cycle completed.  i,e, material which 
*     is to fall in the next year of simluation should be put in 
*     CWD2B2( , ,1).  CWD2B2 will be added to CWD2B at the end of this
*     cycle.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     ANNUAL:  amount of material in a size class that will come down
*              each year
*     DEADYR:  YEAR of Death of current snag (note:  during initializ-
*              ation, this may be more than one year before the next
*              year that will be simulated).
*     DSNAGS:  Density of SNAGS to use in calculating total weight of
*              crown components (depends on whether FMSCRO has been
*              called to deal with fire-killed snags or other snags).
*     DKCL:    decay class
*     ILIFE:   lifespan of the current class of crown material, as an
*              integer value 
*     ICALL:   Where was this called from? 1=after a fire,2=after a cut
*     FALLYR:  position in CWD2B array that corresponds to the year in
*              which a pool of material should fall.
*     RLIFE:   lifespan of the current class of crown material, as a
*              real value
*     SP:      snag species
*     TSOFT:   time for snag of this species and size to become soft
*     YNEXTY:  the number of years between the year of death and the
*              next year that will be simulated.
*     YRSCYC:  YeaRS left in current FVS CYCle (including current year)
*
*  Common block variables and parameters:
*
***********************************************************************
      
C.... Parameter statements.

C.... Parameter include files.   
      INCLUDE 'PRGPRM.F77'
Cppe  INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
Cppe  INCLUDE 'PPCNTL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Variable declarations.
      INTEGER I, SP, DEADYR, SIZE, DKCL, IYR, ILIFE, YNEXTY,FALLYR
      REAL    DSNAGS, TSOFT, RLIFE, ANNUAL, NEWBOT, OLDBOT, X
      INTEGER ICALL, JYRSOFT, JADJ, JSML, JCYC, IYRS
      REAL    YRSCYC, AMT
      CHARACTER VVER*7
      LOGICAL  DEBUG
C
C     CHECK FOR DEBUG.
C
      CALL DBCHK (DEBUG,'FMSCRO',6,ICYC)
      
C.... Begin routine.
      IF (DSNAGS .LE. 0.0) RETURN
      DKCL = DKRCLS(SP) 

      YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )
Csng  YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )
Cppe  YRSCYC = FLOAT( MIY(MICYC)-DEADYR )
      
C     find out how long it will be between the year of death and the 
C     next year simulated, so that only crown material to fall in that 
C     year or later is added to CWD2B2.  

Cppe  IF (DEADYR .LT. MIY(1)) THEN
Cppe    YNEXTY = MIY(1) - DEADYR
Csng  IF (DEADYR .LT. IY(1)) THEN
Csng    YNEXTY = IY(1) - DEADYR

      IF (DEADYR .LT. IY(1)) THEN
         YNEXTY = IY(1) - DEADYR
      ELSE
         YNEXTY = 1
      ENDIF 
      
C     You can skip everything else if all material will fall before
C     the simulation begins.

      IF (YNEXTY .GT. TFMAX) GOTO 101
      
C     Call FMSNGDK to predict years, since death, for snag to become
C     soft.

      CALL VARVER(VVER)
      CALL FMSNGDK(VVER,SP,DBH(I),TSOFT)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, TSOFT, KODFOR, VVER(1:2)
    7 FORMAT(' FMSCRO CYCLE=',I2,' TSOFT=',F6.1,' KODFOR=',I5,
     &       ' VVER=',A2)
      
C     If called from CUTS, then OLDCRW will be holding last year's
C     crown info, not the dead part of the crown. Thus, we need to do
C     some additional calculations (which are normally done in FMSDIT).

      X = 1.0
      IF (ICALL .EQ. 2) THEN        
         OLDBOT = OLDHT(I) - OLDCRL(I)
         NEWBOT = HT(I) - (HT(I) * FLOAT(FMICR(I)) / 100.0)
         IF (OLDCRL(I) .GT. 0.0 .AND. (NEWBOT-OLDBOT) .GT. 0.0) THEN
            X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / YRSCYC
         ELSE
            X = 0.0
         ENDIF
      ENDIF
      
C     Divide each class of crown material equally among all the CWD2B2 
C     pools between next year and the shorter of TSOFT or TFALL. 

      DO SIZE=0,5
         
         RLIFE = TFALL(SP,SIZE)
         IF (RLIFE .GT. TSOFT) RLIFE = TSOFT
         ILIFE = INT(RLIFE)
         
C        next line asks whether ILIFE had to be truncated (or is zero), 
C        and rounds it up to the next highest integer if so.  

         IF (REAL(ILIFE) .LT. RLIFE .OR. ILIFE .LE.0) ILIFE =ILIFE+1
         RLIFE = REAL(ILIFE)

c        figure out how many of the next cycles we need to distribute things over
C        because we are no longer distributing by year. (sb 5/13)
         DO JCYC=ICYC+1,MAXCYC
             IF (RLIFE + DEADYR .LT. IY(JCYC)) GOTO 200
         ENDDO
200      CONTINUE
         JCYC = JCYC - 1
         
C        don't forget to consider the OLDCRW material as well as CROWNW.
C        but only do this if it's not mortality reconciliation time (icall =4)
C        since then oldcrw is no longer holding the dead part of the crown
C        that is to fall each year, but is carrying the crown weight instead.
C        SAR 11/20/12

         ANNUAL = CROWNW(I,SIZE)
         IF (ICALL .NE. 4) THEN
         IF (SIZE .GT. 0) ANNUAL = ANNUAL + YRSCYC*OLDCRW(I,SIZE)*X
         ENDIF
         ANNUAL = ANNUAL * DSNAGS / RLIFE

         IF (DEBUG) WRITE(JOSTND,*) 'annual=',annual,' yrscyc=',yrscyc,
     &   ' oldcrw=',OLDCRW(I,SIZE),' x=',x,' i=',i,' size=',size,
     &   ' CROWNW=', CROWNW(I,SIZE),' dsnags =',dsnags,' rlife=',rlife

         IF (ANNUAL .GT. 0.0) THEN
           FALLYR = 0
c           DO IYR=YNEXTY,ILIFE
           IF (ICALL .EQ. 4) THEN 
             ISTCYC = ICYC + 1
           ELSE
             ISTCYC = ICYC
           ENDIF
           DO IYR=ISTCYC,JCYC
c              FALLYR = IYR + 1 - YNEXTY
              FALLYR = IYR - ISTCYC +  1

C        Since we're estimating the amount per cycle, be careful here to
C        set up the cycles properly.  If you are dealing with end of cycle
C        mortality, start with the next cycle.  if you are dealing with 
C        other mortality (from fire or cuts), start with the current cycle.  
C        SAR April 2014

              IF (ICALL .EQ. 4) THEN
                IYRS = IY(IYR+1) - IY(IYR)  
              ELSE
                IYRS = IY(IYR) - IY(IYR-1)                
              ENDIF

              IF (RLIFE .LE. IYRS) THEN
                  AMT = ANNUAL * RLIFE
              ELSE
                  AMT = ANNUAL * IYRS
C                  AMT = ANNUAL * RLIFE
                  RLIFE = RLIFE - IYRS
              ENDIF        

              IF (DEBUG) WRITE(JOSTND,*) 'annual=',annual,' amt=',amt,
     &        ' iyrs=',iyrs,' fallyr=',fallyr,' iyr=',iyr

            
C           Normally, we want to put the stuff into CWD2B2, but if this is 
C           called during mortality reconciliation, CWD2B2 has already been 
C           copied to CWD2B in preparation for the next cycle, so that is
C           where we need to put the stuff.

             IF (ICALL .NE. 4) THEN
               CWD2B2(DKCL,SIZE,FALLYR) = CWD2B2(DKCL,SIZE,FALLYR) 
     >                                   + AMT
	           ELSE
               CWD2B(DKCL,SIZE,FALLYR) = CWD2B(DKCL,SIZE,FALLYR) 
     >                                   + AMT
             ENDIF
           ENDDO
        ENDIF
      ENDDO

  101 CONTINUE

      RETURN
      END

