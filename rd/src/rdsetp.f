      SUBROUTINE RDSETP
      IMPLICIT NONE
C----------
C  **RDSETP      LAST REVISION:  09/03/14
C----------
C
C  SUBROUTINE FOR CALCULATING VALUES OF ROOT DISEASE VARIABLES
C  IN INITIALIZATION PHASE. ALSO CALLS RDAREA TO CALCULATE INITIAL
C  ROOT DISEASE AREA AND RDPR TO PRINT REPORT OF ROOT DISEASE
C  CONDITIONS. VALUES FOR THESE VARIABLES ARE ZEROED IN RDINIT.
C  PROGNOSIS VARIABLE PROB MAY BE SET IN THIS ROUTINE IF SAMPLING
C  ROOT DISEASE FROM TWO POPULATIONS (IE DISEASED AND NON-DISEASED
C  SUB-PLOTS.) ROOT DISEASE MODEL VARIABLE PROBL IS THEN LOADED
C  FROM PROB.
C
C  Called By :
C     RDMN1   [ROOT DISEASE]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [FVS]
C     RDCLOC  (SUBROUTINE)   [ROOT DISEASE]
C     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
C     RDIPRP  (SUBROUTINE)   [ROOT DISEASE]
C
C  Local Variables :
C  <incomplete>
C
C  Common Block Variables Used :
C     DIMEN  - (RDCOM)   (O)
C     FPROB  - (RDARRY)  (O)
C     I1, I2 - (RDCOM)   (O)
C     ICYC   - (CONTRL)  (I)
C     IND1   - (ARRAYS)  (I)
C     IPCFLG - (RDCOM)   (I)
C     IRDPLT - (RDADD)   (I)
C     IREC1  - (CONTRL)  (I)
C     ISCT   - (CONTRL)  (I)
C     JOSTND - (CONTRL)  (I)
C     LONECT - (RDADD)   (O)
C     MAXSP  - (PRGPRM)  (I)
C     PAREA  - (RDCOM)   (I)
C     PI     - (PLOT)    (I)
C     PRINF  - (RDCOM)   (O)
C     PRKILL - (RDCOM)   (I)
C     PROB   - (PROB)    (I/O)
C     PROBI  - (RDARRY)  (O)
C     PROBIT - (RDARRY)  (I)
C     PROBIU - (RDARRY)  (O)
C     PROBL  - (RDARRY)  (O)
C     PROPI  - (RDARRY)  (O)
C     PRPTOT - (RDCOM)   (O)
C     PRUN   - (RDCOM)   (I)
C     RRGEN  - (RDCOM)   (O)
C     RRINCS - (RDCOM)   (I)
C     RRTINV - (RDADD)   (I)
C     SAREA  - (RDCOM)   (I)
C     <incomplete>
C
C  Revision History :
C     05/03/97 - Matthew K. Thompson (FHTET)
C                Substituted the variable IREC1 for ITRN everywhere
C                in the subroutine.  When RDSETP is called the value
C                of ITRN (number of tree records) still includes
C                recently dead trees.  IREC1 is the number of
C                projectable records.
C
C     05/13/97 - Matthew K. Thompson (FHTET)
C                Changed code so that if number of plots equals number
C                of disease plots than patch area equals stand area. 
C                Re-numbered statement labels.  Fixed call to RDIPRP,
C                an element of the array TMP was being passed to RDIPRP 
C                not the whole array.  Removed the section of code that
C                recalculates PROB.
C     06/02/98 - Robert N. Havis (FHTET)
C                Branched out of tree loop in several places
C                when non-host tree record was beeing analysed
C     17-JUL-2002 Lance R. David (FHTET)
C                Modified/added debug code.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C------------------------------
C
C.... Parameter include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations

      LOGICAL DEBUG
      INTEGER I, I1, I2, IDI, IIPI, IK, J, K, KSP, NDPLTS(ITOTRR)
      REAL    PROPN(MAXTRE), RDRANP, TMP(ITOTRR), TPROP

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDSETP',6,ICYC)
      IF (DEBUG) WRITE (JOSTND,100) ICYC,MINRR,MAXRR
  100 FORMAT(' ENTER RDSETP: CYCLE=',I4,' MINRR=',I1,' MAXRR=',I1)

C.... Calculate number of diseased sub-plots.  If user has specified
C.... diseased and non-diseased sub-plots and number of diseased plots
C.... equals the number of total plots in the inventory then stand is
C.... considered as one center.

      DO 300 IDI=MINRR,MAXRR
         NDPLTS(IDI) = 0

         DO 200 K = 1,50
            IF (IRDPLT(IDI,K) .NE. 0) THEN
               NDPLTS(IDI) = NDPLTS(IDI) + 1
            ENDIF
  200    CONTINUE
  300 CONTINUE

      IIPI = INT(PI) 
      DO 303 IDI=MINRR,MAXRR
         IF (IIPI .EQ. NDPLTS(IDI)) THEN
            LONECT(IDI) = 1
            PAREA(IDI) = SAREA
         ENDIF

         IF (LONECT(IDI) .EQ. 1) NDPLTS(IDI) = 1
  303 CONTINUE

C.... Initialize the variables for the SDI effect on roots.

      IF (SDISLP .EQ. 0.0 .OR. SDNORM .EQ. 0.0) THEN
         YINCPT = 1.0
      ELSE
         YINCPT = 1.0 - (SDNORM * SDISLP)
      ENDIF

C.... Set square dimension of the stand.

      DIMEN = 0.0
      IF (SAREA .NE. 0.0) THEN
         DIMEN = SQRT(SAREA) * 208.7
      ENDIF

C.... Calculate the area of the disease patches.

      DO 600 IRRSP=MINRR,MAXRR
         IF (IPCFLG(IRRSP) .EQ. 1) THEN

C....       Disease centers are assigned by the user.

            CALL RDAREA
         ELSE

C....       DISEASE CENTERS ARE ASSIGNED RANDOMLY.

            IF (PAREA(IRRSP) .GT. 0.0) THEN
               CALL RDCLOC
               CALL RDAREA
            ELSE
               PAREA(IRRSP) = SAREA * NDPLTS(IRRSP) / PI
               CALL RDCLOC
            ENDIF
         ENDIF

         IF (DEBUG) WRITE(JOSTND,400) PAREA(1), PAREA(2), PAREA(3),
     &                                PAREA(4)
  400    FORMAT (' IN RDSETP: PAREA=',4F10.2)

         SPPROP(IRRSP) = 0.0

C....    If not initialized from the tree list, use manual esitmate of
C....    number of infected and uninfected trees in disease pockets??
C....    PRKILL and PRUN are densities of trees inside centers. They
C....    are changed into proportions here, using RRGEN, which is
C....    then set to 1.0.

         IF (.NOT. RRTINV .AND. .NOT. LPLINF) THEN
            RRGEN(IRRSP,1) = PRKILL(IRRSP) + PRUN(IRRSP)

            IF (RRGEN(IRRSP,1) .GT. 0.0) THEN
               PRKILL(IRRSP) = PRKILL(IRRSP) / RRGEN(IRRSP,1)
               PRUN(IRRSP) = PRUN(IRRSP) / RRGEN(IRRSP,1)
               RRGEN(IRRSP,1) = 1.0
            ENDIF

         ELSEIF (.NOT. RRTINV .AND. LPLINF) THEN
            RRGEN(IRRSP,1) = 1.0
         ENDIF

         IF (DEBUG) WRITE (JOSTND,500) PI, NDPLTS(1), NDPLTS(2),
     &                                 NDPLTS(3), NDPLTS(4)
  500    FORMAT (' IN RDSETP: PI NDPLTS=',F3.0,4I5)
  600 CONTINUE

C.... Figure out what proportion of each record becomes infected.
C.... PROPN is by tree record.  If the PLOTINF keyword is used, the
C.... same species may have different infection rates.

      IF (.NOT. RRTINV .AND. .NOT. LPLINF) THEN
         CALL RDIPRP(PROPN,0,PRKILL)
      ENDIF

      IF (.NOT. RRTINV .AND. LPLINF) THEN

C....    Initialized via the PLOTINF keyword

         DO 700 IDI=MINRR,MAXRR
            TMP(IDI) = 0.0
  700    CONTINUE

         DO 900 IDI=MINRR,MAXRR
            IF (IDI .GT. MINRR) TMP(IDI-1) = 0.0

            DO 800 IK=1,50  
               IF (IANPLT(IDI,IK) .LE. 0) GOTO 900
               TMP(IDI) = PLPROP(IDI,IK)

               CALL RDIPRP(PROPN,IANPLT(IDI,IK),TMP)
                
  800       CONTINUE                          
  900    CONTINUE                          
      ENDIF

C.... Load the initial tree list from FVS into the root disease
C.... tree list arrays that characterize the trees as inside and
C.... outside of patches  (also infected and uninfected).
C
C     Note 

      IDI = MAXRR
      DO 2100 I = 1, IREC1

C.....   If Annosus then select S-type or P-type.

C         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I))) ! changed 11/24/2015

C....    If both P and S type Annosus is being used, set disease area
C....    to that of the disease type of the host tree species; otherwise,
C....    use the disease area of the one disease being simulated even though
C....    current tree may not be specified as host species.

         IF (MAXRR .LT. 3 .AND. MINRR .NE. MAXRR) THEN
            IDI = IDITYP(IRTSPC(ISP(I)))
         ENDIF

C
C     Branch outof loop for non-host tree ((RNH June98)
C
      IF (IDI .LE. 0) GO TO 2100
C
C
         IF (RRTINV .AND. .NOT. LPLINF) THEN

C....       The model is being initialized from the tree list.

            IF (IPRFL(I) .EQ. 1) THEN

C....          This tree record has a severity code 1, tree within
C....          30 feet of infected tree.  This tree is given an infection
C....          level around 10% of the tree species infection level at  
C....          death.

               IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 1.'

               TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.1
               PROPI(I,1,1) = RDRANP(TPROP)

            ELSEIF (IPRFL(I) .EQ. 2) THEN

C....          This tree record has a severity code 2, pathogen or
C....          diagnostic symptoms detected.  This tree is given an
C....          infection level around 65% of the tree species infection
C....          level at death.

               IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 2.'

               TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.65
               PROPI(I,1,1) = RDRANP(TPROP)

            ELSEIF (IPRFL(I) .EQ. 3) THEN

C....          This tree record has a severity code 3, crown
C....          deterioration detected.  This tree is given an infection
C....          level around 75% of the tree species infection level at
C....          death.

               IF (DEBUG) WRITE (JOSTND,*) '# Severity Code 3.'

               TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.75
               PROPI(I,1,1) = RDRANP(TPROP)

            ELSE

C....          Model is initialized from the tree list.  This tree
C....          record does not show infection (no root disease damage
C....          code).  The tree record is given a random proportion
C....          of roots infected for new infection. MEAN = 0.001.
C....    
C....          I don't see why we initialize root disease level in
C....          a tree with no root disease. It doesn't matter at any
C....          point but seems useless.
C              PROPI(I,1,1) = RDRANP(0.001)

               IF (DEBUG) WRITE (JOSTND,*) '# No Infection.'

            ENDIF

C....       Scale PROB variables based on how patches are being
C....       defined. 

            IF (LONECT(IDI) .EQ. 1 .OR.
     &             (SAREA .EQ. PAREA(IDI))) THEN

C....          Stand being run as one disease center so scale PROB
C....          variables by value of patch area.

               IF (DEBUG) WRITE (JOSTND,1400)
 1400          FORMAT(' # Run as 1 center, set PROBIU and PROBI.') 

               PROBIU(I) = PROBIU(I) * PAREA(IDI)
               PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)
               FPROB(I) = 0.0

            ELSEIF (LONECT(IDI) .EQ. 0) THEN

C....          Stand being run without knowledge of the patches so
C....          scale PROB variables by value of patch area. 

               IF (DEBUG) WRITE (JOSTND,1500)
 1500          FORMAT(' # No knowledge of patches, ',
     &                'set PROBIU and PROBI.') 

               PROBIU(I) = PROBIU(I) * PAREA(IDI)
               PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)

               IF (PAREA(IDI) .EQ. SAREA) THEN

C....             If patch area equals stand area then there are no
C....             trees outside patches.

                  IF (DEBUG) WRITE (JOSTND,1525)
 1525             FORMAT(' # No knowledge of patches, PAREA = SAREA.') 

                  FPROB(I) = 0.0

               ELSE

C....             Calculate trees/acre outside of the patches.

                  IF (DEBUG) WRITE (JOSTND,1550)
 1550             FORMAT(' # No knowledge of patches, PAREA /= SAREA.') 

                  FPROB(I) = (PROB(I) * SAREA - (PROBIU(I) +
     &                       PROBI(I,1,1))) / (SAREA - PAREA(IDI))
               ENDIF

            ELSEIF (LONECT(IDI) .EQ. 2) THEN

C....          Stand being run as multiple plots so scale PROB
C....          variables by value of the stand area.

               IF (DEBUG) WRITE (JOSTND,1600)
 1600          FORMAT(' # Run as multiple plots, ',
     &                'set PROBIU, PROBI, and FPROB.') 

               PROBIU(I)  = PROBIU(I) * SAREA
               PROBI(I,1,1) = PROBI(I,1,1) * SAREA
               FPROB(I)   = FPROB(I) * (SAREA / (SAREA - PAREA(IDI)))
            ENDIF

            IF (DEBUG) WRITE (JOSTND,1650) I, ISP(I), PROB(I),
     &                                     PROBI(I,1,1), PROBIU(I),
     &                                     FPROB(I)
 1650       FORMAT (' IN RDSETP: BEFORE-1 SCALE: I ISP PROB PROBI ',
     &              'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

         ELSE

C....       Model is initialized manually from the RRINIT keyword
C....       and/or the model is initialized through the PLOTINF keyword.
C....

            IF ((.NOT. LPLINF .OR. IDPLOT(I) .GT. 0) .AND.
     &          IDI .NE. 0) THEN

C....          If (not using PLOTINF or tree records are inside
C....          infected plots) and the tree species is a host then
C....              
C....          The proportion of infection for each tree record is taken
C....          from the random proportion function with the mean being
C....          the value entered by the user in field 5 of the RRINIT
C....          keyword.

               IF (DEBUG) WRITE (JOSTND,1700)
 1700          FORMAT(' # Using only RRINIT, or tree record inside ',
     &                'infected plot specified by PLOTINF ',
     &                'set PROBIU, PROBI, and FPROB.') 

               IF (PAREA(IDI) .GT. 0.0)
     &            PROPI(I,1,1) = RDRANP(RRINCS(IDI))

               PROBI(I,1,1) = PROPN(I) * PROB(I) * PAREA(IDI) /
     &                        (RRGEN(IDI,1) + 0.00001)
               PROBIU(I) = (1.0 - PROPN(I)) * PROB(I) *
     &                      PAREA(IDI) / (RRGEN(IDI,1) + 0.00001)
               FPROB(I) = PROB(I)

            ELSE 

C....          Initialized by PLOTINF keyword, but this record
C....          belongs to a plot which is outside disease centers

C
C     If non-host species exit loop (RNH May98)
C
      IF (IDI .LE. 0) GO TO 2100
C
      IF (DEBUG) WRITE (JOSTND,1800)
 1800          FORMAT(' # Using PLOTINF, this tree is in plot which ',
     &                'is outside disease centers, set FPROB.') 

               PROBI(I,1,1) = 0.0
               PROBIU(I) = 0.0
               FPROB(I)  = (PROB(I) * SAREA - PROBIU(I) -
     &                     PROBI(I,1,1)) / (SAREA - PAREA(IDI))

            ENDIF

            IF (DEBUG) WRITE (JOSTND,1900) I, ISP(I), PROB(I),
     &                                     PROBI(I,1,1), PROBIU(I),
     &                                     FPROB(I)
 1900       FORMAT (' IN RDSETP: BEFORE-2 SCALE: I ISP PROB PROBI ',
     &              'PROBIU FPROB ', I4, I4, 4(2X,F10.2))


         ENDIF

C....    If stand is being modeled as diseased and non-diseased
C....    subplots then modify PROB to recognize the effect of stand
C....    being the sum of two populations.

CM       IF (LONECT(IDI) .EQ. 2) THEN
CM          PROB(I) = (PROBI(I,1,1) + PROBIU(I) + FPROB(I) *
CM   &                (SAREA - PAREA(IDI))) / (SAREA + 1E-6)
CM       ENDIF

C....    Now load PROBL after PROB has been modified.

         PROBL(I) = PROB(I)

         IF (DEBUG) WRITE(JOSTND,2000) I,ISP(I),PROB(I),PROBI(I,1,1),
     &              PROBIU(I),FPROB(I)
 2000    FORMAT (' IN RDSETP: AFTER    SCALE: I ISP PROB PROBI ',
     &           'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

 2100 CONTINUE

C.... End tree loop.

      CALL RDSUM (IREC1,PROBIT,PROBI,ISTEP)

C.... Decay the root system of stumps initialized through STREAD or
C.... the treelist since the stumps may have been there for longer than
C.... just the inventory year.

      CALL RDINOC (.TRUE.)

C.... Accumulate proportion of root systems infected inside patches
C.... for output in inventory year.
C....
C.... (Remember that PRINF & PRPTOT contain RR info for first ITOTRR
C.... slots and tree species info after that.)

      DO 2200 IDI=1,ITOTRR
         PRPTOT(IDI) = 0.0
         PRINF(IDI) = 0.0
 2200 CONTINUE

      IDI = MAXRR

      DO 2400 KSP = 1,MAXSP
         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 2400
C
         IF (ISCT(KSP,1) .EQ. 0) GOTO 2400
         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         PRINF(KSP+ITOTRR)  = 0.0
         PRPTOT(KSP+ITOTRR) = 0.0

         DO 2300 J = I1,I2
            I = IND1(J)

C           If IND1(j) is pointing to a dead tree (bottom part of tree
C           list), then skip it. Also skip if PROPI < 0

            IF (I .GT. IREC1) GOTO 2300

            IF (PROPI(I,1,1) .GT. 0.0) THEN
               PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) + PROBI(I,1,1) *
     &                              PROPI(I,1,1)
               PRINF(IDI) = PRINF(IDI) + PROBI(I,1,1) * PROPI(I,1,1)
            ENDIF

            PRPTOT(KSP+ITOTRR) = PRPTOT(KSP+ITOTRR) + PROBI(I,1,1)
            PRPTOT(IDI)   = PRPTOT(IDI) + PROBI(I,1,1)
 2300    CONTINUE

         PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) /
     &                       (PRPTOT(KSP+ITOTRR) + 1E-6)

 2400 CONTINUE

      DO 2500 IDI=MINRR,MAXRR
         PRINF(IDI) = PRINF(IDI) / (PRPTOT(IDI) + 1E-6)
 2500 CONTINUE
 
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDSETP'

      RETURN
      END
