      SUBROUTINE FMSADD (YEAR,ITYP)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C PURPOSE:
C     ADDS NEW SNAGS TO THE SNAG LIST, AVERAGING THE APPROPRIATE
C     SNAGS INTO ONE RECORD FOR EACH SPECIES-, DBH- AND HEIGHT-
C     CLASS.
C
c     **NOTE: THIS ROUTINE ASSUMES THAT MIN AND
C     MAX HEIGHTS FOR EACH CLASS HAVE ALREADY BEEN CALCULATED, AND THAT
C     IT WILL BE PASSED VARIABLES CONTAINING ALL NECCESSARY INFO.
C     **NOTE:  There is no information on cause-of-death at this time.
C----------
C     CALLED FROM: FMEFF
C                  FMKILL
C                  FMSDIT  [SERIAL FVS ONLY]
C                  FMSCUT
C                  FMSNAG
C                  INSTND  [PPE]
C
C     CALLS:       FMSCRO
C                  FMCBIO
C----------
C     Local variable definitions:
C     DENJ:   total DENsity of snags in record J
C     DBHCUT: the dbh-class number of a snag class considered for disposal
C     GAPS:   indicates whether there might be some 'gaps' in the snag
C             list (i.e., previously used records that are now empty)
C     HTCUT:  the height-class number of a snag class considered for disposal
C     IGAP:   number of the last empty snag record found
C     ITYP:   Shows where the routine is called from.  This is so that during
C             initialization, the loop will go over the entire MAXTRE list.
C             <0=keyword,1=after a fire,2=after a cut,3=initialization,
C              4=nat mort
C     LOSSES: the total number of times that existing snags are overwritten
C             or new snags discarded during this subroutine call.
C     MIDHT:  half-way point between MAXHT and MINHT
C     MINDEN: the minimum density of snags found in any existing record
C     RECORD: the record number being used for each species- and dbh-class
C             of new snags
C     SPCUT:  the species number of a snag class considered for disposal
C     TAKEN:  indicates whether a snag record is already TAKEN by one of the
C             new snag classes (0 = not taken)
C     UNFIRE: the density of snags in the current record that were NOT killed
C             by fire
C     YEAR:   the year of death of all the snags to be added during this
C             subroutine call (during initialization, this may be more than
C             one year before the next year that will be simulated).
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C
      REAL    DENJ, MIDHT(MAXSP,19), MINDEN, TOTDEN, UNFIRE
      REAL    ABIO, MBIO, RBIO, XDCAY
      INTEGER I, J, YEAR, X, DBH2, SP2, HT2, LOSSES
      INTEGER DBHCUT, HTCUT, SPCUT
      INTEGER SPCL, DBHCL, HTCL, RECORD(MAXSP,19,2), NHTCL
      INTEGER OLDITN,ITYP, IGAP, TAKEN(MXSNAG)
      LOGICAL GAPS,DEBUG
      REAL    PRMS(6)
      INTEGER NPRM,IACTK,JYR

      CALL DBCHK (DEBUG,'FMSADD',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, YEAR, LFMON, ITYP, ITRN
    7 FORMAT(' FMSADD CYCLE=',I2,' YEAR=',I5,' LFMON=',L2,
     >       ' ITYP=',I5,' ITRN=',I5)


C     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

      IF (.NOT. LFMON) RETURN

C     Initialize some variables

      LOSSES = 0
      GAPS = .TRUE.
      IF (NSNAG .LE. 3) GAPS = .FALSE.
      IGAP = 1

      DO I=1,MXSNAG
        TAKEN(I) = 0
      ENDDO


      DO SPCL=1,MAXSP
        DO DBHCL=1,19
          MIDHT(SPCL,DBHCL) = 0.0
          DO HTCL=1,2
            RECORD(SPCL,DBHCL,HTCL) = 0
          ENDDO
        ENDDO
      ENDDO

C     Look at each snag species+dbh class.  If there are snags in the class,
C     and some of them have > 0 height, then check whether the class needs
C     to be broken into 2 height classes.  (This means those classes that
C     have a height range of more than 20 feet).

      DO 40 SPCL=1,MAXSP
        DO 35 DBHCL=1,19

          IF ((DSPDBH(SPCL,DBHCL) .LE. 0.0) .OR.
     &        (MAXHT(SPCL,DBHCL) .LE. 0.0)) GOTO 35

          NHTCL = 1

          IF ((MAXHT(SPCL,DBHCL)-MINHT(SPCL,DBHCL)) .GT. 20.0) THEN
            MIDHT(SPCL,DBHCL) = (MAXHT(SPCL,DBHCL) +
     &                          MINHT(SPCL,DBHCL)) / 2.0
            NHTCL = 2
          ENDIF

C         Try to find a record in which to store each species+dbh+height
C         class of snags.

          DO 30 I=1,NHTCL

C         First, if you've never tried it or it's worked before, look
C         for a previously-used record where all the snags have fallen.

            IF (GAPS) THEN
              DO 21 J=IGAP,NSNAG
                DENJ = DENIH(J) + DENIS(J)
                IF ((DENJ .LE. 0.0) .AND. (TAKEN(J) .LE. 0)) THEN
                  RECORD(SPCL,DBHCL,I) = J
                  TAKEN(J) = 1
                  IGAP     = J
                  GOTO 29
                ENDIF
   21         CONTINUE
              GAPS = .FALSE.
            ENDIF

C           If that didn't work, and some the available records have never
C           been used, then just use the next new record.

            IF (NSNAG .LT. MXSNAG) THEN
              NSNAG = NSNAG + 1
              RECORD(SPCL,DBHCL,I) = NSNAG
              TAKEN(NSNAG) = 1
              GOTO 29
            ENDIF

C***********THIS PART IS TO DECIDE WHICH SNAGS TO OVERWRITE*************

C           Otherwise, look through all the possible records from the
C           beginning.  Figure out which has the fewest snags left in it.
C           (note:  don't count empty records that you've already TAKEN)

            MINDEN = 99999.9

            DO 22 J=1,MXSNAG
              DENJ = DENIH(J) + DENIS(J)
              IF ((DENJ .LT. MINDEN) .AND. (TAKEN(J) .LE. 0)) THEN
                MINDEN = DENJ
                RECORD(SPCL,DBHCL,I) = J
              ENDIF
   22       CONTINUE

C           If the sp-dbh class that is
C           looking for a place has as many snags (with its 2 possible
C           height classes combined) as the existing record with the
C           fewest snags, then it gets to overwrite that existing record.

            IF (DSPDBH(SPCL,DBHCL) .GE. MINDEN) THEN
              TAKEN(RECORD(SPCL,DBHCL,I)) = 1
              GOTO 27
            ENDIF

C           OK, so it still doesn't have a place.  Find out which of the
C           new snag classes that have already been assigned records has
C           the fewest snags.

            MINDEN = 99999.9

            DO SP2 = 1,SPCL
              DO DBH2 = 1,19
                DO HT2 = 1,2
                  IF ((RECORD(SP2,DBH2,HT2) .GT. 0) .AND.
     &                (DSPDBH(SP2,DBH2) .LT. MINDEN)) THEN
                    MINDEN = DSPDBH(SP2,DBH2)
                    SPCUT  = SP2
                    DBHCUT = DBH2
                    HTCUT  = HT2
                  ENDIF
                ENDDO
              ENDDO
            ENDDO

C           If this unlucky class has fewer snags than the one still
C           looking for a record, then confiscate its record.  Otherwise,
C           it's the new snags that get discarded.

            IF (DSPDBH(SPCL,DBHCL) .GT. MINDEN) THEN
              RECORD(SPCL,DBHCL,I) = RECORD(SPCUT,DBHCUT,HTCUT)
              RECORD(SPCUT,DBHCUT,HTCUT) = 0
            ELSE
              RECORD(SPCL,DBHCL,I) = 0
            ENDIF

C           When over-writing a record with remaining snags or discarding
C           new snags, monitor the problem.

   27       CONTINUE
            IF (LOSSES .EQ. 0) WRITE (*,*)
     &           'OLD SNAGS OVERWRITTEN OR NEW SNAGS DISCARDED.'
            LOSSES = LOSSES + 1

C           possible future work: if snags are being overwritten,
C           we perhaps should get the
C           overwritten snags and add them to the debris pools.


C***********THIS IS THE END OF THE DECIDING-T0-OVERWRITE SECTION************

C           If this snag class earned a record, then initialize the
C           selected record, and note the species class that will
C           be associated with it.

   29       CONTINUE
            X = RECORD(SPCL,DBHCL,I)
            IF (X .GT. 0) THEN
              DEND(X)   = 0.0
              DBHS(X)   = 0.0
              HTDEAD(X) = 0.0
              SPS(X)    = SPCL
              HARD(X)   = .TRUE.
              YRDEAD(X) = YEAR
            ENDIF

   30       CONTINUE
   35    CONTINUE
   40 CONTINUE

C     Loop over all the tree records, calculating snag averages for each
C     sp+dbh+ht class.
C     ITYP=3 means that the routine is called during initialization, and needs
C     to loop over all possible trees (since the ones it wants are between
C     MAXTRE and ITRN.

      IF (ITYP .EQ. 3) THEN
        OLDITN = ITRN
        ITRN   = MAXTRE
      ENDIF

C     If this routine is called from keyword initialization, then skip the
C     whole loop over the tree list.

      IF (ITYP .LT. 0) GOTO 51

      DO 50 I=1,ITRN

C       Skip tree records with no snags.

        IF (SNGNEW(I) .LE. 0.0) GOTO 50

C       Find the sp, dbh and ht class of the snags in this record.

        SPCL = ISP(I)

        IF (DBH(I) .GE. 36.0) THEN
          DBHCL = 19
        ELSE
          DBHCL = INT( (DBH(I)/2.0) + 1.0)
        ENDIF

        IF (MIDHT(SPCL,DBHCL) .LE. 0.0) THEN
          HTCL = 1
        ELSE
          IF (HT(I) .LT. MIDHT(SPCL,DBHCL)) THEN
            HTCL = 1
          ELSE
            HTCL = 2
          ENDIF
        ENDIF

C       Collect all the crown components of the snags NOT killed by fire
C       into the pools in CWD2B that will become down debris at the
C       appropriate times in the future. (this has already been done
C       for the fire-killed snags, to capture their fire-altered canopy
C       weights)

        IF (DEBUG) WRITE (JOSTND,45) I,SNGNEW(I),FIRKIL(I),HTCL,
     >       SPCL,DBHCL,RECORD(SPCL,DBHCL,HTCL)
   45   FORMAT (' IN FMSADD, I=',I4,' SNGNEW=',F7.3,' FIRKIL=',F7.3,
     >          ' HTCL=',I2,' SPCL=',I2,' DBHCL=',I2,' RECORD=',I6)

        IF (SNGNEW(I).GT.FIRKIL(I)) THEN
          UNFIRE = SNGNEW(I) - FIRKIL(I)
        ELSE
          UNFIRE = 0.0
        ENDIF

        CALL FMSCRO(I,SPCL,YEAR,UNFIRE,ITYP)

C       CALCULATE THE ROOT BIOMASS OF SNAGS. WHEN READING FROM
C       STAND INVENTORY, ASSUME SNAGS HAVE BEEN DEAD 10 YEARS
C       FOR CALCULATING DECAY OF DEAD ROOTS

        CALL FMCBIO(DBH(I), ISP(I), ABIO, MBIO, RBIO)
        XDCAY = 1.0
        IF (ITYP .EQ. 3) THEN
          XDCAY = 1.0
          IF (CRDCAY .GT. 0.0) THEN
            XDCAY = (1.0 - CRDCAY)**10
          ENDIF
        ENDIF
        BIOROOT = BIOROOT + (RBIO * SNGNEW(I) * XDCAY)

C       Move on to the next tree record if the snags in this one did not
C       earn a record in the snag list (due to being too rare or having
C       0 height).

        IF (RECORD(SPCL,DBHCL,HTCL) .EQ. 0) GOTO 50

C       Average the new snags in with others in their class.  Sum the
C       densities last.

        X = RECORD(SPCL,DBHCL,HTCL)
        TOTDEN    = DEND(X) + SNGNEW(I)
        HTDEAD(X) = (HTDEAD(X) * DEND(X) + HT(I) * SNGNEW(I)) / TOTDEN
        DBHS(X)   = (DBHS(X) * DEND(X) + DBH(I) * SNGNEW(I)) / TOTDEN
        DEND(X)   = TOTDEN

C       Zero out the temporary snag array (This probably isn't really
c       necessary to do here once we get the get and put running, but
c       it doesn't hurt anything.)

        SNGNEW(I) = 0.0

   50 CONTINUE
   51 CONTINUE

      IF (ITYP .EQ. 3) ITRN = OLDITN

C     Set up the snag record entered from the keyword. Note that right now
C     only one snag can be entered this way. This is a start, and we can
C     see what can be done later.

      IF (ITYP .LT. 0) THEN
        J = -ITYP
        CALL OPGET(J,6,JYR,IACTK,NPRM,PRMS)
        SPCL = PRMS(1)
        DO 162 DBHCL = 1,19
          DO 164 HTCL = 1,2
            X = RECORD(SPCL,DBHCL,HTCL)
            IF (X .LE. 0) GOTO 164
            DBHS(X)   = PRMS(2)
            HTDEAD(X) = PRMS(3)
            DEND(X)   = PRMS(6)

C           CALCULATE THE ROOT BIOMASS OF THESE SNAGS, TAKING INTO ACCOUNT
C           YEARS DEAD, IF THE KEYWORDS ARE ORDERED CORRECTLY.

            CALL FMCBIO(PRMS(2), SPCL, ABIO, MBIO, RBIO)
            XDCAY = 1.0
            IF (CRDCAY .GT. 0.0 .AND. PRMS(5) .GT. 0.0) THEN
              XDCAY = (1.0 - CRDCAY)**PRMS(5)
            ENDIF
            BIOROOT = BIOROOT + RBIO * PRMS(6) * XDCAY

            GOTO 162
  164     CONTINUE
  162   CONTINUE
      ENDIF

C     Set variables for simulated snag height equal to height at time of death,
C     divide total snags into initially-hard and initially-soft ones, and
C     re-set the max- and min-ht arrays so they're ready for the next batch
C     of snags. (Note that PSOFT is now by species (Feb 2002))

      DO SPCL = 1,MAXSP
        DO DBHCL = 1,19
          MAXHT(SPCL,DBHCL)  =    0.0
          MINHT(SPCL,DBHCL)  = 1000.0
          DSPDBH(SPCL,DBHCL) =    0.0

          DO HTCL=1,2
            X = RECORD(SPCL,DBHCL,HTCL)
            IF (X .GT. 0) THEN
              IF (DEND(X) .GT. 0) THEN
                HTIH(X)  = HTDEAD(X)
                HTIS(X)  = HTDEAD(X)
                DENIH(X) = (1.0 - PSOFT(SPCL)) * DEND(X)
                DENIS(X) = PSOFT(SPCL) * DEND(X)
                IF (DEBUG) WRITE (JOSTND,170) X,DEND(X),DENIH(X),
     >                                        DENIS(X)
  170           FORMAT (' IN FMSADD, X=',I5,' DEND=',F10.3,
     >                  ' DENIH=',F10.3,' DENIS=',F10.3)

C               Note that if this was entered via keyword, then
C               current ht may be different from ht at death:

                IF (ITYP .LT. 0) THEN
                  HTIH(X) = PRMS(4)
                  HTIS(X) = PRMS(4)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      IF (LOSSES .GT. 0) THEN
        WRITE (*,222) LOSSES
  222   FORMAT(' THIS HAPPENED ',I4,' TIMES.')
      ENDIF

      RETURN
      END

