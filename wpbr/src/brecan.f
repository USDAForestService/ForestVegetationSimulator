      SUBROUTINE BRECAN(IBRN,HITE,RINDX,SSTAR,SSTHT,PROP,PIMX,EXPC)
C**********************************************************************
C  **BRECAN       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRECAN calculates the number of new cankers expected for a
C  tree for one year.  When a new canker is placed into the
C  arrays which hold the information of cankers tracked by the
C  model, distance up (DUP array) is randomly placed within the
C  crown and distance out on branch (DOUT) is calulated based
C  on the distance up.
C----------------------------------------------------------------------
C
C  Parameters Passed:
C        IBRN - index for current tree
C        HITE  - total tree height at end of cycle in meters for
C                current tree. (current height  +  height growth)
C        RINDX - rust index value for current tree
C        SSTAR - summed target area for current tree
C        SSTHT - this years height in meters for current tree
C                (current height  +  proportion of height growth)
C        PROP  - proportion of full cycle represented as of current year
C        PIMX  - proportion trees infected maximum for the stand 
C     Returned:
C        EXPC  - expected number of cankers for current tree (record)
C                this year.
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  19-MAY-1999 Lance David
C     Added debug code.
C  15-MAR-2001 Lance R. David (FHTET)
C     Updated DFACT variable to array by species and stock type.
C  22-MAR-2001 Lance R. David (FHTET)
C     Added PIMX parameter and control for assigning escape status
C     code 9 to trees. Once a tree is tagged as escape, it will never
C     become infected.
C  03-MAY-2001 Lance R. David (FHTET)
C     Added species dimension to variables (arrays).
C  10-MAY-2006 Lance R. David (FHTET)
C     Added debug.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.
      INTEGER IBRN,NUMTIM
      REAL    HITE,RINDX,SSTAR,SSTHT,PROP,PIMX,EXPC,RITEM,TNEWC,
     &        CRLEN,PLI,TOUT,TUP,PLETH,XBRAN
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRECAN',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,22) ICYC,
     & IBRN,ISP(IBRN),HITE,RINDX,SSTAR,SSTHT,PROP,
     & PIMX,IBRSTAT(IBRN),ILCAN(IBRN),ITCAN(IBRN)
  22  FORMAT('Entering subroutine BRECAN: cycle = ',I2,/,
     & 'IBRN=',I4,' ISP=',I2,' HITE=',F10.7,' RINDX=',F10.7,
     & ' SSTAR=',F10.4,' SSTHT=',F10.7,' PROP=',F10.7,
     & ' PIMX=',F10.7,' IBRSTAT=',I2,' ILCAN=',I2,' ITCAN=',I3)

      EXPC = 0.0

C.... set species code for index.
      I3 = BRSPM(ISP(IBRN))
      IF(DEBUG) WRITE(JOSTND,*) 'I3=',I3,' PROB=',PROB(IBRN),
     & ' PITCA=',PITCA(I3),' TRETN=',TRETN(I3),' THPROB=',THPROB(I3)

      IF(IBRSTAT(IBRN).EQ.0) THEN
C....    If the current tree is a clean tree and stand infection max has
C....    not been met, need to update proportion of infected trees.
C....    Set tree status code so that this happens only once.
         IF(PITCA(I3) .LT. PIMX) THEN
            IBRSTAT(IBRN) = 1
            TRETN(I3) = TRETN(I3) + PROB(IBRN)
            PITCA(I3) = TRETN(I3)/THPROB(I3)
            IF(DEBUG) WRITE(JOSTND,*) ' NEW TREE - ',IDTREE(IBRN),
     &      'PITCA=',PITCA(I3),'TRETN=',TRETN(I3),'PROB=',PROB(IBRN),
     &      'THPROB=',THPROB
         ELSE IF(PITCA(I3) .GE. PIMX) THEN
C....       Infection level has been met.
C....       Reserve this escape tree with IBRSTAT code 9.
C....       During previous Years of cycle, expected cankers may
C....       have accumulated for this tree. Zero them out.
            IBRSTAT(IBRN) = 9
            ESTCAN(IBRN) = 0.0
            ITCAN(IBRN) = 0
            IF(DEBUG) WRITE(JOSTND,*) ' ESCAPE TREE - ',
     &               IDTREE(IBRN),ITCAN(IBRN)
            GO TO 35
         ENDIF
      ENDIF

C.... Calculate temporary rust index variable based on height at the
C.... end of the cycle.

      IF(HITE.GT.25.0) THEN
         RITEM=RINDX*0.1
      ELSE IF(HITE.GT.15.0) THEN
         RITEM=RINDX*(1.0-(0.09*(HITE-15.0)))
      ELSE
         RITEM=RINDX
      ENDIF
      IF(DEBUG) WRITE(JOSTND,*) 'RITEM=',RITEM,' RINDX=',RINDX

C.... Calculate number of expected cankers for current year
C.... (total and "potentially lethal") based on tree's target area.

      TNEWC=RITEM*SSTAR
      EXPC=TNEWC
      IF(DEBUG) WRITE(JOSTND,*) 'EXPC=',EXPC

C.... Calculate crown length in centimeters for this year and the
C.... probability of infection for the tree.

      CRLEN=(SSTHT*100.0)-BRHTBC(IBRN)
      PIEXP=EXP(-(TNEWC/(1+TNEWC*DFACT(I3,ISTOTY(IBRN)))))
      PLI=1.0-PIEXP
      NUMTIM=INT(TNEWC)+1
      IF(DEBUG) WRITE(JOSTND,*) 'PLI=',PLI,' NUMTIM=',NUMTIM

C.... Loop through for number of expected cankers.

      DO 30 J=1,NUMTIM

C....    If probability of infection >= random number then add canker.

         CALL BRANN(XBRAN)
         IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN

         IF(PLI.GE.XBRAN) THEN

C....       Add a canker to total number of cankers for this tree.
C....       Running total number of cankers is kept but only "lethal"
C....       ones are processed during cycling.

            ITCAN(IBRN)=ITCAN(IBRN)+1

C....       Generate up and out positions for added canker.

            CALL BRANN(XBRAN)
            TUP=(100*SSTHT-CRLEN)+CRLEN*XBRAN
            TOUT=(35*SQRT(SSTHT)*(100*SSTHT-TUP))/CRLEN

            IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN,
     &      ' TUP=',TUP,' TOUT=',TOUT

C....       If out < 50 then canker possibly lethal (different
C....       probability than if the canker is farther out than 50 cm).

            IF(TOUT.LT.50.0) THEN
               PLETH=0.97-0.0158*TOUT
            ELSE
               PLETH=35.4/TOUT**(1+(0.35*TOUT/50))
            ENDIF
            IF(PLETH.LT.0.0) PLETH=0.0

            IF(DEBUG) WRITE(JOSTND,*) 'PLETH=',PLETH

C....       Call random number generator. If PLETH >= number then create
C....       a bole canker, otherwise create a branch canker. If array is
C....       full then skip adding a canker altogether.

            CALL BRANN(XBRAN)
            IF(DEBUG) WRITE(JOSTND,*) 'XBRAN=',XBRAN

            IF(ILCAN(IBRN).LT.10) THEN
               ILCAN(IBRN)=ILCAN(IBRN)+1
               ICANB=ILCAN(IBRN)
               IF(PLETH.GE.XBRAN) THEN
                  DOUT(ICANB,IBRN)=0.0
               ELSE
                  DOUT(ICANB,IBRN)=TOUT
               ENDIF
               DUP(ICANB,IBRN)=TUP
               GIRDL(ICANB,IBRN)=0.0
               ISTCAN(ICANB,IBRN)=0
 
               IF(DEBUG) WRITE(JOSTND,*) 
     &         ' DOUT=',DOUT(ICANB,IBRN),' DUP=',DUP(ICANB,IBRN)
           ENDIF
         ENDIF
   30 CONTINUE
   35 CONTINUE

C.... Common return.
      IF(DEBUG) THEN
      WRITE (JOSTND,38) IBRN,HITE,RINDX,SSTAR,SSTHT,PROP,EXPC
   38    FORMAT(' IBRN=',I4,' HITE=',F5.1,' RINDX=',F7.4,
     &   ' SSTAR=',F7.1,' SSTHT=',F5.1,' PROP=',F4.1,' EXPC=',F3.1)
      ENDIF

      IF(DEBUG) WRITE(JOSTND,40) ICYC
   40 FORMAT('Leaving subroutine BRECAN: cycle = ',I2)
      RETURN
      END
