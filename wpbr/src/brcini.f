      SUBROUTINE BRCINI(IBRNO,HITE)
      IMPLICIT NONE
C**********************************************************************
C  **BRCINI       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRCINI uses the canker counts read from the canker data list
C  (if the CANKDATA keyword is used - there is a required record that
C  contains the canker count for the tree and has 0's for up, out, and
C  girdle) to initialize canker and infection conditions for each tree.
C  This routine randomly generates up, out, and %girdle measurements
C  for any number of cankers up to 10 or up to the canker count,
C  whichever is less, minus the number of cankers already loaded from
C  the actual canker data.
C----------------------------------------------------------------------
C  Parameters
C     IBRNO  - index for current tree
C     HITE   - current tree height (in meters)
C----------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.
      INTEGER IBRNO, ITEMPC, J, NUMCNK
      REAL    HITE, HITEBC, PBOLE, TOUT, TUP, XRAN, YRAN, ZRAN, CRLEN
      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCINI',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,23) ICYC
  23  FORMAT('Entering subroutine BRCINI: cycle = ',I2)

C.... Calculate crown length in centimeters.
C.... Height to base of crown is already in centimeters but HITE
C.... is in meters.

      HITEBC=BRHTBC(IBRNO)
      CRLEN=(HITE*100.0)-HITEBC

C.... Determine the number of cankers to be generated.  Take the total
C.... count, which has been stored already in ITCAN, subtract the lethal
C.... count, stored already in ILCAN (in subroutine BRCANK) and then
C.... add that many cankers, up to 10 total.  Note: ITCAN already has
C.... the correct total canker count; we may add cankers to ILCAN if
C.... it's not already full and if ITCAN is greater that ILCAN.

      NUMCNK=ITCAN(IBRNO)-ILCAN(IBRNO)

C.... Loop through for number of cankers, if any, and stop at 10.

      IF(NUMCNK.GT.0) THEN
         DO 300 J=1,NUMCNK

C....       Check to see if the lethal canker array is already full.

            IF(ILCAN(IBRNO).LT.10) THEN
               ILCAN(IBRNO)=ILCAN(IBRNO)+1
               ITEMPC=ILCAN(IBRNO)

C....          Generate up and out positions for added canker.
C....          Up position can be anywhere on the tree, including
C....          below the base on the crown, whereas out will only be
C....          generated for cankers above the base of the crown.

               CALL BRANN(XRAN)
               TUP=100*HITE*XRAN
               IF(TUP.LT.HITEBC) THEN
                  TOUT=0.0
               ELSE
                  TOUT=(35*SQRT(HITE)*(100*HITE-TUP)/CRLEN)*XRAN
               ENDIF

C....          If out < 50 then the canker is possibly lethal--different
C....          probability than if the canker is farther out than 50 cm.
C....          This stuff came from BRECAN.

               IF(TOUT.LT.50.0) THEN
                  PBOLE=0.97-0.0158*TOUT
               ELSE
                  PBOLE=35.4/TOUT**(1+(0.35*TOUT/50))
               ENDIF
               IF(PBOLE.LT.0.0) PBOLE=0.0

C....          Call random number generator. If PBOLE >= number then
C....          create a bole canker, otherwise create a branch canker.

               CALL BRANN(YRAN)
               IF(PBOLE.GE.YRAN) THEN

C....             Bole canker.

                  DOUT(ITEMPC,IBRNO)=0.0

C....             Call random number generator again and generate
C....             a % girdle measurement.  Don't want to kill trees
C....             in the first cycle so make sure girdling is never
C....             more than 50%.

                  CALL BRANN(ZRAN)
                  GIRDL(ITEMPC,IBRNO)=ZRAN*50.0
               ELSE

C....             Branch canker.

                  DOUT(ITEMPC,IBRNO)=TOUT
                  GIRDL(ITEMPC,IBRNO)=0.0
               ENDIF

C....          Set distance up and canker status.  Canker status will
C....          be reset in routine BRCSTA.

               DUP(ITEMPC,IBRNO)=TUP
               ISTCAN(ITEMPC,IBRNO)=0
            ENDIF
  300    CONTINUE
      ENDIF

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,330) ICYC
  330 FORMAT('Leaving subroutine BRCINI: cycle = ',I2)
      RETURN
      END
