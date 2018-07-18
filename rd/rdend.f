      SUBROUTINE RDEND
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     Reconciles mortality estimates from inside and outside the
C     root disease areas.
C
C  Called By :
C     RDTREG  [ROOT DISEASE]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
C     RDSTP   (SUBROUTINE)   [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C
C  Local Variables :
C     <incomplete>
C
C  Common Block Variables Used :
C     <incomplete>
C
C  Revision History :
C  06/12/96 - Matthew K. Thompson
C           Moved the declaration of DSO, DSII, and DSIU to the
C           parameter include file RDPARM.
C  06/15/2006 - Lance R. David
C           WK2 was being set to 99.9% when it potentially could have been
C           set to 100% by another model.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C----------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations

      LOGICAL DEBUG
      INTEGER DRR, DBB, DWND, DNAT, I, IDI, IP, ISL, J, JJ
      REAL    BACKGD, DIE, DIENAT, DIFF, NATIU, PBB, PMORT, TDIEN,
     &        TDIEOU, TDIUN, TEST, TPAREA, UNAPP, WMESS

      DATA DRR      /1/
      DATA DBB      /2/
      DATA DWND     /3/
      DATA DNAT     /4/
C
C     Special debug ***
C
C     CALL DBCHK (DEBUG,'RDDBUG',6,ICYC)
C     IF (DEBUG) CALL RDDBUG(1)

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDEND',5,ICYC)  
      
C.... Zero the dead tree arrays.

      DO 27 I=1,ITRN
         DO 23 J=1,4
            DPROB(I,J,1) = 0.0      
            DPROB(I,J,2) = 0.0      
   23    CONTINUE
   27 CONTINUE
   
      TPAREA = 0.0

      DO 76 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
   76 CONTINUE      

      IF (ITRN .LE. 0 .OR. TPAREA .EQ. 0.0) RETURN

      IDI = MAXRR
      DO 1000 I=1,ITRN

C        Calculate total number of trees dying inside infected area.

         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
C
C     Skip operation if non-host tree (RNH May98)
C
      IF (IDI .LE. 0) GO TO 1000
C
C....    Infected trees always have at least background mortality.
C....    PROBIT + RRKILL is the number of infected trees before death.

         BACKGD = WK2(I) / PROB(I)
         TDIEN = AMAX1(RRKILL(I), BACKGD*(PROBIT(I)+RRKILL(I)))
C        TDIUN = AMIN1(PROBIU(I) * 0.95, BACKGD*PROBIU(I))

C....    Uninfected trees may not have any mortality during the root
C....    disease model.

         TDIUN = AMIN1(PROBIU(I) * 0.95, OAKL(DSIU,I))

C....    Calculate density of trees dying outside infected area.
C....    Root disease model may not predict any mortality here.

C        PMORT = AMIN1(FPROB(I)*0.95, BACKGD*FPROB(I))
         
         DIFF = SAREA - PAREA(IDI)
         TEST = 0.0
         IF (DIFF .GT. 0.0) TEST = OAKL(DSO,I) / DIFF
         PMORT = AMIN1(FPROB(I) * 0.95, TEST)

         TDIEOU = PMORT * DIFF

C....    Calculate newest value of WK2 to pass to FVS.     

         WMESS = (TDIEOU + TDIEN + TDIUN) / SAREA
         WK2(I) = AMAX1(WMESS,WK2(I))

C....    This condition conflicts with FFE and any other extension 
C....    that can impose 100% mortality. LRD 06/16/2006
C        IF (WK2(I) .GT. PROB(I) * 0.9999) WK2(I) = PROB(I) * 0.999

         IF (WK2(I) .GT. PROB(I)) WK2(I) = PROB(I)
 
         IF (DEBUG) WRITE(JOSTND,888) I, WK2(I), PROB(I)
 888     FORMAT (' IN RDEND :  I WK2 PROB=',I4,2E15.8)

C....    Take weighted average over two kinds of roots.

         RROOTT(I) = (RROOTT(I) * WK22(I) + (ROOTL(I) * WK2(I))) /
     >               (WK22(I) + WK2(I) + .00001)
         WK22(I) = WK22(I) + WK2(I)

C....    Apply natural mortality to inside uninfected trees if not all 
C....    mortality could be accounted for by root disease predictions.
C....    give the outside trees the first chance at natural mortality
C....    and then give any of the remaining natural mortality to the
C....    inside uninfected trees. (this mortality should be weighted
C....    somehow because it is calculated by prognosis using the
C....    average density of the stand, not that inside and outside
C....    centers.)
C....    Otherwise just apply the mortality calculated from the root
C....    disease model.

         DIE = TDIUN           
         NATIU = 0.0
C        OUTNAT = 0.0
         UNAPP = 0.0

         IF (WMESS .LT. WK2(I)) THEN      

C....       figure out how much mortality has not been allocated yet
C....       (in #trees)

            UNAPP = (WK2(I) - WMESS) * SAREA

C....       some of it will be applied to trees on the outside       

C           OUNAT = BACKGD * FPROB(I) * DIFF - TDIEOU
            UNAPP = UNAPP - (BACKGD * FPROB(I) * DIFF - TDIEOU)

C....       figure out how much natural mortality could be applied to
C....       IU trees

            NATIU = BACKGD * PROBIU(I)

C....       but we will assign at most the remaining amount

            IF (NATIU .GT. UNAPP) NATIU = MAX(UNAPP,0.0)

C....       incorporate the natural mortality into the previously
C....       calculated mortality

            IF (NATIU .GT. TDIUN) DIE = NATIU
         ENDIF
         PROBIU(I) = PROBIU(I) - DIE

C....    Reduce outside tree density by mortality from root disease.

         IF (FPROB(I) - PMORT .GT. 1E-6) FPROB(I) = FPROB(I) - PMORT
         
C....    Add dead trees to the dead tree list for use by the main snag
C....    model.
C....
C....    TDIEOU is # trees outside dying 
C....    TDIUN  is # trees inside uninfected dying
C....    TDIEN  is # trees inside, infected dying
C....    BBKILL is bark beetle killed trees
C....    OAKL   is all other agent killed trees

C....    Outside trees: windthrow and bark beetles
          
         IF (OAKL(DSO,I) .GT. 0.0) THEN
            PBB = BBKILL(DSO,I) / OAKL(DSO,I)
            DPROB(I,DBB,DSO) = TDIEOU * PBB
            DPROB(I,DWND,DSO) = TDIEOU * (1 - PBB)
         ENDIF
          
C....    Inside, uninfected trees: windthrow, bark beetles, and natural
C....    (note that in DPROB, DSO is used to mean dead, uninfected)
          
         IF (OAKL(DSIU,I) .GT. 0.0) THEN
            PBB = BBKILL(DSIU,I) / OAKL(DSIU,I)
            DPROB(I,DBB,DSO) = TDIUN * PBB
            DPROB(I,DWND,DSO) = TDIUN * (1 - PBB)
            DPROB(I,DNAT,DSO) = DIE - TDIUN
         ENDIF
          
C....    Inside, infected trees: root disease, windthrow and bark
C....    beetles
C....    (note that all root disease model mortality to infected trees
C....    is always applied).
          
         DPROB(I,DBB,DSII) = BBKILL(DSII,I)
         DPROB(I,DWND,DSII) = OAKL(DSII,I) - BBKILL(DSII,I)
         DPROB(I,DRR,DSII) = RDKILL(I)

         IF (DPROB(I,DWND,DSII) .LT. 0.0) DPROB(I,DWND,DSII) = 0.0
          
C        Calculate natural mortality of infected trees.

         DIENAT = TDIEN - RRKILL(I)
         IF (DIENAT .LE. 1E-6) DIENAT = 0.0

         DO 900 J=1,ISTEP 
            DO 899 IP=1,2
               IF (PROBI(I,J,IP) .LE. 0.0) GOTO 900
               DIE = PROBI(I,J,IP) * DIENAT / PROBIT(I)
               PROBI(I,J,IP) = PROBI(I,J,IP) - DIE
               RRKILL(I) = RRKILL(I) + DIE 
               DPROB(I,DNAT,DSII) = DPROB(I,DNAT,DSII) + DIE
               IF (PROBI(I,J,IP) .LE. 1E-6) PROBI(I,J,IP) = 0.0
  899       CONTINUE
  900    CONTINUE

C....    Update the stump list with natural mortality of infected
C....    trees.

         IF (DIENAT .GT. 0.0) THEN
            JJ = ISP(I)
            CALL RDSSIZ(JJ,DBH(I),STCUT,ISL,ISPS,IRTSPC)
            CALL RDSTP (ISL,JJ,DIENAT,DBH(I),ROOTL(I))
         ENDIF                                            
         
 1000 CONTINUE
      
C.... Re-calculate PROBIT

      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)
      
      RETURN
      END
