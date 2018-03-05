      SUBROUTINE RDTREG
      IMPLICIT NONE
C----------
C RD $Id: rdtreg.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  Purpose :
C     This root disease model subroutine is called from FVS
C     GRADD every cycle.  It calls the other agents model,
C     then calls the master root disease model.  Finally,
C     it calls various subroutines to produce output files.
C
C  Called By :
C     GRADD  [FVS]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [FVS]
C     RDLOAD  (SUBROUTINE)   [ROOT DISEASE]
C     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
C     RDOAGM  (SUBROUTINE)   [ROOT DISEASE]
C     RDTIM   (SUBROUTINE)   [ROOT DISEASE]
C     RDPUSH  (SUBROUTINE)   [ROOT DISEASE]
C     RDCNTL  (SUBROUTINE)   [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C     RDEND   (SUBROUTINE)   [ROOT DISEASE]
C     RDRDEL  (SUBROUTINE)   [ROOT DISEASE]
C     RDGROW  (SUBROUTINE)   [ROOT DISEASE]
C     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
C     ANWRITE (SUBROUTINE)   [ROOT DISEASE]
C
C  Revision History :
C    03-APR-97 Matt Thompson (FHTET)
C              Changed the code that tests to see if a cut occurred
C              so that if the disease area is 0 but there is a chance
C              of spore infection, Annosus can occur.
C    14-JAN-00 Lance David (FHTET)
C              Replaced the literals in option processor calls with
C              references to array MYACT. 
C    06-AUG-01 Lance R. David (FHTET)
C              Added initialization of array DIFF.
C    29-JUL-02 Lance R. David (FHTET)
C              Modified and added debug statements. Changed two return
C              statements to "GOTO 300" so routine has single point of exit.
C   09/04/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/16 Lance R. David (FMSC)
C     Moved check to exit routine if RD not in use. IF (IROOT .EQ. 0) GOTO 300
C-----------------------------------------------------------------------------
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDCRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.

      LOGICAL DEBUG
      INTEGER I, IACTK, IDI, ITR, J, KDT, MYACT(1), NPS, NTODO
      REAL    ANS, DIFF(ITOTRR), PRMS(2), TPAREA

      DATA MYACT / 2430 /

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDTREG',6,ICYC)
      
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDTREG'

      IF (IROOT .EQ. 0) GOTO 300
      DO 10 IDI=1,ITOTRR
         DIFF(IDI) = 0.0
   10 CONTINUE
   
      TPAREA = 0.0
      DO 850 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
  850 CONTINUE       
      
C.... Changed the following two lines so that the annosus model will be run if
C.... there is no area but a cut did occur (LSPFLG(1..3) = .TRUE.).
C.... mt 04-03-97

      IF ((TPAREA .EQ. 0) .AND. 
     &   (.NOT. LSPFLG(1) .AND.
     &    .NOT. LSPFLG(2) .AND.
     &    .NOT. LSPFLG(3))) GOTO 300
      
C.... Check for time to death and infection probability.

      IF (IY(ICYC) .GE. IRGEN(8)) IRHAB = 2
      IF (IY(ICYC) .NE. IRGEN(9)) GOTO 1000

      IDI = MAXRR
      DO 900 I = 1,MAXSP                             
         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(I))
         PNINF(IRTSPC(I),IDI) = SSSFAC(IRTSPC(I),IDI) *
     &                            PNINF(IRTSPC(I),IDI)
  900 CONTINUE

 1000 CONTINUE   
 
C.... Copy the borax option processing information into the next
C.... cycle so it will be there if there is a cut.
C     (Activity Code 2430)

      IF (LBORAX(1)) THEN
         IF (.NOT. LBORAX(2)) THEN
        
            IF (LBORAX(1)) CALL OPFIND (1,MYACT(1),NTODO)
        
            IF (NTODO .GT. 0) THEN
          
               CALL OPGET (1,2,KDT,IACTK,NPS,PRMS)

               BOTRT = PRMS(1)
               BODBH = PRMS(2)
          
               CALL OPDONE (1,IY(ICYC))
            ENDIF
      
         ELSE
            LBORAX(2) = .FALSE.
         ENDIF

      ENDIF
 
      IF (ITRN .LE. 0) GOTO 9999

C.... Calculate outside density. 
      
      DO 839 IDI=MINRR,MAXRR
         DIFF(IDI) = SAREA - PAREA(IDI)
         IF (DIFF(IDI) .GT. 0.0) GOTO 841
  839 CONTINUE    
      GOTO 844
      
  841 CONTINUE    
      IDI = MAXRR

      DO 843 I=1,ITRN                  
C
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
C
C     Exit loop if not a host species (RNH 28May98)
C
      IF (IDI .LE. 0) GO TO 843
C
         IF (DIFF(IDI) .GE. 1E-6) THEN 
            FPROB(I) = (PROB(I) * SAREA - PROBIU(I) - PROBIT(I)) /
     &                 DIFF(IDI)
            IF (FPROB(I) .LE. 1E-6) FPROB(I) = 0.0
         ELSE
            FPROB(I) = 0.0
         ENDIF

  843 CONTINUE

  844 CONTINUE

C.... Load PROB into PROBL (Density of all live trees in stand is PROB).

      CALL RDLOAD (PROBL,PROB,ITRN)

C.... Calculate root radii.

      DO 1001 ITR = 1,ITRN
         I = ISP(ITR)
         CALL RDROOT (I,DBH(ITR), ANS, PROOT(IRTSPC(I)),
     &      RSLOP(IRTSPC(I)), HT(ITR))
         ROOTL(ITR) = ANS
         IF (DEBUG) WRITE (JOSTND,775) ITR, PROB(ITR), PROBI(ITR,1,1),
     &              PROBIU(ITR), FPROB(ITR),ITR,HT(ITR),DBH(ITR),ANS
 775     FORMAT (' IN RDTREG: I PROB PROBI PROBIU FPROB',I5,4(1X,F8.2),
     &   ' ITR HT DBH ROOTL ', I4,3(1X,F7.3))
 1001 CONTINUE

C.... Call the other agents submodels.

      CALL RDOAGM

      CALL RDTIM

 9999 CONTINUE

C.... Call subroutine to push stumps.

      CALL RDPUSH
      CALL RDCNTL
      IF (ITRN .LE. 0) GOTO 10000

C.... Call subroutine to update stump list with natural,
C.... other agent, and root disease killed trees.
C....
C.... Root disease extensions simulated, redefine PROB
C.... after losses to other agents

      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)
      CALL RDEND

C.... New code added.  Call RDRDEL to delete tree records which
C.... have all prob values equal to zero, IE PROBI, PROBIU, PROBIT
C.... and FPROB.  RDRDEL called only if model initialized from treelist.

      IF (RRTINV) CALL RDRDEL  

C.... Call routine which reduces the growth factor due to root disease.
      
      CALL RDGROW

C.... Update growing space factor based on root disease patches.

10000 CONTINUE
      IF (INFLAG .EQ. 0) GOTO 260
      IF (INFLAG .GT. 1) GOTO 260
      INFLAG = 2

      DO 212 IDI=MINRR,MAXRR
         DO 210 I = 1,2
            DO 205 J = 1,5
               CRNSTO(IDI,I,J) = PROBD(IDI,I,J)
  205       CONTINUE
  210    CONTINUE
  212 CONTINUE

  260 CONTINUE
  
C.... Call subroutine to decay the stump root radius
C.... (decrease inoculum).

      CALL RDINOC (.FALSE.)

  300 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDTREG'
      RETURN
      END
