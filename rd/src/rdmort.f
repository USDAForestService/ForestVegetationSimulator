      SUBROUTINE RDMORT
      IMPLICIT NONE
C----------
C  **RDMORT      LAST REVISION:  08/29/14
C----------
C
C  THIS SUBROUTINE CALCULATES THE INFECTION OF THE ROOT SYSTEMS OF
C  LIVE TREES THROUGH TIME AND ESTIMATES WHEN THEY DIE DEPENDING
C  ON THE PROPORTION INFECTED AND THE SPECIES.
C
C  CALLED BY :
C     RDCNTL  [ROOT DISEASE]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDSLP   (FUNCTION)     [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C
C
C  Revision History:
C    17-JUN-2002  Lance R. David
C      Previous revision date note was "MARCH 7, 1995".
C      Added debug code.
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'

      LOGICAL  DEBUG
      INTEGER  I, I1, I2, IDI, IP, ISL, IT, J, KSP
      REAL     HABSP, TAREA, RDSLP

C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK (DEBUG,'RDMORT',6,ICYC)

      IF (DEBUG) THEN
        WRITE(JOSTND,*) 'ENTER RDMORT: ICYC=',ICYC,' ISTEP=',ISTEP
        WRITE(JOSTND,*)
     &  '             I,PROBI(I,1,1),PROBIU(I),PROBIT(I),FPROB(I)'
        DO 333 I=1,ITRN
           WRITE(JOSTND,618)
     &     I,PROBI(I,1,1),PROBIU(I),PROBIT(I),FPROB(I)
  618      FORMAT(' IN RDMORT: BEF',I4,4F8.2)
  333   CONTINUE
      ENDIF
C
C     ZERO OUT ACCUMULATORS.
C
      DO 100 KSP=1,MAXSP
         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)
         IF (ISCT(KSP,1) .EQ. 0) GOTO 100

         DO 150 J=I1,I2
            I = IND1(J)
            RDKILL(I) = 0.0
  150    CONTINUE
  100 CONTINUE
C
C     LOOP OVER TREE LIST IF THERE ARE ANY INFECTION CENTERS.
C
      TAREA = 0.0
      DO 410 IDI=MINRR,MAXRR
         TAREA = TAREA + PAREA(IDI) 
         RRGEN(IDI,3) = 0.0
  410 CONTINUE
      IF (TAREA .LE. 0.0) GOTO 600
      IF (ITRN .EQ. 0) RETURN

      IDI = MAXRR
      DO 500 KSP=1, MAXSP
         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 500
C
         HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)
C
C        MODIFY THE TIME TO DEATH MULTIPLIER (HABSP) BASED ON THE
C        PROPORTION OF CENTERS THAT ARE SPORE INITIATED (SPPROP).
C
         HABSP = HABSP * ((SPPROP(IDI) * SPYTK(IDI)) + 
     &                        (1 - SPPROP(IDI)))
         IF (ISCT(KSP,1) .EQ. 0) GOTO 500

         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         DO 400 J=I1, I2
           I = IND1(J)

           DO 350 IT=1, ISTEP 
             DO 300 IP=1,2
             
               IF (PROBI(I,IT,IP) .LE. 0) GOTO 300

               YTKILL = RDSLP(DBH(I),XXINF,YYINF,NNINF)
               IF (YTKILL .LE. XMINKL(IDI)) GOTO 250

               YTKILL = (YTKILL - XMINKL(IDI)) *
     &                  HABSP *
     &                  RRPSWT(IRTSPC(KSP)) + XMINKL(IDI)

  250          CONTINUE
               CURAGE = PROPI(I,IT,IP) * YTKILL/PKILLS(IRTSPC(KSP),IDI)
               PROPI(I,IT,IP) = (CURAGE + FINT) *
     &                       PKILLS(IRTSPC(KSP),IDI) / YTKILL
               IF (DEBUG) WRITE (JOSTND,*)
     &            'IN RDMORT: I, DBH, CURAGE, PROPI: ',
     &            I, DBH(I), CURAGE, PROPI(I,IT,IP)
               IF (PROPI(I,IT,IP) .LT. PKILLS(IRTSPC(KSP),IDI)) GOTO 300

               RRKILL(I) = RRKILL(I) + PROBI(I,IT,IP)
               RDKILL(I) = RDKILL(I) + PROBI(I,IT,IP)
               RRGEN(IDI,3) = RRGEN(IDI,3) + PROBI(I,IT,IP) /
     &                        (PAREA(IDI) + 1E-9)

               IF (DEBUG) WRITE(JOSTND,260)
     &             I, IT, CURAGE, DBH(I), PROB(I), PROBI(I,IT,IP)
  260          FORMAT(' IN RDMORT:  I, IT, CURAGE, DBH, PROB, PROBI ',
     &            I4,I4,4F10.3)

               PROBI(I,IT,IP) = 0.0
  300        CONTINUE               
  350      CONTINUE

C        UPDATE THE STUMP LIST WITH DEAD INFECTED TREES

         IF (RDKILL(I) .GT. 0.0) THEN
           CALL RDSSIZ(KSP,DBH(I),STCUT,ISL,ISPS,IRTSPC)
           CALL RDSTP (ISL,KSP,RDKILL(I),DBH(I),ROOTL(I))
         ENDIF                                            

  400    CONTINUE    
  500 CONTINUE

  600 CONTINUE 
  
C     RECALCULATE PROBIT (SUM OF PROBI'S)

      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)
  
      IF (DEBUG) THEN
        WRITE (JOSTND,*)
     &  '             I,DBH(I),PROBIU(I),PROBIT(I),FPROB(I)'
        DO 444 I=1,ITRN
           WRITE (JOSTND,619) I,DBH(I),PROBIU(I),PROBIT(I),FPROB(I)
  619      FORMAT(' IN RDMORT: AFT',I4,4F8.2)
  444   CONTINUE
      ENDIF

      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDMORT:'
      RETURN
      END
