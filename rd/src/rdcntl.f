      SUBROUTINE RDCNTL
      IMPLICIT NONE
C----------
C  **RDCNTL      LAST REVISION:  08/27/14
C----------
C  MAIN ROOT DISEASE SUBROUTINE.  CONTROLS SPREAD OF DISEASE
C  PATCHES, CARRYOVER OF ROOT DISEASE BETWEEN ROTATIONS, INFECTS
C  NEW TREES IN STAND, SIMULATES INSIDE PATCH DYNAMICS, CALCULATES
C  MORTALITY DUE TO ROOT DISEASE.
C
C  CALLED BY :
C     RDTREG  [ROOT DISEASE]
C
C  CALLS     :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDINUP  (SUBROUTINE)   [ROOT DISEASE]
C     RDJUMP  (SUBROUTINE)   [ROOT DISEASE]
C     RDSPRD  (SUBROUTINE)   [ROOT DISEASE]
C     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
C     RDINF   (SUBROUTINE)   [ROOT DISEASE]
C     OPADD   (SUBROUTINE)   [PROGNOSIS]
C     OPINCR  (SUBROUTINE)   [PROGNOSIS]
C     OPFIND  (SUBROUTINE)   [PROGNOSIS]
C     OPDONE  (SUBROUTINE)   [PROGNOSIS]
C     RDCENT  (SUBROUTINE)   [ROOT DISEASE]
C     RDINSD  (SUBROUTINE)   [ROOT DISEASE]
C     RDIOUT  (SUBROUTINE)   [ROOT DISEASE]
C     RDMORT  (SUBROUTINE)   [ROOT DISEASE]
C     RDSOUT  (SUBROUTINE)   [ROOT DISEASE]
C     RDSPOR  (SUBROUTINE)   [ROOT DISEASE]
C     RDSHRK  (SUBROUTINE)   [ROOT DISEASE]
C     RDZERO  (SUBROUTINE)   [ROOT DISEASE]
C
C  Revision History :
C    04/03/97 - Matt Thompson (FHTET)
C      Modified the variables that are tested to check
C      for a stand entry before the spore model is called.
C    14-JAN-00 - Lance David (FHTET)
C      Removed literals from option processor calls and replaced
C      them with references to MYACT. Expanded array MYACT to 
C      include activity codes 2401 and 2431.
C    06-AUG-01 Lance R. David (FHTET)
C      Added declaration and initialization of TOTCEN.
C    01-JUL-02 Lance R. David (FHTET)
C      Corrected what looked like a typo with reset of PAREA near line 257.
C   08/27/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
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
      INCLUDE 'RDCRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'RDADD.F77'
C
C********************************************************************
C   NEW CODE ADDED FOR ROOT ROT MODEL (11/6/89). THIS CODE IS USED
C   SO THAT THE CARRYOVER MODEL CAN BE ADDED TO THE OPTIONS
C   PROCESSING LOGIC AND SIGNALED COMPLETED IN THE CYCLE WHEN
C   CARRYOVER IS DONE.
C*******************************************************************
C
      INTEGER  I, I1, I2, IACTK, ICEN, ICLASS, IDI, IDIFF, II, IJ,
     &         IP, IRG, ISIZE, IT, J, KDT, KSP, MYACT(3), NPS,
     &         NTODO
      REAL     DIFF, MRAT, PRMS(5), PTMOVE, RRADEC, RRSURV, SPCEN,
     &         TESTAG, TOTCEN
      LOGICAL  DEBUG

      DATA MYACT / 2401, 2402, 2431/

      KDT=IY(ICYC)
      TOTCEN = 0.0
C
C     SEE IF WE NEED TO DO SOME DEBUG.
C
      CALL DBCHK(DEBUG,'RDCNTL',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT(' IN RDCNTL CYCLE=',I4)

      IF (IROOT .EQ. 0) GOTO 1000

C     CALL THE SPORE OPTION PROCESSOR TO SEE IF THERE ARE ANY
C     CHANGES TO THOSE VARIABLES. THIS CALL WAS ALSO DONE IN RDSPUP
C     BECAUSE SOME SPORE VARIABLES ARE USED THERE (BUT THAT IS NOT
C     CALLED EVERY YEAR SO MAIN CALL IS HERE)
C     (ACTIVITY CODE 2431)

      CALL OPFIND (1,MYACT(3),NTODO)
      
      IF (NTODO .GT. 0) THEN
        
        DO 23 IJ = 1,NTODO
          CALL OPGET (IJ,5,KDT,IACTK,NPS,PRMS)
          
          IRRSP = PRMS(5)
          SPINF(IRRSP) = PRMS(1)
          SPDBH(IRRSP) = PRMS(2) 
          SPYTK(IRRSP) = PRMS(3)
          SPTRAN(IRRSP) = PRMS(4)
          
          CALL OPDONE (IJ,IY(ICYC))
   23   CONTINUE
      ENDIF


C
C        INFECT UNINFECTED TREES INSIDE INFECTION ZONES
C        ----------------------------------------------
C
      DO 525 IRRSP=MINRR,MAXRR

C        BEFORE DOING ANYTHING ELSE, CHANGE SPORE-CREATED CENTERS THAT WERE
C        MADE MORE THAN 10 YEARS AGO INTO ORDINARY CENTERS.
         
         SPCEN = 0.0
         DO 27 ICEN= 1,NCENTS(IRRSP)
           IF ((ICENSP(IRRSP,ICEN) .GT. 0) .AND. 
     &                       (JCENSP(IRRSP,ICEN) .LT. IYEAR)) THEN
             IDIFF = IYEAR - JCENSP(IRRSP,ICEN) 
             IF (IDIFF .GE. 10) ICENSP(IRRSP,ICEN) = 0
           ENDIF
           IF (ICENSP(IRRSP,ICEN) .NE. 0) SPCEN = SPCEN +
     &                                  PCENTS(IRRSP,ICEN,3) ** 2
           TOTCEN = TOTCEN + PCENTS(IRRSP,ICEN,3) ** 2
   27    CONTINUE
         SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

         IF (DEBUG) WRITE(JOSTND,315) IRRSP, PAREA(IRRSP)
  315    FORMAT(' RDCNTL BEFORE CALL TO RDINSD: IRRSP PAREA=',I2,E15.7)

         CALL RDINUP
         IF (IRIPTY .NE. 0) CALL RDINSD

  525 CONTINUE
      CALL RDIOUT

C.... If there has been a recent stand entry than move spore infected
C.... stumps to the regular stump arrays if necessary.

      IF (LSPFLG(1) .OR. LSPFLG(2) .OR. LSPFLG(3)) THEN
         CALL RDSPOR
      ENDIF

C     FIND OUT IF THE SPREAD RATE PARAMETERS ARE GOING TO 
C     CHANGE IN THIS TIMESTEP (ANY SPREAD KEYWORD DESIGNATED 
C     PARAMETER COULD CHANGE, INCLUDING TYPE OF SPREAD CALC.)
C     (ACTIVITY CODE 2401)

      CALL OPFIND (1,MYACT(1),NTODO)
      IF (NTODO .GT. 0) THEN  
          
         CALL OPGET (NTODO,4,KDT,IACTK,NPS,PRMS)
         IRSPTY = INT(PRMS(1))
         RRRSET(MINRR) = PRMS(2)
         RRRSET(MAXRR) = PRMS(3)
         NMONT = INT(PRMS(4))
          
         CALL OPDONE (NTODO,IY(ICYC))
      ENDIF

      DO 75 IRRSP=MINRR,MAXRR 
      
         CALL RDINUP
         CALL RDJUMP
C
C        SPREADING OF INFECTION CENTERS
C        ------------------------------
C
C        ESTIMATE SPREADING (AND SHRINKING) RATE OF INFECTION ZONES
C                                                                   
         IF (LONECT(IRRSP) .EQ. 1) THEN
            RRRATE(IRRSP) = 0.0
            GOTO 75
         ENDIF

         IF (IRSPTY .GE. 1) GOTO 50
         RRRATE(IRRSP) = RRRSET(IRRSP)
         GOTO 75

   50    CONTINUE      
   
         CALL RDSHRK
         CALL RDSPRD

   75 CONTINUE 

C     PRINT THE CALCULATED SPREAD RATES 

      IF (IRSPTY .GE. 1) CALL RDSOUT

      IF (DEBUG) WRITE(JOSTND,333) INFLAG,RRRATE(1),RRRATE(2),
     &                                    RRRATE(3),RRRATE(4)
  333 FORMAT(' RDCNTL: INFLAG RRRATE=',I3,4F9.2)

      MRAT = RRRATE(1)
      IF (RRRATE(2) .LT. MRAT) GOTO 7296
         MRAT = RRRATE(2)
 7296 CONTINUE
      IF (((INFLAG.NE.1).AND.(INFLAG.NE.3))
     >    .OR.(MRAT .LT. 0.0)) GOTO 1003
      AGECUR=3000.0
      INFLAG=0

 1003 CONTINUE
C
C     'GROW' (OR SHRINK) SPREADING CENTERS
C
      DO 300 IRRSP=MINRR,MAXRR

         SPPROP(IRRSP) = 0.0
         IF (NCENTS(IRRSP) .EQ. 0) GOTO 201

         SPCEN = 0.0
         TOTCEN =0.0

         DO 200  I=1, NCENTS(IRRSP)
            IF (PCENTS(IRRSP,I,3) .LE. 0) GOTO 200
            
            IF (IRSPTY .EQ. 1) THEN
               PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) + 
     &                                   RRATES(IRRSP,I) * FINT
            ELSE
               PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) + 
     &                                   RRRATE(IRRSP) * FINT
            ENDIF
            
            IF (ICENSP(IRRSP,I) .NE. 0) SPCEN = SPCEN +
     &                                  PCENTS(IRRSP,I,3) ** 2
            TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2

  200    CONTINUE
         SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

  201    CONTINUE

C        TOTALLY REMOVE ANY CENTERS WHOSE RADIUS IS LESS THAN 0.01 (.1 INCHES)
         CALL RDZERO
C
C        CALCULATE NEW AREA INFECTED BY OVERLAPPING(?) CENTERS
C
         IF (DEBUG) WRITE(JOSTND,811) IRRSP, PAREA(IRRSP)
  811    FORMAT(' RDCNTL BEFORE CALL TO RDAREA TYPE=', I1,
     &          ' PAREA=',E15.7)
         CALL RDAREA
         IF (DEBUG) WRITE(JOSTND,812) IRRSP, PAREA(IRRSP)
  812    FORMAT(' RDCNTL AFTER CALL TO RDAREA TYPE=', I1, ' PAREA=',
     &          E15.7)

         AREANU(IRRSP) = PAREA(IRRSP) - OOAREA(IRRSP)

C        looks like this may be a typo error LRD 01-jul-02
C        IF (PAREA(IRRSP) .LE. 0.0) AREANU(IRRSP) = 0.0
         IF (PAREA(IRRSP) .LE. 0.0) PAREA(IRRSP) = 0.0
         IF (AREANU(IRRSP) .LE. 0.0) AREANU(IRRSP) = 0.0
         OOAREA(IRRSP) = PAREA(IRRSP)
C
C        ESTIMATE NUMBER OF NEW TREES INFECTED
C
         CALL RDINF

  300 CONTINUE
C -------------------------------------------------------------------
C     NEW INFECTION CENTERS (FROM CARRYOVER MODEL)
C     ---------------------
C
      IF (DEBUG) WRITE(JOSTND,334) INFLAG
  334 FORMAT(' RDCNTL : INFLAG=',I4)

      IF (INFLAG .EQ. 0) GOTO 500
      TESTAG = FLOAT(IY(ICYC))
      IF (DEBUG) WRITE(JOSTND,335) TESTAG,AGECUR
  335 FORMAT(' RDCNTL : TESTAG AGECUR=',2F9.2)

      IF (AGECUR .NE. TESTAG) GOTO 500
      IF (DEBUG) WRITE(JOSTND,336) RRRATE(1), RRRATE(2) ,RRGEN(1,7)
  336 FORMAT(' RDCNTL : RRRATE 1&2  RRGEN(1,7) =',3F9.2)

      IF (MRAT .LT. RRGEN(1,7)) GOTO 500
C
C     ESTIMATE PROBABILITY OF INITIATION OF NEW INFECTION CENTER
C     IF IFRRC EQ 1 DYNAMIC CARRYOVER MODEL IS USED. IF IFRRC
C     EQ 0 STATIC CARRYOVER MODEL IS USED. LOOP 380 IS THE
C     STATIC CARRYOVER MODEL !!!
C
      IF (IFRRC.EQ.1) GOTO 390

C
C     SET PATRAMETERS FOR STATIC CARRYOVER MODEL AND INCORPORATE STATIC
C     MODEL INTO THE OPTION PROCESSOR.
C     (ACTIVITY CODE 2402)
C
      PRMS(1) = PINSET
      PRMS(2) = FLOAT(NNCENT)
      PRMS(3) = FLOAT(IRGEN(1))

      CALL OPADD (KDT,MYACT(2),KDT,3,PRMS,I)

  379 CONTINUE
      DO 385 IDI=MINRR,MAXRR
         DO 380 I=1, 2
            DO 370 J=1, 5
               PROBIN(IDI,I,J) = PINSET
  370       CONTINUE
  380    CONTINUE
  385 CONTINUE
      GOTO 400

  390 CONTINUE
C
C     DYNAMIC CARRYOVER MODEL. ADD TO THE OPTION PROCESSOR BY CALLING
C     OPADD.
C     (ACTIVITY CODE 2402)
C
      PRMS(1) = RRGEN(1,7)
      PRMS(2) = FLOAT(IRGEN(1))

      CALL OPADD (KDT,MYACT(2),KDT,2,PRMS,II)

  391 CONTINUE
C
C **************** CD CHANGE *** remove dynamic carryover
C
C     CALL RRCARY
C
C **************** CD END *************
C

  400 CONTINUE
C
C     SET UP NEW INFECTION CENTERS
C
      DO 411 IRRSP=MINRR,MAXRR
         CALL RDCENT(PROBIN,PCENTS,NCENTS,CRNSTO,SAREA,
     &               IRFLAG,NNCENT,PISIZE,PAREA,IRRSP)

         IF (DEBUG) WRITE(JOSTND,813) IRRSP, NCENTS(IRRSP), NNCENT
  813    FORMAT(' RDCNTL AFTER CALL TO RDCENT: IRRSP NCENTS NNCENT',
     &          3I5)

         CALL RDAREA
         IF (DEBUG) WRITE(JOSTND,814) IRRSP, PAREA
  814    FORMAT(' RDCNTL AFTER CALL TO RDAREA: IRRSP, PAREA=',I2,
     &          E15.7)
  411 CONTINUE

C
C     SEE IF WE ARE GOING TO RESCHEDULE CARRYOVER BECAUSE OF A
C     NESTED CUT.  (INFLAG=3).
C
      IF (INFLAG .NE. 3) GOTO 444
      INFLAG = 1
      IRG = ICYC + IRGEN(1)
      AGECUR = 3000.0
      IF (IRG .LE. NCYC) AGECUR = IY(IRG)
      GOTO 447

  444 CONTINUE
      AGECUR = 3000.0
      INFLAG = 0

  447 CONTINUE

      DO 482 IRRSP=MINRR,MAXRR
         RRADEC = OOAREA(IRRSP) - PAREA(IRRSP)
         IF (RRADEC .LE. 0.0) GOTO 8865

C
C        ADJUST TREE LISTS FOR CHANGE IN DISEASE AREA
C
         IF (ITRN .EQ. 0) GOTO 482
         IF (DEBUG) WRITE(JOSTND,69) IRRSP, RRADEC
   69    FORMAT(' RDCNTL : IRRSP  CARRYOVER AREA REDUCTION ',I2, E16.8)

         IDI = IRRSP 
         DO 420 I=1, ITRN
            IF (IRRSP .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
            IF (IDI .NE. IRRSP) GOTO 420

            PTMOVE = PROBIU(I) * RRADEC / (OOAREA(IRRSP) + 1.0E-6)
            PROBIU(I) = PROBIU(I) - PTMOVE
            IF (DEBUG) WRITE(JOSTND,421) PROBIU(I)
  421       FORMAT(' IN RDCNTL : PROBIU',F10.2)
            DIFF = SAREA - PAREA(IRRSP)
            IF (DIFF .LE. 1E-3) FPROB(I) = 0.0
            IF (DIFF .LE. 1E-3) GOTO 420
            FPROB(I) = (FPROB(I) * (SAREA - OOAREA(IRRSP)) + PTMOVE) /
     &                 (SAREA - PAREA(IRRSP) + 1.0E-6)
  420    CONTINUE

C
C        REDUCE STUMP LIST WITH CHANGE IN DISEASE AREA
C
         DO 463 IDI=MINRR,MAXRR
            DO 462 ITYPE=1,2
               DO 461 ISIZE=1,5
                  DO 460 IT = 1,40
                     PROBDA(IDI,ITYPE,ISIZE,IT) =
     &                     PROBDA(IDI,ITYPE,ISIZE,IT) * PAREA(IDI) /
     &                     (OOAREA(IDI) +1.0E-6)
  460             CONTINUE
  461          CONTINUE
  462       CONTINUE
  463    CONTINUE
         GO TO 6688

 8865    CONTINUE
         CALL RDINF

 6688    CONTINUE
         OOAREA(IRRSP) = PAREA(IRRSP)

  482 CONTINUE                 
  
C         **END CARRYOVER MODEL
C -----------------------------------------------------------------------
  
  500 CONTINUE

C
C        INFECT UNINFECTED TREES INSIDE INFECTION ZONES
C        ----------------------------------------------
C
C      DO 525 IRRSP=MINRR,MAXRR
C         IF (DEBUG) WRITE(JOSTND,315) IRRSP, PAREA(IRRSP)
C  315    FORMAT(' RDCNTL BEFORE CALL TO RDINSD: IRRSP PAREA=',I2,E15.7)
C         IF (IRIPTY .NE. 0) CALL RDINSD
C  525 CONTINUE
C
C
C     INFECTION AND DEATH OF LIVE TREES
C     ---------------------------------
      CALL RDMORT

C
C     ACCUMULATE SOME INDICATORS
C     --------------------------
C
      DO 550 J=1,(ITOTRR+MAXSP)
         PRINF(J) = 0
         PRPTOT(J) = 0.0
  550 CONTINUE

C
C     ACCUMULATE STUMPS BY CATEGORY AND TOTALS
C
C     ACCUMULATE LIVE TREES IN INFECTED AREA BY CATEGORY AND TOTALS
C
      IF (ITRN .LE. 0) GOTO 1000
      DO 700 I=1, ITRN
         KSP = ISP(I) + 2
         RRSURV = PROBIT(I) + PROBIU(I)
         IF (RRSURV .LE. 0) GOTO 700
         ICLASS = 1

         DO 660  J=1, 3
            ICLASS = ICLASS + 1
            IF (DBH(I) .LE. DICLAS(J+1)) GOTO 670
  660    CONTINUE

  670    CONTINUE
  700 CONTINUE

      IDI = MAXRR
      DO 730 J=1, 12
         DO 720 I=1, 4
            IF (J .GT. 2) GOTO 705
               IF (MAXRR .LT. 3) IDI = J
               GOTO 710
  705       CONTINUE
               IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(J-2))
  710       CONTINUE
  720    CONTINUE
  730 CONTINUE

  740 CONTINUE
C
C     CALCULATE WEIGHTED AVERAGE PROPORTION OF INFECTED ROOTS
C        (PRINF & PRPTOT ARE ZEROED ABOVE)
C
      IDI = MAXRR
      DO 800 KSP=1,MAXSP
      IF (MAXRR .LT. 3) IDI=IDITYP(IRTSPC(KSP))
C
C     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
C     If non-host species then IDI = 0, and loop should be skipped to
C     prevent array out of bounds error
C
      IF (IDI .LE. 0) GO TO 800 
C
      IF (ISCT(KSP,1) .EQ. 0) GOTO 800
         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)

         DO 750 J=I1, I2
           I = IND1(J)

           DO 745  IT=1, ISTEP 
             DO 742 IP=1,2 
               IF (PROPI(I,IT,IP) .GT. 0.0) THEN
                  PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) + PROBI(I,IT,IP) 
     &                                        * PROPI(I,IT,IP)
                  PRINF(IDI) = PRINF(IDI) + PROBI(I,IT,IP) * 
     &                                        PROPI(I,IT,IP)
               ENDIF
               PRPTOT(KSP+ITOTRR) = PRPTOT(KSP+ITOTRR) + PROBI(I,IT,IP)
               PRPTOT(IDI) = PRPTOT(IDI) + PROBI(I,IT,IP)
  742        CONTINUE
  745      CONTINUE
  750    CONTINUE

         PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) / 
     &                            (PRPTOT(KSP+ITOTRR) + 1E-6)
  800 CONTINUE

      DO 805 IDI=MINRR,MAXRR
         PRINF(IDI) = PRINF(IDI) / (PRPTOT(IDI) + 1E-6)
  805 CONTINUE
  
 1000 CONTINUE
      IF (DEBUG) WRITE (JOSTND,70) INFLAG, TESTAG, AGECUR
   70 FORMAT (' LEAVING RDCNTL : INFLAG, TESTAG, AGECUR = ',I5,2X,2F7.0)

      RETURN
      END
