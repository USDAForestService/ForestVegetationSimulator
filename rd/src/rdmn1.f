      SUBROUTINE RDMN1(INUM)
C----------
C  **RDMN1       LAST REVISION:  03/25/03
C----------
C
C  THIS SUBROUTINE IS CALLED FROM PROGNOSIS MAIN AND INITIALIZES THE
C  GENERAL ROOT DISEASE MODEL.
C
C  CALLED BY :
C     MAIN    [PROGNOSIS]
C
C  CALLS     :
C     RDATV   (SUBROUTINE)   [ROOT DISEASE]
C     RDRANI  (SUBROUTINE)   [ROOT DISEASE]
C     COMPRS  (SUBROUTINE)   [PROGNOSIS]
C     OPADD   (SUBROUTINE)   [PROGNOSIS]
C     OPINCR  (SUBROUTINE)   [PROGNOSIS]
C     OPFIND  (SUBROUTINE)   [PROGNOSIS]
C     OPDONE  (SUBROUTINE)   [PROGNOSIS]
C     SPESRT  (SUBROUTINE)   [PROGNOSIS]
C     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
C     PCTILE  (SUBROUTINE)   [PROGNOSIS]
C     DIST    (SUBROUTINE)   [PROGNOSIS]
C     COMP    (SUBROUTINE)   [PROGNOSIS]
C     RDSETP  (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     INUM   -
C
C  Revision History:
C     21-MAR-00 Lance David (FHTET)
c       Removed equivalence of FVS array WK6 to RD array SPCNT and
C       corrected the initialization process of SPCNT based on
C       parameter MAXSP instead of literal 11.
C       Reduced RETURN statements to 1 at the end of routine.
C       Added debug code.
C     25-MAR-2003 Lance R. David (FHTET)
C       WK2 from ARRAYS common area is reset to zero before compression of
C       treelist is requested. SVS processing had used WK2 and left residual
C       values in it and comprs expects values in WK2 to be mortality which
C       has not yet been predicted.
C       Gary Dixon and Lance David discussed this problem and agreed
C       on this solution to reset WK2 in this routine.
C..................................................................
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'OUTCOM.F77'
C
      LOGICAL LGO,LTEE,DEBUG
      INTEGER I, II, MYACT(1)
      REAL    SPCNT(MAXSP,3), PRMS(2)

CLRD      EQUIVALENCE (WK6,SPCNT)

      DATA MYACT /250/

C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDMN1',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDMN1:  INUM=',INUM

C     CHECK TO SEE IF GENERAL ROOT DISEASE MODEL IS ACTIVE

      CALL RDATV (LGO,LTEE)   

C     IF THIS IS FIRST CALL TO RDMN1 THEN PRINT SUMMARY ROOT DISEASE INFO.
C     READ IN FROM TREE LIST

      IF (LGO .AND. INUM .EQ. 1) CALL RDTOUT
C
C     RETURN IF GENERAL ROOT DISEASE MODEL NOT ACTIVE OR THERE IS NO
C     ROOT DISEASE.
C
      TPAREA = 0.0
      DO 33 IDI=1,ITOTRR
         TPAREA = TPAREA + PAREA(IDI)
   33 CONTINUE     
      IF (.NOT. LGO .OR. TPAREA .EQ. 0.0) GOTO 225      !RETURN
C
C     IF THIS IS FIRST CALL TO RDMN1 THEN SEED RANDOM NUMBER GENERATOR.
C
      IF (INUM .EQ. 1) CALL RDRANI(DSEED)

C
C     IF TREELIST INITIALIZATION IN EFFECT AND THIS IS FIRST PASS
C     THRU RDMN1 THEN CALL RDSETP TO CALCULATE INITIALIZATION VALUES.
C     IF TREELIST INITIALIZATION IN EFFECT AND THIS IS SECOND CALL TO
C     RDMN1 THEN COMPRESS IF NUMBER OF TREES GREATER THAN VALUE
C     SPECIFIED WITH RRCOMP KEYWORD. IF NO COMPRESS TO BE DONE THEN
C     RETURN
C
      IF (LTEE) THEN
         IF (INUM .EQ. 1) THEN
            GOTO 222
         ELSEIF (INUM .EQ. 2 .AND. ITRN .GT. IRCOMP) THEN
            GOTO 90
         ELSE
            GOTO 225
         ENDIF
      ENDIF

C
C     IF MANUAL INITIALIZATION AND THIS IS FIRST PASS THRU RDMN1 THEN
C     RETURN.  IF MANUAL INIT AND THIS IS SECOND PASS THRU RDMN1 AND
C     COMPRESSION DESIRED THEN COMPRESS.  NOTE THAT COMPRESSION IS
C     CALLED IF THERE ARE MORE THAN IRRTRE TREE RECORDS.  IN THIS
C     SITUATION ONLY PROGNOSIS TREE VARIABLES ARE COMPRESSED AND NOT
C     ROOT DISEASE VARIABLES.  OTHERWISE ROOT DISEASE COMPRESS
C     WOULD REACH OUTSIDE ROOT DISEASE ARRAY BOUNDARIES.  DEACTIVATION
C     OF ROOT DISEASE COMPRESS IS ACCOMPLISHED BY INSERTING SPECIAL CODE
C     IN RDCMPR AND RDTDEL.
C
      IF (.NOT. LTEE) THEN
         IF (INUM .EQ. 1) THEN
            GOTO 225                  !RETURN
         ELSEIF (INUM .EQ. 2 .AND. ITRN .LE. IRRTRE) THEN
            GOTO 222
         ELSE
            GOTO 90
         ENDIF
      ENDIF

C
C     COMPRESS THE TREE LIST FOR ROOT DISEASE IF NUMBER OF TREES
C     EXCEEDS DEFAULT VALUE FOR ROOT DISEASE MODEL.
C
C     25-MAR-2003
C     The FVS compression routine expects values in the WK2 array to
C     be mortality. Mortality has not been predicted at this time, but
C     other processes (SVS) may have used WK2 and left residual values.
C     WK2 needs to be reset to zero for compression to function properly.
C     As of this date, the next user of the WK2 array is CUTS which
C     stores (not retrieves) removal priority values in WK2.
C     This change was discussed and agreed upon by Gary Dixon and Lance David.

   90 CONTINUE
      ICCY = 1
      KDT = IY(ICCY)

      DO 91 I =1,ITRN
         WK2(I)=0.0
   91 CONTINUE

      CALL COMPRS (IRCOMP,0.5)

C
C     ADD OPTION PROCESSING LOGIC FOR COMPRS.
C
      PRMS(1) = FLOAT(IRCOMP)
      PRMS(2) = 0.5

      CALL OPADD (KDT,MYACT(1),KDT,2,PRMS,II)

   95 CONTINUE
C
C     REESTABLISH THE SPECIES-ORDER SORT.
C
      CALL SPESRT
C
C     REESTABLISH THE DIAMETER SORT.
C
      CALL RDPSRT (ITRN,DBH,IND,.TRUE.)
C
C     INITIALIZE SPCNT (EQUIVALENCED TO WK6).
C
CLRD     DO 50 I=1,33
CLRD         WK6(I) = 0.0

      DO 50 I=1,MAXSP
        DO 49 II=1,3
          SPCNT(I,II) = 0.0
   49   CONTINUE
   50 CONTINUE

C
C     RE-COMPUTE THE SPECIES COMPOSITION
C
      DO 60 I=1,ITRN
         IS = ISP(I)
         IM = IMC(I)
         SPCNT(IS,IM) = SPCNT(IS,IM) + PROB(I)
   60 CONTINUE

C
C     RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND SPECIES-
C     TREE CLASS COMPOSITON BY TREES PER ACRE.
C
C     (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
C      DISTRIBUTIONS).
C
      IFST = 1
      CALL PCTILE (ITRN,IND,PROB,WK3,ONTCUR(7))
      CALL DIST (ITRN,ONTCUR,WK3)
      CALL COMP (OSPCT,IOSPCT,SPCNT)
      IF (LTEE .AND. INUM .EQ. 2) GOTO 225
C
C     CALL ROOT DISEASE SETUP ROUTINE.
C
  222 CONTINUE
      CALL RDSETP

  225 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT RDMN1'
      RETURN
      END
