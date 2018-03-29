      SUBROUTINE RDTDEL(IVAC,IREC)
      IMPLICIT NONE
C----------
C RD $Id: rdtdel.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  THIS SUBROUTINE DELETES AND PACKS THE ROOT DISEASE TREE LISTS.
C
C  CALLED BY :
C     TREDEL  [PROGNOSIS]
C
C  CALLS     :
C     RDATV   (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     IVAC   -
C     IREC   -
C
C  Revision History:
C    03/07/95 - Last revision date.
C    22-JUL-02 Lance R. David (FHTET)
C      Removed unused array PROBO. It was also unused in the old
C      annosus model.
C   09/04/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved check to exit to top.
C   03/21/2018 Lance R. David (FMSC)
C     Added debug control.
C     Added LGO (true if RD is active) to conditional exit.
C     Removed unused 3rd subroutine parameter.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
C
COMMONS
C
      LOGICAL DEBUG, LGO, LTEE
      INTEGER IDI, IP, IREC, ITSTP, IVAC, J, JINF
      REAL    TPAREA

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDTDEL',6,ICYC)
      
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER RDTDEL'

      CALL RDATV (LGO,LTEE)
      IF (DEBUG) WRITE(JOSTND,*) 'IN RDTDEL: LGO, LTEE, LSTART=',
     &   LGO,LTEE,LSTART

      IF ((.NOT. LTEE .AND. LSTART) .OR. .NOT. LGO) RETURN

C
C     IF ROOT DISEASE NOT ACTIVE OR NO PATCH AREA THEN RETURN.  ALSO
C     RETURN IF USING MANUAL INITIALIZATION AND THIS IS CYCLE 0 BECAUSE
C     ROOT DISEASE ARRAYS ARE EMPTY.  NOTE CODE IN RDMN1.
C
      TPAREA = 0.0
      DO 700 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
  700 CONTINUE       
      IF (TPAREA .EQ. 0.0) RETURN

C
C     PACK THE OUTSIDE TREE DENSITY
C     WHEN COMPRESSION OCCURS BEFORE ROOT DISEASE CYCLING
C     IE, WHEN COMPRS CALLED FROM RDMN1 THEN SET TEMP VARIABLE ITSTP
C     FOR SUMMARIZATION OF PROBI AND PROPI.
C
      ITSTP = ISTEP
      IF (ISTEP .EQ. 0) ITSTP = 1

C     IF (DEBUG) WRITE(JOSTND,777) IVAC,IREC
C777  FORMAT(' IN RDTDEL :  IVAC IREC', 2I5)

      WK22(IVAC) = WK22(IREC)
      WK22(IREC) = -1.0
      RROOTT(IVAC) = RROOTT(IREC)
      RROOTT(IREC) = -1.0

      DO 800 J = 1,4
         ROOTH(J,IVAC) = ROOTH(J,IREC)
         ROOTH(J,IREC) = -1.0
         XMTH(J,IVAC) = XMTH(J,IREC)
         XMTH(J,IREC) = -1.0
  800 CONTINUE

      ROOTL(IVAC) = ROOTL(IREC)
      RRKILL(IVAC) = RRKILL(IREC)
      FPROB(IVAC) = FPROB(IREC)
      PROBIU(IVAC) = PROBIU(IREC)
      PROBIT(IVAC) = PROBIT(IREC)
      PROBL(IVAC) = PROBL(IREC)
      FFPROB(IVAC,1) = FFPROB(IREC,1)
      FFPROB(IVAC,2) = FFPROB(IREC,2)

      DO 999 JINF = 1,ITSTP
        DO 990 IP=1,2
          PROBI(IVAC,JINF,IP) = PROBI(IREC,JINF,IP)
          PROPI(IVAC,JINF,IP) = PROPI(IREC,JINF,IP)
C         IF (DEBUG) WRITE(JOSTND,881) PROBI(IVAC,JINF,IP)
C 881     FORMAT(' IN RDTDEL :  PROBI',E20.8)
  990   CONTINUE
  999 CONTINUE

C     IF (DEBUG) WRITE (JOSTND,888) 
C    &  PROBI(IVAC,1,1),PROBIU(IVAC),FPROB(IVAC)
C 888 FORMAT (' IN RDTDEL :  CALC PROBI,PROBIU,FPROB',3E20.8)

      RETURN
      END
