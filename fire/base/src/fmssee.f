      SUBROUTINE FMSSEE (IT,JSP,D,H,SNUM,ITYP,DEBUG,IOUT)
      IMPLICIT NONE
C----------
C  **FMSSEE  FIRE-DATE OF LAST REVISION:  09/29/03
C----------
C PURPOSE:
C     SEES WHAT RANGE OF SNAG HEIGHTS WILL OCCUR IN EACH SPECIES+DBH
C     CLASS OF NEWLY CREATED SNAGS (HARVESTED OR KILLED MATERIAL), SO THAT
C     FMSADD WILL KNOW WHICH CLASSES NEED TO BE SPLIT INTO 2 HEIGHT CLASSES.
C     ALSO KEEPS TRACK OF TOTAL SNAG DENSITY EXPECTED IN EACH SPECIES+DBH
C     CLASS.
C-----------
C     CALLED FROM:  FMSCUT
C                   FMKILL
C                   CRATET
C-----------
C     Local variable definitions:
C
C     IT:   tree record
C     JSP:  species
C     D:    dbh
C     H:    height
C     SNUM: potential number of snags
C     ITYP: Code of where snags are coming from (0=fmscut,  1=kill,
C                                                3=initial, 4=keyword)
C
COMMONS
      INCLUDE 'PRGPRM.F77'
Cppe  INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

Cppe  INCLUDE 'PPCNTL.F77'
Csng  INCLUDE 'CONTRL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'FMCOM.F77'
C
CCOMMONS

      INTEGER DBHCL
      LOGICAL DEBUG
      INTEGER IOUT,ITYP,JSP,IT
      REAL    SNUM,H,D

C     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

      IF (.NOT. LFMON) RETURN

      IF (DEBUG) WRITE (IOUT,10) IT,JSP,D,H,SNUM,ITYP
   10 FORMAT (' IN FMSSEE, IT=',I4,' JSP=',I3,' D=',F7.3,
     >         ' H=',F7.3,' SNUM=',F7.3,' ITYP=',I3)

C     If this was called after a stand entry set the harvest year
C     even if no snags were created.
C     IF NO NEW SLASH IS CREATED WE DON'T WANT TO SET HARVYR
C     ASSIGNMENT MOVED FROM HERE TO **FMSCUT**
C     SB: MARCH 2002
C
Cppe  IF (ITYP .EQ. 0) HARVYR = MIY(MICYC-1)
Csng  IF (ITYP .EQ. 0) HARVYR = IY(ICYC)
C     IF (ITYP .EQ. 0) HARVYR = IY(ICYC)

C     If there are no snags, then return

      IF (SNUM .LE. 0.0) RETURN

      SNGNEW(IT) = SNUM

C     Find the dbh-class of the new snags

      IF (D .GE. 36.0) THEN
        DBHCL = 19
      ELSE
        DBHCL = INT( (D/2.0) + 1.0)
      END IF

C     Next, find the maximum and minimum height of each class, and
C     sum the number of snags in it.

      IF (H .GT. MAXHT(JSP,DBHCL)) MAXHT(JSP,DBHCL) = H
      IF (H .LT. MINHT(JSP,DBHCL)) MINHT(JSP,DBHCL) = H
      DSPDBH(JSP,DBHCL) = DSPDBH(JSP,DBHCL) + SNGNEW(IT)

      IF (ITYP .EQ. 4) SNGNEW(IT) = 0.0

      RETURN
      END

