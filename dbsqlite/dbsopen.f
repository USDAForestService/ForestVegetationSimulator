      SUBROUTINE DBSOPEN(LCOUT,LCIN,KODE)
      use iso_c_binding, only: C_NULL_CHAR
      IMPLICIT NONE
C
C $Id: dbsopen.f 1389 2014-12-19 21:46:29Z rhavis@msn.com $
C
C     PURPOSE: TO OPEN BOTH THE INPUT AND OUTPUT DATABASE CONNECTIONS
C            LCOUT and LCIN - passed in a TRUE if you want to open the
C             correspoinding databases, 0 if you don't
C            KODE     - RETURN CODE 0: OPEN SUCCESSFUL
C                                   1: OPEN NOT SUCCESSFUL
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER JOSTND,I,KODE
      LOGICAL LCOUT,LCIN
C
C     CLOSE ANY OPEN CONNECTIONS
C
      CALL DBSCLOSE(LCOUT,LCIN)

      IF (LCOUT) THEN
        I = fsql3_open(IOUTDBREF,trim(DSNOUT)//C_NULL_CHAR)
        IF (I.NE.0) THEN
          KODE=1
          RETURN
        ENDIF
      ENDIF

      IF (LCIN) THEN
        I = fsql3_open(IINDBREF,trim(DSNIN)//C_NULL_CHAR)
        IF (I.NE.0) THEN
          KODE=1
          RETURN
        ENDIF
      ENDIF
      RETURN
      END

