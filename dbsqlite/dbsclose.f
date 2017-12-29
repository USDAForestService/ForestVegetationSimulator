      SUBROUTINE DBSCLOSE(LCOUT,LCIN)
      IMPLICIT NONE
C
C $Id: dbsclose.f 295 2012-05-31 18:52:14Z ncrookston.fs@gmail.com $
C
C     PURPOSE: TO CLOSE DATABASE CONNECTIONS
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER fsql3_close,IRT
      LOGICAL LCOUT,LCIN
      IF(IOUTDBREF.NE.-1 .AND. LCOUT) THEN
        IRT=fsql3_close(IOUTDBREF)
        IOUTDBREF = -1
      ENDIF
      IF(IINDBREF.NE.-1 .AND. LCIN) THEN
        IRT=fsql3_close(IINDBREF)
        IINDBREF = -1
      ENDIF

      END
