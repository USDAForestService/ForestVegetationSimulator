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
      INTEGER, PARAMETER :: MxMsg=100
      CHARACTER(LEN=MxMsg) Msg
      INTEGER fsql3_close,fsql3_errmsg,fsql3_finalize,I
      LOGICAL LCOUT,LCIN

      IF(IoutDBref.GE.0 .AND. LCOUT) THEN
        I = fsql3_finalize(IoutDBref)
        I = fsql3_close(IoutDBref)
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IoutDBref,Msg,MxMsg)
          print *,"DBS close error msg for DSNOUT=",Msg(:I)
        ENDIF
      ENDIF
      IoutDBref = -1
      
      IF(IinDBref.GE.0 .AND. LCIN) THEN
        I = fsql3_finalize(IinDBref)
        I = fsql3_close(IinDBref)
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IinDBref,Msg,MxMsg)
          print *,"DBS close error msg for DSNOUT=",Msg(:I)
        ENDIF
      ENDIF
      IinDBref = -1

      END

