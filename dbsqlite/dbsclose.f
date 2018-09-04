      SUBROUTINE DBSCLOSE(LCOUT,LCIN)
      IMPLICIT NONE
C
C DBSQLITE $Id$
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
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IoutDBref,Msg,MxMsg)
          print *," IoutDBref=",IoutDBref,
     >            "DBS finalize error msg for DSNOUT=",Msg(:I)
        ENDIF
        I = fsql3_close(IoutDBref)
        IoutDBref = -1
      ENDIF
      
      IF(IinDBref.GE.0 .AND. LCIN) THEN
        I = fsql3_finalize(IinDBref)
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IinDBref,Msg,MxMsg)
          print *," IoutDBref=",IinDBref,
     >            "DBS finalize error msg for DSNIN=",Msg(:I)
        ENDIF
        I = fsql3_close(IinDBref)
        IinDBref = -1
      ENDIF
      END

