      SUBROUTINE DBSOPEN(LCOUT,LCIN,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     PURPOSE: TO OPEN BOTH THE INPUT AND OUTPUT DATABASE CONNECTIONS
C            LCOUT and LCIN - passed in a TRUE if you want to open the
C             correspoinding databases, 0 if you do not
C            KODE     - RETURN CODE 0: OPEN SUCCESSFUL
C                                   1: OPEN NOT SUCCESSFUL
C---
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER, PARAMETER :: MxMsg=100
      CHARACTER(LEN=MxMsg) Msg
      INTEGER I,KODE,fsql3_open,fsql3_errmsg
      LOGICAL LCOUT,LCIN
C
C     CLOSE ANY OPEN CONNECTIONS
C
      CALL DBSCLOSE(LCOUT,LCIN)

      KODE=0
      
      IF (LCOUT) THEN
        I = fsql3_open(IoutDBref,trim(DSNOUT)//CHAR(0))
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IoutDBref,Msg,MxMsg)
          print *," IoutDBref=",IoutDBref,
     >            "DBS open error msg for DSNOUT=",Msg(:I)
          call abort
          KODE=1
          IF (.NOT.LCIN) RETURN
        ENDIF
      ENDIF

      IF (LCIN) THEN
        I = fsql3_open(IinDBref,trim(DSNIN)//CHAR(0))
        IF (I.NE.0) THEN
          I = fsql3_errmsg(IinDBref,Msg,MxMsg)
          print *," IinDBref=",IinDBref,
     >            " DBS open error msg for DSNIN=",Msg(:I)
          call abort
          KODE=1
          RETURN
        ENDIF
      ENDIF
      RETURN
      END

