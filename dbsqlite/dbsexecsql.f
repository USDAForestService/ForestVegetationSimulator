      SUBROUTINE DBSEXECSQL (SQLCMD,IConn,LSCHED,IRC)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     PURPOSE: EXECUTES USER DEFINED QUERIES UPON OPENED
C              DATABASE CONNECTIONS
C     INPUT: SQLCMD   - SQL COMMAND QUERY TO BE EXECUTED
C            IConn - THE CONNECTION TO THE DBMS
C            LSCHED   - SPECIFIES WHETHER THIS IS COMING FROM THE EVENT
C                       MONITOR
COMMONS
C
C
      INCLUDE  'PRGPRM.F77'
C
C
      INCLUDE  'CONTRL.F77'
C
C
      INCLUDE  'OPCOM.F77'
C
C
      INCLUDE  'PLOT.F77'
C
C
      INCLUDE  'DBSCOM.F77'
C
COMMONS

      integer fsql3_prepare,fsql3_step,fsql3_colcnt,fsql3_colname,
     >        fsql3_colisnull,fsql3_finalize
      real fsql3_colreal
      CHARACTER*(*) SQLCMD
      INTEGER IConn,ColumnCount,MxNameLen,ColNumber,NameLen
      PARAMETER (MxNameLen=50)
      CHARACTER (LEN=MxNameLen) ColName
      INTEGER KODE,I,IOPKD
      LOGICAL LSCHED
      INTEGER iRet,IRC

      IRC = 1

C     MAKE SURE WE HAVE AN OPEN CONNECTION

      IF(IConn.EQ.-1) RETURN 

C     PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
      CALL DBSPRSSQL(SQLCMD,LSCHED,KODE)
      IF(KODE.EQ.0) THEN
C       THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
        WRITE (JOSTND,110) TRIM(SQLCMD)
  110   FORMAT (/'********   ERROR: SQLOUT/SQLIN PARSING FAILED. '
     &            'SQL STMT: ',A)
        CALL RCDSET(2,.TRUE.)
        RETURN
      ENDIF
      
      iRet = fsql3_prepare(Iconn,trim(SQLCMD)//CHAR(0))  
      IF (iRet.NE.0) THEN 
        iRet = fsql3_finalize(Iconn)
        RETURN 
      ENDIF
      DO WHILE(fsql3_step(Iconn)==1)
        ColumnCount = fsql3_colcnt(Iconn)
        IF(ColumnCount.GT.0) THEN
          DO ColNumber = 0,ColumnCount-1
            NameLen = fsql3_colname(Iconn,ColNumber,ColName,MxNameLen)
            ColName(NameLen+1:) = ""
            IOPKD = 0            
            DO I=1,ITST5
              IF (ColName(:8).EQ.CTSTV5(I)) THEN
                IOPKD = I
                EXIT
              ENDIF
            ENDDO
            IF (IOPKD.EQ.0) THEN
              IF(ITST5.LT.MXTST5) THEN
                ITST5 = ITST5+1
                CTSTV5(ITST5) = ColName(:8)
                LTSTV5(ITST5) = .FALSE.
                IOPKD = ITST5
              ENDIF
            ENDIF            
            IF(IOPKD.GT.0) THEN
               IF (fsql3_colisnull(Iconn,ColNumber) .eq. 1) THEN
                 TSTV5(IOPKD) = 0
                 LTSTV5(IOPKD) = .FALSE.
               ELSE 
                 TSTV5(IOPKD) = fsql3_colreal(Iconn,ColNumber,0)
                 LTSTV5(IOPKD) = .TRUE.
               ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      iRet = fsql3_finalize(Iconn)
      IRC = 0
      RETURN
      END
