      SUBROUTINE DBSPRSSQL(ORIGSTR,LSCHED,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C
C     PURPOSE: TO PARSE OUT AND REPLACE EVENT MONITOR VARIABLES AND OTHER
C              KEYWORDS THAT SHOULD BE WRAPPED BY % SIGNS IN THE SQL
C              STATEMENT WITH THEIR VALUES
C     AUTH: D. GAMMEL -- RMRS -- MARCH 2003
C     INPUT: ORIGSTR  - STRING TO BE MODIFIED
C            LSCHED   - SPECIFIES IF THIS IS A SCHEDULED EVENT OR NOT
C            KODE     - RETURN KODE 0 - FAILED IN PARSING, 1 - OK
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
      CHARACTER*1 DELIMSTR
      CHARACTER(LEN=*) ORIGSTR
      CHARACTER(LEN=LEN(ORIGSTR)) STR,NEWSTR,TOKEN
      CHARACTER*20 CTOK,CMPUSTR
      INTEGER KODE,I,IRETVAL,IRC
      REAL,DIMENSION(1)::RVAL
      LOGICAL LSCHED
C
C
      STR = ORIGSTR
      DELIMSTR = '%'
      KODE = 1
C
C     SEND STRING TO DBSPRS WITH DELIMITER
      CALL DBSPRS(TOKEN,STR,DELIMSTR)
      DO WHILE (LEN_TRIM(STR).GT.0)
        IF(LEN_TRIM(ORIGSTR).NE.LEN_TRIM(TOKEN)) THEN
C         FOUND A KEYWORD
          KODE = 0
          NEWSTR = TOKEN
          CALL DBSPRS(TOKEN,STR,DELIMSTR)
          DO I = 1, LEN_TRIM(TOKEN)
            CALL UPCASE(TOKEN(I:I))
          END DO
C         REPLACE KEYWORD WITH VALUE
          SELECT CASE(TRIM(TOKEN))
           CASE ('STANDID')
              IF (NPLT.EQ.' ') THEN
                 NEWSTR = TRIM(NEWSTR)//'NULL'
              ELSE
                 NEWSTR = TRIM(NEWSTR)//TRIM(NPLT)
              ENDIF
              KODE = 1
            CASE ('MGMTID')
              IF (MGMID.EQ.' ') THEN
                 NEWSTR = TRIM(NEWSTR)//'NULL'
              ELSE
                 NEWSTR = TRIM(NEWSTR)//TRIM(MGMID)
              ENDIF
              KODE = 1
            CASE ('STAND_CN')
              IF (DBCN.EQ.' ') THEN
                 NEWSTR = TRIM(NEWSTR)//'NULL'
              ELSE
                 NEWSTR = TRIM(NEWSTR)//TRIM(DBCN)
              ENDIF
              KODE = 1
            CASE('VARIANT')
              NEWSTR = TRIM(NEWSTR)//VARACD
              KODE = 1
            CASE('FVSCASE')
              CALL DBSCASE(1)
              NEWSTR = TRIM(NEWSTR)//' '//CASEID
            CASE DEFAULT
              IF(LSCHED) THEN
C               CHECK AGAINST COMPUTES AND OTHER EVENT MONITOR VARS
                CTOK = TOKEN
                CALL ALGKEY(CTOK,LEN_TRIM(CTOK),IRETVAL,IRC)
                IF(IRC.EQ.0) THEN
C                 FOUND THE TOKEN
                  CALL EVLDX(RVAL,1,IRETVAL,IRC)
                ENDIF
                IF(IRETVAL.EQ.0) THEN
C                 THE VALUE IS DEFINED SO GRAB VALUE TO REPLACE STRIPPED TOKEN
                  KODE = 1
                  IF(FLOAT(IFIX(RVAL(1))).NE.RVAL(1)) THEN
                    WRITE(CMPUSTR,'(E14.7)')RVAL(1)
                  ELSE
                    WRITE(CMPUSTR,'(I8)')IFIX(RVAL(1))
                  ENDIF
                  NEWSTR = TRIM(NEWSTR)//' '//TRIM(adjustl(CMPUSTR))
                ELSE
                  KODE = 1
                  NEWSTR = TRIM(NEWSTR)//' NULL'
                ENDIF
              ELSE
                NEWSTR = TRIM(NEWSTR)//TRIM(TOKEN)
              ENDIF
          END SELECT
        ENDIF
        ORIGSTR = TRIM(NEWSTR)//TRIM(STR)
        STR = ORIGSTR
        CALL DBSPRS(TOKEN,STR,DELIMSTR)
      END DO
      END
