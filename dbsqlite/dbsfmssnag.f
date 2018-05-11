      SUBROUTINE DBSFMSSNAG(IYEAR,NPLT,HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,
     -  HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF,KODE)
 
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE SUMMARY SNAG REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE SUMMARY SNAG OUTPUT FROM THE FIRE MODEL.
C              1: HARD SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
C              2: HARD SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
C              3: HARD SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
C              4: HARD SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
C              5: HARD SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
C              6: HARD SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
C              7: HARD SNAGS - TOTAL       (> 0)
C              8: SOFT SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
C              9: SOFT SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
C             10: SOFT SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
C             11: SOFT SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
C             12: SOFT SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
C             13: SOFT SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
C             14: SOFT SNAGS - TOTAL       (> 0)
C             15: HARD+SOFT  - TOTAL       (> 0)
C             16: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
C     NOTE: THE DBH CLASS BREAKS CAN BE CHANGED BY THE SNAGCLAS KEYWORD
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,KODE,iRet,ColNumber
      REAL HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,
     -  SCL6,SCL7,HDSF
      DOUBLE PRECISION HCL1B,HCL2B,HCL3B,HCL4B,HCL5B,HCL6B,HCL7B,
     -  SCL1B,SCL2B,SCL3B,SCL4B,SCL5B,SCL6B,SCL7B,HDSFB
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=26) NPLT
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize


      IF(ISSUM.EQ.0) RETURN
      IF(ISSUM.EQ.2) KODE = 0

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_SnagSum"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_SnagSum('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Hard_snags_class1 real null,'//
     -              'Hard_snags_class2 real null,'//
     -              'Hard_snags_class3 real null,'//
     -              'Hard_snags_class4 real null,'//
     -              'Hard_snags_class5 real null,'//
     -              'Hard_snags_class6 real null,'//
     -              'Hard_snags_total  real null,'//
     -              'Soft_snags_class1 real null,'//
     -              'Soft_snags_class2 real null,'//
     -              'Soft_snags_class3 real null,'//
     -              'Soft_snags_class4 real null,'//
     -              'Soft_snags_class5 real null,'//
     -              'Soft_snags_class6 real null,'//
     -              'Soft_snags_total  real null,'//
     -              'Hard_soft_snags_total real null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           ISSUM = 0
           RETURN
         ENDIF
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      HCL1B = HCL1
      HCL2B = HCL2
      HCL3B = HCL3
      HCL4B = HCL4
      HCL5B = HCL5
      HCL6B = HCL6
      HCL7B = HCL7
      SCL1B = SCL1
      SCL2B = SCL2
      SCL3B = SCL3
      SCL4B = SCL4
      SCL5B = SCL5
      SCL6B = SCL6
      SCL7B = SCL7
      HDSFB = HDSF

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_SnagSum (CaseID,',
     -  'StandID,Year,Hard_snags_class1,Hard_snags_class2,',
     -  'Hard_snags_class3,Hard_snags_class4,Hard_snags_class5,',
     -  'Hard_snags_class6,Hard_snags_total,Soft_snags_class1,',
     -  'Soft_snags_class2,Soft_snags_class3,Soft_snags_class4,',
     -  'Soft_snags_class5,Soft_snags_class6,Soft_snags_total,',
     -  'Hard_soft_snags_total) VALUES(''',CASEID,''',''',TRIM(NPLT),
     -  ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
     
      iRet = fsql3_prepare(IoutDBref, SQLStmtStr)

      IF (iRet .NE. 0) THEN
         ISSUM = 0
         RETURN
      ENDIF

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL1B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL2B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL4B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL5B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL6B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL7B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL1B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL2B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL4B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL5B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL6B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL7B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HDSFB)
      
      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ISSUM = 0
      ENDIF
      RETURN

      END

