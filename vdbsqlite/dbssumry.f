      SUBROUTINE DBSSUMRY(IYEAR,IAGE,NPLT,ITPA,IBA,ISDI,ICCF,
     -  ITOPHT,FQMD,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -  IATBA,IATSDI,IATCCF,IATTOPHT,FATQMD,IPRDLEN,IACC,IMORT,YMAI,
     -  IFORTP,ISZCL,ISTCL)
      IMPLICIT NONE
C----------
C VDBSQLITE $Id$
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              OUTPUT.
C     AUTH: D. GAMMEL -- SEM -- JUNE 2002
C     INPUT:
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/ACRE
C              4: TOTAL CU FT
C     *        4: MERCH CU FT (PULP AND SAWLOG)
C              5: MERCH CU FT
C     *        5: MERCH CU FT (SAWLOG)
C              6: MERCH BD FT
C     *        6: MERCH BD FT (SAWLOG)
C              7: REMOVED TREES/ACRE
C              8: REMOVED TOTAL CU FT
C     *        8: REMOVED MERCH CU FT (PULP AND SAWLOG)
C              9: REMOVED MERCH CU FT
C     *        9: REMOVED MERCH CU FT (SAWLOG)
C             10: REMOVED MERCH BD FT
C     *       10: REMOVED MERCH BD FT (SAWLOG)
C             11: BASAL AREA/ACRE
C             12: CCF
C             13: AVERAGE DOMINATE HEIGHT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU FT/ACRE)
C             16: MORTALITY  (ANNUAL IN CU FT/ACRE)
C             17: SAMPLE WEIGHT
C             18: FOREST COVER TYPE CODE
C             19: SIZE CLASS
C             20: STOCKING CLASS
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C
      INTEGER IYEAR,IAGE,IPRDLEN,IACC,IMORT,ITPA,IBA,ISDI,ICCF,
     -        ITOPHT,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -        IATBA,IATSDI,IATCCF,IATTOPHT,IFORTP,ISZCL,ISTCL
      INTEGER ColNumber,iRet
      DOUBLE PRECISION FQMDB,FATQMDB,YMAIB
      REAL FQMD,FATQMD,YMAI
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=*) NPLT
C
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(ISUMARY.EQ.0) RETURN
C
      CALL DBSCASE(1)

C     DEFINE TAABLENAME

      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
        iRet=fsql3_tableexists(IoutDBref,'FVS_Summary_East'//CHAR(0))
      ELSE
        iRet=fsql3_tableexists(IoutDBref,'FVS_Summary'//CHAR(0))
      ENDIF
      IF(iRet.EQ.0) THEN
C
C       EASTERN VARIANT VOLUME NOMENCLATURE
C
        IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     1      (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
C
            SQLStmtStr='CREATE TABLE FVS_Summary_East('//
     -                 'CaseID char(36),'//
     -                 'StandID Char(26),'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa int null,'//
     -                 'BA int null,'//
     -                 'SDI int null,'//
     -                 'CCF int null,'//
     -                 'TopHt int null,'//
     -                 'QMD real null,'//
     -                 'MCuFt int null,'//
     -                 'SCuFt int null,'//
     -                 'SBdFt int null,'//
     -                 'RTpa int null,'//
     -                 'RMCuFt int null,'//
     -                 'RSCuFt int null,'//
     -                 'RSBdFt int null,'//
     -                 'ATBA int null,'//
     -                 'ATSDI int null,'//
     -                 'ATCCF int null,'//
     -                 'ATTopHt int null,'//
     -                 'ATQMD real null,'//
     -                 'PrdLen int null,'//
     -                 'Acc int null,'//
     -                 'Mort int null,'//
     -                 'MAI real null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null);'//CHAR(0)
        ELSE
C
C       WESTERN VARIANT VOLUME NOMENCLATURE
C
            SQLStmtStr='CREATE TABLE FVS_Summary('//
     -                 'CaseID char(36),'//
     -                 'StandID Char(26),'//
     -                 'Year int null,'//
     -                 'Age int null,'//
     -                 'Tpa int null,'//
     -                 'BA int null,'//
     -                 'SDI int null,'//
     -                 'CCF int null,'//
     -                 'TopHt int null,'//
     -                 'QMD real null,'//
     -                 'TCuFt int null,'//
     -                 'MCuFt int null,'//
     -                 'BdFt int null,'//
     -                 'RTpa int null,'//
     -                 'RTCuFt int null,'//
     -                 'RMCuFt int null,'//
     -                 'RBdFt int null,'//
     -                 'ATBA int null,'//
     -                 'ATSDI int null,'//
     -                 'ATCCF int null,'//
     -                 'ATTopHt int null,'//
     -                 'ATQMD real null,'//
     -                 'PrdLen int null,'//
     -                 'Acc int null,'//
     -                 'Mort int null,'//
     -                 'MAI real null,'//
     -                 'ForTyp int null,'//
     -                 'SizeCls int null,'//
     -                 'StkCls int null);'//CHAR(0)
        ENDIF
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ISUMARY = 0
          RETURN
        ENDIF
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      FQMDB=FQMD
      FATQMDB=FATQMD
      YMAIB=YMAI

      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     1    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
C
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Summary_East',
     -          ' (CaseID,StandID,',
     -          'Year,Age,Tpa,BA,SDI,CCF,TopHt,QMD,MCuFt,SCuFt,SBdFt,',
     -          'RTpa,RMCuFt,RSCuFt,RSBdFt,ATBA,ATSDI,ATCCF,ATTopHt,',
     -          'ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)',
     -          'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',
     -          '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      ELSE
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Summary',
     -           ' (CaseID,StandID,',
     -           'Year,Age,Tpa,BA,SDI,CCF,TopHt,QMD,TCuFt,MCuFt,BdFt,',
     -           'RTpa,RTCuFt,RMCuFt,RBdFt,ATBA,ATSDI,ATCCF,ATTopHt,',
     -           'ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)',
     -           'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',
     -           '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      ENDIF
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
        ISUMARY = 0
        RETURN
      ENDIF
      
      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGE)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ITPA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IBA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ISDI)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,FQMDB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ITCUFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IMCUFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTPA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IRTCUFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IRMCUFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IRBDFT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IATBA)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IATSDI)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IATCCF)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IATTOPHT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,FATQMDB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IACC)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IMORT)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,YMAIB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IFORTP)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ISUMARY = 0
      ENDIF
      RETURN
      END


