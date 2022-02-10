      SUBROUTINE DBSSUMRY(IYEAR,IAGE,NPLT,ITPA,IBA,ISDI,ICCF,
     -  ITOPHT,FQMD,ITCUFT,IMCUFT,IBDFT,IRTPA,IRTCUFT,IRMCUFT,IRBDFT,
     -  IATBA,IATSDI,IATCCF,IATTOPHT,FATQMD,IPRDLEN,IACC,IMORT,YMAI,
     -  IFORTP,ISZCL,ISTCL)
      IMPLICIT NONE
C----------
C METRIC-VDBSQLITE $Id$
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
C              OUTPUT.
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
C
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif
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
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(ISUMARY.NE.1) RETURN
C
      CALL DBSCASE(1)

C     DEFINE TAABLENAME

      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
        iRet=fsql3_tableexists(IoutDBref,
     >     'FVS_Summary_East_Metric'//CHAR(0))
      ELSE
        iRet=fsql3_tableexists(IoutDBref,'FVS_Summary_Metric'//CHAR(0))
      ENDIF
      IF(iRet.EQ.0) THEN
C
C       EASTERN VARIANT VOLUME NOMENCLATURE
C
      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
C
            SQLStmtStr='CREATE TABLE FVS_Summary_East_Metric('//
     -                 'CaseID text not null,'//
     -                 'StandID text not null,'//
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tph int,'//
     -                 'BA int,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'TCuM int,'//
     -                 'MCuM int,'//
     -                 'CCuM int,'//
     -                 'RTph int,'//
     -                 'RTCuM int,'//
     -                 'RMCuM int,'//
     -                 'RCCuM int,'//
     -                 'ATBA int,'//
     -                 'ATSDI int,'//
     -                 'ATCCF int,'//
     -                 'ATTopHt int,'//
     -                 'ATQMD real,'//
     -                 'PrdLen int,'//
     -                 'Acc int,'//
     -                 'Mort int,'//
     -                 'MAI real,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int);'//CHAR(0)
        ELSE
C
C       WESTERN VARIANT VOLUME NOMENCLATURE
C
            SQLStmtStr='CREATE TABLE FVS_Summary_Metric('//
     -                 'CaseID text not null,'//
     -                 'StandID text not null,'//
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tph int,'//
     -                 'BA int,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'TCuM int,'//
     -                 'MCuM int,'//
     -                 'BdNA int,'//
     -                 'RTph int,'//
     -                 'RTCuM int,'//
     -                 'RMCuM int,'//
     -                 'RBdNA int,'//
     -                 'ATBA int,'//
     -                 'ATSDI int,'//
     -                 'ATCCF int,'//
     -                 'ATTopHt int,'//
     -                 'ATQMD real,'//
     -                 'PrdLen int,'//
     -                 'Acc int,'//
     -                 'Mort int,'//
     -                 'MAI real,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int);'//CHAR(0)
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
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
C
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Summary_East_Metric',
     -          ' (CaseID,StandID,',
     -          'Year,Age,Tph,BA,SDI,CCF,TopHt,QMD,MCuM,SCuM,NCuM,',
     -          'RTph,RMCuM,RSCuM,RNCuM,ATBA,ATSDI,ATCCF,ATTopHt,',
     -          'ATQMD,PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls)',
     -          'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',
     -          '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      ELSE
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_Summary_Metric',
     -           ' (CaseID,StandID,',
     -           'Year,Age,Tph,BA,SDI,CCF,TopHt,QMD,TCuM,MCuM,BdNA,',
     -           'RTph,RTCuM,RMCuM,RBdNA,ATBA,ATSDI,ATCCF,ATTopHt,',
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


      SUBROUTINE DBSSUMRY2
      IMPLICIT NONE
C----------
C METRIC-VDBSQLITE $Id$
C----------
C     PURPOSE: TO POPULATE A DATABASE WITH SUMMARY STATISTICS
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'  
      INCLUDE 'OPCOM.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'SUMTAB.F77'
C
COMMONS
C
      INTEGER IYEAR,ICCF,ITOPHT,IOSDI,IPRDLEN,IHRVC,IAGEOUT
      DOUBLE PRECISION DPTPA,DPTPTPA,DPBA,DPQMD,DPTCUFT,DPTPTCUFT,
     > DPMCUFT,DPTPMCUFT,DPBDFT,DPTPBDFT,DPACC,DPMORT,DPMAI,DPRTPA,
     > DPRTCUFT,DPRMCUFT,DPRBDFT
      INTEGER ColNumber,iRet,I
      CHARACTER*2000 SQLStmtStr
      CHARACTER*24 TABLENAME  
C
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize 
     
      IF(ISUMARY.NE.2) RETURN
C
      IYEAR    = IY(ICYC)
      IAGEOUT  = IOSUM(2,ICYC)
      ICCF     = IOSUM(12,ICYC)
      ITOPHT   = IOSUM(13,ICYC)
      IOSDI    = ISDI(ICYC)
      DPTPA    = OLDTPA/GROSPC
      DPBA     = OLDBA/GROSPC
      DPQMD    = ORMSQD
      IHRVC    = 0
      IF (ICYC.GT.NCYC) THEN
        DPTCUFT  = OCVCUR(7)/GROSPC
        DPMCUFT  = OMCCUR(7)/GROSPC
        DPBDFT   = OBFCUR(7)/GROSPC
      ELSE
        DPTCUFT  = TSTV1(4)
        DPMCUFT  = TSTV1(5)
        DPBDFT   = TSTV1(6)
      ENDIF
      DPTPTPA  = DPTPA   + (TRTPA/GROSPC)
      DPTPTCUFT= DPTCUFT + (TRTCUFT/GROSPC)
      DPTPMCUFT= DPMCUFT + (TRMCUFT/GROSPC)
      DPTPBDFT = DPBDFT  + (TRBDFT/GROSPC)
      DPRTPA   = 0.
      DPRTCUFT = 0.
      DPRMCUFT = 0.
      DPRBDFT  = 0.
      IPRDLEN  = 0
      DPACC    = 0. 
      IPRDLEN  = IOSUM(14,ICYC)
      DPACC    = OACC(7)/GROSPC
      DPMORT   = OMORT(7)/GROSPC
      DPMAI    = BCYMAI(ICYC) 
      IF (ICYC.LE.NCYC) THEN
        DPRTPA   = ONTREM(7)/GROSPC
        DPRTCUFT = OCVREM(7)/GROSPC
        DPRMCUFT = OMCREM(7)/GROSPC
        DPRBDFT  = OBFREM(7)/GROSPC
        IF (DPRTPA.LE.0.) THEN
          IPRDLEN  = IOSUM(14,ICYC)
          DPACC    = OACC(7)/GROSPC
          DPMORT   = OMORT(7)/GROSPC
          DPMAI    = BCYMAI(ICYC)
        ELSE
          IHRVC = 1
        ENDIF
      ENDIF

      CALL DBSCASE(1)

C     DEFINE TABLENAME

      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
        TABLENAME='FVS_Summary2_East_Metric'
      ELSE
        TABLENAME='FVS_Summary2_Metric'
      ENDIF
      iRet=fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
      IF(iRet.EQ.0) THEN
C
C       EASTERN VARIANT VOLUME NOMENCLATURE
C
      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
C                       
          SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)//
     -               ' (CaseID char,'//
     -                 'StandID char,'//
     -                 'Year int,'//
     -                 'RmvCode int,'//
     -                 'Age int,'//
     -                 'Tph real,'//
     -                 'TPrdTph real,'//
     -                 'BA real,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'MCuM real,'//
     -                 'TPrdMCuM real,'//
     -                 'SCuM real,'//
     -                 'TPrdNCuM real,'//
     -                 'NCuM real,'//
     -                 'TPrdNCuM real,'//
     -                 'RTph real,'//
     -                 'RMCuM real,'//
     -                 'RSCuM real,'//
     -                 'RNCuM real,'//
     -                 'PrdLen int,'//
     -                 'Acc real,'//
     -                 'Mort real,'//
     -                 'MAI real,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int);'//CHAR(0)
        ELSE
C
C       WESTERN VARIANT VOLUME NOMENCLATURE
C
          SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)//
     -               ' (CaseID char,'//
     -                 'StandID char,'//
     -                 'Year int,'//
     -                 'RmvCode int,'//
     -                 'Age int,'//
     -                 'Tph real,'//
     -                 'TPrdTph real,'//
     -                 'BA real,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'TCuM real,'//
     -                 'TPrdTCuM real,'//
     -                 'MCuM real,'//
     -                 'TPrdMCuM real,'//
     -                 'NABdM real,'//
     -                 'TPrdNABdM real,'//
     -                 'RTph real,'//
     -                 'RTCuM real,'//
     -                 'RMCuM real,'//
     -                 'RNABdM real,'//
     -                 'PrdLen int,'//
     -                 'Acc real,'//
     -                 'Mort real,'//
     -                 'MAI real,'//
     -                 'ForTyp int,'//
     -                 'SizeCls int,'//
     -                 'StkCls int);'//CHAR(0)
        ENDIF
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ISUMARY = 0
          RETURN
        ENDIF  
      ENDIF
      DO I=1,2
        IF (IHRVC.EQ.1) THEN
          DPTPTPA   = DPTPTPA   - DPRTPA  
          DPTPTCUFT = DPTPTCUFT - DPRTCUFT
          DPTPMCUFT = DPTPMCUFT - DPRMCUFT
          DPTPBDFT  = DPTPBDFT  - DPRBDFT 
        ENDIF      
      IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN') .OR.
     >    (VARACD .EQ. 'ON')) THEN
C
         SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)//
     -    ' (CaseID,StandID,Year,RmvCode,Age,Tph,TPrdTph,BA,SDI,'//
     -    'CCF,TopHt,QMD,MCuM,TPrdMCuM,SCuM,TPrdSCuM,SNCuM,'//
     -    'TPrdNCuM,RTph,RMCuM,RSCuM,RNCuM,'//
     -    'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//
     -    ")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,"//
     -    '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
     -    //CHAR(0)
        ELSE
         SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)//
     -    ' (CaseID,StandID,Year,RmvCode,Age,Tph,TPrdTph,BA,SDI,'//
     -    'CCF,TopHt,QMD,TCuM,TPrdTCuM,MCuM,TPrdMCuM,NABdM,'//
     -    'TPrdNABdM,RTph,RTCuM,RMCuM,RNABdM,'//
     -    'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//
     -    ")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,"//
     -    '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'  
     -    //CHAR(0)
        ENDIF
        iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ISUMARY = 0
          RETURN
        ENDIF
        ColNumber=1 
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IHRVC)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGEOUT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPA)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTPA)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBA)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IOSDI)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPQMD)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPTCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPMCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPBDFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPTPBDFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTPA)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRTCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRMCUFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPRBDFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPRDLEN)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPACC)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMORT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPMAI)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IFORTP)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISZCL)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTCL)
        iRet = fsql3_step(IoutDBref)
        IF (IHRVC.EQ.0) exit
        IHRVC    = 2
        IOSDI    = ISDIAT(ICYC)
        ICCF     = NINT(ATCCF/GROSPC)
        ITOPHT   = NINT(ATAVH)
        DPQMD    = ATAVD
        DPBA     = ATBA/GROSPC
        DPTPA    = ATTPA/GROSPC
        DPTCUFT  = MAX(0.,DPTCUFT-DPRTCUFT)
        DPMCUFT  = MAX(0.,DPMCUFT-DPRMCUFT)
        DPBDFT   = MAX(0.,DPBDFT -DPRBDFT)
        DPRTPA   = 0.
        DPRTCUFT = 0.
        DPRMCUFT = 0.
        DPRBDFT  = 0. 
      ENDDO
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ISUMARY = 0
      ENDIF
      RETURN
      END