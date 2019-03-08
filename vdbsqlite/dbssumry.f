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

      IF(ISUMARY.NE.1) RETURN
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
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tpa int,'//
     -                 'BA int,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'MCuFt int,'//
     -                 'SCuFt int,'//
     -                 'SBdFt int,'//
     -                 'RTpa int,'//
     -                 'RMCuFt int,'//
     -                 'RSCuFt int,'//
     -                 'RSBdFt int,'//
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
            SQLStmtStr='CREATE TABLE FVS_Summary('//
     -                 'CaseID char(36),'//
     -                 'StandID Char(26),'//
     -                 'Year int,'//
     -                 'Age int,'//
     -                 'Tpa int,'//
     -                 'BA int,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'TCuFt int,'//
     -                 'MCuFt int,'//
     -                 'BdFt int,'//
     -                 'RTpa int,'//
     -                 'RTCuFt int,'//
     -                 'RMCuFt int,'//
     -                 'RBdFt int,'//
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


      SUBROUTINE DBSSUMRY2    
      IMPLICIT NONE
C----------
C VDBSQLITE $Id$
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
      CHARACTER*20 TABLENAME  
C
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize 
     
      IF(ISUMARY.NE.2) RETURN
C
      IYEAR    = IY(ICYC)
      IAGEOUT  = IOSUM(2,ICYC)
      ICCF     = IOSUM(12,ICYC)
      ITOPHT   = IOSUM(13,ICYC)
      IOSDI    = ISDIAT(ICYC)
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
     >    (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
        TABLENAME='FVS_Summary2_East'
      ELSE    
        TABLENAME='FVS_Summary2'
      ENDIF
      iRet=fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
      IF(iRet.EQ.0) THEN
C
C       EASTERN VARIANT VOLUME NOMENCLATURE
C
        IF ((VARACD .EQ. 'CS') .OR. (VARACD .EQ. 'LS') .OR.
     >      (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
C                       
          SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)//
     -               ' (CaseID char,'//
     -                 'StandID char,'//
     -                 'Year int,'//
     -                 'RmvCode int,'//
     -                 'Age int,'//
     -                 'Tpa real,'//
     -                 'TPrdTpa real,'//
     -                 'BA real,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'MCuFt real,'//
     -                 'TPrdMCuFt real,'//
     -                 'SCuFt real,'//
     -                 'TPrdSCuFt real,'//
     -                 'SBdFt real,'//
     -                 'TPrdSBdFt real,'//
     -                 'RTpa real,'//
     -                 'RMCuFt real,'//
     -                 'RSCuFt real,'//
     -                 'RSBdFt real,'//
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
     -                 'Tpa real,'//
     -                 'TPrdTpa real,'//
     -                 'BA real,'//
     -                 'SDI int,'//
     -                 'CCF int,'//
     -                 'TopHt int,'//
     -                 'QMD real,'//
     -                 'TCuFt real,'//
     -                 'TPrdTCuFt real,'//
     -                 'MCuFt real,'//
     -                 'TPrdMCuFt real,'//
     -                 'BdFt real,'//
     -                 'TPrdBdFt real,'//
     -                 'RTpa real,'//
     -                 'RTCuFt real,'//
     -                 'RMCuFt real,'//
     -                 'RBdFt real,'//
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
     >      (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'SN')) THEN
C
         SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)//
     -    ' (CaseID,StandID,Year,RmvCode,Age,Tpa,TPrdTpa,BA,SDI,'//
     -    'CCF,TopHt,QMD,MCuFt,TPrdMCuFt,SCuFt,TPrdSCuFt,SBdFt,'//               
     -    'TPrdSBdFt,RTpa,RMCuFt,RSCuFt,RSBdFt,'//
     -    'PrdLen,Acc,Mort,MAI,ForTyp,SizeCls,StkCls'//
     -    ")VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,?,?,?,"//
     -    '?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
     -    //CHAR(0)
        ELSE
         SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)//
     -    ' (CaseID,StandID,Year,RmvCode,Age,Tpa,TPrdTpa,BA,SDI,'//
     -    'CCF,TopHt,QMD,TCuFt,TPrdTCuFt,MCuFt,TPrdMCuFt,BdFt,'//
     -    'TPrdBdFt,RTpa,RTCuFt,RMCuFt,RBdFt,'//
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
        IOSDI    = ISDI(ICYC)
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


