      SUBROUTINE DBSREFERENCE
      IMPLICIT NONE
C----------------------------------------------------------------------
C  ROUTINE TO OUTPUT INVENTORY REFERENCE INFORMATION
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DBSCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'VOLSTD.F77'

      INTEGER iRet, ColNumber, iInvRef, I, SPPNUM,ISPPSDI,ISTIDX
      DOUBLE PRECISION FORMCLS,MIND,MERCHTOPD,STUMP,
     >                 SAWD,SAWTD,SAWSTMP,BFD,BFTD,BFSTUMP
      CHARACTER*4    SPPFVS,SPPPLTS
      CHARACTER*5    SPPFIA
      CHARACTER*2000 SQLStmtStr
      CHARACTER*20   TABLENAME
      CHARACTER*3    CRUISETYPE
      CHARACTER*7    SDITYPE

      INTEGER fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_addcolifabsent


      CALL DBSCASE(1)

      TABLENAME = 'FVS_InvReference'

      iRet=fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE '//TRIM(TABLENAME)//
     -             ' (CaseID text not null, '//
     -             'StandID text not null, '//
     -             'LocationCode int, '//
     -             'SpeciesNum int, '//
     -             'SpeciesFVS text, '//
     -             'SpeciesPlants text, '//
     -             'SpeciesFIA text, '//
     -             'SDIType text,'//
     -             'SDIMax int, '//
     -             'SiteIndex int, '//
     -             'CFCruiseType text,'//
     -             'CFVolEq text, '//
     -             'CFMinDBH real, '//
     -             'CFTopDia real, '//
     -             'CFStump real, '//
     -             'CFSawMinDBH real, '//
     -             'CFSawTopDia real, '//
     -             'CFSawStump real, '//
     -             'BFVolEq text, '//
     -             'BFMinDBH real, '//
     -             'BFTopDia real, '//
     -             'BFStump real);'//CHAR(0)

        iRet=fsql3_exec(IoutDBref, SQLStmtStr)
        IF(iRet.NE.0) THEN 
          iInvRef=0
          RETURN
        ENDIF
      ENDIF

C--------
C     CHECK EXISTING TABLE FOR COLUMN(S) ADDED WITH LOCCODE UPGRADE (2026)
C     `LocationCode`, 
C     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE
C--------
      iRet= fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),
     >        "LocationCode"//CHAR(0),"int"//CHAR(0))

      DO I=1,MAXSP
        IF(TRIM(JSP(I)).EQ.'') CYCLE
        ISPPSDI=NINT(SDIDEF(I))
        ISTIDX=NINT(SITEAR(I))
        MIND=DBHMIN(I)
        MERCHTOPD=TOPD(I)
        STUMP=STMP(I)
        SAWD=SCFMIND(I)
        SAWTD=SCFTOPD(I)
        SAWSTMP=SCFSTMP(I)
        BFD=BFMIND(I)
        BFTD=BFTOPD(I)
        BFSTUMP=BFSTMP(I)
        IF(CFCTYPE .EQ. 'I') THEN
          CRUISETYPE = 'FIA'
        ELSE
          CRUISETYPE = 'FVS'
        END IF

        IF(LZEIDE) THEN
          SDITYPE = '  ZEIDE'
        ELSE
          SDITYPE = 'REINEKE'
        END IF

        SQLStmtStr='INSERT INTO '//TRIM(TABLENAME)//
     -             ' (CaseID,StandID,LocationCode,'//
     -             'SpeciesNum,SpeciesFVS,SpeciesPlants,SpeciesFIA,'//
     -             'SDIType,SDIMax,SiteIndex,'//
     -             'CFCruiseType,CFVolEq,CFMinDBH,CFTopDia,CFStump,'//
     -             'CFSawMinDBH,CFSawTopDia,CFSawStump,'//
     -             'BFVolEQ,BFMinDBH,BFTopDia,BFStump)'//
     -             " VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,'"//
     -             TRIM(JSP(I))//"','"//TRIM(PLNJSP(I))//"','"//
     -             TRIM(FIAJSP(I))//"','"// 
     -             SDITYPE//"',"//'?,?,'//
     -             "'"//CRUISETYPE//"','"//TRIM(VEQNNC(I))//"',?,?,?,"//
     -             '?,?,?,'//
     -             "'"//TRIM(VEQNNB(I))//"',?,?,?);"//CHAR(0)

        iRet=fsql3_prepare(IoutDBref,SQLStmtStr)
        IF(iRet.NE.0) THEN 
          iInvRef=0
          RETURN
        ENDIF

        ColNumber = 1
        iRet= fsql3_bind_int(IoutDBref,ColNumber,KODFOR)
        
        ColNumber = ColNumber + 1
        iRet= fsql3_bind_int(IoutDBref,ColNumber,I)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_int(IoutDBref,ColNumber,ISPPSDI)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_int(IoutDBref,ColNumber,ISTIDX)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,MIND)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,MERCHTOPD)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,STUMP)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,SAWD)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,SAWTD)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,SAWSTMP)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,BFD)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,BFTD)

        ColNumber = ColNumber + 1
        iRet= fsql3_bind_double(IoutDBref,ColNumber,BFSTUMP)

        iRet= fsql3_step(IoutDBref)
      ENDDO
      iRet= fsql3_finalize(IoutDBref)
      IF(iRet.NE.0) iInvRef = 0

      RETURN
      END