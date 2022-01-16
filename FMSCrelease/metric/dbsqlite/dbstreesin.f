      SUBROUTINE DBSTREESIN(IPLOT,ITREE,RCOUNT,HISTORY,SPECIES,DBH,DG,
     -    HT,HTTOPK,HTG,CRWNR,DMG1,SVR1,DMG2,SVR2,DMG3,SVR3,TREEVAL,
     -    PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,SITEPREP,KODE,DEBUG,
     -    JOSTND,LKECHO,ABIRTH,LBIRTH)
      IMPLICIT NONE
C
C METRIC-DBSQLITE $Id$
C
C     NOTE: ALL OF THE VARIABLES IN THIS ROUTINE ARE VOLATILE. THEY GET RESET
C           AT EVERY CALL. BE CAREFUL.
C---
COMMONS
C
      INCLUDE  'DBSCOM.F77'
      INCLUDE  'METRIC.F77'
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
# IF _WIN64
# ELSE
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
# ENDIF

COMMONS
              
      INTEGER, PARAMETER :: MxCname = 50, MxMsg = 500
      CHARACTER(LEN=MxCname) ColName
      INTEGER ITREE,IPLOT,HISTORY,CRWNR,DMG1,DMG2,DMG3,SVR1,SVR2
      INTEGER SVR3,TREEVAL,PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,KODE
      INTEGER SITEPREP,JOSTND
      REAL DBH,DG,HT,HTG,HTTOPK,RCOUNT,ABIRTH,DIAMETER
      CHARACTER(LEN=*) SPECIES
      INTEGER iRet
      LOGICAL MATCHED,LKECHO,LBIRTH,DEBUG
      INTEGER ColNumber,ColumnCount

      integer  fsql3_step,fsql3_finalize,fsql3_colisnull,
     >         fsql3_colname,fsql3_colcnt,fsql3_colint,fsql3_coltext
      real*4   fsql3_colreal

C     CHECK TO MAKE SURE WE ARE PULLING TREE INFO FROM DATABASE

      IF (DEBUG) WRITE(JOSTND,*) 'IN DBSTREESIN'

      IF (ITREEIN.EQ.0) THEN
        KODE = 0
        RETURN
      ENDIF 

      iRet = fsql3_step(IinDBref)
      if (iRet.ne.1) then
        ITREEIN=0     
        KODE=-1
        iRet = fsql3_finalize(IinDBref)
        RETURN
      ENDIF

      ABIRTH = 0
      LBIRTH  =.FALSE.
      IPLOT=0
      ITREE=0
      RCOUNT=0
      HISTORY=0
      SPECIES=' '
      DBH=0
      DIAMETER=0
      DG=0
      HT=0
      HTG=0
      HTTOPK=0
      CRWNR=0
      DMG1=0
      DMG2=0
      DMG3=0
      SVR1=0
      SVR2=0
      SVR3=0
      TREEVAL=0
      PRESCRIPT=0
      SLOPE=0
      ASPECT=0
      HABITAT=0
      TOPOCODE=0
      SITEPREP=0

      ColumnCount = fsql3_colcnt(IinDBref)
      DO ColNumber = 0,ColumnCount-1
        MATCHED=.FALSE.
        iRet = fsql3_colname(IinDBref,ColNumber,ColName,MxCname)
        ColName((iRet+1):) = '' 
      
C       BIND COLUMNS TO THEIR VARIABLES
      
        SELECT CASE(ColName)
      
         CASE('PLOT_ID')
           IPLOT = fsql3_colint(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
         CASE('TREE_ID')
           ITREE = fsql3_colint(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('TREE_COUNT')
           RCOUNT = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('HISTORY')
           HISTORY = fsql3_colint(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('SPECIES')
            iRet = fsql3_coltext(IinDBref,ColNumber,
     -        SPECIES,LEN(SPECIES),CHAR(0))
            SPECIES(iRet+1:) = ' '
            SPECIES = ADJUSTL(SPECIES)
C     
C            CULL OUT FIA CODES THAT MAY HAVE THEIR LEADING 0 
C            TRIMMED FOR SOME REASON. 
C            ASSUME THAT ANY 2 CHARACTER FIA CODE NEEDS A LEADING 0
C     
            IF(LEN_TRIM(SPECIES).LE.2)THEN
              IF((ICHAR(SPECIES(1:1)).GE.48).AND.
     &           (ICHAR(SPECIES(1:1)).LE.57).AND.
     &           (ICHAR(SPECIES(2:2)).GE.48).AND.
     &           (ICHAR(SPECIES(2:2)).LE.57)) THEN
                SPECIES='0'//SPECIES(:LEN(SPECIES)-1)
              ENDIF
            ENDIF
C     
            MATCHED=.TRUE.
      
          CASE('DBH')
           DBH = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('DIAMETER')
           DIAMETER = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('DG')
           DG = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('HT')
           HT = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('HTG')
           HTG = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('HTTOPK')
           HTTOPK = fsql3_colreal(IinDBref,ColNumber,0)
           MATCHED=.TRUE.
      
          CASE('CRRATIO')
            CRWNR = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('DAMAGE1')
            DMG1 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('DAMAGE2')
            DMG2 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('DAMAGE3')
            DMG3 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('SEVERITY1')
            SVR1 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('SEVERITY2')
            SVR2 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('SEVERITY3')
            SVR3 = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('TREEVALUE')
            TREEVAL = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('PRESCRIPTION')
            PRESCRIPT = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('SLOPE')
            SLOPE = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('ASPECT')
            ASPECT = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('HABITAT','PV_CODE')
            HABITAT = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('TOPOCODE')
            TOPOCODE = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('SITEPREP')
            SITEPREP = fsql3_colint(IinDBref,ColNumber,0)
            MATCHED=.TRUE.
      
          CASE('AGE')
            IF (fsql3_colisnull(IinDBref,ColNumber) .eq. 0) THEN
              ABIRTH = fsql3_colreal(IinDBref,ColNumber,0)
              LBIRTH  =.TRUE.    
            ENDIF
            MATCHED=.TRUE.
      
         END SELECT 
         
         IF (ITREEIN.EQ.1) THEN              
            IF (MATCHED) THEN
              IF(LKECHO)WRITE(JOSTND,4) TRIM(ColName),'USED'
    4         FORMAT (T12,A,T30,' WAS ',A)
           ELSE
              IF(LKECHO)WRITE(JOSTND,4) TRIM(ColName),'IGNORED'
           ENDIF
         ENDIF            
      ENDDO
      IF (DBH.EQ.0) DBH=DIAMETER

C----------
C  TREES WITH HISTORY CODES 6,7 ARE RECENT DEAD (GET IMC()=7)
C  TREES WITH HISTORY CODES 8,9 ARE OLDER  DEAD (GET IMC()=9)
C----------
      IF((HISTORY.GE.6).AND.(HISTORY.LE.9))THEN
        TREEVAL=7
        IF((HISTORY.EQ.8).OR.(HISTORY.EQ.9))TREEVAL=9
      ENDIF
      
      ITREEIN = 2
      RETURN
      END
