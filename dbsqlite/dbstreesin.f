      SUBROUTINE DBSTREESIN(IPLOT,ITREE,RCOUNT,HISTORY,SPECIES,DBH,DG,
     -    HT,HTTOPK,HTG,CRWNR,DMG1,SVR1,DMG2,SVR2,DMG3,SVR3,TREEVAL,
     -    PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,SITEPREP,KODE,DEBUG,
     -    JOSTND,LKECHO,ABIRTH,LBIRTH)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     NOTE: ALL OF THE VARIABLES IN THIS ROUTINE ARE VOLITILE. THEY GET RESET
C           AT EVERY CALL. BE CAREFUL.
C---
COMMONS
C
      INCLUDE  'DBSCOM.F77'
C
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
      DIAMETER=0
      DBH=0

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
