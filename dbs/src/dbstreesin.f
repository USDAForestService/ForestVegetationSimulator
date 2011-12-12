      SUBROUTINE DBSTREESIN(IPLOT,ITREE,RCOUNT,HISTORY,SPECIES,DBH,DG,
     -    HT,HTTOPK,HTG,CRWNR,DMG1,SVR1,DMG2,SVR2,DMG3,SVR3,TREEVAL,
     -    PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,SITEPREP,KODE,DEBUG,
     -    JOSTND,LKECHO,ABIRTH)
      IMPLICIT NONE
C
C  **DBSTREESIN--DBS  DATE OF LAST REVISION:  10/31/2011
C
C     NOTE: ALL OF THE VARIABLE IN THIS ROUTINE ARE VOLITILE. THE GET RESET
C           AT EVERY CALL. BE CAREFUL.
C     DBSTREESIN
C     PURPOSE: TO PARSE THE TREE RECORD SET FOR TREEDATA AND RETURN THE RECORD
C         IN THE EXPECTED FORMAT FOR TREESIN
C     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
C     OVERHAUL: NL CROOKSTON  -- RMRS -- SEPTEMBER 2004
C---
COMMONS
C
      INCLUDE  'DBSCOM.F77'
C
COMMONS
      
      CHARACTER*30 ColName
      INTEGER ITREE,IPLOT,HISTORY,CRWNR,DMG1,DMG2,DMG3,SVR1,SVR2
      INTEGER SVR3,TREEVAL,PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,KODE
      INTEGER SITEPREP,JOSTND
      INTEGER(SQLINTEGER_KIND)::PlotId_LI,TreeId_LI,Count_LI,History_LI,
     -    DBH_LI,DG_LI,HT_LI,HTG_LI,HTTOPK_LI,CrwnR_LI,Dmg1_LI,Dmg2_LI,
     -    Dmg3_LI,Svr1_LI,Svr2_LI,Svr3_LI,TreeVal_LI,Prescript_LI,
     -    Slope_LI,Aspect_LI,Habitat_LI,Topo_LI,SitePrep_LI,Species_LI,
     -    Abirth_LI     
      REAL DBH,DG,HT,HTG,HTTOPK,RCOUNT,ABIRTH,XXX
      CHARACTER(LEN=*) SPECIES
      INTEGER I
      LOGICAL DEBUG,MATCHED,LKECHO
      INTEGER(SQLSMALLINT_KIND)::ColNumber,NameLen,ColumnCount
      INTEGER(SQLLEN_KIND)::tmpNotUsed

C     CHECK TO MAKE SURE WE ARE SUPPSOSE TO BE PULLING TREE INFO FROM DATABASE

      IF (ITREEIN.EQ.0) THEN
        KODE = 0
        RETURN
      ENDIF

C     RETRIEVE COLUMN HEADINGS RETURNED IF WE HAVEN'T ALREADY DONE SO

      IF (ITREEIN.NE.2) THEN
        ITREEIN = 2
        iRet = fvsSQLNumResultCols(StmtHndlTree,ColumnCount)
        IF(LKECHO)WRITE (JOSTND,'(/T13,''COLUMN PROCESSING RESULTS:'')')
        DO ColNumber = 1,ColumnCount
           MATCHED=.FALSE.

C          GET COLUMN NAME

           iRet = fvsSQLColAttribute(StmtHndlTree, ColNumber,
     -                     SQL_DESC_NAME, ColName,
     -                     int(LEN(ColName),SQLUSMALLINT_KIND),
     -                     NameLen, tmpNotUsed)
           ColName(NameLen+1:)=' '

C          UPPER CASE THE COLUMN NAME FOR EASIER COMPARISONS

           DO I = 1, NameLen
             CALL UPCASE(ColName(I:I))
           END DO

C          BIND COLUMNS TO THEIR VARIABLES

           SELECT CASE(ColName(:NameLen))

            CASE('PLOT_ID')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(1),int(4,SQLLEN_KIND),VAL_LI(1))
               MATCHED=.TRUE.

            CASE('TREE_ID')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(2),int(4,SQLLEN_KIND),VAL_LI(2))     
               MATCHED=.TRUE.

             CASE('TREE_COUNT')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(3),int(4,SQLLEN_KIND),VAL_LI(3))
               MATCHED=.TRUE.

             CASE('HISTORY')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(4),int(4,SQLLEN_KIND),VAL_LI(4))
               MATCHED=.TRUE.

             CASE('SPECIES')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_CHAR,
     -           SPECIES,int(LEN(SPECIES),SQLLEN_KIND),VAL_LI(24))
               MATCHED=.TRUE.

             CASE('DBH','DIAMETER')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(5),int(4,SQLLEN_KIND),VAL_LI(5))
               MATCHED=.TRUE.

             CASE('DG')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(6),int(4,SQLLEN_KIND),VAL_LI(6))
               MATCHED=.TRUE.

             CASE('HT')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(7),int(4,SQLLEN_KIND),VAL_LI(7))
               MATCHED=.TRUE.

             CASE('HTG')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(8),int(4,SQLLEN_KIND),VAL_LI(8))
               MATCHED=.TRUE.

             CASE('HTTOPK')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(9),int(4,SQLLEN_KIND),VAL_LI(9))
               MATCHED=.TRUE.

             CASE('CRRATIO')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(10),int(4,SQLLEN_KIND),VAL_LI(10))
               MATCHED=.TRUE.

             CASE('DAMAGE1')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(11),int(4,SQLLEN_KIND),VAL_LI(11))
               MATCHED=.TRUE.

             CASE('DAMAGE2')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(12),int(4,SQLLEN_KIND),VAL_LI(12))
               MATCHED=.TRUE.

             CASE('DAMAGE3')
              iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(13),int(4,SQLLEN_KIND),VAL_LI(13))
               MATCHED=.TRUE.

             CASE('SEVERITY1')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(14),int(4,SQLLEN_KIND),VAL_LI(14))
               MATCHED=.TRUE.

             CASE('SEVERITY2')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(15),int(4,SQLLEN_KIND),VAL_LI(15))
               MATCHED=.TRUE.

             CASE('SEVERITY3')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(16),int(4,SQLLEN_KIND),VAL_LI(16))
               MATCHED=.TRUE.

             CASE('TREEVALUE')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(17),int(4,SQLLEN_KIND),VAL_LI(17))
               MATCHED=.TRUE.

             CASE('PRESCRIPTION')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(18),int(4,SQLLEN_KIND),VAL_LI(18))
               MATCHED=.TRUE.

             CASE('SLOPE')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(19),int(4,SQLLEN_KIND),VAL_LI(19))
               MATCHED=.TRUE.

             CASE('ASPECT')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(20),int(4,SQLLEN_KIND),VAL_LI(20))
               MATCHED=.TRUE.

             CASE('HABITAT','PV_CODE')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_CHAR,
     -           CTMP,int(LEN(CTMP),SQLLEN_KIND),VAL_LI(21))
               MATCHED=.TRUE.

             CASE('TOPOCODE')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(22),int(4,SQLLEN_KIND),VAL_LI(22))
               MATCHED=.TRUE.

             CASE('SITEPREP')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(23),int(4,SQLLEN_KIND),VAL_LI(23))
               MATCHED=.TRUE.

             CASE('AGE')
               iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,
     -           RTREEDATA(25),int(4,SQLLEN_KIND),VAL_LI(25))
               MATCHED=.TRUE.
            END SELECT

            IF (MATCHED) THEN
               IF(LKECHO)WRITE(JOSTND,4) ColName(1:15),'BOUND'
    4          FORMAT (T13,A,' WAS ',A)
            ELSE
               IF(LKECHO)WRITE(JOSTND,4) ColName(1:15),'IGNORED'
            ENDIF
         ENDDO
      ENDIF

C     INITIALIZE THE RESULT VECTOR.

      VAL_LI = SQL_NULL_DATA
      
C     FETCH DATA FROM DATABASE

      iRet = fvsSQLFetch(StmtHndlTree)
      IF (iRet.EQ.SQL_SUCCESS.OR.iRet
     -    .EQ.SQL_SUCCESS_WITH_INFO) THEN
          KODE = 1
      ELSE
          KODE = -1
      ENDIF

      IF (DEBUG) WRITE (JOSTND,5) iRet,KODE
    5 FORMAT (' FETCH RETURN=',I6,' KODE=',I3)

C     ASSIGN DATABASE VALUES TO VARIABLES
C     CATCH NULLS AND ASSIGN VALUES TO 0 INSTEAD

      IF (KODE.EQ.1) THEN
        IF(VAL_LI(5).EQ.SQL_NULL_DATA) THEN
          DBH     = 0
        ELSE
          DBH     = RTREEDATA(5)
        END IF
        IF(VAL_LI(6).EQ.SQL_NULL_DATA) THEN
          DG      = 0
        ELSE
          DG      = RTREEDATA(6)
        END IF
        IF(VAL_LI(7).EQ.SQL_NULL_DATA) THEN
          HT      = 0
        ELSE
          HT      = RTREEDATA(7)
        END IF
        IF(VAL_LI(8).EQ.SQL_NULL_DATA) THEN
          HTG     = 0
        ELSE
          HTG     = RTREEDATA(8)
        END IF
        IF(VAL_LI(9).EQ.SQL_NULL_DATA) THEN
          HTTOPK  = 0
        ELSE
          HTTOPK  = RTREEDATA(9)
        END IF
        IF(VAL_LI(1).EQ.SQL_NULL_DATA) THEN
          IPLOT   = 0
        ELSE
          IPLOT   = RTREEDATA(1)
        END IF
        IF(VAL_LI(2).EQ.SQL_NULL_DATA) THEN
          ITREE   = 0
        ELSE
          ITREE   = RTREEDATA(2)
        END IF
        IF(VAL_LI(3).EQ.SQL_NULL_DATA) THEN
          RCOUNT  = 0
        ELSE
          RCOUNT  = RTREEDATA(3)
        END IF
        IF(VAL_LI(4).EQ.SQL_NULL_DATA) THEN
          HISTORY = 0
        ELSE
          HISTORY = RTREEDATA(4)
        END IF
        IF(VAL_LI(10).EQ.SQL_NULL_DATA) THEN
          CRWNR   = 0
        ELSE
          CRWNR   = RTREEDATA(10)
        END IF
        IF(VAL_LI(11).EQ.SQL_NULL_DATA) THEN
          DMG1    = 0
        ELSE
          DMG1    = RTREEDATA(11)
        END IF
        IF(VAL_LI(12).EQ.SQL_NULL_DATA) THEN
          DMG2    = 0
        ELSE
          DMG2    = RTREEDATA(12)
        END IF
        IF(VAL_LI(13).EQ.SQL_NULL_DATA) THEN
          DMG3    = 0
        ELSE
          DMG3    = RTREEDATA(13)
        END IF
        IF(VAL_LI(14).EQ.SQL_NULL_DATA) THEN
          SVR1    = 0
        ELSE
          SVR1    = RTREEDATA(14)
        END IF
        IF(VAL_LI(15).EQ.SQL_NULL_DATA) THEN
          SVR2    = 0
        ELSE
          SVR2    = RTREEDATA(15)
        END IF
        IF(VAL_LI(16).EQ.SQL_NULL_DATA) THEN
          SVR3    = 0
        ELSE
          SVR3    = RTREEDATA(16)
        END IF
        IF(VAL_LI(17).EQ.SQL_NULL_DATA) THEN
          TREEVAL = 0
        ELSE
          TREEVAL = RTREEDATA(17)
        END IF
        IF(VAL_LI(18).EQ.SQL_NULL_DATA) THEN
          PRESCRIPT = 0
        ELSE
          PRESCRIPT = RTREEDATA(18)
        END IF
        IF(VAL_LI(19).EQ.SQL_NULL_DATA) THEN
          SLOPE   = 0
        ELSE
          SLOPE   = RTREEDATA(19)
        END IF
        IF(VAL_LI(20).EQ.SQL_NULL_DATA) THEN
          Aspect  = 0
        ELSE
          Aspect  = RTREEDATA(20)
        END IF

C       TODO:  (DELETE THIS COMMENT WHEN DONE!)
C       HABITAT IS A STRING IN THE DATABASE, BUT ONLY NUMBERS
C       ARE RECOGNIZED AT THE TREE LEVEL. A FUTURE UPDATE
C       IS NEEDED TO ALLOW FOR PV_CODES OF ALL TYPES AND
C       MAPPING OF THOSE CODES TO HABITATS RECOGNIZED BY FVS

        HABITAT = 0
        IF(VAL_LI(21).NE.SQL_NULL_DATA) THEN
          CTMP(VAL_LI(21)+1:)=' '
          READ (CTMP,*,ERR=10) HABITAT
          GOTO 20
   10     CONTINUE
          READ (CTMP,*,ERR=20) XXX
          HABITAT = IFIX(XXX)
   20     CONTINUE
        END IF
        IF(VAL_LI(22).EQ.SQL_NULL_DATA) THEN
          TOPOCODE = 0
        ELSE
          TOPOCODE = RTREEDATA(22)
        END IF
        IF(VAL_LI(23).EQ.SQL_NULL_DATA) THEN
          SITEPREP = 0
        ELSE
          SITEPREP = RTREEDATA(23)
        END IF
        IF(VAL_LI(24).EQ.SQL_NULL_DATA) THEN
           SPECIES = ' '
        ELSE
           SPECIES(VAL_LI(24)+1:)=' '
C
C  CULL OUT FIA CODES THAT MAY HAVE THEIR LEADING 0 TRIMMED FOR SOME
C  REASON. ASSUME THAT ANY 2 CHARACTER FIA CODE NEEDS A LEADING 0 
C
           IF(LEN_TRIM(SPECIES).LE.2)THEN
              IF((ICHAR(ADJUSTL(SPECIES(1:1))).GE.48).AND.
     &        (ICHAR(ADJUSTL(SPECIES(1:1))).LE.57).AND.
     &        (ICHAR(ADJUSTL(SPECIES(2:2))).GE.48).AND.
     &        (ICHAR(ADJUSTL(SPECIES(2:2))).LE.57))THEN
                 SPECIES='0'//ADJUSTL(SPECIES)
              ENDIF
           ENDIF
C
           SPECIES = ADJUSTL(SPECIES)
        ENDIF
        IF(VAL_LI(25).EQ.SQL_NULL_DATA) THEN
          ABIRTH  = 0
        ELSE
          ABIRTH  = RTREEDATA(25)
        END IF          
C----------
C  TREES WITH HISTORY CODES 6,7 ARE RECENT DEAD (GET IMC()=7)
C  TREES WITH HISTORY CODES 8,9 ARE OLDER  DEAD (GET IMC()=9)
C----------
        IF((HISTORY.GE.6).AND.(HISTORY.LE.9))THEN
          TREEVAL=7
          IF((HISTORY.EQ.8).OR.(HISTORY.EQ.9))TREEVAL=9
        ENDIF
      ENDIF

C     RELEASE STMT HANDLE WHEN NECESSARY

      IF (KODE.NE.1) THEN
       iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlTree)
      ENDIF

      END
