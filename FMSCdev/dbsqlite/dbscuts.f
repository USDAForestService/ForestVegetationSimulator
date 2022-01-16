      SUBROUTINE DBSCUTS(IWHO,KODE)
C
C DBSQLITE $Id$
C
C     PURPOSE: TO OUTPUT THE CUTS LIST DATA TO THE DATABASE
C
C     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 2
C                     INORDER FOR US TO CONTINUE
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO
C
      IMPLICIT NONE
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ESTREE.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C
      CHARACTER*8 TID,CSPECIE1,CSPECIE2,CSPECIE3
      CHARACTER*17 TBLNAME
      CHARACTER*5 NTCUFT,NMCUFT,NBDFT
      CHARACTER*8 NAMDCF,NAMDBF
      CHARACTER*2000 SQLStmtStr
      INTEGER IWHO,I,JYR,IP,ITPLAB,IRCODE,IDMR,ICDF,IBDF,IPTBAL,KODE
      INTEGER ISPC,I1,I2,I3
      INTEGER*4 IDCMP1,IDCMP2
      DATA IDCMP1,IDCMP2/10000000,20000000/
      REAL CW,P,DGI,DP,ESTHT,TREAGE

      INTEGER fsql3_tableexists,fsql3_exec
C---------
C     IF CUTSOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 2
C     THEN JUST RETURN
C---------
      IF(ICUTLIST.EQ.0.OR.IWHO.NE.2) RETURN
C---------
C     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0
C---------
      IF(ICUTLIST.EQ.2) KODE = 0
C---------
C     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER
C---------
      CALL DBSCASE(1)
      
C     For CS, LS, NE and SN, the table name is FVS_TreeList_East and the following
C     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

      IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR.
     >    VARACD.EQ.'NE') THEN
        TBLNAME = 'FVS_CutList_East'
        NTCUFT  = 'MCuFt'
        NMCUFT  = 'SCuFt'
        NBDFT   = 'SBdFt'
        NAMDCF  = 'Ht2TDMCF'
        NAMDBF  = 'Ht2TDSCF'
      ELSE
        TBLNAME = 'FVS_CutList'
        NTCUFT  = 'TCuFt'
        NMCUFT  = 'MCuFt'
        NBDFT   = 'BdFt'
        NAMDCF  = 'Ht2TDCF '
        NAMDBF  = 'Ht2TDBF '
      ENDIF

C     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
C     IF IT DOESNT THEN WE NEED TO CREATE IT

      IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))
      IF (IRCODE .NE. 0) THEN
        ICUTLIST = 0
        RETURN
      ENDIF
      IRCODE = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
      IF(IRCODE.EQ.0) THEN
          SQLStmtStr='CREATE TABLE ' // TRIM(TBLNAME) //
     -             ' (CaseID text not null,'//
     -             'StandID text not null,'//
     -             'Year int null,'//
     -             'PrdLen int null,'//
     -             'TreeId text null,'//
     -             'TreeIndex int null,'//
     -             'SpeciesFVS text null,'//
     -             'SpeciesPLANTS text null,'//
     -             'SpeciesFIA text null,'//
     -             'TreeVal int null,'//
     -             'SSCD int null,'//
     -             'PtIndex int null,'//
     -             'TPA real null,'//
     -             'MortPA real null,'//
     -             'DBH real null,'//
     -             'DG real null,'//
     -             'Ht real null,'//
     -             'HtG real null,'//
     -             'PctCr int null,'//
     -             'CrWidth real null,'//
     -             'MistCD int null,'//
     -             'BAPctile real null,'//
     -             'PtBAL real null,'//
     -             NTCUFT // ' real null,'//
     -             NMCUFT // ' real null,'//
     -             NBDFT  // ' real null,'//
     -             'MDefect int null,'//
     -             'BDefect int null,'//
     -             'TruncHt int null,'//
     -             'EstHt real null,'//
     -             'ActPt int null,'//
     -             NAMDCF // ' real null,'//
     -             NAMDBF // ' real null,'//
     -             'TreeAge real null);'// CHAR(0)
        IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (IRCODE .NE. 0) THEN
          ICUTLIST = 0
          RETURN
        ENDIF
      ENDIF
C---------
C     SET THE CUTS LIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.
C---------
      JYR=IY(ICYC)
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            IP=0
            DO I=1,ITRN
              IF (WK3(I).GT.0) IP=IP+1
            ENDDO
            I=IND1(I3)
            ITPLAB=3
            P = WK3(I)/GROSPC
            DP = 0.0
C           SKIP OUTPUT IF P <= 0
            IF (P.LE.0.0) CYCLE

C----------
C           TRANSLATE TREES IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C           GENERATED THROUGH THE ESTAB SYSTEM.
C----------
            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
            ENDIF
C----------
C           GET MISTLETOE RATING FOR CURRENT TREE RECORD.
C----------
            CALL MISGET(I,IDMR)
C----------
C           SET CROWN WIDTH.
C----------
            CW=CRWDTH(I)
C----------
C           DECODE DEFECT AND ROUND OFF POINT BAL.
C----------
            ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
            IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
            IPTBAL=NINT(PTBALT(I))
C           DETERMINE ESTIMATED HEIGHT
C           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HAS NOT
C           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT
            IF (NORMHT(I) .NE. 0) THEN
              ESTHT = (REAL(NORMHT(I))+5)/100
            ELSE
              ESTHT = HT(I)
            ENDIF
C----------            
C           DETERMINE TREE AGE
C----------
            IF (LBIRTH(I)) THEN
              TREAGE = ABIRTH(I)
            ELSE
              TREAGE = 0
            ENDIF
C----------
C           GET DG INPUT
C----------
            DGI=DG(I)

C           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

            CSPECIE1 = JSP(ISP(I))
            CSPECIE2 = PLNJSP(ISP(I))
            CSPECIE3 = FIAJSP(ISP(I))

            WRITE(SQLStmtStr,*)'INSERT INTO ',TBLNAME,
     -        ' (CaseID,StandID,Year,PrdLen,',
     -        'TreeId,TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',
     -        'TreeVal,SSCD,PtIndex,TPA,',
     -        'MortPA,DBH,DG,HT,HTG,PctCr,',
     -        'CrWidth,MistCD,BAPctile,PtBAL,',NTCUFT,',', 
     -        NMCUFT,',',NBDFT,',MDefect,BDefect,TruncHt,',
     -      'EstHt,ActPt,',NAMDCF,',',NAMDBF,',','TreeAge) VALUES (''',
     -        CASEID,''',''',TRIM(NPLT),''',',JYR,',',
     -        IFINT,",'",TRIM(ADJUSTL(TID)),"',",I,
     -        ",'",TRIM(CSPECIE1),"'",
     -        ",'",TRIM(CSPECIE2),"'",
     -        ",'",TRIM(CSPECIE3),"',",
     -        IMC(I),',',ISPECL(I),',',ITRE(I),
     -        ',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),
     -        ',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',
     -        CFV(I),',',WK1(I),',',BFV(I),',',ICDF,',',IBDF,',',
     -        ((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),
     -        ',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,');'

           IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
           IF (IRCODE .NE. 0) THEN
             ICUTLIST = 0
             RETURN
           ENDIF
          ENDDO
        ENDIF
      ENDDO
      IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
      RETURN
      END
