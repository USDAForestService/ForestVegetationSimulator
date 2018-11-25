      SUBROUTINE DBSTRLS(IWHO,KODE,TEM)
      IMPLICIT NONE
C----------
C DBSQLITE $Id$
C----------
C     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE
C
C     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
C                     INORDER FOR US TO CONTINUE
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO IT
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
      CHARACTER*8 TID,CSPECIES
      CHARACTER*17 TBLNAME
      CHARACTER*5 NTCUFT,NMCUFT,NBDFT
      CHARACTER*2000 SQLStmtStr
      INTEGER IWHO,I,IP,ITPLAB,iRet,IDMR,ICDF,IBDF,IPTBAL,KODE
      INTEGER ISPC,I1,I2,I3,ColNumber
      INTEGER IDCMP1,IDCMP2,ITRNK
      DATA IDCMP1,IDCMP2/10000000,20000000/
      REAL TEM
      REAL*8 CW,P,DGI,DP,ESTHT,TREAGE,DDBH,DHT,DHTG,DPCT,
     >       DCFV,DWK1,DBFV,DHT2TD2,DHT2TD1
    
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text,fsql3_reset
     
C     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
C     THEN JUST RETURN

      IF(ITREELIST.EQ.0.OR.IWHO.NE.1) RETURN

C     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

      IF(ITREELIST.EQ.2) KODE = 0

      CALL DBSCASE(1)
      
C     For CS, LS, NE and SN, the table name is FVS_TreeList_East and the following
C     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

      IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN') THEN
        TBLNAME = 'FVS_TreeList_East'
        NTCUFT  = 'MCuFt'
        NMCUFT  = 'SCuFt'
        NBDFT   = 'SBdFt'
      ELSE
        TBLNAME = 'FVS_TreeList'
        NTCUFT  = 'TCuFt'
        NMCUFT  = 'MCuFt'
        NBDFT   = 'BdFt'
      ENDIF

      iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
      iRet = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE ' // TRIM(TBLNAME) //
     -             ' (CaseID text,'//
     -             'StandID text,'//
     -             'Year int null,'//
     -             'PrdLen int null,'//
     -             'TreeId text null,'//
     -             'TreeIndex int null,'//
     -             'Species text null,'//
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
     -             'Ht2TDCF real null,'//
     -             'Ht2TDBF real null,'//
     -             'TreeAge real null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr) 
         IF (iRet .NE. 0) THEN
           ITREELIST = 0
           RETURN
         ENDIF
      ENDIF
      WRITE(SQLStmtStr,*)'INSERT INTO ',TBLNAME,
     -  ' (CaseID,StandID,Year,PrdLen,',
     -  'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',
     -  'MortPA,DBH,DG,',
     -  'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',NTCUFT,',',  
     -  NMCUFT,',',NBDFT,',MDefect,BDefect,TruncHt,',
     -  'EstHt,ActPt,Ht2TDCF,Ht2TDBF,TreeAge) VALUES (''',
     -  CASEID,''',''',TRIM(NPLT),''',',IY(ICYC+1),',',IFINT,
     -  ',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
        ITREELIST = 0
        RETURN
      ENDIF

C     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.

      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            I=IND1(I3)
         
            IP=ITRN
            ITPLAB=1
            P = PROB(I) / GROSPC
            IF (ICYC.GT.0) THEN
              DP = WK2(I)/ GROSPC
            ELSE                          
              DP = 0.0
            ENDIF

C           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C           GENERATED THROUGH THE ESTAB SYSTEM.

            IF (IDTREE(I) .GT. IDCMP1) THEN
              IF (IDTREE(I) .GT. IDCMP2) THEN
                WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              ELSE
                WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              ENDIF
            ELSE
              WRITE(TID,'(I8)') IDTREE(I)
              TID=ADJUSTL(TID)
            ENDIF

C           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

            CALL MISGET(I,IDMR)

C           SET CROWN WIDTH.

            CW=CRWDTH(I)

C           DECODE DEFECT AND ROUND OFF POINT BAL.

            ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
            IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
            IPTBAL=NINT(PTBALT(I))

C           DETERMINE ESTIMATED HEIGHT
C           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
C           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

            IF (NORMHT(I) .NE. 0) THEN
              ESTHT = (REAL(NORMHT(I))+5)/100
            ELSE
              ESTHT = HT(I)
            ENDIF
            
C           DETERMINE TREE AGE

            IF (LBIRTH(I)) THEN
              TREAGE = ABIRTH(I)
            ELSE
              TREAGE = 0
            ENDIF
 
C           GET DG INPUT

            DGI=DG(I)
            IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

C           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C           KEYWORD OVER RIDES

            IF(JSPIN(ISP(I)).EQ.1)THEN
              CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
            ELSEIF(JSPIN(ISP(I)).EQ.2)THEN
              CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
            ELSEIF(JSPIN(ISP(I)).EQ.3)THEN
              CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
            ELSE
              CSPECIES=ADJUSTL(PLNJSP(ISP(I)))
            ENDIF
C
            IF(ISPOUT6.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
            IF(ISPOUT6.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
            IF(ISPOUT6.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I)))) 
            
            ColNumber=1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,TID)            
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIES,
     >                             LEN_TRIM(CSPECIES))
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I))
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,P)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DP)
            DDBH=DBH(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DGI)
            DHT=HT(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT)
            DHTG=HTG(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DHTG)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,CW)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)
            DPCT = PCT(I)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCT)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)
            ColNumber=ColNumber+1
            DCFV = CFV(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV)
            ColNumber=ColNumber+1
            DWK1 = WK1(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DWK1)
            ColNumber=ColNumber+1
            DBFV = BFV(I)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DBFV)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
            DHT2TD2 = HT2TD(I,2)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD2)
            ColNumber=ColNumber+1
            DHT2TD1 = HT2TD(I,2)
            iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD1)
            ColNumber=ColNumber+1
            iRet = fsql3_bind_double(IoutDBref,ColNumber,TREAGE)
            
            iRet = fsql3_step(IoutDBref)
            iRet = fsql3_reset(IoutDBref)
          ENDDO
        ENDIF
      ENDDO

C     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
C     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
C
      IF (ITREELIST .EQ. 0) RETURN
      IF(.NOT.((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR.
     >         (ICYC.GE.1))) THEN
        iRet = fsql3_finalize(IoutDBref)
        RETURN
      ENDIF
      
      DO I=IREC2,MAXTRE
        P =(PROB(I) / GROSPC) / (FINT/FINTM)
        WRITE(TID,'(I8)') IDTREE(I)
        TID=ADJUSTL(TID)

C       GET MISTLETOE RATING FOR CURRENT TREE RECORD.
        CALL MISGET(I,IDMR)

C       SET CROWN WIDTH.
        CW=CRWDTH(I)

C       DECODE DEFECT AND ROUND OFF POINT BAL.

        ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
        IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
        IPTBAL=NINT(PTBALT(I))

C       DETERMINE TREE AGE

        IF (LBIRTH(I)) THEN
          TREAGE = ABIRTH(I)
        ELSE
          TREAGE = 0
        ENDIF        

C       CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.

        DGI=DG(I)
        IF(ICYC.EQ.0 .AND. TEM.EQ.0) DGI=WORK1(I)

C       PUT PROB IN MORTALITY COLUMN
        DP = P
        P = 0.

C       DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C       KEYWORD OVER RIDES

        IF(JSPIN(ISP(I)).EQ.1)THEN
          CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
        ELSEIF(JSPIN(ISP(I)).EQ.2)THEN
          CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
        ELSEIF(JSPIN(ISP(I)).EQ.3)THEN
          CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
        ELSE
          CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
        ENDIF
C       
        IF(ISPOUT6.EQ.1)CSPECIES=ADJUSTL(TRIM(JSP(ISP(I))))
        IF(ISPOUT6.EQ.2)CSPECIES=ADJUSTL(TRIM(FIAJSP(ISP(I))))
        IF(ISPOUT6.EQ.3)CSPECIES=ADJUSTL(TRIM(PLNJSP(ISP(I))))
        
        ColNumber=1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,           
     >                         LEN_TRIM(TID))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,I)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIES,
     >                         LEN_TRIM(CSPECIES))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,P)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DP)
        DDBH=DBH(I)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DGI)
        DHT=HT(I)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT)
        DHTG=HTG(I)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHTG)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,CW)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)
        DPCT = PCT(I)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)
        ColNumber=ColNumber+1
        DCFV = CFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV)
        ColNumber=ColNumber+1
        DWK1 = WK1(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DWK1)
        ColNumber=ColNumber+1
        DBFV = BFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DBFV)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
        DHT2TD2 = HT2TD(I,2)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD2)
        ColNumber=ColNumber+1
        DHT2TD1 = HT2TD(I,2)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD1)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,TREAGE)
        
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref)
      ENDDO
      iRet = fsql3_finalize(IoutDBref)
      iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

      RETURN
      END
