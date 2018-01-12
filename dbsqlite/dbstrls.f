      SUBROUTINE DBSTRLS(IWHO,KODE,TEM)
      IMPLICIT NONE
C----------
C $Id$
C----------
C     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE
C
C     AUTH: D. GAMMEL -- SEM -- JULY 2002
C
C     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
C                     INORDER FOR US TO CONTINUE
C            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
C                     REDIRECT OF THE FLAT FILE REPORT OR IN
C                     ADDITION TO
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
      CHARACTER*8 TID,CSPECIES
      CHARACTER*2000 SQLStmtStr
      INTEGER IWHO,I,JYR,IP,ITPLAB,iRet,IDMR,ICDF,IBDF,IPTBAL,KODE
      INTEGER ISPC,I1,I2,I3
      INTEGER*4 IDCMP1,IDCMP2
      DATA IDCMP1,IDCMP2/10000000,20000000/
      REAL CW,P,DGI,DP,TEM,ESTHT,TREAGE
      
      integer fsql3_tableexists,fsql3_exec      

C     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
C     THEN JUST RETURN

      IF(ITREELIST.EQ.0.OR.IWHO.NE.1) RETURN

C     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

      IF(ITREELIST.EQ.2) KODE = 0

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_TreeList"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_TreeList('//
     -             'CaseID char(36),'//
     -             'StandID char(26),'//
     -             'Year int null,'//
     -             'PrdLen int null,'//
     -             'TreeId char(8) null,'//
     -             'TreeIndex int null,'//
     -             'Species char(8) null,'//
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
     -             'TCuFt real null,'//
     -             'MCuFt real null,'//
     -             'BdFt real null,'//
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

C     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.

      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.NE.0) THEN
          I2=ISCT(ISPC,2)
          DO I3=I1,I2
            I=IND1(I3)

            JYR=IY(ICYC+1)
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

C
C           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C           KEYWORD OVER RIDES
C
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

            WRITE(SQLStmtStr,*)'INSERT INTO FVS_TreeList',
     -           ' (CaseID,StandID,Year,PrdLen,',
     -           'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',
     -           'MortPA,DBH,DG,',
     -           'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,TCuFt,',
     -           'MCuFt,BdFt,MDefect,BDefect,TruncHt,',
     -           'EstHt,ActPt,Ht2TDCF,Ht2TDBF,TreeAge) VALUES(''',
     -           CASEID,''',''',TRIM(NPLT),
     -           ''',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,",'",
     -           trim(CSPECIES),"',",IMC(I),',',ISPECL(I),',',ITRE(I),
     -           ',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),
     -           ',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',
     -           CFV(I),',',WK1(I),',',BFV(I),',',ICDF,',',IBDF,',',
     -           ((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),
     -           ',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,');'
            iRet = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
            IF (iRet .NE. 0) THEN
              ITREELIST = 0
              RETURN
            ENDIF
          ENDDO
        ENDIF
      ENDDO

C     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
C     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
C
      IF (ITREELIST .EQ. 0) RETURN
      IF((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR.(ICYC.GE.1)) RETURN
      DO I=IREC2,MAXTRE
        P =(PROB(I) / GROSPC) / (FINT/FINTM)
        WRITE(TID,'(I8)') IDTREE(I)

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

C
C       DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C       KEYWORD OVER RIDES
C
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
        
        WRITE(SQLStmtStr,*)'INSERT INTO FVS_TreeList',
     -       ' (CaseID,StandID,Year,PrdLen,',
     -       'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',
     -       'MortPA,DBH,DG,',
     -       'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,TCuFt,',
     -       'MCuFt,BdFt,MDefect,BDefect,TruncHt,',
     -       'EstHt,ActPt,Ht2TDCF,Ht2TDBF,TreeAge) VALUES(''',
     -       CASEID,''',''',TRIM(NPLT),
     -       ''',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,
     -       ",'",trim(CSPECIES),"',",IMC(I),',',ISPECL(I),',',ITRE(I),
     -       ',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),
     -       ',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',
     -       CFV(I),',',WK1(I),',',BFV(I),',',ICDF,',',IBDF,',',
     -       ((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),
     -       ',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,');'
          iRet = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
          IF (iRet .NE. 0) THEN
            ITREELIST = 0
            RETURN
          ENDIF
      ENDDO
      RETURN
      END
