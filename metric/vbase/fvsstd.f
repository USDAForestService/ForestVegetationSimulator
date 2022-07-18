      SUBROUTINE FVSSTD (IWHO)
      IMPLICIT NONE
C----------
C METRIC-VBASE $Id$
C----------
C
C     CREATE A FILE FOR FVSTAND POST-PROCESSOR INPUT.
C
C     IWHO=1 IF CALLED FROM MAIN, IWHO=2 IF CALLED FROM CUTS
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
      INCLUDE 'HTCAL.F77'
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
      INCLUDE 'STDSTK.F77'
C
C
      INCLUDE 'SUMTAB.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'FVSSTDCM.F77'
C
C
      INCLUDE 'METRIC.F77'
C
C
COMMONS
C----------
      INTEGER IWHO,MYACT(1),KSDI,IOPEN,I,NTODO,ITODO,NPRMS,IACTK,IDT
      INTEGER KODE,ISTLNB,JYR,IP,ITPLAB,ITMFOR,IBA,IKNT,ISITE
      INTEGER KAGE,ISPC,I1,I2,I3,IICR,IDMR,ICDF,IBDF,IPTBAL
      REAL TEM(6),TEMSDI,STAGEA,STAGEB,XXWT,TEMBA,P,DP,CW,DGI
      REAL PDBHI,BRATIO,PHTI,DUMSDI
      DIMENSION CLAB1(2)
      CHARACTER CISN*11,TIM*8,DAT*10,TID*8,CLAB1*4
      CHARACTER REV*10
      INTEGER*4 IDCMP1,IDCMP2
      DATA MYACT/204/
      DATA IDCMP1,IDCMP2/10000000,20000000/
      DATA CLAB1/'TREE','CUT '/
C----------
C  COMPUTE CURRENT SDI FOR PRINTING
C----------
      TEMSDI=0.
      CALL SDICLS(0,0.,999.,1,TEMSDI,DUMSDI,STAGEA,STAGEB,0)
      KSDI=INT((TEMSDI/ACRtoHA)/GROSPC + 0.5)
C----------
C  SET OLD VOLUME VARIABLES TO ZERO IF THIS IS CYCLE 0
C----------
      IF (LSTART) THEN
        IOPEN=0
        DO 10 I=1,ITRN
        PTOCFV(I)=0.
        PMRCFV(I)=0.
        PMRBFV(I)=0.
        NCFDEF(I)=0
        NBFDEF(I)=0
   10   CONTINUE
      ENDIF
C----------
C  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT
C----------
      CISN = '           '
C----------
C  FIND OUT IF THERE IS A FVSSTAND OPTION.
C----------
      CALL OPFIND (1,MYACT(1),NTODO)
C----------
C  IF THERE ARE NONE TO DO, RETURN
C----------
      IF (NTODO.EQ.0) GO TO 220
C----------
C  ELSE: DO THE OPTIONS PRESENT.
C----------
      DO 200 ITODO=1,NTODO
      CALL OPGET (ITODO,2,IDT,IACTK,NPRMS,TEM)
      IF (IACTK.LT.0) GOTO 200
C----------
C  IF THE OUTPUT FILE IS NOT OPEN, OPEN IT.
C----------
      IF(.NOT.FSTOPEN) THEN
        CALL MYOPEN(KOLIST,KWDFIL(1:ISTLNB(KWDFIL))//'.fst',
     >              5,170,0,1,1,0,KODE)
        IF(KODE .GT. 0) THEN
          WRITE(JOSTND,*) ' FVSSTAND FILE DID NOT OPEN'
          CALL RCDSET (2,.TRUE.)
          GO TO 200
        ENDIF
        FSTOPEN=.TRUE.
      ENDIF
C----------
C  LET IP BE THE RECORD OUTPUT COUNT
C  SET THE OUTPUT REPORTING YEAR.
C----------
      IF(IWHO .EQ. 1) THEN
        JYR=IY(ICYC+1)
        IP=ITRN
        ITPLAB=1
      ELSE
        JYR=IY(ICYC)
        IP=0
        DO 1 I=1,ITRN
        IF(WK3(I) .GT. 0.) IP=IP+1
    1   CONTINUE
        ITPLAB=2
      ENDIF
C----------
C  IF AT THE PRE-PROJECTION TIME, FIND OUT IF A FVSSTAND HAS BEEN
C  SUPPRESSED. IF YES, BRANCH TO NOT ACCOMPLISH THE REQUEST.
C  ONLY MARK THE OPTION DONE IF THIS IS CALLED FROM MAIN.
C----------
      IF (LSTART) THEN
        IF (NPRMS.GE.1 .AND. TEM(1).EQ.1.) GOTO 200
        IF(NPRMS.GE.1 .AND. TEM(1).EQ.2)CALL OPDONE(ITODO,JYR)
      ELSE
        IF (NPRMS.GE.1 .AND. TEM(1).EQ.2.) GOTO 200
        IF(IWHO .EQ. 1)THEN
          CALL OPDONE (ITODO,JYR-1)
        ELSE
C         CALL OPDONE(ITODO,JYR)
        ENDIF
      ENDIF
C----------
C  WRITE A MARKER AND THE NUMBER OF RECORDS WHICH FOLLOW.
C----------
      XXWT=SAMWT
      CALL REVISE (VARACD,REV)
      CALL GRDTIM (DAT,TIM)
      SELECT CASE (VARACD)
        CASE ('KT')
          ITMFOR=KODFOR/100000
        CASE DEFAULT
          ITMFOR=KODFOR
      END SELECT
      IBA=NINT((BA*FT2pACRtoM2pHA)/GROSPC)
      IF(IWHO.NE.1)THEN
        TEMBA=0.
        DO 40 I=1,ITRN
        TEMBA=TEMBA+0.0054542*DBH(I)*DBH(I)*PROB(I)
   40   CONTINUE
        IBA=NINT((TEMBA*FT2pACRtoM2pHA)/GROSPC)
      ENDIF
      IKNT=ICYC+1
      ISITE=IFIX(SITEAR(ISISP)*FTtoM)
      IF(LSTART) THEN
        KAGE=IAGE
      ELSE
        IF(IWHO .EQ. 1) THEN
          KAGE=IAGE + IY(IKNT) - IY(1)
        ELSE
          KAGE=IAGE + IY(ICYC) - IY(1)
        ENDIF
      ENDIF
      WRITE (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,
     &                 CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN,
     & KAGE,ITMFOR,FIAJSP(ISISP),ISITE,IPTINV,IBA,KSDI
    2 FORMAT (' -999',3I5,3(1X,A),5X,3(1X,A),I3,E14.7,2(1X,A),
     & 5X,I4,12X,I9,1X,A3,I5,I2,I3,I5)
C----------
C  PROCESS TREELIST
C----------
      DO 60 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 60
      I2=ISCT(ISPC,2)
      DO 50 I3=I1,I2
      I=IND1(I3)
      IF(ITPLAB .EQ. 1) THEN
        P = PROB(I) / GROSPC
        IF(ICYC.GT.0) THEN
          DP = WK2(I)/ GROSPC
        ELSE
          DP = 0.0
        ENDIF
      ELSE
        P = WK3(I)/GROSPC
        DP = 0.
        IF(P .LE. 0.) GO TO 50
      ENDIF
      IICR=((ICR(I)-1)/10)+1
      IF (IICR.GT.9) IICR=9
C----------
C   TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
C   GENERATED THROUGH THE ESTAB SYSTEM.
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
C  GET MISTLETOE RATING FOR CURRENT TREE RECORD.
C----------
      CALL MISGET(I,IDMR)
C----------
C  SET CROWN WIDTH.
C----------
      CW=CRWDTH(I)
C----------
C  DECODE DEFECT AND ROUND OFF POINT BAL.
C----------
      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      NCFDEF(I)=ICDF
      NBFDEF(I)=IBDF
      IPTBAL=NINT(PTBALT(I)*FT2pACRtoM2pHA)
C----------
C  CYCLE 0, PRINT INPUT DG ONLY
C----------
      DGI = DG(I)
      IF(LSTART) DGI = WORK1(I)
C
      IF(IDG.EQ.0 .OR. IDG.EQ.2)THEN
        IF(DGI .GT. 0. .AND. 
     &   ((DGI/BRATIO(ISP(I),DBH(I)-DG(I),HT(I)).LT.DBH(I))))THEN
          PDBHI=DBH(I)-(DG(I)/BRATIO(ISP(I),DBH(I)-DG(I),HT(I)))
        ELSE
          PDBHI=0.
        ENDIF
      ELSE
        PDBHI=PDBH(I)
      ENDIF
      PDBH(I)=0.
C
      IF(IHTG.EQ.0 .OR. IHTG.EQ.2)THEN
        IF(HTG(I) .GT. 0.)THEN
          PHTI=HT(I)-HTG(I)
        ELSE
          PHTI=0.
        ENDIF
      ELSE
        PHTI=PHT(I)
      ENDIF
      PHT(I)=0.
C
      IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
        WRITE(KOLIST,21)
     >    TID,
     >    I,
     >    NSP(ISP(I),1)(1:2),
     >    ISP(I),
     >    IMC(I),
     >    ISPECL(I),
     >    ITRE(I),
     >    P/ACRtoHA,
     >    DP/ACRtoHA,
     >    DBH(I)*INtoCM,
     >    DGI*INtoCM,
     >    HT(I)*FTtoM,
     >    HTG(I)*FTtoM,
     >    ICR(I),
     >    CW*FTtoM,
     >    IDMR,
     >    PCT(I),
     >    IPTBAL,
     >    CFV(I)*FT3toM3,
     >    WK1(I)*FT3toM3,
     >    BFV(I)*FT3toM3,
     >    ICDF,
     >    IBDF,
     >    NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),
     >    PDBHI*INtoCM,
     >    PHTI*FTtoM,
     >    PTOCFV(I)*FT3toM3,
     >    PMRCFV(I)*FT3toM3,
     >    PMRBFV(I)*FT3toM3,
     >    NCFDEF(I),
     >    NBFDEF(I),
     >    YRDLOS(I),
     >    NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),
     >    ABIRTH(I),
     >    IPVEC(ITRE(I))
   21   FORMAT(1X,A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,
     >       1X,F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,
     >       1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,I3,
     >       1X,F5.1,1X,F5.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,F7.5,1X,I5,1X,F6.0,1X,I8)
      ELSE
        WRITE(KOLIST,20)
     >    TID,
     >    I,
     >    NSP(ISP(I),1)(1:2),
     >    ISP(I),
     >    IMC(I),
     >    ISPECL(I),
     >    ITRE(I),
     >    P/ACRtoHA,
     >    DP/ACRtoHA,
     >    DBH(I)*INtoCM,
     >    DGI*INtoCM,
     >    HT(I)*FTtoM,
     >    HTG(I)*FTtoM,
     >    ICR(I),
     >    CW*FTtoM,
     >    IDMR,
     >    PCT(I),
     >    IPTBAL,
     >    CFV(I)*FT3toM3,
     >    WK1(I)*FT3toM3,
     >    BFV(I)*FT3toM3,
     >    ICDF,
     >    IBDF,
     >    NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),
     >    PDBHI*INtoCM,
     >    PHTI*FTtoM,
     >    PTOCFV(I)*FT3toM3,
     >    PMRCFV(I)*FT3toM3,
     >    PMRBFV(I)*FT3toM3,
     >    NCFDEF(I),
     >    NBFDEF(I),
     >    YRDLOS(I),
     >    NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),
     >    ABIRTH(I),
     >    IPVEC(ITRE(I))
   20   FORMAT(1X,A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.2,1X,F8.2,
     >       1X,F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,
     >       1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,I3,
     >       1X,F5.1,1X,F5.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,F7.5,1X,I5,1X,F6.0,1X,I8)
      ENDIF
   50 CONTINUE
   60 CONTINUE
C
C  FOR CYCLE 0 LIST, PRINT DEAD TREES WHICH WERE PRESENT IN
C  THE INVENTORY DATA AT THE BOTTOM OF THE FVSSTAND LIST
C
      IF(IREC2.GT.MAXTRE .OR. ITPLAB.EQ.2) GO TO 200
      DO 150 I=IREC2,MAXTRE
      P =(PROB(I) / GROSPC) / (FINT/FINTM)
      IICR=((ICR(I)-1)/10)+1
      IF (IICR.GT.9) IICR=9
      WRITE(TID,'(I8)') IDTREE(I)
C----------
C     GET MISTLETOE RATING FOR CURRENT TREE RECORD.
C----------
      CALL MISGET(I,IDMR)
C----------
C     SET CROWN WIDTH.
C----------
      CW=CRWDTH(I)
C----------
C     DECODE DEFECT AND ROUND OFF POINT BAL.
C----------
      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      NCFDEF(I)=ICDF
      NBFDEF(I)=IBDF
      IPTBAL=NINT(PTBALT(I)*FT2pACRtoM2pHA)
C----------
C  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
C----------
      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM(6).EQ.0) DGI=WORK1(I)
      PDBHI = 0.
      PHTI = 0.
C----------
C  PUT PROB IN MORTALITY COLUMN
C----------
      DP = P
      P = 0.
C
      IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
        WRITE(KOLIST,21)
     >    TID,
     >    I,
     >    NSP(ISP(I),1)(1:2),
     >    ISP(I),
     >    IMC(I),
     >    ISPECL(I),
     >    ITRE(I),
     >    P/ACRtoHA,
     >    DP/ACRtoHA,
     >    DBH(I)*INtoCM,
     >    DGI*INtoCM,
     >    HT(I)*FTtoM,
     >    HTG(I)*FTtoM,
     >    ICR(I),
     >    CW*FTtoM,
     >    IDMR,
     >    PCT(I),
     >    IPTBAL,
     >    CFV(I)*FT3toM3,
     >    WK1(I)*FT3toM3,
     >    BFV(I)*FT3toM3,
     >    ICDF,
     >    IBDF,
     >    NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),
     >    PDBHI*INtoCM,
     >    PHTI*FTtoM,
     >    PTOCFV(I)*FT3toM3,
     >    PMRCFV(I)*FT3toM3,
     >    PMRBFV(I)*FT3toM3,
     >    NCFDEF(I),
     >    NBFDEF(I),
     >    YRDLOS(I),
     >    NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),
     >    ABIRTH(I),
     >    IPVEC(ITRE(I))
      ELSE
        WRITE(KOLIST,20)
     >    TID,
     >    I,
     >    NSP(ISP(I),1)(1:2),
     >    ISP(I),
     >    IMC(I),
     >    ISPECL(I),
     >    ITRE(I),
     >    P/ACRtoHA,
     >    DP/ACRtoHA,
     >    DBH(I)*INtoCM,
     >    DGI*INtoCM,
     >    HT(I)*FTtoM,
     >    HTG(I)*FTtoM,
     >    ICR(I),
     >    CW*FTtoM,
     >    IDMR,
     >    PCT(I),
     >    IPTBAL,
     >    CFV(I)*FT3toM3,
     >    WK1(I)*FT3toM3,
     >    BFV(I)*FT3toM3,
     >    ICDF,
     >    IBDF,
     >    NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),
     >    PDBHI*INtoCM,
     >    PHTI*FTtoM,
     >    PTOCFV(I)*FT3toM3,
     >    PMRCFV(I)*FT3toM3,
     >    PMRBFV(I)*FT3toM3,
     >    NCFDEF(I),
     >    NBFDEF(I),
     >    YRDLOS(I),
     >    NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),
     >    ABIRTH(I),
     >    IPVEC(ITRE(I))
      ENDIF
  150 CONTINUE
C
  200 CONTINUE
C
  220 CONTINUE
      RETURN
      END
