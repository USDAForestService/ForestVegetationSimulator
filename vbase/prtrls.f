      SUBROUTINE PRTRLS (IWHO)
      IMPLICIT NONE
C----------
C VBASE $Id: prtrls.f 2438 2018-07-05 16:54:21Z gedixon $
C----------
C
C     PRINT THE TREE LIST.
C
C     IWHO = 1 IF CALLED NORMALLY, AND 2 OR 3 IF CALLED FROM CUTS.
C----------
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
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL   LFORMT,LHD,LOK,LRC,LTREE
C
      CHARACTER CISN*11,DAT*10,REV*10,TID*8,TIM*8
C
      CHARACTER CLAB1(4)*4,CLAB2(3)*3
C
      INTEGER*4 DBSKODE,IDCMP1,IDCMP2
C
      INTEGER I,I1,I2,I3,IACTK,IBDF,ICDF,IDMR,IDT,IICR,IP,IPTBAL,ITODO
      INTEGER ITPLAB,ISPC,IWHO,J,JYR,NPRMS,NTODO,NUMREQ,KNTREC,KOLIST
C
      INTEGER MYACT(3)
C
      REAL CW,DGI,DP,P,XXWT
C
      REAL DUPCHK(5,5),TEM(6)
C----------
C  DATA STATEMENTS:
C----------
      DATA MYACT/80,199,198/
      DATA IDCMP1,IDCMP2/10000000,20000000/
      DATA CLAB1/'TREE','DEAD','CUT','ATRT'/
      DATA CLAB2/'END','CUT','CUT'/
C----------
C  INITIALIZATION
C----------
      DO 500 I= 1,6
      TEM(I)= 0.
  500 CONTINUE
C----------
C  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT
C----------
      CISN = '           '
C----------
C  ARRAY DUPCHK STORES UP TO 5 REQUESTS PER CYCLE AND ELIMINATES 
C  DUPLICATES OF THESE 5 REQUESTS.
C----------
      DO 502 I=1,5
      DO 501 J=1,5
      DUPCHK(I,J)=0.
  501 CONTINUE
  502 CONTINUE
      NUMREQ = 0
C----------
C     FIND OUT IF THERE IS A TREELIST OPTION.
C
      CALL OPFIND (1,MYACT(IWHO),NTODO)
C
C     IF THERE ARE NONE TO DO, THEN: RETURN.
C
      IF (NTODO.EQ.0) RETURN
C
C     ELSE: DO THE OPTIONS PRESENT.
C
      DO 200 ITODO=1,NTODO
      CALL OPGET (ITODO,6,IDT,IACTK,NPRMS,TEM)
      IF (IACTK.LT.0) GOTO 200
C----------
C  STORE THE REQUEST IF IT ISN'T A DUPLICATE. IGNORE THE REQUEST
C  IF IT IS A DUPLICATE. SKIP THE DUPLICATE LOGIC IF THERE IS ONLY
C  ONE REQUEST.
C----------
      IF(NTODO .EQ. 1)GO TO 510
      IF(ITODO .EQ. 1) THEN
        DUPCHK(1,1)=TEM(1)
        DUPCHK(1,2)=TEM(2)
        DUPCHK(1,3)=TEM(3)
        DUPCHK(1,4)=TEM(4)
        DUPCHK(1,5)=TEM(6)
        NUMREQ=1
      ELSE
        DO I=1,NUMREQ
        IF(TEM(1).EQ.DUPCHK(I,1) .AND. TEM(2).EQ.DUPCHK(I,2) .AND.
     &     TEM(3).EQ.DUPCHK(I,3) .AND. TEM(4).EQ.DUPCHK(I,4) .AND.
     &     TEM(6).EQ.DUPCHK(I,5)) GO TO 200
        ENDDO
        IF(NUMREQ.GE.5)GO TO 510
        NUMREQ=NUMREQ+1
        DUPCHK(NUMREQ,1)=TEM(1)
        DUPCHK(NUMREQ,2)=TEM(2)
        DUPCHK(NUMREQ,3)=TEM(3)
        DUPCHK(NUMREQ,4)=TEM(4)
        DUPCHK(NUMREQ,5)=TEM(6)
      ENDIF
C
C     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
C     AND THE OUTPUT REPORTING YEAR.
C     ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
C             3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.
C 
C
  510 CONTINUE
      IF (IWHO.EQ.1) THEN
         JYR=IY(ICYC+1)
         IP=ITRN
         ITPLAB=1
         IF(NPRMS.GE.4 .AND. TEM(4).GT.0.)ITPLAB=2
      ELSEIF(IWHO.EQ.2)THEN
         JYR=IY(ICYC)
         IP=0
         DO 1 I=1,ITRN
         IF (WK3(I).GT.0) IP=IP+1
    1    CONTINUE
         ITPLAB=3
      ELSE
         JYR=IY(ICYC)
         IP=0
         DO I=1,ITRN
         IF (PROB(I).GT.0)IP=IP+1
         ENDDO
         ITPLAB=4
      ENDIF
      LTREE=.FALSE.
C
C     IF AT THE PRE-PROJECTION TIME, FIND OUT IF A TREELIST HAS BEEN
C     SUPPRESSED OR IF A DEAD TREE LIST IS REQUESTED. IF YES, THEN
C     BRANCH TO NOT ACCOMPLISH THE REQUEST.
C
      IF (LSTART) THEN
         IF ((NPRMS.GE.3 .AND. TEM(3).EQ.1.)) GOTO 200
         IF(NPRMS.GE.3 .AND. TEM(3).EQ.2.)CALL OPDONE(ITODO,JYR)
      ELSE
         IF(NPRMS.GE.3 .AND. TEM(3).EQ.2) GO TO 200
C
C        SIGNAL THE OPTION DONE.
C
         IF(IWHO.EQ.1)THEN
           CALL OPDONE (ITODO,JYR-1)
         ELSEIF (IWHO.EQ.2)THEN
           CALL OPDONE (ITODO,JYR)
         ELSE
           CALL OPDONE (ITODO,JYR)
         ENDIF
      ENDIF
C
C     CALL THE DATABASE TREELIST OUTPUT FUNCTION
C
      DBSKODE = 1
      CALL DBSTRLS(IWHO,DBSKODE,TEM(6))
C     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
C
C     CALL THE DATABASE AFTER TREATMENT TREELIST OUTPUT FUNCTION
C
      DBSKODE = 1
      CALL DBSATRTLS(IWHO,DBSKODE,TEM(6))
C     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
C
C     CALL THE DATABASE CUTLIST OUTPUT FUNCTION
C
      DBSKODE = 1
      CALL DBSCUTS(IWHO,DBSKODE)
C     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
C
C     SET VARIABLES AND SWITCHES
C
      LHD=TEM(2).EQ.0.0
      LRC=TEM(2).GE.0.0
      KOLIST=IFIX(TEM(1))
      LFORMT=KOLIST.GT.0
      IF (.NOT.LFORMT) THEN
         LRC=.TRUE.
         LHD=.FALSE.
         KOLIST=IABS(KOLIST)
      ENDIF
C
C     MAKE SURE THE OUTPUT FILE IS OPENNED
C
      CALL openIfClosed (KOLIST,"trl",LOK)
      IF (.NOT.LOK) RETURN
C
C     IF NO HEADING IS BEING WRITTEN, WRITE A MARKER AND THE NUMBER OF
C     RECORDS WHICH FOLLOW.
C
      IF (.NOT.LHD .AND. LRC) THEN
         XXWT=SAMWT
         CALL REVISE (VARACD,REV)
         CALL GRDTIM (DAT,TIM)
         IF (LFORMT) THEN
            WRITE (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,
     &                 CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
    2       FORMAT ('-999',3I5,6(1X,A),I3,E14.7,2(1X,A))
         ELSE
            WRITE (KOLIST) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,
     &                 CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
         ENDIF
      ENDIF
      IP=0
      KNTREC=0
C
C     PROCESS TREELIST
C
      DO 60 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 60
      I2=ISCT(ISPC,2)
      DO 50 I3=I1,I2
      LTREE=.TRUE.
      I=IND1(I3)
      IF (.NOT.LHD) GOTO 120
      IF(KNTREC.NE.0) GO TO 110
      IP=IP+1
      IF (LFORMT) CALL GROHED (KOLIST)
      IF(LFORMT) THEN
C
        SELECT CASE (VARACD)
C----------
C  EASTERN VARIANTS
C----------
        CASE ('CS','LS','NE','SN')
          WRITE (KOLIST,10) CLAB1(ITPLAB),NPLT,MGMID,
     >                      CLAB2(IWHO),ICYC,IFINT,JYR,IP
   10     FORMAT(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',
     >     A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',
     >     T109,'YEAR: ',I4,T121,'PAGE: ',I2/
     >     '  TREE   TREE SP SP TR SS PNT  ',
     >     'TREES    MORTAL   CURR  DIB   CURR  HT',5X,'MAX',7X,'BA   ',
     >     'POINT TOT MCH SAW CU SAW BD PW SL TRC',/,
     >     ' NUMBER  INDX CD NO CL CD NUM PER ACRE PER ACRE  ',
     >     'DIAM  INCR   HT  INCR CR  CW  MS ',
     >     '%-TILE  BAL CU FT VL FT VOL FT VL  DF DF  HT',/,
     >     '-------- ---- -- -- -- -- --- -------- -------- ----- ',
     >     '----- ----- ---- -- ---- -- ------ ----- ------ ',
     >     '------ ------- -- -- ---')
C----------
C  WESTERN VARIANTS
C----------
        CASE DEFAULT
          WRITE (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID,
     >                      CLAB2(IWHO),ICYC,IFINT,JYR,IP
   11     FORMAT(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',
     >     A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',
     >     T109,'YEAR: ',I4,T121,'PAGE: ',I2/
     >     '  TREE   TREE SP SP TR SS PNT  ',
     >     'TREES    MORTAL   CURR  DIAM  CURR  HT',5X,'MAX',7X,'BA   ',
     >     'POINT TOT CU MCH CU  MCH BD MC BF TRC',/,
     >     ' NUMBER  INDX CD NO CL CD NUM PER ACRE PER ACRE  ',
     >     'DIAM  INCR   HT  INCR CR  CW  MS ',
     >     '%-TILE  BAL  FT VOL FT VOL  FT VOL DF DF  HT',/,
     >     '-------- ---- -- -- -- -- --- -------- -------- ----- ',
     >     '----- ----- ---- -- ---- -- ------ ----- ------ ',
     >     '------ ------- -- -- ---')
        END SELECT
C
      ENDIF
      KNTREC=1
  110 CONTINUE
      IF (ITPLAB.EQ.3) THEN
         IF (WK3(I).GT.0) KNTREC=KNTREC+1
      ELSEIF(ITPLAB.EQ.4)THEN
         IF (PROB(I).GT.0) KNTREC=KNTREC+1
      ELSE
         KNTREC=KNTREC+1
      ENDIF
      IF(KNTREC.GT.50) KNTREC=0
  120 CONTINUE
C
C      ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
C              3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.
C 
      IF (ITPLAB.EQ.1) THEN
         P = PROB(I) / GROSPC
         IF (ICYC.GT.0) THEN
            DP = WK2(I)/ GROSPC
         ELSE
            DP = 0.0
         ENDIF
      ELSEIF (ITPLAB.EQ.2) THEN
         IF (ICYC.GT.0) THEN
            DP = WK2(I)/ GROSPC
            P= 0.
         ELSE
            DP= 0.
            P = 0.
         ENDIF
      ELSEIF (ITPLAB.EQ.3) THEN
         P = WK3(I)/GROSPC
         DP = 0.0
         IF (P.LE.0.0) GOTO 50
      ELSE
         P=PROB(I)/GROSPC
         DP = 0.0
         IF (P.LE.0.0) GOTO 50
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
      IPTBAL=NINT(PTBALT(I))
C----------
C  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
C----------
      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM(6).EQ.0) DGI=WORK1(I)
C
      SELECT CASE (VARACD)
C----------
C  EASTERN VARIANTS
C----------
      CASE ('CS','LS','NE','SN')
        IF (LFORMT) THEN
          IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
            WRITE(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >      ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >      IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >      ((ITRUNC(I)+5)/100)
   21       FORMAT(A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,
     >       F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,
     >       1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,I3)
          ELSE
            WRITE(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >      ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >      IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >      ((ITRUNC(I)+5)/100)
   20       FORMAT(A8,1X,2I4,I5,F9.3,F8.2,3F7.2,I3,I4,F9.3,I4,F9.2,
     >       F9.3,F9.2,F9.2,I4,I7,I4)
          ENDIF
        ELSE
          WRITE(KOLIST) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >    ISPECL(I),ITRE(I),P,DP,DBH(I),DGI,HT(I),HTG(I),ICR(I),CW,
     >    IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >    ((ITRUNC(I)+5)/100)
        ENDIF
C----------
C  WESTERN VARIANTS
C----------
      CASE DEFAULT
        IF (LFORMT) THEN
          IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
            WRITE(KOLIST,23) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >      ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >      IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >      ((ITRUNC(I)+5)/100)
   23       FORMAT(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,
     >       F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,
     >       1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,I3)
          ELSE
            WRITE(KOLIST,24) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >      ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >      IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >      ((ITRUNC(I)+5)/100)
   24       FORMAT(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.2,1X,F8.2,1X,
     >       F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,
     >       1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,
     >       1X,I3)
          ENDIF
        ELSE
          WRITE(KOLIST) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >    ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >    IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >    ((ITRUNC(I)+5)/100)
        ENDIF
C
      END SELECT
C
   50 CONTINUE
   60 CONTINUE
C
C  FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
C  THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
C
      IF((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR.(ICYC.GE.1))GO TO 200
C
      DO 150 I=IREC2,MAXTRE
C----------
C  WRITE HEADER FOR CASE WHERE NO LIVE TREES IN INVENTORY
C----------
      IF((.NOT.LTREE).AND.LFORMT.AND.LHD) THEN
      IF (LFORMT) CALL GROHED (KOLIST)
        WRITE (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID,
     >                    CLAB2(IWHO),ICYC,IFINT,JYR,IP
        LTREE=.TRUE.
      ENDIF
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
      IPTBAL=NINT(PTBALT(I))
C----------
C  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
C----------
      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM(6).EQ.0) DGI=WORK1(I)
C----------
C  PUT PROB IN MORTALITY COLUMN
C----------
      DP = P
      P = 0.
C
      IF (LFORMT) THEN
        IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
          WRITE(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >    ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >    IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >    ((ITRUNC(I)+5)/100)
        ELSE
          WRITE(KOLIST,20) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >    ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >    IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >    ((ITRUNC(I)+5)/100)
        ENDIF
      ELSE
          WRITE(KOLIST) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),
     >    ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW,
     >    IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF,
     >    ((ITRUNC(I)+5)/100)
      ENDIF
  150 CONTINUE
C
  200 CONTINUE
      RETURN
      END
