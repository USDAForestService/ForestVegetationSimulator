      SUBROUTINE SSTAGE(INBA,INICYCLE,LSUPRT)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C   THIS SUBROUTINE CALCULATES THE STAND STRUCTURAL STAGE CLASSES, AS
C   DEFINED BY STAGE ET AL.
C
C   N.L.CROOKSTON - INT MOSCOW - JUNE 1996 AND WITH

C   A.R.STAGE - INT MOSCOW - JUNE 1997
C
C   INBA = 1 IF CALL IS PRIOR TO THINNINGS.  OTHER VALUES MEAN AFTER
C           THINNING.
C   INICYCLE = THE CYCLE NUMBER.
C   LSUPRT = IF TRUE, SUPPRESS THE PRINTING, EVEN IF IT IS REQUESTED.
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
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'SSTGMC.F77'
C
C
COMMONS
C
      COMMON/SSTAGC/TMPCCC
C
      INTEGER IOUT,ICD,MSP1,MSP2,IS3OK,IS2OK,I2,IS1OK,IS3I2,IS3I1
      INTEGER IS2I2,IS2I1,IS1I2,IS1I1,ILARGE,IILG,ID2I2,ID2I1,IHTLS3
      INTEGER IHTS1,IHTS2,IHTS3,IHTSS1,IHTSS2,IHTSS3,IHTLS1,IHTLS2
      INTEGER ICRBS1,ICRBS2,ICRBS3,ICRCV1,ICRCV2,ICRCV3,ICOVR,NTREES
      INTEGER II,ID1I1,ID1I2,NTODO,IDATE,IACTK,NP,MYACTS(1)
      INTEGER IBA2
      REAL COVER,DMIND,CRS3,CRS2,CRS1,DIFF,X,SUMPRB,DIFF1,DIFF2
      REAL DBHS3,DBHS2,DBHS1,XBAMAX,TMPCCC,PRM(1)
      LOGICAL LSUPRT

      REAL SSWK(MAXTRE)

      INTEGER INDEX(MAXTRE),IHTLS,IHTSS,NSTR,DBSKODE
      INTEGER INBA,IBA,INICYCLE,ICYCLE,I,J
      REAL XHTLS,XHTSS,XNSTR,XSDI
      REAL TPA1, TPA2, TPA3

      LOGICAL DEBUG

      CHARACTER*3 SP11,SP21,SP12,SP22,SP13,SP23
      INTEGER ISP11,ISP21,ISP12,ISP22,ISP13,ISP23,J1,J2 

C     STAGE CLASS CODES ARE:
C
C     0 = BG = BARE GROUND
C     1 = SI = STAND INITIATION
C     2 = SE = STEM EXCLUSION
C     3 = UR = UNDERSTORY REINITIATION
C     4 = YM = YOUNG FOREST MULTISTRATA
C     5 = OS = OLD FOREST SINGLE STRATUM
C     6 = OM = OLD FOREST MULTISTRATA

      CHARACTER*4 SSCODES(7)/'0=BG','1=SI','2=SE','3=UR','4=YM',
     >                       '5=OS','6=OM'/

C     DEFINE TEMPORARY VARIABLES SO THAT STRUCTURE STAGE LOGIC CAN BE
C     CALLED BY FFE THRU ENTRY POINT BELOW.

      REAL TMPTPA, TMPCCM, TMPPCT, TMPSAW, TMPSSD, TMPGAP, TMPDBH
      REAL FMTPA, FMCCM, FMPCT, FMSAW, FMSSD, FMGAP, FMDBH
      REAL TMPPRB(MAXTRE),FPROB(MAXTRE)
      INTEGER TMPSCL, FMSTCL, FMFLAG, TMPICR(MAXTRE),FICR(MAXTRE)
C----------
C  DATA STATEMENTS.
C  ACTIVITY 444 = CCADJ KEYWORD
C----------
      DATA MYACTS/444/

      TMPTPA = TPAMIN
      TMPCCM = CCMIN
      TMPPCT = PCTSMX
      TMPSAW = SAWDBH
      TMPSSD = SSDBH
      TMPGAP = GAPPCT
      FMFLAG = 0
      IBA    = INBA
      IBA2   = IBA
      ICYCLE = INICYCLE
      DEBUG  = .FALSE.

C     SETUP THE DEBUG FLAG.

      CALL DBCHK (DEBUG,'SSTAGE',6,ICYCLE)

      DO I=1, ITRN
        TMPPRB(I) = PROB(I)
        TMPICR(I) = ICR(I)
      ENDDO
C-----------
C  PROCESS CCADJ KEYWORD.
C-----------
      IF(INBA.EQ.1.AND.GLOCCC.EQ.0)CALL OPFIND(1,MYACTS,NTODO)    
      IF(NTODO.EQ.0)THEN
        IF(INBA.EQ.1)THEN
          IF(GLOCCC.EQ.1)THEN
C
C           CCAdj activity not present at this call, but value saved
C           from previous CCAdj processing is loaded and GLOCC set
C           for use in cuts.f
C
            CCCOEF=TMPCCC
            GLOCCC=2
          ENDIF
        ELSEIF(INBA.EQ.2)THEN
          IF(TMPCCC.NE.0)THEN
            CCCOEF=TMPCCC
          ENDIF
        ENDIF   
      ELSE
        CALL OPGET(1,1,IDATE,IACTK,NP,PRM)
        IF(NTODO.EQ.1)THEN
          IF(ICYCLE.EQ.1)THEN
C
C           Load value directly
C
            CCCOEF=PRM(1)
          ELSE
C
C           Load value in temp for later call
C           and set GLOCCC indicator
C
            TMPCCC=PRM(1)
            GLOCCC=1
          ENDIF
          CALL OPDONE(1,IDATE)        
        ENDIF
      ENDIF
   
      IF(DEBUG)WRITE(JOSTND,*)'CCCOEF=',CCCOEF,'PRM(1)=',PRM(1),
     &'NTODO=',NTODO,'GLOCCC=',GLOCCC
C             
C     IF WE ARE NOT CALCULATING SSTAGE, THEN SET THE EVENT MONITOR
C     VARIABLES TO FALSE AND RETURN.

      IF (ICYCLE.EQ.1) THEN
         IF (.NOT. LCALC) THEN
            CALL EVUST4(16)
            CALL EVUST4(17)
            CALL EVUST4(18)
            CALL EVUST4(19)
            CALL EVUST4(24)
            CALL EVUST4(25)
         ENDIF
      ENDIF

      IF (.NOT.LCALC) RETURN

      GOTO 10

C *******************************************************************
C     ENTRY POINT FOR CALLS FROM FFE

      ENTRY FMSSTAGE(FMTPA,FMCCM,FMPCT,FMSAW,FMSSD,FMGAP,FMSTCL,FMDBH,
     & FPROB,FICR)

      TMPTPA = FMTPA
      TMPCCM = FMCCM
      TMPPCT = FMPCT
      TMPSAW = FMSAW
      TMPSSD = FMSSD
      TMPGAP = FMGAP
      FMFLAG = 1
      DEBUG = .FALSE.

C     SETUP THE DEBUG FLAG.

      CALL DBCHK (DEBUG,'SSTAGE',6,ICYCLE)

      DO I=1, ITRN
        TMPPRB(I) = FPROB(I)
        TMPICR(I) = FICR(I)
      ENDDO

   10 CONTINUE

C *******************************************************************

C     IF THIS CALL FOLLOWS THINNING, AND IF THERE ARE NO REMOVALS,
C     THEN THE STAGE AFTER THINNING IS THE SAME AS THE ONE PRIOR.
C     IF CALLED FROM FFE, THEN USE AFTER THINNING VALUES.

      IF (FMFLAG .EQ. 1) THEN
        XBAMAX = ATSDIX
        IBA = 2
        ICYCLE = ICYC
      ELSE
        XBAMAX = BTSDIX
        IF(IBA.NE.1 .AND. ONTREM(7).GT.0.)XBAMAX=ATSDIX
      ENDIF

      IF (DEBUG) WRITE (JOSTND,'('' IN SSTAGE, IBA, ICYCLE='',2I5)')
     >       IBA,ICYCLE

C     INITIALIZE

      NSTR   = 0
      TMPSCL = 0
      TMPDBH = 0.
      XHTLS  = 0.
      XHTSS  = 0.
      XNSTR  = 0.
      XSDI   = 0.
C
      DBHS1  = 0.
      DBHS2  = 0.
      DBHS3  = 0.
      IHTS1  = 0
      IHTS2  = 0
      IHTS3  = 0
      IHTLS  = 0
      IHTSS  = 0
      IHTSS1 = 0
      IHTSS2 = 0
      IHTSS3 = 0
      IHTLS1 = 0
      IHTLS2 = 0
      IHTLS3 = 0
      ICRBS1 = 0
      ICRBS2 = 0
      ICRBS3 = 0
      ICRCV1 = 0
      ICRCV2 = 0
      ICRCV3 = 0
      ICOVR  = 0
      ISP11  = 0
      ISP21  = 0
      ISP12  = 0
      ISP22  = 0
      ISP13  = 0
      ISP23  = 0
      TPA1  = 0
      TPA2  = 0
      TPA3  = 0
      SP11   = '-- '
      SP21   = '-- '
      SP12   = '-- '
      SP22   = '-- '
      SP13   = '-- '
      SP23   = '-- '

C
C     LOAD THE INDEX WITH TREES THAT REPRESENT OVER A LIMITED
C     TREES/ACRE
C
      NTREES = 0
      DO I=1,ITRN
         IF (TMPPRB(I).GT. 0.00001) THEN
            NTREES=NTREES+1
            INDEX(NTREES)=I
         ENDIF

      IF (DEBUG) WRITE (JOSTND,
     >   '('' IN SSTAGE. I, DBH, HT, TMPICR, TMPPRB= '',
     >    I4,F9.3,F9.3,I4,F9.5)')
     >    I,DBH(I),HT(I),TMPICR(I),TMPPRB(I)

      ENDDO

      IF (NTREES.EQ.0) GOTO 100

      IF (NTREES.LE.1) THEN

         I=INDEX(1)
         WK6(I)=CRWDTH(I)

C        NOTE:  0.785398 = PI/4

         WK6(I) = WK6(I)*WK6(I)*TMPPRB(I)*0.785398
         ICRCV1 = IFIX((WK6(I)/43560.)+.5)

         IF (WK6(I) .LT. 435.60*TMPCCM) THEN
            TMPSCL = 0
            IF (TMPPRB(I).GE. TMPTPA) TMPSCL=1
         ELSEIF (DBH(I).LT. TMPSSD) THEN
            TMPSCL = 1
         ELSEIF (DBH(I).LT. TMPSAW) THEN
            TMPSCL = 2
            IF (SDIAC .LT. .01*TMPPCT*XBAMAX)
     >         TMPSCL = 1
         ELSE
            TMPSCL = 5
         ENDIF
         TMPDBH = DBH(I)
         DBHS1  = TMPDBH
         IHTS1  = IFIX(HT(I)+.5)
         IHTSS1 = IHTS1
         IHTLS1 = IHTS1
         ICRBS1 = IFIX(HT(I)*(1.-FLOAT(TMPICR(I))*.01)+.5)
         SP11   = JSP(ISP(I))(1:3)
         ISP11   = ISP(I)
         TPA1 = TMPPRB(I)
         GOTO 80
      ENDIF

      CALL RDPSRT (NTREES,HT,INDEX,.FALSE.)

C     CALCULATE THE CCF-BASED "CROWN WIDTH" FOR EACH TREE...AND
C     CONVERT THIS TO CROWN COVER.

      DO II= 1,NTREES
      I= INDEX(II)
      WK6(I)=CRWDTH(I)
      WK6(I) = WK6(I)*WK6(I)*TMPPRB(I)*0.785398

      IF (DEBUG) WRITE (JOSTND,
     >   '('' IN SSTAGE. I, II, DBH, HT, TMPICR, WK6= '',
     >    I4,I4,F9.3,F9.3,I4,F9.1)')
     >    I,II,DBH(I),HT(I),TMPICR(I),WK6(I)

      ENDDO

C     FIND THE TWO LARGEST DIFFERENCES.  THE LARGEST DIFFERENCE
C     WILL BE DIFF1 AND THE SECOND LARGEST DIFFERENCE WILL BE
C     DIFF2.

      DIFF1 = -1E20
      DIFF2 = DIFF1

C     THE FOLLOWING POINTERS POINT TO THE FIRST AND THE LAST TREE
C     RECORD IN A GAP.  THAT IS, A GAP MAY SPAN MORE THAN ONE TREE
C     RECORD (IN THE CASE THAT WE FIND INSEGNIFICANT "LADDER TREES").

      ID1I1 = 0
      ID1I2 = 0
      ID2I1 = 0
      ID2I2 = 0

      IILG  = 1
      ILARGE= INDEX(IILG)
      SUMPRB= 0.0

      DO II=2,NTREES

         ISMALL=INDEX(II)

C        IF THE SMALLER TREE CREATES A GAP...
C        (A GAP MUST BE AT LEAST 10 FEET)...

         X = HT(ILARGE)*TMPGAP*.01
         IF (X .LT. 10.) X = 10.

         IF (HT(ISMALL) .LT. HT(ILARGE)-X) THEN

C           IF THE SMALL TREE HAS AN INSIGNIFICANT PROB, THEN SKIP IT!

            IF (TMPPRB(ISMALL)+SUMPRB .LT. 2.0) THEN
               SUMPRB = SUMPRB+TMPPRB(ISMALL)
            ELSE
               DIFF = HT(ILARGE)-HT(ISMALL)

               IF (DIFF .GT. DIFF1) THEN
                  DIFF2 = DIFF1
                  DIFF1 = DIFF
                  ID2I1 = ID1I1
                  ID2I2 = ID1I2
                  ID1I1 = IILG
                  ID1I2 = II
               ELSEIF (DIFF .GT. DIFF2) THEN
                  DIFF2 = DIFF
                  ID2I1 = IILG
                  ID2I2 = II
               ENDIF

               ILARGE = ISMALL
               IILG   = II
               SUMPRB= 0.0
            ENDIF
         ELSE

C        IF THE SMALLER TREE DOES NOT CREATE A GAP...THEN SEE IF
C        IF IT IS AN INSIGNIFICANT "LADER" TREE.

            IF (TMPPRB(ISMALL)+SUMPRB.LT.2.0) THEN
               SUMPRB = SUMPRB+TMPPRB(ISMALL)
            ELSE

C              IT IS A LADDER TREE...AND NO GAP WAS FOUND.
C              RESET THE SUMMATION AND MOVE THE "TOP" TREE DOWN.

               ILARGE= ISMALL
               IILG  = II
               SUMPRB= 0.0

            ENDIF
         ENDIF
         IF (DEBUG) WRITE (JOSTND,15) II,IILG,ILARGE,ISMALL,
     >              HT(ILARGE),HT(ISMALL),
     >              SUMPRB,ID1I1,ID1I2,ID2I1,ID2I2
  15     FORMAT(' II=',i4,' IILG=',I4,' LG=',i4,' SM=',I4,' HTL&S=',
     >          2F7.1,' SPB=',F7.4,' D11=',I4,' 2=',I4,
     >         ' D21=',I4,' 2=',I4)
      ENDDO

C     MAKE SURE THAT THE "UPPER" STRATUM IS ON "TOP". AT THIS
C     POINT, THE LARGEST GAP MAY BE BELOW THE UPPER GAP.

      IF (ID1I1.GT.ID2I1 .AND. ID2I1.GT.0) THEN

C         FIRST SWITCH THE POINTERS TO THE FIRST TREE IN THE GAP, THEN
C         THE POINTERS TO THE LAST TREE IN THE GAP.

          II    = ID1I1
          ID1I1 = ID2I1
          ID2I1 = II
          II    = ID1I2
          ID1I2 = ID2I2
          ID2I2 = II

      ENDIF

C     THESE POINTERS POINT TO THE TOP AND BOTTOM OF EACH STRATUM...
C     AND NSTR IS THE NUMBER OF POTENTIAL STRATUM.

      NSTR  = 1
      IS1I1 = 1
      IS1I2 = NTREES
      IS2I1 = 0
      IS2I2 = 0
      IS3I1 = 0
      IS3I2 = 0

      IF (ID1I1.GT.0) THEN
         NSTR  = 2
         IS1I2 = ID1I1
         IS2I1 = ID1I2
         IS2I2 = NTREES
      ENDIF

      IF (ID2I1.GT.0) THEN
         NSTR  = 3
         IS2I2 = ID2I1
         IS3I1 = ID2I2
         IS3I2 = NTREES
      ENDIF

      IF (DEBUG) WRITE (JOSTND,20) ID1I1,ID1I2,ID2I1,ID2I2,
     >   DIFF1,DIFF2,NSTR,IS1I1,IS1I2,IS2I1,IS2I2,IS3I1,IS3I2

   20    FORMAT(' ID1I1=',I4,' ID1I2=',I4,' ID2I1=',I4,' ID2I2=',I4,
     >          ' DIFF1&2=',2E14.7/
     >          ' NSTR=',I2,' IS1I1=',I4,' IS1I2=',I4,' IS2I1=',I4,
     >          ' IS2I2=',I4,' IS3I1=',I4,' IS3I2=',I4)

C     THE FIRST POTENTIAL STRATUM CONTAINS TREES FROM 1 TO ID1I1
C     THE SECOND CONTAINS TREES FROM ID1I2 TO ID2I1
C     THE THIRD CONTAINS TREES FROM LOC2+1 TO NTREES

C     COMPUTE AND SAVE THE COVER IN EACH POTENTIAL STRATUM AND
C     CHECK FOR MIN COVER.

C     CHECK THE FIRST....

      IS1OK = 0

C     FOR THE PURPOSES OF COMPUTING THE COVER, INCLUDE THE TREES THAT
C     ARE WITHIN THE GAP IN THE UPPER STRATUM.

      I2 = MAX(IS1I2,IS2I1-1)
      CALL COVOLP (DEBUG,JOSTND,I2-IS1I1+1,INDEX(IS1I1),WK6,CRS1,
     &CCCOEF)
      IF (DEBUG) WRITE (JOSTND,'('' I2='',I4,'' CRS1='',F8.2)') I2,CRS1
      IF (CRS1.GT. TMPCCM) IS1OK = 1

C     CHECK THE SECOND....

      IS2OK = 0
      CRS2 = 0.
      IF (NSTR .GE. 2) THEN
         I2 = MAX(IS2I2,IS3I1-1)
         CALL COVOLP (DEBUG,JOSTND,I2-IS2I1+1,INDEX(IS2I1),WK6,CRS2,
     &    CCCOEF)
         IF (DEBUG) WRITE (JOSTND,'('' I2='',I4,'' CRS2='',F8.2)')
     >              I2,CRS2
         IF (CRS2.GT. TMPCCM) IS2OK = 1
      ENDIF

C     CHECK THE THIRD...

      IS3OK = 0
      CRS3 = 0.
      IF (NSTR .GE. 3) THEN
         CALL COVOLP (DEBUG,JOSTND,IS3I2-IS3I1+1,INDEX(IS3I1),WK6,CRS3,
     &    CCCOEF)
         IF (DEBUG) WRITE (JOSTND,'('' I2='',I4,'' CRS3='',F8.2)')
     >              I2,CRS3
          IF (CRS3.GT. TMPCCM) IS3OK = 1
      ENDIF

      IF (DEBUG) WRITE (JOSTND, 30) 1, CRS1, CRS2, CRS3,
     >     IS1OK, IS2OK, IS3OK, NSTR
   30 FORMAT(I2,' CRS1,2,3=',3F8.2,' IS1,2,3OK=',3I2,' NSTR=',I2)

C     COUNT THE STRATA

      NSTR = IS1OK + IS2OK + IS3OK

      IF (DEBUG) WRITE (JOSTND, 30) 2, CRS1, CRS2, CRS3,
     >     IS1OK, IS2OK, IS3OK, NSTR


C     IF THERE ARE NOT VALID STRATA AND IF THE NUMBER OF TREES ARE
C     TPAMIN (TMPTPA), THEN WE ARE DONE.  OTHERWISE, FORM ONE STRATUM OF
C     ALL THE TREES.

      IF (NSTR.EQ.0) THEN
         IF (TPROB.LT.TMPTPA) GOTO 80
         IS1OK =1
         NSTR  =1
         IS1I1 =1
         IS1I2 =NTREES
      ENDIF

C     THE CALLS BELOW TO SSTGHTPA COMPUTE THE TREES PER ACRE IN EACH
C     STRATUM.  THESE VALUES INCLUDE THE TREES IN THE GAPS (LIKE THE
C     COVER CALCULATIONS ABOVE).

      J1 = MAX(IS1I2,IS2I1-1)
      J2 = MAX(IS2I2,IS3I1-1)

      CALL SSTGHTPA (IS1I1,J1,INDEX,TMPPRB,MAXTRE,TPA1)
      CALL SSTGHTPA (IS2I1,J2,INDEX,TMPPRB,MAXTRE,TPA2)
      CALL SSTGHTPA (IS3I1,IS3I2,INDEX,TMPPRB,MAXTRE,TPA3)

      CALL SSTGHP (IS1I1,IS1I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB,
     >     MAXTRE,MAXSP,DBHS1,IHTS1,IHTSS1,IHTLS1,ICRBS1,MSP1,MSP2)
      IF (MSP1.GT.0) SP11 = JSP(MSP1)(1:3)
      IF (MSP2.GT.0) SP21 = JSP(MSP2)(1:3)
      IF (MSP1.GT.0) ISP11 = MSP1
      IF (MSP2.GT.0) ISP21 = MSP2

      CALL SSTGHP (IS2I1,IS2I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB,
     >     MAXTRE,MAXSP,DBHS2,IHTS2,IHTSS2,IHTLS2,ICRBS2,MSP1,MSP2)
      IF (MSP1.GT.0) SP12 = JSP(MSP1)(1:3)
      IF (MSP2.GT.0) SP22 = JSP(MSP2)(1:3)
      IF (MSP1.GT.0) ISP12 = MSP1
      IF (MSP2.GT.0) ISP22 = MSP2

      CALL SSTGHP (IS3I1,IS3I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB,
     >     MAXTRE,MAXSP,DBHS3,IHTS3,IHTSS3,IHTLS3,ICRBS3,MSP1,MSP2)
      IF (MSP1.GT.0) SP13 = JSP(MSP1)(1:3)
      IF (MSP2.GT.0) SP23 = JSP(MSP2)(1:3)
      IF (MSP1.GT.0) ISP13 = MSP1
      IF (MSP2.GT.0) ISP23 = MSP2

C     SELECT THE STRATUM THAT WILL DICTATE THE "DOM" STRATUM.

      IF (IS1OK.EQ.1) THEN
         IS1OK = 2
         TMPDBH = DBHS1
         DMIND  = DBH(INDEX(IS1I2))
         IHTLS=IHTLS1
         IHTSS=IHTSS1
      ELSE IF (IS2OK.EQ.1) THEN
         IS2OK = 2
         TMPDBH = DBHS2
         DMIND  = DBH(INDEX(IS2I2))
         IHTLS=IHTLS2
         IHTSS=IHTSS2
      ELSE IF (IS3OK.EQ.1) THEN
         IS3OK = 2
         TMPDBH = DBHS3
         DMIND  = DBH(INDEX(IS3I2))
         IHTLS=IHTLS3
         IHTSS=IHTSS3
      ENDIF

C     CLASSIFIY THE STAND STRUCTURAL STAGE

      IF (NSTR.EQ.1) THEN
         IF (TMPDBH .LT. TMPSSD) THEN
            TMPSCL = 1
         ELSEIF (TMPDBH .LT. TMPSAW) THEN
            TMPSCL = 2
            IF(IBA.EQ.1)THEN
               XSDI=SDIBC
            ELSE
               XSDI=SDIAC
            ENDIF
            IF (XSDI .LT. .01*TMPPCT*XBAMAX)
     >           TMPSCL = 1
         ELSE
            TMPSCL = 5
C
C           IF THE MIN DBH OF THE DOM STRATUM IS LESS THAN A
C           CRITICAL VALUE (WE PICKED 3.0, FOR NOW), THEN THE
C           STAND IS MULTISTRATUM (A.K.A, CONTINUOUS STRATUM).
C
            IF (DMIND .LT. 3.0) TMPSCL = 6
         ENDIF
      ELSEIF (NSTR.EQ.2) THEN
         IF (TMPDBH.LT. TMPSSD) THEN
            TMPSCL = 1
         ELSEIF (TMPDBH .LT. TMPSAW) THEN
            TMPSCL = 3
         ELSE
            TMPSCL = 6
         ENDIF
      ELSE
         IF (TMPDBH.LT. TMPSSD) THEN
            TMPSCL = 1
         ELSEIF (TMPDBH.LT. TMPSAW) THEN
            TMPSCL = 4
         ELSE
            TMPSCL = 6
         ENDIF
      ENDIF
C
      IF (DEBUG) WRITE (JOSTND,
     >   '('' IN SSTAGE. TMPSSD,TMPSAW,TMPPCT,XSDI,XBAMAX= '',
     >    F9.1,F9.1,F9.1,F9.1,F9.1)')
     >   TMPSSD,TMPSAW,TMPPCT,XSDI,XBAMAX

      IF (DEBUG) WRITE (JOSTND,
     >   '('' IN SSTAGE. TMPTPA,TMPCCM,TMPGAP= '',
     >    F9.1,F9.1,F9.1)')
     >   TMPTPA,TMPCCM,TMPGAP

      ICRCV1 = IFIX(CRS1+.5)
      ICRCV2 = IFIX(CRS2+.5)
      ICRCV3 = IFIX(CRS3+.5)

   80 CONTINUE
C
      CALL COVOLP (DEBUG,JOSTND,NTREES,INDEX,WK6,COVER,CCCOEF)
C
      ICOVR = IFIX(COVER+.5)
      IF (NTREES.LE.1)ICRCV1=ICOVR

C     SET THE STRUCTURE CLASS AND LEAVE IF THE CALL IS FROM FFE.  OTHERWISE
C     STAY, PRINT, AND SET EVENT MONITOR VARIABLES.

      IF (DEBUG) WRITE (JOSTND,
     >     '('' IN SSTAGE. ISTRCL & DBHDOM= '',I2,F9.1)')
     >     TMPSCL,TMPDBH

  100 CONTINUE

      IF (FMFLAG .EQ. 1) THEN
        FMSTCL = TMPSCL
        FMDBH = TMPDBH
        RETURN
      ELSE
        ISTRCL = TMPSCL
        DBHDOM = TMPDBH
      ENDIF

      ICD = 0
      IF (IBA.NE.1) ICD = 1

      IF (DEBUG) WRITE (JOSTND,
     >     '('' IN SSTAGE. ISTRCL & DBHDOM= '',I2,F9.1)')
     >     ISTRCL,DBHDOM
      IF (IBA.EQ.1) THEN
         XHTLS=FLOAT(IHTLS)
         XHTSS=FLOAT(IHTSS)
         XNSTR=FLOAT(NSTR)
         CALL EVSET4(16,FLOAT(ISTRCL))
         CALL EVSET4(18,DBHDOM)
         CALL EVSET4(24,FLOAT(ICOVR))
         CALL EVSET4(40,XHTLS)
         CALL EVSET4(42,XHTSS)
         CALL EVSET4(44,XNSTR)
C
         CALL EVUST4(17)
         CALL EVUST4(19)
         CALL EVUST4(25)
         CALL EVUST4(41)
         CALL EVUST4(43)
         CALL EVUST4(45)
      ELSE
         XHTLS=FLOAT(IHTLS)
         XHTSS=FLOAT(IHTSS)
         XNSTR=FLOAT(NSTR)
         CALL EVSET4(17,FLOAT(ISTRCL))
         CALL EVSET4(19,DBHDOM)
         CALL EVSET4(25,FLOAT(ICOVR))
         CALL EVSET4(41,XHTLS)
         CALL EVSET4(43,XHTSS)
         CALL EVSET4(45,XNSTR)
      ENDIF


C     SET THE IOSTR, WHICH IS USED BY STRSTAT, AN EVENT MONITOR FUNCTION.
      IF (IBA .EQ. 1) THEN
        DO I = 1,33
          DO J = 1,2
            OSTRST(I,J) = 0
          ENDDO
        ENDDO
      ENDIF
      
      J = 1
      IF (IBA.NE.1) J=2
      OSTRST(1,J) = DBHS1              ! nominal dbh for stratum 1
      OSTRST(2,J) = FLOAT(IHTS1)       ! nominal height for stratum 1
      OSTRST(3,J) = FLOAT(IHTLS1)      ! height of the tallest tree in stratum 1
      OSTRST(4,J) = FLOAT(IHTSS1)      ! height of the shortest tree in stratum 1      
      OSTRST(5,J) = FLOAT(ICRBS1)      ! height to crown base for stratum 1
      OSTRST(6,J) = FLOAT(ICRCV1)      ! canopy cover for stratum 1
      OSTRST(7,J) = ISP11              ! major species 1 for stratum 1
      OSTRST(8,J) = ISP21              ! major species 2 for stratum 1
      OSTRST(9,J) = FLOAT(IS1OK)       ! stratum status code for stratum 1
      OSTRST(10,J) = TPA1              ! trees per acre for stratum 1
      OSTRST(11,J) = DBHS2             ! nominal dbh for stratum 2
      OSTRST(12,J) = FLOAT(IHTS2)      ! nominal height for stratum 2
      OSTRST(13,J) = FLOAT(IHTLS2)     ! height of the tallest tree in stratum 2
      OSTRST(14,J) = FLOAT(IHTSS2)     ! height of the shortest tree in stratum 2         
      OSTRST(15,J) = FLOAT(ICRBS2)     ! height to crown base for stratum 2
      OSTRST(16,J) = FLOAT(ICRCV2)     ! canopy cover for stratum 2
      OSTRST(17,J) = ISP12             ! major species 1 for stratum 2
      OSTRST(18,J) = ISP22             ! major species 2 for stratum 2
      OSTRST(19,J) = FLOAT(IS2OK)      ! stratum status code for stratum 2
      OSTRST(20,J) = TPA2              ! trees per acre for stratum 2
      OSTRST(21,J) = DBHS3             ! nominal dbh for stratum 3
      OSTRST(22,J) = FLOAT(IHTS3)      ! nominal height for stratum 3
      OSTRST(23,J) = FLOAT(IHTLS3)     ! height of the tallest tree in stratum 3
      OSTRST(24,J) = FLOAT(IHTSS3)     ! height of the shortest tree in stratum 3             
      OSTRST(25,J) = FLOAT(ICRBS3)     ! height to crown base for stratum 3
      OSTRST(26,J) = FLOAT(ICRCV3)     ! canopy cover for stratum 3
      OSTRST(27,J) = ISP13             ! major species 1 for stratum 3
      OSTRST(28,J) = ISP23             ! major species 2 for stratum 3
      OSTRST(29,J) = FLOAT(IS3OK)      ! stratum status code for stratum 3
      OSTRST(30,J) = TPA3              ! trees per acre for stratum 3
      OSTRST(31,J) = NSTR              ! number of strata
      OSTRST(32,J) = FLOAT(ICOVR)      ! canopy cover
      OSTRST(33,J) = FLOAT(ISTRCL)     ! structure class

      DBSKODE = 1
      CALL DBSSTRCLASS(IY(ICYCLE), NPLT, ICD,
     >         DBHS1,IHTS1,IHTLS1,IHTSS1,ICRBS1,ICRCV1,SP11,SP21,IS1OK,
     >         DBHS2,IHTS2,IHTLS2,IHTSS2,ICRBS2,ICRCV2,SP12,SP22,IS2OK,
     >         DBHS3,IHTS3,IHTLS3,IHTSS3,ICRBS3,ICRCV3,SP13,SP23,IS3OK,
     >         NSTR,ICOVR,SSCODES(ISTRCL+1), DBSKODE, NTREES)
      IF(DBSKODE.EQ.0) RETURN

      IF (ICYCLE.EQ.1) THEN
            IF (.NOT. LSUPRT .AND. LPRNT .AND. IRREF.EQ.-1) THEN
               CALL GETID (IRREF)
               CALL GETLUN(IOUT)
               WRITE (IOUT,85) IRREF,NPLT,MGMID,
     >              (' ------------ Stratum',I,' ------------',I=1,3)
   85          FORMAT (1X,I5,' $#*%'
     >                 //'Structural statistics for stand: ',A,
     >                  '  MgmtID: ',A//T8,3(A,I2,A)/
     >      '     Rm',3('       ---Height-- -Crown- -Major- C'),
     >      ' N Tot Struc'/
     >      'Year Cd',3('  DBH  Nom  Lg  Sm Bas Cov Sp1 Sp2 D'),
     >      ' S Cov Class'/
     >      '---- --',3(' ----- --- --- --- --- --- --- --- -'),
     >      ' - --- -----'
     >                 /'$#*%')
            ENDIF
      ENDIF
C
      IF (IRREF.GT.0 .AND. .NOT. LSUPRT .AND. NTREES .GT. 0) THEN
         CALL GETLUN(IOUT)
         WRITE (IOUT,90) IRREF,IY(ICYCLE),ICD,
     >         DBHS1,IHTS1,IHTLS1,IHTSS1,ICRBS1,ICRCV1,SP11,SP21,IS1OK,
     >         DBHS2,IHTS2,IHTLS2,IHTSS2,ICRBS2,ICRCV2,SP12,SP22,IS2OK,
     >         DBHS3,IHTS3,IHTLS3,IHTSS3,ICRBS3,ICRCV3,SP13,SP23,IS3OK,
     >         NSTR,ICOVR,SSCODES(ISTRCL+1)
   90    FORMAT (1X,I5,1X,I4,I3,3(1X,F5.1,1X,I3,1X,I3,1X,
     >           I3,1X,I3,1X,I3,1X,A3,1X,A3,1X,I1),1X,I1,1X,I3,2X,A4)
      ENDIF
C
    
      RETURN
      END
C
      SUBROUTINE SSTGHP (I1,I2,INDEX,WK6,WK4,DBH,HT,TMPICR,ISP,TMPPRB,
     >          MAXTRE,MAXSP,DBHNOM,IHT,IHTS,IHTL,ICRB,MSP1,MSP2)

C     THIS IS A PRIVATE HELPER ROUTINE CALLED BY SSTAGE
C     (AND, PLEASE, NO OTHER ROUTINES SHOULD CALL THIS ROUTINE).

      REAL TMPPRB
      INTEGER TMPICR

      DIMENSION INDEX(MAXTRE),WK6(MAXTRE),WK4(MAXTRE),DBH(MAXTRE),
     >          HT(MAXTRE),TMPICR(MAXTRE),ISP(MAXTRE),TMPPRB(MAXTRE)

C     INITIALIZE.

      DO I=1,MAXSP
         WK4(I)=0.
      ENDDO

      MSP1 = 0
      MSP2 = 0
      ACB  = 0.
      SP   = 0.
      SUM  = 0.

      IF (I1.EQ.0 .OR. I2.EQ.0) RETURN

C     PICK THE LARGEST AND SMALLEST TREES IN THE STRATUM.

      IHTL = IFIX(HT(INDEX(I1))+.5)
      IHTS = IFIX(HT(INDEX(I2))+.5)

C     COMPUTE THE NOMINAL DIAMETER AND HEIGHT FOR THE TREES IN THE
C     STRATUM.  ALSO, AND AVERAGE CROWN BASE HEIGHT AND TOTAL PERCENT
C     COVER.

C     FIRST, COMPUTE THE PERCENTILE IN THE CROWN COVER DISTRIBUTION.
C     CUT OFF THE LIST OF TREES IN THE STRATUM AT 95% COVER. ...
C     WHILE WE ARE AT IT, COMPUTE THE AVERAGE CROWN BASE HT AND
C     FIND THE SUM OF BA BY SPECIES.

      ITOP = I2
      I3 = -1
      DO II=I1,ITOP
         I=INDEX(II)

C        SUM IS THE SUM OF COVER, SP IS THE SUM OF PROB FOR THE
C        TREES IN THE STRATUM, AND ACB IS THE AVERAGE CROWN BASE HT.

         SUM = SUM+WK6(I)
         SP  = SP +TMPPRB(I)
         ACB = ACB+((HT(I)*(1.-FLOAT(TMPICR(I))*.01))*TMPPRB(I))

C        SUM THE COVER BY SPECIES.

         IS=ISP(I)
         WK4(IS)=WK4(IS)+(WK6(I))

C        41382 = .95 * 43560  ... SET I3 TO POINT TO 95% COVER CUT OFF.
C        (THIS PART OF THE ROUTINE DOES NOT ACCOUNT FOR OVERLAP).

         IF (SUM.GT.41382. .AND. I3.EQ.-1) I3 = II
      ENDDO

C     IF THERE WAS LESS THAN 95% COVER, I3 SHOULD BE EQUAL TO I2.

      IF (I3.EQ.-1) I3=I2

C     FIND THE SPECIES INDICES FOR THE TOP 2 CONTRIBUTORS TO COVER
C     (NOTE: THE STARTING VALUES OF X1 AND X2 MUST BE ZERO OF THE
C     METHOD WILL NOT WORK).

      X1 = 0.
      X2 = 0.
      DO I=1,MAXSP
         IF (WK4(I).GT.X1) THEN
            X2 = X1
            X1 = WK4(I)
            MSP2 = MSP1
            MSP1 = I
         ELSEIF (WK4(I).GT.X2) THEN
            X2 = WK4(I)
            MSP2 = I
         ENDIF
      ENDDO

C     COMPUTE THE AVERAGE CROWN BASE.

      ICRB = 0
      IF (SP .GT. 0.0001) ICRB = IFIX(ACB/SP+.5)

      DO II=I1,I3
         I=INDEX(II)
         WK4(I)=WK6(I)/TMPPRB(I)
      ENDDO

      CALL RDPSRT (I3-I1+1,WK4,INDEX(I1),.FALSE.)
      CALL PCTILE (I3-I1+1,INDEX(I1),WK6,WK4,T)

C     FIND THE 70 PERCENTILE TREE... (30 %TILE "DOWN FROM THE TOP").

      I70 = 0
      DIFF = 1E30
      DO II=I1,I3
         I=INDEX(II)
         IF (ABS(WK4(I)-70.) .LT. DIFF) THEN
            I70 = II
            DIFF = ABS(WK4(I)-70.)
         ENDIF
      ENDDO

      K1 = I70-4
      IF (K1.LT.I1) K1 = I1
      K2 = I70+4
      IF (K2.GT.I3) K2 = I3

      SD = 0.
      SH = 0.
      SP = 0.

      DO II=K1,K2
         I=INDEX(II)
         SD=SD+(DBH(I)*TMPPRB(I))
         SH=SH+(HT(I) *TMPPRB(I))
         SP=SP+TMPPRB(I)
      ENDDO

      DBHNOM = 0.
      IHT    = 0
      IF (SP.GT. .0001) THEN
         DBHNOM = SD/SP
         IHT    = IFIX(SH/SP+.5)
      ENDIF

      RETURN
      END

      SUBROUTINE SSTGHTPA (I1,I2,INDEX,TMPPRB,MAXTRE,STRTPA)

C     THIS IS A PRIVATE HELPER ROUTINE CALLED BY SSTAGE
C     (AND, PLEASE, NO OTHER ROUTINES SHOULD CALL THIS ROUTINE).

C     THIS ROUTINE COMPUTES THE TREES PER ACRE IN EACH STRATUM.
C     IT IS DIFFERENT THAN THE OTHER HELPER ROUTINE (SSTGHP) BECAUSE
C     I WANTED TO INCLUDE THE TREES IN THE "GAPS" BETWEEN THE STRATA
C     (LIKE THE COVER CALCULATIONS DO).  SAR - AUG 2006

      REAL TMPPRB

      DIMENSION INDEX(MAXTRE),TMPPRB(MAXTRE)

      SP   = 0.

      IF (I1.EQ.0 .OR. I2.EQ.0) RETURN

      DO II=I1,I2
         I=INDEX(II)
         SP  = SP +TMPPRB(I)
      ENDDO

      STRTPA = SP

      RETURN
      END
