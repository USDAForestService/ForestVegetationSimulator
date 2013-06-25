      SUBROUTINE EVLDX (XLDREG,NXLDX,INSTR,IRC)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     CALLED FROM ALGEVL
C
C     LOADS X VALUES OWNED BY THE PROGNOSIS MODEL AS OPPOSED TO PPE.
C
C     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID
C
C     XLDREG= THE VALUE WANTED IS RETURNED AS XLDREG(1).
C     NXLDX = THE LENGTH OF THE XLDREG ARRAY.
C     INSTR = THE CODE THAT SAYS WHICH VALUE IS REQUESTED.
C     IRC   = RETURN CODE, 0=OK, 1=VARIABLE IS CURRENTLY UNDEFINED,
C             2=INSTRUCTION CODE COULD NOT BE DECIPHERED.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'OPCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'STDSTK.F77'
      INCLUDE 'WORKCM.F77'
      INCLUDE 'SSTGMC.F77'
      INCLUDE 'METRIC.F77'

COMMONS

      INTEGER M, I1,I2,I3,I4,INDEX(MAXTRE),ISRTI
      LOGICAL LDEB,LACTV,LINCL
      REAL XLDREG(NXLDX),DUMSDI
      INTEGER ITMPDX(1)
      INTEGER IHI,ILO,ISPC,IDMR,IG,IULIM,ILIM,NTREES,IDMI,K
      INTEGER IRC,INSTR,NXLDX,I,J,JARGS,MYSTR,L,IGRP
      REAL STAGEB,STAGEA,XSDI,CUT,DEAD,RES,XLDBH,XHDBH,SUMP,TPA,TREERD
      REAL TEMSUM,RVAL,XLHT,XHHT,HTPCT,P,SUMPIN,X
      INTEGER JPNUM,IGSP,IPNTR(MAXTRE)
      EXTERNAL RANN
      REAL TPA3,DCM,ADIV,HPCT(MAXTRE),JUNK,BADJ,ACRN

      ITMPDX(1) = 0

      CALL DBCHK (LDEB,'EVLDX',5,ICYC)
      IF (LDEB) THEN
         WRITE (JOSTND,5) INSTR,NXLDX
    5    FORMAT (/' IN EVLDX, INSTR=',I6,'; NXLDX=',I3)
         IF (NXLDX.GT.0) WRITE (JOSTND,6) (XLDREG(I),I=1,MIN(10,NXLDX))
    6    FORMAT (' IN EVLDX: ARGUMENTS= ',10F7.2)
      ENDIF
C----------
C     DECODE THE INSTRUCTION AND EXECUTE: LOAD A CONSTANT.
C----------
      IF (INSTR.GT.1000 .AND. INSTR.LT. 7000) THEN
         XLDREG(1)=PARMS(INSTR-1000)
         GOTO 1000
      ENDIF
C----------
C     DECODE THE INSTRUCTION AND EXECUTE: LOAD A SAVED VARIABLE.
C----------
      IF (INSTR.LT.1000) THEN
        I=MOD(INSTR,100)
        J=INSTR/100
        IF (LDEB) WRITE (JOSTND,'('' IN EVLDX, I, J='',2I4)') I,J
        GOTO (1001,11,12,13,14,15,16,17,18,19,1001),(J+1)
   11   CONTINUE
        GOTO (1100,1100,1101,1102,1102,1100,1103,1101,1104,1105,
     +        1100,1100,1100,1100,1101,1101,1101,1101,1101,1101,
     +        1101,1101,1101,1101,1101,1100,1100,1100,1104,1100,
     +        1100,1100,1100,1100,1104,1100,1101,1100,1100,1100) I
 1100   CONTINUE
        XLDREG(1) = TSTV1(I)
        GOTO 1000
 1101   CONTINUE
        XLDREG(1) = TSTV1(I) / ACRtoHA
        GOTO 1000
 1102   CONTINUE
        XLDREG(1) = TSTV1(I) * FT3pACRtoM3pHA
        GOTO 1000
 1103   CONTINUE
        XLDREG(1) = TSTV1(I) * FT2pACRtoM2pHA
        GOTO 1000
 1104   CONTINUE
        XLDREG(1) = TSTV1(I) * FTtoM
        GOTO 1000
 1105   CONTINUE
        XLDREG(1) = TSTV1(I) * INtoCM
        GOTO 1000

   12   CONTINUE
        IF (IPHASE.LT.2) GOTO 1001
        GOTO (1201,1202,1202,1200,1203,1200,1204,1205,1201,1202,
     +        1202,1200,1200,1200,1200,1200,1200,1200,1200,1200,
     +        1200,1200,1200,1201,1200) I
 1200   CONTINUE
        XLDREG(1) = TSTV2(I)
        GOTO 1000
 1201   CONTINUE
        XLDREG(1) = TSTV2(I) / ACRtoHA
        GOTO 1000
 1202   CONTINUE
        XLDREG(1) = TSTV2(I) * FT3pACRtoM3pHA
        GOTO 1000
 1203   CONTINUE
        XLDREG(1) = TSTV2(I) * FT2pACRtoM2pHA
        GOTO 1000
 1204   CONTINUE
        XLDREG(1) = TSTV2(I) * FTtoM
        GOTO 1000
 1205   CONTINUE
        XLDREG(1) = TSTV2(I) * INtoCM
        GOTO 1000

   13   CONTINUE
        IF (ICYC.LT.2) GOTO 1001
        GOTO (1302,1302,1302,1302,1301,1300,1303,1300,1300,1300,
     +        1300,1300,1300,1300,1300,1300,1300,1300,1300,1300) I
 1300   CONTINUE
        XLDREG(1) = TSTV3(I)
        GOTO 1000
 1301   CONTINUE
        XLDREG(1) = TSTV3(I) / ACRtoHA
        GOTO 1000
 1302   CONTINUE
        XLDREG(1) = TSTV3(I) * FT3pACRtoM3pHA
        GOTO 1000
 1303   CONTINUE
        XLDREG(1) = TSTV3(I) * FT2pACRtoM2pHA
        GOTO 1000

   14   CONTINUE
        IF (.NOT.LTSTV4(I)) GOTO 1001
        GOTO (1400,1400,1400,1400,1400,1400,1400,1400,1400,1400,
     +        1400,1400,1400,1400,1400,1400,1400,1401,1401,1400,
     +        1400,1402,1400,1400,1400) I
 1400   CONTINUE
        XLDREG(1) = TSTV4(I)
        GOTO 1000
 1401   CONTINUE
        XLDREG(1) = TSTV4(I) * INtoCM
        GOTO 1000
 1402   CONTINUE
        XLDREG(1) = TSTV4(I) * FTtoM
        GOTO 1000

   15   CONTINUE
        IF (.NOT.LTSTV5(I)) GOTO 1001
        XLDREG(1)=TSTV5(I)
        GOTO 1000
   16   CONTINUE
        IF (.NOT.LTSTV5(I+100)) GOTO 1001
        XLDREG(1)=TSTV5(I+100)
        GOTO 1000
   17   CONTINUE
        IF (.NOT.LTSTV5(I+200)) GOTO 1001
        XLDREG(1)=TSTV5(I+200)
        GOTO 1000
   18   CONTINUE
        IF (.NOT.LTSTV5(I+300)) GOTO 1001
        XLDREG(1)=TSTV5(I+300)
        GOTO 1000
   19   CONTINUE
        IF (I.EQ.0) THEN
           CALL RANN (XLDREG(1))
        ELSE
           XLDREG(1)=FLOAT(I)
        ENDIF
        GOTO 1000
      ENDIF
C----------
C     IF THE INSTRUCTION CODE IS LT 10000, THEN IT CAN NOT BE
C     PROCESSED BY THIS PART OF THIS SUBROUTINE.
C----------
      IF (INSTR.LE.10000) GOTO 1002
C----------
C     THE INSTRUCTION CODE IS GT 10000, THEN SET THE NUMBER OF
C     ARGUMENTS TO THE OPCODE (JARGS) AND SET MYSTR TO THE INSTRUCTION
C     LESS THE ARGUMENT COUNT.
C----------
      JARGS=MOD(INSTR,100)
      MYSTR=INSTR/100*100
C----------
C  DECODE INSTRUCTION AND EXECUTE.    SPMCDBH:
C   1ST ARGUMENT:
C      1 = TREES PER ACRE
C      2 = BASAL AREA PER ACRE
C      3 = TOTAL CUBIC FOOT VOLUME PER ACRE
C      4 = TOTAL MERCH BOARD FOOT VOLUME PER ACRE
C      5 = QUADRATIC MEAN DIAMETER
C      6 = AVERAGE HEIGHT
C      7 = PERCENT COVER
C      8 = AVERAGE DWARF MISTLETOE RATING
C      9 = TOTAL MERCH CUBIC FOOT VOLUME PER ACRE (WESTERN VARIANTS)
C        = TOTAL SAWTIMBER CUBIC FOOT VOLUME PER ACRE (EASTERN VARIANTS)
C     10 = AVERAGE DIAMETER GROWTH INSIDE BARK
C     11 = STAND DENSITY INDEX
C     12 = SILVAH RELATIVE DENSITY
C     13 = ZEIDE STAND DENSITY INDEX
C   2ND ARGUMENT:
C         0 = ALL SPECIES
C        -X = SPECIES GROUP X
C
C  VARIABLE LINCL IS USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
C  THE CALCULATION OR NOT.
C----------
      IF (MYSTR.EQ.10600) THEN
        IF (JARGS.LT.3) GOTO 1002
        L=IFIX(XLDREG(1)+.5)
        IF(XLDREG(2).GE. 0.)THEN
          J=IFIX(XLDREG(2)+.5)
        ELSE
          J=IFIX(XLDREG(2)-.5)
        ENDIF
        K=IFIX(XLDREG(3)+.5)
CC----------
C       IF THE ATTRIBUTE (L) IS OUT OF RANGE OR UNDEFINED IN THE
C       PHASE, THEN: ISSUE ERROR CODE.
C----------
        IF (L.LE.0 .OR. L.GT.13) GOTO 1002
C----------
C       IF THE SPECIES (J) IS OUT OF RANGE, THEN: ISSUE ERROR CODE.
C       NOTE: A NEGATIVE SPECIES NUMBER INDICATES A SPECIES GROUP
C----------
        IF (J.GT.MAXSP) GOTO 1002
        IF (J.LT.0 .AND. NSPGRP.LT.-J) GOTO 1002
C----------
C       IF THE MANAGEMENT CODE (K) IS OUT OF RANGE, THEN: ISSUE ERROR
C       CODE.
C----------
        IF (K.LT.0 .OR. K.GT.3) GOTO 1002
C----------
C       IF CALCULATING STAGE'S SDI, GET THE STAGE PARAMETERS
C----------
        IF(L.EQ.11)CALL SDICLS(0,0.,999.,1,XSDI,DUMSDI,STAGEA,STAGEB,0)
C----------
C       FIND THE DBH RANGE AND THE HT RANGE.
C----------
        CUT=0.
        DEAD=0.
        RES=0.
        IDMI=0.
        XLDBH=0.0
        XHDBH=1E30
        XLHT=0.0
        XHHT=1E30
        JPNUM=0
        IF (JARGS.GE.4) XLDBH = XLDREG(4) * CMtoIN
        IF (JARGS.GE.5) XHDBH = XLDREG(5) * CMtoIN
        IF (JARGS.GE.6) XLHT  = XLDREG(6) * MtoFT
        IF (JARGS.GE.7) XHHT  = XLDREG(7) * MtoFT
        IF (JARGS.GE.8 .AND. XLDREG(8).EQ.1) DEAD=XLDREG(8)
        IF (JARGS.GE.8 .AND. XLDREG(8).EQ.2) CUT=XLDREG(8)
        IF (JARGS.GE.8 .AND. XLDREG(8).EQ.3) RES=XLDREG(8)
        IF (JARGS.GE.8 .AND. XLDREG(8).EQ.4) IDMI=IFIX(XLDREG(8)+0.5)
        IF (JARGS.GE.9) JPNUM=IFIX(XLDREG(9)+0.5)
C
C IF THE POINT NUMBER IS PRESENT, AND VALID, AND A POINT NUMBER FROM
C THE INVENTORY DATA, CONVERT IT TO THE CORRESPONDING FVS SEQUENTIAL
C POINT NUMBER
C
        XLDREG(1)=0.0
        IF(JPNUM.NE.0)THEN
          IF(ITHNPI .LE. 0 .OR. ITHNPI.GT.2)THEN
            GO TO 1000
          ELSEIF(ITHNPI .EQ. 1)THEN
            DO I=1,IPTINV
            IF(JPNUM .EQ. IPVEC(I))GO TO 89
            ENDDO
            GO TO 1000
   89       CONTINUE
            JPNUM=I
          ELSEIF(ITHNPI .EQ. 2)THEN
            IF(JPNUM .GT. IPTINV)THEN
            GO TO 1000
            ENDIF
          ENDIF
        ENDIF
        IF(LDEB)WRITE(JOSTND,*)' AFTER POINT PROCESSING JPNUM= ',JPNUM
C
        SUMP=0.0
        NTREES=0
        IF(CUT.GT.0. .AND. IPHASE.LT.2) GOTO 1001
        IF(RES.GT.0. .AND. IPHASE.LT.2) GO TO 1001
        ILIM=ITRN
        IF(CUT.NE. 0) ILIM=MAXTRE
        IF (ILIM.GT.0) THEN
          DO 190 I=1,ILIM
          LINCL = .FALSE.
          IF(J.EQ.0 .OR. J.EQ.ISP(I))THEN
            LINCL = .TRUE.
          ELSEIF(J.LT.0)THEN
            IGRP = -J
            IULIM = ISPGRP(IGRP,1)+1
            DO 90 IG=2,IULIM
            IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
              LINCL = .TRUE.
              GO TO 91
            ENDIF
   90       CONTINUE
          ENDIF
   91     CONTINUE
          IF(JPNUM.GT.0 .AND. JPNUM.NE.ITRE(I)) LINCL=.FALSE.
          IF (LINCL .AND.
     >       (K.EQ.0  .OR. K.EQ.IMC(I)) .AND.
     >       (DBH(I).GE.XLDBH)          .AND.
     >       (DBH(I).LT.XHDBH)          .AND.
     >       (HT(I).GE.XLHT)            .AND.
     >       (HT(I).LT.XHHT))           THEN
C
               TPA=PROB(I)
               IF(DEAD.NE.0.)THEN
             IF(ICYC.LE.1)THEN
               TPA=0.
             ELSE
               TPA=WK2(I)
             ENDIF
          ELSEIF (CUT.NE.0.)THEN
             TPA=WK4(I)
          ELSEIF (IDMI.NE.0.)THEN
            CALL MISGET(I,IDMR)
            IF(IDMR .EQ. 0)TPA=0.
          ENDIF
          IF(JPNUM.GT.0)TPA=TPA*(PI-FLOAT(NONSTK))
C
          SUMP=SUMP+TPA
          GOTO (111,112,113,114,115,116,117,118,119,120,121,122,123),L
  111       CONTINUE
              XLDREG(1)=XLDREG(1)+TPA
              GOTO 190
  112       CONTINUE
              XLDREG(1)=XLDREG(1)+(TPA*DBH(I)*DBH(I)*.005454154)
              GOTO 190
  113       CONTINUE
              IF(DEAD.NE.0.) THEN
                XLDREG(1)=XLDREG(1)+(TPA*PTOCFV(I))
              ELSE
                XLDREG(1)=XLDREG(1)+(TPA*CFV(I))
              ENDIF
              GOTO 190
  114         CONTINUE
              IF(DEAD.NE.0.) THEN
                 XLDREG(1)=XLDREG(1)+(TPA*PMRBFV(I))
               ELSE
                 XLDREG(1)=XLDREG(1)+(TPA*BFV(I))
              ENDIF
              GOTO 190
  115         CONTINUE
              XLDREG(1)=XLDREG(1)+(TPA*DBH(I)*DBH(I))
              GOTO 190
  116         CONTINUE
              XLDREG(1)=XLDREG(1)+(TPA*HT(I))
              GOTO 190
  117         CONTINUE
              NTREES = NTREES+1
              WORK1(NTREES)=CRWDTH(I)
C----------
C                  NOTE:  0.785398 DEALS WITH PI*TRECW/2*TRECW/2
C----------
              WORK1(NTREES)=WORK1(NTREES)*WORK1(NTREES)*TPA*0.785398
              GOTO 190
  118         CONTINUE
              CALL MISGET(I,IDMR)
              XLDREG(1)=XLDREG(1)+(FLOAT(IDMR)*TPA)
              GOTO 190
  119         CONTINUE
              IF(DEAD.NE.0.) THEN
                XLDREG(1)=XLDREG(1)+(TPA*PMRCFV(I))
              ELSE
                XLDREG(1)=XLDREG(1)+(TPA*WK1(I))
              ENDIF
              GOTO 190
  120         CONTINUE
              XLDREG(1)=XLDREG(1)+(TPA*DG(I))
              GOTO 190
  121         CONTINUE
              XLDREG(1)=XLDREG(1)+(STAGEA + STAGEB*(DBH(I)**2.0))*TPA
              GO TO 190
  122         CONTINUE
              CALL RDSLTR(ISP(I),I,TREERD)
              XLDREG(1)=XLDREG(1)+(TPA*TREERD)/GROSPC
              GO TO 190
  123         CONTINUE
              XLDREG(1)=XLDREG(1)+((DBH(I)/10)**1.605)*TPA
C
            ENDIF
  190     CONTINUE
  192     CONTINUE
          IF (L.EQ.5 .OR. L.EQ.6 .OR. L.EQ.8 .OR. L.EQ.10) THEN
            IF (SUMP.GT.0.0001) THEN
              IF (L.EQ.5) XLDREG(1)=SQRT(XLDREG(1)/SUMP)
              IF (L.EQ.6 .OR. L.EQ.8 .OR. L.EQ.10)
     &          XLDREG(1)=XLDREG(1)/SUMP
            ELSE
              XLDREG(1)=0.0
            ENDIF
          ELSEIF (L.EQ.7) THEN
            CALL COVOLP (LDEB,JOSTND,NTREES,ITMPDX,WORK1,XLDREG(1))
          ELSEIF ((L.GE.1 .AND. L.LE.4) .OR. L.EQ.9) THEN
            XLDREG(1)=XLDREG(1)/GROSPC
          ENDIF

          GOTO (911,912,913,914,915,916,917,918,919,920,921,922,923), L

  911     CONTINUE
          XLDREG(1) = XLDREG(1) / ACRtoHA
          GOTO 1000
  912     CONTINUE
          XLDREG(1) = XLDREG(1) * FT2pACRtoM2pHA
          GOTO 1000
  913     CONTINUE
          XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
          GOTO 1000
  914     CONTINUE
          XLDREG(1) = XLDREG(1)
          GOTO 1000
  915     CONTINUE
          XLDREG(1) = XLDREG(1) * INtoCM
          GOTO 1000
  916     CONTINUE
          XLDREG(1) = XLDREG(1) * FTtoM
          GOTO 1000
  917     CONTINUE
          XLDREG(1) = XLDREG(1)
          GOTO 1000
  918     CONTINUE
          XLDREG(1) = XLDREG(1)
          GOTO 1000
  919     CONTINUE
          XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
          GOTO 1000
  920     CONTINUE
          XLDREG(1) = XLDREG(1) * INtoCM
          GOTO 1000
  921     CONTINUE
          XLDREG(1) = XLDREG(1) / ACRtoHA
          GOTO 1000
  922     CONTINUE
          XLDREG(1) = XLDREG(1) / ACRtoHA
          GOTO 1000
  923     CONTINUE
          XLDREG(1) = XLDREG(1) / ACRtoHA
          GOTO 1000
        ENDIF
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    ACCFSP & BCCFSP:
C----------
      IF (MYSTR.EQ.11200.OR.MYSTR.EQ.11300) THEN
        IF (JARGS.NE.1) GOTO 1002
        IF(XLDREG(1).GE. 0.)THEN
          I=IFIX(XLDREG(1)+.5)
        ELSE
          I=IFIX(XLDREG(1)-.5)
        ENDIF
        IF (I.GT.MAXSP) GOTO 1002
        IF (I.LT.0 .AND. NSPGRP.LT.-I) GOTO 1002
        IF (MYSTR.EQ.11300 .AND. IPHASE.LT.2) GOTO 1001
        IF(I .LT. 0)THEN
          IGRP = -I
          IULIM = ISPGRP(IGRP,1)+1
          TEMSUM=0.
          DO 191 IG=2,IULIM
          IGSP = ISPGRP(IGRP,IG)
          IF (MYSTR.EQ.11200) THEN
            TEMSUM = TEMSUM + BCCFSP(IGSP)
          ELSE
            TEMSUM = TEMSUM + ACCFSP(IGSP)
          ENDIF
  191     CONTINUE
          XLDREG(1)=TEMSUM
        ELSEIF(I .EQ. 0)THEN
          TEMSUM=0.
          DO 195 ISPC=1,MAXSP
          IF (MYSTR.EQ.11200) THEN
            TEMSUM = TEMSUM + BCCFSP(ISPC)
          ELSE
            TEMSUM = TEMSUM + ACCFSP(ISPC)
          ENDIF
  195     CONTINUE
          XLDREG(1)=TEMSUM
        ELSE
          IF (MYSTR.EQ.11200) THEN
            XLDREG(1)=BCCFSP(I)
          ELSE
           XLDREG(1)=ACCFSP(I)
          ENDIF
        ENDIF
        GO TO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    DBHDIST:
C----------
      IF (MYSTR.EQ.10500) THEN
         IF (JARGS.LT.2) GOTO 1002
         I=IFIX(XLDREG(1)+.5)
         J=IFIX(XLDREG(2)+.5)
C----------
C        IF THE ATTRIBUTE (I) IS OUT OF RANGE OR UNDEFINED IN THE
C        PHASE, THEN: ISSUE ERROR CODE.
C----------
         IF (I.LE.0 .OR. I.GT.11) GOTO 1002
         IF (I.GE.7 .AND. IPHASE.EQ.1) GOTO 1001
         IF (J.LE.0 .OR. J.GT. 7) GOTO 1002
         GOTO (201,202,203,204,205,206,207,208,209,210,211), I
  201    CONTINUE
         XLDREG(1) = OACC(J)
         GOTO 1999
  202    CONTINUE
         XLDREG(1) = OMORT(J)
         GOTO 1999
  203    CONTINUE
         XLDREG(1) = ONTCUR(J)
         GOTO 1999
  204    CONTINUE
         XLDREG(1) = OCVCUR(J)
         GOTO 1999
  205    CONTINUE
         XLDREG(1) = OMCCUR(J)
         GOTO 1999
  206    CONTINUE
         XLDREG(1) = OBFCUR(J)
         GOTO 1999
  207    CONTINUE
         XLDREG(1) = ONTREM(J)
         GOTO 1999
  208    CONTINUE
         XLDREG(1) = OCVREM(J)
         GOTO 1999
  209    CONTINUE
         XLDREG(1) = OMCREM(J)
         GOTO 1999
  210    CONTINUE
         XLDREG(1) = OBFREM(J)
         GOTO 1999
  211    CONTINUE
         XLDREG(1) = ONTRES(J)
         GOTO 1999

 1999    CONTINUE
 2000    CONTINUE
         GOTO 1000
 2001    CONTINUE
         XLDREG(1) = XLDREG(1) / ACRtoHA
         GOTO 1000
 2002    CONTINUE
         XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
         GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    SUMSTAT:
C----------
      IF (MYSTR.EQ.10400) THEN
        IF(LDEB) WRITE(JOSTND,310) (J,(IOSUM(J,I),J=1,17),I=1,ICYC+1)
  310   FORMAT (' IN EVLDX: CURRENT IOSUM:'/((1X,I3,17I7)))
        IF (JARGS.LT.2) GOTO 1002
        I=IFIX(XLDREG(1)+.5)
        J=IFIX(XLDREG(2)+.5)
        IF (I.LE.0 .OR. I.GT.MAXCY1) GOTO 1001
        IF (J.LE.0 .OR. J.GT.17) GOTO 1002
        IF (I.GT.ICYC+1) GOTO 1001
        GOTO(3000,3000,3001,3002,3002,3000,3001,3002,3002,3000,
     +       3003,3000,3004,3000,3002,3002,3000), J
 3000   CONTINUE
        XLDREG(1) = IOSUM(J,I)
        GOTO 1000
 3001   CONTINUE
        XLDREG(1) = IOSUM(J,I) / ACRtoHA
        GOTO 1000
 3002   CONTINUE
        XLDREG(1) = IOSUM(J,I) * FT3pACRtoM3pHA
        GOTO 1000
 3003   CONTINUE
        XLDREG(1) = IOSUM(J,I) * FT2pACRtoM2pHA
        GOTO 1000
 3004   CONTINUE
        XLDREG(1) = IOSUM(J,I) * FTtoM
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    COARSEWD:
C----------
      IF (MYSTR.EQ.11700) THEN
C----------
C     RETURN IF FIRE MODEL IS INACTIVE
C----------
      CALL FMATV(LACTV)
      IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.2) GOTO 1002
C----------
C     ONE OR TWO ARGS CAN BE GIVEN
C----------
        ILO = IFIX(XLDREG(1)+.5)
        IF (JARGS.EQ.1) THEN
          IHI = ILO
        ELSE
          IHI = IFIX(XLDREG(2)+.5)
        ENDIF
C----------
C     CHECK RANGE AND VALUES: MUST BE 1-11 (SEE DIMENSIONING OF 2ND
C     DIMENSION OF CWD ARRAY IN FMCOM.F77)
C----------
        IF (IHI.LT.ILO) GOTO 1002
        IF (ILO.LT.1 .OR. IHI.GT.11) GOTO 1002
C
        CALL FMEVCWD(RVAL, ILO, IHI, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
C
        XLDREG(1) = XLDREG(1) * TItoTM / ACRtoHA
        GOTO 1000
      ENDIF

C----------
C     DECODE INSTRUCTION AND EXECUTE.    SNAGS:
C----------
      IF (MYSTR.EQ.11800) THEN
C----------
C     RETURN IF FIRE MODEL IS INACTIVE
C----------
      CALL FMATV(LACTV)
      IF (.NOT.LACTV) GOTO 1002

      IF (JARGS.LT.3) GOTO 1002
        L=IFIX(XLDREG(1)+.5)
        J=IFIX(XLDREG(2)+.5)
        K=IFIX(XLDREG(3)+.5)
C----------
C     CHECK BOUNDARIES FOR
C     (L=1:3)  TPA,BA OR VOL
C     (J=0:11) ALL,SPP
C     (K=0:2)  ALL,HARD,SOFT
C----------
        IF (L.LT.1 .OR. L.GT.3) GOTO 1002
        IF (J.LT.0 .OR. J.GT.MAXSP) GOTO 1002
        IF (K.LT.0 .OR. K.GT.2) GOTO 1002
C----------
C     FIND THE DBH RANGE AND THE HT RANGE.
C----------
        XLDBH=0.0
        XHDBH=1E30
        XLHT=0.0
        XHHT=1E30
        M=0
        IF (JARGS.GE.4) XLDBH = XLDREG(4)
        IF (JARGS.GE.5) XHDBH = XLDREG(5)
        IF (JARGS.GE.6) XLHT  = XLDREG(6)
        IF (JARGS.GE.7) XHHT  = XLDREG(7)
        IF (JARGS.GE.8) M     = XLDREG(8)

        IF ((M.EQ.1) .AND. (IPHASE.LT.2)) GOTO 1001

        XLDBH = XLDBH * CMtoIN
        XHDBH = XHDBH * CMtoIN
        XLHT = XLHT * MtoFT
        XHHT = XHHT * MtoFT

        CALL FMEVSNG(RVAL, L, J, K, XLDBH, XHDBH, XLHT, XHHT, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
C
        XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    POTFLEN:
C     1 = SEVERE (WILDFIRE) FLAME LENGTH
C     2 = MODERATE (PRESCRIBED) FLAME LENGTH
C----------
      IF (MYSTR.EQ.11900) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.2) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J = IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-8 (SEE DIMENSIONING OF 2ND
C       DIMENSION OF CWD ARRAY IN FMCOM.F77)
C----------
        IF (J. LT. 1 .OR. J .GT. 4) GOTO 1002
C
        CALL FMEVFLM(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL * FTtoM
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    POTFMORT:
C----------
      IF (MYSTR.EQ.12100) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-4
C----------
        IF (J. LT. 1 .OR. J .GT. 4) GOTO 1002
C
        CALL FMEVMRT(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001

        IF (J.LE.2) THEN
          XLDREG(1) = RVAL
        ELSE
          XLDREG(1) = RVAL * FT3pACRtoM3pHA
        ENDIF
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    FUELMODS:
C----------
      IF (MYSTR.EQ.12200) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.2) GOTO 1002
C----------
C       TWO ARGS CAN BE GIVEN
C----------
        L=IFIX(XLDREG(1)+.5)
        J=IFIX(XLDREG(2)+.5)
C----------
C       CHECK RANGE AND VALUES: ARG1 MUST BE 1-4, ARG2 MUST BE 1-2
C----------
        IF (L .LT. 1 .OR. L .GT. 4) GOTO 1002
        IF (J .LT. 1 .OR. J .GT. 2) GOTO 1002
        CALL FMEVFMD(RVAL, L, J, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    SALVVOL:
C----------
      IF (MYSTR.EQ.12300) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.3) GOTO 1002
C----------
C       CHECK BOUNDARIES FOR
C       (J=0:11) ALL,SPP
C----------
        IF(XLDREG(1).GE. 0.)THEN
          J=IFIX(XLDREG(1)+.5)
        ELSE
          J=IFIX(XLDREG(1)-.5)
        ENDIF
        J=IFIX(XLDREG(1)+.5)
        IF (J.LT.0 .OR. J.GT.MAXSP) GOTO 1002
C----------
C       FIND THE DBH RANGE
C----------
        XLDBH = XLDREG(2) * CMtoIN
        XHDBH = XLDREG(3) * CMtoIN

        CALL FMEVSAL(RVAL, J, XLDBH, XHDBH, I)
        IF ((I.EQ.1) .OR. (IPHASE.LT.2)) GOTO 1001
        XLDREG(1) = RVAL * FT3pACRtoM3pHA
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    POINTID:
C----------
      IF (MYSTR.EQ.12400) THEN
        IF (JARGS.NE.1) GOTO 1002
        I=IFIX(XLDREG(1)+.5)
        IF (I.LE.0 .OR. I.GT.IPTINV) GOTO 1001
        XLDREG(1)=IPVEC(I)
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    STRSTAT:
C----------
      IF (MYSTR.EQ.12500) THEN
        IF (.NOT.LCALC) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.2) GOTO 1002
C----------
C       TWO ARGS CAN BE GIVEN, FIRST IS MANDATORY
C----------
        J = 0
        L=IFIX(XLDREG(1)+.5)
        IF (JARGS.EQ.2) J=IFIX(XLDREG(2)+.5)
C----------
C       CHECK RANGE AND VALUES: ARG1 MUST BE 1-33, ARG2 MUST BE 0-1
C----------
        IF (L .LT. 1 .OR. L .GT. 33) GOTO 1002
        IF (J .LT. 0 .OR. J .GT. 1) GOTO 1002
        IF (J.EQ.1 .AND. IPHASE.LT.2) GO TO 1001

        X = 1.0
        SELECT CASE (L)
          CASE (1,11,21)
            X = INtoCM
          CASE (2:5,12:15,22:25)
            X = FTtoM
          CASE (10,20,30)
            X = HAtoACR
        END SELECT
        XLDREG(1)=OSTRST(L,J+1) * X
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    POTFTYPE:
C----------
      IF (MYSTR.EQ.12600) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-2
C----------
        IF (J. LT. 1 .OR. J .GT. 2) GOTO 1002
C
        CALL FMEVTYP(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
C
        GOTO 1000
      ENDIF

C----------
C     DECODE INSTRUCTION AND EXECUTE.    POTSRATE:
C----------
      IF (MYSTR.EQ.12700) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-4
C----------
        IF (J. LT. 1 .OR. J .GT. 4) GOTO 1002
C
        CALL FMEVSRT(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL * FTtoM ! ft/min -> m/min
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    POTREINT:
C----------
      IF (MYSTR.EQ.12800) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-2
C----------
        IF (J. LT. 1 .OR. J .GT. 2) GOTO 1002
C
        CALL FMEVRIN(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001
        ! btu/ft**2/min -> kJ/m**2/min
        XLDREG(1) = RVAL * BTUtoKJ * M2toFT2
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    TREEBIO:
C     NO ARGUMENTS ARE REQUIRED
C----------
      IF (MYSTR.EQ.12900) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.3) GOTO 1002
C----------
C  SET DEFAULTS
C----------
        I1=1         ! INCLUDE STANDING AND REMOVED
        I2=1         ! INCLUDE LIVE AND DEAD
        I3=1         ! INCLUDE STEM AND CROWN
        I4=0         ! ICLUDE ALL SPECIES
C----------
C  ARG1-FIND TREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)
C----------
        IF(XLDREG(1).GE. 0.)THEN
          I1=IFIX(XLDREG(1)+.5)
        ELSEIF(XLDREG(1).EQ. 0.)THEN
          I1=IFIX(XLDREG(1))
        ELSE
          I1=IFIX(XLDREG(1)-.5)
        ENDIF
C----------
C  ARG2-FIND TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)
C----------
        IF(XLDREG(2).GE. 0.)THEN
          I2=IFIX(XLDREG(2)+.5)
        ELSEIF(XLDREG(2).EQ. 0.)THEN
          I2=IFIX(XLDREG(2))
        ELSE
          I2=IFIX(XLDREG(2)-.5)
        ENDIF
C----------
C  ARG3-FIND TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)
C----------
        IF(XLDREG(3).GE. 0.)THEN
          I3=IFIX(XLDREG(3)+.5)
        ELSEIF(XLDREG(3).EQ. 0.)THEN
          I3=IFIX(XLDREG(3))
        ELSE
          I3=IFIX(XLDREG(3)-.5)
        ENDIF
C----------
C  ARG4-FIND SPECIES
C       (J=0:MAXSP) ALL,SPP
C----------
        IF(XLDREG(4).GE. 0.)THEN
          I4=IFIX(XLDREG(4)+.5)
        ELSE
          I4=IFIX(XLDREG(4)-.5)
        ENDIF
        IF (I4.GT.MAXSP) GOTO 1002
        IF (I4.LT.0 .AND. NSPGRP.LT.-I4) GOTO 1002
C----------
C       FIND THE DBH RANGE AND THE HT RANGE.
C----------
        XLDBH=0.0
        XHDBH=1E30
        XLHT=0.0
        XHHT=1E30
        M=0
        IF (JARGS.GE.5) XLDBH = XLDREG(5) * CMtoIN
        IF (JARGS.GE.6) XHDBH = XLDREG(6) * CMtoIN
        IF (JARGS.GE.7) XLHT  = XLDREG(7) * MtoFT
        IF (JARGS.GE.8) XHHT  = XLDREG(8) * MtoFT
C
        CALL FMEVTBM(RVAL,I1,I2,I3,I4,XLDBH,XHDBH,XLHT,XHHT,I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL * TItoTM / ACRtoHA
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    CARBSTAT:
C----------
      IF (MYSTR.EQ.13000) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-17
C----------
        IF (J. LT. 1 .OR. J .GT. 17) GOTO 1002
C
        CALL FMEVCARB(RVAL, J, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    HTDIST:
C----------
      IF (MYSTR.EQ.13100) THEN
        IF (JARGS.LT.1 .OR. JARGS.GT.1) GOTO 1002
C----------
C       ONE ARG CAN BE GIVEN
C----------
        J=IFIX(XLDREG(1)+.5)
C----------
C       CHECK RANGE AND VALUES: MUST BE 1-100
C----------
        IF (J. LT. 1 .OR. J .GT. 100) GOTO 1002
        HTPCT=1-(FLOAT(J)/100)

      DO 400 I=1,MAXTRE
      IF(I .LE. ITRN)THEN
        INDEX(I)=I
      ELSE
        INDEX(I)=0
      ENDIF
  400 CONTINUE
      CALL RDPSRT(ITRN,HT,INDEX,.FALSE.)
C
      SUMPIN = 0.
      XLDREG(1) = 0.
      DO 410 I=1,ITRN
      ISRTI = INDEX(I)
      P = PROB(ISRTI)
      IF(HT(ISRTI) .GE. XLDREG(1))THEN
        SUMPIN = SUMPIN + P
      ENDIF
      IF(SUMPIN.GT.TPROB*HTPCT .AND. XLDREG(1).EQ.0.)
     >  XLDREG(1) = HT(ISRTI)
  410 CONTINUE
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    HERBSHRB:
C----------
      IF (MYSTR.EQ.13200) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT
C----------
        CALL FMATV(LACTV)

        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.NE.1) GOTO 1002
C----------
C       CHECK RANGE OF VALUES: MUST BE 1-3
C----------
        ILO = IFIX(XLDREG(1)+.5)
        IF (ILO.LT.1 .OR. ILO.GT.3) GOTO 1002
C
        CALL FMEVLSF(RVAL, ILO, I) ! tons/acre
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL * TItoTM / ACRtoHA
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    DWDVAL:
C----------
      IF (MYSTR.EQ.13300) THEN
C----------
C       RETURN IF FIRE MODEL IS INACTIVE
C       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT
C----------
        CALL FMATV(LACTV)
        IF (.NOT.LACTV) GOTO 1002
        IF (JARGS.NE.4) GOTO 1002
C
        L=IFIX(XLDREG(1)+.5)
        J=IFIX(XLDREG(2)+.5)
        K=IFIX(XLDREG(3)+.5)
        M=IFIX(XLDREG(4)+.5)
C----------
C  CHECK BOUNDARIES FOR
C  (L=1:2)  VOLUME OR COVER
C  (J=0:2)  ALL,HARD,SOFT
C  (K=1:7)  LOWER SIZE CLASS: 1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
C                             6=35-50", 7=50"+
C  (M=1:7)  UPPER SIZE CLASS: 1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
C                             6=35-50", 7=50"+
C----------
        IF (L.LT.1 .OR. L.GT.2) GOTO 1002
        IF (J.LT.0 .OR. J.GT.2) GOTO 1002
        IF (K.LT.1 .OR. K.GT.7) GOTO 1002
        IF (M.LT.1 .OR. M.GT.7) GOTO 1002
        IF (K.GT.M) GOTO 1002

        CALL FMDWD(RVAL, L, J, K, M, I)
        IF (I.EQ.1) GOTO 1001
        XLDREG(1) = RVAL
        IF (L .EQ. 1) XLDREG(1) = RVAL * FT3pACRtoM3pHA
C
        GOTO 1000
      ENDIF
C----------
C     DECODE INSTRUCTION AND EXECUTE.    ACORNS: << DR: Requires metrification March 2012 >>
C----------
C      1ST ARUGMENT:
C         1 = NUMBER OF ACORNS PER ACRE
C         2 = LBS OF ACORNS PER ACRE
C      2ND ARGUMENT: SPECIES
C         0 = ALL SPECIES
C        -X = SPECIES GROUP X
C
C  VARIABLE LINCL IS USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
C  THE CALCULATION OR NOT.
      IF (MYSTR.EQ.13400) THEN
C----------
C       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT
C----------
        IF (JARGS.NE.2) GOTO 1002
        L=IFIX(XLDREG(1)+.5)
        IF(XLDREG(2).GE. 0.)THEN
          J=IFIX(XLDREG(2)+.5)
        ELSE
          J=IFIX(XLDREG(2)-.5)
        ENDIF
C----------
C       IF THE ATTRIBUTE (L) IS OUT OF RANGE OR UNDEFINED IN THE
C       PHASE, THEN: ISSUE ERROR CODE.
C----------
        IF (L.LE.0 .OR. L.GT.2) GOTO 1002
C----------
C       IF THE SPECIES (J) IS OUT OF RANGE, THEN: ISSUE ERROR CODE.
C       NOTE: A NEGATIVE SPECIES NUMBER INDICATES A SPECIES GROUP
C----------
        IF (J.GT.MAXSP) GOTO 1002
        IF (J.LT.0 .AND. NSPGRP.LT.-J) GOTO 1002
C----------
        ACRN=0.
        XLDREG(1)=0.
        ILIM=ITRN
        IF (ILIM.GT.0) THEN
          DO 450 I=1,ILIM
          LINCL = .FALSE.
          IF(FIAJSP(ISP(I)).EQ."802" .OR. FIAJSP(ISP(I)).EQ."806" .OR.
     >       FIAJSP(ISP(I)).EQ."832" .OR. FIAJSP(ISP(I)).EQ."833" .OR.
     >       FIAJSP(ISP(I)).EQ."837") THEN
            IF(J.EQ.0 .OR. J.EQ.ISP(I))THEN
              LINCL = .TRUE.
            ELSEIF(J.LT.0)THEN
              IGRP = -J
              IULIM = ISPGRP(IGRP,1)+1
              DO 451 IG=2,IULIM
              IF(ISP(I) .EQ. ISPGRP(IGRP,IG))THEN
                LINCL = .TRUE.
              GO TO 452
              ENDIF
  451       CONTINUE
            ENDIF
  452       CONTINUE
          ENDIF
C----------
C       CALCULATE PERCENTAGE IN HEIGHT DISTRIBUTION FOR USE IN
C       IDENTIFYING CO-DOMINANT AND DOMINANT TREES.
C----------
          CALL RDPSRT(ITRN,HT,IPNTR,.TRUE.)
          CALL PCTILE (ITRN,IPNTR,PROB,HPCT,JUNK)
          IF(LINCL .AND.
     >      (DBH(I).GE.5.0) .AND.
     >      (HPCT(I).GE.60.0)) THEN
            DCM = DBH(I)*2.54
            TPA3 = PROB(I)
            ACRN = 0.
C----------
C       SET RETURN VALUE TO # IF L=1, LBS IF L=2
C----------
            SELECT CASE (L)
            CASE(1)
              ADIV = 1.
            CASE(2)
              SELECT CASE (FIAJSP(ISP(I)))
              CASE("802")
                ADIV = 140.
              CASE("806")
                ADIV = 180.
              CASE("832")
                ADIV = 115.
              CASE("833")
                ADIV = 100.
              CASE("837")
                ADIV = 160.
              END SELECT
            END SELECT
            SELECT CASE (FIAJSP(ISP(I)))
              CASE("802")
                BADJ = 0.6**2*(1-0.6**2)/2
                ACRN = (0.71155+0.06346*DCM -0.00034290*DCM*DCM)
                ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              CASE("806")
                BADJ = 0.5**2*(1-0.5**2)/2
                ACRN = (1.16744+0.05158*DCM -0.00026797*DCM*DCM)
                ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              CASE("832")
                BADJ = 0.6**2*(1-0.6**2)/2
                ACRN = (0.20984+0.06029*DCM-0.00039431*DCM*DCM)
                ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              CASE("833")
                BADJ = 0.6**2*(1-0.6**2)/2
                ACRN = (-0.14836+0.07539*DCM-0.00039950*DCM*DCM)
                ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              CASE("837")
                BADJ = 0.4**2*(1-0.4**2)/2
                ACRN = (TPA3*(10**(1.06367+0.03123*DCM+BADJ)-1))/ADIV
            END SELECT
            XLDREG(1)=XLDREG(1)+(ACRN)
          ENDIF
  450     CONTINUE
        ENDIF
C
        GOTO 1000
      ENDIF
C----------
C  **INSERT ADDITIONAL FUNCTIONS HERE**
C----------
 1000 CONTINUE
      IRC=0
      GOTO 1010
 1001 CONTINUE
      IRC=1
      GOTO 1010
 1002 CONTINUE
      IRC=2
 1010 CONTINUE
      IF (LDEB) WRITE (JOSTND,1020) IRC,XLDREG(1)
 1020 FORMAT (' IN EVLDX: IRC= ',I2,' XLDREG=',E14.5)
      RETURN
      END
