      SUBROUTINE ESUCKR
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C  CREATE STUMP & ROOT SPROUTS FROM TREES CUT AT BEGINNING OF CYCLE.
C  ASSUMPTION: THE TREE LIST HAS BEEN COMPRESSED TO ABOUT 1/2 THE
C  VALUE OF MAXTRE. COMPRS IS CALLED IN ESNUTR.
C----------
COMMONS
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ESPARM.F77'
      INCLUDE 'ESHOOT.F77'
      INCLUDE 'ESCOMN.F77'
      INCLUDE 'ESHAP.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'STDSTK.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'METRIC.F77'
COMMONS

      EXTERNAL ESRANN
      LOGICAL DEBUG
      INTEGER NUMSPR,I,ICL,ISSP,IPLOT,ISPSTO,II,MXRR,J,MXTODO
      INTEGER ITRGT,IGRP,IULIM,IG,IGSP,INDXAS
      REAL BACHLO,CRDUM,CW,TPATOT,PREM,DSTMP,XR
      CHARACTER CLABEL*2
      INTEGER*4 MDBH,INUMB,MSP
      INTEGER ISHAG,NSPRT
      REAL RANDEV,ASPRTR,HTI,NEWDBH
      REAL COUNTR(NSPSPE),TPASUM(NSPSPE),HTAVE(NSPSPE)
      INTEGER MYACTS(1),JJ,IDATE,IT
      INTEGER NTODO,KDT,NP,IACTK,IDT
      REAL PRMS(6),SPRMLT(NSPSPE,100),HTMSPR(NSPSPE,100),SMULT,HMULT
      REAL DMIN(NSPSPE,100),DMAX(NSPSPE,100)
      DATA MDBH/10000000/,MSP/10000/,NUMSPR/2/
      DATA MYACTS/450/
      DATA MDBH/10000000/,MSP/10000/,NUMSPR/2/
      CALL DBCHK (DEBUG,'ESUCKR',6,ICYC)
      IF(ITRNRM.LT.1) GO TO 900

      DO 10 I=1,NSPSPE
      COUNTR(I)=0.0
      TPASUM(I)=0.0
      HTAVE(I)=0.0
   10 CONTINUE
      TPATOT=0.0
C 
      DO I=1,NSPSPE
      DO J=1,100
      SPRMLT(I,J)=1.
      HTMSPR(I,J)=1.
      DMIN(I,J)=0.
      DMAX(I,J)=0.
      ENDDO
      ENDDO
      HMULT=1.
      SMULT=1.

C  PROCESS SPROUT KEYWORD OPTIONS.

      CALL OPFIND (1,MYACTS(1),NTODO)
      IF (DEBUG) WRITE (JOSTND,5) NTODO,ITRNRM
    5 FORMAT (' IN ESUCKR: OPTS NTODO,ITRNRM=',I2,2X,I2)
      IF(NTODO.LE.0)GO TO 60
C
      DO 50 IT=1,NTODO
      CALL OPGET (IT,5,IDATE,IACTK,NP,PRMS)
      IDT=IDATE
      KDT=IY(ICYC)
      J=IFIX(PRMS(1))
C
C  SPECIES GROUP
C
      IF(J .LT. 0)THEN
        IGRP = -J
        IULIM = ISPGRP(IGRP,1)+1
        DO IG=2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        DO JJ=1,NSPSPE
        IF(IGSP.EQ.ISPSPE(JJ))THEN
          SPRMLT(JJ,IT)=PRMS(2)
          HTMSPR(JJ,IT)=PRMS(3)
          DMIN(JJ,IT)  =PRMS(4)
          DMAX(JJ,IT)  =PRMS(5)
        ENDIF
        ENDDO
        ENDDO
C
C  ALL SPROUTABLE SPECIES
C
      ELSEIF (J .EQ. 0) THEN
        DO JJ=1,NSPSPE
        SPRMLT(JJ,IT)=PRMS(2)
        HTMSPR(JJ,IT)=PRMS(3)
        DMIN(JJ,IT)  =PRMS(4)
        DMAX(JJ,IT)  =PRMS(5)
        ENDDO
C
C  SINGLE SPECIES
C
      ELSE
        DO JJ=1,NSPSPE 	
        IF(J.EQ.ISPSPE(JJ))THEN
          SPRMLT(JJ,IT)=PRMS(2)
          HTMSPR(JJ,IT)=PRMS(3)
          DMIN(JJ,IT)  =PRMS(4)
          DMAX(JJ,IT)  =PRMS(5)
        ENDIF
        ENDDO
      ENDIF
      CALL OPDONE(IT,IDT)
   50 CONTINUE
C
   60 CONTINUE      
C----------
C  DO LOOP FOR EACH TREE RECORD WITH REMOVALS
C----------
      DO 500 I=1,ITRNRM
      INUMB=ISHOOT(I)
      ICL = INUMB/MDBH
      ISSP = INUMB/MSP - ICL*1000
      IPLOT = MOD(INUMB,MSP)
      PREM = PRBREM(I)
      ISHAG = JSHAGE(I)
      DSTMP = DSTUMP(I)

C     IF MID-DBH OF STUMP CLASS < 15CM THEN NO SPROUTING
      
      IF ((DBHMID(ICL)*INtoCM) .LT. 15.0) GO TO 500

C  IF AN INSIGNIFICANT AMOUNT OF A SPROUTING SPECIES WAS CUT,
C  DO NOT GENERATE SPROUTS.  GED 4-8-97.

      IF(PREM.LT. 0.001) GO TO 500
C
      ISPSTO=ISSP
      DO 200 II=1,NSPSPE
      IF(ISSP.NE.II) GO TO 200
      ISSP=ISPSPE(II)
      GO TO 201
  200 CONTINUE
      WRITE (JOREGT,1000) ISSP,ITRNRM
 1000 FORMAT(' IN ESPROT: NO SPECIES MATCH WHEN DECODING ISHOOT(',
     &  'ITRNRM). ISSP=',I5,'; ITRNRM=',I5)
      GO TO 900
  201 CONTINUE
      IF(DEBUG) WRITE(JOSTND,300) I,IPLOT,ISSP,ICL,PREM
  300 FORMAT(' IN ESUCKR: I=',I5,'; IPLOT=',I5,'; ISSP=',I5,
     &  '; ICL=',I5,'; PREM=',F6.2)
C
C     MAKE SURE THAT THERE IS ROOM IN THE TREE LIST FOR SPROUTS.
C     THE CALL TO RDESCP INSURES THAT THE SPACE REQUIRED IS BELOW THAT
C     NEEDED BY THE ROOT DISEASE MODEL.  RDESCP RETURNS THE MAXIMUM
C     NUMBER OF TREES THAT ROOT DISEASE CAN HANDLE.  IF RROT IS NOT
C     BEING RUN, MXRR IS RETURNED AS MAXTRE.
C
      CALL RDESCP (MAXTRE, MXRR)
C
C     SET SPROUT KEYWORD MULTIPLIERS BASED ON DBH RANGE
C
      SMULT=1.
      HMULT=1.
      DO 450 IT=1,NTODO
      IF((DSTMP.GE.DMIN(ISPSTO,IT)).AND.
     &   (DSTMP.LT.DMAX(ISPSTO,IT)))THEN
        SMULT=SPRMLT(ISPSTO,IT)
        HMULT=HTMSPR(ISPSTO,IT)
      ENDIF
  450 CONTINUE
      IF(DEBUG)WRITE(JOSTND,*)'IT,ISPSTO,DSTMP,ISSP,SMULT= ',
     &IT,ISPSTO,DSTMP,ISSP,SMULT
C----------
C  IF SPROUT MULTIPLIER (SMULT) IS ZERO DO NOT ADD RECORDS TO TREE LIST
C----------
      IF(SMULT.LE.0.)GOTO 500
C----------
C  DETERMINE THE NUMBER OF SPROUT RECORDS TO CREATE
C----------
      NSPRT = 0
      CALL NSPREC(VARACD,ISSP,NSPRT)
      NUMSPR = NSPRT
      IF(DEBUG)WRITE(JOSTND,*)' AFTER NSPREC NUMSPR = ',NUMSPR
C----------
C  IF QUAKING ASPEN IS PART OF THIS VARIANT, DETERMINE THE TREES-PER-ACRE
C  REPRESENTED BY EACH ASPEN CUT.
C----------
      INDXAS = 0
      ASPRTR = 0.
      CALL ESASID(VARACD,INDXAS)
      IF(ISSP .EQ. INDXAS)THEN
        CALL ASSPTN (ISHAG,ASBAR,ASTPAR,PREM,ASPRTR)
        PREM = ASPRTR
      ENDIF

      IF(DEBUG)WRITE(JOSTND,*)' AFTER ASSPTN INDXAS,ASPRTR= ',
     &INDXAS,ASPRTR
C----------
C  CHECK TO SEE IF THERE ARE OTHER VARIANT AND SPECIES SPECIFIC RULES
C  FOR THE TREES-PER-ACRE A SPROUT RECORD WILL REPRESENT
C----------
      IF(DEBUG)WRITE(JOSTND,*)' CALLING ESSPRT VARACD,ISSP,NUMSPR,PREM= 
     &',VARACD,ISSP,NUMSPR,PREM 
      CALL ESSPRT(VARACD,ISSP,NUMSPR,PREM)
      IF(DEBUG)WRITE(JOSTND,*)' AFTER ESSPRT PREM= ',PREM
C----------
C  CREATE SPROUT RECORDS
C----------
      DO 499 J=1,NUMSPR
      IF(ITRN.LT.MXRR) GO TO 100
      ITRGT=ITRNRM-I
      MXTODO=FLOAT(MXRR)* 0.70
      IF(MXTODO.GT.ITRGT) ITRGT=MXTODO
      CALL ESCPRS (ITRGT,DEBUG)
  100 CONTINUE
      ITRN=ITRN+1
      CALL RANN(XR)
      IF (XR .LE. 0.5) THEN
        IMC(ITRN)=2   ! these are 'sucker class'
	ELSE
	  IMC(ITRN)=1   ! these are 'AGS class'
	ENDIF
      ISP(ITRN)=ISSP
      ITRE(ITRN)=IPLOT
      CFV(ITRN)=0.0
      ITRUNC(ITRN)=0
      NORMHT(ITRN)=0
C
C     INSERT CODE TO CALCULATE DBH AND HEIGHT OF SPROUTS
C
      PROB(ITRN)=PREM*SMULT
      HTI = 0.
      IF(DEBUG)WRITE(JOSTND,*)' BEFORE SPRTHT VARACD,ISSP,SI,ISHAG= ',
     &VARACD,ISSP,SITEAR(ISSP),ISHAG 
      CALL SPRTHT (VARACD,ISSP,SITEAR(ISSP),ISHAG,HTI)
      IF(DEBUG)WRITE(JOSTND,*)' AFTER SPRTHT HTI= ',HTI
      HT(ITRN)=HTI*HMULT
  162 RANDEV=BACHLO(0.,0.5,ESRANN)
      IF(RANDEV.LT.-1.0 .OR. RANDEV.GT.1.0) GO TO 162
      RANDEV = RANDEV*HT(ITRN)/5.5
      HT(ITRN)=HT(ITRN)+RANDEV
C
      IF(HT(ITRN) .GT.4.5)THEN
        CALL ONHTDBH(ISSP,0.1,BA,HT(ITRN),RMSQD,NEWDBH)
c        BX=HT2(ISSP)
c        IF(IABFLG(ISSP).EQ.1)THEN     ! new implementation does not account for self calibration
c          AX=HT1(ISSP)
c        ELSE
c          AX=AA(ISSP)
c        ENDIF
        DBH(ITRN)= NEWDBH
        IF (DBH(ITRN).LT.0.1) DBH(ITRN)=0.1
      ELSE
        DBH(ITRN)=0.1
      ENDIF
C
      ICR(ITRN)=70
C----------
C  CALCULATE A CROWN WIDTH FOR SPROUTS
C----------
      CRDUM=1.
      CALL CWCALC(ISSP,PROB(ITRN),DBH(ITRN),HT(ITRN),CRDUM,
     &            ICR(ITRN),CW,0,JOSTND)
      CRWDTH(ITRN)=CW
C
      DG(ITRN)=0.0
      HTG(ITRN)=0.0
      PCT(ITRN)=0.0
      OLDPCT(ITRN)=0.0
      WK1(ITRN)=0.0
      WK2(ITRN)=0.
      WK4(ITRN)=0.
      BFV(ITRN)=0.0
      IESTAT(ITRN)=0
      PTBALT(ITRN)=0.
      IDTREE(ITRN)=10000000+ICYC*10000+ITRN
      CALL MISPUTZ(ITRN,0)
C
      ABIRTH(ITRN)=ISHAG
      DEFECT(ITRN)=0.
      ISPECL(ITRN)=0 
      OLDRN(ITRN)=0.
      PTOCFV(ITRN)=0.
      PMRCFV(ITRN)=0.
      PMRBFV(ITRN)=0.
      NCFDEF(ITRN)=0
      NBFDEF(ITRN)=0
      PDBH(ITRN)=0.
      PHT(ITRN)=0.
      COUNTR(ISPSTO)=COUNTR(ISPSTO) +1.0
      TPASUM(ISPSTO)=TPASUM(ISPSTO) +PREM*SMULT
      TPATOT=TPATOT+PREM*SMULT
      HTAVE(ISPSTO)=HTAVE(ISPSTO) +HT(ITRN)
  499 CONTINUE
  500 CONTINUE
      IF(IPRINT.NE.0) THEN
        WRITE(JOREGT,1100) NPLT,MGMID,IY(ICYC+1)-1
 1100   FORMAT(' ',54('-'),
     &         /, ' REGENERATION FROM STUMP & ROOT SPROUTS',
     &         //,' STAND ID: ',A26,'  MANAGEMENT CODE: ',A4,
     &                                              '  YEAR: ',I5,
     &         //,T15,        'TREES  AVERAGE',
     &          /,T6,'SPECIES  /HA    HEIGHT',
     &          /,T6,'-------  -----  -------')
         DO 700 I=1,NSPSPE
        CLABEL=NSP(ISPSPE(I),1)
        HTAVE(I)=HTAVE(I)/(COUNTR(I)+.00001)
        IF (TPASUM(I) .GT. 0.0) 
     &  WRITE(JOREGT,1200) CLABEL,TPASUM(I)/ACRtoHA,HTAVE(I)*FTtoM
 1200   FORMAT(T9,A2,T14,F6.0,T22,F6.1)
  700   CONTINUE
        WRITE(JOREGT,1300) TPATOT / ACRtoHA
 1300   FORMAT(T15,'-----',/,T14,F6.0,/,54('-') )
      ENDIF  
  900 CONTINUE
      ITRNRM=0
      RETURN
      END