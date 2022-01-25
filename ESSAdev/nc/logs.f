      SUBROUTINE LOGS
     &     (DBH,HT,ICCD,IBCD,CUMIN,BDMIN,ISP,CSTMP,BV,JOSTND)
      IMPLICIT NONE
C----------
C NC $Id$
C----------
C
C  TAPER EQUATIONS BY G.S.BIGING
C  BY K.STUMPF, ADAPTED BY B.KRUMLAND, P.J.DAUGHERTY
C----------
      REAL XPS(9),TPCF(2,9)
      INTEGER IPRGSP(12),IMAP(12)
      REAL SF(4,2),TBV,QTLN
      INTEGER JOSTND,ISP,ICCD,I,JSP,ILN,IMAX,NX,IH,J
      INTEGER IBCD,M,IC,NLOGS,ITEM,IDU,N,LNGTH
      REAL BV,CSTMP,BDMIN,CUMIN,HT,DBH,X,D,B1,B2,HI,DIBAHI,TRIM
      REAL SLN,CTP,BTP,DTOP,Z1,HTM,DL,TV,DU,FRAC,QLOGS,S1,AH
      DATA TPCF/
     & 1.01959  ,.33567 ,1.06932  ,.41563 ,1.07134  ,.47216
     &,1.02929  ,.33401 ,1.09262  ,.36530 ,1.07588  ,.35378
     &,1.01959  ,.33567 ,1.06932  ,.41563 ,1.01959  ,.33567/
      DATA IPRGSP/10,2,6,3,4,9,7,1,8,11,5,12/
      DATA IMAP  / 1,2,3,4,5,6,7,9,9,10,11,4/
C----------
C  SCRIBNER VOL. FACTORS FOR SMALL LOGS
C----------
      DATA SF/1.249,1.608,1.854,2.410,1.160,1.400,1.501,2.084/
C----------
C  STATEMENT FUNCTION TO CALC. DIB AT H(I)
C----------
      DIBAHI(HI,HT,B1,B2,D,X)=D*(B1+B2*ALOG(1.-X*(HI/HT)**(1./3.)))
C----------
C   FIND VALID SPECIES CODE AND BRANCH TO APPROPRIATE STATEMENT TO
C   COMPUTE BOARD FOOT VOLUME. SPECIES 1 AND 8 USE THE SAME VOLUME
C   COEFFICIENTS.
C----------
      DO 7 I=1,12
      IF (ISP .EQ. IPRGSP(I)) GO TO 9
    7 CONTINUE
      WRITE(JOSTND,907) ISP
  907 FORMAT(' IN SIERRA VOLS NO MATCH FOR SPECIES',I4,
     &'.  SPECIES 10 USED.')
      I=10
    9 CONTINUE
      JSP=IMAP(I)
C----------
C   SPECIES 5, 7, 8 AND 11 ARE CALCULATED DIFFERENTLY FROM THE
C   OTHER SPECIES.
C----------
      IF(ISP .EQ. 5) GO TO 15
      IF(ISP .EQ. 7) GO TO 16
      IF(ISP .EQ. 8 .OR. ISP .EQ. 11) GO TO 17
      GO TO 18
C----------
C   MADRONNE USE PNW 414. VOL IS REALLY SAW LOG CUBIC.
C----------
   15 CONTINUE
      BV = .0006182*DBH**1.726*HT**1.265
      BV=BV*5.0
      GO TO 100
C----------
C BLACK OAK EQ FROM PNW 414
C EQN IS IN CUBIC FEET, APPLY BF/CF RATIO   R.JOHNSON
C----------
   16 CONTINUE
      BV = .00124 * DBH**2.68 * HT**0.424
      BV = BV * 5.0
      GO TO 100
C----------
C  TANOAK AND OTHER HARDWOODS
C----------
   17 CONTINUE
        BV = .000252*DBH**2.309*HT**1.211
        BV=BV*5.0
        GO TO 100
C----------
C   CALCULATE VOLUME HERE FOR ALL OTHER SPECIES.
C   DEFINE NOMINAL SCALING STANDARDS
C----------
   18 CONTINUE
      ILN=16
      BV=0.0
      TRIM=.3
      SLN=ILN+TRIM
      IMAX=0
      DO 5 M=1,9
      XPS(M) = 1. - EXP(-TPCF(1,M)/TPCF(2,M))
   5  CONTINUE
      CTP=ICCD
      BTP=IBCD
C----------
C  CUBIC VOLUME
C----------
        NX=1
        IF(DBH.LE.(ICCD))GOTO 50
        IF(DBH.LT.CUMIN)GOTO 50
        DTOP=CTP
C----------
C  FIND MERCH TOP
C----------
20      Z1= EXP((DTOP/DBH - TPCF(1,JSP))/TPCF(2,JSP)) - 1.
        HTM = (Z1/(-XPS(JSP)))**3*HT
        IF(HTM.LE.(.5*SLN))GOTO 100
        IF(NX.EQ.2)GOTO 65
        IH=INT(HTM+.49999999)
        DL = DIBAHI(CSTMP,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
        TV=0.0
        J=0
C----------
C   SCALE THE TREE
C----------
        DO 30 I=2,IH
          HI = FLOAT(I)
          DU = DIBAHI(HI,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
          TV=TV+.005454*((DU+DL)/2.)**2
          DL=DU
          LNGTH=MOD(I,ILN)
          IF(I.EQ.IH)GOTO 24
          IF(LNGTH.NE.0)GOTO 30
24        CONTINUE
          IF(LNGTH.EQ.0)LNGTH=ILN
          FRAC=FLOAT(LNGTH)/FLOAT(ILN)
          IC=INT((DU-3.)/2.)
          IC=IC+1
          IF(IC.LT.1)IC=1
          IF(IC.GT.25)IC=25
          IF(IC.GT.IMAX)IMAX=IC
          TV=0.0
30      CONTINUE
C----------
C   BOARD VOLUMES
C----------
50      IF(DBH.LE.(IBCD))GOTO 100
        IF(DBH.LT.BDMIN)GOTO 100
        DTOP=BTP
        NX=2
        GOTO 20
65      QLOGS=(HTM-1.)/SLN
        NLOGS=INT(QLOGS)
        NLOGS=NLOGS-1
        IF(NLOGS.LE.0)GOTO 80
        S1=SLN
        AH=0.0
      BV=0.0
        ITEM=1
68      DO 70 J=1,NLOGS
          HI = FLOAT(J)*S1+AH
          DU = DIBAHI(HI,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
          FRAC=((S1-TRIM)/FLOAT(ILN))
          IF(DU.LE.9.0) THEN
            IDU = INT(DU + .5)
            IDU = IDU - 5
            N = 1
            IF(S1.LT.SLN)N=2
            TBV = SF(IDU,N) * S1
          ELSE
            TBV=((.79*DU -2)*DU -4.)*FRAC
          ENDIF
          TBV = INT(TBV/10. + .5) * 10.
          IC=INT((DU-3.)/2.)
          IC=IC+1
          IF(IC.LT.1)IC=1
          IF(IC.GT.25)IC=25
          IF(IC.GT.IMAX)IMAX=IC
          BV=BV+TBV
70      CONTINUE
        IF(ITEM.EQ.2)GOTO 100
C----------
C  TOP LOGS FOR BOARD FOOT
C----------
80      ITEM=2
        AH=NLOGS*SLN
        IF(AH.LE.0)AH=0.0
        QTLN= HTM-AH-1.
        NLOGS=2
        IF(QTLN.LT.(1.5*SLN))NLOGS=1
        S1=QTLN/FLOAT(NLOGS)
        GOTO 68
100   CONTINUE
      RETURN
      END
