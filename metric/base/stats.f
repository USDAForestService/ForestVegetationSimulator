      SUBROUTINE STATS
      IMPLICIT NONE
C----------
C METRIC-BASE $Id$
C----------
C  THIS ROUTINE COMPUTES STATISTICS THAT DESCRIBE THE INPUT
C  DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE PLOTS.  CALLED FROM
C  **MAIN**.  **TVALUE** IS CALLED TO CALCULATE STUDENT'S T FOR
C  CONSTRUCTION OF CONFIDENCE INTERVALS.
C---------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'VOLSTD.F77'
      INCLUDE 'PLOT.F77' 
      INCLUDE 'METRIC.F77'
C
COMMONS
      CHARACTER*16 LABELS(4)
      REAL TOTCF(MAXSP),
     &   TOTTR(MAXSP),TOTBA(MAXSP),TOTBF(MAXSP),
     &   SUMT(MAXPLT),SUMBA(MAXPLT),SUMCF(MAXPLT),SUMBF(MAXPLT)
      INTEGER IFLG(MAXSP),I,ISPC,J,IALP,IERR,NDF
      REAL P,TBA,TBF,TCF,T,SUM,SUMSQ,ST,XBAR,S,SS,SE,UL,UU,CV
      REAL SEU,SEP,SCF,SBF,SBA
      EQUIVALENCE (WK3,SUMT),(WK3(MAXPLT+1),SUMBA),
     &            (WK4,SUMCF),(WK4(MAXPLT+1),SUMBF),
     &            (WK5,TOTTR),(WK5(MAXSP+1),TOTBA),
     &            (WK6,TOTCF),(WK6(MAXSP+1),TOTBF)
      DATA LABELS/
     &  'BOARD FEET/HA  ',
     &  'CUBIC METRES/HA ',
     &  'TREES/HA    ',
     &  'BASAL AREA/HA   '/

C----------
C  IF THERE ARE NO TREE RECORDS, OR STATS OPTION WAS NOT REQUESTED,
C  RETURN.
C----------
      IF(.NOT.LSTATS.OR.ITRN.EQ.0) RETURN
C----------
C  INITIALIZE.
C----------
      DO 1 I=1,MAXSP
      TOTTR(I)=0.0
      TOTBA(I)=0.0
      TOTBF(I)=0.0
      TOTCF(I)=0.0
      IFLG(I)=0
    1 CONTINUE
      DO 2 I=1,MAXPLT
      SUMT(I)=0.0
      SUMBA(I)=0.0
      SUMBF(I)=0.0
      SUMCF(I)=0.0
    2 CONTINUE
C----------
C  ACCUMULATE SUMS FOR OVERALL STATISTICS.
C----------
      DO 30 I=1,ITRN
      ISPC=ISP(I)
      J=ITRE(I)
      P=PROB(I)
      TBA=DBH(I)*DBH(I)*P*0.005454154
      TBF=BFV(I)*P
      TCF=CFV(I)*P
      TOTTR(ISPC)=TOTTR(ISPC)+P
      TOTBA(ISPC)=TOTBA(ISPC)+TBA
      TOTCF(ISPC)=TOTCF(ISPC)+TCF
      TOTBF(ISPC)=TOTBF(ISPC)+TBF
      IFLG(ISPC)=1
      IF(IPTINV.LE.1) GO TO 30
      SUMT(J)=SUMT(J)+IPTINV*P
      SUMBA(J)=SUMBA(J)+IPTINV*TBA
      SUMBF(J)=SUMBF(J)+IPTINV*TBF
      SUMCF(J)=SUMCF(J)+IPTINV*TCF
   30 CONTINUE
      DO 35 ISPC=1,MAXSP
      TOTTR(ISPC)=TOTTR(ISPC)/GROSPC
      TOTBA(ISPC)=TOTBA(ISPC)/GROSPC
      TOTCF(ISPC)=TOTCF(ISPC)/GROSPC
      TOTBF(ISPC)=TOTBF(ISPC)/GROSPC
   35 CONTINUE
C----------
C  WRITE TABLE HEADING FOR GENERAL SPECIES SUMMARY.
C----------
      WRITE(JOSTND,9000)
 9000 FORMAT(/)
      WRITE(JOSTND,9001)
 9001 FORMAT(T5,'GENERAL SPECIES SUMMARY FOR THE CRUISE',
     &' (PER HECTARE)')
      WRITE(JOSTND,9002)
 9002 FORMAT(/3X, 'SPECIES',
     &        T16,'CUBIC METRES',
     &        T37,'TREES',
     &        T47,'BASAL AREA',
     &       /1X,55('-'))
      DO 41 I=1,MAXSP
        IF(IFLG(I).EQ.0) GO TO 41
        WRITE(JOSTND,9003) JSP(I),
     &                NSP(I,1)(1:2), 
     &                TOTCF(I)*FT3pACRtoM3pHA,
     &                TOTTR(I)/ACRtoHA,
     &                TOTBA(I)*FT2pACRtoM2pHA
 9003   FORMAT(2X,A4,'=',A4,T18,F10.1,T32,F10.1,T47,F10.1)
   41 CONTINUE
      IF(IPTINV.GT.1) GO TO 50
C----------
C  1 POINT.  PRINT ERROR MESSAGE AND RETURN.
C----------
      WRITE(JOSTND,9004)
 9004 FORMAT(' DISTRIBUTION OF ATTRIBUTES AMONG SAMPLE POINTS',
     &   ' CANNOT BE COMPUTED WITH ONE SAMPLE POINT.'/)
      RETURN
   50 CONTINUE
      WRITE(JOSTND,9005)
 9005 FORMAT(//T20,'DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE'
     &              ,' POINTS'/)
      IALP=100.0-100.0*ALPHA+0.5
      WRITE(JOSTND,9008)IALP
 9008 FORMAT(' ',T32,'STANDARD  COEFF OF SAMPLE',T68,I4,'%',
     &T84,'SAMPLING ERROR IN')
      WRITE(JOSTND,9006)
 9006 FORMAT(' CHARACTERISTIC',T26,'MEAN DEVIATION VARIATION   SIZE',
     &'     CONFIDENCE  LIMITS    PERCENT     UNITS',/,1X,16('-'),
     &3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))
C----------
C  CALL **TVALUE TO APPROXIMATE 'T' FOR (IPTINV-1) DF AT THE 'ALPHA'
C  PROBABILITY LEVEL.
C----------
      NDF=IPTINV-1
      CALL TVALUE(NDF,ALPHA,T,IERR)
C----------
C  COMPUTE AND PRINT STATISTICS FOR TREES PER HECTARE.
C----------
      SUM=0.0
      SUMSQ=0.0
      DO 60 I=1,IPTINV
      ST=SUMT(I)/GROSPC
      SUM=SUM+ST
      SUMSQ=SUMSQ+ST*ST
   60 CONTINUE
      IF(SUM.GT.0.0) GO TO 65
         WRITE(JOSTND,9007)  LABELS(3),SUM,SUM
 9007 FORMAT(1X,A16,T20,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2,
     &4X,F6.1,1X,F10.1)
      GO TO 70
   65 CONTINUE
      XBAR=SUM/PI
      S=0.0
      SS=SUMSQ-SUM*SUM/PI
      IF(SS.LE.0.0) GO TO 67
      S=SQRT(SS/(PI-1.0))
   67 CONTINUE
      SE=S/SQRT(PI)
      UL=XBAR-T*SE
      IF(UL.LT.0.0) UL=0.0
      UU=XBAR+T*SE
      CV=S/XBAR  
      SEU=T*SE
      SEP=SEU*100./XBAR
      WRITE(JOSTND,9007) LABELS(3),
     &                   XBAR/ACRtoHA,
     &                   S/ACRtoHA,
     &                   CV,IPTINV,
     &                   UL/ACRtoHA,
     &                   UU/ACRtoHA,
     &                   SEP/ACRtoHA,
     &                   SEU
   70 CONTINUE
C----------
C  COMPUTE AND PRINT STATISTICS FOR CUBIC METRE VOL.
C----------
      SUM=0.0
      SUMSQ=0.0
      DO 80 I=1,IPTINV
      SCF=SUMCF(I)/GROSPC
      SUM=SUM+SCF
      SUMSQ=SUMSQ+SCF*SCF
   80 CONTINUE
      IF(SUM.GT.0.0) GO TO 85 
      WRITE(JOSTND,9007)  LABELS(2),SUM,SUM
      GO TO 90
   85 CONTINUE
      XBAR=SUM/PI
      S=0.0
      SS=SUMSQ-SUM*SUM/PI
      IF(SS.LE.0.0) GO TO 87
      S=SQRT(SS/(PI-1.0))
   87 CONTINUE
      SE=S/SQRT(PI)
      UL=XBAR-T*SE
      IF(UL.LT.0.0) UL=0.0
      UU=XBAR+T*SE
      CV=S/XBAR
      SEU=T*SE
      SEP=SEU*100./XBAR
      WRITE(JOSTND,9007) LABELS(2),
     &                   XBAR*FT3pACRtoM3pHA,
     &                   S*FT3pACRtoM3pHA,
     &                   CV,IPTINV,
     &                   UL*FT3pACRtoM3pHA,
     &                   UU*FT3pACRtoM3pHA,
     &                   SEP*FT3pACRtoM3pHA,
     &                   SEU
   90 CONTINUE
C----------
C  COMPUTE AND PRINT STATISTICS FOR BOARD FOOT VOLUME (IF NOT METRIC).
C----------
      SUM=0.0
      SUMSQ=0.0
      DO 100 I=1,IPTINV
      SBF=SUMBF(I)/GROSPC
      SUM=SUM+SBF
      SUMSQ=SUMSQ+SBF*SBF
  100 CONTINUE
      IF(SUM.GT.0.0) GO TO 105
C
C     WRITE(JOSTND,9007)  LABELS(1),SUM,SUM
C
      GO TO 110
  105 CONTINUE
      XBAR=SUM/PI
      S=0.0
      SS=SUMSQ-SUM*SUM/PI
      IF(SS.LE.0.0) GO TO 107
      S=SQRT(SS/(PI-1.0))
  107 CONTINUE
      SE=S/SQRT(PI)
      UL=XBAR-T*SE
      IF(UL.LT.0.0) UL=0.0
      UU=XBAR+T*SE
      CV=S/XBAR
      SEU=T*SE
      SEP=SEU*100./XBAR
C
C     WRITE(JOSTND,9007) LABELS(1),XBAR,S,CV,IPTINV, UL,UU
C
  110 CONTINUE
C----------
C  COMPUTE AND PRINT STATISTICS FOR BASAL AREA PER HECTARE.
C----------
      SUM=0.0
      SUMSQ=0.0
      DO 120 I=1,IPTINV
      SBA=SUMBA(I)/GROSPC
      SUM=SUM+SBA
      SUMSQ=SUMSQ+SBA*SBA
  120 CONTINUE
      IF(SUM.GT.0.0) GO TO 130
      WRITE(JOSTND,9007) LABELS(4),SUM,SUM
      RETURN
  130 CONTINUE
      XBAR=SUM/PI
      S=0.0
      SS=SUMSQ-SUM*SUM/PI
      IF(SS.LE.0.0) GO TO 137
      S=SQRT(SS/(PI-1.0))
  137 CONTINUE
      SE=S/SQRT(PI)
      UL=XBAR-T*SE
      IF(UL.LT.0.0) UL=0.0
      UU=XBAR+T*SE
      CV=S/XBAR
      SEU=T*SE
      SEP=SEU*100./XBAR
      WRITE(JOSTND,9007) LABELS(4),
     &                   XBAR*FT2pACRtoM2pHA,
     &                   S*FT2pACRtoM2pHA,
     &                   CV,IPTINV,
     &                   UL*FT2pACRtoM2pHA,
     &                   UU*FT2pACRtoM2pHA,
     &                   SEU*FT2pACRtoM2pHA,
     &                   SEP
      RETURN
      END
