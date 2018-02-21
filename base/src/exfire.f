      SUBROUTINE EXFIRE
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C  EXTRA EXTERNAL REFERENCES FOR THE FIRE MODEL.
C----------
C
      LOGICAL     LACTV,LDEB,LFM,LKECHO,L1,L2
C
      CHARACTER*4 NSP(1,*)
      CHARACTER*7 CH7
      CHARACTER*8 KEYWRD,NOFM
      CHARACTER*13 CFOTO
C
      INTEGER     I,I1,I2,I3,I4,J,J1,J2,K,IYR,I5
      INTEGER     IVAC,IREC,ISPC,IC,ITFN,NCLAS,NR,NC,KEY
C
      REAL        XMXVOL(*),CTCRWN(*),SSNG(*),DSNG(*),TKCRWN(*),FLNY(*)
      REAL        D,H,P,X,WEIGHT,XXX(*),X1,X2,X3,X4,X5,X6
      REAL        CRBSHT,CRBLKD,CANCOV,POKL,POVK
      REAL        KFMS(*),FMWTS(*),FMLOADS(*),AX(*)
C----------
C  DATA STATEMENTS:
C----------
      DATA NOFM/'*NO FIRE'/
C----------
C  ENTRY POINTS:
C----------
      ENTRY FMIN (I1,NSP,LKECHO)
        CALL ERRGRO (.TRUE.,11)
      RETURN
C
      ENTRY FMINIT
      RETURN
C
      ENTRY FMCMPR (NCLAS)
      RETURN
C
      ENTRY FMTDEL (IVAC,IREC)
      RETURN
C
      ENTRY FMSDIT
      RETURN
C
      ENTRY FMMAIN
      RETURN
C
      ENTRY FMKILL(I)
      RETURN
C
      ENTRY FMSSEE (I,ISPC,D,H,P,IC,LDEB,J)
      RETURN
C
      ENTRY FMSCUT(XMXVOL,NR,NC,SSNG,DSNG,CTCRWN,TKCRWN)
      RETURN
C
      ENTRY FMSALV(IYR,X1)
      RETURN
C
      ENTRY FMSVTREE(I1,I2)
      RETURN
C
      ENTRY FMSVFL(I)
      RETURN
C
      ENTRY FMGETFL(I,FLNY)
      RETURN
C
      ENTRY FMKEY(KEY,KEYWRD)
        KEYWRD=NOFM
      RETURN
C
      ENTRY FMOUT
      RETURN
C
      ENTRY FMTRIP(ITFN,I,WEIGHT)
      RETURN
C
      ENTRY FMPRUN(CTCRWN)
      RETURN
C
      ENTRY FMATV(LACTV)
        LACTV = .FALSE.
      RETURN
C
      ENTRY FMLNKD(LACTV)
        LACTV = .FALSE.
      RETURN
C
      ENTRY FMEVSNG(X, I, J, K, X1, X2, X3, X4, I4)
        I4=1
      RETURN
C
      ENTRY FMEVCWD(X, I, J, I4)
        I4=1
      RETURN
C
      ENTRY FMEVFLM(X, I, I4)
        I4=1
      RETURN
C
      ENTRY FMEVSAL(X,I,I1,I2,I4)
        I4=1
      RETURN
C
      ENTRY FMEVMRT(X,I,I4)
        I4=1
      RETURN
C
      ENTRY FMEVTYP(X,I,I4)
        I4=1
      RETURN
C
      ENTRY FMEVCARB(X,I,I4)
        I4=1
      RETURN      
C
      ENTRY FMDWD(X,I,J,K,I2,I4)
        I4=1
      RETURN
C
      ENTRY FMEVSRT(X,I,I4)
        I4=1
      RETURN 
C
      ENTRY FMEVRIN(X,I,I4)
        I4=1
      RETURN                   
C
      ENTRY FMEVFMD(X,I,J,I4)
        I4=1
      RETURN
C
      ENTRY FMSADD (I,J)
      RETURN
C
      ENTRY FMSATV(LFM)
      RETURN
C
      ENTRY FMPHOTOCODE(I,CFOTO,J,K)
      RETURN
C
      ENTRY FMTREM(DSNG,SSNG,AX)
      RETURN
C
      ENTRY FMEVTBM(X1,I,I1,I2,I3,X2,X3,X4,X5,I4)
      RETURN
C
      ENTRY FMEVMSN(X)
        X=0.
      RETURN
C
      ENTRY FMEVLSF(X, I, I4)
        X=0.
        I4=1
      RETURN
C----------
C     ENTRIES FOR THE PPE
C----------
      ENTRY FMPPHV(IYR,CRBSHT,CRBLKD,CANCOV,KFMS,FMWTS,
     &             POKL,POVK,FMLOADS)
      RETURN
C
      ENTRY FMPPGET (XXX,I1,I2)
      RETURN
C
      ENTRY FMPPPUT (XXX,I1,I2)
      RETURN
C
      ENTRY FMSVL2(I1,X1,X2,X3,X4,L1,L2,I2)
      RETURN
C
      ENTRY FMSNGHT(CH7,I1,X1,X2,I2,X3)
      RETURN
C
      ENTRY FMSNGDK(CH7,I1,X1,X2)
      RETURN
C
      ENTRY FMSFALL(I1,I2,X1,X2,X3,I3,X4,X5,X6)
        X6 = 0.
      RETURN
C
      ENTRY SNGCOE
      RETURN
C
      ENTRY FMCBA (I1,I2)
      RETURN
C
      END
