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
      INTEGER     I,I1,I2,I3,I4,J,K,IYR
      INTEGER     IVAC,IREC,ISPC,IC,ITFN,NCLAS,NR,NC,KEY
C
      REAL        XMXVOL(*),CTCRWN(*),SSNG(*),DSNG(*),TKCRWN(*),FLNY(*)
      REAL        D,H,P,X,WEIGHT,XXX(*),X1,X2,X3,X4,X5,X6
      REAL        CRBSHT,CRBLKD,CANCOV,POKL,POVK
      REAL        KFMS(*),FMWTS(*),FMLOADS(*),AX(*)
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*1 CDANUW
      LOGICAL LDANUW
C----------
C  DATA STATEMENTS:
C----------
      DATA NOFM/'*NO FIRE'/
C----------
C  ENTRY POINTS:
C
C  ENTRY FMIN
C----------
      ENTRY FMIN (I1,NSP,LKECHO)
        CALL ERRGRO (.TRUE.,11)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        CDANUW = NSP(1,1)(1:1)
        LDANUW = LKECHO
      RETURN
C----------
C  ENTRY FMINIT
C----------
      ENTRY FMINIT
      RETURN
C----------
C  ENTRY FMCMPR
C----------
      ENTRY FMCMPR (NCLAS)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = NCLAS
      RETURN
C----------
C  ENTRY FMTDEL
C----------
      ENTRY FMTDEL (IVAC,IREC)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = IVAC
        IDANUW = IREC
      RETURN
C----------
C  ENTRY FMSDIT
C----------
      ENTRY FMSDIT
      RETURN
C----------
C  ENTRY FMMAIN
C----------
      ENTRY FMMAIN
      RETURN
C----------
C  ENTRY FMKILL
C----------
      ENTRY FMKILL(I)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
      RETURN
C----------
C  ENTRY FMSSEE
C----------
      ENTRY FMSSEE (I,ISPC,D,H,P,IC,LDEB,J)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
        IDANUW = ISPC
        RDANUW = D
        RDANUW = H
        RDANUW = P
        IDANUW = IC
        LDANUW = LDEB
        IDANUW = J
      RETURN
C----------
C  ENTRY FMSCUT
C----------
      ENTRY FMSCUT(XMXVOL,NR,NC,SSNG,DSNG,CTCRWN,TKCRWN)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = XMXVOL(1)
        IDANUW = NR
        IDANUW = NC
        RDANUW = SSNG(1)
        RDANUW = DSNG(1)
        RDANUW = CTCRWN(1)
        RDANUW = TKCRWN(1)
      RETURN
C----------
C  ENTRY FMSALV
C----------
      ENTRY FMSALV(IYR,X1)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = IYR
        RDANUW = X1
      RETURN
C----------
C  ENTRY FMSVTREE
C----------
      ENTRY FMSVTREE(I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMSVFL
C----------
      ENTRY FMSVFL(I)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
      RETURN
C----------
C  ENTRY FMGETFL
C----------
      ENTRY FMGETFL(I,FLNY)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
        RDANUW = FLNY(1)
      RETURN
C----------
C  ENTRY FMKEY
C----------
      ENTRY FMKEY(KEY,KEYWRD)
        KEYWRD=NOFM
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C----------
C  ENTRY FMOUT
C----------
      ENTRY FMOUT
      RETURN
C----------
C  ENTRY FMTRIP
C----------
      ENTRY FMTRIP(ITFN,I,WEIGHT)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = ITFN
        IDANUW = I
        RDANUW = WEIGHT
      RETURN
C----------
C  ENTRY FMPRUN
C----------
      ENTRY FMPRUN(CTCRWN)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = CTCRWN(1)
      RETURN
C----------
C  ENTRY FMATV
C----------
      ENTRY FMATV(LACTV)
        LACTV = .FALSE.
      RETURN
C----------
C  ENTRY FMLNKD
C----------
      ENTRY FMLNKD(LACTV)
        LACTV = .FALSE.
      RETURN
C----------
C  ENTRY FMEVSNG
C----------
      ENTRY FMEVSNG(X, I, J, K, X1, X2, X3, X4, I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
        IDANUW = J
        IDANUW = K
        RDANUW = X1
        RDANUW = X2
        RDANUW = X3
        RDANUW = X4
      RETURN
C----------
C  ENTRY FMEVCWD
C----------
      ENTRY FMEVCWD(X, I, J, I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
        IDANUW = J
      RETURN
C----------
C  ENTRY FMEVFLM
C----------
      ENTRY FMEVFLM(X, I, I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN
C----------
C  ENTRY FMEVSAL
C----------
      ENTRY FMEVSAL(X,I,I1,I2,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMEVMRT
C----------
      ENTRY FMEVMRT(X,I,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN
C----------
C  ENTRY FMEVTYP
C----------
      ENTRY FMEVTYP(X,I,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN
C----------
C  ENTRY FMEVCARB
C----------
      ENTRY FMEVCARB(X,I,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN      
C----------
C  ENTRY FMDWD
C----------
      ENTRY FMDWD(X,I,J,K,I2,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
        IDANUW = J
        IDANUW = K
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMEVSRT
C----------
      ENTRY FMEVSRT(X,I,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN 
C----------
C  ENTRY FMEVRIN
C----------
      ENTRY FMEVRIN(X,I,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
      RETURN                   
C----------
C  ENTRY FMEVFMD
C----------
      ENTRY FMEVFMD(X,I,J,I4)
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X
        IDANUW = I
        IDANUW = J
      RETURN
C----------
C  ENTRY FMSADD
C----------
      ENTRY FMSADD (I,J)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
        IDANUW = J
      RETURN
C----------
C  ENTRY FMSATV
C----------
      ENTRY FMSATV(LFM)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LFM
      RETURN
C----------
C  ENTRY FMPHOTOCODE
C----------
      ENTRY FMPHOTOCODE(I,CFOTO,J,K)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
        CDANUW = CFOTO(1:1)
        IDANUW = J
        IDANUW = K
      RETURN
C----------
C  ENTRY FMTREM
C----------
      ENTRY FMTREM(DSNG,SSNG,AX)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = DSNG(1)
        RDANUW = SSNG(1)
        RDANUW = AX(1)
      RETURN
C----------
C  ENTRY FMEVTBM
C----------
      ENTRY FMEVTBM(X1,I,I1,I2,I3,X2,X3,X4,X5,I4)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = X1
        IDANUW = I
        IDANUW = I1
        IDANUW = I2
        IDANUW = I3
        RDANUW = X2
        RDANUW = X3
        RDANUW = X4
        RDANUW = X5
        IDANUW = I4
      RETURN
C----------
C  ENTRY FMEVMSN
C----------
      ENTRY FMEVMSN(X)
        X=0.
      RETURN
C----------
C  ENTRY FMEVLSF
C----------
      ENTRY FMEVLSF(X, I, I4)
        X=0.
        I4=1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I
      RETURN
C----------
C     ENTRIES FOR THE PPE:
C
C  ENTRY FMPPHV
C----------
      ENTRY FMPPHV(IYR,CRBSHT,CRBLKD,CANCOV,KFMS,FMWTS,
     &             POKL,POVK,FMLOADS)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = IYR
        RDANUW = CRBSHT
        RDANUW = CRBLKD
        RDANUW = CANCOV
        RDANUW = KFMS(1)
        RDANUW = FMWTS(1)
        RDANUW = POKL
        RDANUW = POVK
        RDANUW = FMLOADS(1)
      RETURN
C----------
C  ENTRY FMPPGET
C----------
      ENTRY FMPPGET (XXX,I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = XXX(1)
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMPPPUT
C----------
      ENTRY FMPPPUT (XXX,I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        RDANUW = XXX(1)
        IDANUW = I1
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMSVL2
C----------
      ENTRY FMSVL2(I1,X1,X2,X3,X4,L1,L2,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        RDANUW = X1
        RDANUW = X2
        RDANUW = X3
        RDANUW = X4
        LDANUW = L1
        LDANUW = L2
        IDANUW = I2
      RETURN
C----------
C  ENTRY FMSNGHT
C----------
      ENTRY FMSNGHT(CH7,I1,X1,X2,I2,X3)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW = CH7(1:1)
        IDANUW = I1
        RDANUW = X1
        RDANUW = X2
        IDANUW = I2
        RDANUW = X3
      RETURN
C----------
C  ENTRY FMSNGDK
C----------
      ENTRY FMSNGDK(CH7,I1,X1,X2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW = CH7(1:1)
        IDANUW = I1
        RDANUW = X1
        RDANUW = X2
      RETURN
C----------
C  ENTRY FMSFALL
C----------
      ENTRY FMSFALL(I1,I2,X1,X2,X3,I3,X4,X5,X6)
        X6 = 0.
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
        RDANUW = X1
        RDANUW = X2
        RDANUW = X3
        IDANUW = I3
        RDANUW = X4
        RDANUW = X5
        RDANUW = X6
      RETURN
C----------
C  ENTRY SNGCOE
C----------
      ENTRY SNGCOE
      RETURN
C----------
C  ENTRY FMCBA
C----------
      ENTRY FMCBA (I1,I2)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = I1
        IDANUW = I2
      RETURN
C
      END
