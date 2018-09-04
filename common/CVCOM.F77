CODE SEGMENT CVCOM
C----------
C COMMON $Id$
C----------
      INTEGER COVOPT,ICEHAB,ICVBGN,IDIST,IGFHAB,IHTYPE,INF,IOV,IPHYS,
     &        ITUN,IUN,JOSHRB,NKLASS,NSHOW,ISHOW(6),ILAYR(31),
     &        ICVAGE(MAXCY1),JCVNOH,ISHAPE(MAXTRE),ISTAGE(MAXCY1,2),
     &        INDSP(MAXCY1,2,6),ISSP(MAXCY1,2,12)
      REAL    CRAREA,HTMAX,HTMIN,SAGE,SUMCVR,TCOV,TALLSH,TCON(2),
     &        AVGBHT(3),AVGBPC(3),CVAVG(3),CVFRAC(3),HTAVG(3),
     &        HTFRAC(3),CRXHT(16),SD2XHT(16),BHTCF(31),BPCCF(31),
     &        CCON(31),HCON(31),PB(31),PBCV(31),PCON(31),SH(31),
     &        SHRBHT(31),SHRBPC(31),XCV(31),XPB(31),XSH(31),
     &        TRECW(MAXTRE),TRFBMS(MAXTRE),ASHT(MAXCY1,2),
     &        CLOW(MAXCY1,2),CMED(MAXCY1,2),CTALL(MAXCY1,2),
     &        PGT0(MAXCY1,2),SBMASS(MAXCY1,2),SDIAM(MAXCY1,2),
     &        STDHT(MAXCY1,2),TCVOL(MAXCY1,2),TIMESD(MAXCY1,2),
     &        TOTBMS(MAXCY1,2),TOTLCV(MAXCY1,2),
     &        TPCTCV(MAXCY1,2),TPROAR(MAXCY1,2),
     &        TRETOT(MAXCY1,2),TWIGS(MAXCY1,2),CIND(MAXCY1,2,6),
     &        PIND(MAXCY1,2,6),HIND(MAXCY1,2,6),SCOV(MAXCY1,2,11),
     &        TRSH(MAXCY1,2,11),SCV(MAXCY1,2,12),SHT(MAXCY1,2,12),
     &        SPB(MAXCY1,2,12),CFBXHT(MAXCY1,2,16),
     &        PCXHT(MAXCY1,2,16),PROXHT(MAXCY1,2,16),TXHT(MAXCY1,2,16),
     &        VOLXHT(MAXCY1,2,16)
      Logical LBROW,LCALIB,LCAL1,LCAL2,LCNOP,LCOV,LCOVER,LCVSUM,
     &        LSHOW,LSHRUB,LTHIND(MAXCY1),LCVNOH,LSAGE
      COMMON /CVCOM/ COVOPT,CRAREA,HTMAX,HTMIN,ICEHAB,ICVBGN,IDIST,
     &               IGFHAB,IHTYPE,INF,IOV,IPHYS,ITUN,IUN,JOSHRB,LBROW,
     &               LCALIB,LCAL1,LCAL2,LCNOP,LCOV,LCOVER,LCVSUM,LSAGE,
     &               LSHOW,LSHRUB,NKLASS,NSHOW,SAGE,SUMCVR,TCOV,TALLSH,
     &               TCON,AVGBHT,AVGBPC,CVAVG,CVFRAC,HTAVG,HTFRAC,ISHOW,
     &               CRXHT,SD2XHT,BHTCF,BPCCF,CCON,HCON,ILAYR,PB,PBCV,
     &               PCON,SH,SHRBHT,SHRBPC,XCV,XPB,XSH,ICVAGE,LTHIND,
     &               LCVNOH,JCVNOH,ISHAPE,TRECW,TRFBMS,ASHT,CLOW,CMED,
     &               CTALL,ISTAGE,PGT0,SBMASS,SDIAM,STDHT,TCVOL,TIMESD,
     &               TOTBMS,TOTLCV,TPCTCV,TPROAR,TRETOT,TWIGS,CIND,PIND,
     &               HIND,INDSP,SCOV,TRSH,SCV,SHT,SPB,ISSP,CFBXHT,PCXHT,
     &               PROXHT,TXHT,VOLXHT
C----------
C  DEFINITIONS OF VARIABLES IN 'CVCOM' COMMON BLOCK:
C----------
C  ASHT     --
C  AVGBHT   --
C  AVGBPC   --
C  BHTCF    --
C  BPCCF    --
C  CCON     --
C  CFBXHT   --
C  CIND     --
C  CLOW     --
C  CMED     --
C  COVOPT   --
C  CRAREA   --
C  CRXHT    --
C  CTALL    --
C  CVAVG    --
C  CVFRAC   --
C  HCON     --
C  HIND     --
C  HTAVG    --
C  HTFRAC   --
C  HTMAX    --
C  HTMIN    --
C  ICEHAB   --
C  ICVAGE   --
C  ICVBGN   --
C  IDIST    --
C  IGFHAB   --
C  IHTYPE   --
C  ILAYR    --
C  INDSP    --
C  INF      --
C  IOV      --
C  IPHYS    --
C  ISHAPE   --
C  ISHOW    --
C  ISSP     --
C  ISTAGE   --
C  ITUN     --
C  IUN      --
C  JCVNOH   --
C  JOSHRB   --
C  LBROW    --
C  LCAL1    --
C  LCAL2    --
C  LCALIB   --
C  LCNOP    --
C  LCOV     --
C  LCOVER   --
C  LCVNOH   --
C  LCVSUM   --
C  LSAGE    --
C  LSHOW    --
C  LSHRUB   --
C  LTHIND   --
C  NKLASS   --
C  NSHOW    --
C  PB       --
C  PBCV     --
C  PCON     --
C  PCXHT    --
C  PGT0     --
C  PIND     --
C  PROXHT   --
C  SAGE     --
C  SBMASS   --
C  SCOV     -- SCOV(I,J,K), I-CYCLE No., J=1 NO THIN =2 THIN,
C              K= TOTAL COVER OF SHRUBS GREATER THAN HEIGHT(in SHTRHT)
C  SCV      --
C  SD2XHT   --
C  SDIAM    --
C  SH       --
C  SHRBHT   --
C  SHRBPC   --
C  SHT      --
C  SPB      --
C  STDHT    --
C  SUMCVR   --
C  TALLSH   --
C  TCON     --
C  TCOV     --
C  TCVOL    --
C  TIMESD   --
C  TOTBMS   --
C  TOTLCV   --
C  TPCTCV   --
C  TPROAR   --
C  TRECW    --
C  TRETOT   --
C  TRFBMS   --
C  TRSH     --TRSH(I,J,K), I-CYCLE No., J=1 NO THIN =2 THIN,       
C             K=TOTAL NUMBER OF TREES/ACRE GREATER THAN HEIGHT (SHTRHT)
C  TWIGS    --
C  TXHT     --
C  VOLXHT   --
C  XCV      --
C  XPB      --
C  XSH      --
C-----END SEGMENT











