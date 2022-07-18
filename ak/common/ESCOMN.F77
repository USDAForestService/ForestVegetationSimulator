CODE SEGMENT ESCOMN
C----------
C COMMON $Id$
C----------
C
C *********************************************************************
C *** This version is specific to AK variant for development period,
C *** but may be placed back into base level common folder in future
C *** with appropriate modifications made to other variant subroutines.
C *********************************************************************
C
C     COMMON AREA TO USE WITH ALL REGENERATION MODEL VARIANTS.
C     BE SURE TO INCLUDE ESPARM BEFORE ESCOMN.
C     USED IN ESADVH, ESBLKD, ESDLAY, ESGENT, ESNSPE, ESPADV, ESPREP, ESPSUB,
C     ESPXCS, ESSUBH, ESXCSH, ESTAB, ESTIME, ESTOCK, ESTPP, ESTUMP, & ESUCKR.
C----------
C
      INTEGER IHAB,IPHY,IPREP,NNID,IFT0
C
      INTEGER IFORCD(MXFRCDS),IFORST(MXFRCDS),ISPSPE(NSPSPE)
C
      REAL    BAA,BAALN,BAASQ,BWAF,BWB4,ELEVSQ,REGT,SLO,SQBWAF,SQREGT
      REAL    TIME,XCOS,XCOSAS,XSIN,XSINAS
C
      REAL    BNORML(20),DBHMID(NDBHCL),HHTMAX(MAXSP),OCURHT(16,MAXSP)
      REAL    OCURNF(MXFRCDS,MAXSP),SUMPI(MAXSP),SUMPX(MAXSP)
      REAL    XMIN(MAXSP),OCURFT(MAXSP,14)
C
      COMMON /ESCOMN/ BAA,BAALN,BAASQ,BNORML,BWAF,BWB4,DBHMID,ELEVSQ,
     &                HHTMAX,IFORCD,IFORST,IHAB,IPHY,IPREP,ISPSPE,NNID,
     &                OCURHT,OCURNF,REGT,SLO,SQBWAF,SQREGT,SUMPI,SUMPX,
     &                TIME,XCOS,XCOSAS,XMIN,XSIN,XSINAS,OCURFT,IFT0
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C     BAA --
C   BAALN --
C   BAASQ --
C  BNORML --
C    BWAF --
C    BWB4 --
C  DBHMID --
C  ELEVSQ --
C  HHTMAX --
C  IFORCD -- 
C  IFORST --
C    IHAB --
C    IPHY --
C   IPREP --
C  ISPSPE --
C    NNID --
C  OCURHT --
C  OCURNF --
C  OCURFT --
C    REGT --
C     SLO --
C  SQBWAF --
C  SQREGT --
C   SUMPI --
C   SUMPX --
C    TIME --
C    XCOS --
C  XCOSAS --
C    XMIN --
C    XSIN --
C  XSINAS --
C
C-----END SEGMENT
