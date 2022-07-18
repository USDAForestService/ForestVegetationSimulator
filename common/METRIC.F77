CODE SEGMENT METRIC
C----------
C COMMON $Id$
C----------
C     PARAMETERS FOR THE METRIC CONVERSION ARE:
C----------
C
      LOGICAL LMTRIC
C
      INTEGER N_METRIC_PRMS
C
      REAL  CMtoIN, CMtoFT, MtoIN, MtoFT
      REAL  INtoCM, FTtoCM, INtoM, FTtoM
      REAL  M2toFT2, HAtoACR
      REAL  FT2toM2, ACRtoHA
      REAL  M3toFT3
      REAL  FT3toM3
      REAL  KGtoLB, TMtoTI
      REAL  LBtoKG, TItoTM
      REAL  M2pHAtoFT2pACR, M3pHAtoFT3pACR
      REAL  FT2pACRtoM2pHA, FT3pACRtoM3pHA
      REAL  TpHAtoTpACR, TpACRtoTpHA
      REAL  MItoKM, KMtoMI
      REAL  FtoC1, FtoC2, CtoF1, CtoF2
      REAL  KJtoBTU, BTUtoKJ
C----------
C     Simple Metric to US Conversions
C----------
      PARAMETER (N_METRIC_PRMS=13)
      PARAMETER (CMtoIN = 0.3937)
      PARAMETER (CMtoFT = 0.0328084)
      PARAMETER (MtoIN  = 39.37)
      PARAMETER (MtoFT  = 3.28084)
      PARAMETER (KMtoMI = 0.6214)
C
      PARAMETER (M2toFT2 = 10.763867)
      PARAMETER (HAtoACR = 2.471)
C
      PARAMETER (M3toFT3 = 35.314455)
C
      PARAMETER (KGtoLB  = 2.2046226)
      PARAMETER (TMtoTI  = 1.102311)
C
      PARAMETER (CtoF1   = 1.8)
      PARAMETER (CtoF2   = 32)
C
      PARAMETER (KJtoBTU = 0.9478171)
C----------
C     Simple US to Metric Conversions
C----------
      PARAMETER (INtoCM = 2.54)
      PARAMETER (FTtoCM = 30.48)
      PARAMETER (INtoM  = 0.0254001)
      PARAMETER (FTtoM  = 0.3048)
      PARAMETER (MItoKM = 1.609)
C
      PARAMETER (FT2toM2  = 0.0929034)
      PARAMETER (ACRtoHA  = 0.4046945)
C
      PARAMETER (FT3toM3  = 0.028317)
C
      PARAMETER (LBtoKG = 0.4535924)
      PARAMETER (TItoTM = 0.90718)
C
      PARAMETER (FtoC1 = 0.554)
      PARAMETER (FtoC2 = -17.7)
C      
      PARAMETER (BTUtoKJ = 1.0550559)
C----------
C     Complex Metric to US Conversions
C----------
      PARAMETER (M2pHAtoFT2pACR = 4.3560773)
      PARAMETER (M3pHAtoFT3pACR = 14.291564)
      PARAMETER (TpHAtoTpACR    = 0.4460896)
C----------
C     Complex US to Metric Conversions
C----------
      PARAMETER (FT2pACRtoM2pHA = 0.2295643)
      PARAMETER (FT3pACRtoM3pHA = 0.0699713)
      PARAMETER (TpACRtoTpHA    = 2.2417023)
C
C----------
C     LEGACY DECLARATION FOR WRD3
C----------
C
      COMMON /METRIC/ LMTRIC
C
C-----END SEGMENT