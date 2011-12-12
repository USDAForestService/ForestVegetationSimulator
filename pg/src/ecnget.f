      SUBROUTINE ECNGET (WK3,IPNT,ILIMIT)
      IMPLICIT NONE
C----------
C  **ECNPUT--PPBASE   DATE OF LAST REVISION:   09/02/10
C----------
C
C     STORE THE ECONOMIC DATA FOR A GIVEN STAND.
C
C     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'CONTRL.F77'
C
      INCLUDE 'ECNCOM.F77'
C
C
COMMONS
C
C
      INTEGER MXL,MXI,MXR
C      
      PARAMETER (MXL=5,MXI=9,MXR=7)
      INTEGER ILIMIT,IPNT,I
      LOGICAL LOGICS(MXL)
      REAL WK3 (MAXTRE)
      INTEGER INTS (MXI)
      REAL REALS (MXR)
C
C  GET THE INTEGER SCALARS.
C
      CALL IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
      annCostCnt    = INTS(1)  
      annRevCnt     = INTS(2)  
      econStartYear = INTS(3)  
      fixHrvCnt     = INTS(4)  
      fixPctCnt     = INTS(5)  
      pctMinUnits   = INTS(6)  
      plntCostCnt   = INTS(7)  
      varHrvCnt     = INTS(8)  
      varPctCnt     = INTS(9)  
C
C  GET THE INTEGER ARRAYS
C  ONE DIMENSIONAL
C
      CALL IFREAD (WK3,IPNT,ILIMIT, hrvCostBf, MAX_KEYWORDS,          2)
      CALL IFREAD (WK3,IPNT,ILIMIT, hrvCostFt3, MAX_KEYWORDS,         2)
      CALL IFREAD (WK3,IPNT,ILIMIT, hrvCostTpa, MAX_KEYWORDS,         2)
      CALL IFREAD (WK3,IPNT,ILIMIT, varHrvUnits, MAX_KEYWORDS,        2)
      CALL IFREAD (WK3,IPNT,ILIMIT, varPctUnits, MAX_KEYWORDS,        2)
      CALL IFREAD (WK3,IPNT,ILIMIT, plntCostUnits, MAX_PLANT_COSTS,   2)
      CALL IFREAD (WK3,IPNT,ILIMIT, hrvRevCnt,MAXSP*MAX_REV_UNITS,    2)
      CALL IFREAD (WK3,IPNT,ILIMIT, hrvRevDiaIndx,MAXSP*
     &             MAX_REV_UNITS*MAX_KEYWORDS,                        2)
      CALL IFREAD (WK3,IPNT,ILIMIT, burnCostDur, MAX_RATES,           2)
      CALL IFREAD (WK3,IPNT,ILIMIT, mechCostDur, MAX_RATES,           2)
      CALL IFREAD (WK3,IPNT,ILIMIT, annCostDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, annRevDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, fixHrvDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, fixPctDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, varHrvDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, varPctDur,
     &             MAX_KEYWORDS*MAX_RATES,                            2)
      CALL IFREAD (WK3,IPNT,ILIMIT, plntCostDur,
     &             MAX_PLANT_COSTS*MAX_RATES,                         2)
      CALL IFREAD (WK3,IPNT,ILIMIT,hrvRevDur,MAXSP*
     &             MAX_REV_UNITS*MAX_KEYWORDS*MAX_RATES,              2)
C
C  GET THE LOGICAL SCALARS.
C
      CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
      noLogStockTable = LOGICS ( 1)
      noOutputTables  = LOGICS ( 2)
      isFirstEcon     = LOGICS ( 3)
      doSev           = LOGICS ( 4)
      isEconToBe      = LOGICS ( 5)
C
C  LOGICAL ARRAYS
C
      CALL LFREAD (WK3,IPNT,ILIMIT,hasRevAmt,MAXSP*MAX_REV_UNITS,     2)
C
C  GET THE REAL SCALARS.
C
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR,                     2)
      burnCostAmt  = REALS (1)
      dbhSq        = REALS (2)
      discountRate = REALS (3)
      mechCostAmt  = REALS (4)
      pctMinDbh    = REALS (5)
      pctMinVolume = REALS (6)
      sevInput     = REALS (7)
C
C  REAL ARRYS
C
      CALL BFREAD (WK3,IPNT,ILIMIT,lbsFt3Amt,MAXSP,                   2)
      DO I=TPA,FT3_100
      CALL BFREAD (WK3,IPNT,ILIMIT,harvest(I),1,                      2)
      ENDDO
      CALL BFREAD (WK3,IPNT,ILIMIT,annCostAmt,MAX_KEYWORDS,           2)
      CALL BFREAD (WK3,IPNT,ILIMIT,annRevAmt,MAX_KEYWORDS,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,fixHrvAmt,MAX_KEYWORDS,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,fixPctAmt,MAX_KEYWORDS,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,pctBf,MAX_KEYWORDS,                2)
      CALL BFREAD (WK3,IPNT,ILIMIT,pctFt3,MAX_KEYWORDS,               2)
      CALL BFREAD (WK3,IPNT,ILIMIT,pctTpa,MAX_KEYWORDS,               2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varHrvAmt,MAX_KEYWORDS,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varHrvDbhLo,MAX_KEYWORDS,          2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varHrvDbhHi,MAX_KEYWORDS,          2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varPctAmt,MAX_KEYWORDS,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varPctDbhLo,MAX_KEYWORDS,          2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varPctDbhHi,MAX_KEYWORDS,          2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,plntCostAmt,MAX_PLANT_COSTS,       2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,burnCostRate,MAX_RATES,            2)
      CALL BFREAD (WK3,IPNT,ILIMIT,mechCostRate,MAX_RATES,            2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,annCostRate,MAX_KEYWORDS
     &             *MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,annRevRate,MAX_KEYWORDS*MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,fixHrvRate,MAX_KEYWORDS*MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,fixPctRate,MAX_KEYWORDS*MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varHrvRate,MAX_KEYWORDS*MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,varPctRate,MAX_KEYWORDS*MAX_RATES, 2)
      CALL BFREAD (WK3,IPNT,ILIMIT,plntCostRate,MAX_PLANT_COSTS*
     &             MAX_RATES, 2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,hrvRevPrice,MAXSP*MAX_REV_UNITS*
     &             MAX_KEYWORDS,                                      2)
      CALL BFREAD (WK3,IPNT,ILIMIT,hrvRevDia,MAXSP*MAX_REV_UNITS*
     &             MAX_KEYWORDS,                                      2)
      CALL BFREAD (WK3,IPNT,ILIMIT,revVolume,MAXSP*MAX_REV_UNITS*
     &             MAX_KEYWORDS,                                      2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,hrvRevRate,MAXSP*MAX_REV_UNITS*
     &             MAX_KEYWORDS*MAX_RATES,                            2)
C
      CALL BFREAD (WK3,IPNT,ILIMIT,logBfVol,ITRN*MAX_LOGS,          2)
      CALL BFREAD (WK3,IPNT,ILIMIT,logDibBf, ITRN*MAX_LOGS,           2)
      CALL BFREAD (WK3,IPNT,ILIMIT,logFt3Vol,ITRN*MAX_LOGS,         2)
      CALL BFREAD (WK3,IPNT,ILIMIT,logDibFt3,ITRN*MAX_LOGS,           2)
      RETURN
      END
