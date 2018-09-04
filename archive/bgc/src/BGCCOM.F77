CODE SEGMENT BGCCOM
C----------
C  **BGCCOM -- BGC  DATE OF LAST REVISION: 10/15/99
C              Revised 11/12/02.  removing PPE includes. This change is in
C              conjunction with changes made to BGCGO, BGCGROW, BGCIN,
C              BGCINT, BGCFVS & BINITIAL. Variables IBGC, LBGCON and IBCYC
C              are no longer dimensioned by MXSTND.  The FVS-BGC extension
C              is now, once again, a single stand model.
C----------
C
C     LBGCON -- TRUE IF THE BGC EXTENSION IS ACTIVE, FALSE OTHERWISE
C
C      INCLUDE 'PPEPRM.F77' ! removed 11/02 ajm
      REAL BGC_DG(1400), BGC_HG(1400), BGC_CR(1400), BGC_EXP(1400)
C      INTEGER IBGC(MXSTND), IBCYC(MXSTND)              ! removed 11/02 ajm
C      LOGICAL LBGCON(MXSTND)                           ! ditto
      INTEGER IBGC, IBCYC
      LOGICAL LBGCON
      COMMON /BGCCOM/ LBGCON, IBGC, BGC_DG, BGC_HG, BGC_CR, BGC_EXP,
     +                IBCYC
C    
C     END SEGMENT
