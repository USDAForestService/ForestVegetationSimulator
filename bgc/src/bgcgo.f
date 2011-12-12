       SUBROUTINE BGCGO(LBGCGO)
C----------
C  **BGCGO  BGC--DATE OF LAST REVISION: 05/29/01
C                Revised 11/12/02.  Removing index ISTND, and removing PPE
C                                   common "includes" (PPEPRM, PPCNTL, &
C                                   PRGPRM).  AJM
C                These changes--also made in BGCFVS, BGCGROW, BGCIN, BGCINT,
C                BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
C                The FVS-BGC code is now, once again, a single stand model.
C----------
C
C     TELLS FVS WHETHER OR NOT BGC IS ACTIVE (REGARDLESS OF WHETHER OR NOT
C     INCREMENTS WILL BE PASSED FROM BGC TO FVS).  PASSES LBGCON (AS LBGCGO) TO FVS.
C     WHETHER OR NOT BGC's INCREMENTS WILL BE PASSED TO FVS WILL BE DEALT WITH
C     INTERNALLY WITHIN BGC IN SUBROUTINE FVSBGC (VIA FLAG IBGC).
C
C     CALLED FROM: GRINCR
C
COMMONS
C
C
      INCLUDE 'BGCCOM.F77'
C      INCLUDE 'PPCNTL.F77'                       ! removed ajm 11/02
C      INCLUDE 'PRGPRM.F77'                       ! removed ajm 11/02
C
C----------------------
C
      LOGICAL LBGCGO
C      IF(LBGCON(ISTND)) LBGCGO = .TRUE.          !removed 11/02 ajm
      IF(LBGCON) LBGCGO = .TRUE.

      RETURN
      END