      SUBROUTINE BGCGRO(IY1,IY2)
C----------
C  **BGCGROW  BGC--DATE OF LAST REVISION: 3/13/00
C    Revised 11/12/02.  Removing index ISTND, and removing PPE common 
C                       "includes" (PPEPRM, PPCNTL, & PRGPRM).  AJM
C           These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
C           BGCIN, and BGCCOM.f77--remove all PPE funtionality.
C           The FVS-BGC code is now, once again, a single stand model.
C----------
C
C     RUNS THE BGC EXTENSION
C
C     CALLED FROM: GRINCR 
C
COMMONS
C
      INCLUDE 'BGCCOM.F77'
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PDEN.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CVCOM.F77'
C      INCLUDE 'PPCNTL.F77'                         ! removed 11/02 ajm
C
COMMONS
C
C      print *, 'in BGCGROW, LBGCON=',LBGCON(ISTND)
C      IF(.NOT.LBGCON(ISTND)) RETURN                ! removed 11/02 ajm
      IF(.NOT.LBGCON) RETURN
C----------------------------------------------------------------------
C  LOAD BGC ENTITY ARRAYS WITH FVS OUTPUT
C
C  SKIP CALLS TO BENTYLOAD AND BSITELOAD IF BGC INCREMENTS ARE TO
C  BE USED AND FVS CYCLE IS GREATER THAN ONE. INSTEAD CALL BENTYUPDT
C  WHICH WILL ADD NEW TREES FROM REGEN ESTAB MODEL AND UPDATE PROB
C  IN CASE THINNING HAS OCCURRED.
C----------------------------------------------------------------------
C      IF(IBGC(ISTND).EQ.1 .AND. ICYC.GT.1) THEN   ! removed 11/02 ajm
C        IBCYC(ISTND)=1                            ! ditto
      IF(IBGC.EQ.1 .AND. ICYC.GT.1) THEN
        IBCYC=1
        CALL BENTYUPDT(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE,
     $                 CLOW,CMED,CTALL,MAXCY1)
      ELSE
C        IBCYC(ISTND)=0 !removed 11/02 ajm
        IBCYC=0
      CALL BENTYLOAD(ICYC,LTHIND,ITRN,DBH,HT,ICR,PROB,ISP,JSP,IDTREE,
     $               CLOW,CMED,CTALL,MAXCY1)
      CALL BSITELOAD(SLOPE,ASPECT,ELEV,TLAT,KODTYP,KODFOR)
      END IF
C---------------------------------
C  RUN STAND-BGC
C---------------------------------
      NUMYRS=IY2-IY1
      CALL BSTNDBGCN(NUMYRS,ICYC)
      RETURN
      END
