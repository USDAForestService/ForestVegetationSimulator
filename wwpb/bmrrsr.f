      SUBROUTINE BMRRSS
C----------
C WWPB $Id$
C----------
C  **BMRRSS--WWPB   DATE OF LAST REVISION:  07/10/94
C     A quick model that will provide some sensitivity to root
C     disease and stem rust in the inventory. These should be
C     replaced as soon as possible by a proper landscape level
C     model for these pests.
C----------

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BMRRCM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'

      LOGICAL     L,LNOTBK,LTR,LTEE
      CHARACTER*8 KEYWRD,NORR
      INTEGER*4   IDD
      INTEGER     IND, IND1, IRECN, IX1, IX2
      REAL        XP, TXP, TXP1
      REAL        X1
      DIMENSION   ARRAY(7),LNOTBK(7),ICODES(6)
      DIMENSION   RX1(MAXTRE)
      DIMENSION   PROB(MAXTRE), IND(MAXTRE), IND1(MAXTRE)
      
      DATA NORR/'*NO RROT'/

C     Keyword control is not available with this model.
      ENTRY RRIN (KEYWRD,ARRAY,LNOTBK)
      CALL ERRGRO (.TRUE.,11)
      RETURN

C     The model *is* running and is initialized from *inventory*.
      ENTRY RRATV (L,LTEE)
      L=.TRUE.
      LTEE=.TRUE.
      RETURN

C     No records are required for root diseased trees in this model.
      ENTRY RRTRES (ITN1,ITN2)
      ITN2 = MAXTP1
      RETURN

C     Called from INITRE. Normally initialization would happen here,
C     for each stand. Because there is only landscape control, this
C     is kludged in BMPPIN. 

      ENTRY RRINIT
      DO 90 I = 1, MAXTRE
        BMRR(I) = 0.0
        BMSR(I) = 0.0
   90 CONTINUE
      RETURN

C     Called from DAMCDS. This picks up all root disease codes.
C     Many parameters are ignored here. Must recognize 61,62,64
C     for root disease, and 35-39 for stem rust. Subratings are
C     not used.

      ENTRY RRDAM (II,IITH,IDD,ICODES,PROBB,HHT,DDBH,IISPI,
     >                  ITREII,TTHT)
      
      IF (IITH .LT. 6 .OR. IITH .GT. 9) THEN
        IF (LRRON) THEN
          DO 100 J = 1, 5, 2
            IC = ICODES(J)
            IF (IC.EQ.61.OR.IC.EQ.62.OR.IC.EQ.64) BMRR(II)= 0.01
  100     CONTINUE
        ENDIF
        IF (LSRON) THEN
          DO 101 J = 1, 5, 2
            IC = ICODES(J)
            IF (IC.GE.35.AND.IC.LE.39) BMSR(II)= 0.001
  101     CONTINUE
        ENDIF
        
      ENDIF

      RETURN

C     Not relevant to this model.
      ENTRY RRTRP (LTR)
      RETURN

C     Not relevant to this model.
      ENTRY RRROUT
      RETURN

C     Not relevant to this model.
      ENTRY RRPRIN (IX1)
      RETURN

C     Not relevant to this model.
      ENTRY RRESIN
      RETURN

C     Not relevant to this model.
      ENTRY RRTOUT
      RETURN

c     A very complicated model. Only inventoried infections increase.
c     Note that both RR (Root Rot) and SR (Stem Rust) are done here.
c     Option 1: 10% of existing infection. The following lines would be
c     used:
c       X1= EXP(FINT * LOG(1.0 + BMRRSR) / 10.)
c       BMRR(I) = AMIN1(1.0, BMRR(I) * X1)
c     Similar changes would be needed for BMSR() calculation.
c     Option 2: 10% increase. This requires testing if the value is > 0,
c     or everything will get infected. This is what is used in the code.

      ENTRY RRTREG
      IF (LRRON) THEN      
        X1 = BMRRSR * FINT / 10.
        DO 201 I = 1, ITRN
          IF (BMRR(I) .GT. 0.0) BMRR(I) = AMIN1(1.0, BMRR(I) + X1)
  201   CONTINUE
      ENDIF
      
      IF (LSRON) THEN      
        X1 = BMSRSR * FINT / 10.
        DO 202 I = 1, ITRN
          IF (BMSR(I) .GT. 0.0) BMSR(I) = AMIN1(1.0, BMSR(I) + X1)
  202   CONTINUE
      ENDIF
      
      RETURN

C     Compress BMRR and BMSR if they are in use.
      ENTRY RRCMPR (NCLAS,PROB,IND,IND1)

      IF (LRRON) THEN 
        I1 = 1
        DO 500 ICL=1,NCLAS
                  
          I2 = IND1(ICL)
          IREC1 = IND(I1)
          IF (I1 .EQ. I2) GOTO 480
              
          XP = PROB(IREC1)
          TXP = XP
          K = I1 + 1
          TXP1 = BMRR(IREC1) * XP
              
          DO 215 I=K,I2
            IREC = IND(I)
            XP = PROB(IREC)
            TXP = TXP + XP
            TXP1 = TXP1 + BMRR(IREC) * XP
  215     CONTINUE
      
C         DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
C         THE 'IREC1' POSITION IN THE ARRAYS.
      
          IF (TXP .LE. 0.0) THEN
            BMRR(IREC1) = 0.0
          ELSE
            BMRR(IREC1) = TXP1 / TXP
          ENDIF
      
  480     CONTINUE
          I1 = I2+1
  500   CONTINUE
      ENDIF

      IF (LSRON) THEN 
        I1 = 1
        DO 501 ICL=1,NCLAS
                  
          I2 = IND1(ICL)
          IREC1 = IND(I1)
          IF (I1 .EQ. I2) GOTO 481
              
          XP = PROB(IREC1)
          TXP = XP
          K = I1 + 1
          TXP1 = BMSR(IREC1) * XP
              
          DO 216 I=K,I2
            IREC = IND(I)
            XP = PROB(IREC)
            TXP = TXP + XP
            TXP1 = TXP1 + BMSR(IREC) * XP
  216     CONTINUE
      
C         DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
C         THE 'IREC1' POSITION IN THE ARRAYS.
      
          IF (TXP .LE. 0.0) THEN
            BMSR(IREC1) = 0.0
          ELSE
            BMSR(IREC1) = TXP1 / TXP
          ENDIF
      
  481     CONTINUE
          I1 = I2+1
  501   CONTINUE
      ENDIF
      
      RETURN

C     Not relevant to this model.
      ENTRY RRSTR (II1,RR1,RR2)
      RETURN

C     Not relevant to this model.
      ENTRY RRESCP (II1,II2)
      II2=II1
      RETURN

C     Not relevant to this model.
      ENTRY RRESTB (IX1,RX1)
      RETURN

C     Not relevant to this model.
      ENTRY RRMN1 (II)
      RETURN

C     Not relevant to this model.
      ENTRY RRMN2 (OLD)
      RETURN

C     Not relevant to this model.
      ENTRY RRPR
      RETURN

C     Since measure is *proportion*, weight is not required.
      ENTRY RRTRIP (IX1,IX2,RR1)
      BMRR(IX1) = BMRR(IX2)
      BMSR(IX1) = BMRR(IX2)
      RETURN

C     Setting to 0.0 may not be necessary.
      ENTRY RRTDEL (IVAC,IRECN,IX1)
      BMRR(IVAC) = BMRR(IRECN)
      BMSR(IVAC) = BMSR(IRECN)
      BMRR(IRECN) = 0.0
      BMSR(IRECN) = 0.0
      RETURN

      ENTRY RRKEY (KEY,KEYWRD)
      KEYWRD=NORR
      RETURN

      END
