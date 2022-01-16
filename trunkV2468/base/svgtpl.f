      SUBROUTINE SVGTPL
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     RETURNS THE LOWER LEFT AND UPPER RIGHT POINTS FOR A PLOT,
C     GIVEN IPTINV PLOTS ARE ON ONE SQUARE ACRE...IN FEET!!!!
C
C     OR IF THE PLOT IS CIRCULAR, RETURNS THE TWO DIFFERENT RADII
C     AND ANGLES BOUNDING THE REGION
C
C     NR IS THE NUMBER OF ROWS
C     NC IS THE NUMBER OF COLUMNS
C     NCF IS THE NUMBER OF THE FIRST COLUMN THAT CONTAINS AN
C         EXTRA ROW...0 IF NONE CONTAIN EXTRA ROWS.
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'ARRAYS.F77'

COMMONS

      INTEGER NPLPRG(105)
      INTEGER MPLPRG(105)
      REAL SQFTAC,SQRTFA,RADPI,YLEN,XLEN,AP,YLEN1,XLEN1,YLEN2,XLEN2,
     >     APP,ANG
      REAL SQFTACI,SQRTFAI
      REAL SQFTACM,SQRTFAM
      INTEGER I,NR,NC,NCF,IC,IR,K,IXN,N,J,NP,IPPC

      DATA RADPI   /3.14159/
      DATA SQFTACI /43560./, SQRTFAI /208.7103/
      DATA SQFTACM /10000./, SQRTFAM /100./

      IF (IMETRIC.EQ.0) THEN
        SQFTAC = SQFTACI
        SQRTFA = SQRTFAI
      ELSE
        SQFTAC = SQFTACM
        SQRTFA = SQRTFAM
      ENDIF

C     THIS CALCULATES THE MAX OF ITRE() OR IPTINV, WHICH EVER IS GREATER
C     THAT VALUE IS ASSIGNED TO ISVINV

      ISVINV = IPTINV
      IF (ITRN.GT.0) THEN
        DO I = 1,ITRN
          IF (ITRE(I) .GT. ISVINV) ISVINV = ITRE(I)
        ENDDO
      ENDIF

C     THIS ROUTINE IS FOR A SQUARE PLOT THAT IS SUBDIVIDED

      IF (IPLGEM.EQ.1) THEN
        DO I=1,ISVINV
          NR = IFIX(SQRT(FLOAT(ISVINV)))
          IF (NR*NR .EQ. ISVINV) THEN
            NC = NR
            NCF= 0
          ELSEIF (NR*(NR+1) .LT. ISVINV) THEN
            NC = NR+1
            NCF= ISVINV-(NC*NR)
            NCF= NC-NCF+1
          ELSE
            NC = NR
            NCF= ISVINV-(NC*NR)
            NCF= NC-NCF+1
          ENDIF

C         IR IS THE ROW NUMBER FOR POINT I
C         IC IS THE COLUMN NUMBER FOR POINT I

          IF (NCF.EQ.0) THEN
            IC = I/NR
            IF (MOD(I,NR) .GT. 0) IC=IC+1
            IR = I-((IC-1)*NR)
          ELSEIF (I .LE. NR*(NCF-1)) THEN
            IC = I/NR
            IF (MOD(I,NR) .GT. 0) IC=IC+1
            IR = I-((IC-1)*NR)
          ELSE
            K  = I-(NR*(NCF-1))
            IC = K/(NR+1)
            IF (MOD(K,(NR+1)) .GT. 0) IC=IC+1
            IR = K-((IC-1)*(NR+1))
            IC = IC+NCF-1
          ENDIF

          IF (NCF.EQ.0) THEN
            YLEN = SQRTFA/FLOAT(NR)
            XLEN = SQRTFA/FLOAT(NC)
            X1R1S(I) = XLEN*(IC-1)
            X2R2S(I) = X1R1S(I)+XLEN
            Y1A1S(I) = YLEN*(IR-1)
            Y2A2S(I) = Y1A1S(I)+YLEN
          ELSE
            AP = SQFTAC/ISVINV
            YLEN1 = SQRTFA/NR
            XLEN1 = AP/YLEN1
            IF (IC.LT.NCF) THEN
              X1R1S(I) = XLEN1*(IC-1)
              X2R2S(I) = X1R1S(I)+XLEN1
              Y1A1S(I) = YLEN1*(IR-1)
              Y2A2S(I) = Y1A1S(I)+YLEN1
            ELSE
              YLEN2 = SQRTFA/(NR+1)
              XLEN2 = AP/YLEN2
              X1R1S(I) = ((NCF-1)*XLEN1)+((IC-NCF)*XLEN2)
              X2R2S(I) = X1R1S(I)+XLEN2
              Y1A1S(I) = YLEN2*(IR-1)
              Y2A2S(I) = Y1A1S(I)+YLEN2
            ENDIF
          ENDIF
        ENDDO

C     THIS IS FOR A CIRCULAR PLOT THAT IS SUBDIVIDED

      ELSEIF(IPLGEM.EQ.3) THEN

        IF (ISVINV.GT.4) THEN

C         FINDS THE NUMBER OF CIRCULAR RINGS IN AREA
C         IXN IS THE NUMBER OF RINGS FOR THE PLOT

          IXN=IFIX(SQRT(FLOAT(ISVINV)))
          IF(IXN*IXN.NE.ISVINV) IXN=IXN+1
          IF(MOD(IXN,2).EQ.0) THEN
            IXN=IXN+2
          ELSE
            IXN=IXN+1
          ENDIF
          IXN=IXN/2

C         FINDS THE MAX NUMBER OF PLOTS PER RING
C         MPLPRG IS THE ARRAY FOR THE MAX NUMBER OF PLOTS PER RING
C         NPLPRG IS THE ARRAY FOR THE NUMBER OF PLOTS PER RING

          MPLPRG(1)=1
          MPLPRG(2)=8
          N=ISVINV
          DO J=3,IXN
            MPLPRG(J)=8+MPLPRG(J-1)
          ENDDO

          DO J=1,105
            NPLPRG(J)=0
          ENDDO

          DO J=1,(IXN-1)
            NPLPRG(J)=J*J
            N=N-NPLPRG(J)
          ENDDO

   10     CONTINUE
          NPLPRG(IXN)=NPLPRG(IXN)+1
          N=N-1
          IF (N.GT.0) THEN
            IF (NPLPRG(IXN).LT.(NPLPRG(IXN-1)+8)) GOTO 10
          ENDIF

   20     CONTINUE

          DO J=2,IXN
            IF (N.GT.0) THEN
              IF (NPLPRG(J).LT.MPLPRG(J)) THEN
                NPLPRG(J)=NPLPRG(J)+1
                N=N-1
              ENDIF
            ENDIF
          ENDDO
          IF (N.GT.0) GOTO 20

        ELSE
          IXN=1
          NPLPRG(1)=ISVINV
        ENDIF

C        NP IS THE PLOT NUMBER
C        APP IS THE AREA PER PLOT
C        IPPC IS THE PLOTS PER CIRCLE WITHIN THE WHOLE ACRE
C        ANG IS THE ANGLE DIFFERENCE PER RING

        X1R1S(1)=0
        Y1A1S(1)=0
        NP=1
        APP=SQFTAC/ISVINV
        IF(IXN.GT.1) THEN
          Y2A2S(1)= 2*RADPI
          X2R2S(1)= SQRT(APP/RADPI)
          X1R1S(2)=X2R2S(1)
          Y1A1S(2)=0
          NP=NP+1

          IF (IXN.GT.2) THEN
            IPPC = 1
            DO I=2,(IXN-1)
              IPPC=IPPC+NPLPRG(I)
              X2R2S(NP)=SQRT((APP*IPPC)/RADPI)
              ANG = ((2*RADPI)/NPLPRG(I))
              Y2A2S(NP) = ANG
              DO J=1,NPLPRG(I)-1
                X2R2S(NP+1)=X2R2S(NP)
                X1R1S(NP+1)=X1R1S(NP)
                Y2A2S(NP+1)=Y2A2S(NP)+ANG
                Y1A1S(NP+1)=Y2A2S(NP)
                NP=NP+1
              ENDDO
              NP=IPPC+1
              X1R1S(NP)=X2R2S(NP-1)
              Y1A1S(NP)=0
            ENDDO
          ENDIF
        ENDIF

        IF (IMETRIC.EQ.0) THEN
          X2R2S(NP)=117.7522
        ELSE
          X2R2S(NP)=56.42
        ENDIF

        ANG = ((2*RADPI)/NPLPRG(IXN))
        Y2A2S(NP) = ANG
        DO J=1,NPLPRG(IXN)-1
          X2R2S(NP+1)=X2R2S(NP)
          X1R1S(NP+1)=X1R1S(NP)
          Y2A2S(NP+1)=Y2A2S(NP)+ANG
          Y1A1S(NP+1)=Y2A2S(NP)
          NP=NP+1
        ENDDO

C     THIS IS FOR A NON-SUBDIVIDED SQUARE PLOT

      ELSEIF(IPLGEM.EQ.0) THEN
        DO I=1,ISVINV
          X1R1S(I)=0
          Y1A1S(I)=0
          IF (IMETRIC.EQ.0) THEN
            X2R2S(I)=208.7103
            Y2A2S(I)=208.7103
          ELSE
            X2R2S(I)=100.0
            Y2A2S(I)=100.0
          ENDIF
        ENDDO

C     THIS IS FOR A NON-SUBDIVIDED CIRCULAR PLOT

      ELSE
        DO I=1,ISVINV
          X1R1S(I)=0
          Y1A1S(I)=0

          IF (IMETRIC.EQ.0) THEN
            X2R2S(I)=117.7522
          ELSE
            X2R2S(I)=56.42
          ENDIF

          Y2A2S(I)=2*RADPI
        ENDDO
      ENDIF

      RETURN
      END
