      SUBROUTINE SPNBBD (II,JJ,BORD,IRC)
      IMPLICIT NONE
C----------
C  **SPNBBD  DATE OF LAST REVISION:  07/31/08
C----------
C     RETURNS THE AMOUNT OF BORDER BETWEEN STAND II AND JJ.
C     IRC = RETURN CODE. O=DO BORDER, 1=DO NOT BORDER, 2=NO DATA.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PPEPRM.F77'
C
C
      INCLUDE 'PPSPNB.F77'
C
C
COMMONS
C
      INTEGER IRC,JJ,II,I,J,L,M,K
      REAL BORD
C
      BORD=0.0
      IF (NIDS.LE.0) THEN
         IRC=2
      ELSE
C
C        IF THE SECOND COL OF KEYLST CORRESPONDING TO II OR JJ IS
C        NEGETIVE, THEN ITS ABSOLUTE VALUE POINTS TO A ROW IN KEYLST
C        THAT SHOULD BE USE TO ACCESS THE NEIGHBOR INFO.
C
         IF (KEYLST(II,2).LT.0) THEN
            I=-KEYLST(II,2)
         ELSE
            I=II
         ENDIF
         IF (KEYLST(JJ,2).LT.0) THEN
            J=-KEYLST(JJ,2)
         ELSE
            J=JJ
         ENDIF
C
C        IF I AND J ARE EQUAL, THEY ARE THE SAME STAND AND A STAND
C        DOES NOT BORDER ITSELF.
C
         IF (I.NE.J) THEN
            IF (KEYLST(I,1).LE.KEYLST(J,1)) THEN
               L=I
               M=J
            ELSE
               L=J
               M=I
            ENDIF
            DO 10 K=KEYLST(L,1),KEYLST(L,2)
            IF (M.EQ.NBID2(NBORD(K))) THEN
               BORD=BORDER(NBORD(K))
               IRC=0
               GOTO 20
            ENDIF
   10       CONTINUE
         ENDIF
         IRC=1
      ENDIF
   20 CONTINUE
      RETURN
      END
