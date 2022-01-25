      SUBROUTINE COLMOD (IFINT,ICYC,IYEAR,NPLT)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     BOUSFIELD'S NEW Q VALUE EQUATION IS IN THIS VERSION.
C
C     VARIABLES :
C        DEADCL - ARRAY OF TOTAL INFESTED TREES BY DIAMETER CLASS.
C        DEAD   - ARRAY OF INFESTED TREES BY DIAMETER CLASS AND YEAR IN
C                 INFESTATION CYCLE.
C        GREEN  - ARRAY OF UNINFESTED TREES BY DBH CLASS AND INF. YEAR.
C        ZINMRT - ARRAY OF INITIAL MORTALITY RATES USED TO
C                 DETERMINE INITIAL NUMBER OF INFESTED TREES
C        PRNOIN - ARRAY OF PROBABILITIES OF A TREE  -NOT-  BEING
C                 INFESTED.  ARRAY IS REFERENCED BY  ????
C                 (DBH CLASSES FOR THIS ARRAY ARE 6-14'',14-16',
C                 AND 16+")
C        TDEAD  - ARRAY BY INFESTATION YEAR OF TOTAL NUMBER OF
C                 DEAD TREES
C        TGREEN - ARRAY BY INFESTATION YEAR OF TOTAL NUMBER OF
C                 LIVE TREES
C
C Revision History
C   12/23/99; Lance R. David (FHTET-FC)
C     Updated for expansion of FVS stand id (variable NPLT)
C     from 8 to 26 characters.
C   02/06/04  Lance R. David (FHTET)
C     Changed the minimum number of GREEN trees that must exist in
C     a DBH class before trees from that class can be killed.
C     The original value of 1.0E-12 was changed to 0.03.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C
C**********************************************************************
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COLCOM.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS
C
C
      CHARACTER*26 NPLT

      INTEGER MDGREN(7), MDDEAD(7), MDDTOT, MDGTOT, IOUT,
     &        IYEAR, ICYC, IFINT, IRND, I, J, K, L, NUMYRS

      REAL    PMIN(10), PMAX(10), WAYNE(10), X, DTOTAL, PNEW
      

C
C      Q VALUES FOR MIN AND MAX BOUNDARIES, BOUSFIELD'S NORMAL TPA
C
      DATA PMIN/.970,.950,.900,.800,.600,.500,.400,.150,.100,.050/
      DATA PMAX/1.0,1.0,1.0,1.0,.990,.980,.970,.900,.800,.700/
      DATA WAYNE/150.,100.,95.,66.,35.5,16.5,5.5,3.,1.5,.80/
 
C
C     STATEMENT FUNCTION IRND (ROUNDING FUNCTION)
C
      IRND(X) = IFIX(X + 0.5)
 
C
C     PRINT DEBUG IF REQUESTED.
C
      IF (DEBUIN) WRITE(JOMPB,1) IYEAR,NPLT
    1 FORMAT ('ENTER COLMOD (MPB), IYEAR=',I4,'; NPLT=',A26)

C
C     ADD UP THE STARTING GREEN TREES.
C
      STOTAL = 0.0
      DO 2 I = 1,10
         STOTAL = STOTAL + START(I)
    2 CONTINUE
 
      IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, STOTAL=', STOTAL
C
C     FIRST OBTAIN INITIAL INFESTATION VALUES.
C
      DTOTAL = 0.0

      DO 10 I = 1,10
         IF (ICYC .EQ. 1 .AND. (LCURMR .OR. LINVMR)) THEN
            DTOTAL = DTOTAL + CURRMR(I)
            IF (START(I) * ZINMOR(I) .GT. GREINF(I) + CURRMR(I))
     >          GREINF(I) = START(I) * ZINMOR(I) - CURRMR(I)
            IF (GREINF(I) .GT. START(I)) GREINF(I) = START(I)
            DEAD(1,I) = GREINF(I) + CURRMR(I)
            GREEN(1,I) = START(I) - GREINF(I)
         ELSE
            DEAD(1,I) = START(I) * ZINMOR(I)
            GREEN(1,I) = START(I) - DEAD(1,I)
         ENDIF

         TDEAD(1) = TDEAD(1) + DEAD(1,I)
         TGREEN(1) = TGREEN(1) + GREEN(1,I)
   10 CONTINUE

      IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, TDEAD(1)=', TDEAD(1),
     >   ' TGREEN(1)=', TGREEN(1), ' DTOTAL=', DTOTAL
C
C     SET NUMYRS TO THE LENGTH OF THE OUTBREAK IN YEARS.
C
      IF (MPMXYR .GT. IFINT) THEN
         NUMYRS = IFINT
      ELSE
         NUMYRS = MPMXYR
      ENDIF

      IF (NUMYRS .GT. 10)  NUMYRS = 10

      IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, NUMYRS=', NUMYRS,
     >  ' IBOUSE=', IBOUSE

      DO 35 K=2,NUMYRS
         IF ((TDEAD(K-1) .LE. 0.0005) .OR. (TGREEN(K-1) .LE. 0.0005))
     &      GOTO 39
C
C        OTHERWISE, AN OUTBREAK IS IN PROGRESS
C
         DO 30 J=1,10
C
C           NEW ADDITIONS TO MODEL 4/13/88 TO ADD EFFECT OF WAYNE
C           BOUSFIELD'S CHANGES TO THE MODEL.
C           06-FEB-04 Lance David
C           Changed 1.E12 to 0.03 in .LT. condition below. In this
C           case, a size class having less than 3 tree per 100 acres is
C           of no significance and well outside the min and max values
C           imposed directly following.

            IF (IBOUSE .EQ. 1) THEN
               IF (GREEN(1,J) .LT. 0.03 ) THEN
                  PNEW = 0.0
               ELSE
                  IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, PRNOIN(',
     >              J,')=',PRNOIN(J),' WAYNE=',WAYNE(J),' GREEN=',
     >              GREEN(1,J)
                  PNEW = PRNOIN(J)**(WAYNE(J) / GREEN(1,J))
               ENDIF

               IF (PNEW .LT. PMIN(J)) PNEW = PMIN(J)
               IF (PNEW .GT. PMAX(J)) PNEW = PMAX(J)
            ELSE
               PNEW = PRNOIN(J)
            ENDIF

            IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, PNEW=', PNEW,
     >        ' GREEN=', GREEN(K-1,J),' DEAD=', DEAD(K-1,J)

            DEAD(K,J) = GREEN(K-1,J) * (1.0 - (PNEW**DEAD(K-1,J)))

            DEADCL(J) = DEADCL(J) + DEAD(K,J)
            TDEAD(K) = TDEAD(K) + DEAD(K,J)
            GREEN(K,J) = GREEN(K-1,J) - DEAD(K,J)
            TGREEN(K) = TGREEN(K) + GREEN(K,J)
   30    CONTINUE
         TDEAD(K) = TDEAD(K) + TDEAD(K-1)
   35 CONTINUE

C.... Tell the event monitor the number of trees killed by mountain
C.... pine beetle.

      IF (DEBUIN) WRITE(JOMPB,*) 'IN COLMOD, CALL EVSET4, TDEAD(10)=',
     >   TDEAD(10)

      CALL EVSET4 (4,TDEAD(10))
      
C
C     OUTPUT TREES PER DBH, TOTAL DEAD AND TOTAL LIVE PER TIME
C
   39 CONTINUE
      WRITE(JOMPB,37) IYEAR, NUMYRS
   37 FORMAT (//,'MOUNTAIN PINE BEETLE RATE OF LOSS MODEL :  OUTBREAK',
     >       ' IN YEAR = ',I4,' FOR ',I2,' YEARS.',/)

      WRITE(JOMPB,40)
   40 FORMAT (19X,'TREES/ACRE OF LIVE LODGEPOLE PINE BY DBH CLASS',T85,
     &        ' TOTAL    TOTAL')

      WRITE(JOMPB,41)
   41 FORMAT ('TIME     1-3    3-5    5-7    7-9    9-11  11-13 ',
     &        ' 13-15  15-17  17-19   19+          LIVE     DEAD'/)

      IF (JOMPBX .NE. 0) WRITE(JOMPBX,*) 'SUMMARY BY YEAR'

      WRITE(JOMPB,42) (START(J),J=1,10),STOTAL,DTOTAL
      IF (JOMPBX .NE. 0) WRITE(JOMPBX,42) (START(J),J=1,10),
     &                   STOTAL, DTOTAL

   42 FORMAT ('START ',10F7.2,5X,2F9.2)

      DO 100 L = 1,NUMYRS
         IOUT = L + IYEAR
         WRITE(JOMPB,50) IOUT,(GREEN(L,J),J=1,10), TGREEN(L), TDEAD(L)
         IF (JOMPBX .NE. 0) WRITE(JOMPBX,50) IOUT,(GREEN(L,J),J=1,10),
     &                      TGREEN(L), TDEAD(L)
   50    FORMAT (I4,2X,10F7.2,5X,2F9.2)
  100 CONTINUE

C
C     IF DEBUG IS REQUESTED, WRITE INFO.
C
      IF (DEBUIN) THEN
C
C     PREPARE OUTPUT THAT HAS THE SAME FORMAT AS GENERATED FROM
C     THE OTHER STAND ALONE MODEL...(WITH 7 CLASSES).
C
        DO 120 L = 1,NUMYRS
           IOUT = L + IYEAR - 1
           MDDEAD(1) = IRND(DEAD(L,3))
           MDGREN(1) = IRND(GREEN(L,3))

           DO 115 J = 4,8
              MDDEAD(J-2) = IRND(DEAD(L,J))
              MDGREN(J-2) = IRND(GREEN(L,J))
  115      CONTINUE

           MDDEAD(7) = IRND(DEAD(L,9) + DEAD(L,10))
           MDGREN(7) = IRND(GREEN(L,9) + GREEN(L,10))
           MDGTOT = 0
           MDDTOT = 0

           DO 116 J = 1,7
              MDGTOT = MDGTOT + MDGREN(J)
              MDDTOT = MDDTOT + MDDEAD(J)
  116      CONTINUE

           WRITE(JOMPB,117) IOUT,NPLT,
     >                      (MDGREN(J),MDDEAD(J),J=1,7),
     >                      MDGTOT, MDDTOT
  117      FORMAT (I4,1X,A26,5X,16I5)
  120   CONTINUE
      ENDIF

      IF (DEBUIN) WRITE(JOMPB,*) 'EXIT COLMOD'
      RETURN
      END
