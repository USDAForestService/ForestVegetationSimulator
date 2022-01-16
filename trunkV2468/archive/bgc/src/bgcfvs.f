      SUBROUTINE BGCFVS(X)
C--------------------
C     BGCFVS BGC--DATE OF LAST REVISION: 05/29/01
C                 Revised 11/12/02.  Removing index ISTND, and removing PPE
C                                    common "includes" (PPEPRM, PPCNTL, &
C                                    PRGPRM).  AJM
C                 These changes--also made in BGCGO, BGCGROW, BGCIN, BGCINT,
C                 BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
C                 The FVS-BGC code is now, once again, a single stand model.
C                 Revised 6/3/03 AJM.  Added line re-setting WK2(I) to 0.0 if 
C                                      there is no BGC-projected mortality
C--------------------
C
C     REPLACES DIAMETER, HEIGHT, CR AND PROB, IF USER INVOKES INTERACTIVE MODE
C     (i.e. IF KEYWORD BGCGROW IS USED).
C     CHANGES ARE MADE FROM HERE DIRECTLY TO FVS's COMMON BLOCK VARIABLES
C     THIS ROUTINE IS CALLED TWICE,  FIRST CALL DEALS WITH HT/DIAM/PROB; SECOND CALL
C     DEALS WITH CROWN RATIO.
C
C     IBGC(ISTND)[note: IBGC is no longer dimensioned!--11/02 AJM ] IS SET
C     TO 1 IN BGCIN IF BGC INCREMENTS ARE TO BE USED IN FVS.
C     X IS FLAG PASSED FROM FVS INDICATING WHICH INCREMENTS ARE READY TO BE UPDATED.
C     WHEN X=1, HT, DIAM, AND PROB ARE UPDATED; WHEN X=2, CROWN RATIOS ARE UPDATED.
C
C     CALLED FROM: GRADD OF FVS BASE CODE
C 
COMMONS
      INCLUDE 'ENTITY.F77'
      INCLUDE 'BGCCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
C      INCLUDE 'PPCNTL.F77'                            ! removed 11/02 ajm
      INCLUDE 'PRGPRM.F77'
C
      INTEGER X     ! FLAG FROM FVS DICTATING WHICH INCREMENT IT IS READY TO HAVE BGC UPDATE.
C     REAL BGC_DG(*), BGC_HG(*)
C---------------------
C     LOAD BGC INCREMENTS INTO FVS ARRAYS. VALUES ARE IN ENGLISH UNITS
C---------------------
C      IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 1) THEN       !removed ajm 11/02
      IF (IBGC .EQ. 1 .AND. X .EQ. 1) THEN
         DO 100 I=1,ITRN
            DG(I)=-99.
            DO 200 J=1, NT
               IF(IDTREE(I).EQ.TREENO(J)) THEN
                 DG(I)=PASSDG(J)
                 HTG(I)=PASSHG(J)
                 WK2(I)=0.0
                 GO TO 100
               END IF
  200       CONTINUE
         IF (DG(I) .LT. 0.0) THEN
            DG(I)=0.0
            HTG(I)=0.0
            WK2(I)=PROB(I)
         END IF
  100    CONTINUE

C      ELSE IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 2) THEN  !removed ajm 11/02
      ELSE IF (IBGC .EQ. 1 .AND. X .EQ. 2) THEN
        DO 300 I=1,ITRN
          DO 400 J=1, NT
            IF(IDTREE(I).EQ.TREENO(J)) THEN
              ICR(I)=IFIX(CR(J)*100)
              GO TO 300
            END IF
  400     CONTINUE
  300   CONTINUE
C
      END IF
C
      RETURN
      END
