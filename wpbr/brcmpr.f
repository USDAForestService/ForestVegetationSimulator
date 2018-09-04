      SUBROUTINE BRCMPR (NCLAS,PROB2,INDX,INDX1)
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C  This Blister Rust Model subroutine is called from the FVS routine
C  COMPRS.  It compresses the blister rust model variables.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Local variable declarations.

      INTEGER BOLCNT,BOLAVE,BRACNT,BRAAVE,ITRCNT,NCLAS,INDX(MAXTRE),
     &   INDX1(MAXTRE), I, I1, I2, I3, I4, ICL, IREC, J, K
      REAL TXP2,TXP3,TXP4,TXP5,TXP6,TXP7,TXP8,TXP9,TXP10,
     &   TXP12(10),TXP13(10),TXP14(10),TXP15(10),TXP16,PROB2(MAXTRE),
     &   GIRD, OUT, TXP, UP, XP
      LOGICAL LGO,DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCMPR',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRCMPR: cycle = ', I2)

C.... If Blister Rust not active, exit subroutine.

      CALL BRATV (LGO)
      IF(.NOT.LGO) GO TO 550

C.... Compress the blister rust tree records.
C.... Set flag, BRCMP, so statuses for the compressed cankers will
C.... be assigned by calling subroutine BRCSTA from subroutine BRPR.

      BRCMP=.TRUE.
      I1=1

      DO 500 ICL=1,NCLAS

C....    Set the pointers to the trees in the class.

         I2=INDX1(ICL)

C....    If there is only one record in clase ICL, then skip the class.

         IREC1=INDX(I1)
         IF(I1.EQ.I2) GO TO 300

C....    Create a cumulative PROB2 for all the tree in the class.
C....    Set tree record, bole canker, and branch canker counters for
C....    variables not compressed by a weighted average.

         ITRCNT=1
         I3=0
         I4=0
         BRACNT=0
         BOLCNT=0
         XP=PROB2(IREC1)
         TXP=XP
         K=I1+1
         TXP2=UPMARK(IREC1)
         TXP3=BRHTBC(IREC1)
         TXP4=GI(IREC1)*XP
         TXP5=RI(IREC1)*XP
         TXP6=BRGD(IREC1)*XP
         TXP7=ESTCAN(IREC1)*XP
         TXP8=TSTARG(IREC1)*XP
         TXP9=BRAGE(IREC1)
         TXP10=BRPB(IREC1)
         TXP16=ITCAN(IREC1)*XP

C....    Initialize arrays used to accumulate the values for bole
C....    and branch cankers.  Inactivated cankers are not included.
C....    Bole and branch cankers are accumulated seperately
C....    according to their order of occurance in the canker arrays.
C....    The first bole canker values for each tree are accumulated in
C....    TXP12(1) and TXP13(1).  The first branch canker values for
C....    each tree are accumulated in TXP14(1) and TXP(1).

         DO 50 J=1,10
            TXP12(J)=0.0
            TXP13(J)=0.0
            TXP14(J)=0.0
            TXP15(J)=0.0
            UP=DUP(J,IREC1)
            OUT=DOUT(J,IREC1)
            GIRD=GIRDL(J,IREC1)

            IF(ISTCAN(J,IREC1).EQ.-1.OR.J.GT.ILCAN(IREC1)) THEN
               CONTINUE
            ELSE IF(OUT.EQ.0.0.AND.UP.GT.0.0) THEN
               BOLCNT=BOLCNT+1
               I3=I3+1
               TXP12(I3)=UP
               TXP13(I3)=GIRD
            ELSE IF(OUT.GT.0.0) THEN
               BRACNT=BRACNT+1
               I4=I4+1
               TXP14(I4)=UP
               TXP15(I4)=OUT
            ENDIF
   50    CONTINUE

C....    Add in the other trees which form the class.

         DO 90 I=K,I2
            I3=0
            I4=0
            ITRCNT=ITRCNT+1
            IREC=INDX(I)
            XP=PROB2(IREC)
            TXP=TXP+XP
            TXP2=TXP2+UPMARK(IREC)
            TXP3=TXP3+BRHTBC(IREC)
            TXP4=TXP4+GI(IREC)*XP
            TXP5=TXP5+RI(IREC)*XP
            TXP6=TXP6+BRGD(IREC)*XP
            TXP7=TXP7+ESTCAN(IREC)*XP
            TXP8=TXP8+TSTARG(IREC)*XP
            TXP9=TXP9+BRAGE(IREC)
            TXP10=TXP10+BRPB(IREC)
            TXP16=TXP16+ITCAN(IREC)*XP

            DO 80 J=1,ILCAN(IREC)
               UP=DUP(J,IREC)
               OUT=DOUT(J,IREC)
               GIRD=GIRDL(J,IREC)

               IF(ISTCAN(J,IREC).EQ.-1) THEN
                  CONTINUE
               ELSE IF(OUT.EQ.0.0.AND.UP.GT.0.0) THEN
                  BOLCNT=BOLCNT+1
                  I3=I3+1
                  TXP12(I3)=TXP12(I3)+UP
                  TXP13(I3)=TXP13(I3)+GIRD
               ELSE IF(OUT.GT.0.0) THEN
                  BRACNT=BRACNT+1
                  I4=I4+1
                  TXP14(I4)=TXP14(I4)+UP
                  TXP15(I4)=TXP15(I4)+OUT
               ENDIF
   80       CONTINUE
   90    CONTINUE

C....    Divide by the total PROB for weighted average values.
C....    Divide by the tree record count for average values.
C....    Move the values into the IREC1 position in the arrays.

         UPMARK(IREC1)=TXP2/ITRCNT
         BRHTBC(IREC1)=TXP3/ITRCNT
         GI(IREC1)=TXP4/TXP
         RI(IREC1)=TXP5/TXP
         BRGD(IREC1)=TXP6/TXP
         ESTCAN(IREC1)=TXP7/TXP
         TSTARG(IREC1)=TXP8/TXP
         BRAGE(IREC1)=TXP9/ITRCNT
         BRPB(IREC1)=TXP10
         ITCAN(IREC1)=INT(TXP16/ITRCNT)
         ISTOTY(IREC1)=5
         ICRED(IREC1)=0

C....    Load the cankers for the compressed tree record.
C....    The average (not weighted average) number of cankers (ILCAN)
C....    is carried to the compression record.
C....    Bole cankers, if any, are stored in the first positions of the
C....    canker arrays and branch cankers will be stored in the
C....    following positions.

C....    Tree's status code is set to 1 if it has any lethal cankers,
C....    just to indicate that it is not clean.  A more accurate
C....    status code will be assigned during the regular cycling process

         BOLAVE=INT(BOLCNT/ITRCNT)
         BRAAVE=INT(BRACNT/ITRCNT)
         ILCAN(IREC1)=BOLAVE+BRAAVE

         IF(ILCAN(IREC1).GT.0) THEN
            IBRSTAT(IREC1)=1
         ELSE
            IBRSTAT(IREC1)=0
         ENDIF

         IF(BOLAVE.GT.0) THEN
            DO 110 I=1,BOLAVE
               DUP(I,IREC1)=TXP12(I)/BOLCNT
               DOUT(I,IREC1)=0.0
               GIRDL(I,IREC1)=TXP13(I)/BOLCNT
               ISTCAN(I,IREC1)=0
  110       CONTINUE
         ENDIF

         IF(BRAAVE.GT.0) THEN
            DO 120 I=1,BRAAVE
               I4=BOLAVE+I
               DUP(I4,IREC1)=TXP14(I)/BRACNT
               DOUT(I4,IREC1)=TXP15(I)/BRACNT
               GIRDL(I4,IREC1)=0.0
               ISTCAN(I4,IREC1)=0
  120       CONTINUE
         ENDIF

C....    End of compression loop.

  300    CONTINUE

C....    Re-define I1 such that it points to the first
C....    index in the next class.

         I1=I2+1
  500 CONTINUE

C.... Common return.

  550 CONTINUE
      IF(DEBUG) WRITE(JOSTND, 600) ICYC
  600 FORMAT('Leaving subroutine BRCMPR: cycle = ', I2)
      RETURN
      END
