!== last modified 03-25-2014
      SUBROUTINE SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,
     >  TRIM,NUMSEG,LOGLEN)
C--  THIS SUBROUTINE WILL DETERMINE THE LENGTH OF EACH
C--  SEGMENTS GIVEN MERCHANTABLE LENGTH OF TREE STEM AND
C--  THE NUMBER OF SEGMENTS IN IT (DETERMINED IN SUBROUTINE
C--  NUMLOG).  SEGMENT LENGTHS ARE DETERMINED ACCORDING TO
C--  ONE OF THE DEFINED SEGMENTATION RULES IN THE VOLUME
C--  ESTIMATOR HANDBOOK FSH ???.

C--  THE LOGIC WAS DEVELOPED BY JIM BRICKELL, TCFPM R-1, AND
C--  IMPLEMENTED BY WO-TM.
C--  VARIABLES OF INTEREST LISTED AT END OF ROUTINE

      INTEGER EVOD, NUMSEG, OPT,I,AVLEN,KNTIT
      REAL LEFTOV, LMERCH, LOGLEN(20), MAXLEN, MINLEN, TRIM

c      INCLUDE 'COMM0'
c      INCLUDE 'COMM6'


      DO 5 I=1,20
        LOGLEN(I)=0.0
    5 CONTINUE

      IF (NUMSEG .EQ. 0) THEN
          LMERCH=0.0
          RETURN
      ENDIF


C--  DETERMINE MERCHANTABLE LENGTH MINUS TRIM AND ROUND
C--  TO THE NEAREST FOOT OR NEAREST EVEN FOOT


      LMERCH=LMERCH-(FLOAT(NUMSEG)*TRIM)
      IF (EVOD .EQ. 1) THEN
          LMERCH=INT(LMERCH+0.5)
      ELSE
          LMERCH=INT((LMERCH+1.0)/2.0)*2.0
c      ELSE
c          WRITE (REPOUT,10)
c   10     FORMAT(' ERROR FROM SEGMNT - LMERCH NOT ROUNDED')
      ENDIF

C--   BECAUSE OF ROUNDING LMERCH MIGHT BE LONGER THAN THE
C--   THE SUM OF THE PIECES BUT NOT LONG ENOUGH TO MAKE A NEW
C--   SEGMENT IF TRIM IS CONSIDERED.  IF THIS IS THE CASE FORCE
C--   LMERCH TO BE EQUAL TO THE SUM OF THE MAXIMUN SEGMENT LENGTHS

      IF (LMERCH .GT. (FLOAT(NUMSEG)*MAXLEN)) THEN
          LMERCH=FLOAT(NUMSEG)*MAXLEN
      ENDIF

C--   CHECK FOR ONE LOG TREE

      IF (NUMSEG .EQ. 1) THEN
        IF (OPT .EQ. 24) THEN

C--       OPTION 24 ROUNDS TO HALF LOG
             IF (LMERCH .LT. (MAXLEN*.25)) THEN
                 LOGLEN(NUMSEG)=0.0
             ELSEIF (LMERCH .GE. (MAXLEN*.25) .AND.
     >               LMERCH .LE. (MAXLEN*.75)) THEN
                  LOGLEN(NUMSEG)=MAXLEN/2.0
             ELSE
                  LOGLEN(NUMSEG)=MAXLEN
             ENDIF

        ELSEIF (LMERCH .GE. MINLEN) THEN
            IF (LMERCH .GT. MAXLEN) LMERCH=MAXLEN
            LOGLEN(1)=LMERCH
        ELSE
            LOGLEN(1)=0
        ENDIF
      ELSE

C--   MORE THAN ONE LOG TREES

        IF (OPT .LT. 20) THEN

C--        DEAL WITH ALL SEGMENTS - ODD LENGTHS OK

           AVLEN=INT(LMERCH/FLOAT(NUMSEG))
           LEFTOV=LMERCH-(AVLEN*FLOAT(NUMSEG))

C--        SET ALL LENGTHS EQUAL TO THE AVGLEN.

           DO 15 I=1,NUMSEG
               LOGLEN(I)=AVLEN
   15      CONTINUE

C--        IF AVLEN IS AN ODD NUMBER ADD ONE FOOT TO BOTTOM
C--        LOG, NEXT LOWEST LOG, ECT., WHILE AT THE SAME TIME
C--        SUBTRACTING ONE FOOT FROM THE TOP LOG, THE NEXT
C--        TO THE TOP LOG, ECT.

           IF(AVLEN .GT. INT(AVLEN/2.0)*2.0) THEN
              DO 20 I=1,NUMSEG
                 IF((NUMSEG-2*I+1) .GE. 1) THEN
                    LOGLEN(I)=LOGLEN(I)+1.0
                    LOGLEN(NUMSEG-I+1)=LOGLEN(NUMSEG-I+1)-1.0
                 ENDIF
   20         CONTINUE
           ENDIF

           IF( LEFTOV .GT. 0) THEN

C--           THERE WAS A LEFTOVER AMOUNT, LMERCH WAS NOT EVENLY
C--           DIVISIBLE BY NUMSEG.

              IF (NUMSEG .GT. INT(FLOAT(NUMSEG)/2.0)*2.0) THEN

C--           DISTRIBUTE LEFTOV LENGTH AMONGST THE ODD
C--           LENGTH SEGMENTS, IF THERE ARE ANY.  THERE WILL
C--           ONLY BE ODD LENGTHS IF NUMSEG IS ODD.

                 DO 30 I=1,NUMSEG
                    IF (LEFTOV .GT. 0) THEN
                       IF(LOGLEN(I) .GT. INT(LOGLEN(I)/2.0)*2) THEN
                          LOGLEN(I)=LOGLEN(I)+1.0
                          LEFTOV=LEFTOV-1.0
                       ENDIF
                    ENDIF
   30            CONTINUE
              ENDIF

C--           DISTRIBUTE THE REST OF LEFTOV, AT THIS POINT
C--           ALL LOG LENGTHS ARE ASSUMED TO BE EVEN IF LEFTOV
C--           IS GREATER THAN ZERO

              IF (LEFTOV .GT. 0) THEN
                 KNTIT=0
   40            DO 50 I=1,NUMSEG
                    IF (LEFTOV.GT.0 .AND. LOGLEN(I).LT.MAXLEN) THEN

C--                    MAKE SURE 1 OR 2 FOOT SECTION IS ADDED
C--                    TO THE LOWEST AND SHORTEST PIECE.

                       IF(LEFTOV .GE. 2) THEN
                          IF(LOGLEN(I).EQ.LOGLEN(NUMSEG))THEN
                             LOGLEN(I)=LOGLEN(I)+2.0
                             LEFTOV=LEFTOV-2
                          ELSEIF (LOGLEN(I) .GT. LOGLEN(I+1)) THEN
                             LOGLEN(I+1)=LOGLEN(I+1)+2.0
                             LEFTOV=LEFTOV-2
                          ENDIF
                       ELSE
                          IF(LOGLEN(I).EQ.LOGLEN(NUMSEG))THEN
                             LOGLEN(I)=LOGLEN(I)+1.0
                             LEFTOV=LEFTOV-1
                          ELSEIF (LOGLEN(I) .GT. LOGLEN(I+1)) THEN
                             LOGLEN(I+1)=LOGLEN(I+1)+1.0
                             LEFTOV=LEFTOV-1
                          ENDIF
                       ENDIF
                    ENDIF
   50            CONTINUE

                 KNTIT=KNTIT+1
                 IF (KNTIT .GT. 500) THEN
c                    WRITE(*,60)
c   60               FORMAT(' PRINTED IN SEGMNT STATEMENT',
c     >              ' NUMBER 60 - ERROR IN PARAMETERS')
                    return
                 ENDIF
                 IF(LEFTOV .GT. 0) GOTO 40
              ENDIF
           ENDIF


        ELSE

C--     NOMINAL LOG SECTION - STACK BOTTOM LOGS THEN DEAL WITH
C--     THE TOP ONE OR TWO LOGS

           LEFTOV=LMERCH-(INT(MAXLEN)*(NUMSEG-1))

C--            SET ALL LOGS TO THE NOMINAL LOG LENGTH

           DO 100 I=1,NUMSEG
              LOGLEN(I)=MAXLEN
  100      CONTINUE

           IF (OPT .EQ. 21) THEN
              IF (LEFTOV .GE. (MAXLEN/2.0)) THEN
                 LOGLEN(NUMSEG)=LEFTOV
              ELSE
                 LOGLEN(NUMSEG)=INT((MAXLEN+LEFTOV)/2.0)
                 LOGLEN(NUMSEG-1)=MAXLEN+LEFTOV-LOGLEN(NUMSEG)
C--              IF BOTH SEGMENTS ARE ODD LENGTHS ADD ONE
C--              TO LOWER SEGMENT AND SUBTRACT ONE FROM UPPER

                 IF(LOGLEN(NUMSEG) .EQ. LOGLEN(NUMSEG-1) .AND.
     >              LOGLEN(NUMSEG).GT.INT(LOGLEN(NUMSEG)/2.0)*2.0)THEN
                    
                    LOGLEN(NUMSEG)=LOGLEN(NUMSEG)-1.0
                    LOGLEN(NUMSEG-1)=LOGLEN(NUMSEG-1)+1.0
                 ENDIF
              ENDIF
           ELSEIF (OPT .EQ. 22) THEN
              LOGLEN(NUMSEG)=INT((MAXLEN+LEFTOV)/2.0)
              LOGLEN(NUMSEG-1)=MAXLEN+LEFTOV-LOGLEN(NUMSEG)
C             Check min log len for last log (3/25/14)
              IF (LOGLEN(NUMSEG) .LT. MINLEN) THEN
                   LOGLEN(NUMSEG) = 0
                   LOGLEN(NUMSEG-1) = MAXLEN
C             Reset the NUMSEG (YW 2015/10/08)
                  NUMSEG = NUMSEG - 1
              ELSE                    
C              ENDIF

C--              IF BOTH SEGMENTS ARE ODD LENGTHS ADD ONE
C--              TO LOWER SEGMENT AND SUBTRACT ONE FROM UPPER

                IF(LOGLEN(NUMSEG) .EQ. LOGLEN(NUMSEG-1) .AND.
     >             LOGLEN(NUMSEG).GT.INT(LOGLEN(NUMSEG)/2.0)*2.0) THEN
              
                 LOGLEN(NUMSEG)=LOGLEN(NUMSEG)-1.0
                 LOGLEN(NUMSEG-1)=LOGLEN(NUMSEG-1)+1.0
                ENDIF
              ENDIF
           ELSEIF (OPT .EQ. 23) THEN
              IF(LEFTOV .GE. MINLEN) THEN
                 LOGLEN(NUMSEG)=LEFTOV
              ELSE
                 LOGLEN(NUMSEG)=0
C             Reset the NUMSEG (YW 2015/10/08)
                  NUMSEG = NUMSEG - 1
              ENDIF
           ELSEIF (OPT .EQ. 24) THEN
              IF (LEFTOV .LT. (MAXLEN*.25)) THEN
                 LOGLEN(NUMSEG)=0
C             Reset the NUMSEG (YW 2015/10/08)
                  NUMSEG = NUMSEG - 1
              ELSEIF (LEFTOV .GE. (MAXLEN*.25) .AND.
     >                               LEFTOV .LE. (MAXLEN*.75)) THEN
                 LOGLEN(NUMSEG)=INT((MAXLEN*.5)+.5)
              ELSE
                 LOGLEN(NUMSEG)=MAXLEN
              ENDIF
           ENDIF
        ENDIF
      ENDIF

      RETURN
      END

C--  EVOD - INTEGER - EVEN OR ODD LENGTH SEGMENTS ALLOWED
C--         SEGMENTATION OPTIONS 11-14 ALLOW ODD LENGTHS BY
C--         DEFINITION
C--        1 = ODD SEGMENTS ALLOWED
C--        2 = ONLY EVEN SEGMENTS ALLOWED

C--  LOGLEN - REAL(20) - SEGMENT LENGTHS COMPUTED BY THIS
C--                         PROGRAM DO NOT INCLUDE TRIM

C--  NUMSEG - INTEGER - THE COMPUTED NUMBER OF SEGMENTS

C--  OPT - INTEGER - SPECIFIED SEGMENTATION OPTION
C--        OPTION CODES ARE AS FOLLOWS:
C--        11 = 16 FT LOG SCALE (FSH 2409.11)
C--        12 = 20 FT LOG SCALE (FSH 2409.11)
C--        13 = 32 FT LOG SCALE
C--        14 = 40 FT LOG SCALE
C--        21 = NOMINAL LOG LENGTH (NLL), IF TOP LESS THAN HALF
C--             OF NLL IT IS COMBINED WITH NEXT LOWEST LOG AND
C--             SEGMENTED ACORDING TO RULES FOR NLL MAX LENGTH.
C--             IF SEGMENT IS HALF OR MORE OF NLL THEN SEGMENT
C--             STANDS ON ITS' OWN.
C--        22 = NOMINAL LOG LENGTH, TOP IS PLACED WITH NEXT LOWEST
C--             LOG AND SEGMENTED ACORDING TO RULES FOR NLL MAX
C--             LENGTH.
C--        23 = NOMINAL LOG LENGTH, TOP SEGMENT STANDS ON ITS' OWN.
C--        24 = NOMINAL LOG LENGTH, TOP SEGMENT LESS THAN 1/4 OF
C--             NNL THEN SEGMENT IS DROPED, IF SEGMENT IS
C--             1/4 TO 3/4 THEN SEGMENT IS = 1/2 OF NNL,
C--             IF SEGMENT IS GREATER THAN 3/4 OF NNL THEN
C--             SEGMENT IS = NNL.

C--  LEFTOV - REAL - LEFTOVER OR FRACTIONAL PORTION OF A SEGMENT
C--  LMERCH - REAL - GIVEN MERCHANTABLE LENGTH OF STEM AND
C--                  MERCHANTABLE LENGTH OF STEM WITH TRIM REMOVED
C--  MAXLEN - REAL - MAXIMUM SEGMENT LENGTH
C--  MINLEN - REAL - MINIMUM SEGMENT LENGTH
C--  TRIM - REAL - TRIM LENGTH FOR EACH SEGMENT

