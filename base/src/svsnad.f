      SUBROUTINE SVSNAD(IYEAR,ISNADD,NSNAG,ISWTCH)
      IMPLICIT NONE
C----------
C  **SVSNAD--BASE  DATE OF LAST REVISION: 05/30/08
C----------
C
C     STAND VISUALIZATION GENERATION
C     J.J.MARCHINEK -- RMRS MOSCOW -- MAY 1999
C     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
C     L.R. DAVID    -- FHTET FORT COLLINS -- JULY 2005 
C     S.N.SCHAROSCH -- ABACUS -- APRIL 2008
C
C     USED FOR PROCESSING SNAG ADDING INFORMATION
C
C     ISNADD = VECTOR OF SNAGS THAT NEED TO BE ADDED
C     NSNAG  = NUMBER OF SNAGS THAT NEED TO BE ADDED
C     IYEAR  = CURRENT YEAR
C     IYOFTD = YEAR OF TREE DEATH
C     ISWTCH = 0 IF SVESTB CALLED SVSNAD DIRECTLY
C            = 1 IF SVMORT CALLED SVRMOV, FIRE-CAUSED MORTALITY
C            = 2 IF SVMORT CALLED SVRMOV, NORMAL MORTALITY
C            = 3 IF SVMORT CALLED SVRMOV, WESTWIDE PINE BEETLE MORT               
C            = 4 IF SVCUTS CALLED SVRMOV
C
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'SVDEAD.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
C
C     ISNADD - VECTOR OF SNAGS THAT NEED TO BE ADDED
C     SNASCO - SNAG SCORE
C     ADDSCO - SCORE FOR ISNADD
C     INDEX  - INDEX OF SNAG LIST
C     IBACK  - INDEX SNAGS REMOVED, POINTING BACK TO THE OBJECT LIST
C
      INTEGER ISNADD(MXSVOB)
      REAL ADDSCO(MXSVOB)

      REAL SNASCO(MXDEAD)
      INTEGER INDEX(MXDEAD)
      INTEGER IBACK(MXDEAD)

      INTEGER ISWTCH,NSNAG,IYEAR,ITCYC,IYOFTD,IX,ISVOBJ,ISNAG,I,IXX,
     >        IDEAD,HRATE2,ITIDIF,IIX,IPUT,J,K
      REAL    X,CHPOS,CW,CCFT,XHMOD,Y,FACTOR,TEMP,SNDI,SNHT,SNCRTO,
     >        SNCRDI,XPROB

      LOGICAL DEBUG
      CALL DBCHK (DEBUG,'SVSNAD',6,ICYC)
C
C     SINCE
C
      IF (ICYC.EQ.0) THEN
         ITCYC=1
      ELSE
         ITCYC=ICYC
      ENDIF
C
C     TO DETERMINE IF SVCUTS(1) CALLED IT, OR SVMORTS(0)
C
C     INITIALIZE THE ILYEAR
C
      IF (ICYC.EQ.0 .AND. ISWTCH.EQ.0) ILYEAR=IYEAR

C
C     Initialize the year-of-tree-death variable.
C     This variable was originally designed to be incremented with each
C     new snag record, so that the tree deaths were spread evenly over
C     the current growth cycle.
C     However, that logic conflicts with the FFE snag logic that adds
C     snags in the final year of the growth period. The end result
C     was significantly different snagfall rates being predicted between
C     the FFE and SVS logic. Therefore, the SVS logic is now set to
C     match the FFE logic, with all mortality trees added at the end of
C     the current growth cycle.
C        IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
C              IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
C              OF FORTIETH CYCLE.
C

C>>>  IYOFTD = IYEAR
      IF ( ICYC.EQ.0 .OR. ISWTCH.EQ.1 .OR. ISWTCH.EQ.4 ) THEN
        IYOFTD = IYEAR
      ELSE
        IYOFTD = IY(ITCYC+1) - 1
      ENDIF

C
C     DEBUG
C
      IF (DEBUG) THEN
         IX=0
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
         ENDDO
         WRITE (JOSTND,1010) ICYC, IYEAR, ISWTCH,
     &                       NDEAD, NSVOBJ, IX
 1010    FORMAT (/ ' ','ENTERING SVSNAD, ICYC=',I2,', IYEAR=',I4,
     &                 ', ISWTCH=',I1,':', / ,
     &             ' ',T5,'NDEAD=',I4,', SVS TOTAL OBJECTS=',I6,
     &                 ', SVS SNAG OBJECTS=',I5,':',/)
      ENDIF
C
C     RETURN IF NO OBJECTS TO ADD
C
      IF (NSNAG .EQ. 0) RETURN
C
      IF (DEBUG) THEN
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).EQ.2)
     >         WRITE (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
         ENDDO
         WRITE(JOSTND,1020) NSNAG
 1020    FORMAT(/, '    SNAG RECORDS TO ADD (NSNAG)=',I4)
      ENDIF
C
C     CALCULATE THE SCORES FOR THE SNAGS BEING ADDED.
C
      DO ISNAG=1,NSNAG
         I=ISNADD(ISNAG)
         ADDSCO(I) = DBH(IS2F(I))*DBH(IS2F(I))*HT(IS2F(I))
      ENDDO
C
C     IF THERE IS ENOUGH ROOM, ADD THE SNAGS
C
      IF (MXDEAD - NDEAD .GE. NSNAG) THEN
         IXX=0
         ISNAG=0
         DO IDEAD=1,MXDEAD

C           LOOPS THROUGH THE LIST OF SNAGS TO FIND AN OPEN SPOT

            IF (ISTATUS(IDEAD).EQ.0 .AND. NSNAG.GT.ISNAG) THEN

C              WE FOUND AN OPEN SPOT, AND STILL HAVE SNAGS TO ADD

               ISNSP(IDEAD)=ISP(IS2F(ISNADD(NSNAG-ISNAG)))
               IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -3) THEN

C**               PRINT *, 'IOBJTP IS -3'

                  ISTATUS(IDEAD)=1
                  CALL SVRANN(X)
                  FALLDIR(IDEAD) = IFIX(360. *X +.5)
                  CRNRTO(IDEAD)=99.
                  OLEN(IDEAD)=HT(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                 ICR(IS2F(ISNADD(NSNAG-ISNAG)))*.01
                  SNGLEN(IDEAD)=OLEN(IDEAD)
                  CHPOS=HT(IS2F(ISNADD(NSNAG-ISNAG)))-
     >                 OLEN(IDEAD)

C                 0.0174532778 IS APPROXIMATELY PI/180.

                  XSLOC(ISNADD(NSNAG-ISNAG))=XSLOC(ISNADD(NSNAG-ISNAG))
     >                 +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
                  YSLOC(ISNADD(NSNAG-ISNAG))=YSLOC(ISNADD(NSNAG-ISNAG))
     >                 +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
                  ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                 OLEN(IDEAD)/HT(IS2F(ISNADD(NSNAG-ISNAG)))
                  SNGDIA(IDEAD)=ODIA(IDEAD)
                  CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                  OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

               ELSE
                  OLEN(IDEAD)=HT(IS2F(ISNADD(NSNAG-ISNAG)))
                  SNGLEN(IDEAD)=OLEN(IDEAD)
                  ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNAG-ISNAG)))
                  SNGDIA(IDEAD)=ODIA(IDEAD)
                  CRNRTO(IDEAD)=ICR(IS2F(ISNADD(NSNAG-ISNAG)))
                  CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                  OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

                  IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -2) THEN
                     CALL SVRANN(X)
                     FALLDIR(IDEAD) = IFIX(360. *X +.5)
                  ELSE
                     FALLDIR(IDEAD)=-1
                  ENDIF
                  IF (ISWTCH .EQ. 1) THEN
C                    THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
                     ISTATUS(IDEAD) = 5
                  ELSEIF (ISWTCH .EQ. 3) THEN
C                    THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
                     ISTATUS(IDEAD) = 90
                  ELSE
C                    CALCULATE ISTATUS NORMALLY.
                     IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -1) THEN
                        ISTATUS(IDEAD) = 2
                     ELSE
                        ISTATUS(IDEAD)=1
                     ENDIF
                  ENDIF
               ENDIF
               IYRCOD(IDEAD)=IYOFTD
C
C              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
C              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
C              ORIGINAL INFORMATION
C
               IF (ICYC .EQ. 0) THEN
                  IF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 7) THEN
                     IYRCOD(IDEAD)=IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
                     ISTATUS(IDEAD)=3
                  ELSEIF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 9) THEN
                     IYRCOD(IDEAD)=IYRCOD(IDEAD)-IFIX(FINTM+2)
                     ISTATUS(IDEAD)=4
                  ENDIF
                  CALL SVHABT(XHMOD)
                  IF (ISTATUS(IDEAD) .EQ. 4) THEN
                     HRATE2=2
                     Y=YHFHTS(ISNSP(IDEAD))
                  ELSE
                     HRATE2=1
                     Y=YHFHTH(ISNSP(IDEAD))
                  ENDIF
                  ITIDIF=IYEAR-IYRCOD(IDEAD)
C
                  IF ( DEBUG ) THEN
                     WRITE(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF,
     &                                  ODIA(IDEAD), OLEN(IDEAD)
 1025                FORMAT (/ ' ',T5,'ADDING REC FOR ',
     &                             'INITIAL TREELIST SNAG:',/,
     &                         ' ',T8,'IDEAD=',I4,', IYRCOD=',I4,
     &                             ', ITIDIF=',I2,/,
     &                         ' ',T8,'BEFORE BACKDATING, ODIA=',F4.1,
     &                             ', OLEN=',F5.1)
                  ENDIF
C
                  SNGLEN(IDEAD)=OLEN(IDEAD)
                  IF (ITIDIF .GT. 0) THEN
                     FACTOR=(1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
                     IF (FACTOR**(ITIDIF) .GT. 0.5) THEN
                        OLEN(IDEAD)=OLEN(IDEAD)/(FACTOR**(ITIDIF))
                     ELSE
                        OLEN(IDEAD)=OLEN(IDEAD) /
     &                              (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                     ENDIF
                  ENDIF
C
                  IF ( DEBUG ) THEN
                     WRITE (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
 1026                FORMAT (' ',T8,'AFTER BACKDATING, OLEN=',F5.1,
     &                              ',SNGLEN=',F5.1)
                  ENDIF
                  CRNDIA(IDEAD)=CRNDIA(IDEAD)/(.90**ITIDIF)
C>>>           ELSE
C
C                 The following logic increments IYOFTD (yr of tree death),
C                 by one for each consecutively-added snag record, so that
C                 snag additions are spread equally across the years
C                 comprising the current growth period.
C
C                 Override IYOFTD assignment if SVSNAD is being called for
C                 fire mortality. In this case, assign all snags a year-of-death
C                 equal to the burn year.
C
C>>>              NOTE: logic to spread mortality across years in current growth
C>>>                    period is disabled, in order to match FFE logic.
C>>>
C>>>              IF (IYOFTD+1 .LT. IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
C>>>                 IYOFTD=IYOFTD+1
C>>>              ELSE
C>>>                 IYOFTD=IYEAR
C>>>              ENDIF
C>>>              IF ( ISWTCH .EQ. 1 ) THEN
C>>>                 IYOFTD=IYEAR
C>>>              ENDIF
               ENDIF
               IS2F(ISNADD(NSNAG-ISNAG))=IDEAD
               IOBJTP(ISNADD(NSNAG-ISNAG))=2
               NDEAD=NDEAD+1
               ISNAG=ISNAG+1
            ENDIF
         ENDDO

      ELSE
C        THERE IS NOT ENOUGH ROOM.  ONLY ADD THE LARGEST ONES
         IF (DEBUG) THEN
            IX=0
            DO ISVOBJ=1,NSVOBJ
               IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
            ENDDO
            WRITE (JOSTND,*) 'SVSNAD1NDEAD=', NDEAD,
     >           'DEADOBJECTS=',IX
         ENDIF
C
C        SORT ADDSCO: ORIGINAL CODE USED IAPSRT, WHICH RETURNS INDEX TO AN
C        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
C        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
C        INDICES (ISNADD) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
C        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
C        TO THE CODE. - DR/ESSA
C
         CALL RDPSRT(NSNAG,ADDSCO,ISNADD,.FALSE.)
         IF (NSNAG .GT. 1) THEN
           K = NSNAG
           J = ISNADD(K)
           DO I = 1, (NSNAG/2)
             ISNADD(K) = ISNADD(I)
             ISNADD(I) = J
             K = K-1
             J = ISNADD(K)
           ENDDO
         ENDIF
C
         ISNAG=0
         IF(MXDEAD .GT. NDEAD) THEN
            DO IDEAD=1,MXDEAD
               IF (ISTATUS(IDEAD) .EQ. 0 .AND. NSNAG.GT.ISNAG) THEN
                  ISNSP(IDEAD)=ISP(IS2F(ISNADD(NSNAG-ISNAG)))
                  IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -3) THEN
                     PRINT *, 'IOBJTP IS -3'
                     ISTATUS(IDEAD)=1
                     CALL SVRANN(X)
                     FALLDIR(IDEAD) = IFIX(360. *X +.5)
                     CRNRTO(IDEAD)=99.
                     OLEN(IDEAD)=HT(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                    ICR(IS2F(ISNADD(NSNAG-ISNAG)))*.01
                     SNGLEN(IDEAD)=OLEN(IDEAD)
                     CHPOS=HT(IS2F(ISNADD(NSNAG-ISNAG)))-
     >                    OLEN(IDEAD)
C                    0.0174532778 IS APPROXIMATELY PI/180.
                     XSLOC(ISNADD(NSNAG-ISNAG))=
     >                    XSLOC(ISNADD(NSNAG-ISNAG))
     >                    +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
                     YSLOC(ISNADD(NSNAG-ISNAG))=
     >                    YSLOC(ISNADD(NSNAG-ISNAG))
     >                    +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
                     ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                    OLEN(IDEAD)/HT(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGDIA(IDEAD)=ODIA(IDEAD)
                     CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                     OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

                  ELSE
                     OLEN(IDEAD)=HT(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGLEN(IDEAD)=OLEN(IDEAD)
                     ODIA(IDEAD)=DBH(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGDIA(IDEAD)=ODIA(IDEAD)
                     CRNRTO(IDEAD)=ICR(IS2F(ISNADD(NSNAG-ISNAG)))
                     CRNDIA(IDEAD)=CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                     OIDTRE(IDEAD)=IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

                     IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -2) THEN
                        CALL SVRANN(X)
                        FALLDIR(IDEAD) = IFIX(360. *X +.5)
                     ELSE
                        FALLDIR(IDEAD)=-1
                     ENDIF
                     IF (ISWTCH .EQ. 1) THEN
C                       THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
                        ISTATUS(IDEAD) = 5
                        TEMP = IOBJTP(ISNADD(NSNAG-ISNAG))
                     ELSEIF (ISWTCH .EQ. 3) THEN
C                       THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
                        ISTATUS(IDEAD) = 90
                     ELSE
C                       THIS IS NOT A FIRE MORTALITY, CALCULATE ISTATUS NORMALLY.
                        IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -1) THEN
                           ISTATUS(IDEAD) = 2
                        ELSE
                           ISTATUS(IDEAD)=1
                        ENDIF
                     ENDIF
                  ENDIF
                  IYRCOD(IDEAD)=IYOFTD
C
C              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
C              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
C              ORIGINAL INFORMATION
C
                  IF (ICYC .EQ. 0) THEN
                     IF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 7) THEN
                        IYRCOD(IDEAD)=
     >                       IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
                        ISTATUS(IDEAD)=3
                     ELSEIF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 9) THEN
                        IYRCOD(IDEAD)=
     >                       IYRCOD(IDEAD)-IFIX(FINTM+2)
                        ISTATUS(IDEAD)=4
                     ENDIF
                     CALL SVHABT(XHMOD)
                     IF (ISTATUS(IDEAD) .EQ. 4) THEN
                        HRATE2=2
                        Y=YHFHTS(ISNSP(IDEAD))
                     ELSE
                        HRATE2=1
                        Y=YHFHTH(ISNSP(IDEAD))
                     ENDIF
                     ITIDIF=IYEAR-IYRCOD(IDEAD)
C
                     IF ( DEBUG ) THEN
                        WRITE(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF,
     &                                     ODIA(IDEAD), OLEN(IDEAD)
                     ENDIF
C
                     SNGLEN(IDEAD)=OLEN(IDEAD)
                     IF (ITIDIF .GT. 0) THEN
                        FACTOR=
     >                       (1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
                        IF (FACTOR**(ITIDIF) .GT. 0.5) THEN
                           OLEN(IDEAD) = OLEN(IDEAD)/(FACTOR**(ITIDIF))
                        ELSE
                           OLEN(IDEAD) = OLEN(IDEAD) /
     >                            (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                        ENDIF
                     ENDIF
C
                     IF ( DEBUG ) THEN
                        WRITE (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
                     ENDIF
                     CRNDIA(IDEAD)=CRNDIA(IDEAD)/(.90**ITIDIF)
C>>>              ELSE
C
C                    The following logic increments IYOFTD (yr of tree death),
C                    by one for each consecutively-added snag record, so that
C                    snag additions are spread equally across the years
C                    comprising the current growth period.
C
C                    Override IYOFTD assignment if SVSNAD is being called for
C                    fire mortality. In this case, assign all snags a year-of-death
C                    equal to the burn year.
C
C>>>              NOTE: logic to spread mortality across years in current growth
C>>>                    period is disabled, in order to match FFE logic.
C>>>
C>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1).AND. ISWTCH.NE.4) THEN
C>>>                    IYOFTD=IYOFTD+1
C>>>                 ELSE
C>>>                    IYOFTD=IYEAR
C>>>                 ENDIF
C>>>                 IF ( ISWTCH .EQ. 1 ) THEN
C>>>                    IYOFTD=IYEAR
C>>>                 ENDIF
                  ENDIF
                  IS2F(ISNADD(NSNAG-ISNAG))=IDEAD
                  IOBJTP(ISNADD(NSNAG-ISNAG))=2
                  NDEAD=NDEAD+1
                  ISNAG=ISNAG+1
               ENDIF
            ENDDO
         ENDIF
C
C        CREATE INDEX AND THE SCORE VECTOR FOR THE SNAG LIST
C
         IX=0
         DO IDEAD=1,MXDEAD
            IF (ISTATUS(IDEAD).GT.0) THEN

C
C              AGES THE SNAG LIST
C
               CALL SVSNAGE(IYEAR,IDEAD,SNCRDI,SNCRTO,SNHT,SNDI)
               IX=IX+1
               INDEX(IX)=IDEAD
               SNASCO(IDEAD)=SNDI*SNDI*SNHT
            ENDIF
         ENDDO
C
C        MODIFY ILYEAR TO SHOW THAT THESE SNAGS HAVE BEEN AGED.
C
         ILYEAR = IYEAR
C
C        SORT SNASCO: ORIGINAL CODE USE IAPSRT, WHICH RETURNS INDEX TO AN
C        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
C        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
C        INDICES (INDEX) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
C        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
C        TO THE CODE. - DR/ESSA
C
         CALL RDPSRT(NDEAD,SNASCO,INDEX,.FALSE.)
         IF (NDEAD .GT. 1) THEN
           K = NDEAD
           J = INDEX(K)
           DO I = 1, (NDEAD/2)
             INDEX(K) = INDEX(I)
             INDEX(I) = J
             K = K-1
             J = INDEX(K)
           ENDDO
         ENDIF
C
         IF (DEBUG) THEN
            IX=0
            DO ISVOBJ=1,NSVOBJ
               IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
            ENDDO
            WRITE (JOSTND,*) 'BETWEENNDEAD=', NDEAD,
     >           'DEADOBJECTS=',IX
         ENDIF

         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ) .EQ. 2) THEN
               IBACK(IS2F(ISVOBJ))=ISVOBJ
            ENDIF
         ENDDO

         DO IDEAD=1,MXDEAD
            IF (NSNAG.GT.ISNAG .AND. INDEX(IDEAD).NE.0) THEN
               IF (ADDSCO(ISNADD(NSNAG-ISNAG)) .GT.
     >              SNASCO(INDEX(IDEAD))) THEN

                  IF (DEBUG) WRITE (JOSTND,*) IX,NDEAD,
     >                 'ISNADD(',NSNAG-ISNAG,')=',
     >                 ISNADD(NSNAG-ISNAG),'INDEX(',IDEAD,
     >                 ')=', INDEX(IDEAD),
     >                 'IS2F(',ISNADD(NSNAG-ISNAG),')=',
     >                 IS2F(ISNADD(NSNAG-ISNAG)),
     >                 'ADDSCO=',ADDSCO(ISNADD(NSNAG-ISNAG)),
     >                 'SNASCO=',SNASCO(INDEX(IDEAD))

                  ISNSP(INDEX(IDEAD))=ISP(IS2F(ISNADD(NSNAG-ISNAG)))
                  IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -3) THEN

C**                     PRINT *, 'IOBJTP IS -3'

                     IF (ISTATUS(INDEX(IDEAD)).EQ.0) THEN
                        NDEAD=NDEAD+1
                        IF (DEBUG) WRITE (JOSTND,*) 'ADDEDTO', NDEAD
                     ENDIF
                     ISTATUS(INDEX(IDEAD))=1
                     CALL SVRANN(X)
                     FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
                     CRNRTO(INDEX(IDEAD))=99.
                     OLEN(INDEX(IDEAD))=HT(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                    ICR(IS2F(ISNADD(NSNAG-ISNAG)))*.01
                     SNGLEN(INDEX(IDEAD))=OLEN(INDEX(IDEAD))
                     CHPOS=HT(IS2F(ISNADD(NSNAG-ISNAG)))-
     >                    OLEN(INDEX(IDEAD))

C                    0.0174532778 IS APPROXIMATELY PI/180.

                     XSLOC(ISNADD(NSNAG-ISNAG))=
     >                    XSLOC(ISNADD(NSNAG-ISNAG))
     >                    +CHPOS*SIN(FALLDIR(INDEX(IDEAD))*0.0174532778)
                     YSLOC(ISNADD(NSNAG-ISNAG))=
     >                    YSLOC(ISNADD(NSNAG-ISNAG))
     >                    +CHPOS*COS(FALLDIR(INDEX(IDEAD))*0.0174532778)
                     ODIA(INDEX(IDEAD))=
     >                    DBH(IS2F(ISNADD(NSNAG-ISNAG)))*
     >                    OLEN(INDEX(IDEAD))
     >                    /HT(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGDIA(INDEX(IDEAD))=ODIA(INDEX(IDEAD))
                     CRNDIA(INDEX(IDEAD))=
     >                    CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                     OIDTRE(INDEX(IDEAD))=
     >                    IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

                  ELSE
                     OLEN(INDEX(IDEAD))=HT(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGLEN(INDEX(IDEAD))=OLEN(INDEX(IDEAD))
                     ODIA(INDEX(IDEAD))=DBH(IS2F(ISNADD(NSNAG-ISNAG)))
                     SNGDIA(INDEX(IDEAD))=ODIA(INDEX(IDEAD))
                     CRNRTO(INDEX(IDEAD))=ICR(IS2F(ISNADD(NSNAG-ISNAG)))

                     CRNDIA(INDEX(IDEAD))=
     >                    CRWDTH(IS2F(ISNADD(NSNAG-ISNAG)))

                     OIDTRE(INDEX(IDEAD))=
     >                    IDTREE(IS2F(ISNADD(NSNAG-ISNAG)))

                     IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -2) THEN
                        CALL SVRANN(X)
                        FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
                     ELSE
                        FALLDIR(INDEX(IDEAD))=-1
                     ENDIF
                     IF (ISTATUS(INDEX(IDEAD)).EQ.0) THEN
                        NDEAD=NDEAD+1
                        IF (DEBUG) WRITE (JOSTND,*) 'ADDEDTO', NDEAD
                     ENDIF
                     IF (IOBJTP(ISNADD(NSNAG-ISNAG)) .EQ. -1) THEN
                        ISTATUS(INDEX(IDEAD)) = 2
                     ELSE
                        ISTATUS(INDEX(IDEAD))=1
                     ENDIF
                     IIX=0
                     IF (DEBUG) THEN
                        DO ISVOBJ=1,NSVOBJ
                           IF (IOBJTP(ISVOBJ).EQ.2) IIX=IIX+1
                        ENDDO
                        IF (DEBUG) WRITE (JOSTND,*) 'DEADOBJECTS=',IIX
                     ENDIF
                  ENDIF
                  IS2F(ISNADD(NSNAG-ISNAG))=INDEX(IDEAD)
                  IOBJTP(ISNADD(NSNAG-ISNAG))=2
                  IF (IBACK(INDEX(IDEAD)).GE.1 .AND.
     >                IBACK(INDEX(IDEAD)).LE.MXSVOB)
     >                  IOBJTP(IBACK(INDEX(IDEAD)))=0
                  ISNAG=ISNAG+1
                  IYRCOD(INDEX(IDEAD))=IYOFTD
C
C              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
C              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
C              ORIGINAL INFORMATION
C
                  IF (ICYC .EQ. 0) THEN
                     IF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 7) THEN
                        IYRCOD(INDEX(IDEAD))=
     >                       IYRCOD(INDEX(IDEAD))-MAX(IFIX(FINTM*.7),1)
                        ISTATUS(INDEX(IDEAD))=3
                     ELSEIF (IMC(IS2F(ISNADD(NSNAG-ISNAG))) .EQ. 9) THEN
                        IYRCOD(INDEX(IDEAD))=
     >                       IYRCOD(INDEX(IDEAD))-IFIX(FINTM+2)
                        ISTATUS(INDEX(IDEAD))=4
                     ENDIF
                     CALL SVHABT(XHMOD)
                     IF (ISTATUS(INDEX(IDEAD)) .EQ. 4) THEN
                        HRATE2=2
                        Y=YHFHTS(ISNSP(INDEX(IDEAD)))
                     ELSE
                        HRATE2=1
                        Y=YHFHTH(ISNSP(INDEX(IDEAD)))
                     ENDIF
                     ITIDIF=IYEAR-IYRCOD(INDEX(IDEAD))
C
                     IF ( DEBUG ) THEN
                        WRITE(JOSTND,1025) IDEAD, IYRCOD(INDEX(IDEAD)),
     &                                     ITIDIF, ODIA(INDEX(IDEAD)),
     &                                     OLEN(INDEX(IDEAD))
                     ENDIF
C
                     SNGLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))
                     IF (ITIDIF .GT. 0) THEN
                        FACTOR=(1-0.0228*XHMOD*
     >                       HRATE(ISNSP(INDEX(IDEAD)))*HRATE2)
                        IF (FACTOR**ITIDIF .GT. 0.5) THEN
                           OLEN(INDEX(IDEAD)) =
     >                          OLEN(INDEX(IDEAD))/(FACTOR**(ITIDIF))
                        ELSE
                           OLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))/
     >                          (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                        ENDIF
                     ENDIF
                     IF ( DEBUG ) THEN
                        WRITE (JOSTND,1026) OLEN(INDEX(IDEAD)),
     &                                      SNGLEN(INDEX(IDEAD))
                     ENDIF
                     CRNDIA(INDEX(IDEAD))=
     >                    CRNDIA(INDEX(IDEAD))/(.90**ITIDIF)
C>>>              ELSE
C
C                    The following logic increments IYOFTD (yr of tree death),
C                    by one for each consecutively-added snag record, so that
C                    snag additions are spread equally across the years
C                    comprising the current growth period.
C
C                    Override IYOFTD assignment if SVSNAD is being called for
C                    fire mortality. In this case, assign all snags a year-of-death
C                    equal to the burn year.
C
C>>>              NOTE: logic to spread mortality across years in current growth
C>>>                    period is disabled, in order to match FFE logic.
C>>>
C>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
C>>>                    IYOFTD=IYOFTD+1
C>>>                 ELSE
C>>>                    IYOFTD=IYEAR
C>>>                 ENDIF
C>>>                 IF ( ISWTCH .EQ. 1 ) THEN
C>>>                    IYOFTD=IYEAR
C>>>                 ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
C
C
      IF (DEBUG) THEN
         IX=0
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
         ENDDO
         WRITE (JOSTND,1030) NDEAD, NSVOBJ, IX
 1030    FORMAT (/ ' ',T5,'AFTER SNAG ADDITIONS:', / ,
     &             ' ',T5,'   NDEAD=',I4,', SVS TOTAL OBJECTS=',I6,
     &                 ', SVS SNAG OBJECTS=',I5,':',/)
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).EQ.2)
     >         WRITE (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
         ENDDO
      ENDIF
C
C     CLEAN UP OBJECTS IF NOT CALLED BY FIRE MODEL.
C
      IF ( ISWTCH.NE.1 ) THEN
         IPUT=0
         DO ISVOBJ=1,NSVOBJ
            IF (IOBJTP(ISVOBJ).LE.0 .OR. IS2F(ISVOBJ).EQ.0) THEN
               IF (IPUT.EQ.0) IPUT=ISVOBJ
            ELSE
               IF (IPUT.GT.0 .AND. IPUT.LT.ISVOBJ) THEN
                  IS2F(IPUT)=IS2F(ISVOBJ)
                  XSLOC(IPUT)=XSLOC(ISVOBJ)
                  YSLOC(IPUT)=YSLOC(ISVOBJ)
                  IOBJTP(IPUT)=IOBJTP(ISVOBJ)
                  IPUT=IPUT+1
               ENDIF
            ENDIF
         ENDDO
         IF (IPUT.GT.0) NSVOBJ=IPUT-1

         IF (DEBUG) THEN
            IX=0
            DO ISVOBJ=1,NSVOBJ
               IF (IOBJTP(ISVOBJ).EQ.2) IX=IX+1
            ENDDO
            WRITE (JOSTND,*) 'SVSNADOUTNDEAD=', NDEAD,
     >           'DEADOBJECTS=',IX

            DO ISVOBJ=1,NSVOBJ
               IF (IOBJTP(ISVOBJ).EQ.2)
     >              WRITE (JOSTND,*) 'IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
            ENDDO
         ENDIF
      ENDIF
C
C     Compute, by source tree record and year of death:
C        1) Initial snag records created.
C        2) Snag records still standing.
C

      IF (ICYC .EQ. 0) THEN
        DO IDEAD=1,NDEAD
          IF ( IYRCOD(IDEAD) .LE. IYEAR ) THEN
            XPROB = 0.0
            DO J=1,NDEAD
              IF ( OIDTRE(J) .EQ. OIDTRE(IDEAD) .AND.
     &             IYRCOD(J) .EQ. IYRCOD(IDEAD) ) XPROB=XPROB+1
            ENDDO
            SPROBS(IDEAD,1) = XPROB
            SPROBS(IDEAD,3) = 1.0
          ENDIF
        ENDDO
      ELSE
        DO IDEAD=1,NDEAD
          IF ( IYRCOD(IDEAD) .GE. IYEAR ) THEN
            XPROB = 0.0
            DO J=1,NDEAD
              IF ( OIDTRE(J) .EQ. OIDTRE(IDEAD) .AND.
     &             IYRCOD(J) .EQ. IYRCOD(IDEAD) ) XPROB=XPROB+1
            ENDDO
            SPROBS(IDEAD,1) = XPROB
            SPROBS(IDEAD,3) = 1.0
          ENDIF
        ENDDO
      ENDIF

      DO IDEAD=1,NDEAD
        XPROB = 0.0
        DO J=1,NDEAD
          IF ( OIDTRE(J)  .EQ. OIDTRE(IDEAD) .AND.
     &         IYRCOD(J)  .EQ. IYRCOD(IDEAD) .AND.
     &         FALLDIR(J) .EQ. -1 ) XPROB=XPROB+1
        ENDDO
        SPROBS(IDEAD,2) = XPROB
      ENDDO

      IF (DEBUG) THEN
        WRITE (JOSTND,1040) ICYC, IYEAR,
     &                      NSNAG, NDEAD, NSVOBJ
 1040   FORMAT (' ','LEAVING SVSNAD, ICYC=',I2,
     &              ', IYEAR=',I4,':', / ,
     &          ' ',T5,'NSNAG(add)=',I4,', NDEAD=',I4,', NSVOBJ=',I5,//,
     &          ' ',T5,'SNAG LIST AFTER ADDING CUT/MORTALITY:',//,
     &          ' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS '
     &                 'FALLDIR OPROB STDNG',/,
     &          ' ',T5,'---- -------- --- ---- ---- ------ ------ '
     &                 '------- ----- -----' )
C                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
C                       XXXXXX. XXXX. XXXX.
        DO 300 I=1,NDEAD
          WRITE(JOSTND,1050) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),
     &                       IYRCOD(I), ISTATUS(I),
     &                       FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
 1050     FORMAT(' ',T5,I4,1X,I8,1X,I3,1X,F4.1,1X,F4.0,1X,
     &               I6,1X,I6,1X,F7.0,1X,F5.0,1X,F5.0)
  300   CONTINUE
      ENDIF
C
      RETURN
      END
