      SUBROUTINE BGCIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
C----------
C  **BGCIN  BGC--DATE OF LAST REVISION: 05/21/13 AJM. ADDED 10TH ARG TO KEYRDR CALL
C                10/15/99
C                Revised 11/12/02.  Removing index ISTND, and removing PPE
C                                   common "includes" (PPEPRM, PPCNTL, &
C                                   PRGPRM).  AJM
C                These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
C                BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
C                The FVS-BGC code is now, once again, a single stand model.
C----------
C
C     OPTION PROCESSOR FOR THE BGC EXTENSION
C
C     CALLED FROM: INITRE 
C
COMMONS
C
C
      INCLUDE 'BGCCOM.F77'
      INCLUDE 'ENTITY.F77'
      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPCNTL.F77'  ! removed 11/02 ajm
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C     
      PARAMETER (KWCNT = 12)
      CHARACTER*8 TABLE, KEYWRD, PASKEY
      CHARACTER*10 KARD(7)
      LOGICAL LNOTBK,LKECHO
      DIMENSION PRMS(8),KNDX1(5),KNDX2(33)
      DIMENSION ARRAY(7),TABLE(KWCNT),LNOTBK(7)
      CHARACTER*1 ATTRIB1
      CHARACTER*2 ATTRIB2
      CHARACTER*7 ENDFLAG
      DATA TABLE /
     >     'END     ','UNDERVEG','VEGOPT  ','BGCGROW ','        ',
     >     '        ','        ','        ','        ','        ',
     >     '        ','        '/
C
C     **********          EXECUTION BEGINS          **********
C----------
C----------
   10 CONTINUE
      CALL KEYRDR (IREAD,JOSTND,.FALSE.,KEYWRD,LNOTBK,
     >             ARRAY,IRECNT,KODE,KARD,.FALSE.,LKECHO)
      PRINT *,'in bgcin, after Keyrdr, keywrd= ', keywrd 
      PRINT *,'in bgcin, after Keyrdr, Kode= ', kode
C
C     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF
C
      IF (KODE .EQ. 0) GOTO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)
      GOTO 10
   30 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.FALSE.,JOSTND)
      PRINT *,'in bgcin, after FNDKEY, Number= ', number 
      PRINT *,'in bgcin, after FNDKEY, keywrd= ', keywrd 
C
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.
C
      IF (KODE .EQ. 0) GOTO 90     
      IF (KODE .EQ. 1) THEN
         CALL ERRGRO (.TRUE.,1)
         GOTO 10
      ENDIF
      GOTO 90
C
C     SPECIAL END-OF-FILE TARGET
C
   80 CONTINUE
      CALL ERRGRO (.FALSE.,2)
   90 CONTINUE
C
C     SIGNAL THAT THE BGC EXTENSION IS NOW ACTIVE.
C
      PRINT *,'in bgcin, lbgcon=',lbgcon
C      IF (.NOT.LBGCON(ISTND)) THEN ! removed 11/02 ajm
C         LBGCON(ISTND) = .TRUE.    ! ditto
      IF (.NOT.LBGCON) THEN
         LBGCON = .TRUE.
         CALL BGCINITALL
      ENDIF
C
C     PROCESS OPTIONS
C
      GO TO( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     &      1100,1200), NUMBER

  100 CONTINUE
      PRINT *,'in bgcin, option=1, keyword= ', keywrd
C========== OPTION NUMBER 1: END ==========================
      IF(LKECHO)WRITE(JOSTND,110) KEYWRD
  110 FORMAT (/A8,'   END OF BGC OPTIONS.')
      RETURN
  200 CONTINUE
      PRINT *,'in bgcin, option=2, keyword= ', keywrd
C========== OPTION NUMBER 2: UNDERVEG =====================
      IF(LKECHO)WRITE(JOSTND,205) KEYWRD
  205 FORMAT (/A8,'   STAND-BGC UNDERSTORY VEGETATION INPUT')
  206 READ(IREAD,*)ENDFLAG
      DO 210 I=1,7
         CALL UPCASE(ENDFLAG(I:I))
  210 CONTINUE
      IF(ENDFLAG.NE.'ENDENT') THEN
         BACKSPACE IREAD
         READ(IREAD,*) ATTRIB1, ATTRIB2, ATTRIB3, ATTRIB4
         NVE=NVE+1
         VHT(NVE) = ATTRIB3
         VCOV(NVE) = ATTRIB4
         VSP(NVE) = ATTRIB2
         VID(NVE) = ATTRIB1
         GOTO 206
      END IF
      PRINT *,'in bgcin, option=2, end of veg data'
      PRINT *,'in bgcin, option=2, no. of veg entities= ', NVE
      GOTO 10       
  300 CONTINUE
      PRINT *,'in bgcin, option=3, keyword= ', keywrd
C=================OPTION NUMBER 3: VEGOPT =============
      ISOURCE=IFIX(ARRAY(1))
      PRINT *,'in bgcin, option=3, isource= ', ISOURCE 
      IF(LKECHO)WRITE(JOSTND,305) KEYWRD,ISOURCE
  305 FORMAT (/A8,'   SOURCE OF VEG DATA= ', I1)
      GOTO 10
  400 CONTINUE
C=================OPTION NUMBER 4: BGCGROW ============ 
C      IBGC(ISTND)=1  ! removed 11/02 ajm
      IBGC=1
      PRINT *,'in bgcin, option=4'
      IF(LKECHO)WRITE(JOSTND,405) KEYWRD
  405 FORMAT (/A8,'   INCREMENTS WILL BE FROM STAND-BGC')
      GOTO 10
  500 CONTINUE
C=================OPTION NUMBER 5: BGCIN ==============
      GOTO 10
  600 CONTINUE
C                        OPTION NUMBER 6 -- 
      GOTO 10
  700 CONTINUE
C                        OPTION NUMBER 7 --    
      GOTO 10                             
  800 CONTINUE
C                        OPTION NUMBER 8 -- 
      GOTO 10      
  900 CONTINUE
C                        OPTION NUMBER 9 -- 
      GOTO 10
 1000 CONTINUE
C                        OPTION NUMBER 10 -- 
      GOTO 10
 1100 CONTINUE
C                        OPTION NUMBER 11 -- 
      GOTO 10
 1200 CONTINUE
C                        OPTION NUMBER 12 --  
      GOTO 10
C	  
C     Special entry to retrieve keywords.
C
      ENTRY BGCKEY (KEY,PASKEY)
      PASKEY= TABLE(KEY)
      RETURN
      END


