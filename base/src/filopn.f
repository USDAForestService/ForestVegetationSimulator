      SUBROUTINE FILOPN
      IMPLICIT NONE
C--------
C  **FILOPN--BASE   DATE OF LAST REVISION:  01/22/2012
C----------
C
C  THIS ROUTINE OPENS THE FILES FOR THE PROGNOSIS MODEL.
C  TO PROMPT FOR FILE NAMES, SET LPT TRUE,
C  IF PROMPTS ARE NOT WANTED, SET LPT FALSE.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
COMMONS
C
      INTEGER LENKEY,KODE,I,LENNAM,ISTLNB,IRTNCD
      CHARACTER*250 KEYFIL
      CHARACTER*250 CNAME
      CHARACTER VVER*7,REV*10
      LOGICAL LOPEN
      LOGICAL LPT
      DATA LPT/.TRUE./
C----------
C  KEYWORD and OUTPUT FILES.
C----------
      KWDFIL=' '
      call fvsGetKeywordFileName(KWDFIL,len(KWDFIL))
      KWDFIL=ADJUSTL(TRIM(KWDFIL))
      if (KWDFIL.ne.' ') then
        inquire(unit=iread,opened=LOPEN)
        if (LOPEN) close(unit=iread)

        call fvsGetRestartCode (i)
        if (i.eq.0) open(unit=IREAD,file=KWDFIL,status="old",err=101)
        lenkey=index(KWDFIL,".k")
        if (lenkey == 0) lenkey=index(KWDFIL,".K")
        if (lenkey == 0) lenkey=len_trim(KWDFIL)
        cname = KWDFIL(1:lenkey-1)//".out"

        inquire(unit=JOSTND,opened=LOPEN)
        if (LOPEN) close(unit=JOSTND)

        if (i.eq.0) then
          open(unit=JOSTND,file=trim(cname),status="replace",
     -         err=102)
        else
          open(unit=JOSTND,file=trim(cname),status="unknown",
     -         position="append",err=102)
        endif
        CALL KEYFN(KWDFIL)
        CALL DBSVKFN(KWDFIL)

c       open the scratch file (should be removed sometime)
        open(unit=JOTREE,status="scratch",form="unformatted")

        return
  101   continue
        print *,"File open error on: ",trim(KWDFIL)
        call fvsSetRtnCode(1)
        return
  102   continue
        print *,"File open error on: ",trim(cname)
        call fvsSetRtnCode(1)
        return
      endif

      IF (LPT) THEN
C----------
C  GET VARIANT NAME AND REVISION DATE.
C  NOTE: CR VARIANT WILL ALWAYS BE SM (SOUTHWEST MIXED CONIFERS
C  (DEFAULT)) AT THIS POINT BECAUSE KEYWORDS HAVE NOT BEEN READ.
C----------
      CALL VARVER (VVER)
      CALL REVISE (VVER,REV)
        IF(VVER(:2).EQ.'SM') THEN
           WRITE(*,1) REV
 1         FORMAT(/T20,'CR FVS VARIANT -- RV:',A10/)
        ELSE
           WRITE(*,2) VVER(:2),REV
 2         FORMAT(/T20,A2,' FVS VARIANT -- RV:',A10/)
        ENDIF
C
      WRITE (*,'('' ENTER KEYWORD FILE NAME ('',I2.2,
     >        ''): '')') IREAD
C
      ENDIF
C

      READ (*,'(A)',END=100) KWDFIL
      CALL UNBLNK(KWDFIL,LENKEY)
      IF (LENKEY.LE.0) THEN
         WRITE (*,'('' A KEYWORD FILE NAME IS REQUIRED'')')
         CALL RCDSET (3,.FALSE.)
         CALL fvsGetRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN
         RETURN
      ENDIF
      CALL MYOPEN (IREAD,KWDFIL,3,150,0,1,1,0,KODE)
      IF (KODE.GT.0) THEN
         WRITE (*,'('' OPEN FAILED FOR '',A)')
     >        KWDFIL(1:LENKEY)
         WRITE (*,'('' A KEYWORD FILE IS REQUIRED'')')
         CALL RCDSET (3,.FALSE.)
         CALL fvsGetRtnCode(IRTNCD)
         IF (IRTNCD.NE.0) RETURN
         RETURN
      ENDIF
C----------
C     DBS EXTENSION NEEDS THIS FILENAME WITH EXTENSION FOR CASES TABLE
C----------
      CALL DBSVKFN(KWDFIL)
C----------
C  MAIN OUTPUT FILE NEEDS KEYFILE NAME WITH EXTENSION. KEYFN ENTRY
C  IS IN KEYRDR ROUTINE
C----------
      CALL KEYFN(KWDFIL)
C ----------
C  FIND THE LAST PERIOD IN THE FILENAME AND SET THE REST OF THE
C  KEYWORD FILE NAME TO BLANKS
C----------
      DO I= LENKEY, 1, -1
        IF (KWDFIL(I:I) .EQ. '.') THEN
          KEYFIL=KWDFIL
          KWDFIL(I:)=' '
          GO TO 10
        END IF
      END DO
   10 CONTINUE
C----------
C  TREE DATA FILE.
C----------
      IF (LPT) THEN
         WRITE (*,'('' ENTER TREE DATA FILE NAME ('',I2.2,
     >                 ''): '')') ISTDAT
      ENDIF
      READ (*,'(A)',END=100) CNAME
      CALL UNBLNK(CNAME,LENNAM)
      IF (LENNAM.GT.0) THEN
         CALL MYOPEN (ISTDAT,CNAME,1,150,0,1,1,0,KODE)
         IF (KODE.GT.0) WRITE (*,'('' OPEN FAILED FOR '',A)') CNAME
      ENDIF
C----------
C  PRINT FILE.
C----------
      IF (LPT) THEN
         WRITE (*,'('' ENTER MAIN OUTPUT FILE NAME ('',I2.2,
     >                  ''): '')') JOSTND
      ENDIF
      READ (*,'(A)',END=100) CNAME
      CALL UNBLNK(CNAME,LENNAM)
      IF (LENNAM.LE.0) CNAME=KWDFIL(:ISTLNB(KWDFIL))//'.out'
      CALL MYOPEN (JOSTND,CNAME,5,133,0,1,1,1,KODE)
      IF (KODE.GT.0) THEN
         WRITE (*,'('' OPEN FAILED FOR '',A)') CNAME
         WRITE (*,'('' ALL OUTPUT IS SENT TO STANDARD OUT'')')
         JOSTND=6
      ENDIF
C----------
C  TREELIST OUTPUT.
C----------
      IF (LPT) THEN
         WRITE (*,'('' ENTER TREELIST OUTPUT FILE NAME ('',
     >        I2.2,''):  '')') JOLIST
      ENDIF
      READ (*,'(A)',END=100) CNAME
      CALL UNBLNK(CNAME,LENNAM)
      IF (LENNAM.LE.0) CNAME=KWDFIL(:ISTLNB(KWDFIL))//'.trl'
      CALL UNBLNK(CNAME,LENNAM)
      CALL MYOPEN (JOLIST,CNAME,5,133,0,1,1,1,KODE)
      IF (KODE.GT.0) WRITE (*,'('' OPEN FAILED FOR '',A)') CNAME
C----------
C  SUMMARY OUTPUT FILE.
C----------
      IF (LPT) THEN
         WRITE (*,'('' ENTER SUMMARY OUTPUT FILE NAME ('',
     >                  I2.2,''): '')') JOSUM
      ENDIF
      READ (*,'(A)',END=100) CNAME
      CALL UNBLNK(CNAME,LENNAM)
      IF (LENNAM.LE.0) CNAME=KWDFIL(:ISTLNB(KWDFIL))//'.sum'
      CALL UNBLNK(CNAME,LENNAM)
      CALL MYOPEN (JOSUM,CNAME,5,133,0,1,1,0,KODE)
      IF (KODE.GT.0) WRITE (*,'('' OPEN FAILED FOR '',A)') CNAME
C----------
C  AUXILIARY FILE (CHEAPOII) FILE
C----------
      IF (LPT) THEN
         WRITE (*,'('' ENTER CHEAPOII/CALBSTAT '',
     >        ''OUTPUT FILE NAME ('',I2.2,''): '')') JOSUME
      ENDIF
      READ (*,'(A)',END=100) CNAME
      CALL UNBLNK(CNAME,LENNAM)
      IF (LENNAM.LE.0) CNAME=KWDFIL(:ISTLNB(KWDFIL))//'.chp'
      CALL MYOPEN (JOSUME,CNAME,5,91,0,1,1,0,KODE)
      IF (KODE.GT.0) WRITE (*,'('' OPEN FAILED FOR '',A)') CNAME
C----------
C  OPEN THE SAMPLE TREE SCRATCH FILE.
C----------
      CNAME=' '
      CALL MYOPEN (JOTREE,CNAME,4,512, 0,2,1,0,KODE)
      IF (KODE.GT.0) WRITE (*,'('' OPEN FAILED FOR '',I4)') JOTREE
  100 CONTINUE
C
      RETURN

      ENTRY FILClose
      CALL DBSCLOSE(.TRUE.,.TRUE.)
      inquire(unit=iread,opened=LOPEN)
      if (LOPEN) close(unit=iread)
      if (JOSTND.ne.6) then
        inquire(unit=JOSTND,opened=LOPEN)
        if (LOPEN) close(unit=JOSTND)
      endif
      inquire(unit=ISTDAT,opened=LOPEN)
      if (LOPEN) close(unit=ISTDAT)
      inquire(unit=JOTREE,opened=LOPEN)
      if (LOPEN) close(unit=JOTREE)
      inquire(unit=JOSUM,opened=LOPEN)
      if (LOPEN) close(unit=JOSUM)
      inquire(unit=JOLIST,opened=LOPEN)
      if (LOPEN) close(unit=JOLIST)
      inquire(unit=JOSUME,opened=LOPEN)
      if (LOPEN) close(unit=JOSUME)

      return
      end

      SUBROUTINE openIfClosed (ifileref,sufx)
      integer ifileref
      character (len=*) sufx
      character (len=256) keywrdfn
      logical lconn

      INQUIRE(UNIT=ifileref,opened=lconn)
      IF (.NOT.lconn) THEN
        CALL fvsGetKeywordFileName(keywrdfn,len(keywrdfn))
        IF (keywrdfn.NE.' ') THEN
          I = index(keywrdfn,".k")
          IF (I == 0) I=index(keywrdfn,".K")
          IF (I == 0) I=len_trim(keywrdfn)
          keywrdfn=TRIM(keywrdfn(:I-1))//"."//trim(sufx)
          OPEN(UNIT=ifileref,FILE=TRIM(keywrdfn),STATUS='replace')
        ENDIF
      ENDIF


      END





