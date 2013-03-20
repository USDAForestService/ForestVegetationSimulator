      SUBROUTINE GENRPT
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     THIS ROUTINE HANDLES MULTIPLE PRINT REPORTS.  THAT IS
C     YOU CAN WRITE SEVERAL REPORTS TO THE FILE OWNED BY THIS
C     ROUTINE AND ALL THE REPORTS WILL BE WRITTEN TO THE FVS OUTPUT
C     IN SEQUENCE.
C
      INCLUDE "PRGPRM.F77"
      INCLUDE "CONTRL.F77"
C      
      COMMON /GENRCM/ JOSCRT,NRPTS,IFOPN

#ifdef _WINDLL
!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'GENRPT'::GENRPT
!DEC$ ATTRIBUTES REFERENCE :: JROUT,IFID
#endif

      INTEGER IFOPN,NRPTS,JOSCRT,IFID,KODE,JROUT,ID,IR
      
      CHARACTER*140 RECORD

C     INITIALIZE THIS ROUTINE.

      JOSCRT = 93
      NRPTS  = 0
      IFOPN  = 0
      RETURN

      ENTRY GETID (IFID)

C     RETURN A REPORT ID.  IF THE SCRATCH FILE IS NOT OPENED, IT
C     OPENS IT.

      IF (IFOPN.EQ.0) THEN
         RECORD=' '
         CALL MYOPEN(JOSCRT,TRIM(KWDFIL)//'_genrpt.txt',5,
     -        140,0,1,1,0,KODE)
         IF (KODE.EQ.1) THEN
            CALL ERRGRO (.TRUE.,26)
            IFID=-1
            RETURN
         ELSE
            IFOPN = 1
         ENDIF
      ENDIF

      NRPTS=NRPTS+1
      IFID=NRPTS
      RETURN

      ENTRY GETLUN (JROUT)

      IF (IFOPN.EQ.0) THEN
         CALL MYOPEN(JOSCRT,TRIM(KWDFIL)//'_genrpt.txt',5,
     -        140,0,1,1,0,KODE)
         IF (KODE.EQ.1) THEN
            CALL ERRGRO (.TRUE.,26)
            JROUT=-1
            RETURN
         ELSE
            IFOPN = 1
         ENDIF
      ENDIF

      JROUT=JOSCRT
      RETURN

      ENTRY GENPRT

      IF (IFOPN.EQ.0 .OR. NRPTS.EQ.0) RETURN

      ENDFILE JOSCRT

C     USE ALTERNATIVE PROCESSING IF THERE ARE MANY REPORTS

      DO 100 ID=1,NRPTS

         REWIND JOSCRT

   10    CONTINUE

         READ (JOSCRT,'(1X,I5,1X,A)',END=100) IR,RECORD

   15    FORMAT(' GENPRT: IR ID RECORD:',2I5,A)
C
         IF (RECORD(1:4).EQ.'$#*%' .AND. RECORD(5:).EQ.' ') THEN
   20       CONTINUE
               READ (JOSCRT,'(A)',END=100) RECORD
               IF (RECORD(1:4).EQ.'$#*%'.AND.RECORD(5:).EQ.' ')GOTO 10
               IF (IR.EQ.ID) WRITE (JOSTND,25) TRIM(RECORD)
   25          FORMAT (1X,A)
               GOTO 20
            ELSE
               IF (IR.EQ.ID) WRITE (JOSTND,25) TRIM(RECORD)
            ENDIF
            GOTO 10
  100 CONTINUE

      CLOSE (unit=JOSCRT,status='delete')
      IFOPN = 0
      NRPTS = 0

      RETURN
      
      ENTRY GETNRPTS(IFID)
      IFID=NRPTS
      RETURN
      
      ENTRY SETNRPTS(IFID)
      NRPTS=IFID
      RETURN
      
      END
