      PROGRAM CHECKVOLEQNOS
      IMPLICIT NONE
C
C		THis program parses the .out files and writes volume equation number table
C   for each variant including all location codes for each variant. The file
C   extensin is .dat. THese files may be compared with the .txt files developed
C   from the varinat overviews. Because the file .dat and .txt variant tables
C   are the same verification of the NVEL taboes is facilitated. The .dat files
C   need minor editing. Remove the trailing comma on each record in Ultra Edit.
C   Also remove the carrige returns from each record in the CR.dat file.
C   Open all .DAT filesin Ultra Edit and run a search and replace on all files.
C   THe serach string is ",^P" and the replace string is "^P" without the quotes.
C   Use the following commands to compile and assemble the checkVolEqNos program
C   with the gfortran compiler.
C
C   Compile the routine with.
C   gfortran -c checkVolEqNos.f
C
C   Assenmble the program with.
C   gfortran -o checkVolEqNos.exe checkVolEqNos.o
C
      INTEGER I,J,DEBUG,NUMFCODES,NUMRECORDS,MAXSPECIES
      CHARACTER (LEN=200) BUFFER,FVSoutput,volumeTable
      CHARACTER(LEN=120) LINE,CTEMP
      CHARACTER(LEN=13)FCODE(40)
      CHARACTER(LEN=2)SPECIES(40,3000)
      CHARACTER(LEN=10)VOLEQNO(40,3000)
C
      DEBUG=0

      CALL GETARG(1,BUFFER)
      READ(BUFFER,*) FVSoutput
      IF(DEBUG.EQ.1)WRITE(*,*)' DEBUG= ',DEBUG
      IF(DEBUG.EQ.1)WRITE(*,*)' STARTING RUN **************************'
      IF(DEBUG.EQ.1)WRITE(*,*)' FVSoutput= ',FVSoutput
      volumeTable	=TRIM(ADJUSTL(FVSoutput))
      volumeTable=FVSoutput(1:2)//'.dat'
      IF(DEBUG.EQ.1)WRITE(*,*)' volumeTable= ',volumeTable

C
      OPEN(10,FILE=FVSoutput,STATUS="UNKNOWN")
      OPEN(20,FILE=volumeTable)
      IF(DEBUG.EQ.1)WRITE(*,*)' **** After OPEN statements'

C   10 CONTINUE

      NUMFCODES=0
C
C  READ VARIABLES FROM 'OPTIONS SELECTED BY default' SECTION
C

C  THIS IS THE MAIN SEARCH LOOP
      DO
      READ(10,100,END=900)LINE
      CTEMP=TRIM(ADJUSTL(LINE))
      NUMRECORDS=0
C      IF(DEBUG.EQ.1)WRITE(*,*)CTEMP
C
C FIND DEFAULT OPTIONS KEYWORD ECHO SEQUENCE
      IF(CTEMP.EQ.'OPTIONS SELECTED BY DEFAULT')THEN
C
      IF(DEBUG.EQ.1)WRITE(*,*)'+++++++++++++++++++', CTEMP
C
C Find STDINFO keyword and read forst code from STDINFO keyword
C
        DO
        READ(10,100,END=900)LINE
        CTEMP=TRIM(ADJUSTL(LINE))
        IF((CTEMP(:7).EQ.'STDINFO'))THEN
        	NUMFCODES=NUMFCODES+1
          FCODE(NUMFCODES)=CTEMP(28:40)
          IF(DEBUG.EQ.1)WRITE(*,*)'++++++++found forest Code',
     &    NUMFCODES,FCODE(NUMFCODES)
          EXIT
        ENDIF
        ENDDO
C
C  Find NVEL table
C
        DO
        READ(10,100,END=900)LINE
        CTEMP=TRIM(ADJUSTL(LINE))

c        if(debug.eq.1)write(*,*)CTEMP(35:59)

        IF((CTEMP(:25).EQ.'NATIONAL VOLUME ESTIMATOR'))THEN
          IF(DEBUG.EQ.1)WRITE(*,*)'++++++++found NVEL table'
          EXIT
        ENDIF
        ENDDO
C
C  read 2 lines
C
        DO I=1,2
        READ(10,100,END=900)LINE
        ENDDO
C
C   REad the NVEL table into arrays, read species and vol eq nos
C
        DO I=1,150,4
        	IF(DEBUG.EQ.1)WRITE(*,*)'************ IN LOOP TO READ TABLE'
        	NUMRECORDS=NUMRECORDS+1

        	IF(DEBUG.EQ.1)WRITE(*,*)'************NUMRECORDS= ',NUMRECORDS
        	IF(DEBUG.EQ.1)WRITE(*,*)'************NUMFCODES= ',NUMFCODES

        	READ(10,1000) SPECIES(NUMFCODES,I),VOLEQNO(NUMFCODES,I),
     &                  SPECIES(NUMFCODES,I+1),VOLEQNO(NUMFCODES,I+1),
     &                  SPECIES(NUMFCODES,I+2),VOLEQNO(NUMFCODES,I+2),
     &                  SPECIES(NUMFCODES,I+3),VOLEQNO(NUMFCODES,I+3)

          IF(DEBUG.EQ.1)WRITE(*,*)'************ AFTER READ STATEMENTS'

        	IF(DEBUG.EQ.1)WRITE(*,*)'********* I,SPECIES(NUMFCODES,I)= ',
     &I,SPECIES(NUMFCODES,I)

        IF(SPECIES(NUMFCODES,I).EQ.'  ')THEN
          NUMRECORDS=NUMRECORDS-1
          MAXSPECIES=I-1
        EXIT
        ELSEIF(SPECIES(NUMFCODES,I+1).EQ.'  ')THEN
          NUMRECORDS=NUMRECORDS-1
          MAXSPECIES=I+1-1
        EXIT
        ELSEIF(SPECIES(NUMFCODES,I+2).EQ.'  ')THEN
          NUMRECORDS=NUMRECORDS-1
          MAXSPECIES=I+2-1
        EXIT
        ELSEIF(SPECIES(NUMFCODES,I+3).EQ.'  ')THEN
          NUMRECORDS=NUMRECORDS-1
          MAXSPECIES=I+3-1
        EXIT
        ENDIF
        ENDDO
 1000 FORMAT(4(2X,A2,4X,A10,12X))
        	IF(DEBUG.EQ.1)WRITE(*,*)'********* MAXSPECIES= ',MAXSPECIES

C      EXIT
      ENDIF
      ENDDO                                 ! main DO LOOP

 900  CONTINUE
C
C  write output table
C
      WRITE(20,1200)(FCODE(I),I=1,NUMFCODES)
      DO J=1,MAXSPECIES
      WRITE(20,1100)(SPECIES(I,J),VOLEQNO(I,J),I=1,NUMFCODES)
      ENDDO

 1100 FORMAT(20(A2,',',A10','))
 1200 FORMAT(20(A13,','))
 100  FORMAT(A120)
C
      WRITE(*,9000)TRIM(ADJUSTL(VolumeTable))
 9000 FORMAT(//,T20,'*** Processing Has Completed ***'/,
     &          T20,' Output Written to: ',A8)
C
      STOP
      END
