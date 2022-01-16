      SUBROUTINE HABTYP (KARD2,ARRAY2)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     TRANSLATES BEC STRINGS INTO VARIOUS USEFUL FORMATS;
C     RETURNS ICHmw2/01 BY DEFAULT.
C
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BCPLOT.F77'
C
COMMONS
C----------
      REAL         ARRAY2
      CHARACTER*20 KARD2

      LOGICAL      LOK
      CHARACTER*3  RGN(3)
      CHARACTER*4  ZN(7)
      CHARACTER*2  SZ(19)
	CHARACTER*20 FULL
	INTEGER      I,J,J1,J2,K
      TYPE         (BEC_DESCRIPTION) TMP

C----------
      DATA RGN /'CAR',
     >          'KAM',
     >          'NEL'/ 

      DATA ZN  /'ESSF',
     >          'ICH',
     >          'IDF', 
     >          'MS',
     >          'PP',
     >          'SBS',
     >          'SBPS'/

      DATA SZ  /'DC',
     >          'DK',
     >          'DM',
     >          'DW',
     >          'MC',
     >          'MH',
     >          'MK',
     >          'MM',
     >          'MW',
     >          'VK',
     >          'WC',
     >          'WK',
     >          'WM',
     >          'WW',
     >          'XC',
     >          'XH',
     >          'XK',
     >          'XM',
     >          'WM' /
C
C     IF THIS ISN'T DONE, THE STRINGS ARE ALL '0' NULL, RATHER THAN '32' SPACE
C
      TMP%Region     = ''
      TMP%Zone       = ''
      TMP%SubZone    = ''
      TMP%Series     = ''
      TMP%FullName   = ''
      TMP%PrettyName = ''
C
C     MERGE FIRST 2 FIELDS AND SHIFT TO UPPERCASE
C
	FULL = ' '
      K=1
      DO J=1,20
	  IF (KARD2(J:J) .NE. ' ') THEN
	    FULL(K:K) = KARD2(J:J)
          CALL UPCASE(FULL(K:K))
	    K = K + 1
	  ENDIF
      ENDDO
C
C     ZERO DEFAULT WITH CURRENT BEC INFORMATION
C     POPULATE THE BEC DATA STRUCTURE
C
      LOK = .FALSE.
      DO I = 1,3
	  J1 = INDEX(FULL,RGN(I))
	  IF (J1 .GT. 0) THEN
	    LOK = .TRUE.
          TMP%Region = RGN(I)
          GOTO 5
	  ENDIF
      ENDDO
    5 IF (.NOT. LOK) GOTO 3

      LOK = .FALSE.
      DO I = 1,7
	  K = LEN_TRIM(ZN(I))
	  J1 = INDEX(FULL,ZN(I)(1:K))
	  IF (J1 .GT. 0) THEN
	    LOK = .TRUE.
          TMP%Zone = ZN(I)
          GOTO 6
	  ENDIF
      ENDDO
    6 IF (.NOT. LOK) GOTO 3

      LOK = .FALSE.
      DO I = 1,11
	  J1 = INDEX(FULL,SZ(I))
	  IF (J1 .GT. 0) THEN
	    LOK = .TRUE.
          TMP%SubZone = SZ(I)
          J2 = INDEX(FULL,'/')
	    IF(J2 .EQ. J1+3) TMP%SubZone(3:3) = FULL(J1+2:J1+2)
	    GOTO 7
	  ENDIF
      ENDDO
    7 IF (.NOT. LOK) GOTO 3
C
C     LONGEST SERIES IS SOMETHING LIKE '1-YC'
C
      LOK = .FALSE.
      J2 = INDEX(FULL,'/')
	IF (J2 .GT. 0) THEN
	  LOK = .TRUE.
	  K = 1
        DO I = J2+1,J2+5
          IF(FULL(I:I) .NE. ' ') THEN
            TMP%Series(K:K) = FULL(I:I)
	      K = K + 1
	    ENDIF
	  ENDDO
	ENDIF
	IF (.NOT. LOK) GOTO 3
C
C     CONVERT SUBZONE AND SERIES TO LOWERCASE
C
	DO I = 1, LEN_TRIM(TMP%SubZone)
        CALL LOCASE(TMP%SubZone(I:I))
	ENDDO
	DO I = 1, LEN_TRIM(TMP%Series)
        CALL LOCASE(TMP%Series(I:I))
	ENDDO
C
C     CREATE A COMBINED BEC IDENTIFIER
C
      TMP%FullName = ''
      K=1
      DO I=1,4
        IF (TMP%Zone(I:I) .NE. ' ') THEN
	    TMP%FullName(K:K) = TMP%Zone(I:I)
          K = K + 1
	  ENDIF
	ENDDO
      DO I=1,3
        IF (TMP%SubZone(I:I) .NE. ' ') THEN
	    TMP%FullName(K:K) = TMP%SubZone(I:I)
          K = K + 1
	  ENDIF
	ENDDO
	TMP%PrettyName = TMP%FullName

      TMP%PrettyName(K:K)   = '/'
      TMP%PrettyName(K+1:K+5) = TMP%Series(1:5)
C
C     COPY THE TEMPORARY DESCRIPTION TO THE GLOBAL VARIABLE
C
	BEC = TMP

	GOTO 2
    3 CALL ERRGRO (.TRUE.,3)
    2 CONTINUE

      RETURN
	END

C--------------------------------------------------------------------
C
C     CONVERT CASE FOR KEYWORD INPUT ARGS: REGION AND ZONE ARE
C     UPPERCASE; SUBZONE IS LOWERCASE
C
      SUBROUTINE LOCASE (C)

      CHARACTER C
      CHARACTER*26 UPPER,LOWER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      IP=INDEX(UPPER,C)
      IF (IP.GT.0) C=LOWER(IP:IP)
      RETURN

      END
