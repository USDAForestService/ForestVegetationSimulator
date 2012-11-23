      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C  **SPCTRN--SEI DATE OF LAST REVISION: 05/05/08
C----------
C     PURPOSE:
C     TAKES TREELIST FILE UPPER CASE SPECIES CODE AS INPUT AND RETURNS
C     AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN THE 1-15
C     RANGE; NON-MATCHES ARE ASSIGNED 14. THIS MESSY FORM IS SIMPLEST
C     GIVEN THE WIDE VARIETY OF INPUT SPECIES CODES.
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'      

      CHARACTER*(*)SPCIN
      INTEGER   ISPC1
      
      SELECT CASE (SPCIN(:2))
	  CASE ("PW")
	    ISPC1 = 1
	  CASE ("LW","L ")
          ISPC1 = 2
        CASE ("FD","F ")
          ISPC1 = 3
        CASE ("BG")
          ISPC1 = 4
	  CASE ("HW","H ","HM")
          ISPC1 = 5
	  CASE ("CW","C ")
          ISPC1 = 6
	  CASE ("PL")
          ISPC1 = 7
	  CASE ("SE","SX","S ","SW","SB","SS")
          ISPC1 = 8
        CASE ("BL","B ")
          ISPC1 = 9
	  CASE ("PY")
          ISPC1 = 10
	  CASE ("EP","E ")
          ISPC1 = 11
	  CASE ("AT")
          ISPC1 = 12
	  CASE ("AC")
          ISPC1 = 13
	  CASE ("OC","Y ","YC","YP","BA","J ","JR","LA","LT","P ")
          ISPC1 = 14
	  CASE ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
          ISPC1 = 14
	  CASE ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
          ISPC1 = 14
	  CASE ("OT")
          ISPC1 = 14
	  CASE ("OH","D ","DR","U ","UP","A ","AX","R ","RA","EA")
          ISPC1 = 15
	  CASE ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
          ISPC1 = 15
	  CASE ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
          ISPC1 = 15
	  CASE ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
          ISPC1 = 15
	  CASE ("MN","MS","OD","OE","OF","OG","QE")
          ISPC1 = 15
        CASE DEFAULT
          ISPC1 = 14
	END SELECT
C ------------------
C These codes can be used if PN,AK,WC variants are developed
C
C     PN VARIANT CUSTOMIZED FOR BC SPECIES CODES
C     THIS IS A DRAFT VERSION... MINOR CODES MAY NEED SOME MORE WORK
C
!      CASE("PN")
!        SELECT CASE (SPCIN(1:2))
!	    CASE ("BA")
!	      ISPC1 = 1
!	    CASE ("BC")
!            ISPC1 = 2
!          CASE ("BG")
!            ISPC1 = 3
!          CASE ("BL")
!            ISPC1 = 4
!	    CASE ("BM")
!            ISPC1 = 5
!	    CASE ("SS")
!            ISPC1 = 6
!	    CASE ("BP")
!            ISPC1 = 7
!	    CASE ("YC","LW","L ")
!            ISPC1 = 8
!          CASE ("OA")
!            ISPC1 = 9
!	    CASE ("SE","SX","S ","SW","SB")
!            ISPC1 = 10
!	    CASE ("PL")
!            ISPC1 = 11
!	    CASE ("JP")
!            ISPC1 = 12
!	    CASE ("PS")
!            ISPC1 = 13
!	    CASE ("PW")
!            ISPC1 = 14
!	    CASE ("PP")
!            ISPC1 = 15
!	    CASE ("FD","F ")
!            ISPC1 = 15
!	    CASE ("OC")
!            ISPC1 = 17
!	    CASE ("CW","C ")
!            ISPC1 = 18
!	    CASE ("HW","H ")
!            ISPC1 = 19
!	    CASE ("HM")
!            ISPC1 = 20
!	    CASE ("MB")
!            ISPC1 = 21
!	    CASE ("DR","D ")
!            ISPC1 = 22
!	    CASE ("RA","R ")
!            ISPC1 = 23
!	    CASE ("EP","E ")
!            ISPC1 = 24
!	    CASE ("GC")
!            ISPC1 = 25
!	    CASE ("AT")
!            ISPC1 = 26
!	    CASE ("AC")
!            ISPC1 = 27
!	    CASE ("QG","Q ")
!            ISPC1 = 28
!	    CASE ("J ")
!            ISPC1 = 29
!	    CASE ("LA")
!            ISPC1 = 30
!	    CASE ("PA","PX")
!            ISPC1 = 31
!	    CASE ("KP")
!            ISPC1 = 32
!	    CASE ("TW","T ")
!            ISPC1 = 33
!	    CASE ("GP","G ")
!            ISPC1 = 34
!	    CASE ("HT")
!            ISPC1 = 35
!	    CASE ("VB","V ")
!            ISPC1 = 36
!	    CASE ("W ","WB","WP","WA","WD","WS","WT")
!            ISPC1 = 37
!	    CASE ("OT")
!            ISPC1 = 39
!
!CC - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...
!
!	    CASE ("Y ","YP","JR","LT","P ")
!            ISPC1 = 39
!	    CASE ("PJ","PF","X ","XC","Z ","ZC")
!            ISPC1 = 39
!	    CASE ("BB","OB","PM","PR","SN")
!            ISPC1 = 39
!	    CASE ("OH","U ","UP","A ","AX","EA")
!            ISPC1 = 39
!	    CASE ("EX","EW","K ","KC","VV","VP")
!            ISPC1 = 39
!	    CASE ("M ","MV")
!            ISPC1 = 39
!	    CASE ("XH","ZH","UA","AD","EE","ES","VS","ME")
!            ISPC1 = 39
!	    CASE ("MN","MS","OD","OE","OF","OG","QE")
!            ISPC1 = 39
!          CASE DEFAULT
!            ISPC1 = 39
!	    END SELECT
!
!      CASE("WC")
!        SELECT CASE (SPCIN(1:2))
!	    CASE ("BA")
!	      ISPC1 = 1
!	    CASE ("BC")
!            ISPC1 = 2
!          CASE ("BG")
!            ISPC1 = 3
!          CASE ("BL")
!            ISPC1 = 4
!	    CASE ("BM")
!            ISPC1 = 5
!	    CASE ("BP")
!            ISPC1 = 7
!	    CASE ("YC","LW","L ")
!            ISPC1 = 8
!          CASE ("OA")
!            ISPC1 = 9
!	    CASE ("SE","SX","S ","SW","SB","SS")
!            ISPC1 = 10
!	    CASE ("PL")
!            ISPC1 = 11
!	    CASE ("JP")
!            ISPC1 = 12
!	    CASE ("PS")
!            ISPC1 = 13
!	    CASE ("PW")
!            ISPC1 = 14
!	    CASE ("PY")
!            ISPC1 = 15
!	    CASE ("FD","F ")
!            ISPC1 = 15
!	    CASE ("OC")
!            ISPC1 = 17
!	    CASE ("CW","C ")
!            ISPC1 = 18
!	    CASE ("HW","H ")
!            ISPC1 = 19
!	    CASE ("HM")
!            ISPC1 = 20
!	    CASE ("MB")
!            ISPC1 = 21
!	    CASE ("DR","D ")
!            ISPC1 = 22
!	    CASE ("RA","R ")
!            ISPC1 = 23
!	    CASE ("EP","E ")
!            ISPC1 = 24
!	    CASE ("GC")
!            ISPC1 = 25
!	    CASE ("AT")
!            ISPC1 = 26
!	    CASE ("AC")
!            ISPC1 = 27
!	    CASE ("QG","Q ")
!            ISPC1 = 28
!	    CASE ("J ")
!            ISPC1 = 29
!	    CASE ("LA")
!            ISPC1 = 30
!	    CASE ("PA","PX")
!            ISPC1 = 31
!	    CASE ("KP")
!            ISPC1 = 32
!	    CASE ("TW","T ")
!            ISPC1 = 33
!	    CASE ("GP","G ")
!            ISPC1 = 34
!	    CASE ("HT")
!            ISPC1 = 35
!	    CASE ("VB","V ")
!            ISPC1 = 36
!	    CASE ("W ","WB","WP","WA","WD","WS","WT")
!            ISPC1 = 37
!	    CASE ("OT")
!            ISPC1 = 39
!
!CC - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...
!
!	    CASE ("Y ","YP","JR","LT","P ")
!            ISPC1 = 39
!	    CASE ("PJ","PF","X ","XC","Z ","ZC")
!            ISPC1 = 39
!	    CASE ("BB","OB","PM","PR","SN")
!            ISPC1 = 39
!	    CASE ("OH","U ","UP","A ","AX","EA")
!            ISPC1 = 39
!	    CASE ("EX","EW","K ","KC","VV","VP")
!            ISPC1 = 39
!	    CASE ("M ","MV")
!            ISPC1 = 39
!	    CASE ("XH","ZH","UA","AD","EE","ES","VS","ME")
!            ISPC1 = 39
!	    CASE ("MN","MS","OD","OE","OF","OG","QE")
!            ISPC1 = 39
!          CASE DEFAULT
!            ISPC1 = 39
!	    END SELECT
!
!      CASE("AK")
!        SELECT CASE (SPCIN(1:2))
!	    CASE ("SE","SX","S ","SW","SB")
!	      ISPC1 = 1
!	    CASE ("CW","C ")
!            ISPC1 = 2
!          CASE ("BA")
!            ISPC1 = 3
!          CASE ("HM")
!            ISPC1 = 4
!	    CASE ("HW","H ")
!            ISPC1 = 5
!	    CASE ("Y ","YC")
!            ISPC1 = 6
!	    CASE ("PL")
!            ISPC1 = 7
!	    CASE ("SS")
!            ISPC1 = 8
!          CASE ("BL","B ")
!            ISPC1 = 9
!	    CASE ("D ","DR")
!            ISPC1 = 10
!	    CASE ("AC")
!            ISPC1 = 11
!	    CASE ("PW","LW","L ","FD","F ","BG","PY")
!            ISPC1 = 13
!	    CASE ("OC","YP","J ","JR","LA","LT","P ")
!            ISPC1 = 13
!	    CASE ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
!            ISPC1 = 13
!	    CASE ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
!            ISPC1 = 13
!	    CASE ("OT")
!            ISPC1 = 13
!	    CASE ("OH","EP","E ","U ","UP","A ","AX","R ","RA","EA")
!            ISPC1 = 12
!	    CASE ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
!            ISPC1 = 12
!	    CASE ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
!            ISPC1 = 12
!	    CASE ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
!            ISPC1 = 12
!	    CASE ("MN","MS","OD","OE","OF","OG","QE","AT")
!            ISPC1 = 12
!          CASE DEFAULT
!            ISPC1 = 13
!	    END SELECT
      RETURN
      END
