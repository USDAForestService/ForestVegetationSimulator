      SUBROUTINE BCSPP(VARNAME, SPKODE, INDX)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     PURPOSE: TAKES TREELIST FILE SPECIES CODE AS INPUT, AND RETURNS
C              AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN
C              THE 1-10 RANGE; NON-MATCHES ARE ASSIGNED 11. THIS MESSY
C              FORM IS SIMPLEST GIVEN THE WIDE VARIETY OF INPUT SPECIES
C              CODES
C
      CHARACTER*7 VARNAME
      CHARACTER*4 SPKODE
      INTEGER     I,INDX

      DO I=1,2
        CALL UPCASE(SPKODE(I:I))
      ENDDO

      SELECT CASE (VARNAME(1:2))

	CASE("IB ")
        SELECT CASE (SPKODE(1:2))
	    CASE ("PW")
	      INDX = 1
	    CASE ("LW","L ")
            INDX = 2
          CASE ("FD","F ")
            INDX = 3
          CASE ("BG")
            INDX = 4
	    CASE ("HW","H ","HM")
            INDX = 5
	    CASE ("CW","C ")
            INDX = 6
	    CASE ("PL")
            INDX = 7
	    CASE ("SE","SX","S ","SW","SB","SS")
            INDX = 8
          CASE ("BL","B ")
            INDX = 9
	    CASE ("PY")
            INDX = 10
	    CASE ("EP","E ")
            INDX = 11
	    CASE ("AT")
            INDX = 12
	    CASE ("AC")
            INDX = 13
	    CASE ("OC","Y ","YC","YP","BA","J ","JR","LA","LT","P ")
            INDX = 14
	    CASE ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
            INDX = 14
	    CASE ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
            INDX = 14
	    CASE ("OT")
            INDX = 14
	    CASE ("OH","D ","DR","U ","UP","A ","AX","R ","RA","EA")
            INDX = 15
	    CASE ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
            INDX = 15
	    CASE ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
            INDX = 15
	    CASE ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
            INDX = 15
	    CASE ("MN","MS","OD","OE","OF","OG","QE")
            INDX = 15
          CASE DEFAULT
            INDX = 14
	    END SELECT
C
C     PN VARIANT CUSTOMIZED FOR BC SPECIES CODES
C     THIS IS A DRAFT VERSION... MINOR CODES MAY NEED SOME MORE WORK
C
      CASE("PN")
        SELECT CASE (SPKODE(1:2))
	    CASE ("BA")
	      INDX = 1
	    CASE ("BC")
            INDX = 2
          CASE ("BG")
            INDX = 3
          CASE ("BL")
            INDX = 4
	    CASE ("BM")
            INDX = 5
	    CASE ("SS")
            INDX = 6
	    CASE ("BP")
            INDX = 7
	    CASE ("YC","LW","L ")
            INDX = 8
          CASE ("OA")
            INDX = 9
	    CASE ("SE","SX","S ","SW","SB")
            INDX = 10
	    CASE ("PL")
            INDX = 11
	    CASE ("JP")
            INDX = 12
	    CASE ("PS")
            INDX = 13
	    CASE ("PW")
            INDX = 14
	    CASE ("PP")
            INDX = 15
	    CASE ("FD","F ")
            INDX = 15
	    CASE ("OC")
            INDX = 17
	    CASE ("CW","C ")
            INDX = 18
	    CASE ("HW","H ")
            INDX = 19
	    CASE ("HM")
            INDX = 20
	    CASE ("MB")
            INDX = 21
	    CASE ("DR","D ")
            INDX = 22
	    CASE ("RA","R ")
            INDX = 23
	    CASE ("EP","E ")
            INDX = 24
	    CASE ("GC")
            INDX = 25
	    CASE ("AT")
            INDX = 26
	    CASE ("AC")
            INDX = 27
	    CASE ("QG","Q ")
            INDX = 28
	    CASE ("J ")
            INDX = 29
	    CASE ("LA")
            INDX = 30
	    CASE ("PA","PX")
            INDX = 31
	    CASE ("KP")
            INDX = 32
	    CASE ("TW","T ")
            INDX = 33
	    CASE ("GP","G ")
            INDX = 34
	    CASE ("HT")
            INDX = 35
	    CASE ("VB","V ")
            INDX = 36
	    CASE ("W ","WB","WP","WA","WD","WS","WT")
            INDX = 37
	    CASE ("OT")
            INDX = 39

CC - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...

	    CASE ("Y ","YP","JR","LT","P ")
            INDX = 39
	    CASE ("PJ","PF","X ","XC","Z ","ZC")
            INDX = 39
	    CASE ("BB","OB","PM","PR","SN")
            INDX = 39
	    CASE ("OH","U ","UP","A ","AX","EA")
            INDX = 39
	    CASE ("EX","EW","K ","KC","VV","VP")
            INDX = 39
	    CASE ("M ","MV")
            INDX = 39
	    CASE ("XH","ZH","UA","AD","EE","ES","VS","ME")
            INDX = 39
	    CASE ("MN","MS","OD","OE","OF","OG","QE")
            INDX = 39
          CASE DEFAULT
            INDX = 39
	    END SELECT

      CASE("WC")
        SELECT CASE (SPKODE(1:2))
	    CASE ("BA")
	      INDX = 1
	    CASE ("BC")
            INDX = 2
          CASE ("BG")
            INDX = 3
          CASE ("BL")
            INDX = 4
	    CASE ("BM")
            INDX = 5
	    CASE ("BP")
            INDX = 7
	    CASE ("YC","LW","L ")
            INDX = 8
          CASE ("OA")
            INDX = 9
	    CASE ("SE","SX","S ","SW","SB","SS")
            INDX = 10
	    CASE ("PL")
            INDX = 11
	    CASE ("JP")
            INDX = 12
	    CASE ("PS")
            INDX = 13
	    CASE ("PW")
            INDX = 14
	    CASE ("PY")
            INDX = 15
	    CASE ("FD","F ")
            INDX = 15
	    CASE ("OC")
            INDX = 17
	    CASE ("CW","C ")
            INDX = 18
	    CASE ("HW","H ")
            INDX = 19
	    CASE ("HM")
            INDX = 20
	    CASE ("MB")
            INDX = 21
	    CASE ("DR","D ")
            INDX = 22
	    CASE ("RA","R ")
            INDX = 23
	    CASE ("EP","E ")
            INDX = 24
	    CASE ("GC")
            INDX = 25
	    CASE ("AT")
            INDX = 26
	    CASE ("AC")
            INDX = 27
	    CASE ("QG","Q ")
            INDX = 28
	    CASE ("J ")
            INDX = 29
	    CASE ("LA")
            INDX = 30
	    CASE ("PA","PX")
            INDX = 31
	    CASE ("KP")
            INDX = 32
	    CASE ("TW","T ")
            INDX = 33
	    CASE ("GP","G ")
            INDX = 34
	    CASE ("HT")
            INDX = 35
	    CASE ("VB","V ")
            INDX = 36
	    CASE ("W ","WB","WP","WA","WD","WS","WT")
            INDX = 37
	    CASE ("OT")
            INDX = 39

CC - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...

	    CASE ("Y ","YP","JR","LT","P ")
            INDX = 39
	    CASE ("PJ","PF","X ","XC","Z ","ZC")
            INDX = 39
	    CASE ("BB","OB","PM","PR","SN")
            INDX = 39
	    CASE ("OH","U ","UP","A ","AX","EA")
            INDX = 39
	    CASE ("EX","EW","K ","KC","VV","VP")
            INDX = 39
	    CASE ("M ","MV")
            INDX = 39
	    CASE ("XH","ZH","UA","AD","EE","ES","VS","ME")
            INDX = 39
	    CASE ("MN","MS","OD","OE","OF","OG","QE")
            INDX = 39
          CASE DEFAULT
            INDX = 39
	    END SELECT

      CASE("AK")
        SELECT CASE (SPKODE(1:2))
	    CASE ("SE","SX","S ","SW","SB")
	      INDX = 1
	    CASE ("CW","C ")
            INDX = 2
          CASE ("BA")
            INDX = 3
          CASE ("HM")
            INDX = 4
	    CASE ("HW","H ")
            INDX = 5
	    CASE ("Y ","YC")
            INDX = 6
	    CASE ("PL")
            INDX = 7
	    CASE ("SS")
            INDX = 8
          CASE ("BL","B ")
            INDX = 9
	    CASE ("D ","DR")
            INDX = 10
	    CASE ("AC")
            INDX = 11
	    CASE ("PW","LW","L ","FD","F ","BG","PY")
            INDX = 13
	    CASE ("OC","YP","J ","JR","LA","LT","P ")
            INDX = 13
	    CASE ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
            INDX = 13
	    CASE ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
            INDX = 13
	    CASE ("OT")
            INDX = 13
	    CASE ("OH","EP","E ","U ","UP","A ","AX","R ","RA","EA")
            INDX = 12
	    CASE ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
            INDX = 12
	    CASE ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
            INDX = 12
	    CASE ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
            INDX = 12
	    CASE ("MN","MS","OD","OE","OF","OG","QE","AT")
            INDX = 12
          CASE DEFAULT
            INDX = 13
	    END SELECT

      CASE DEFAULT
        INDX = 0
	END SELECT

      RETURN
      END
