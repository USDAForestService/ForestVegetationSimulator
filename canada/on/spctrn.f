      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C     PURPOSE:
C     TAKES TREELIST FILE UPPER CASE SPECIES CODE AS INPUT AND RETURNS
C     AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN THE 1-72
C     RANGE; NON-MATCHES ARE ASSIGNED 49.
C
C     THIS IS DUMMY CODE, SINCE THE SPECIES SHOWN HERE ARE ALL IDENTICAL
C     TO THOSE DECLARED AND INITIALIZED IN BLKDAT; THIS COULD BE USED
C     TO EXPAND THE ALPHA CODES THAT ARE RECOGNIZED
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'

      CHARACTER*(*)SPCIN
      INTEGER   ISPC1

      SELECT CASE (SPCIN(:2))
        CASE ("PJ")
          ISPC1 = 1
        CASE ("PS")
          ISPC1 = 2
        CASE ("RN")
          ISPC1 = 3
        CASE ("RP")
          ISPC1 = 4
        CASE ("PW")
          ISPC1 = 5
        CASE ("SW")
          ISPC1 = 6
        CASE ("SN")
          ISPC1 = 7
        CASE ("BF")
          ISPC1 = 8
c          
        CASE ("SB")
          ISPC1 = 9
        CASE ("TA")
          ISPC1 = 10
        CASE ("CE")
          ISPC1 = 11
        CASE ("HE")
          ISPC1 = 12
        CASE ("SO")
          ISPC1 = 13
        CASE ("CR")
          ISPC1 = 14
        CASE ("AB")
          ISPC1 = 15
        CASE ("AR")
          ISPC1 = 16
        CASE ("CW")
          ISPC1 = 17
        CASE ("MV")
          ISPC1 = 18
c          
        CASE ("MR")
          ISPC1 = 19
        CASE ("CB")
          ISPC1 = 20
        CASE ("EW")
          ISPC1 = 21
        CASE ("ES")
          ISPC1 = 22
        CASE ("ER")
          ISPC1 = 23
        CASE ("BY")
          ISPC1 = 24
        CASE ("BD")
          ISPC1 = 25
        CASE ("MH")
          ISPC1 = 26
        CASE ("MB")
          ISPC1 = 27
        CASE ("BE")
          ISPC1 = 28
c          
        CASE ("AW")
          ISPC1 = 29
        CASE ("OW")
          ISPC1 = 30
        CASE ("OP")
          ISPC1 = 31
        CASE ("OB")
          ISPC1 = 32
        CASE ("OC")
          ISPC1 = 33
        CASE ("OR")
          ISPC1 = 34
        CASE ("BO")
          ISPC1 = 35
        CASE ("PN")
          ISPC1 = 36
        CASE ("HB")
          ISPC1 = 37
        CASE ("HP")
          ISPC1 = 38
C          
        CASE ("HU")
          ISPC1 = 39
        CASE ("PG")
          ISPC1 = 40
        CASE ("PT")
          ISPC1 = 41
        CASE ("PB")
          ISPC1 = 42
        CASE ("BW")
          ISPC1 = 43
        CASE ("CH")
          ISPC1 = 44
        CASE ("BT")
          ISPC1 = 45
        CASE ("WB")
          ISPC1 = 46
        CASE ("IW")
          ISPC1 = 47
        CASE ("LB")
          ISPC1 = 48
C
        CASE ("NC")
          ISPC1 = 49
        CASE ("MM")
          ISPC1 = 50
        CASE ("MS")
          ISPC1 = 51
        CASE ("MT")
          ISPC1 = 52
        CASE ("BB")
          ISPC1 = 53
        CASE ("CA")
          ISPC1 = 54
        CASE ("BH")
          ISPC1 = 55
        CASE ("DF")
          ISPC1 = 56
        CASE ("HT")
          ISPC1 = 57
        CASE ("ML")
          ISPC1 = 58
C
        CASE ("GB")
          ISPC1 = 59
        CASE ("SY")
          ISPC1 = 60
        CASE ("CP")
          ISPC1 = 61
        CASE ("CC")
          ISPC1 = 62
        CASE ("PL")
          ISPC1 = 63
        CASE ("WI")
          ISPC1 = 64
!       CASE ("WI")   ! 3 willow species... so
!          ISPC1 = 65 ! 65-66 are not really 
!       CASE ("WI")   ! used at all
!          ISPC1 = 66
        CASE ("SS")
          ISPC1 = 67
        CASE ("AM")
          ISPC1 = 68
C          
        CASE ("JP")
          ISPC1 = 69
        CASE ("WP")
          ISPC1 = 70
        CASE ("SP")
          ISPC1 = 71
        CASE ("BP")
          ISPC1 = 72
C         
        CASE DEFAULT ! non-commercial hardwood
          ISPC1 = 49
        END SELECT

      RETURN
      END
