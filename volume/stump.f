C----------
C VOLUME $Id$
C----------
c     Subroutines to calculate diameter inside and ouside bark diameter between 0 and 4.5 and also stump volume.
c     From Raile, G 1982. Estimating stump volume. Res. Pap. NC-224 St Paul, USDA FS North Central Forest Experimental Station
c     Created by Y. Wang 03/13/2012
      subroutine stumpdia(spn, dbh, stumpht, stumpdib, stumpdob)
      integer spn
      real dbh, stumpdib, stumpdob, stumpht 
      real dob_b, dib_a, dib_b
      
      
      if (stumpht.gt.4.5 .or. stumpht.lt.0.0001) return
      CALL STUMPCOEF(spn, dob_b, dib_a, dib_b)
      
      stumpdob = dbh + dbh*dob_b*(4.5-stumpht)/(stumpht+1.0)
      stumpdib = dbh*dib_a+dbh*dib_b*(4.5-stumpht)/(stumpht+1.0)
      
      return
      END
      
      SUBROUTINE STUMPCOEF(SPN, DOBB, DIBA, DIBB)
      INTEGER SPN, IDX, SPLIST(23)
      REAL dob_b(23), dib_a(23), dib_b(23)
      INTEGER DONE,LAST,ERRFLAG
      INTEGER spgrp123(55),spgrp4(39),spgrp5(9),spgrp6(27),spgrp7(18)
      INTEGER spgrp9(59),spgrp10(38)
      
      DATA (splist(I),I=1,23)/129,125,105,090,094,095,012,261,241,802,
     +                        833,531,371,318,317,544,543,375,743,746,
     +                        950,740,970/
C     Jenkins species group 1,2,3 use 261
      DATA (spgrp123(I),I=1,55)/
     +    10,11,12,14,15,16,17,18,19,20,
     +    21,22,40,41,42,43,50,51,52,53,
     +    54,55,56,57,64,67,68,70,71,72,
     +    73,81,200,201,202,211,212,220,221,222,
     +    223,230,231,232,240,241,242,250,251,252,
     +    260,261,262,263,264/
C     Jenkins species group 4 use 125
      DATA (spgrp4(I),I=1,39)/
     +    100,101,102,103,104,105,107,108,109,110,
     +    111,112,113,114,115,116,117,118,119,120,
     +    121,122,123,124,125,126,127,128,129,130,
     +    131,132,135,136,137,139,142,144,299/
C     Jenkins species group 5 use 090
      DATA (spgrp5(I),I=1,9)/90,91,92,93,94,95,96,97,98/
C     Jenkins species group 6 use 746
      DATA (spgrp6(I),I=1,27)/
     +     350,351,352,353,355,740,741,742,743,744,
     +     745,746,747,748,749,752,753,920,921,922,
     +     923,924,925,926,927,928,929/
C     Jenkins species group 7 use 317
      DATA (spgrp7(I),I=1,18)/
     +     310,311,312,313,315,316,317,319,320,370,
     +     371,372,373,374,375,377,378,379/
      
C     Jenkins species group 8 use 544
C     Jenkins species group 9 use 833
      DATA (spgrp9(I),I=1,59)/
     +     314,318,400,401,402,403,404,405,406,407,
     +     408,409,410,411,412,413,531,800,801,802,
     +     804,805,806,807,808,809,811,812,813,815,
     +     816,817,818,819,820,821,822,823,824,825,
     +     826,827,828,830,831,832,833,834,835,836,
     +     837,838,839,840,841,842,844,845,851/
C     Jenkins species group 10 use 833
      DATA (spgrp10(I),I=1,38)/
     +     58,59,60,61,62,63,65,66,69,106,
     +     133,134,138,140,141,143,300,303,304,321,
     +     322,363,475,523,755,756,757,758,803,810,
     +     814,829,843,846,847,867,902,990/
     
      DATA (dob_b(I),I=1,23)/.11694,.08091,.08076,.14525,.16903,
     +                       .12147,.15359,.12667,.18850,.14872,
     +                       .12798,.15113,.15350,.12111,.11585,
     +                       .12766,.17376,.11655,.06834,.09658,
     +                       .14413,.17123,.16638/
      DATA (dib_a(I),I=1,23)/.91385,.90698,.90973,.94804,.95487,
     +                       .94122,.93793,.91400,.94698,.91130,
     +                       .92267,.96731,.94423,.93818,.94181,
     +                       .91979,.93502,.93763,.91625,.91882,
     +                       .92442,.92736,.93257/
      DATA (dib_b(I),I=1,23)/.11182,.08469,.07926,.13722,.15664,
     +                       .11781,.14553,.11975,.18702,.14907,
     +                       .12506,.14082,.14335,.11424,.10740,
     +                       .12152,.17071,.10640,.06478,.08593,
     +                       .14240,.17626,.15803/
5     CONTINUE
      idx = 0
      DOBB = 0;
      DIBA = 1;
      DIBB = 0;
      DO I=1,23
        if(spn.eq.splist(I)) then
          idx = I
          exit;
        endif
      ENDDO
      
      IF (idx .eq. 0) THEN
        DONE = 0
        LAST = 55
        CALL SEARCH(LAST, spgrp123, SPN, DONE, ERRFLAG)
        
        IF(DONE .GT. 0)THEN
          SPN = 261
          GOTO 5
        ELSE
C         Not in group 1,2,3 and continue search group 4
          DONE = 0
          LAST = 34
          CALL SEARCH(LAST, spgrp4, SPN, DONE, ERRFLAG)
          IF(DONE .GT. 0)THEN
            SPN = 125
            GOTO 5
          ELSE
C           Not find in group 4 and continue search group 5
            DONE = 0
            LAST = 9
            CALL SEARCH(LAST, spgrp5, SPN, DONE, ERRFLAG)
            IF(DONE .GT. 0)THEN
              SPN = 90
              GOTO 5
            ELSE
C             Not find in group 5 and continue search group 6
              DONE = 0
              LAST = 27
              CALL SEARCH(LAST, spgrp6, SPN, DONE, ERRFLAG)
              IF(DONE .GT. 0)THEN
                SPN = 746
                GOTO 5
              ELSE
C               Not find in group 6 and continue search group 7
                DONE = 0
                LAST = 18
                CALL SEARCH(LAST, spgrp7, SPN, DONE, ERRFLAG)
                IF(DONE .GT. 0)THEN
                  SPN = 317
                  GOTO 5
                ELSE
C                 Not find in group 7 and continue search group 9
                  DONE = 0
                  LAST = 59
                  CALL SEARCH(LAST, spgrp9, SPN, DONE, ERRFLAG)
                  IF(DONE .GT. 0)THEN
                    SPN = 833
                    GOTO 5
                  ELSE
C                   Not find in group 9 and continue search group 10
                    DONE = 0
                    LAST = 38
                    CALL SEARCH(LAST, spgrp10, SPN, DONE, ERRFLAG)
                    IF(DONE .GT. 0)THEN
                      SPN = 833
                      GOTO 5
                    ELSE
                      SPN = 544
                      GOTO 5
                    END IF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      if(idx.ne.0) then
        DOBB = dob_b(idx)
        DIBA = dib_a(idx)
        DIBB = dib_b(idx)
      endif
      return
      END

C  Cubic foot volume of stump including wood and bark based on
C  integration formula provided by Raile
      SUBROUTINE RAILEVOL(SPN, DBH, HTUP, VOLIB, VOLOB)
      INTEGER SPN
      REAL DBH, HTUP, VOLIB, VOLOB
      REAL DOB_B, DIB_A, DIB_B
      
      IF (HTUP.LT.0.01) HTUP = 1
      CALL STUMPCOEF(SPN, DOB_B, DIB_A, DIB_B)
      VOLIB = 0.0054541539*DBH*DBH*(((DIB_A-DIB_B)**2*HTUP+
     +        11*DIB_B*(DIB_A-DIB_B)*LOG(HTUP+1)-
     +        30.25/(HTUP+1)*DIB_B**2)-((DIB_A-DIB_B)**2*0+
     +        11*DIB_B*(DIB_A-DIB_B)*LOG(0+1.0)-
     +        30.25/(0+1)*DIB_B**2)) 
      VOLOB = 0.0054541539*DBH*DBH*(((1-DOB_B)**2*HTUP+
     +        11*DOB_B*(1-DOB_B)*LOG(HTUP+1)-
     +        30.25/(HTUP+1)*DOB_B**2)-((HTUP-DOB_B)**2*0+
     +        11*DOB_B*(1-DOB_B)*LOG(0+1.0)-
     +        30.25/(0+1)*DOB_B**2))
      RETURN
      END
C  Cubic foot volume of stump inside and outside bark. DIB/DOB pridicated by 
C  Raile computed for half way up stump and then volume computed for cylinder
      SUBROUTINE RAILEVOL_CYLND(SPN, DBH, HTUP, VOLIB, VOLOB)
      INTEGER SPN
      REAL DBH, HTUP, VOLIB, VOLOB, DIB_MID, DOB_MID, STUMP_MID
      STUMP_MID = HTUP/2
      CALL STUMPDIA(SPN, DBH, STUMP_MID, DIB_MID, DOB_MID)
      VOLIB = 0.0054541539*(DIB_MID**2)*HTUP
      VOLOB = 0.0054541539*(DOB_MID**2)*HTUP
      RETURN
      END
C  Cubic foot volume of stump inside and outside bark. DIB/DOB pridicated by 
C  Raile and sum of segments using Smailian formula
      SUBROUTINE RAILEVOL_SEG(SPN, DBH, HTUP, VOLIB, VOLOB)
      INTEGER SPN, SEGS
      REAL DIB1, DIB2, HT1, HT2, SEGLEN, SEGVOL, LEFTOV, CU
      REAL DOB_B, DIB_A, DIB_B
      SEGLEN = .1
      SEGVOL = 0
      SEGS = INT(HTUP/SEGLEN)
      LEFTOV = HTUP-SEGS*SEGLEN
      CALL STUMPCOEF(SPN, DOB_B, DIB_A, DIB_B)
      DO I=1,2
        CU = 0
        DIB1 = 0
        DIB2 = 0
        HT1 = 0
        HT2 = 0
        IF (I.eq.2) THEN
          DIB_A = 1
          DIB_B = DOB_B
        ENDIF
        DIB1 =  DBH*DIB_A + DBH*DIB_B * (4.5-HT1) / (HT1+1)
        DO J=1,SEGS
          HT2 = HT2 + SEGLEN
          DIB2 = DBH*DIB_A + DBH*DIB_B * (4.5-HT2) / (HT2+1)
          SEGVOL = .00272708*(DIB1*DIB1+DIB2*DIB2)*SEGLEN
          CU = CU+SEGVOL
          DIB1 = DIB2
          HT1 = HT2
        ENDDO
        IF (I.eq.1) THEN
          VOLIB = CU
        ELSE
          VOLOB = CU
        ENDIF
      ENDDO
      RETURN
      END

C  This rubroutine searches an array to find the maching element
      SUBROUTINE SEARCH(M,ARY,SPC,DONE,ERRFLAG)
      INTEGER M
      integer ARY(M)
      INTEGER SPC,FIRST,LAST,HALF,DONE,ERRFLAG
      FIRST=1
      LAST=M
      DONE=0
      DO 5, WHILE (DONE.EQ.0)
         HALF = (LAST - FIRST +1)/2 + FIRST
          IF(ARY(HALF) .EQ. SPC)THEN
             DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
             ERRFLAG = 1
             DONE = -1
         ELSE IF (ARY(HALF) .LT. SPC) THEN
             FIRST = HALF
          ELSE
             LAST = HALF - 1
          ENDIF
  5   CONTINUE 
      RETURN
      END       