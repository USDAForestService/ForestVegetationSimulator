      SUBROUTINE REVISE (VAR,REV)
      IMPLICIT NONE
C----------
C  **REVISE--BASE  DATE OF LAST REVISION:  03/22/11
C $Id: revise.f 271 2012-11-14 22:17:42Z jdh $
C $Revision: 271 $
C $Date: 2012-11-14 14:17:42 -0800 (Wed, 14 Nov 2012) $
C $HeadURL: https://www.forestinformatics.com/svn/fvs/trunk/cacor/src/revise.f $
C  (DON'T CHANGE THIS DATE UNLESS THE SUBROUTINE LOGIC CHANGES.)
C----------
C
C  THIS ROUTINE PROVIDES THE LATEST REVISION DATE FOR EACH VARIANT
C  WHICH GETS PRINTED IN THE MAIN HEADER ON THE OUTPUT.
C  CALLED FROM GROHED, FILOPN, SUMHED, SUMOUT, ECVOLS, PRTRLS,
C  AND DGDRIV.
C----------
      CHARACTER VAR*7,REV*10
C----------
C SOUTHEAST ALASKA / COASTAL BRITISH COLUMBIA
C----------
      IF(VAR(:2).EQ.'AK') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C BLUE MOUNTAINS
C----------
      IF(VAR(:2).EQ.'BM') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C INLAND CALIFORNIA / SOUTHERN CASCADES
C----------
      IF(VAR(:2).EQ.'CA') THEN
        REV = '11/14/12'
        GO TO 100
      ENDIF
C----------
C CENTRAL IDAHO
C----------
      IF(VAR(:2).EQ.'CI') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C CENTRAL ROCKIES
C----------
      IF(VAR(:2).EQ.'SM' .OR. VAR(:2).EQ.'SP' .OR. VAR(:2).EQ.'BP'
     & .OR. VAR(:2).EQ.'SF' .OR. VAR(:2).EQ.'LP') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C CENTRAL STATES
C----------
      IF(VAR(:7).EQ.'CS') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C EAST CASCADES
C----------
      IF(VAR(:2).EQ.'EC') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C EASTERN MONTANA
C----------
      IF(VAR(:2).EQ.'EM') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C INLAND EMPIRE (AKA NI23)
C----------
      IF(VAR(:2).EQ.'IE') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C KOOTENAI / KANIKSU / TALLY LAKE
C----------
      IF(VAR(:2).EQ.'KT') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C LAKE STATES
C----------
      IF(VAR(:7).EQ.'LS') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C KLAMATH MOUNTAINS
C----------
      IF(VAR(:2).EQ.'NC') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C NORTHEAST STATES
C----------
      IF(VAR(:7).EQ.'NE') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C NORTH IDAHO
C----------
      IF(VAR(:2).EQ.'NI') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C PACIFIC NORTHWEST
C----------
      IF(VAR(:2).EQ.'PN') THEN
        REV = '11/14/12'
        GO TO 100
      ENDIF
C----------
C SOUTHERN
C----------
      IF(VAR(:2).EQ.'SN') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C SOUTH CENTRAL OREGON / NORTHEAST CALIFORNIA
C----------
      IF(VAR(:2).EQ.'SO') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C TETONS
C----------
      IF(VAR(:2).EQ.'TT') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C UTAH
C----------
      IF(VAR(:2).EQ.'UT') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C----------
C WEST CASCADES
C----------
      IF(VAR(:2).EQ.'WC') THEN
        REV = '04/26/12'
        GO TO 100
      ENDIF
C----------
C WESTERN SIERRA NEVADA
C----------
      IF(VAR(:2).EQ.'WS') THEN
        REV = '12/20/11'
        GO TO 100
      ENDIF
C
  100 CONTINUE
      RETURN
      END
