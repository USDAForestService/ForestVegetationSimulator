      SUBROUTINE REVISE (VAR,REV)
      IMPLICIT NONE
C----------
C  **REVISE--BASE  DATE OF LAST REVISION:  01/15/15
C  (DON'T CHANGE THIS DATE UNLESS THE SUBROUTINE LOGIC CHANGES.)
C----------
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
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C BLUE MOUNTAINS
C----------
      IF(VAR(:2).EQ.'BM') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C INLAND CALIFORNIA / SOUTHERN CASCADES
C----------
      IF(VAR(:2).EQ.'CA') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C CENTRAL IDAHO
C----------
      IF(VAR(:2).EQ.'CI') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C CENTRAL ROCKIES
C----------
      IF(VAR(:2).EQ.'SM' .OR. VAR(:2).EQ.'SP' .OR. VAR(:2).EQ.'BP'
     & .OR. VAR(:2).EQ.'SF' .OR. VAR(:2).EQ.'LP') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C CENTRAL STATES
C----------
      IF(VAR(:7).EQ.'CS') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C EAST CASCADES
C----------
      IF(VAR(:2).EQ.'EC') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C EASTERN MONTANA
C----------
      IF(VAR(:2).EQ.'EM') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C INLAND EMPIRE (AKA NI23)
C----------
      IF(VAR(:2).EQ.'IE') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C KOOTENAI / KANIKSU / TALLY LAKE
C----------
      IF(VAR(:2).EQ.'KT') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C LAKE STATES
C----------
      IF(VAR(:7).EQ.'LS') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C KLAMATH MOUNTAINS
C----------
      IF(VAR(:2).EQ.'NC') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C NORTHEAST STATES
C----------
      IF(VAR(:7).EQ.'NE') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C NORTH IDAHO
C----------
      IF(VAR(:2).EQ.'NI') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C SOUTHWEST OREGON ORGANON
C----------
      IF(VAR(:2).EQ.'OC') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C NORTHWEST OREGON ORGANON
C----------
      IF(VAR(:2).EQ.'OP') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C PACIFIC NORTHWEST
C----------
      IF(VAR(:2).EQ.'PN') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C SOUTHERN
C----------
      IF(VAR(:2).EQ.'SN') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C SOUTH CENTRAL OREGON / NORTHEAST CALIFORNIA
C----------
      IF(VAR(:2).EQ.'SO') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C TETONS
C----------
      IF(VAR(:2).EQ.'TT') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C UTAH
C----------
      IF(VAR(:2).EQ.'UT') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C WEST CASCADES
C----------
      IF(VAR(:2).EQ.'WC') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C----------
C WESTERN SIERRA NEVADA
C----------
      IF(VAR(:2).EQ.'WS') THEN
        REV = '06/26/15'
        GO TO 100
      ENDIF
C
  100 CONTINUE
      RETURN
      END
