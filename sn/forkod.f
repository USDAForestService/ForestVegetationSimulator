      SUBROUTINE FORKOD
      IMPLICIT NONE
C----------
C SN $Id$
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
C
      INCLUDE 'SNCOM.F77'
C
COMMONS
C
      INTEGER JFOR(20),NUMFOR,IFORDI,I
      DATA JFOR/801,802,803,804,805,806,807,808,809,810,
     &          811,812,813,905,908,836,824,860,835,701/
      DATA NUMFOR/20/
C
C  Proclaimed National Forest  District     Region/Forest/District Code
C  National Forests in Alabama Bankhead             80101
C         Conecuh                                   80103
C         Oakmulgee                                 80104
C         Shoal Creek                               80105
C         Talledega                                 80106
C         Tuskegee                                  80107
C  Daniel Boone  Morehead                           80211
C         Stanton                                   80212
C         Berea                                     80213
C         London                                    80214
C         Somerset                                  80215
C         Stearns                                   80216
C         Redbird                                   80217
C  Chattahoochee-Oconee Armuchee                    80301
C         Toccoa                                    80302
C         Brasstown                                 80304
C         Talluah                                   80305
C         Chattooga                                 80306
C         Cohutta                                   80307
C         Oconee                                    80308
C  Cherokee      Hiwassee                           80401
C         Nolichucky                                80402
C         Ocoee                                     80403
C         Tellico                                   80404
C         Unaka                                     80405
C         Watuga                                    80406
C  National Forests in Florida Apalachicola         80501
C         Lake George                               80502
C         Osceola                                   80504
C         Seminole                                  80505
C         Wakulla                                   80506
C  Kisatchie     Catahoula                          80601
C         Evangeline/Vernon                         80602
C         Kisatchie                                 80603
C         Winn                                      80604
C         Caney                                     80605
C  National Forests in Miss. Bienville              80701
C         Desoto                                    80702
C         Homochitto                                80704
C         Chickasawhay                              80705
C         Delta                                     80706
C         Holly Springs                             80707
C         Tombigbee                                 80717
C   George Washington/Jefferson NFs Deerfield       80801
C         Dry River                                 80802
C         James River                               80803
C         Lee                                       80804
C         Pedlar                                    80805
C         Warm Springs                              80806
C         Blacksburg                                80811
C         Clinch                                    80812
C         Glenwood                                  80813
C         Mt. Rogers                                80814
C         New Castle                                80815
C         Wythe                                     80816
C  Quachita      Choctaw                            80901
C         Caddo                                     80902
C         Cold Springs                              80903
C         Fourche                                   80904
C         Jessieville                               80905
C         Kiamichi                                  80906
C         Mena                                      80907
C         Oden                                      80908
C         Poteau                                    80909
C         Womble                                    80910
C         Winona                                    80911
C         Tiak                                      80912
C  Ozark & St. Francis NFs     Sylamore             81001
C         Buffalo                                   81002
C         Bayou                                     81003
C         Pleasant Hill                             81004
C         Boston Mountain                           81005
C         Magazine                                  81006
C         St. Francis                               81007
C  National Forests in North Carolina Cheoah        81102
C         Croatan                                   81103
C         Appalachian (old)                         81104
C         Grandfather                               81105
C         Highlands                                 81106
C         Pisgah                                    81107
C         Appalachian                               81108
C         Tusquitee                                 81109
C         Uwharrie                                  81110
C         Waysh                                     81111
C  Francis Marion & Sumter NFs Enoree/Tyger         81201
C         Andrew Pickens                            81202
C         Long cane                                 81203
C         Wambaw/Witherbee                          81205
C  National Forests in Texas   Angelina             81301
C         Davy Crockett                             81303
C         Sam Houston                               81304
C         Sabine                                    81307
C         Caddo/LBJ                                 81308
C
C  Mark Twain National Forest                       905
C
C  Shawnee National Forest                          908
C
C  Savannah River (Administrative)                  836 (map to Sumter)
C  Savannah River (Proclaimed)                      824 (map to Sumter)
C  Land Between the Lakes (Administrative)          860 (map to Daniel Boone)
C  Land Between the Lakes (Proclaimed)              835 (map to Daniel Boone)
C
C  Dept of Defense, Fort Bragg                      701
C----------
C  ------------------------
C  RESERVATION PSUEDO CODES:
C
C  7111 = SANTEE SDTSA                81205 FRANCIS MARION NF
C  7201 = ALABAMA-COUSHATTA RES.      81304 TEXAS NF SAM HOUSTON DIST
C  7207 = KIOWA-COMANCHE-APACHE-      80906 OUACHITA NF KIAMICHI DIST
C         FORT SILL APACHE OTSA
C  7210 = KAW OTSA                    81005 OZARK NF BOSTON MOUNTAIN DIST
C  7211 = OTOE-MISSOURIA OTSA         81005 OZARK NF BOSTON MOUNTAIN DIST
C  7212 = PAWNEE OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
C  7213 = PONCA OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
C  7215 = CITIZEN POTAWATOMI NATION-  80901 OUACHITA NF CHOCTAW DIST
C         ABSENTEE SHAWNEE OTSA
C  7216 = IOWA OTSA                   80901 OUACHITA NF CHOCTAW DIST
C  7218 = SAC AND FOX OTSA            80901 OUACHITA NF CHOCTAW DIST
C  7601 = CHICKASAW OTSA              80906 OUACHITA NF KIAMICHI DIST
C  7602 = QUAPAW OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
C  7603 = EASTERN SHAWNEE OTSA        81005 OZARK NF BOSTON MOUNTAIN DIST
C  7604 = SENECA-CAYUGA OTSA          81005 OZARK NF BOSTON MOUNTAIN DIST
C  7605 = WYANDOTTE OTSA              81005 OZARK NF BOSTON MOUNTAIN DIST
C  7606 = MIAMI OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
C  7607 = PEORIA OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
C  7608 = MODOC OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
C  7609 = OSAGE RES.                  81005 OZARK NF BOSTON MOUNTAIN DIST
C  7610 = CREEK OTSA                  80901 OUACHITA NF CHOCTAW DIST
C  7611 = CHEROKEE OTSA               81005 OZARK NF BOSTON MOUNTAIN DIST
C  7612 = CHOCTAW OTSA                80906 OUACHITA NF KIAMICHI DIST
C  7613 = SEMINOLE OTSA               80901 OUACHITA NF CHOCTAW DIST
C  8205 = MICCOSUKEE RES.             80505 FLORIDA NF SEMINOLE DIST
C  8207 = POARCH CREEK RES.           80103 ALABAMA NF CONECUH DIST
C  8210 = CATAWBA RES.                81201 SUMTER NF ENOREE DIST
C  8212 = TUNICA-BILOXI RES.          80601 KISATCHIE NF CATAHOULA DIST
C  8213 = COUSHATTA RES.              80602 KISATCHIE NF CALCASIEU DIST
C  8219 = EASTERN CHEROKEE RES.       81111 NORTH CAROLINA NF NANTAHALA DIST
C  8220 = SEMINOLE (FL) TRUST LAND    80505 FLORIDA NF SEMINOLE DIST
C  8221 = MISSISSIPPI CHOCTAW RES.    80701 MISSISSIPPI NF BIENVILLE DIST
C
C  HANDLE REGION 9 FORESTS DIFFERENTLY
C----------
        SELECT CASE (KODFOR)
          CASE (7111)
            WRITE(JOSTND,61)
   61       FORMAT(T12,'SANTEE SDTSA (7111) being MAPPED ',
     &      'TO 81205 FRANCIS MARION NF FOR FURTHER PROCESSING.')
            KODFOR = 81205
          CASE (7201)
            WRITE(JOSTND,62)
   62       FORMAT(T12,'ALABAMA-COUSHATTA RES. (7201) being MAPPED ',
     &      'TO 81304 TEXAS NF SAM HOUSTON DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81304
          CASE (7207)
            WRITE(JOSTND,63)
   63       FORMAT(T12,'KIOWA-COMANCHE-APACHE-FORT SILL APACHE OTSA ',
     &      '(7207) being MAPPED TO 80906 OUACHITA NF KIAMICHI DIST ', 
     &      'FOR FURTHER PROCESSING.')
            KODFOR = 80906
          CASE (7210)
            WRITE(JOSTND,64)
   64       FORMAT(T12,'KAW OTSA (7210) being MAPPED ',
     &      'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7211)
            WRITE(JOSTND,65)
   65       FORMAT(T12,'OTOE-MISSOURIA OTSA (7211) being MAPPED ',
     &      'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7212)
            WRITE(JOSTND,66)
   66       FORMAT(T12,'PAWNEE OTSA (7212) being MAPPED ',
     &      'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7213)
            WRITE(JOSTND,67)
   67       FORMAT(T12,'PONCA OTSA (7213) being MAPPED ',
     &      'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7215)
            WRITE(JOSTND,68)
   68       FORMAT(T12,'CITIZEN POTAWATOMI NATION-ABSENTEE SHAWNEE ',
     &      'OTSA (7215) being MAPPED TO 80901 OUACHITA NF CHOCTAW ', 
     &      'DIST FOR FURTHER PROCESSING.')
            KODFOR = 80901
          CASE (7216)
            WRITE(JOSTND,69)
   69       FORMAT(T12,'IOWA OTSA (7216) being MAPPED TO 80901 ',
     &      'OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80901
          CASE (7218)
            WRITE(JOSTND,70)
   70       FORMAT(T12,'SAC AND FOX OTSA (7218) being MAPPED TO ',
     &      '80901 OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80901
          CASE (7601)
            WRITE(JOSTND,71)
   71       FORMAT(T12,'CHICKASAW OTSA (7601) being MAPPED TO 80906 ',
     &      'OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80906
          CASE (7602)
            WRITE(JOSTND,72)
   72       FORMAT(T12,'QUAPAW OTSA (7602) being MAPPED TO 81005 ',
     &      'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7603)
            WRITE(JOSTND,73)
   73       FORMAT(T12,'EASTERN SHAWNEE OTSA (7603) being MAPPED ',
     &      'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7604)
            WRITE(JOSTND,74)
   74       FORMAT(T12,'SENECA-CAYUGA OTSA (7604) being MAPPED TO ',
     &      '81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81005
          CASE (7605)
            WRITE(JOSTND,75)
   75       FORMAT(T12,'WYANDOTTE OTSA (7605) being MAPPED TO 81005 ',
     &      'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7606)
            WRITE(JOSTND,76)
   76       FORMAT(T12,'MIAMI OTSA (7606) being MAPPED TO 81005 ',
     &      'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7607)
            WRITE(JOSTND,77)
   77       FORMAT(T12,'PEORIA OTSA (7607) being ',
     &      'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &      'FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7608)
            WRITE(JOSTND,78)
   78       FORMAT(T12,'MODOC OTSA (7608) being ',
     &      'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &      'FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7609)
            WRITE(JOSTND,79)
   79       FORMAT(T12,'OSAGE RES. (7609) being ',
     &      'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &      'FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7610)
            WRITE(JOSTND,80)
   80       FORMAT(T12,'CREEK OTSA (7610) being ',
     &      'MAPPED TO 80901 OUACHITA NF CHOCTAW DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 80901
          CASE (7611)
            WRITE(JOSTND,81)
   81       FORMAT(T12,'CHEROKEE OTSA (7611) being MAPPED TO 81005 ',
     &      'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.') 
            KODFOR = 81005
          CASE (7612)
            WRITE(JOSTND,82)
   82       FORMAT(T12,'CHOCTAW OTSA (7612) being MAPPED TO 80906 ',
     &      'OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80906
          CASE (7613)
            WRITE(JOSTND,83)
   83       FORMAT(T12,'SEMINOLE OTSA (7613) being MAPPED TO 80901 ',
     &      'OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80901
          CASE (8205)
            WRITE(JOSTND,84)
   84       FORMAT(T12,'MICCOSUKEE RES. (8205) being MAPPED TO ',
     &      '80505 FLORIDA NF SEMINOLE DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80505
          CASE (8207)
            WRITE(JOSTND,85)
   85       FORMAT(T12,'POARCH CREEK RES. (8207) being MAPPED TO ',
     &      '80103 ALABAMA NF CONECUH DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80103
          CASE (8210)
            WRITE(JOSTND,86)
   86       FORMAT(T12,'CATAWBA RES. (8210) being MAPPED TO 81201 ',
     &      'SUMTER NF ENOREE DIST FOR FURTHER PROCESSING.') 
            KODFOR = 81201
          CASE (8212)
            WRITE(JOSTND,87)
   87       FORMAT(T12,'TUNICA-BILOXI RES. (8212) being MAPPED TO ',
     &      '80601 KISATCHIE NF CATAHOULA DIST FOR FURTHER '
     &      'PROCESSING.') 
            KODFOR = 80601
          CASE (8213)
            WRITE(JOSTND,88)
   88       FORMAT(T12,'COUSHATTA RES. (8213) being MAPPED TO 80602 ',
     &      'KISATCHIE NF CALCASIEU DIST FOR FURTHER PROCESSING.') 
            KODFOR = 80602
          CASE (8219)
            WRITE(JOSTND,89)
   89       FORMAT(T12,'EASTERN CHEROKEE RES. (8219) being MAPPED ',
     &      'TO 81111 NORTH CAROLINA NF NANTAHALA DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 81111
          CASE (8220)
            WRITE(JOSTND,90)
   90       FORMAT(T12,'SEMINOLE (FL) TRUST LAND (8220) being ',
     &      'MAPPED TO 80505 FLORIDA NF SEMINOLE DIST FOR FURTHER ',
     &      'PROCESSING.') 
            KODFOR = 80505
          CASE (8221)
            WRITE(JOSTND,91)
   91       FORMAT(T12,'MISSISSIPPI CHOCTAW RES. (8221) being ',
     &      'MAPPED TO 80701 MISSISSIPPI NF BIENVILLE DIST FOR ',
     &      'FURTHER PROCESSING.') 
            KODFOR = 80701
      END SELECT
      
    5 CONTINUE
      IF(KODFOR.EQ.0.AND.(IFOR.EQ.14.OR.IFOR.EQ.15.OR.IFOR.EQ.20))
     &   GO TO 15
      IF(KODFOR.EQ.905 .OR. KODFOR.EQ.908)THEN
        IF(KODFOR .EQ. 905)THEN
          IFOR=14
        ELSEIF(KODFOR .EQ. 908)THEN
          IFOR=15
        ENDIF
        KODIST=1
        ISEFOR=0
        WRITE(JOSTND,11)JFOR(IFOR),KODIST
        GOTO 900
      ENDIF
C----------
C  HANDLE SAVANNAH RIVER AND LAND BETWEEN THE LAKES
C----------
      IF(KODFOR.EQ.836 .OR. KODFOR.EQ.824)THEN
        KODFOR=81203
        WRITE(JOSTND,21)
   21   FORMAT(T12,'SAVANNAH RIVER BEING MAPPED TO SUMTER NF ',
     &  '(81203) FOR FURTHER PROCESSING.')
      ENDIF
      IF(KODFOR.EQ.860 .OR. KODFOR.EQ.835)THEN
        KODFOR=80216
        WRITE(JOSTND,22)
   22   FORMAT(T12,'LAND BETWEEN THE LAKES BEING MAPPED TO DANIEL ',
     &  'BOONE (80216) FOR FURTHER PROCESSING.')
      ENDIF
C----------
C  HANDLE FORT BRAGG MILITARY INSTALLATION
C----------
      IF(KODFOR .EQ. 701)THEN
        KODFOR=81110
        WRITE(JOSTND,23)
   23   FORMAT(T12,'FORT BRAGG BEING MAPPED TO NFs IN NC, UWHARRIE ',
     &  'DISTRICT (81110) FOR FURTHER PROCESSING.')
        IFOR=20
        KODIST=10
        ISEFOR=701
        GO TO 900
      ENDIF
C----------
C  REGION 8 SECTION
C----------
      IFORDI = KODFOR/100
      IF (IFORDI.EQ.0 .OR. IFORDI.EQ.800) GO TO 15
      IF (IFORDI.EQ.905 .OR. IFORDI.EQ.908 .OR.
     &    IFORDI.EQ.836 .OR. IFORDI.EQ.824 .OR.
     &    IFORDI.EQ.860 .OR. IFORDI.EQ.835)THEN
        KODFOR=IFORDI
        GO TO 5
      ELSEIF (IFORDI .EQ. 701)THEN
        KODFOR=701
        GO TO 5
      ENDIF
      DO 10 I=1,NUMFOR
      IF (IFORDI .EQ. JFOR(I)) GOTO 20
   10 CONTINUE
   15 CONTINUE
      CALL ERRGRO (.TRUE.,3)
      WRITE(JOSTND,11) JFOR(IFOR),KODIST
   11 FORMAT(T12,'FOREST CODE USED FOR THIS PROJECTION IS',I4,/,
     &   T12,'DISTRICT USED FOR THIS PROJECTION IS',I3)
      GOTO 50
   20 CONTINUE
      IFOR=I
C----------
C  DEFINE KODIST
C----------
      KODIST = KODFOR-(IFORDI*100)
      IF (JFOR(I) .EQ. 801) THEN
        IF(KODIST.LT.1 .OR. KODIST.EQ.2 .OR. KODIST.GT.7)KODIST=3
      ELSEIF (JFOR(I) .EQ. 802) THEN
        IF(KODIST.LT.11 .OR. KODIST.EQ.16 .OR. KODIST.GT.17)KODIST=11
      ELSEIF (JFOR(I) .EQ. 803) THEN
        IF(KODIST.LT.1 .OR. KODIST.EQ.3 .OR. KODIST.GT.8)KODIST=8
      ELSEIF(JFOR(I) .EQ. 804)THEN
        IF(KODIST.LT.1 .OR. KODIST.GT.6)KODIST=1
      ELSEIF(JFOR(I) .EQ. 805)THEN
        IF(KODIST.LT.1 .OR. KODIST.EQ.3 .OR. KODIST.GT.6)KODIST=4
      ELSEIF (JFOR(I) .EQ. 806) THEN
        IF(KODIST.LT.1 .OR. KODIST.GT.5)KODIST=3
      ELSEIF(JFOR(I) .EQ. 807)THEN
        IF(KODIST.LT.1 .OR. KODIST.EQ.5 .OR. (KODIST.GT.7 .AND.
     &     KODIST.LT.17) .OR. KODIST.GT.17)KODIST=7
      ELSEIF (JFOR(I) .EQ. 808) THEN
        IF(KODIST.LT.1 .OR. (KODIST.GE.7 .AND. KODIST.LE.10) .OR.
     &     KODIST.GT.16)KODIST=16
      ELSEIF (JFOR(I) .EQ. 809) THEN
        IF(KODIST.LT.1 .OR. KODIST.GT.12)KODIST=7
      ELSEIF (JFOR(I) .EQ. 810) THEN
        IF(KODIST.LT.1 .OR. KODIST.GT.7)KODIST=5
      ELSEIF(JFOR(I) .EQ. 811)THEN
        IF(KODIST.LT.2 .OR. KODIST.GT.11)KODIST=5
      ELSEIF(JFOR(I) .EQ. 812)THEN
        IF(KODIST.LT.1 .OR. KODIST.EQ.4 .OR. KODIST.GT.5)KODIST=1
      ELSE
        IF(KODIST.LT.1 .OR. KODIST.EQ.2 .OR. KODIST.EQ.5 .OR.
     &     KODIST.EQ.6 .OR. KODIST.GT.8)KODIST=3
      ENDIF
   50 CONTINUE
      IF(IFOR.NE.14 .AND. IFOR.NE.15 .AND. IFOR.NE.20)ISEFOR=JFOR(IFOR)
      IF(IFOR.NE.20)KODFOR=(JFOR(IFOR)*100)+KODIST
  900 CONTINUE
C----------
C  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
C----------
      SELECT CASE(JFOR(IFOR))
        CASE(801)
          IF(TLAT.EQ.0) TLAT=32.37
          IF(TLONG.EQ.0)TLONG=86.30
          IF(ELEV.EQ.0) ELEV=7.
        CASE(802)
          IF(TLAT.EQ.0) TLAT=37.99
          IF(TLONG.EQ.0)TLONG=84.18
          IF(ELEV.EQ.0) ELEV=12.
        CASE(803)
          IF(TLAT.EQ.0) TLAT=34.30
          IF(TLONG.EQ.0)TLONG=83.82
          IF(ELEV.EQ.0) ELEV=17.
        CASE(804)
          IF(TLAT.EQ.0) TLAT=35.16
          IF(TLONG.EQ.0)TLONG=84.88
          IF(ELEV.EQ.0) ELEV=22.
        CASE(805)
          IF(TLAT.EQ.0) TLAT=30.44
          IF(TLONG.EQ.0)TLONG=84.28
          IF(ELEV.EQ.0) ELEV=1.
        CASE(806)
          IF(TLAT.EQ.0) TLAT=31.32
          IF(TLONG.EQ.0)TLONG=92.43
          IF(ELEV.EQ.0) ELEV=2.
        CASE(807)
          IF(TLAT.EQ.0) TLAT=33.31
          IF(TLONG.EQ.0)TLONG=89.17
          IF(ELEV.EQ.0) ELEV=3.
        CASE(808)
          IF(TLAT.EQ.0) TLAT=37.27
          IF(TLONG.EQ.0)TLONG=79.94
          IF(ELEV.EQ.0) ELEV=21.
        CASE(809)
          IF(TLAT.EQ.0) TLAT=34.50
          IF(TLONG.EQ.0)TLONG=93.06
          IF(ELEV.EQ.0) ELEV=9.
        CASE(810)
          IF(TLAT.EQ.0) TLAT=35.28
          IF(TLONG.EQ.0)TLONG=93.13
          IF(ELEV.EQ.0) ELEV=13.
        CASE(811)
          IF(TLAT.EQ.0) TLAT=35.60
          IF(TLONG.EQ.0)TLONG=82.55
          IF(ELEV.EQ.0) ELEV=25.
        CASE(812)
          IF(TLAT.EQ.0) TLAT=34.00
          IF(TLONG.EQ.0)TLONG=81.04
          IF(ELEV.EQ.0) ELEV=4.
        CASE(813)
          IF(TLAT.EQ.0) TLAT=31.34
          IF(TLONG.EQ.0)TLONG=94.73
          IF(ELEV.EQ.0) ELEV=3.
        CASE(905)
          IF(TLAT.EQ.0) TLAT=37.95
          IF(TLONG.EQ.0)TLONG=91.77
          IF(ELEV.EQ.0) ELEV=10.
        CASE(908)
          IF(TLAT.EQ.0) TLAT=37.74
          IF(TLONG.EQ.0)TLONG=88.54
          IF(ELEV.EQ.0) ELEV=4.
        CASE(701)
          IF(TLAT.EQ.0) TLAT=35.60
          IF(TLONG.EQ.0)TLONG=82.55
          IF(ELEV.EQ.0) ELEV=25.
          
      END SELECT
      RETURN
      END
C