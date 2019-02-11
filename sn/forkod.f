      SUBROUTINE FORKOD
      IMPLICIT NONE
C----------
C SN $ID: FORKOD.F 2591 2018-12-14 18:21:52Z TCOURTER $
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
      LOGICAL FORFOUND/.FALSE./


C  PROCLAIMED NATIONAL FOREST  DISTRICT     REGION/FOREST/DISTRICT CODE
C  NATIONAL FORESTS IN ALABAMA BANKHEAD             80101
C         CONECUH                                   80103
C         OAKMULGEE                                 80104
C         SHOAL CREEK                               80105
C         TALLEDEGA                                 80106
C         TUSKEGEE                                  80107
C  DANIEL BOONE  MOREHEAD                           80211
C         STANTON                                   80212
C         BEREA                                     80213
C         LONDON                                    80214
C         SOMERSET                                  80215
C         STEARNS                                   80216
C         REDBIRD                                   80217
C  CHATTAHOOCHEE-OCONEE ARMUCHEE                    80301
C         TOCCOA                                    80302
C         BRASSTOWN                                 80304
C         TALLUAH                                   80305
C         CHATTOOGA                                 80306
C         COHUTTA                                   80307
C         OCONEE                                    80308
C  CHEROKEE      HIWASSEE                           80401
C         NOLICHUCKY                                80402
C         OCOEE                                     80403
C         TELLICO                                   80404
C         UNAKA                                     80405
C         WATUGA                                    80406
C  NATIONAL FORESTS IN FLORIDA APALACHICOLA         80501
C         LAKE GEORGE                               80502
C         OSCEOLA                                   80504
C         SEMINOLE                                  80505
C         WAKULLA                                   80506
C  KISATCHIE     CATAHOULA                          80601
C         EVANGELINE/VERNON                         80602
C         KISATCHIE                                 80603
C         WINN                                      80604
C         CANEY                                     80605
C  NATIONAL FORESTS IN MISS. BIENVILLE              80701
C         DESOTO                                    80702
C         HOMOCHITTO                                80704
C         CHICKASAWHAY                              80705
C         DELTA                                     80706
C         HOLLY SPRINGS                             80707
C         TOMBIGBEE                                 80717
C   GEORGE WASHINGTON/JEFFERSON NFS DEERFIELD       80801
C         DRY RIVER                                 80802
C         JAMES RIVER                               80803
C         LEE                                       80804
C         PEDLAR                                    80805
C         WARM SPRINGS                              80806
C         BLACKSBURG                                80811
C         CLINCH                                    80812
C         GLENWOOD                                  80813
C         MT. ROGERS                                80814
C         NEW CASTLE                                80815
C         WYTHE                                     80816
C  QUACHITA      CHOCTAW                            80901
C         CADDO                                     80902
C         COLD SPRINGS                              80903
C         FOURCHE                                   80904
C         JESSIEVILLE                               80905
C         KIAMICHI                                  80906
C         MENA                                      80907
C         ODEN                                      80908
C         POTEAU                                    80909
C         WOMBLE                                    80910
C         WINONA                                    80911
C         TIAK                                      80912
C  OZARK & ST. FRANCIS NFS     SYLAMORE             81001
C         BUFFALO                                   81002
C         BAYOU                                     81003
C         PLEASANT HILL                             81004
C         BOSTON MOUNTAIN                           81005
C         MAGAZINE                                  81006
C         ST. FRANCIS                               81007
C  NATIONAL FORESTS IN NORTH CAROLINA CHEOAH        81102
C         CROATAN                                   81103
C         APPALACHIAN (OLD)                         81104
C         GRANDFATHER                               81105
C         HIGHLANDS                                 81106
C         PISGAH                                    81107
C         APPALACHIAN                               81108
C         TUSQUITEE                                 81109
C         UWHARRIE                                  81110
C         WAYSH                                     81111
C  FRANCIS MARION & SUMTER NFS ENOREE/TYGER         81201
C         ANDREW PICKENS                            81202
C         LONG CANE                                 81203
C         WAMBAW/WITHERBEE                          81205
C  NATIONAL FORESTS IN TEXAS   ANGELINA             81301
C         DAVY CROCKETT                             81303
C         SAM HOUSTON                               81304
C         SABINE                                    81307
C         CADDO/LBJ                                 81308
C
C  MARK TWAIN NATIONAL FOREST                       905
C
C  SHAWNEE NATIONAL FOREST                          908
C
C  SAVANNAH RIVER (ADMINISTRATIVE)                  836 (MAP TO SUMTER)
C  SAVANNAH RIVER (PROCLAIMED)                      824 (MAP TO SUMTER)
C  LAND BETWEEN THE LAKES (ADMINISTRATIVE)          860 (MAP TO DANIEL BOONE)
C  LAND BETWEEN THE LAKES (PROCLAIMED)              835 (MAP TO DANIEL BOONE)
C
C  DEPT OF DEFENSE, FORT BRAGG                      701
C----------
C  ------------------------
C  RESERVATION PSUEDO CODES:
C
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


C----------------------------------------------------------------------
C  DETERMINE IF KODFOR IS STANDARD FIVE DIGIT REGION/FOREST/DISTRICT CODE
C  FOR REGION 8; EX. 80101. ESTABLISH IFORDI AS THREE DIGIT FOREST CODE
C  FOR LOGICAL COMPARISON
C----------------------------------------------------------------------
      IFORDI = KODFOR/100

      IF (IFORDI.EQ.905 .OR. IFORDI.EQ.908 .OR.
     &    IFORDI.EQ.836 .OR. IFORDI.EQ.824 .OR.
     &    IFORDI.EQ.860 .OR. IFORDI.EQ.835 .OR.
     &    IFORDI .EQ. 701) KODFOR=IFORDI



C----------------------------------------------------------------------
C  INTIAL KODFOR EVALUATION FOR DISCRETE GEOGRAPHIES AND RE-MAPPING
C----------------------------------------------------------------------
      SELECT CASE (KODFOR)
C----------------------------------------------------------------------
C  HANDLE SAVANNAH RIVER
C----------------------------------------------------------------------
        CASE (836,824)
          KODFOR = 81203
          IFORDI = 812
          WRITE(JOSTND,21)
   21     FORMAT(T12,'SAVANNAH RIVER BEING MAPPED TO SUMTER NF ',
     &    '(81203) FOR FURTHER PROCESSING.')
C----------------------------------------------------------------------
C  HANDLE LAND BETWEEN THE LAKES
C----------------------------------------------------------------------
        CASE (860,835)
          KODFOR = 80216
          IFORDI = 802
          WRITE(JOSTND,22)
   22     FORMAT(T12,'LAND BETWEEN THE LAKES BEING MAPPED TO DANIEL ',
     &    'BOONE (80216) FOR FURTHER PROCESSING.')

C----------------------------------------------------------------------
C  HANDLE RESERVATION PSUEDO CODES
C----------------------------------------------------------------------
        CASE (7201)
          WRITE(JOSTND,62)
   62     FORMAT(T12,'ALABAMA-COUSHATTA RES. (7201) BEING MAPPED ',
     &    'TO 81304 TEXAS NF SAM HOUSTON DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81304
          IFORDI = 813
        CASE (7207)
          WRITE(JOSTND,63)
   63     FORMAT(T12,'KIOWA-COMANCHE-APACHE-FORT SILL APACHE OTSA ',
     &    '(7207) BEING MAPPED TO 80906 OUACHITA NF KIAMICHI DIST ',
     &    'FOR FURTHER PROCESSING.')
          KODFOR = 80906
          IFORDI = 809
        CASE (7210)
          WRITE(JOSTND,64)
   64     FORMAT(T12,'KAW OTSA (7210) BEING MAPPED ',
     &    'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7211)
          WRITE(JOSTND,65)
   65     FORMAT(T12,'OTOE-MISSOURIA OTSA (7211) BEING MAPPED ',
     &    'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7212)
          WRITE(JOSTND,66)
   66     FORMAT(T12,'PAWNEE OTSA (7212) BEING MAPPED ',
     &    'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7213)
          WRITE(JOSTND,67)
   67     FORMAT(T12,'PONCA OTSA (7213) BEING MAPPED ',
     &    'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7215)
          WRITE(JOSTND,68)
   68     FORMAT(T12,'CITIZEN POTAWATOMI NATION-ABSENTEE SHAWNEE ',
     &    'OTSA (7215) BEING MAPPED TO 80901 OUACHITA NF CHOCTAW ',
     &    'DIST FOR FURTHER PROCESSING.')
          KODFOR = 80901
          IFORDI = 809
        CASE (7216)
          WRITE(JOSTND,69)
   69     FORMAT(T12,'IOWA OTSA (7216) BEING MAPPED TO 80901 ',
     &    'OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.')
          KODFOR = 80901
          IFORDI = 809
        CASE (7218)
          WRITE(JOSTND,70)
   70     FORMAT(T12,'SAC AND FOX OTSA (7218) BEING MAPPED TO ',
     &    '80901 OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.')
          KODFOR = 80901
          IFORDI = 809
        CASE (7601)
          WRITE(JOSTND,71)
   71     FORMAT(T12,'CHICKASAW OTSA (7601) BEING MAPPED TO 80906 ',
     &    'OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.')
          KODFOR = 80906
          IFORDI = 809
        CASE (7602)
          WRITE(JOSTND,72)
   72     FORMAT(T12,'QUAPAW OTSA (7602) BEING MAPPED TO 81005 ',
     &    'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7603)
          WRITE(JOSTND,73)
   73     FORMAT(T12,'EASTERN SHAWNEE OTSA (7603) BEING MAPPED ',
     &    'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7604)
          WRITE(JOSTND,74)
   74     FORMAT(T12,'SENECA-CAYUGA OTSA (7604) BEING MAPPED TO ',
     &    '81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7605)
          WRITE(JOSTND,75)
   75     FORMAT(T12,'WYANDOTTE OTSA (7605) BEING MAPPED TO 81005 ',
     &    'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7606)
          WRITE(JOSTND,76)
   76     FORMAT(T12,'MIAMI OTSA (7606) BEING MAPPED TO 81005 ',
     &    'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7607)
          WRITE(JOSTND,77)
   77     FORMAT(T12,'PEORIA OTSA (7607) BEING ',
     &    'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &    'FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7608)
          WRITE(JOSTND,78)
   78     FORMAT(T12,'MODOC OTSA (7608) BEING ',
     &    'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &    'FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7609)
          WRITE(JOSTND,79)
   79     FORMAT(T12,'OSAGE RES. (7609) BEING ',
     &    'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',
     &    'FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7610)
          WRITE(JOSTND,80)
   80     FORMAT(T12,'CREEK OTSA (7610) BEING ',
     &    'MAPPED TO 80901 OUACHITA NF CHOCTAW DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 80901
          IFORDI = 809
        CASE (7611)
          WRITE(JOSTND,81)
   81     FORMAT(T12,'CHEROKEE OTSA (7611) BEING MAPPED TO 81005 ',
     &    'OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
          KODFOR = 81005
          IFORDI = 810
        CASE (7612)
          WRITE(JOSTND,82)
   82     FORMAT(T12,'CHOCTAW OTSA (7612) BEING MAPPED TO 80906 ',
     &    'OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.')
          KODFOR = 80906
          IFORDI = 809
        CASE (7613)
          WRITE(JOSTND,83)
   83     FORMAT(T12,'SEMINOLE OTSA (7613) BEING MAPPED TO 80901 ',
     &    'OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.')
          KODFOR = 80901
          IFORDI = 809
        CASE (8205)
          WRITE(JOSTND,84)
   84     FORMAT(T12,'MICCOSUKEE RES. (8205) BEING MAPPED TO ',
     &    '80505 FLORIDA NF SEMINOLE DIST FOR FURTHER PROCESSING.')
          KODFOR = 80505
          IFORDI = 805
        CASE (8207)
          WRITE(JOSTND,85)
   85     FORMAT(T12,'POARCH CREEK RES. (8207) BEING MAPPED TO ',
     &    '80103 ALABAMA NF CONECUH DIST FOR FURTHER PROCESSING.')
          KODFOR = 80103
          IFORDI = 801
        CASE (8210)
          WRITE(JOSTND,86)
   86     FORMAT(T12,'CATAWBA RES. (8210) BEING MAPPED TO 81201 ',
     &    'SUMTER NF ENOREE DIST FOR FURTHER PROCESSING.')
          KODFOR = 81201
          IFORDI = 812
        CASE (8212)
          WRITE(JOSTND,87)
   87     FORMAT(T12,'TUNICA-BILOXI RES. (8212) BEING MAPPED TO ',
     &    '80601 KISATCHIE NF CATAHOULA DIST FOR FURTHER '
     &    'PROCESSING.')
          KODFOR = 80601
          IFORDI = 806
        CASE (8213)
          WRITE(JOSTND,88)
   88     FORMAT(T12,'COUSHATTA RES. (8213) BEING MAPPED TO 80602 ',
     &    'KISATCHIE NF CALCASIEU DIST FOR FURTHER PROCESSING.')
          KODFOR = 80602
          IFORDI = 806
        CASE (8219)
          WRITE(JOSTND,89)
   89     FORMAT(T12,'EASTERN CHEROKEE RES. (8219) BEING MAPPED ',
     &    'TO 81111 NORTH CAROLINA NF NANTAHALA DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 81111
          IFORDI = 811
        CASE (8220)
          WRITE(JOSTND,90)
   90     FORMAT(T12,'SEMINOLE (FL) TRUST LAND (8220) BEING ',
     &    'MAPPED TO 80505 FLORIDA NF SEMINOLE DIST FOR FURTHER ',
     &    'PROCESSING.')
          KODFOR = 80505
          IFORDI = 805
        CASE (8221)
          WRITE(JOSTND,91)
   91     FORMAT(T12,'MISSISSIPPI CHOCTAW RES. (8221) BEING ',
     &    'MAPPED TO 80701 MISSISSIPPI NF BIENVILLE DIST FOR ',
     &    'FURTHER PROCESSING.')
          KODFOR = 80701
          IFORDI = 807

      END SELECT


C----------------------------------------------------------------------
C     FINAL EVALUATION AND ASSIGN REGION/FOREST/DISTRICT CODE
C----------------------------------------------------------------------

      SELECT CASE (KODFOR)

C----------------------------------------------------------------------
C       HANDLE REGION 9 FORESTS DIFFERENTLY
C----------------------------------------------------------------------
        CASE (905,908)
          IF(KODFOR .EQ. 905)THEN
            IFOR=14
          ELSEIF(KODFOR .EQ. 908)THEN
            IFOR=15
          ENDIF
          KODIST=1
          ISEFOR=0
          WRITE(JOSTND,11)JFOR(IFOR),KODIST

          KODFOR=(JFOR(IFOR)*100)+KODIST

C----------------------------------------------------------------------
C       HANDLE FORT BRAGG MILITARY INSTALLATION
C----------------------------------------------------------------------
        CASE (701)

          WRITE(JOSTND,23)
   23     FORMAT(T12,'FORT BRAGG BEING MAPPED TO NFS IN NC, UWHARRIE ',
     &    'DISTRICT (81110) FOR FURTHER PROCESSING.')
          IFOR=20
          KODIST=10
          ISEFOR=701

          KODFOR=81110

C----------------------------------------------------------------------
C       STANDARD REGION 8 REGION/FOREST/DISTRICT CODES
C----------------------------------------------------------------------
        CASE DEFAULT
          DO 10 I=1,NUMFOR
          	
            IF (IFORDI .EQ. JFOR(I)) THEN

C----------------------------------------------------------------------
C             DEFINE DISTRICT CODES (KODIST)
C----------------------------------------------------------------------
              KODIST = KODFOR-(IFORDI*100)
              IF (JFOR(I) .EQ. 801) THEN
                IF(KODIST.LT.1.OR.KODIST.EQ.2.OR.KODIST.GT.7)THEN
                  KODIST=3
              ENDIF
              ELSEIF (JFOR(I) .EQ. 802) THEN
                IF(KODIST.LT.11.OR.KODIST.EQ.16.OR.KODIST.GT.17)THEN
                KODIST=11
                ENDIF
              ELSEIF (JFOR(I) .EQ. 803) THEN
                IF(KODIST.LT.1.OR.KODIST.EQ.3.OR.KODIST.GT.8)THEN
                  KODIST=8
                ENDIF
              ELSEIF(JFOR(I) .EQ. 804)THEN
                IF(KODIST.LT.1.OR.KODIST.GT.6) KODIST=1
              ELSEIF(JFOR(I) .EQ. 805)THEN
                IF(KODIST.LT.1.OR.KODIST.EQ.3.OR.KODIST.GT.6)THEN
                  KODIST=4
                ENDIF
              ELSEIF (JFOR(I) .EQ. 806) THEN
                IF(KODIST.LT.1.OR.KODIST.GT.5)KODIST=3
              ELSEIF(JFOR(I) .EQ. 807)THEN
                IF(KODIST.LT.1.OR.KODIST.EQ.5.OR.(KODIST.GT.7.AND.
     &             KODIST.LT.17).OR.KODIST.GT.17)KODIST=7
              ELSEIF (JFOR(I) .EQ. 808) THEN
                IF(KODIST.LT.1.OR.(KODIST.GE.7.AND.KODIST.LE.10).OR.
     &             KODIST.GT.16)KODIST=16
              ELSEIF (JFOR(I) .EQ. 809) THEN
                IF(KODIST.LT.1.OR.KODIST.GT.12)KODIST=7
              ELSEIF (JFOR(I) .EQ. 810) THEN
                IF(KODIST.LT.1.OR.KODIST.GT.7)KODIST=5
              ELSEIF(JFOR(I) .EQ. 811)THEN
                IF(KODIST.LT.2.OR.KODIST.GT.11)KODIST=5
              ELSEIF(JFOR(I) .EQ. 812)THEN
                IF(KODIST.LT.1.OR.KODIST.EQ.4.OR.KODIST.GT.5)THEN
                  KODIST=1
                ENDIF
              ELSE
                IF(KODIST.LT.1.OR.KODIST.EQ.2.OR.KODIST.EQ.5 .OR.
     &             KODIST.EQ.6.OR.KODIST.GT.8)KODIST=3
              ENDIF

              IFOR = I
              ISEFOR=JFOR(IFOR)
              KODFOR=(JFOR(IFOR)*100) + KODIST
              FORFOUND = .TRUE.

              EXIT

            ENDIF

   10     CONTINUE

C----------------------------------------------------------------------
C         DEFAULT ERROR TRAP ASSIGN Talledega NF ALABAMA 80106
C----------------------------------------------------------------------
          IF (.NOT. FORFOUND) THEN
            CALL ERRGRO (.TRUE.,3)
            WRITE(JOSTND,11) JFOR(IFOR),KODIST
   11       FORMAT(T12,'FOREST CODE USED FOR THIS PROJECTION IS',I4,/,
     &      T12,'DISTRICT USED FOR THIS PROJECTION IS',I3)
          ENDIF

      END SELECT


C----------------------------------------------------------------------
C  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
C----------------------------------------------------------------------
      SELECT CASE(JFOR(IFOR))
        CASE(801)
          IF(TLAT.EQ.0)  TLAT  = 32.37
          IF(TLONG.EQ.0) TLONG = 86.30
          IF(ELEV.EQ.0)  ELEV  = 7.
        CASE(802) 
          IF(TLAT.EQ.0)  TLAT  = 37.99
          IF(TLONG.EQ.0) TLONG = 84.18
          IF(ELEV.EQ.0)  ELEV  = 12.
        CASE(803) 
          IF(TLAT.EQ.0)  TLAT  = 34.30
          IF(TLONG.EQ.0) TLONG = 83.82
          IF(ELEV.EQ.0)  ELEV  = 17.
        CASE(804) 
          IF(TLAT.EQ.0)  TLAT  = 35.16
          IF(TLONG.EQ.0) TLONG = 84.88
          IF(ELEV.EQ.0)  ELEV  = 22.
        CASE(805) 
          IF(TLAT.EQ.0)  TLAT  = 30.44
          IF(TLONG.EQ.0) TLONG = 84.28
          IF(ELEV.EQ.0)  ELEV  = 1.
        CASE(806) 
          IF(TLAT.EQ.0)  TLAT  = 31.32
          IF(TLONG.EQ.0) TLONG = 92.43
          IF(ELEV.EQ.0)  ELEV  = 2.
        CASE(807) 
          IF(TLAT.EQ.0)  TLAT  = 33.31
          IF(TLONG.EQ.0) TLONG = 89.17
          IF(ELEV.EQ.0)  ELEV  = 3.
        CASE(808) 
          IF(TLAT.EQ.0)  TLAT  = 37.27
          IF(TLONG.EQ.0) TLONG = 79.94
          IF(ELEV.EQ.0)  ELEV  = 21.
        CASE(809) 
          IF(TLAT.EQ.0)  TLAT  = 34.50
          IF(TLONG.EQ.0) TLONG = 93.06
          IF(ELEV.EQ.0)  ELEV  = 9.
        CASE(810) 
          IF(TLAT.EQ.0)  TLAT  = 35.28
          IF(TLONG.EQ.0) TLONG = 93.13
          IF(ELEV.EQ.0)  ELEV  = 13.
        CASE(811) 
          IF(TLAT.EQ.0)  TLAT  = 35.60
          IF(TLONG.EQ.0) TLONG = 82.55
          IF(ELEV.EQ.0)  ELEV  = 25.
        CASE(812) 
          IF(TLAT.EQ.0)  TLAT  = 34.00
          IF(TLONG.EQ.0) TLONG = 81.04
          IF(ELEV.EQ.0)  ELEV  = 4.
        CASE(813) 
          IF(TLAT.EQ.0)  TLAT  = 31.34
          IF(TLONG.EQ.0) TLONG = 94.73
          IF(ELEV.EQ.0)  ELEV  = 3.
        CASE(905) 
          IF(TLAT.EQ.0)  TLAT  = 37.95
          IF(TLONG.EQ.0) TLONG = 91.77
          IF(ELEV.EQ.0)  ELEV  = 10.
        CASE(908) 
          IF(TLAT.EQ.0)  TLAT  = 37.74
          IF(TLONG.EQ.0) TLONG = 88.54
          IF(ELEV.EQ.0)  ELEV  = 4.
        CASE(701) 
          IF(TLAT.EQ.0)  TLAT  = 35.60
          IF(TLONG.EQ.0) TLONG = 82.55
          IF(ELEV.EQ.0)  ELEV  = 25.

      END SELECT
      RETURN
      END