!== last modified  08-08-2011
      SUBROUTINE VOLEQDEF (VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

C    SUBROUTINE WILL RETURN THE DEFAULT VOLUME EQUATION NUMBER
C        SPEC = 3 DIGIT FIA SPECIES CODE

      CHARACTER*2 FORST,PROD,DIST,VAR
      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG,REGN,SPEC

      IF(VAR .EQ. '  ') THEN
         CALL GETVARIANT(REGN,FORST,DIST,VAR)
      ENDIF
      IF(REGN .EQ. 1) THEN
         CALL R1_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.2)THEN
         CALL R2_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.3)THEN
         CALL R3_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.4)THEN
         CALL R4_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.5)THEN
         CALL R5_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.6)THEN
         CALL R6_EQN(VAR,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.7)THEN
         CALL R7_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'02'))THEN
         CALL R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'01'))THEN
         CALL R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.9)THEN
         CALL R9_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.10)THEN
         CALL R10_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      ENDIF

      RETURN      
      END

C//////////////////////////////////////////////////////////////////
      SUBROUTINE GETVARIANT(REGN,FORST,DIST,VAR)
      CHARACTER*2 FORST,DIST,VAR
      INTEGER REGN,FORNUM,DISTNUM
      
      IF(FORST(2:2) .LT. '0') THEN 
        FORST(2:2) = FORST(1:1)
        FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      IF(DIST(2:2) .LT. '0') THEN
        DIST(2:2) = DIST(1:1)
        DIST(1:1) = '0'
        IF(DIST(2:2) .LT. '0') DIST(2:2) = '0'
      ENDIF
      READ(FORST,'(I2)')FORNUM
      READ(DIST,'(I2)')DISTNUM

      IF(REGN .EQ. 8)THEN
         VAR = 'SN'

      ELSE IF(REGN.EQ.1)THEN
         IF(FORNUM.EQ.4 .OR. FORNUM.EQ.5 .OR. FORNUM.EQ.17 .OR. 
     >      FORNUM.EQ.3 .OR. FORNUM.EQ.14 .OR. FORNUM.EQ.16)THEN
            VAR = 'IE'
         ELSE
            VAR = 'EM'
         ENDIF

      ELSE IF(REGN.EQ.5) THEN
C       DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
C       INLAND CALIFORNIA
         IF(FORNUM.EQ.5 .OR. FORNUM.EQ.6 .OR. FORNUM.EQ.8 .OR. 
     >      FORNUM.EQ.11 .OR. FORNUM.EQ.14) THEN
            VAR = 'CA'
C        SOUTHERN OREGON
         ELSEIF(FORNUM.EQ.9) THEN
            VAR = 'SO'
C        WESTERN SIERRA
         ELSEIF(FORNUM.EQ.17 .OR. FORNUM.EQ.16 .OR. FORNUM.EQ.15 .OR. 
     >          FORNUM.EQ.13 .OR. FORNUM.EQ.3) THEN
            VAR = 'WS'
C        KLAMATH/NORTHERN CALIFORNIA
         ELSEIF(FORNUM.EQ.5) THEN
            VAR = 'NC'
         ENDIF

      ELSE IF(REGN.EQ.6) THEN
C        DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
C        BLUE MTN VARIANT
         IF(FORNUM.EQ.4 .OR. FORNUM.EQ.7 .OR. FORNUM.EQ.14 .OR. 
     >      FORNUM.EQ.16) THEN
            VAR = 'BM'
C        EASTERN CASCADES
         ELSEIF(FORNUM.EQ.17 .OR. FORNUM.EQ.8 .OR. (FORNUM.EQ.3 .AND. 
     >          DISTNUM .EQ. 3) .OR. (FORNUM.EQ.6 .AND. 
     >         (DISTNUM.EQ.1.OR.DISTNUM.EQ.2.OR.DISTNUM.EQ.6))) THEN
C           MOUNT HOOD Barlow RD
            VAR = 'EC'
C        SOUTHERN OREGON
         ELSEIF(FORNUM.EQ.1 .OR. FORNUM.EQ. 2 .OR. FORNUM.EQ.20) THEN
            VAR = 'SO'
C        WESTERN CASCADES
         ELSEIF(FORNUM.EQ.5 .OR. FORNUM.EQ.15 .OR. FORNUM.EQ.18 .OR. 
     >          FORNUM.EQ.10 .OR. FORNUM.EQ.3 .OR. FORNUM.EQ.6) THEN
            VAR = 'WC'
C        PACFIC NORTHWEST
         ELSEIF(FORNUM.EQ.9 .OR. FORNUM.EQ.12) THEN
            VAR = 'PN'
C        NORTHERN CALIFORNIA
         ELSEIF(FORNUM.EQ.11) THEN
            VAR = 'NC'
         ELSEIF(FORNUM.EQ.21) THEN
            VAR = 'IE'
         ENDIF

      ELSE IF(REGN.EQ.7) THEN
C     DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
         IF(FORNUM.EQ.2) THEN
            VAR = 'WC'
         ELSEIF(FORNUM.EQ.3) THEN
            VAR = 'NC'
         ELSE
            VAR = 'SO'
         ENDIF

      ELSE IF (REGN .EQ. 9) THEN
         IF(FORNUM.EQ.13 .OR. FORNUM.EQ.10 .OR. FORNUM.EQ.3 .OR. 
     >        FORNUM.EQ.9 .OR. FORNUM.EQ.4 .OR. FORNUM.EQ.7 .OR. 
     >        FORNUM.EQ.2 .OR. FORNUM.EQ.6)THEN
            VAR = 'LS'
         ELSE IF(FORNUM.EQ.12 .OR. FORNUM.EQ.8 .OR. FORNUM.EQ.5)THEN
            VAR = 'CS'
         ELSE IF(FORNUM.EQ.21 .OR. FORNUM.EQ.20 .OR. FORNUM.EQ.19 .OR. 
     >           FORNUM.EQ.14 .OR. FORNUM.EQ.22)THEN
            VAR = 'NE'
         ENDIF

      ENDIF

      RETURN
      END

C//////////////////////////////////////////////////////////////////
      SUBROUTINE R1_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
C      SEARCH BY FIA SPECIES CODE
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST,VAR
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(39)
      INTEGER FIA(38), FIRST, HALF, LAST, DONE,FORNUM,I
C SPECIES
C Balsam fir,           White fir,           Grand fir,       Subalpine fir,   Western juniper,
C Rocky Mt Juniper,     Subalpine larch,     Western larch,   Engelmann spruce, White spruce,
C Blue spruce,          Whitebark pine,      Bristlecone pine,Pinyon Pine,      Lodgepole pine,
C Limber pine,          Western white pine,  Ponderosa pine,  Singleleaf pinyon,border pinyon,
C Douglas fir,          Pacific yew,         Western redcedar,Western hemlock,  Mountain hemlock,
C Other Softwoods,      Rocky Mountain maple,Paper birch,     curlleaf mtn-mahog,green ash,       
C Cottonwood,           balsam poplar,       plains cottonwood,Quaking aspen,  black cottonwood,
C Narrowleaf cottonwood,Other hardwood,      Unknown
          
      DATA (FIA(I),I=1,38)/ 12,   15,   17,   19,   64,
     >                      66,   72,   73,   93,   94,   
     >                      96,  101,  102,  106,  108,  
     >                     113,  119,  122,  133,  134,    
     >                     202,  231,  242,  263,  264,    
     >                     298,  321,  375,  475,  544,  
     >                     740,  741,  745,  746,  747,  
     >                     749,  998,  999/
     
     
      DATA (EQNUM(I),I=1,39)/
     >'I00FW2W012','102DVEW017','I00FW2W017','I00FW2W019','102DVEW060',
     >'102DVEW106','I00FW2W019','I00FW2W073','I00FW2W093','102DVEW090',
     >'102DVEW090','I00FW2W119','102DVEW108','102DVEW106','I00FW2W108',
     >'I00FW2W073','I00FW2W119','I00FW2W122','102DVEW106','102DVEW106',
     >'I00FW2W202','616BEHW231','I00FW2W242','I00FW2W260','I00FW2W260',
     >'I00FW2W260','200DVEW746','101DVEW375','400DVEW475','101DVEW740',
     >'101DVEW740','101DVEW740','101DVEW740','200DVEW746','101DVEW740',
     >'101DVEW740','200DVEW746','I00FW2W260',
     >'203FW2W122'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,39
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
        RETURN
      ENDIF
C
      LAST = 38
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      IF(SPEC.EQ.122 .AND. FORNUM.EQ.8) THEN
         VOLEQ = EQNUM(38)
      ELSEIF(SPEC.EQ.101 .AND. VAR.EQ.'EM')THEN
         VOLEQ = 'I00FW2W012'
      ELSE
         FIRST = 1

         DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
            IF(FIA(HALF) .EQ. SPEC)THEN
               DONE = HALF
            ELSEIF(FIRST .EQ. LAST) THEN
               ERRFLAG = 1
               DONE = -1
            ELSE IF (FIA(HALF) .LT. SPEC) THEN
               FIRST = HALF
            ELSE
               LAST = HALF - 1
            ENDIF
  5      CONTINUE 
      
         IF(DONE .LT. 0)THEN
            IF(SPEC.LT.300)THEN
C              Other Softwood
               VOLEQ = EQNUM(26)
            ELSE
C              Other Hardwood
               VOLEQ = EQNUM(36)
            ENDIF
         ELSE
            VOLEQ = EQNUM(DONE)   
         ENDIF
      ENDIF

      RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R2_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(45)
      INTEGER FIA(42), FIRST, HALF, LAST, DONE ,FORNUM,I
C     SPECIES
C     White fir,        Grand fir,         Corkbark fir,      Subalpine fir,        Juniper,
C     Alligator juniper,Utah Juniper,      Rocky Mtn. Juniper,Eastern redcedar,     Oneseed Juniper,        
C     Western Larch,    Engelmann's spruce,White spruce,      Blue spruce,          Whitebark pine,         
C     Bristlecode pine, Pinyon Pine,       Lodgepole pine,    Limber pine,          Southwestern white pine,
C     Chihuahua pine,   Ponderosa pine,    Singleleaf pinyon, Border pinyon,        Arizona pinyon,
C     Douglas fir,      Western Redcedar,  Mountain Hemlock,  Other softwoods,      Paper birch,
C     Mountain Mahogany,Cottonwoods,       Plains cottonwood, Quaking aspen,        Narrowleaf cottonwood,
C     Oak,              Arizona white oak, Emory Oak,         Gambel Oak,           Bur Oak,
C     Silverleaf oak,   Other Hardwoods

      DATA (FIA(I),I=1,42)/ 15,  17,  18,  19,  57,
     >                      63,  65,  66,  68,  69,
     >                      73,  93,  94,  96, 101, 
     >                     102, 106, 108, 113, 114, 
     >                     118, 122, 133, 134, 143, 
     >                     202, 242, 264, 298, 375,
     >                     475, 740, 745, 746, 749,
     >                     800, 803, 810, 814, 823,
     >                     843, 998/
   
      DATA (EQNUM(I),I=1,45)/
     >'200FW2W015','I00FW2W019','I00FW2W019','I00FW2W019','300DVEW060',
     >'300DVEW060','200DVEW065','300DVEW060','300DVEW060','200DVEW069',
     >'407FW2W093','407FW2W093','407FW2W093','407FW2W093','200FW2W122',
     >'200FW2W122','300DVEW106','200FW2W108','200FW2W122','200FW2W122',
     >'200FW2W122','200FW2W122','300DVEW106','300DVEW106','300DVEW106',
     >'200FW2W202','407FW2W093','407FW2W093','300DVEW060','300DVEW999',
     >'200DVEW475','300DVEW999','300DVEW999','200FW2W746','300DVEW999',
     >'300DVEW800','300DVEW800','300DVEW800','300DVEW800','200DVEW823',
     >'300DVEW800','200DVEW998','203FW2W122','213FW2W122','202FW2W108'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,45
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      LAST = 42
   
      IF(SPEC.EQ.122 .AND. FORNUM.EQ.3) THEN
         VOLEQ = EQNUM(43)
      ELSE IF(SPEC.EQ.122 .AND. FORNUM.EQ.13) THEN
         VOLEQ = EQNUM(44)
      ELSE IF(SPEC.EQ.108 .AND. FORNUM.EQ.2) THEN
         VOLEQ = EQNUM(45)
      ELSE
         FIRST = 1

          DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(FIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
                ELSE IF (FIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  5      CONTINUE 
         IF(DONE .LT. 0)THEN
C           Other Hardwood
             VOLEQ = EQNUM(42)
             ELSE
            VOLEQ = EQNUM(DONE)   
          ENDIF
       ENDIF

       RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R3_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(47)
      INTEGER FIA(45), FIRST, HALF, LAST, DONE, FORNUM,I

C     SPECIES
C     White fir,              Grand fir,        Corkbark fir,         Subalpine fir,     Juniper,
C     Juniper,                Alligator juniper,Utah juniper,         Rocky Mtn. Juniper,Eastern redcedar
C     Oneseed juniper,        Western Larch,    Engelmann's spruce,   White spruce,      Blue spruce,
C     Whitebark pine,         Bristlecode pine, Pinyon Pine,          Lodgepole pine,    Limber pine,
C     Southwestern white pine,Chihuahua pine,   Ponderosa pine,       Singleleaf pinyon, Border pinyon,
C     Arizona pinyon,         Douglas fir,      Western Redcedar,     Mountain Hemlock,  Other softwoods,
C     Maple,                  Black maple,      Paper Birch,          Mountain Mahogany, Cottonwoods,
C     Plains cottonwood,      Quaking aspen,    Narrowleaf cottonwood,Oak,               Arizona white oak, 
C     Emory oak,              Gambel oak,       Bur oak,              Silverleaf oak,    Other Hardwoods

 
      DATA (FIA(I),I=1,45)/ 15,  17,  18,  19,  57,   
     >                      60,  63,  65,  66,  68,
     >                      69,  73,  93,  94,  96, 
     >                     101, 102, 106, 108, 113,
     >                     114, 118, 122, 133, 134, 
     >                     143, 202, 242, 264, 298,  
     >                     310, 314, 375, 475, 740, 
     >                     745, 746, 749, 800, 803, 
     >                     810, 814, 823, 843, 998/

      DATA (EQNUM(I),I=1,47)/
     >'301DVEW015','300DVEW093','300DVEW093','300DVEW093','300DVEW060',
     >'300DVEW060','300DVEW060','300DVEW060','300DVEW060','300DVEW060',
     >'300DVEW060','301DVEW015','300DVEW093','300DVEW093','300DVEW093',
     >'300DVEW113','300DVEW113','300DVEW106','301DVEW202','300DVEW113',
     >'300DVEW113','300DVEW122','300DVEW122','300DVEW106','300DVEW106',
     >'300DVEW106','301DVEW202','301DVEW015','301DVEW015','300DVEW060',
     >'300DVEW310','300DVEW314','300DVEW999','300DVEW999','300DVEW999',
     >'300DVEW999','300DVEW746','300DVEW999','300DVEW800','300DVEW800',
     >'300DVEW800','300DVEW800','300DVEW800','300DVEW800','300DVEW999',
     >'302DVEW202','302DVEW202'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,47
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      LAST = 45
   
      IF(SPEC.EQ.202.AND.(FORNUM.EQ.2.OR.FORNUM.EQ.3.OR.
     >                    FORNUM.EQ.7.OR.FORNUM.EQ.10)) THEN
         VOLEQ = EQNUM(46)
      ELSEIF(SPEC.EQ.15.AND.(FORNUM.EQ.2.OR.FORNUM.EQ.3.OR.
     >                       FORNUM.EQ.7.OR.FORNUM.EQ.10)) THEN
         VOLEQ = EQNUM(47)
      ELSE
         FIRST = 1

          DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(FIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
                ELSE IF (FIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  5      CONTINUE 
      
         IF(DONE .LT. 0)THEN
C           Unknown
             VOLEQ = EQNUM(45)
             ELSE
            VOLEQ = EQNUM(DONE)   
          ENDIF
       ENDIF

       RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R4_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(48)
      INTEGER FIA(27), FIRST, HALF, LAST, DONE, FORNUM,I

C SPECIES
C California red fir,    Juniper,          Western juniper,      Rocky Mtn juniper,    Western larch,
C Incense-cedar,         Common pinyon,    Sugar pine,           Western white pine,   Singleleaf pinyon,
C Pacific yew,           Western redcedar, Western hemlock,      Mountain hemlock,     Other softwoods,
C Box elder,             Rocky Mtn maple,  Bigtooth maple,       Curlleaf Mtn Mahogany,Quaking aspen,    
C Black cottonwood,      Fremont cottonwod,Narrowleaf cottonwood,Oak (sp.)             Gambel oak,
C Other hardwoods,       Other

      DATA (FIA(I),I=1,27)/   20,   60,   64,   66,   73,  
     >                        81,  106,  117,  119,  133,   
     >                       231,  242,  263,  264,  298,  
     >                       313,  321,  322,  475,  746,    
     >                       747,  748,  749,  800,  814,
     >                       998,  999/

      DATA (EQNUM(I),I=1,48)/
     >'400MATW020','300DVEW060','300DVEW060','400DVEW066','400MATW073',
     >'400MATW081','300DVEW106','400MATW117','400MATW117','400DVEW133',
     >'400DVEW998','400MATW081','400MATW015','401MATW015','400MATW108',
     >'400MATW108','400MATW108','400MATW108','400DVEW475','400MATW746',
     >'400DVEW998','400MATW108','400MATW108','300DVEW800','300DVEW800',
     >'400MATW108','400MATW108',
     >'I15FW2W017','401MATW015','400MATW015','I15FW2W017','405MATW019',
     >'400MATW019','401DVEW065','400DVEW065','I15FW2W093','407FW2W093',
     >'400MATW093','401MATW108','400MATW108','I15FW2W122','401MATW122',
     >'402MATW122','403MATW122','400MATW122','I15FW2W202','405MATW202',
     >'400MATW202'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,48
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C      
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      LAST = 27
 
C     White fir
      IF(SPEC.EQ.15.) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13.OR.
     >                                               FORNUM.EQ.6)THEN
            VOLEQ = EQNUM(28)
         ELSEIF (FORNUM.EQ.9.OR.FORNUM.EQ.17)THEN
            VOLEQ = EQNUM(29)
          ELSE
            VOLEQ = EQNUM(30)
          ENDIF
C     Grand fir
      ELSEIF(SPEC.EQ.17) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13.OR.
     >                                               FORNUM.EQ.6)THEN
            VOLEQ = EQNUM(31)
          ELSE
            VOLEQ = EQNUM(30)
          ENDIF
C     Subalpine fir
      ELSEIF(SPEC.EQ.19)THEN
         IF (FORNUM.EQ.5)THEN
            VOLEQ = EQNUM(32)
          ELSE
            VOLEQ = EQNUM(33)
          ENDIF
C     Utah Juniper
      ELSEIF(SPEC.EQ.65) THEN
         IF (FORNUM.EQ.1.OR.FORNUM.EQ.10) THEN
            VOLEQ = EQNUM(34)
          ELSE
            VOLEQ = EQNUM(35)
          ENDIF
C     Engelmann spruce
C     Blue spruce
      ELSEIF(SPEC.EQ.93.OR.SPEC.EQ.96) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13.OR.
     >                                               FORNUM.EQ.6)THEN
            VOLEQ = EQNUM(36)
         ELSEIF (FORNUM.EQ.7.OR.FORNUM.EQ.8) THEN
            VOLEQ = EQNUM(37)
          ELSE
            VOLEQ = EQNUM(38)
          ENDIF
C     Whitebark pine
C     Lodgepole pine
C     Limber pine
C     Bristlecone pine
      ELSEIF(SPEC.EQ.101.OR.SPEC.EQ.108.OR.SPEC.EQ.113.OR.
     &       SPEC.EQ.142) THEN
         IF (FORNUM.EQ.9.OR.FORNUM.EQ.17) THEN
            VOLEQ = EQNUM(39)
          ELSE
            VOLEQ = EQNUM(40)
          ENDIF
C     Ponderosa pine
      ELSEIF(SPEC.EQ.122) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13.OR.
     >                                               FORNUM.EQ.6)THEN
            VOLEQ = EQNUM(41)
         ELSEIF (FORNUM.EQ.01)THEN
            VOLEQ = EQNUM(42)
         ELSEIF (FORNUM.EQ.7.OR.FORNUM.EQ.8.OR.FORNUM.EQ.10.OR.
     >           FORNUM.EQ.18.OR.FORNUM.EQ.19) THEN
            VOLEQ = EQNUM(43)
         ELSEIF (FORNUM.EQ.9.OR.FORNUM.EQ.17)THEN
            VOLEQ = EQNUM(44)
          ELSE
            VOLEQ = EQNUM(45)
          ENDIF
C     Douglas fir
      ELSEIF(SPEC.EQ.202) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13.OR.
     >                                               FORNUM.EQ.6)THEN
            VOLEQ = EQNUM(46)
         ELSEIF (FORNUM.EQ.05)THEN
            VOLEQ = EQNUM(47)
          ELSE
            VOLEQ = EQNUM(48)
          ENDIF
      ELSE
         FIRST = 1

          DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(FIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
                ELSE IF (FIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  5      CONTINUE 
      
         IF(DONE .LT. 0)THEN
C           Other Hardwood
            VOLEQ = EQNUM(26)
         ELSE
            VOLEQ = EQNUM(DONE)   
         ENDIF
       ENDIF


       RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R5_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST,VAR
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(71)
      INTEGER FIA(71), FIRST, HALF, LAST, DONE, FORNUM,I

C     SPECIES
C     Pacific silver fir,    White fir,             Grand fir,           Subalpine fir,       California red fir,
C     Shasta red fir,        Noble fir,             Port Orford cedar,   California juniper,  Juniper,  
C     Utah juniper,          Western Larch,         Incense cedar,       Brewer spruce,       Engelmann spruce,
C     White bark pine,       Knobcone pine,         Foxtail pine,        Lodgepole pine,      Coulter pine,
C     Limber pine,           Jeffrey pine,          Sugar pine,          Western white pine,  Ponderosa pine,
C     Monterey pine,         Grey pine,             Singleleaf pinyon,   Washoe pine,         Great Basin bristlecone pine
C     Bigcone Douglas-fir,   Douglas-fir,
C     Redwood,               Giant sequoia,         Pacific yew,         Western red cedar,   California nutmeg, 
C     Western hemlock,       Mountain hemlock,      Other softwoods,     Koa,                 Bigleaf maple,
C     California buckeye,    Red alder,             White alder          Pacific madrone,     Golden chinkapin, 
C     Curl-leaf Mtn Mahogany,Birchleaf Mtn Mahogany,Pacific dogwood,     Eucalyptus,          Oregon Ash,
C     Walnut,                Tanoak,                Ohia,                California sycamore, Quacking aspen, 
C     Black cottonwood,      Bitter Cherry,         California live oak, Canyon live oak,     Blue oak, 
C     Engelmann's oak,       Oregon white oak,      California black oak,California white oak,Interior live oak,
C     Willow,                California laurel,     Other hardwoods,     unkown

      DATA (FIA(I),I=1,32)/   11,   15,   17,   19,   20,
     >                        21,   22,   41,   62,   64,
     >                        65,   73,   81,   92,   93,
     >                       101,  103,  104,  108,  109,
     >                       113,  116,  117,  119,  122,
     >                       124,  127,  133,  137,  142,
     >                       201,  202/  
      DATA (FIA(I),I=33,71)/ 211,  212,  231,  242,  251,
     >                       263,  264,  298,  301,  312,
     >                       333,  351,  352,  361,  431,
     >                       475,  478,  492,  510,  542,
     >                       600,  631,  671,  730,  746, 
     >                       747,  768,  801,  805,  807,
     >                       811,  815,  818,  821,  839,
     >                       920,  981,  998,  999/

      DATA (EQNUM(I),I=1,32)/
     >'500WO2W015','500WO2W015','500WO2W015','500WO2W020','500WO2W020',
     >'500WO2W020','500WO2W020','500WO2W081','500DVEW060','500DVEW060',
     >'500DVEW060','500WO2W202','500WO2W081','500WO2W015','500WO2W015',
     >'500WO2W108','500WO2W108','500WO2W108','500WO2W108','500WO2W108',
     >'500WO2W108','500WO2W116','500WO2W117','500WO2W117','500WO2W122',
     >'500WO2W108','500WO2W108','500WO2W116','500WO2W117','500WO2W108',
     >'500WO2W202','500WO2W202'/
      DATA (EQNUM(I),I=33,71)/
     >'500WO2W211','500DVEW212','500WO2W108','500WO2W081','500DVEW807',
     >'500WO2W015','500WO2W015','500WO2W108','H00SN2W301','500DVEW312',
     >'500DVEW807','500DVEW351','500DVEW351','500DVEW361','500DVEW431',
     >'500DVEW801','500DVEW801','500DVEW807','H01SN2W510','500DVEW807',
     >'500DVEW818','500DVEW631','H00SN2W671','500DVEW818','500DVEW818',
     >'500DVEW818','500DVEW801','500DVEW801','500DVEW805','500DVEW807',
     >'500DVEW811','500DVEW815','500DVEW818','500DVEW821','500DVEW839',
     >'500DVEW807','500DVEW981','500DVEW981','500DVEW631'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,71
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      DONE = 0
C     Whitebark pine
      IF(SPEC.EQ.101.) THEN
          IF(VAR.EQ."SO" .OR. VAR.EQ."so") THEN
              VOLEQ = '500WO2W117'
          ELSE
              VOLEQ = '500WO2W108'   
          ENDIF
C     other softwoods
      ELSEIF(SPEC.EQ.298 .OR. SPEC.EQ.290) THEN
          IF(VAR.EQ."SO" .OR. VAR.EQ."so") THEN
              VOLEQ = '500WO2W202'
          ELSEIF(VAR.EQ."NC" .OR. VAR.EQ."nc") THEN
              VOLEQ = '500WO2W202'
           ELSE
              VOLEQ = '500WO2W108'   
          ENDIF
C     other hardwoods
      ELSEIF(SPEC.EQ.998.) THEN
          IF(VAR.EQ."SO" .OR. VAR.EQ."so") THEN
              VOLEQ = '500DVEW801'
          ELSEIF(VAR.EQ."WS" .OR. VAR.EQ."ws") THEN
              VOLEQ = '500DVEW818'
          ELSEIF(VAR.EQ."NC" .OR. VAR.EQ."nc") THEN
              VOLEQ = '500DVEW631'
          ELSE
              VOLEQ = '500DVEW981'   
          ENDIF
      ELSE
          LAST = 71

          FIRST = 1

          DO 5, WHILE (DONE.EQ.0)
              HALF = (LAST - FIRST +1)/2 + FIRST
              IF(FIA(HALF) .EQ. SPEC)THEN
                  DONE = HALF
              ELSEIF(FIRST .EQ. LAST) THEN
                  ERRFLAG = 1
                  DONE = -1
              ELSE IF (FIA(HALF) .LT. SPEC) THEN
                  FIRST = HALF
              ELSE
                  LAST = HALF - 1
              ENDIF
  5       CONTINUE 
      
          IF(DONE .LT. 0) THEN
              VOLEQ = EQNUM(71)
          ELSE
              VOLEQ = EQNUM(DONE)   
          ENDIF
      ENDIF
      
      RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R6_EQN(VAR,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST,VAR,DIST
       CHARACTER*3 ASPEC
      INTEGER SPEC,ERRFLAG,FORNUM,I,FIA(53),DISTNUM
      CHARACTER*10 EQNUMW(54),EQNUME(54),EQNUMI(60)
      CHARACTER*10 EQNUMC(33),EQNUMF(27),EQNUMD(8)
      INTEGER FIRST, HALF, LAST, DONEI, DONEF

C     All species 32' logs, All species 16' logs,
C     Balsam fir,      Grand Fir,  Alpine Fir,       Western Larch, Engelmann spruce
C     Lodgepole pine,  White Pine, Ponderosa pine,   Douglas Fir,   Western red cedar,
C     Mountain Hemlock,Douglas fir,Western red cedar,Mountain Hemlock

      DATA (FIA(I),I=1,53)/  011, 015, 017, 019, 020,
     >                       021, 022, 042, 064, 066,
     >                       072, 073, 081, 093, 098,
     >                       101, 103, 106, 108, 113,
     >                       116, 117, 119, 122, 202,
     >                       211, 231, 242, 263, 264,
     >                       290, 298, 312, 321, 351,
     >                       352, 361, 375, 431, 475,
     >                       478, 492, 500, 631, 740,
     >                       746, 747, 768, 815, 818,
     >                       920, 998, 999/

C      DATA (EQNUMW(I),I=1,54)/
C     >'632BEHW011','632BEHW015','632BEHW017','632BEHW019','632BEHW020',
C     >'632BEHW021','632BEHW022','632BEHW042','632BEHW064','632BEHW066',
C     >'632BEHW072','632BEHW073','632BEHW081','632BEHW093','632BEHW098',
C     >'632BEHW101','632BEHW103','632BEHW106','632BEHW108','632BEHW113',
C     >'632BEHW116','632BEHW117','632BEHW119','632BEHW122','632BEHW202',
C     >'632BEHW211','632BEHW231','632BEHW242','632BEHW263','632BEHW264',
C     >'632BEHW290','632BEHW298','632BEHW312','632BEHW321','632BEHW351',
C     >'632BEHW352','632BEHW361','632BEHW375','632BEHW431','632BEHW475',
C     >'632BEHW478','632BEHW492','632BEHW500','632BEHW631','632BEHW740',
C     >'632BEHW746','632BEHW747','632BEHW768','632BEHW815','632BEHW818',
C     >'632BEHW920','632BEHW998','632BEHW999','632BEHW000'/

C      DATA (EQNUME(I),I=1,54)/
C     >'616BEHW011','616BEHW015','616BEHW017','616BEHW019','616BEHW020',
C     >'616BEHW021','616BEHW022','616BEHW042','616BEHW064','616BEHW066',
C     >'616BEHW072','616BEHW073','616BEHW081','616BEHW093','616BEHW098',
C     >'616BEHW101','616BEHW103','616BEHW106','616BEHW108','616BEHW113',
C     >'616BEHW116','616BEHW117','616BEHW119','616BEHW122','616BEHW202',
C     >'616BEHW211','616BEHW231','616BEHW242','616BEHW263','616BEHW264',
C     >'616BEHW290','616BEHW298','616BEHW312','616BEHW321','616BEHW351',
C     >'616BEHW352','616BEHW361','616BEHW375','616BEHW431','616BEHW475',
C     >'616BEHW478','616BEHW492','616BEHW500','616BEHW631','616BEHW740',
C     >'616BEHW746','616BEHW747','616BEHW768','616BEHW815','616BEHW818',
C     >'616BEHW920','616BEHW998','616BEHW999','616BEHW000'/

      DATA (EQNUMI(I),I=1,60)/
     >'I00FW2W012','I00FW2W017','I00FW2W019','I00FW2W073','I00FW2W093',
     >'I00FW2W108','I00FW2W119','I00FW2W122','I00FW2W202','I00FW2W242',
     >'I00FW2W260','I00FW2W263','I11FW2W012','I11FW2W017','I11FW2W019',
     >'I11FW2W073','I11FW2W093','I11FW2W108','I11FW2W119','I11FW2W122',
     >'I11FW2W202','I11FW2W242','I11FW2W260','I11FW2W263','I12FW2W012',
     >'I12FW2W017','I12FW2W019','I12FW2W073','I12FW2W093','I12FW2W108',
     >'I12FW2W119','I12FW2W122','I12FW2W202','I12FW2W242','I12FW2W260',
     >'I12FW2W263','I13FW2W012','I13FW2W017','I13FW2W019','I13FW2W073',
     >'I13FW2W093','I13FW2W108','I13FW2W119','I13FW2W122','I13FW2W202',
     >'I13FW2W242','I13FW2W260','I13FW2W263','I14FW2W012','I14FW2W017',
     >'I14FW2W019','I14FW2W073','I14FW2W093','I14FW2W108','I14FW2W119',
     >'I14FW2W122','I14FW2W202','I14FW2W242','I14FW2W260','I14FW2W263'/
     
      DATA (EQNUMF(I),I=1,27)/
     >'F00FW2W202','F00FW2W242','F00FW2W263','F01FW2W202','F01FW2W242',
     >'F01FW2W263','F02FW2W202','F02FW2W242','F02FW2W263','F03FW2W202',
     >'F03FW2W242','F03FW2W263','F04FW2W202','F04FW2W242','F04FW2W263',
     >'F05FW2W202','F05FW2W242','F05FW2W263','F06FW2W202','F06FW2W242',
     >'F06FW2W263','F07FW2W202','F07FW2W242','F07FW2W263','F08FW2W202',
     >'F08FW2W242','F08FW2W263'/

      DATA (EQNUMC(I),I=1,33)/
     >'I21FW2W012','I21FW2W017','I21FW2W019','I21FW2W073','I21FW2W093',
     >'I21FW2W108','I21FW2W119','I21FW2W122','I21FW2W202','I21FW2W242',
     >'I21FW2W260','I22FW2W012','I22FW2W017','I22FW2W019','I22FW2W073',
     >'I22FW2W093','I22FW2W108','I22FW2W119','I22FW2W122','I22FW2W202',
     >'I22FW2W242','I22FW2W260','I23FW2W012','I23FW2W017','I23FW2W019',
     >'I23FW2W073','I23FW2W093','I23FW2W108','I23FW2W119','I23FW2W122',
     >'I23FW2W202','I23FW2W242','I23FW2W260'/

      DATA (EQNUMD(I),I=1,8)/
     >'601DVEW205','601DVEW263','601DVEW015','602DVEW122','602DVEW204',
     >'602DVEW015','602DVEW108','602DVEW122'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        IF(VOLEQ(1:7).EQ.'616BEHW')THEN
C
C  FOUND VALID WESTSIDE EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF

        IF(VOLEQ(1:7).EQ.'632BEHW')THEN
C
C  FOUND VALID EASTSIDE EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF

        DO I=1,60
        IF(VOLEQ.EQ.EQNUMI(I))THEN
C
C  FOUND VALID INGY EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,27
        IF(VOLEQ.EQ.EQNUMF(I))THEN
C
C  FOUND VALID WESTSIDE FLEWELLING EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,33
        IF(VOLEQ.EQ.EQNUMC(I))THEN
C
C  FOUND VALID INGY EQUATION NUMBER - CANADIAN MODEL
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,8
        IF(VOLEQ.EQ.EQNUMD(I))THEN
C
C  FOUND VALID DIRECT VOLUME ESTIMATORS
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      READ(DIST,'(I2)')DISTNUM
      DONEI = 0
      DONEF = 0

c      WRITE(ASPEC,'(I3)')SPEC
c      IF(ASPEC(1:1).EQ.' ')ASPEC(1:1) = '0'
c     Westside Variants
      IF(VAR.EQ.'PN' .OR. VAR.EQ.'WC' .OR. VAR.EQ.'NC' .OR.
     >   VAR.EQ.'CA')THEN
         
c        Gifford Pinchot
         IF(FORNUM.EQ.3)THEN
            IF(SPEC.EQ. 11) THEN
                DONEI = 26
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                IF(DISTNUM.EQ.1) THEN
                  DONEF = 21
                ELSE IF (DISTNUM.EQ.5) THEN
                   DONEF = 15
                ENDIF
            ELSE IF(SPEC.EQ.202) THEN
                IF(DISTNUM.EQ.1) THEN
                  DONEF = 22
                ELSE IF (DISTNUM.EQ.5) THEN
                   DONEF = 10
                ENDIF
             ENDIF
c        Mt Hood
         ELSE IF(FORNUM.EQ.6)THEN
            IF(SPEC.EQ.11) THEN
                DONEI = 26
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEI = 23
             ELSE IF(SPEC.EQ.22) THEN
                DONEI = 38
             ELSE IF(SPEC.EQ.202) THEN
                IF(DISTNUM.EQ.5 .OR. DISTNUM.EQ.9) DONEF = 10
              END IF
c        Mt Baker - Snoqualmie
         ELSE IF(FORNUM.EQ.5)THEN
             IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEF = 12
             ELSE IF(SPEC.EQ.202) THEN
                DONEF = 25
            ENDIF
         ENDIF

         IF(DONEI.GT.0) THEN
             VOLEQ = EQNUMI(DONEI)
          ELSE IF(DONEF.GT.0) THEN
             VOLEQ = EQNUMF(DONEF)
          ELSE
c           No INGY, find Behre's hyperbola model
            LAST = 53

            FIRST = 1

            DO 5, WHILE (DONEF.EQ.0)
               HALF = (LAST - FIRST +1)/2 + FIRST
               IF(FIA(HALF) .EQ. SPEC)THEN
                  DONEF = HALF
               ELSEIF(FIRST .EQ. LAST) THEN
                  ERRFLAG = 1
                  DONEF = -1
               ELSE IF (FIA(HALF) .LT. SPEC) THEN
                  FIRST = HALF
               ELSE
                  LAST = HALF - 1
               ENDIF
  5         CONTINUE 
      
            IF(DONEF .LT. 0) THEN
               VOLEQ = '632BEHW000'
            ELSE
                IF(FIA(DONEF) .LT. 10) THEN
                  WRITE(ASPEC,'(I1)')FIA(DONEF)
                  VOLEQ(1:9) = "632BEHW00"
                  VOLEQ(10:10) = ASPEC
                ELSE IF (FIA(DONEF) .LT. 100) THEN
                  WRITE(ASPEC,'(I2)')FIA(DONEF)
                  VOLEQ(1:8) = "632BEHW0"
                  VOLEQ(9:10) = ASPEC
                ELSE
                  WRITE(ASPEC,'(I3)')FIA(DONEF)
                  VOLEQ(1:7) = "632BEHW"
                  VOLEQ(8:10) = ASPEC
                ENDIF
            ENDIF
          ENDIF

c     Eastside Variants          
      ELSE
C        first check for INGY equations by forest and species
c        Deschutes          
         IF(FORNUM.EQ.1) THEN
            IF(SPEC.EQ.15 .OR. SPEC.EQ.17) THEN
               DONEI = 2
            ELSE IF(SPEC.EQ. 73) THEN
               DONEI = 16
            ELSE IF(SPEC.EQ. 108) THEN
               DONEI = 18
            ELSE IF(SPEC.EQ. 122) THEN
               DONEI = 20
            ELSE IF(SPEC.EQ.202 ) THEN
               DONEI = 14
            ELSE IF(SPEC.EQ. 81) THEN
               DONEI = 22
            ENDIF
c        Fremont
         ELSEIF(FORNUM.EQ.2 .OR. FORNUM.EQ.20) THEN
             IF(SPEC.EQ. 15 .OR. SPEC.EQ.17) THEN
               DONEI = 14
             ELSE IF(SPEC.EQ.108) THEN
               DONEI = 6
             ELSE IF(SPEC.EQ.122) THEN
               DONEI = 32
             ENDIF
c        Gifford Pinchot
         ELSE IF(FORNUM.EQ.3)THEN
            IF(SPEC.EQ. 11) THEN
                DONEI = 26
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                IF(DISTNUM.EQ.3) DONEF = 3
            ELSE IF(SPEC.EQ.202) THEN
                IF(DISTNUM.EQ.3) DONEI = 10
            ENDIF
c        Malheur
         ELSE IF(FORNUM.EQ.4)THEN
             IF(SPEC.EQ. 108) THEN
               DONEI = 30
             ELSE IF(SPEC.EQ.17 .OR. SPEC.EQ.15) THEN
               DONEI = 26
             ELSE IF(SPEC.EQ.122) THEN
               DONEI = 32
             ELSE IF(SPEC.EQ.202) THEN
               DONEI = 33
             ENDIF
c        Mt Hood
         ELSE IF(FORNUM.EQ.6)THEN
            IF(SPEC.EQ.11) THEN
                DONEI = 26
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEI = 23
             ELSE IF(SPEC.EQ.22) THEN
                DONEI = 38
             ELSE IF(SPEC.EQ.202) THEN
                IF(DISTNUM.EQ.1 .OR. DISTNUM.EQ.6)DONEF = 22
              END IF
c        Ochoco
         ELSE IF(FORNUM.EQ.7) THEN
            IF(SPEC.EQ. 17 .OR. SPEC.EQ. 15) THEN
               DONEI = 26
            ELSE IF(SPEC.EQ. 73) THEN
               DONEI = 28
            ELSE IF(SPEC.EQ.122) THEN
               DONEI = 32
            ELSE IF(SPEC.EQ.202) THEN
               DONEI = 33
             ELSE IF(SPEC.EQ.108) THEN
                DONEI = 30
            ENDIF          
c        Umatilla
         ELSE IF(FORNUM.EQ.14)THEN
            IF(SPEC.EQ. 17 .OR. SPEC.EQ. 15) THEN
               DONEI = 38
            ELSE IF(SPEC.EQ. 19)THEN
               DONEI = 3
            ELSE IF(SPEC.EQ. 73) THEN
               DONEI = 45
            ELSE IF(SPEC.EQ. 93) THEN
               DONEI = 5
            ELSE IF(SPEC.EQ.108) THEN
               DONEI = 6
            ELSE IF(SPEC.EQ.122) THEN
               DONEI = 44
            ELSE IF(SPEC.EQ.202) THEN
               DONEI = 38
            ENDIF
c        Wallowa-Whitman
         ELSE IF(FORNUM.EQ.16) THEN
            IF(SPEC.EQ. 17 .OR. SPEC.EQ. 15) THEN
               DONEI = 14
            ELSE IF(SPEC.EQ.122) THEN
               DONEI = 20
            ELSE IF(SPEC.EQ.202) THEN
               DONEI = 21
            ENDIF
c        Okanogan - Wenatchee
         ELSE IF(FORNUM.EQ.8 .OR. FORNUM.EQ.17) THEN
            IF(SPEC.EQ. 17) THEN
                IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR. DISTNUM.EQ.5  
     >                                  .OR. DISTNUM.EQ.7 ) DONEI = 14
            ELSE IF(SPEC.EQ.202) THEN
                IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR. DISTNUM.EQ.4  
     >              .OR. DISTNUM.EQ.5 .OR. DISTNUM.EQ.7 .OR. 
     >              DISTNUM.EQ.9) DONEI = 33
            ELSE IF(SPEC.EQ.108) THEN
                IF(DISTNUM.EQ.4) DONEI = 30
            ELSE IF(SPEC.EQ.93) THEN
                IF(DISTNUM.EQ.9) DONEI = 17
            ELSE IF(SPEC.EQ.122) THEN
                IF(DISTNUM.EQ.4) THEN
                  DONEI = 32
                ELSE IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR. DISTNUM.EQ.5  
     >            .OR. DISTNUM.EQ.7) THEN
                  DONEI = 20
                ENDIF
            ENDIF
         ENDIF
       
         IF(DONEI.GT.0) THEN
             VOLEQ = EQNUMI(DONEI)
          ELSE IF(DONEF.GT.0) THEN
             VOLEQ = EQNUMF(DONEF)
         ELSE
c           No INGY, find Behre's hyperbola model
            LAST = 53

            FIRST = 1

            DO 7, WHILE (DONEF.EQ.0)
              HALF = (LAST - FIRST +1)/2 + FIRST
              IF(FIA(HALF) .EQ. SPEC)THEN
                DONEF = HALF
              ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONEF = -1
              ELSE IF (FIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
              ELSE
                LAST = HALF - 1
              ENDIF
  7         CONTINUE 
      
            IF(DONEF .LT. 0) THEN
              VOLEQ = '616BEHW000'
            ELSE
                IF(FIA(DONEF) .LT. 10) THEN
                  WRITE(ASPEC,'(I1)')FIA(DONEF)
                  VOLEQ(1:9) = "616BEHW00"
                  VOLEQ(10:10) = ASPEC
                ELSE IF (FIA(DONEF) .LT. 100) THEN
                  WRITE(ASPEC,'(I2)')FIA(DONEF)
                  VOLEQ(1:8) = "616BEHW0"
                  VOLEQ(9:10) = ASPEC
                ELSE
                  WRITE(ASPEC,'(I3)')FIA(DONEF)
                  VOLEQ(1:7) = "616BEHW"
                  VOLEQ(8:10) = ASPEC
                ENDIF
            ENDIF
         ENDIF
     
      ENDIF
      RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R7_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 VAR,FORST
      INTEGER SPEC,ERRFLAG,FORNUM
      CHARACTER*10 EQNUM(44)
      INTEGER FIA(41), FIRST, HALF, LAST, DONE,I

C     SPECIES
C     Pacific. silver fir,White fir,       Grand fir,        Subalpine fir,    California red fir,
C     Shasta red fir,     Noble fir,       Port Orford cedar,Alaska cedar,     Western juniper, 
C     Subalpine larch,    Western larch,   Incense cedar,    Engelmann spruce, Sitka spruce,  
C     Whitebark pine,     Knobcone pine,   Lodgepole pine,   Jeffery pine,     Sugar pine,
C     Western white pine, Ponderosa pine,  Douglas fir,      Redwood,          Pacific yew,
C     Western red cedar,  Hemlock,         Western Hemlock,  Mountain Hemlock, Big leaf maple,
C     Red alder,          White alder,     Pacific madrone,  Golden chinquapin,Oregon ash,
C     Tanoak,             Black cottonwood,Oak species,      Oregon White Oak, Oregon myrtle,
C     Other species,

      DATA (FIA(I),I=1,41)/  11,   15,   17,   19,   20,
     >                       21,   22,   41,   42,   64,
     >                       72,   73,   81,   93,   98,
     >                      101,  103,  108,  116,  117,  
     >                      119,  122,  202,  211,  231,  
     >                      242,  260,  263,  264,  312,  
     >                      351,  352,  361,  431,  542,  
     >                      631,  747,  800,  815,  981,  
     >                      999/

      DATA (EQNUM(I),I=1,44)/
     >'B00BEHW011','B00BEHW015','B00BEHW017','B00BEHW015','B00BEHW021',
     >'B00BEHW021','B00BEHW022','B00BEHW041','B00BEHW042','B00BEHW242',
     >'B00BEHW073','B00BEHW073','B00BEHW081','B00BEHW093','B00BEHW098',
     >'B00BEHW119','B00BEHW108','B00BEHW108','B00BEHW116','B00BEHW117',
     >'B00BEHW119','B00BEHW122','B01BEHW202','B00BEHW211','B00BEHW231',
     >'B00BEHW242','B00BEHW260','B00BEHW260','B00BEHW260','B00BEHW312',
     >'B00BEHW351','B00BEHW361','B00BEHW361','B00BEHW431','B00BEHW542',
     >'B00BEHW631','B00BEHW747','B00BEHW800','B00BEHW800','B00BEHW998',
     >'B00BEHW999',
     >'B02BEHW202','B03BEHW202','B01BEHW202'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,44
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      LAST = 41

      FIRST = 1
      IF(SPEC.EQ.202) THEN
          IF(VAR.EQ."WC" .OR. VAR.EQ."wc") THEN
                DONE = 42
          ELSEIF(VAR.EQ."NC" .OR. VAR.EQ."nc") THEN
                DONE = 43
          ELSE
                DONE = 44
          ENDIF
       ENDIF

      DO 5, WHILE (DONE.EQ.0)
         HALF = (LAST - FIRST +1)/2 + FIRST
          IF(FIA(HALF) .EQ. SPEC)THEN
             DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
             ERRFLAG = 1
             DONE = -1
         ELSE IF (FIA(HALF) .LT. SPEC) THEN
             FIRST = HALF
          ELSE
             LAST = HALF - 1
          ENDIF
  5   CONTINUE 
      
      IF(DONE .LT. 0)THEN
C           Other Hardwood
          VOLEQ = EQNUM(41)
      ELSE
         VOLEQ = EQNUM(DONE)   
       ENDIF
C       
       RETURN
      END
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      CHARACTER*2 GEOAREA,GEOCODES(33)
      CHARACTER*2 PROD,VAR,FORST,DIST
      CHARACTER*10 VOLEQ,VEQTEM
      INTEGER SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J
      CHARACTER*3 SNSP(92),SESP(118)
      INTEGER SNFIA(92),SEFIA(118)

c     match species to valid species equation code
      DATA (SNFIA(I),I=1,92)/
     >  10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
     > 129, 130, 131, 132, 221, 222, 260, 298, 311, 313, 
     > 316, 317, 318, 330, 370, 372, 391, 400, 450, 460, 
     > 471, 491, 521, 531, 540, 541, 543, 544, 552, 555, 
     > 580, 591, 601, 602, 611, 621, 650, 651, 652, 653, 
     > 654, 660, 680, 691, 693, 694, 701, 711, 721, 731, 
     > 740, 743, 762, 802, 806, 812, 813, 819, 820, 822, 
     > 824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838, 
     > 901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

      DATA (SEFIA(I),I=1,118)/
     >  57,  68, 110, 111, 115, 121, 122, 126, 128, 129,
     > 131, 132, 221, 222, 261, 310, 313, 316, 317, 318,
     > 323, 331, 370, 371, 372, 373, 391, 400, 401, 402, 
     > 403, 404, 405, 407, 408, 409, 450, 461, 462, 471, 
     > 491, 500, 521, 531, 540, 541, 543, 544, 545, 546,
     > 552, 555, 571, 591, 601, 602, 611, 621, 641, 651, 
     > 652, 653, 680, 681, 682, 691, 692, 693, 694, 701, 
     > 711, 721, 731, 740, 741, 742, 743, 746, 762, 802, 
     > 804, 806, 809, 812, 813, 817, 819, 820, 822, 823, 
     > 824, 825, 826, 827, 828, 830, 831, 832, 833, 834,
     > 835, 836, 837, 838, 840, 842, 901, 920, 922, 931, 
     > 951, 970, 971, 972, 974, 975, 977, 994/


      DATA (SNSP(I), I=1,92)/
     &'261','100','115','132','110','111','115','121','126','126','128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800','835',
     &'901','300','300','300','970','970','970','970','300','300'/

      DATA (SESP(I), I=1,118)/
     >'100','100','110','111','115','121','100','126','128','129',
     >'131','132','221','222','261','300','500','316','300','500',
     >'300','330','370','370','370','370','370','400','400','400',
     >'400','404','400','400','400','400','300','500','460','300',
     >'300','500','500','531','541','541','300','544','300','300',
     >'500','300','500','300','500','500','611','621','500','300',
     >'652','653','300','300','300','300','693','693','694','500',
     >'300','300','731','300','300','300','300','300','300','802',
     >'800','806','800','812','813','800','800','800','822','800',
     >'800','800','800','827','828','800','831','832','833','800',
     >'835','835','837','800','835','800','901','300','300','300',
     >'300','970','970','970','970','970','970','300'/

      DATA (GEOCODES(I), I=1,33)/
     >  '01','02','03','04','05','06','07','08','09','10',
     >  '11','12','13','14','15','16','17','18','19','20',
     >  '21','22','23','24','25','26','27','28','29','30',
     >  '31','32','33'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        VEQTEM(1:1)='8'
        VEQTEM(4:7)='DVEE'
        DO I=1,33
        VEQTEM(2:3)=GEOCODES(I)
C  SN
        DO J=1,67
        VEQTEM(8:10)=SNSP(J)
        IF(VOLEQ.EQ.VEQTEM)THEN
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
C  SE
        DO J=1,96
        VEQTEM(8:10)=SESP(J)
        IF(VOLEQ.EQ.VEQTEM)THEN
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
        ENDDO
      RETURN
      ENDIF

      READ(FORST,'(I2)')FORNUM
      READ(DIST,'(I2)')DISTNUM

      IF (FORNUM.EQ.1)THEN
C              // alabama
              IF (DISTNUM.EQ.1) THEN
                     GEOAREA = "13";
              ELSE IF (DISTNUM.EQ.3) THEN
                     GEOAREA = "15";
              ELSE IF (DISTNUM.EQ.4) THEN
                     GEOAREA = "16";
              ELSE IF (DISTNUM.EQ.5 .OR. DISTNUM.EQ.6) THEN
                 GEOAREA = "14";
              ELSE
                 GEOAREA = "17";
         ENDIF

       ELSE IF (FORNUM.EQ.2)THEN
C                     // daniel boone
                     GEOAREA = "10";

       ELSE IF (FORNUM.EQ.3)THEN
C                     // Chattahoochee/Oconee
                     GEOAREA = "01";
                     IF (DISTNUM .EQ. 8) GEOAREA = "06";

       ELSE IF (FORNUM.EQ.4 .OR. FORNUM .EQ. 60)THEN
C                     // Cherokee
C                      // land between the lakes
                     GEOAREA = "01";

       ELSE IF (FORNUM.EQ.5)THEN
C                     // Florida
                     GEOAREA = "03";

       ELSE IF (FORNUM.EQ.6) THEN
C                     // Kisatchie
                     GEOAREA = "02";
                     IF (DISTNUM .EQ. 6) GEOAREA = "09";

       ELSE IF (FORNUM .EQ. 7) THEN  
C                     // Mississippi
                     
              IF (DISTNUM.EQ.1) THEN
                     GEOAREA = "19";
              ELSE IF (DISTNUM.EQ.2) THEN
                     GEOAREA = "20";
              ELSE IF (DISTNUM.EQ.4) THEN
                     GEOAREA = "21";
              ELSE IF (DISTNUM.EQ.5) THEN
                 GEOAREA = "22";
              ELSE IF (DISTNUM.EQ.6) THEN
                 GEOAREA = "18";
              ELSE
                 GEOAREA = "23";
         END IF

       ELSE IF (FORNUM .EQ. 8) THEN  
C                     // GW/Jeff
          GEOAREA = "11";
                     
            IF (DISTNUM.EQ.11 .OR. DISTNUM.EQ.12 .OR. DISTNUM.EQ.13 .OR. 
     >         DISTNUM.EQ.14 .OR. DISTNUM.EQ.15 .OR. DISTNUM.EQ.16) THEN
                     GEOAREA = "12";
         END IF

       ELSE IF (FORNUM .EQ. 9) THEN
C                     // Ouachita
              GEOAREA = "31";
              IF (DISTNUM.EQ.1 .OR. DISTNUM.EQ.6) THEN
                     GEOAREA = "30";
              ELSE IF (DISTNUM.EQ.12) THEN
                 GEOAREA = "32";
         END IF
       
      ELSE IF (FORNUM .EQ. 10) THEN   
C                     // Ozark/St Francis
                     GEOAREA = "04";
                 IF (DISTNUM .EQ. 7) GEOAREA = "05";

       ELSE IF (FORNUM .EQ. 11) THEN
C                     // North Carolina
                     GEOAREA = "01";
                     IF (DISTNUM .EQ. 3)THEN
                        GEOAREA = "07";
                     ELSE IF (DISTNUM .EQ. 10) THEN
                            GEOAREA = "08";
                     ENDIF

       ELSE IF (FORNUM .EQ. 12)THEN
C                     // Francis Marion/Sumpter
                 GEOAREA = "24";
                     IF(DISTNUM .EQ. 2) THEN
                        GEOAREA = "01";
                     ELSE IF(DISTNUM .EQ. 5)THEN 
                            GEOAREA = "25";
                     ENDIF

       ELSE IF (FORNUM.EQ.13) THEN
C                      // Texas
                     IF(DISTNUM .EQ. 1) THEN
                        GEOAREA = "26";
                     ELSE IF(DISTNUM .EQ. 3)THEN 
                            GEOAREA = "27";
                     ELSE IF(DISTNUM .EQ. 4)THEN 
                            GEOAREA = "29";
                     ELSE
                            GEOAREA = "28";
                     ENDIF

       ELSE IF (FORNUM.EQ.36)THEN
C            // savannah river
                     GEOAREA = "25";

      ENDIF

C     CREATE THE VOLUME EQUATION NUMBER
      
      VOLEQ(1:1) = '8'
      VOLEQ(2:3) = GEOAREA
      VOLEQ(4:7) = "DVEE"

C     FIND CORRECT SPECIES
      DONE = 0
      FIRST = 1
      IF(VAR.EQ."SE" .OR. VAR.EQ."se") THEN
         LAST = 118
         DO 15, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(SEFIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
            ELSE IF (SEFIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  15     CONTINUE 
         IF(DONE .LT. 0) DONE = 118

         VOLEQ(8:10) = SESP(DONE)
      ELSE
         LAST = 92
         DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(SNFIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
            ELSE IF (SNFIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  5      CONTINUE 
         IF(DONE .LT. 0) DONE = 90

         VOLEQ(8:10) = SNSP(DONE)
      ENDIF
       RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      CHARACTER*1 GEOAREA,TOPCODE(4),ICHAR
      CHARACTER*2 PROD,VAR,FORST,DIST
      CHARACTER*10 VOLEQ,VEQTEM
      CHARACTER*3 SNSP(92),SESP(118)
      INTEGER SNFIA(92),SEFIA(118)
      INTEGER SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J,K

c     match species to valid species equation code
      DATA (SNFIA(I),I=1,92)/
     >  10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
     > 129, 130, 131, 132, 221, 222, 260, 298, 311, 313, 
     > 316, 317, 318, 330, 370, 372, 391, 400, 450, 460, 
     > 471, 491, 521, 531, 540, 541, 543, 544, 552, 555, 
     > 580, 591, 601, 602, 611, 621, 650, 651, 652, 653, 
     > 654, 660, 680, 691, 693, 694, 701, 711, 721, 731, 
     > 740, 743, 762, 802, 806, 812, 813, 819, 820, 822, 
     > 824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838, 
     > 901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

      DATA (SEFIA(I),I=1,118)/
     >  57,  68, 110, 111, 115, 121, 122, 126, 128, 129,
     > 131, 132, 221, 222, 261, 310, 313, 316, 317, 318,
     > 323, 331, 370, 371, 372, 373, 391, 400, 401, 402, 
     > 403, 404, 405, 407, 408, 409, 450, 461, 462, 471, 
     > 491, 500, 521, 531, 540, 541, 543, 544, 545, 546,
     > 552, 555, 571, 591, 601, 602, 611, 621, 641, 651, 
     > 652, 653, 680, 681, 682, 691, 692, 693, 694, 701, 
     > 711, 721, 731, 740, 741, 742, 743, 746, 762, 802, 
     > 804, 806, 809, 812, 813, 817, 819, 820, 822, 823, 
     > 824, 825, 826, 827, 828, 830, 831, 832, 833, 834,
     > 835, 836, 837, 838, 840, 842, 901, 920, 922, 931, 
     > 951, 970, 971, 972, 974, 975, 977, 994/


      DATA (SNSP(I), I=1,92)/
     &'261','100','115','132','110','111','115','121','126','126','128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800','835',
     &'901','300','300','300','970','970','970','970','300','300'/

      DATA (SESP(I), I=1,118)/
     >'100','100','110','111','115','121','100','126','128','129',
     >'131','132','221','222','261','300','500','316','300','500',
     >'300','330','370','370','370','370','370','400','400','400',
     >'400','404','400','400','400','400','300','500','460','300',
     >'300','500','500','531','541','541','300','544','300','300',
     >'500','300','500','300','500','500','611','621','500','300',
     >'652','653','300','300','300','300','693','693','694','500',
     >'300','300','731','300','300','300','300','300','300','802',
     >'800','806','800','812','813','800','800','800','822','800',
     >'800','800','800','827','828','800','831','832','833','800',
     >'835','835','837','800','835','800','901','300','300','300',
     >'300','970','970','970','970','970','970','300'/
C
      DATA TOPCODE / '4','7','8','9' /
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        VEQTEM(1:1)='8'
        VEQTEM(4:7)='CLKE'
        DO I=1,7
        WRITE(ICHAR,'(I1)')I
        VEQTEM(2:2)=ICHAR
        DO J=1,4
        VEQTEM(3:3)=TOPCODE(J)
C  SN
        DO K=1,90
        VEQTEM(8:10)=SNSP(K)
        IF(VOLEQ.EQ.VEQTEM)THEN
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
C  SE
        DO K=1,118
        VEQTEM(8:10)=SESP(K)
        IF(VOLEQ.EQ.VEQTEM)THEN
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
        ENDDO
        ENDDO
      RETURN
      ENDIF

C     FIND CORRECT FOREST
      READ(FORST,'(I2)')FORNUM
      READ(DIST,'(I2)')DISTNUM

C     SET GEOCODE
C     SET DIAMETER BY PRODUCT

      IF (FORNUM.EQ.1)THEN
C             // alabama
              GEOAREA = "4";
              IF (DISTNUM.EQ.3) GEOAREA = '1'

       ELSE IF (FORNUM.EQ.2 .OR. FORNUM.EQ.4 .OR. FORNUM .EQ. 8 .OR. 
     >                                             FORNUM .EQ. 60)THEN
C                     // daniel boone
C                     // Cherokee
C                     // GW/Jeff
C                 // land between the lakes
                     GEOAREA = "3";

       ELSE IF (FORNUM.EQ.3)THEN
C                     // Chattahoochee/Oconee
                     GEOAREA = "3";
                     IF (DISTNUM .EQ. 8) GEOAREA = "2";

       ELSE IF (FORNUM.EQ.5 .OR. FORNUM.EQ.36)THEN
C                     // Florida
C           // savannah river
                     GEOAREA = "1";

       ELSE IF (FORNUM.EQ.6 .OR. FORNUM.EQ.13) THEN
C                     // Kisatchie
C                     // Texas
                     GEOAREA = "5";

       ELSE IF (FORNUM .EQ. 7) THEN  
C                     // Mississippi
                     GEOAREA = "5";
                     
            IF (DISTNUM .EQ. 6) THEN
                            GEOAREA = "7";
                     ELSE IF (DISTNUM.EQ.7 .OR. DISTNUM.EQ.17) THEN
                            GEOAREA = "4";
                     ENDIF
       ELSE IF (FORNUM .EQ. 9) THEN
C                     // Ouachita
                     GEOAREA = "6";
       ELSE IF (FORNUM .EQ. 10) THEN   
C                     // Ozark/St Francis
                     GEOAREA = "6";
                 IF (DISTNUM .EQ. 7) GEOAREA = "7";

       ELSE IF (FORNUM .EQ. 11) THEN
C                     // North Carolina
                     GEOAREA = "3";
                     IF (DISTNUM .EQ. 3)THEN
                   GEOAREA = "1";
                     ELSE IF (DISTNUM .EQ. 10) THEN
                            GEOAREA = "2";
                     ENDIF

       ELSE IF (FORNUM .EQ. 12)THEN
C                     // Francis Marion/Sumpter
                 GEOAREA = "2";
                     IF(DISTNUM .EQ. 2) THEN
                        GEOAREA = "3";
                     ELSE IF(DISTNUM .EQ. 5)THEN 
                            GEOAREA = "1";
                     ENDIF
      ENDIF

C     CREATE THE VOLUME EQUATION NUMBER
      VOLEQ(1:1) = '8'
      VOLEQ(2:2) = GEOAREA
      IF(PROD.EQ.'01')THEN
         IF(SPEC.LT.300) THEN
C           7 INCH TOP
            VOLEQ(3:3) = '7'
          ELSE
C           9 INCH TOP
            VOLEQ(3:3) = '9'
          ENDIF
       ELSEIF (PROD.EQ.'08') THEN
C           USE PRODUCT 08 LOGIC
          VOLEQ(3:3) = '8'
       ELSE
C           4 INCH TOP
         VOLEQ(3:3) = '4'
       ENDIF
      VOLEQ(4:7) = "CLKE"

C     FIND CORRECT SPECIES
      DONE = 0
      FIRST = 1
      IF(VAR.EQ."SE" .OR. VAR.EQ."se") THEN
         LAST = 118
         DO 15, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(SEFIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
            ELSE IF (SEFIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  15     CONTINUE 
         IF(DONE .LT. 0) DONE = 118

         VOLEQ(8:10) = SESP(DONE)
      ELSE
         LAST = 92
         DO 5, WHILE (DONE.EQ.0)
            HALF = (LAST - FIRST +1)/2 + FIRST
             IF(SNFIA(HALF) .EQ. SPEC)THEN
                DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
                ERRFLAG = 1
                DONE = -1
            ELSE IF (SNFIA(HALF) .LT. SPEC) THEN
                FIRST = HALF
             ELSE
                LAST = HALF - 1
             ENDIF
  5      CONTINUE 
         IF(DONE .LT. 0) DONE = 90

         VOLEQ(8:10) = SNSP(DONE)
      ENDIF

       RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R9_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
       CHARACTER*10 VOLEQ,VEQTEM
       CHARACTER*2 GEOAREA,VAR, FORST,GEOCODES(6)
      CHARACTER*3 LSSP(69),CSSP(97),NESP(108),SNSP(92),ASPEC
       INTEGER SPEC,ERRFLAG,FORNUM,LSFIA(69),CSFIA(97),NEFIA(108),
     &         SNFIA(92)
      INTEGER FIRST, HALF, LAST, DONE, I, J
            
      DATA (LSSP(I), I=1,69)/
     >'012','068','071','091','094','095','105','125','125','129',
     >'130','241','261','001','313','314','315','316','317','318',
     >'319','371','375','391','402','403','407','421','462','491',
     >'500','531','541','543','544','601','602','660','693','701',
     >'731','741','742','743','746','766','761','762','763','766',
     >'802','804','809','823','826','833','837','901','920','922',
     >'923','931','935','951','972','975','977','300','999'/
       
      DATA (CSSP(I), I=1,97)/
     >'068','068','110','129','131','132','221','001','316','316',
     >'316','318','331','373','391','400','401','402','403','404',
     >'405','407','408','409','400','450','460','460','471','490',
     >'500','521','531','541','541','543','541','543','543','552',
     >'571','601','602','611','621','641','651','653','680','690',
     >'691','693','694','701','711','731','742','741','742','746',
     >'746','762','802','823','806','812','813','813','813','823',
     >'824','823','823','816','813','830','813','832','833','813',
     >'835','835','837','901','920','920','931','951','970','970',
     >'970','970','970','970','999','999','999'/


      DATA (NESP(I), I=1,108)/
     >'012','068','068','068','071','097','097','094','095','097',
     >'105','105','110','105','125','105','105','129','105','131',
     >'125','068','261','261','105','313','318','315','316','318',
     >'318','330','332','341','355','371','371','373','374','375',
     >'375','391','400','400','400','400','400','462','490','500',
     >'521','531','541','541','543','541','541','591','601','602',
     >'611','621','641','650','651','653','660','691','693','701',
     >'711','712','731','741','742','746','742','746','760','761',
     >'762','802','802','832','806','833','813','806','823','832',
     >'802','806','830','831','832','833','835','837','901','920',
     >'931','951','951','970','970','970','999','004'/

      DATA (SNSP(I), I=1,92)/
     &'261','100','115','132','110','111','115','121','126','126','128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800','835',
     &'901','300','300','300','970','970','970','970','300','300'/

      DATA (LSFIA(I), I=1,69)/
     > 12,  68,  71,   91,  94,  95, 105, 125, 125, 129,
     > 130, 241, 261, 298, 313, 314, 315, 316, 317, 318,
     > 319, 371, 375, 391, 402, 403, 407, 421, 462, 491,
     > 500, 531, 541, 543, 544, 601, 602, 660, 693, 701,
     > 731, 741, 742, 743, 746, 760, 761, 762, 763, 766, 
     > 802, 804, 809, 823, 826, 833, 837, 901, 920, 922,
     > 923, 931, 935, 951, 972, 975, 977, 993, 994/

      DATA (CSFIA(I), I =1,97)/
     >  57,  68, 110, 129, 131, 132, 221, 298, 313, 316,
     > 317, 318, 331, 373, 391, 400, 401, 402, 403, 404, 
     > 405, 407, 408, 409, 410, 450, 461, 462, 471, 491,
     > 500, 521, 531, 540, 541, 543, 544, 545, 546, 552,
     > 571, 601, 602, 611, 621, 641, 651, 653, 680, 690,
     > 691, 693, 694, 701, 711, 731, 740, 741, 742, 743, 
     > 746, 762, 802, 804, 806, 812, 813, 817, 822, 823, 
     > 824, 825, 826, 827, 828, 830, 831, 832, 833, 834, 
     > 835, 836, 837, 901, 920, 922, 931, 951, 970, 971, 
     > 972, 974, 975, 977, 991, 992, 994/

      DATA (NEFIA(I), I=1,108)/
     >  12,  43,  57,  68,  71,  90,  91,  94,  95,  97,
     > 100, 105, 110, 123, 125, 126, 128, 129, 130, 131,
     > 132, 241, 260, 261, 298, 313, 314, 315, 316, 317, 
     > 318, 330, 332, 341, 356, 371, 372, 373, 374, 375, 
     > 379, 391, 400, 403, 405, 407, 409, 462, 491, 500, 
     > 521, 531, 540, 541, 543, 544, 545, 591, 601, 602, 
     > 611, 621, 641, 650, 651, 653, 660, 691, 693, 701, 
     > 711, 712, 731, 741, 742, 743, 744, 746, 760, 761, 
     > 762, 800, 802, 804, 806, 812, 813, 817, 823, 825, 
     > 826, 827, 830, 831, 832, 833, 835, 837, 901, 922,
     > 931, 951, 952, 970, 972, 975, 994, 998/

      DATA (SNFIA(I),I=1,92)/
     >  10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
     > 129, 130, 131, 132, 221, 222, 260, 298, 311, 313, 
     > 316, 317, 318, 330, 370, 372, 391, 400, 450, 460, 
     > 471, 491, 521, 531, 540, 541, 543, 544, 552, 555, 
     > 580, 591, 601, 602, 611, 621, 650, 651, 652, 653, 
     > 654, 660, 680, 691, 693, 694, 701, 711, 721, 731, 
     > 740, 743, 762, 802, 806, 812, 813, 819, 820, 822, 
     > 824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838, 
     > 901, 920, 931, 950, 970, 971, 972, 975, 998, 999/
C
C  SEARCH FOR VALID EQUATION NUMBER
C  FIRST, SEARCH FOR CLKE OR DVEE
C
      IF(SPEC.EQ.9999)THEN
        IF(VOLEQ(1:7) .EQ. '900CLKE')THEN
C  LS
          DO J=1,69
          IF(VOLEQ(8:10).EQ.LSSP(J))THEN
            SPEC=8888
            RETURN
          ENDIF
          ENDDO
C  CS
          DO J=1,97
          IF(VOLEQ(8:10).EQ.CSSP(J))THEN
            SPEC=8888
            RETURN
          ENDIF
          ENDDO
C  NE
          DO J=1,108
          IF(VOLEQ(8:10).EQ.NESP(J))THEN
            SPEC=8888
            RETURN
          ENDIF
          ENDDO
C  SN
          DO J=1,92
          IF(VOLEQ(8:10).EQ.SNSP(J))THEN
            SPEC=8888
            RETURN
          ENDIF
          ENDDO
          RETURN

        ELSEIF(VOLEQ(1:7) .EQ. '900DVEE')THEN

          IF(SPEC.EQ.9999)THEN
            VEQTEM(1:7)='900DVEE'
C  LS
            DO J=1,69
            VEQTEM(8:10)=LSSP(J)
            IF(VOLEQ.EQ.VEQTEM)THEN
              SPEC=8888
              RETURN
            ENDIF
            ENDDO
C  CS
            DO J=1,97
            VEQTEM(8:10)=CSSP(J)
            IF(VOLEQ.EQ.VEQTEM)THEN
              SPEC=8888
              RETURN
            ENDIF
            ENDDO
C  NE
            DO J=1,108
            VEQTEM(8:10)=NESP(J)
            IF(VOLEQ.EQ.VEQTEM)THEN
              SPEC=8888
              RETURN
            ENDIF
            ENDDO
C  SN
            DO J=1,92
            VEQTEM(8:10)=SNSP(J)
            IF(VOLEQ.EQ.VEQTEM)THEN
              SPEC=8888
              RETURN
            ENDIF
            ENDDO
          RETURN
          ENDIF
        ELSE
C     NOT A VALID REGION 9 EQUATION
          RETURN
        ENDIF
      ENDIF
C
      IF(VOLEQ(1:7).EQ.'900CLKE')THEN
C     NEW CLARK'S PROFILE MODEL VOLUME EQUATION NUMBERS
C     MAKE SURE SPEC IS A 3 CHARACTER FIELD.      
C
        WRITE(ASPEC,'(I3)')SPEC
     
        IF(SPEC .LT.10)THEN
          ASPEC(1:2) = '00'
        ELSEIF(SPEC.LT.100)THEN
          ASPEC(1:1) = '0'
        ENDIF

        VOLEQ(8:10) = ASPEC

        RETURN
      ELSEIF(VOLEQ(1:7).EQ.'900DVEE')THEN
C
C     DIRECT VOLUME ESTIMATORS
C     FIND CORRECT SPECIES
        FIRST = 1
        DONE = 0
        IF(VAR.EQ."LS" .OR. VAR.EQ."ls") THEN
          LAST = 69
          DO 5, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(LSFIA(HALF) .EQ. SPEC)THEN
            DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
            ERRFLAG = 1
            DONE = -1
          ELSEIF(LSFIA(HALF) .LT. SPEC) THEN
            FIRST = HALF
          ELSE
            LAST = HALF - 1
          ENDIF
  5       CONTINUE 
          IF(DONE .LT. 0) DONE = 69

          VOLEQ(8:10) = LSSP(DONE)
        ELSE IF(VAR.EQ.'CS' .OR. VAR.EQ.'cs')THEN
          LAST = 97
          DO 15, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(CSFIA(HALF) .EQ. SPEC)THEN
            DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
            ERRFLAG = 1
            DONE = -1
          ELSE IF (CSFIA(HALF) .LT. SPEC) THEN
            FIRST = HALF
          ELSE
            LAST = HALF - 1
          ENDIF
  15      CONTINUE 
          IF(DONE .LT. 0) DONE = 97
           VOLEQ(8:10) = CSSP(DONE)
        ELSE
          LAST = 108
          DO 25, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(NEFIA(HALF) .EQ. SPEC)THEN
            DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
            ERRFLAG = 1
            DONE = -1
          ELSE IF (NEFIA(HALF) .LT. SPEC) THEN
            FIRST = HALF
          ELSE
            LAST = HALF - 1
          ENDIF
  25      CONTINUE 
          IF(DONE .LT. 0) DONE = 108
          VOLEQ(8:10) = NESP(DONE)
        ENDIF
      ENDIF
      RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R10_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG,FORNUM

      CHARACTER*10 TONEQN(15), CHUEQN(15)
      INTEGER FIA(13), FIRST, HALF, LAST, DONE,I
C     SPECIES
C     Pacific silver fir,Subalpine fir,   Alaska yellow cedar,White spruce,    Sitka spruce,
C     Lodgepole pine,    Western redcedar,Western hemlock,    Mountain Hemlock,Other Softwood,
C     Red Alder,         Black cottonwood,Other Hardwood


      DATA (FIA(I),I=1,13)/    011,   019,   042,   094,   098,
     >                         108,   242,   263,   264,   298,   
     >                         351,   747,   998/

      DATA (TONEQN(I),I=1,15)/
     > 'A00F32W260','A00F32W260','A00F32W042','A00DVEW094','A00F32W098',
     > 'A00F32W260','A00F32W242','A00F32W260','A00F32W260','A00F32W260',
     > 'A32CURW000','A00F32W260','A00F32W260',
     > 'A02F32W098','A02F32W260'/
      
      
      DATA (CHUEQN(I),I=1,15)/
     > 'A01DEMW000','A01DEMW000','A00DVEW094','A01DEMW000','A01DEMW000',
     > 'A01DEMW000','A01DEMW000','A01DEMW000','A01DEMW000','A16DEMW098',
     > 'A16CURW260','A01DEMW000','A16DEMW098',
     > 'A16DEMW098','A16DEMW098'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,15
        IF((VOLEQ.EQ.TONEQN(I)).OR.(VOLEQ.EQ.CHUEQN(I)))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      LAST = 13
      FIRST = 1
      DONE = 0

      DO 5, WHILE (DONE.EQ.0)
         HALF = (LAST - FIRST +1)/2 + FIRST
          IF(FIA(HALF) .EQ. SPEC)THEN
             DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
             ERRFLAG = 1
             DONE = -1
         ELSE IF (FIA(HALF) .LT. SPEC) THEN
             FIRST = HALF
          ELSE
             LAST = HALF - 1
          ENDIF
  5   CONTINUE 
      
C           If not found, use Other Hardwood
      IF(DONE .LT. 0) DONE = 13

      IF(FORNUM .EQ. 4) THEN
         VOLEQ = CHUEQN(DONE)   
      ELSE
         VOLEQ = TONEQN(DONE)   
       ENDIF

       RETURN
      END

