!== last modified  09-15-2016 reconciled Vol. Eq. No. output from FVS with Cruise software-RNH
C 01/18/2013 added FIAVOLEQDEF, R5_PNWEQN and R6_PNWEQN for PNE FIA equations.
C 03/25/2014 changed default equation for Region 3 (R3_EQN) Ponderosa pine in the forest Apache Sitgreaves, Coconino, Kaibab and Tonto to 300FW2W122.
C 09/09/2016 Modified R3_EQN default equation for PP, DF, WF, and WP in Sante Fe NF
C 09/15/2016 Corrected R4 default equation for other species to DVE equation
C 04/12/2017 removed 532WO2W*** equation from R5_EQN per the email from Craig Bodenhausen (4/12/2017)
C 07/19/2021 Changed R8_CEQN to use the R8 new Clark equation 8*1CLKE***. Added 1 to TOPCODE array.
      SUBROUTINE VOLEQDEF (VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)
C
C    SUBROUTINE WILL RETURN THE DEFAULT VOLUME EQUATION NUMBER
C        SPEC = 3 DIGIT FIA SPECIES CODE
      !DEC$ ATTRIBUTES DLLEXPORT::VOLEQDEF

      CHARACTER*2 FORST,PROD,DIST,VAR
      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG,REGN,SPEC
C
C  The GETVARIANT routine is required by the NVEL system to find the
C  FVS variant. When VOLEQDEF is used by FVS, the call to VOLEQDEF always
C  carries the variant 2 character symbol so the GETVARIANT routine
C  is not used by FVS.
C
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
C      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'02'))THEN
C         CALL R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
C      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'01'))THEN
C         CALL R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.8)THEN
         CALL R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
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
      CHARACTER VVER*7
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
C  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
C  INLAND CALIFORNIA
         IF(FORNUM.EQ.5 .OR. FORNUM.EQ.6 .OR. FORNUM.EQ.8 .OR. 
     >      FORNUM.EQ.11 .OR. FORNUM.EQ.14) THEN
            VAR = 'CA'
C  SOUTHERN OREGON
         ELSEIF(FORNUM.EQ.9) THEN
            VAR = 'SO'
C  WESTERN SIERRA
         ELSEIF(FORNUM.EQ.17 .OR. FORNUM.EQ.16 .OR. FORNUM.EQ.15 .OR. 
     >          FORNUM.EQ.13 .OR. FORNUM.EQ.3) THEN
            VAR = 'WS'
C  KLAMATH/NORTHERN CALIFORNIA
         ELSEIF(FORNUM.EQ.5) THEN
            VAR = 'NC'
         ENDIF
 
      ELSE IF(REGN.EQ.6) THEN
C  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
C  BLUE MTN VARIANT
         IF(FORNUM.EQ.4 .OR. FORNUM.EQ.7 .OR. FORNUM.EQ.14 .OR. 
     >      FORNUM.EQ.16) THEN
            VAR = 'BM'
C  EASTERN CASCADES
         ELSEIF(FORNUM.EQ.17 .OR. FORNUM.EQ.8 .OR. (FORNUM.EQ.3 .AND. 
     >          DISTNUM .EQ. 3) .OR. (FORNUM.EQ.6 .AND. 
     >         (DISTNUM.EQ.1.OR.DISTNUM.EQ.2.OR.DISTNUM.EQ.6))) THEN
C  MOUNT HOOD Barlow RD
            VAR = 'EC'
C  SOUTHERN OREGON
         ELSEIF(FORNUM.EQ.1 .OR. FORNUM.EQ. 2 .OR. FORNUM.EQ.20) THEN
            VAR = 'SO'
C  WESTERN CASCADES
         ELSEIF(FORNUM.EQ.5 .OR. FORNUM.EQ.15 .OR. FORNUM.EQ.18 .OR. 
     >          FORNUM.EQ.10 .OR. FORNUM.EQ.3 .OR. FORNUM.EQ.6) THEN
            VAR = 'WC'
C  PACFIC NORTHWEST
         ELSEIF(FORNUM.EQ.9 .OR. FORNUM.EQ.12) THEN
            VAR = 'PN'
C  NORTHERN CALIFORNIA
         ELSEIF(FORNUM.EQ.11) THEN
            VAR = 'NC'
         ELSEIF(FORNUM.EQ.21) THEN
            VAR = 'IE'
         ENDIF
 
      ELSE IF(REGN.EQ.7) THEN
C  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
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
C
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R1_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
C      SEARCH BY FIA SPECIES CODE
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST,VAR
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(81)
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
     >                     299,  321,  375,  475,  544,  
     >                     740,  741,  745,  746,  747,  
     >                     749,  998,  999/
     
     
      DATA (EQNUM(I),I=1,81)/
     >'I00FW2W012','102DVEW017','I00FW2W017','I00FW2W019','102DVEW060',
     >'102DVEW106','I00FW2W019','I00FW2W073','I00FW2W093','102DVEW090',
     >'102DVEW090','I00FW2W119','102DVEW108','102DVEW106','I00FW2W108',
     >'I00FW2W073','I00FW2W119','I00FW2W122','102DVEW106','102DVEW106',
     >'I00FW2W202','616BEHW231','I00FW2W242','I00FW2W260','I00FW2W260',
     >'I00FW2W260','200DVEW746','101DVEW375','400DVEW475','101DVEW740',
     >'102DVEW740','101DVEW740','102DVEW740','102DVEW746','102DVEW740',
     >'102DVEW740','200DVEW746','I00FW2W260','203FW2W122','102DVEW740',
     >'101DVEW108','100JB2W202','101DVEW202','102DVEW202','100FW2W202',
     >'103DVEW202','101DVEW017','100JB2W017','101DVEW060','100JB2W108',
     >'104DVEW108','103DVEW108','102DVEW260','100JB2W122','104DVEW122',
     >'102DVEW122','105DVEW122','106DVEW122','101DVEW122','102DVEW019',
     >'101DVEW019','100JB2W073','102DVEW073','101DVEW073','101DVEW999',
     >'101DVEW260','102DVEW101','I12FW2W012','I11FW2W012','I13FW2W012',
     >'I14FW2W012','I21FW2W012','I22FW2W012','102DVEW070','101DVEW240',
     >'102DVEW240','102DVEW119','101DVEW119','103DVEW122','100JB2W019',
     >'101DVEW090'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,81
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
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

        IF(VOLEQ(1:7).EQ.'B00BEHW')THEN
C
C  FOUND VALID BLM/BIA/INDUSTRY LANDS EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        RETURN
      ENDIF
C
      LAST = 38
      READ(FORST,'(I2)')FORNUM
      DONE = 0

      IF(SPEC.EQ.122 .AND. FORNUM.EQ.8) THEN
         VOLEQ = EQNUM(39)
      ELSEIF(SPEC.EQ.101)THEN
         IF((VAR.EQ.'EM').OR.(VAR.EQ.'em').OR.
     &      (VAR.EQ.'IE').OR.(VAR.EQ.'ie').OR.
     &     (VAR.EQ.'CI').OR.(VAR.EQ.'ci'))THEN
         VOLEQ = EQNUM(1)
        ENDIF
      ELSEIF((VAR.EQ.'EM'.OR.VAR.EQ.'em').AND.((SPEC.EQ.745).OR.
     &       (SPEC.EQ.747).OR.(SPEC.EQ.749)))THEN
         VOLEQ=EQNUM(40)
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
      CHARACTER*10 EQNUM(67)
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
     >                     202, 242, 264, 299, 375,
     >                     475, 740, 745, 746, 749,
     >                     800, 803, 810, 814, 823,
     >                     843, 998/
   
      DATA (EQNUM(I),I=1,67)/
     >'200FW2W015','I00FW2W019','I00FW2W019','I00FW2W019','300DVEW060',
     >'300DVEW060','200DVEW065','300DVEW060','300DVEW060','200DVEW069',
     >'407FW2W093','407FW2W093','407FW2W093','407FW2W093','200FW2W122',
     >'200FW2W122','200DVEW106','200FW2W108','200FW2W122','200FW2W122',
     >'200FW2W122','200FW2W122','300DVEW106','300DVEW106','300DVEW106',
     >'200FW2W202','407FW2W093','407FW2W093','300DVEW060','300DVEW999',
     >'200DVEW475','300DVEW999','300DVEW999','200FW2W746','300DVEW999',
     >'300DVEW800','300DVEW800','300DVEW800','300DVEW800','200DVEW823',
     >'300DVEW800','200DVEW998','203FW2W122','213FW2W122','202FW2W108',
     >'200CZ2W202','200DVEW093','200CZ2W093','210DVEW093','200DVEW814',
     >'200DVEW108','210DVEW108','200CZ2W108','212DVEW122','213DVEW122',
     >'200CZ2W122','203CZ2W122','210DVEW122','200DVEW122','203DVEW122',
     >'200CZ2W746','200DVEW746','210DVEW746','200DVEW066','200CZ2W019',
     >'200CZ2W015','200DVEW015'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,67
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
      ELSE IF(SPEC.EQ.108 .AND. ((FORNUM.EQ.2).OR.(FORNUM.EQ.14))) THEN
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
C
      RETURN
      END
C
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R3_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(52)
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
     >                     143, 202, 242, 264, 299,  
     >                     310, 314, 375, 475, 740, 
     >                     745, 746, 749, 800, 803, 
     >                     810, 814, 823, 843, 998/

      DATA (EQNUM(I),I=1,52)/
     >'301DVEW015','300DVEW093','300DVEW093','300DVEW093','300DVEW060',
     >'300DVEW060','300DVEW060','300DVEW060','300DVEW060','300DVEW060',
     >'300DVEW060','301DVEW015','300DVEW093','300DVEW093','300DVEW093',
     >'300DVEW113','300DVEW113','300DVEW106','301DVEW202','300DVEW113',
     >'300DVEW113','300DVEW122','300FW2W122','300DVEW106','300DVEW106',
     >'300DVEW106','301DVEW202','301DVEW015','301DVEW015','300DVEW060',
     >'300DVEW310','300DVEW314','300DVEW999','300DVEW999','300DVEW999',
     >'300DVEW999','300DVEW746','300DVEW999','300DVEW800','300DVEW800',
     >'300DVEW800','300DVEW800','300DVEW800','300DVEW800','300DVEW999',
     >'302DVEW202','302DVEW202','302DVEW015','301FW2W122','301FW2W202',
     >'301FW2W015','301FW2W108'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,52
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
     >                    FORNUM.EQ.7)) THEN
         DONE=46
      ELSEIF(SPEC.EQ.15.AND.(FORNUM.EQ.2.OR.FORNUM.EQ.3.OR.
     >                       FORNUM.EQ.7)) THEN
         DONE=48
C    using Fleweling profile model for Ponderosa pine in following forest:
C    Apache Sitgreaves, Coconino, Kaibab, Tonto. The 300FW2W122 equation
c    is default for PP, so tht other forests 2,3,5,6,8,9,10 and 11 are
c    set to the DVE equation blow.
      ELSEIF(SPEC.EQ.122.AND.(FORNUM.EQ.2.OR.FORNUM.EQ.3.OR.
     >                        FORNUM.EQ.5.OR.FORNUM.EQ.6.OR.
     >                        FORNUM.EQ.8.OR.FORNUM.EQ.9.OR.
     >                        FORNUM.EQ.11))THEN
         DONE=22
C    Added profile model for DF, PP, WF and WP to Santa Fe NF
      ELSEIF(SPEC.EQ.122.AND.FORNUM.EQ.10) THEN
         DONE = 49
      ELSEIF(SPEC.EQ.202.AND.FORNUM.EQ.10) THEN
         DONE = 50
      ELSEIF(SPEC.EQ.15.AND.FORNUM.EQ.10) THEN
         DONE = 51    
      ELSEIF(SPEC.EQ.114.AND.FORNUM.EQ.10) THEN
         DONE = 52           
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
      ENDIF
      IF(DONE .LT. 0)THEN
C        Unknown
         VOLEQ = EQNUM(45)
      ELSE
         VOLEQ = EQNUM(DONE)   
      ENDIF
C
      RETURN
      END
C           
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R4_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG
      CHARACTER*10 EQNUM(57)
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
     >                       231,  242,  263,  264,  299,  
     >                       313,  321,  322,  475,  746,    
     >                       747,  748,  749,  800,  814,
     >                       998,  999/

      DATA (EQNUM(I),I=1,57)/
     >'400MATW020','300DVEW060','300DVEW060','400DVEW066','400MATW073',
     >'400MATW081','300DVEW106','400MATW117','400MATW117','400DVEW133',
     >'400DVEW998','400MATW081','400MATW015','401MATW015','400MATW108',
     >'400MATW108','400MATW108','400MATW108','400DVEW475','400MATW746',
     >'400DVEW998','400MATW108','400MATW108','300DVEW800','300DVEW800',
     >'400DVEW998','400DVEW998','I15FW2W017','401MATW015','400MATW015',
     >'I15FW2W017','405MATW019','400MATW019','401DVEW065','400DVEW065',
     >'I15FW2W093','407FW2W093','400MATW093','401MATW108','400MATW108',
     >'I15FW2W122','401MATW122','402MATW122','403MATW122','400MATW122',
     >'I15FW2W202','405MATW202','400MATW202','400DVEW064','400DVEW106',
     >'407MATW093','401MATW202','407MATW093','401MATW202','401MATW202',
     >'407MATW093','407MATW093'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,57
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
      IF(SPEC.EQ.15) THEN
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
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13)THEN
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
      ELSEIF(SPEC.EQ.64) THEN
         IF((FORNUM.EQ.1).OR.(FORNUM.EQ.4).OR.(FORNUM.EQ.7).OR.
     &      (FORNUM.EQ.8).OR.(FORNUM.EQ.9).OR.(FORNUM.EQ.10).OR.
     &      (FORNUM.EQ.17).OR.(FORNUM.EQ.18).OR.(FORNUM.EQ.19))THEN
            VOLEQ=EQNUM(49)
         ELSEIF((FORNUM.EQ.2).OR.(FORNUM.EQ.6).OR.(FORNUM.EQ.12).OR.
     &      (FORNUM.EQ.13).OR.(FORNUM.EQ.14))THEN
            VOLEQ=EQNUM(49)
         ENDIF
      ELSEIF(SPEC.EQ.65) THEN
         IF (FORNUM.EQ.3.OR.FORNUM.EQ.5.OR.
     &       FORNUM.EQ.15.OR.FORNUM.EQ.16) THEN
            VOLEQ = EQNUM(34)
          ELSE
            VOLEQ = EQNUM(35)
          ENDIF
C     Engelmann spruce
C     Blue spruce
      ELSEIF(SPEC.EQ.93.OR.SPEC.EQ.96) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13)THEN
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
C     pinyon pine
      ELSEIF(SPEC.EQ.106) THEN
         IF((FORNUM.EQ.1).OR.(FORNUM.EQ.4).OR.(FORNUM.EQ.7).OR.
     &      (FORNUM.EQ.8).OR.(FORNUM.EQ.9).OR.(FORNUM.EQ.10).OR.
     &      (FORNUM.EQ.17).OR.(FORNUM.EQ.18).OR.(FORNUM.EQ.19))THEN
            VOLEQ=EQNUM(50)
         ENDIF
C     Ponderosa pine
      ELSEIF(SPEC.EQ.122) THEN
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13)THEN
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
         IF (FORNUM.EQ.2.OR.FORNUM.EQ.12.OR.FORNUM.EQ.13)THEN
            VOLEQ = EQNUM(46)
         ELSEIF (FORNUM.EQ.05)THEN
            VOLEQ = EQNUM(47)
          ELSE
            VOLEQ = EQNUM(48)
          ENDIF
C     Other hardwoods
      ELSEIF(SPEC.EQ.998) THEN
        IF((FORNUM.EQ.2).OR.(FORNUM.EQ.6).OR.(FORNUM.EQ.12).OR.
     &        (FORNUM.EQ.13).OR.(FORNUM.EQ.14))THEN
              VOLEQ=EQNUM(17)
        ELSE
              VOLEQ=EQNUM(21)
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
      CHARACTER*10 EQNUM(107)
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
     >                       263,  264,  299,  301,  312,
     >                       333,  351,  352,  361,  431,
     >                       475,  478,  492,  510,  542,
     >                       600,  631,  671,  730,  746, 
     >                       747,  768,  801,  805,  807,
     >                       811,  815,  818,  821,  839,
     >                       920,  981,  998,  999/

      DATA (EQNUM(I),I=1,107)/
     >'500WO2W015','500WO2W015','500WO2W015','500WO2W020','500WO2W020',
     >'500WO2W020','500WO2W020','500WO2W081','500DVEW060','500DVEW060',
     >'500DVEW060','500WO2W202','500WO2W081','500WO2W015','500WO2W015',
     >'500WO2W108','500WO2W108','500WO2W108','500WO2W108','500WO2W108',
     >'500WO2W108','500WO2W116','500WO2W117','500WO2W117','500WO2W122',
     >'500WO2W108','500WO2W108','500WO2W116','500WO2W117','500WO2W108',
     >'500WO2W202','500WO2W202',
     >'500WO2W211','500DVEW212','500WO2W108','500WO2W081','500DVEW807',
     >'500WO2W015','500WO2W015','500WO2W108','H00SN2W301','500DVEW312',
     >'500DVEW807','500DVEW351','500DVEW351','500DVEW361','500DVEW431',
     >'500DVEW801','500DVEW801','500DVEW807','H01SN2W510','500DVEW807',
     >'500DVEW818','500DVEW631','H00SN2W671','500DVEW818','500DVEW818',
     >'500DVEW818','500DVEW801','500DVEW801','500DVEW805','500DVEW807',
     >'500DVEW811','500DVEW815','500DVEW818','500DVEW821','500DVEW839',
     >'500DVEW807','500DVEW981','500DVEW981','500DVEW631','532WO2W015',
     >'532WO2W020','532WO2W081','532WO2W108','532WO2W117','532WO2W122',
     >'532WO2W202','H00SN2W510','532WO2W020','532WO2W202','H00SN2W510',
     >'532WO2W081','532WO2W116','616BEHW000','532WO2W108','532WO2W122',
     >'532WO2W211','532WO2W117','532WO2W015','516TRFW015','532TRFW015',
     >'516TRFW021','532TRFW021','516TRFW060','532TRFW060','516TRFW081',
     >'532TRFW081','516TRFW108','532TRFW108','516TRFW117','532TRFW117',
     >'516TRFW122','532TRFW122','516TRFW202','616TRFW211','632TRFW211'/
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        DO I=1,107
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
C     white fir
c      IF(SPEC.EQ.15)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
C                 VOLEQ=EQNUM(72)
c              DONE=72
c              ENDIF
c          ENDIF
C     California red fir
c      ELSEIF(SPEC.EQ.20)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
C                 VOLEQ=EQNUM(73)
c              DONE=73
c              ENDIF
c          ENDIF
C     incense cedar
c      ELSEIF(SPEC.EQ.81)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
C                 VOLEQ=EQNUM(74)
c                  DONE=74
c              ENDIF
c          ENDIF
C     Whitebark pine
      IF(SPEC.EQ.101) THEN
          IF(VAR.EQ.'SO' .OR. VAR.EQ.'so') THEN
               DONE=30
          ELSE
               DONE=40   
          ENDIF
C     lodgepole pine
c      ELSEIF(SPEC.EQ.108)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
c              DONE=75
c              ENDIF
c          ENDIF
C     sugar pine
c      ELSEIF(SPEC.EQ.117)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
c                 DONE=76
c              ENDIF
c          ENDIF
C     ponderosa pine
c      ELSEIF(SPEC.EQ.122)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
c                 DONE=77
c              ENDIF
c          ENDIF
C     Douglas fir
c      ELSEIF(SPEC.EQ.202)THEN
c          IF((VAR.EQ.'CA').OR.(VAR.EQ.'ca').OR.
c     &       (VAR.EQ.'OC').OR.(VAR.EQ.'oc'))THEN
c              IF((FORNUM.EQ.5).OR.(FORNUM.EQ.6).OR.
c     >           (FORNUM.EQ.8).OR.(FORNUM.EQ.14))THEN
c                 DONE=78
c              ENDIF
c          ENDIF
C     other softwoods
      ELSEIF(SPEC.EQ.299 .OR. SPEC.EQ.290) THEN
          IF(VAR.EQ.'SO' .OR. VAR.EQ.'so') THEN
              DONE=30
          ELSEIF(VAR.EQ.'NC' .OR. VAR.EQ.'nc') THEN
              DONE=30
          ELSE
              DONE=40
          ENDIF
C     other hardwoods
      ELSEIF(SPEC.EQ.998.) THEN
          IF(VAR.EQ.'SO' .OR. VAR.EQ.'so') THEN
              DONE=70
          ELSEIF(VAR.EQ.'WS' .OR. VAR.EQ.'ws') THEN
              DONE=67
          ELSEIF(VAR.EQ.'NC' .OR. VAR.EQ.'nc') THEN
              DONE=70
          ELSE
              DONE=60
          ENDIF
C      ELSE
       ENDIF
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
  5    CONTINUE 
      
       IF(DONE .LT. 0) THEN
           VOLEQ = EQNUM(71)
       ELSE
           VOLEQ = EQNUM(DONE)   
       ENDIF
C      ENDIF
      
      RETURN
      END
      
      
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R6_EQN(VAR,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST,VAR,DIST
      CHARACTER*3 ASPEC
      INTEGER SPEC,ERRFLAG,FORNUM,I,FIA(53),DISTNUM
      CHARACTER*10 EQNUMI(87),EQNUM(44)
      CHARACTER*10 EQNUMC(39),EQNUMF(50),EQNUMD(15)
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
     >                       290, 299, 312, 321, 351,
     >                       352, 361, 375, 431, 475,
     >                       478, 492, 500, 631, 740,
     >                       746, 747, 768, 815, 818,
     >                       920, 998, 999/
C
      DATA (EQNUMI(I),I=1,87)/
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
     >'I14FW2W122','I14FW2W202','I14FW2W242','I14FW2W260','I14FW2W263',
     >'I00FW2W012','I00FW2W202','I00FW2W093','I00FW2W017','I00FW2W108',
     >'I00FW2W122','I00FW2W019','I00FW2W073','I00FW2W119','I00FW2W242',
     >'I00FW2W260','I13FW2W202','I11FW2W202','I12FW2W202','I14FW2W017',
     >'I13FW2W017','I14FW2W122','I12FW2W122','I13FW2W122','I11FW2W122',
     >'I11FW2W017','I11FW2W073','I11FW2W242','I12FW2W017','I12FW2W073',
     >'I13FW2W073','I14FW2W073'/
     
      DATA (EQNUMF(I),I=1,50)/
     >'F00FW2W202','F00FW2W242','F00FW2W263','F01FW2W202','F01FW2W242',
     >'F01FW2W263','F02FW2W202','F02FW2W242','F02FW2W263','F03FW2W202',
     >'F03FW2W242','F03FW2W263','F04FW2W202','F04FW2W242','F04FW2W263',
     >'F05FW2W202','F05FW2W242','F05FW2W263','F06FW2W202','F06FW2W242',
     >'F06FW2W263','F07FW2W202','F07FW2W242','F07FW2W263','F08FW2W202',
     >'F08FW2W242','F08FW2W263','F03FW2W202','F01FW2W202','F02FW2W202',
     >'F00FW2W202','F04FW2W202','F08FW2W202','F07FW2W202','F06FW2W202',
     >'F05FW2W202','F00FW2W242','F00FW2W260','F01FW2W242','F01FW2W260',
     >'F02FW2W242','F02FW2W260','F03FW2W242','F03FW2W260','F04FW2W242',
     >'F04FW2W260','F05FW2W260','F06FW2W260','F07FW2W260','F08FW2W260'/

      DATA (EQNUMC(I),I=1,39)/
     >'I21FW2W012','I21FW2W017','I21FW2W019','I21FW2W073','I21FW2W093',
     >'I21FW2W108','I21FW2W119','I21FW2W122','I21FW2W202','I21FW2W242',
     >'I21FW2W260','I22FW2W012','I22FW2W017','I22FW2W019','I22FW2W073',
     >'I22FW2W093','I22FW2W108','I22FW2W119','I22FW2W122','I22FW2W202',
     >'I22FW2W242','I22FW2W260','I23FW2W012','I23FW2W017','I23FW2W019',
     >'I23FW2W073','I23FW2W093','I23FW2W108','I23FW2W119','I23FW2W122',
     >'I23FW2W202','I23FW2W242','I23FW2W260','I22FW2W122','I21FW2W122',
     >'I23FW2W122','I21FW2W073','I22FW2W073','I23FW2W073'/

      DATA (EQNUMD(I),I=1,15)/
     >'601DVEW205','601DVEW263','601DVEW015','602DVEW122','602DVEW204',
     >'602DVEW015','602DVEW108','602DVEW122','601DVEW205','602DVEW204',
     >'602DVEW108','600DVEW122','601DVEW015','601DVEW263','602DVEW015'/

      DATA (EQNUM(I),I=1,44)/
     >'B00BEHW011','B00BEHW015','B00BEHW017','B00BEHW015','B00BEHW021',
     >'B00BEHW021','B00BEHW022','B00BEHW041','B00BEHW042','B00BEHW242',
     >'B00BEHW073','B00BEHW073','B00BEHW081','B00BEHW093','B00BEHW098',
     >'B00BEHW119','B00BEHW108','B00BEHW108','B00BEHW116','B00BEHW117',
     >'B00BEHW119','B00BEHW122','B01BEHW202','B00BEHW211','B00BEHW231',
     >'B00BEHW242','B00BEHW260','B00BEHW260','B00BEHW260','B00BEHW312',
     >'B00BEHW351','B00BEHW361','B00BEHW361','B00BEHW431','B00BEHW542',
     >'B00BEHW631','B00BEHW747','B00BEHW800','B00BEHW800','B00BEHW998',
     >'B00BEHW999','B02BEHW202','B03BEHW202','B01BEHW202'/
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

        if(VOLEQ(1:7).EQ.'628BEHW')THEN
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
C
C  CHECK FOR VALID PNW TARRIF EQUATION
C
        IF(VOLEQ(1:7).EQ.'616TRFW')THEN
          SPEC=8888
          RETURN
        ENDIF

        IF(VOLEQ(1:7).EQ.'632TRFW')THEN
         SPEC=8888
         RETURN
       ENDIF

        DO I=1,44
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID BLM/BIA/INDUSTRY LANDS EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,87
        IF(VOLEQ.EQ.EQNUMI(I))THEN
C
C  FOUND VALID INGY EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,50
        IF(VOLEQ.EQ.EQNUMF(I))THEN
C
C  FOUND VALID WESTSIDE FLEWELLING EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,39
        IF(VOLEQ.EQ.EQNUMC(I))THEN
C
C  FOUND VALID INGY EQUATION NUMBER - CANADIAN MODEL
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO

        DO I=1,15
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
     >   VAR.EQ.'CA' .OR. VAR.EQ.'OC' .OR. VAR.EQ.'OP')THEN
         
c        Gifford Pinchot
         IF(FORNUM.EQ.3)THEN
            IF(SPEC.EQ. 11) THEN
                DONEI = 26
            ELSE IF (SPEC.EQ.19) THEN
                DONEI=6 
            ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEF = 3
                IF(DISTNUM.EQ.1) THEN
                  DONEF = 21
                ELSE IF (DISTNUM.EQ.5) THEN
                   DONEF = 15
                ENDIF
            ELSE IF(SPEC.EQ.202) THEN
                DONEF = 10
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
             ELSE IF(SPEC.EQ.17) THEN
                DONEI = 38
             ELSE IF(SPEC.EQ.93)THEN
                DONEI = 17
             ELSE IF(SPEC.EQ.108)THEN
                DONEI = 18
             ELSE IF(SPEC.EQ.122)THEN
                DONEI = 32
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEI = 23
             ELSE IF(SPEC.EQ.22) THEN
                DONEI = 38
             ELSE IF(SPEC.EQ.202) THEN
                DONEF = 10
                IF(DISTNUM.EQ.5 .OR. DISTNUM.EQ.9) DONEF = 10
              END IF
c        Mt Baker - Snoqualmie
         ELSE IF(FORNUM.EQ.5)THEN
             IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEF = 12
             ELSE IF(SPEC.EQ.202) THEN
                DONEF = 25
             ENDIF
c        Rogue River/Siskiyo
         ELSE IF(FORNUM.EQ.10.OR.FORNUM.EQ.11) THEN
             IF(SPEC.EQ.15) THEN
                IF((VAR.NE.'NC').OR.(VAR.NE.'nc'))THEN
                   DONEI = 5
                ENDIF
             ELSE IF(SPEC.EQ.122) THEN
                IF((VAR.NE.'NC').OR.(VAR.NE.'nc'))THEN
                   DONEI = 4
                ENDIF
             ELSE IF(SPEC.EQ.202) THEN
                DONEF = 19
             ENDIF
c        Siuslaw
         ELSE IF(FORNUM.EQ.12) THEN
             IF(SPEC.EQ.202) THEN
                DONEF = 1
             ELSE IF(SPEC.EQ.263) THEN
                DONEF = 12
             ENDIF
c        Olympic
         ELSE IF(FORNUM.EQ.9) THEN
             IF(SPEC.EQ.202) THEN
                DONEF = 10
             ELSE IF(SPEC.EQ.98) THEN
                DONEF = 12
             ELSE IF(SPEC.EQ.263) THEN
                DONEF = 3
             ENDIF
c        Umpqua
         ELSE IF(FORNUM.EQ.15) THEN
             IF(SPEC.EQ.15) THEN
                DONEI = 2
             ELSE IF(SPEC.EQ.20)THEN
                DONEI = 1
             ELSE IF(SPEC.EQ.81)THEN
                DONEI = 4
             ELSE IF(SPEC.EQ.93)THEN
                DONEI = 5
             ELSE IF(SPEC.EQ.108)THEN
                DONEI = 6
             ELSE IF(SPEC.EQ.122)THEN
                DONEI = 4
             ELSE IF(SPEC.EQ.202)THEN
                DONEF = 1
             ELSE IF(SPEC.EQ.242)THEN
                DONEI = 1
             ELSE IF(SPEC.EQ.263)THEN
                DONEI = 23
             ELSE IF(SPEC.EQ.264)THEN
                DONEI = 10
             ENDIF
c         Willamette
         ELSE IF(FORNUM.EQ.18) THEN
             IF(SPEC.EQ.22) THEN
                DONEI = 6
             ELSE IF(SPEC.EQ.202)THEN
                DONEF = 16
             ELSE IF(SPEC.EQ.263)THEN
                DONEF = 12
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
               VOLEQ = '616BEHW000'
            ELSE
                IF(FIA(DONEF) .LT. 10) THEN
                  WRITE(ASPEC,'(I1)')FIA(DONEF)
                  VOLEQ(1:9) = '616BEHW00'
                  VOLEQ(10:10) = ASPEC
                ELSE IF (FIA(DONEF) .LT. 100) THEN
                  WRITE(ASPEC,'(I2)')FIA(DONEF)
                  VOLEQ(1:8) = '616BEHW0'
                  VOLEQ(9:10) = ASPEC
                ELSE
                  WRITE(ASPEC,'(I3)')FIA(DONEF)
                  VOLEQ(1:7) = '616BEHW'
                  VOLEQ(8:10) = ASPEC
                ENDIF
            ENDIF
          ENDIF

c     Eastside Variants          
      ELSE
C        first check for INGY equations by forest and species
c        Deschutes          
         IF(FORNUM.EQ.1) THEN
            IF(SPEC.EQ.11 .OR. SPEC.EQ.15 .OR. SPEC.EQ.17
     &                    .OR. SPEC.EQ.21) THEN
               IF((VAR.NE.'SO').OR.(VAR.NE.'so'))DONEI = 2
            ELSE IF(SPEC.EQ. 73) THEN
               DONEI = 16
            ELSE IF(SPEC.EQ. 108) THEN
               DONEI = 18
            ELSE IF(SPEC.EQ. 122) THEN
               DONEI = 20
            ELSE IF(SPEC.EQ.202 ) THEN
               DONEI = 21
            ELSE IF(SPEC.EQ. 81) THEN
               DONEI = 22
            ENDIF
c        Fremont
         ELSEIF(FORNUM.EQ.2 .OR. FORNUM.EQ.20) THEN
             IF(SPEC.EQ. 15 .OR. SPEC.EQ.17) THEN
               DONEI = 14
             ELSE IF(SPEC.EQ.81) THEN
               DONEI = 9 
             ELSE IF(SPEC.EQ.108) THEN
               DONEI = 6
             ELSE IF(SPEC.EQ.122) THEN
               DONEI = 8
             ELSE IF(SPEC.EQ.202) THEN
              DONEI = 2
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
            ELSE IF(SPEC.EQ.17) THEN
                DONEI = 38
            ELSE IF(SPEC.EQ.93) THEN
                DONEI = 17
            ELSE IF(SPEC.EQ.108) THEN
                DONEI = 18
            ELSE IF(SPEC.EQ.122) THEN
                DONEI = 32
            ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.260) THEN
                DONEI = 23
            ELSE IF(SPEC.EQ.22) THEN
                DONEI = 38
            ELSE IF(SPEC.EQ.202) THEN
                DONEF=16
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
            ELSE IF(SPEC.EQ.73) THEN
                DONEI = 40
            ELSE IF(SPEC.EQ.93) THEN
                DONEI = 17
            ELSE IF(SPEC.EQ.108) THEN
                DONEI = 6
            ELSE IF(SPEC.EQ.122) THEN
               DONEI = 20
            ELSE IF(SPEC.EQ.202) THEN
               DONEI = 21
            ENDIF
c        Okanogan - Wenatchee
         ELSE IF(FORNUM.EQ.8 .OR. FORNUM.EQ.17) THEN
            IF(SPEC.EQ. 17) THEN
                DONEI = 14
                IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR. DISTNUM.EQ.5  
     >                                  .OR. DISTNUM.EQ.7 ) DONEI = 14
            ELSE IF(SPEC.EQ.202) THEN
                DONEI = 33
                IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR. DISTNUM.EQ.4  
     >              .OR. DISTNUM.EQ.5 .OR. DISTNUM.EQ.7 .OR. 
     >              DISTNUM.EQ.9) DONEI = 33
            ELSE IF(SPEC.EQ.108) THEN
                DONEI = 30
                IF(DISTNUM.EQ.4) DONEI = 30
            ELSE IF(SPEC.EQ.93) THEN
                DONEI = 17
                IF(DISTNUM.EQ.9) DONEI = 17
            ELSE IF(SPEC.EQ.122.OR.SPEC.EQ.73) THEN
                DONEI = 32
                IF(DISTNUM.EQ.4) THEN
                  DONEI = 32
                ELSE IF(DISTNUM.EQ.2 .OR. DISTNUM.EQ.3 .OR.   
     >            DISTNUM.EQ.5.OR. DISTNUM.EQ.7) THEN
                  DONEI = 20
                ENDIF
            ENDIF
c        Colville
         ELSE IF(FORNUM.EQ.21) THEN
             IF(SPEC.EQ.17) THEN
                DONEI = 14
             ELSE IF(SPEC.EQ.19)THEN
                DONEI = 21
             ELSE IF(SPEC.EQ.73)THEN
                DONEI = 16
             ELSE IF(SPEC.EQ.93)THEN
                DONEI = 41
             ELSE IF(SPEC.EQ.108)THEN
                DONEI = 18
             ELSE IF(SPEC.EQ.119)THEN
                DONEI = 7
             ELSE IF(SPEC.EQ.122)THEN
                DONEi = 32
             ELSE IF(SPEC.EQ.202)THEN
                DONEI = 21
             ELSE IF(SPEC.EQ.242)THEN
                DONEI = 22
             ELSE IF(SPEC.EQ.263 .OR. SPEC.EQ.264)THEN
                DONEI = 14
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
                  VOLEQ(1:9) = '616BEHW00'
                  VOLEQ(10:10) = ASPEC
                ELSE IF (FIA(DONEF) .LT. 100) THEN
                  WRITE(ASPEC,'(I2)')FIA(DONEF)
                  VOLEQ(1:8) = '616BEHW0'
                  VOLEQ(9:10) = ASPEC
                ELSE
                  WRITE(ASPEC,'(I3)')FIA(DONEF)
                  VOLEQ(1:7) = '616BEHW'
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
      CHARACTER*10 EQNUM(45)
      CHARACTER*10 EQNUMI(60)
      CHARACTER*10 EQNUMC(33),EQNUMF(27),EQNUMD(8)
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

      DATA (EQNUM(I),I=1,45)/
     >'B00BEHW011','B00BEHW015','B00BEHW017','B00BEHW015','B00BEHW021',
     >'B00BEHW021','B00BEHW022','B00BEHW041','B00BEHW042','B00BEHW242',
     >'B00BEHW073','B00BEHW073','B00BEHW081','B00BEHW093','B00BEHW098',
     >'B00BEHW119','B00BEHW108','B00BEHW108','B00BEHW116','B00BEHW117',
     >'B00BEHW119','B00BEHW122','B01BEHW202','B00BEHW211','B00BEHW231',
     >'B00BEHW242','B00BEHW260','B00BEHW260','B00BEHW260','B00BEHW312',
     >'B00BEHW351','B00BEHW361','B00BEHW361','B00BEHW431','B00BEHW542',
     >'B00BEHW631','B00BEHW747','B00BEHW800','B00BEHW800','B00BEHW998',
     >'B00BEHW999',
     >'B02BEHW202','B03BEHW202','B01BEHW202','B00BEHW263'/
C
C  REGION 6 EQUATIONS
C
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
        DO I=1,44
        IF(VOLEQ.EQ.EQNUM(I))THEN
C
C  FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
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
      DONE = 0

      LAST = 41

      FIRST = 1
      IF(SPEC.EQ.202) THEN
          IF(VAR.EQ.'WC' .OR. VAR.EQ.'wc')THEN
            DONE=44
          ELSEIF((VAR.EQ.'PN') .OR. (VAR.EQ.'pn') .OR.
     &       (VAR.EQ.'NC') .OR. (VAR.EQ.'nc') .OR.
     &       (VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &       (VAR.EQ.'OC') .OR. (VAR.EQ.'oc') .OR.
     &       (VAR.EQ.'OP') .OR. (VAR.EQ.'op')) THEN
            DONE = 44
            IF(FORNUM.EQ.12)DONE=42
          ELSE
                DONE = 44
          ENDIF
      ELSEIF(SPEC.EQ.41)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=13
         ENDIF
      ELSEIF(SPEC.EQ.263)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'PN') .OR. (VAR.EQ.'pn') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc') .OR.
     &      (VAR.EQ.'OP') .OR. (VAR.EQ.'op')) THEN
            DONE=45
         ENDIF
      ELSEIF((SPEC.EQ.109).OR.(SPEC.EQ.113).OR.(SPEC.EQ.124).OR.
     &   (SPEC.EQ.127))THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=18
         ENDIF
      ELSEIF(SPEC.EQ.92)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=14
         ENDIF
      ELSEIF(SPEC.EQ.212)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=24
         ENDIF
      ELSEIF((SPEC.EQ.801).OR.(SPEC.EQ.805).OR.(SPEC.EQ.807).OR.
     &       (SPEC.EQ.811).OR.(SPEC.EQ.818).OR.(SPEC.EQ.821).OR.
     &       (SPEC.EQ.839).OR.(SPEC.EQ.333).OR.(SPEC.EQ.730))THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=38
         ELSEIF((VAR.EQ.'NC').OR.(VAR.EQ.'nc'))THEN
            IF(SPEC.EQ.818)DONE=38
         ENDIF
      ELSEIF(SPEC.EQ.542)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=30
         ENDIF
      ELSEIF(SPEC.EQ.251)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
           DONE=25
         ENDIF
      ELSEIF(SPEC.EQ.981)THEN
         IF((VAR.EQ.'CA') .OR. (VAR.EQ.'ca') .OR.
     &      (VAR.EQ.'OC') .OR. (VAR.EQ.'oc'))THEN
            DONE=36
         ENDIF
       ENDIF
C
C  FINDS SPECIES INDEX TO FIA ARRAY
C
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
C      
      IF(DONE .LT. 0) THEN
          VOLEQ = 'B00BEHW999'
      ELSE
          VOLEQ=EQNUM(DONE)
      ENDIF
      RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      CHARACTER*2 GEOAREA,GEOCODES(33)
      CHARACTER*2 PROD,VAR,FORST,DIST
      CHARACTER*10 VOLEQ,VEQTEM
      INTEGER SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J
      CHARACTER*3 SNSP(92)
      INTEGER SNFIA(92)

c     match species to valid species equation code
      DATA (SNFIA(I),I=1,92)/
     >  10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
     > 129, 130, 131, 132, 221, 222, 260, 299, 311, 313, 
     > 316, 317, 318, 330, 370, 372, 391, 400, 450, 460, 
     > 471, 491, 521, 531, 540, 541, 543, 544, 552, 555, 
     > 580, 591, 601, 602, 611, 621, 650, 651, 652, 653, 
     > 654, 660, 680, 691, 693, 694, 701, 711, 721, 731, 
     > 740, 743, 762, 802, 806, 812, 813, 819, 820, 822, 
     > 824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838, 
     > 901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

      DATA (SNSP(I), I=1,92)/
     &'261','100','115','132','110','111','115','121','126','126',
     &'128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800',
     &'835',
     &'901','300','300','300','970','970','970','970','300','300'/

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
        DO J=1,92
        VEQTEM(8:10)=SNSP(J)
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
                     GEOAREA = '13';
              ELSE IF (DISTNUM.EQ.3) THEN
                     GEOAREA = '15';
              ELSE IF (DISTNUM.EQ.4) THEN
                     GEOAREA = '16';
              ELSE IF (DISTNUM.EQ.5 .OR. DISTNUM.EQ.6) THEN
                 GEOAREA = '14';
              ELSE
                 GEOAREA = '17';
         ENDIF

       ELSE IF (FORNUM.EQ.2)THEN
C                     // daniel boone
                     GEOAREA = '10';

       ELSE IF (FORNUM.EQ.3)THEN
C                     // Chattahoochee/Oconee
                     GEOAREA = '01';
                     IF (DISTNUM .EQ. 8) GEOAREA = '06';

       ELSE IF (FORNUM.EQ.4 .OR. FORNUM .EQ. 60)THEN
C                     // Cherokee
C                      // land between the lakes
                     GEOAREA = '01';

       ELSE IF (FORNUM.EQ.5)THEN
C                     // Florida
                     GEOAREA = '03';

       ELSE IF (FORNUM.EQ.6) THEN
C                     // Kisatchie
                     GEOAREA = '02';
                     IF (DISTNUM .EQ. 6) GEOAREA = '09';

       ELSE IF (FORNUM .EQ. 7) THEN  
C                     // Mississippi
                     
              IF (DISTNUM.EQ.1) THEN
                     GEOAREA = '19';
              ELSE IF (DISTNUM.EQ.2) THEN
                     GEOAREA = '20';
              ELSE IF (DISTNUM.EQ.4) THEN
                     GEOAREA = '21';
              ELSE IF (DISTNUM.EQ.5) THEN
                 GEOAREA = '22';
              ELSE IF (DISTNUM.EQ.6) THEN
                 GEOAREA = '18';
              ELSE
                 GEOAREA = '23';
         END IF

       ELSE IF (FORNUM .EQ. 8) THEN  
C                     // GW/Jeff
          GEOAREA = '11';
                     
         IF (DISTNUM.EQ.11 .OR. DISTNUM.EQ.12 .OR. DISTNUM.EQ.13 .OR. 
     >       DISTNUM.EQ.14 .OR. DISTNUM.EQ.15 .OR. DISTNUM.EQ.16) THEN
            GEOAREA = '12';
         END IF

       ELSE IF (FORNUM .EQ. 9) THEN
C                     // Ouachita
              GEOAREA = '31';
              IF (DISTNUM.EQ.1 .OR. DISTNUM.EQ.6) THEN
                     GEOAREA = '30';
              ELSE IF (DISTNUM.EQ.12) THEN
                 GEOAREA = '32';
         END IF
       
      ELSE IF (FORNUM .EQ. 10) THEN   
C                     // Ozark/St Francis
                     GEOAREA = '04';
                 IF (DISTNUM .EQ. 7) GEOAREA = '05';

       ELSE IF (FORNUM .EQ. 11) THEN
C                     // North Carolina
                     GEOAREA = '01';
                     IF (DISTNUM .EQ. 3)THEN
                        GEOAREA = '07';
                     ELSE IF (DISTNUM .EQ. 10) THEN
                            GEOAREA = '08';
                     ENDIF

       ELSE IF (FORNUM .EQ. 12)THEN
C                     // Francis Marion/Sumpter
                 GEOAREA = '24';
                     IF(DISTNUM .EQ. 2) THEN
                        GEOAREA = '01';
                     ELSE IF(DISTNUM .EQ. 5)THEN 
                            GEOAREA = '25';
                     ENDIF

       ELSE IF (FORNUM.EQ.13) THEN
C                      // Texas
                     IF(DISTNUM .EQ. 1) THEN
                        GEOAREA = '26';
                     ELSE IF(DISTNUM .EQ. 3)THEN 
                            GEOAREA = '27';
                     ELSE IF(DISTNUM .EQ. 4)THEN 
                            GEOAREA = '29';
                     ELSE
                            GEOAREA = '28';
                     ENDIF

       ELSE IF (FORNUM.EQ.36)THEN
C            // savannah river
                     GEOAREA = '25';

      ENDIF

C     CREATE THE VOLUME EQUATION NUMBER
      
      VOLEQ(1:1) = '8'
      VOLEQ(2:3) = GEOAREA
      VOLEQ(4:7) = 'DVEE'

C     FIND CORRECT SPECIES
      DONE = 0
      FIRST = 1
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
  5   CONTINUE 
      IF(DONE .LT. 0) THEN
        IF(SPEC.LT.300) THEN
C USE OTHER SOFTWOOD 299
          DONE = 19
        ELSE
          DONE = 92
        ENDIF
      ENDIF

      VOLEQ(8:10) = SNSP(DONE)
      RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
      CHARACTER*1 GEOAREA,TOPCODE(5),ICHAR
      CHARACTER*2 PROD,VAR,FORST,DIST
      CHARACTER*10 VOLEQ,VEQTEM
      CHARACTER*3 SNSP(92)
      INTEGER SNFIA(92)
      INTEGER SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J,K

c     match species to valid species equation code
      DATA (SNFIA(I),I=1,92)/
     >  10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
     > 129, 130, 131, 132, 221, 222, 260, 299, 311, 313, 
     > 316, 317, 318, 330, 370, 372, 391, 400, 450, 460, 
     > 471, 491, 521, 531, 540, 541, 543, 544, 552, 555, 
     > 580, 591, 601, 602, 611, 621, 650, 651, 652, 653, 
     > 654, 660, 680, 691, 693, 694, 701, 711, 721, 731, 
     > 740, 743, 762, 802, 806, 812, 813, 819, 820, 822, 
     > 824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838, 
     > 901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

      DATA (SNSP(I), I=1,92)/
     &'261','100','115','132','110','111','115','121','126','126',
     &'128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800',
     &'835',
     &'901','300','300','300','970','970','970','970','300','300'/
C
      DATA TOPCODE / '1','4','7','8','9' /
C
C  SEARCH FOR VALID EQUATION NUMBER
C
      IF(SPEC.EQ.9999)THEN
        VEQTEM(1:1)='8'
        VEQTEM(4:7)='CLKE'
        DO I=1,7
        WRITE(ICHAR,'(I1)')I
        VEQTEM(2:2)=ICHAR
        DO J=1,5
        VEQTEM(3:3)=TOPCODE(J)
C  SN
        DO K=1,92
        VEQTEM(8:10)=SNSP(K)
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
              GEOAREA = '4';
              IF (DISTNUM.EQ.3) GEOAREA = '1'

       ELSE IF (FORNUM.EQ.2 .OR. FORNUM.EQ.4 .OR. FORNUM .EQ. 8 .OR. 
     >          FORNUM .EQ. 60)THEN
C                     // daniel boone
C                     // Cherokee
C                     // GW/Jeff
C                 // land between the lakes
                     GEOAREA = '3';

       ELSE IF (FORNUM.EQ.3)THEN
C                     // Chattahoochee/Oconee
                     GEOAREA = '3';
                     IF (DISTNUM .EQ. 8) GEOAREA = '2';

       ELSE IF (FORNUM.EQ.5 .OR. FORNUM.EQ.36)THEN
C                     // Florida
C           // savannah river
                     GEOAREA = '1';

       ELSE IF (FORNUM.EQ.6 .OR. FORNUM.EQ.13) THEN
C                     // Kisatchie
C                     // Texas
                     GEOAREA = '5';

       ELSE IF (FORNUM .EQ. 7) THEN  
C                     // Mississippi
                     GEOAREA = '5';
                     
            IF (DISTNUM .EQ. 6) THEN
                     GEOAREA = '7';
            ELSE IF (DISTNUM.EQ.7 .OR. DISTNUM.EQ.17) THEN
                     GEOAREA = '4';
            ENDIF
       ELSE IF (FORNUM .EQ. 9) THEN
C                     // Ouachita
                     GEOAREA = '6';
       ELSE IF (FORNUM .EQ. 10) THEN   
C                     // Ozark/St Francis
                     GEOAREA = '6';
                 IF (DISTNUM .EQ. 7) GEOAREA = '7';

       ELSE IF (FORNUM .EQ. 11) THEN
C              // North Carolina
               GEOAREA = '3';
              IF (DISTNUM .EQ. 3)THEN
                   GEOAREA = '1';
              ELSE IF (DISTNUM .EQ. 10) THEN
                   GEOAREA = '2';
              ENDIF

       ELSE IF (FORNUM .EQ. 12)THEN
C                     // Francis Marion/Sumpter
                 GEOAREA = '2';
                 IF(DISTNUM .EQ. 2) THEN
                    GEOAREA = '3';
                 ELSE IF(DISTNUM .EQ. 5)THEN 
                    GEOAREA = '1';
                 ENDIF
      ENDIF

C     CREATE THE VOLUME EQUATION NUMBER
      VOLEQ(1:1) = '8'
      VOLEQ(2:2) = GEOAREA
C     Changed to use New Clark equation 8*1CLKE*** (YW 20210719)      
!      IF(PROD.EQ.'01')THEN
!         IF(SPEC.LT.300) THEN
C           7 INCH TOP
!            VOLEQ(3:3) = '7'
!          ELSE
C           9 INCH TOP
!            VOLEQ(3:3) = '9'
!          ENDIF
!       ELSEIF (PROD.EQ.'08') THEN
C           USE PRODUCT 08 LOGIC
!          VOLEQ(3:3) = '8'
!       ELSE
C           4 INCH TOP
!         VOLEQ(3:3) = '4'
!       ENDIF
       IF (PROD.EQ.'08') THEN
C           USE PRODUCT 08 LOGIC
          VOLEQ(3:3) = '8'
       ELSE
         VOLEQ(3:3) = '1'
       ENDIF
      VOLEQ(4:7) = 'CLKE'

C     FIND CORRECT SPECIES
      DONE = 0
      FIRST = 1
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
  5   CONTINUE 
      IF(DONE .LT. 0) THEN
        IF(SPEC.LT.300) THEN
C USE OTHER SOFTWOOD 299
          DONE = 19
        ELSE
          DONE = 92
        ENDIF
      ENDIF

      VOLEQ(8:10) = SNSP(DONE)

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
     &'261','100','115','132','110','111','115','121','126','126',
     &'128',
     &'129','132','131','132','221','222','261','132','500','500',
     &'316','300','500','330','370','370','370','400','300','460',
     &'300','300','500','531','541','541','300','544','500','300',
     &'300','300','500','500','611','621','652','300','652','653',
     &'300','300','300','300','693','694','500','300','300','731',
     &'300','300','300','802','806','812','813','800','800','822',
     &'800','800','800','827','827','832','833','800','835','800',
     &'835',
     &'901','300','300','300','970','970','970','970','300','300'/

      DATA (LSFIA(I), I=1,69)/
     > 12,  68,  71,   91,  94,  95, 105, 125, 125, 129,
     > 130, 241, 261, 299, 313, 314, 315, 316, 317, 318,
     > 319, 371, 375, 391, 402, 403, 407, 421, 462, 491,
     > 500, 531, 541, 543, 544, 601, 602, 660, 693, 701,
     > 731, 741, 742, 743, 746, 760, 761, 762, 763, 766, 
     > 802, 804, 809, 823, 826, 833, 837, 901, 920, 922,
     > 923, 931, 935, 951, 972, 975, 977, 993, 994/

      DATA (CSFIA(I), I =1,97)/
     >  57,  68, 110, 129, 131, 132, 221, 299, 313, 316,
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
     > 132, 241, 260, 261, 299, 313, 314, 315, 316, 317, 
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
     > 129, 130, 131, 132, 221, 222, 260, 299, 311, 313, 
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
        IF((VOLEQ(1:7).EQ.'900CLKE').OR.(VOLEQ(1:7).EQ.'900DVEE'))THEN
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
        IF(VAR.EQ.'LS' .OR. VAR.EQ.'ls') THEN
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
        ELSE IF(VAR.EQ.'NE' .OR. VAR.EQ.'ne')THEN
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
        ELSE
C
C  SOUTHERN VARIANT RUNNING REGION 9 FORESTS AND GEVORKIANTZ METH=5
C
          LAST = 92
          DO 35, WHILE (DONE.EQ.0)
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
  35      CONTINUE 
          IF(DONE .LT. 0) DONE = 92
          VOLEQ(8:10) = SNSP(DONE)
          ENDIF
        RETURN
      ENDIF
C     End FVS check equation
C     NEW CLARK'S PROFILE MODEL VOLUME EQUATION NUMBERS
C     MAKE SURE SPEC IS A 3 CHARACTER FIELD.      
C
        VOLEQ(1:7)='900CLKE'
        WRITE(ASPEC,'(I3)')SPEC
     
        IF(SPEC .LT.10)THEN
          ASPEC(1:2) = '00'
        ELSEIF(SPEC.LT.100)THEN
          ASPEC(1:1) = '0'
        ENDIF

        VOLEQ(8:10) = ASPEC
      
      RETURN
      END
C//////////////////////////////////////////////////////////////////
      SUBROUTINE R10_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      INTEGER SPEC,ERRFLAG,FORNUM
      CHARACTER*10 TONEQN(23),CHUEQN(23),OTHEREQN(27)
      INTEGER FIA(23), FIRST, HALF, LAST, DONE,I
C
C     AK SPECIES LIST
C     Pacific silver fir  Subalpine fir     Alaska yellow Cedar  Tamarack
C     White spruce        Lutz's spruce     Black spruce         Sitka spurce
C     Lodgepole pine      Western redcedar  Western hemlock      Mountain Hemlock
C     Other Softwood      Alder species     Red alder            Paper birch
C     Alaska birch        Balsam poplar     Quaking aspen        Black cottonwood
C     Willow species      Scouler's willow  Other hardwood
C
C     LUTZ SPRUCE IS TREATED AS FIA CODE 94 (WHITE SPRUCE)
      DATA FIA / 011, 019, 042, 071, 094,
     >           094, 095, 098, 108, 242,
     >           263, 264, 299, 350, 351,
     >           375, 376, 741, 746, 747,
     >           920, 928, 998/
C
C     DEFAULT VOLUME EQUATIONS FOR LOCATION CODES: 703, 1002, 1003, 1005,
C     8134, 8135, 8112
      DATA TONEQN /
     > 'A00F32W260','A00F32W260','A00F32W042','A00DVEW094','A00DVEW094',
     > 'A00DVEW094','A00DVEW094','A00F32W098','A00F32W260','A00F32W242',
     > 'A00F32W260','A00F32W260','A00DVEW094','A32CURW351','A32CURW351',
     > 'A00DVEW375','A00DVEW375','A00DVEW747','A00DVEW375','A00DVEW747',
     > 'A00DVEW747','A00DVEW747','A00DVEW747'/

C     DEFAULT VOLUME EQUATIONS FOR LOCATION CODES: 1004, 713, 720, 7400, 7401,
C     7402, 7403, 7404, 7405, 7406, 7407, 7408
      DATA CHUEQN /
     > 'A01DEMW000','A01DEMW000','A00DVEW094','A00DVEW094','A00DVEW094',
     > 'A00DVEW094','A00DVEW094','A00F32W098','A01DEMW000','A01DEMW000',
     > 'A00F32W260','A01DEMW000','A00DVEW094','A32CURW351','A32CURW351',
     > 'A00DVEW375','A00DVEW375','A00DVEW747','A00DVEW375','A00DVEW747',
     > 'A00DVEW747','A00DVEW747','A00DVEW747'/

C     OTHER VALID EQUATIONS THAT CAN BE USED -- THESE ARE NOT DEFAULT
C     EQUATION NUMBERS.
      DATA OTHEREQN /
     > 'A00FW2W042','A16DEMW042','A61DEMW042','A32DEMW042','A01DVEW094',
     > 'A02DVEW094','A16DEMW098','A00FW2W098','A02F32W098','A02FW2W098',
     > 'A02DEMW000','A32CURW000','A32DEMW098','A61DEMW098','A00FW2W242',
     > 'A61DEMW242','A16DEMW242','A32DEMW242','A00FW2W260','A02F32W260',
     > 'A02FW2W260','A16CURW260','A01DVEW375','A01DVEW747','A00DVEW108',
     > 'A00DVEW310','A00DVEW351'/
C
C     SEARCH FOR VALID EQUATION NUMBER IN TONEQN AND CHUEQN
      IF(SPEC.EQ.9999)THEN
        DO I=1,23
        IF((VOLEQ.EQ.TONEQN(I)).OR.(VOLEQ.EQ.CHUEQN(I)))THEN
C
C     FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
C
C     SEARCH FOR VALID EQUATION NUMBER IN OTHEREQN
        DO I=1,27
        IF(VOLEQ.EQ.OTHEREQN(I))THEN
C
C     FOUND VALID EQUATION NUMBER
C
          SPEC=8888
          RETURN
        ENDIF
        ENDDO
      RETURN
      ENDIF
C
      READ(FORST,'(I2)')FORNUM
      LAST = 23
      FIRST = 1
      DONE = 0
C
C     CHECK FOR VALID SPECIES
      DO WHILE (DONE.EQ.0)
         HALF = (LAST - FIRST +1)/2 + FIRST
          IF(FIA(HALF) .EQ. SPEC)THEN
             DONE = HALF
          ELSE IF(FIRST .EQ. LAST) THEN
             ERRFLAG = 1
             DONE = -1
          ELSE IF (FIA(HALF) .LT. SPEC) THEN
             FIRST = HALF
          ELSE
             LAST = HALF - 1
          ENDIF
      ENDDO

C     IF SPECIES NOT FOUND, USE OTHER SOFTWOOD
C     LUTZ SPRUCE WILL NEVER BE FOUND IN THE ABOVE LOOP SINCE
C     IT DOES NOT HAVE A FIA CODE. AS SUCH IT WILL BE ASSIGNED 
C     THE EQUATION NUMBER FOR OTHER SOFTWOOD (INDEX 13).
      IF(DONE .LT. 0) DONE = 13
C
C     DETERMINE DEFAULT EQUATION NUMBER TO USE BASED ON INDEX (DONE)
C     AND FOREST NUMBER
      IF(FORNUM .EQ. 4) THEN
         VOLEQ = CHUEQN(DONE)
      ELSE
         VOLEQ = TONEQN(DONE)
      ENDIF

      RETURN
      END

C =======================================================================
      SUBROUTINE FIAVOLEQDEF(VAR,REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)

C    SUBROUTINE WILL RETURN THE DEFAULT FIA VOLUME EQUATION NUMBER
C        SPEC = 3 DIGIT FIA SPECIES CODE
c ----------------------------------------------------------------------
c FIA species default equation
c species     common name           WOR     WWA    EOR    EWA     CA
c ----------  -------------------   ---     ---    ---    ---     --
c 11          Pacific silver fir     11     11     10     10     --
c 14          Bristlecone fir        --     --     --     --     18
c 15          White fir              23     --     10     --     23
c 17          Grand fir              11     11     10     10     23
c 19          Subalpine fir          11     11     10     10     18
c 20          California red fir     --     --     --     --     18
c 21          Shasta red fir         18     18     18     18     18
c 22          Noble fir              11     11     10     10     18
c 41          Port-Orford-cedar      19     19     19     19      8
c 42          Alaska-cedar            9      9      8      8      8
c 50          Cypress                --     --     --     --     19
c 51          Arizona cypress        --     --     --     --     19
c 56          McNabb cypress         --     --     --     --     19
c 62          California juniper     --     --     --     --     21
c 64          Western juniper        21     21     21     21     21
c 65          Utah juniper           --     --     --     --     21
c 72          Subalpine larch        --     22     --     22     --
c 73          Western larch          --     22     22     22     --
c 81          Incense cedar          19     19     19     19     19
c 92          Brewer spruce          13     --     13     --     12
c 93          Engelmann spruce       13     13     12     12     12
c 98          Sitka spruce           13     13     --     --     12
c 101          Whitebark pine        15     15     15     15     20
c 102          Bristlecone pine      --     --     --     --     16
c 103          Knobcone pine         15     --     15     --     16
c 104          Foxtail pine          --     --     --     --     16
c 108          Lodgepole pine        15     15     15     15     16
c 109          Coulter pine          --     --     --     --     5
c 113          Limber pine           15     --     15     --     16
c 116          Jeffrey pine           5     --      4     --     5
c 117          Sugar pine            20     20     20     20     20
c 119          Western white pine    15     15     15     15     20
c 120          Bishop pine           --     --     --     --     16
c 122          Ponderosa pine(>=5dbh) 5      4      4      4      5
c 122          Ponderosa pine(<5�dbh) 5      5      5      5      5
c 124          Monterey pine         --     --     --     --     16
c 127          Gray pine             --     --     --     --     5
c 130          Scotch pine           17     17     17     17     17
c 133          Singleleaf pinyon     --     --     --     --     21
c 137          Washoe pine           --     --     --     --     5
c 201          Bigcone Douglas-fir   --     --     --     --     3
c 202          Douglas-fir            1      1      2      2     3
c 211          Redwood               24     --     --     --     24
c 212          Giant Sequoia         24     --     --     --     24
c 231          Pacific yew            9      9      8      8     8
c 242          Western redcedar       9      9      8      8     8
c 251          California nutmeg     --     --     --     --     8
c 263          Western hemlock        6      6      6      6     6
c 264          Mountain hemlock      17     17     17     17     17
c 299          Unknown Conifer       17     17     17     17     17
c 312          Bigleaf maple         37     26     37     26     37
c 313          Boxelder              --     --     --     --     38
c 321          Rocky Mountain maple  --     --     --     --     --
c 322          Bigtooth maple        --     --     --     --     --
c 330          California buckeye    --     --     --     --     43
c 341          Tree of heaven        --     --     --     --     26
c 351          Red alder             26     25     26     25     26
c 352          White alder           26     --     26     --     26
c 361          Pacific madrone       40     26     40     26     40
c 374          Water birch           --     --     --     --     26
c 375          Paper birch           --     --     --     --     --
c 376          Western paper birch   --     26     --     26     --
c 431          Golden chinkapin      32     26     --     26     32
c 475 Curlleaf mountain-mahogany     --     --     45     --     45
c 492          Pacific dogwood       --     26     --     26     26
c 500          Hawthorn              --     --     --     --     --
c 510          Eucalyptus            26     --     --     --     31
c 542          Oregon ash            38     26     38     26     38
c 590          Holly                 26     26     26     26     26
c 600          Walnut                26     26     26     --     38
c 631          Tanoak                34     --     --     --     34
c 660          Apple                 26     26     26     26     42
c 730          California sycamore   26     26     26     26     42
c 740          Cottonwood and poplar --     --     --     --     --
c 741          Balsam poplar         --     --     --     --     --
c 742          Eastern cottonwood    --     --     --     --     --
c 745          Plains cottonwood     --     --     --     --     --
c 746          Quaking aspen         26     26     26     26     28
c 747          Black cottonwood      26     26     26     26     27
c 748          Fremont poplar        --     --     --     --     27
c 755          Mesquite              --     --     --     --     --
c 760          Cherry                26     26     26     26     26
c 800          Oak-deciduous         --     --     --     --     43
c 801          California live oak   --     --     --     --     43
c 805          Canyon live oak       42     --     --     --     42
c 807          Blue oak              --     --     --     --     39
c 810          Emory oak             --     --     --     --     --
c 811          Englemann oak         --     --     --     --     36
c 815          Oregon white oak      41     26     41     26     41
c 818          California black oak  38     --     38     26     38
c 821          California white oak  --     --     --     --     35
c 839          Interior live oak     --     --     --     --     44
c 901          Black locust          --     --     --     --     41
c 920          Willow                26     26     26     26     40
c 981          California-laurel     33     --     --     --     33
c 998          Unknown hardwood      26     26     26     26     41
c 999          Unknown Tree          26     26     26     26     41
c ------------------------------------------------------------------------
      CHARACTER*2 FORST,DIST,VAR
      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG,REGN,SPEC

      IF(VAR .EQ. '  ') THEN
         CALL GETVARIANT(REGN,FORST,DIST,VAR)
      ENDIF
      IF(REGN .EQ. 5) THEN
         CALL R5_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE IF(REGN.EQ.6)THEN
         CALL R6_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      ELSE
         ERRFLAG=1
      ENDIF

      RETURN      
      END

C FIA DEFAULT VOLUME EQUATION FOR CA
      SUBROUTINE R5_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      CHARACTER*2 FORST,DIST,VAR
      CHARACTER*10 VOLEQ, EQNUM(91)
      INTEGER ERRFLAG,SPEC,FIA(91)
      INTEGER DONE,HALF,FIRST,LAST

      DATA (FIA(I),I=1,91)/  
     &  11, 14, 15, 17, 19, 20, 21, 22, 41, 42,
     &  50, 51, 56, 62, 64, 65, 72, 73, 81, 92, 
     &  93, 98,101,102,103,104,108,109,113,116,
     & 117,119,120,122,124,127,130,133,137,142,
     & 201,202,211,212,231,242,251,263,264,299,
     & 301,312,313,330,333,341,351,352,361,374,
     & 431,475,478,492,510,542,590,600,631,660,
     & 671,730,746,747,748,760,768,800,801,805,
     & 807,811,815,818,821,839,901,920,981,998,
     & 999/
      DATA (EQNUM(I),I=1,91)/
c        11,14, 15, 17, 19,
     &'616TRFW264','516TRFW021','516TRFW015','516TRFW015','516TRFW021',
c        20,21, 22, 41, 42,
     &'516TRFW021','516TRFW021','516TRFW021','616TRFW242','616TRFW242',
c        50,51, 56, 62, 64,
     &'516TRFW081','516TRFW081','516TRFW081','516TRFW060','516TRFW060',
c        65,72, 73,81, 92,
     &'516TRFW060','616TRFW073','616TRFW073','516TRFW081','616TRFW094',
c        93,98,101,102,103,
     &'616TRFW094','616TRFW094','516TRFW117','516TRFW108','516TRFW108',
c       104,108,109,113,116,
     &'516TRFW108','516TRFW108','516TRFW122','516TRFW108','516TRFW122',
c       117,119,120,122,124,
     &'516TRFW117','516TRFW117','516TRFW108','516TRFW122','516TRFW108',
c       127,130,133,137,142,
     &'516TRFW122','616TRFW264','516TRFW060','516TRFW122','616TRFW264',
c       201,202,211,212,231,
     &'516TRFW202','516TRFW202','616TRFW211','616TRFW211','616TRFW242',
c       242,251,263,264,299,
     &'616TRFW242','616TRFW242','616TRFW263','616TRFW264','616TRFW264',
c       301,312,313,330,333,
     &'500DVEW815','500DVEW312','500DVEW818','500DVEW801','500DVEW815',
c       341,351,352,361,374,
     &'500DVEW351','500DVEW351','500DVEW351','500DVEW361','500DVEW351',
c       431,475,478,492,510,
     &'500DVEW431','400DVEW475','500DVEW815','500DVEW351','616TRFW998',
c       542,590,600,631,660,
     &'500DVEW818','500DVEW351','500DVEW818','500DVEW631','500DVEW805',
c       671,730,746,747,748,
     &'500DVEW815','500DVEW805','616TRFW746','616TRFW747','616TRFW747',
c       760,768,800,801,805,
     &'500DVEW351','500DVEW815','500DVEW801','500DVEW801','500DVEW805',
c       807,811,815,818,821,
     &'500DVEW807','500DVEW811','500DVEW815','500DVEW818','500DVEW821',
c       839,901,920,981,998,
     &'500DVEW839','500DVEW815','500DVEW361','500DVEW981','500DVEW815',
c       999
     &'500DVEW815'/

C     GET EQUATION FROM EQNUME ARRAY
          DONE=0
          LAST = 91
          FIRST = 1
          DO 55, WHILE (DONE.EQ.0)
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
  55       CONTINUE 
      
          IF(DONE .LT. 0) THEN
              VOLEQ = EQNUM(LAST)
          ELSE
              VOLEQ = EQNUM(DONE)   
          ENDIF

      RETURN
      END SUBROUTINE R5_PNWEQN
      
C FIA DEFAULT VOLUME EQUATION FOR WA AND OR
      SUBROUTINE R6_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
      CHARACTER*2 FORST,DIST,VAR
      CHARACTER*10 VOLEQ, EQNUMW(66), EQNUME(66)
      INTEGER ERRFLAG,SPEC,FIA(66)
      INTEGER DONE,HALF,FIRST,LAST
      DATA (FIA(I),I=1,66)/  
     &    11, 15, 17, 19, 20, 21, 22, 41, 42, 64,
     &    66, 72, 73, 81, 92, 93, 98,101,103,106,
     &   108,113,116,117,119,122,130,202,211,212,
     &   231,242,263,264,299,312,321,351,352,361,
     &   375,376,431,475,478,492,500,510,542,590,
     &   600,631,660,730,740,746,747,760,768,805,
     &   815,818,920,981,998,999/     
      DATA (EQNUMW(I),I=1,66)/  
c         11, 15, 17, 19, 20, 
     &'632TRFW011','532TRFW015','632TRFW011','632TRFW011','532TRFW021',
c         21, 22, 41, 42, 64,
     &'532TRFW021','632TRFW011','532TRFW081','632TRFW242','532TRFW060',
c         66, 72, 73, 81, 92,
     &'532TRFW060','632TRFW073','632TRFW073','532TRFW081','632TRFW098',
c         93, 98,101,103,106,
     &'632TRFW098','632TRFW098','632TRFW108','632TRFW108','632TRFW264',
c        108,113,116,117,119,
     &'632TRFW108','632TRFW108','532TRFW122','532TRFW117','632TRFW108',
c        122,130,202,211,212,
     &'532TRFW122','632TRFW264','632TRFW202','632TRFW211','632TRFW211',
c        231,242,263,264,299,
     &'632TRFW242','632TRFW242','632TRFW263','632TRFW264','632TRFW264',
c        312,321,351,352,361,
     &'500DVEW312','500DVEW351','500DVEW351','500DVEW351','500DVEW361',
c        375,376,431,475,478,
     &'500DVEW351','500DVEW351','500DVEW431','400DVEW475','400DVEW475',
c        492,500,510,542,590,
     &'500DVEW351','500DVEW351','500DVEW351','500DVEW818','500DVEW351',
c        600,631,660,730,740,
     &'500DVEW351','500DVEW631','500DVEW351','500DVEW351','500DVEW351',
c        746,747,760,768,805,
     &'500DVEW351','500DVEW351','500DVEW351','500DVEW351','500DVEW805',
c        815,818,920,981,998,
     &'500DVEW815','500DVEW818','500DVEW351','500DVEW981','500DVEW351',
c       999
     &'500DVEW351'/
      DATA (EQNUME(I),I=1,66)/  
c         11, 15, 17, 19, 20,21,      
     &'616TRFW019','616TRFW019','616TRFW019','616TRFW019','516TRFW021',
c         21, 22, 41, 42, 64,
     &'516TRFW021','616TRFW019','516TRFW081','616TRFW242','516TRFW060',
c         66, 72, 73, 81, 92,
     &'516TRFW060','616TRFW073','616TRFW073','516TRFW081','616TRFW098',
c         93, 98,101,103,106,
     &'616TRFW094','616TRFW094','616TRFW108','616TRFW108','616TRFW264',
c        108,113,116,117,119,
     &'616TRFW108','616TRFW108','616TRFW122','516TRFW117','616TRFW108',
c        122,130,202,211,212,
     &'616TRFW122','616TRFW264','616TRFW202','616TRFW211','616TRFW211',
c        231,242,263,264,299,
     &'616TRFW242','616TRFW242','616TRFW263','616TRFW264','616TRFW264',
c        312,321,351,352,361,
     &'500DVEW312','500DVEW351','500DVEW351','500DVEW351','500DVEW361',
c        375,376,431,475,478,
     &'500DVEW351','500DVEW351','500DVEW351','400DVEW475','400DVEW475',
c        492,500,510,542,590,
     &'500DVEW351','500DVEW351','500DVEW351','500DVEW818','500DVEW351',
c        600,631,660,730,740,
     &'500DVEW351','500DVEW631','500DVEW351','500DVEW351','500DVEW351',
c        746,747,760,768,805,
     &'500DVEW351','500DVEW351','500DVEW351','500DVEW351','500DVEW805',
c        815,818,920,981,998,
     &'500DVEW815','500DVEW818','500DVEW351','500DVEW981','500DVEW351',
c       999
     &'500DVEW351'/

      DONE=0
      LAST = 66
      FIRST = 1
c     Westside Variants
      IF(VAR.EQ.'PN' .OR. VAR.EQ.'WC' .OR. VAR.EQ.'NC' .OR.
     >   VAR.EQ.'CA')THEN
C     GET EQUATION FROM EQNUMW ARRAY
          DO 65, WHILE (DONE.EQ.0)
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
  65       CONTINUE 
      
          IF(DONE .LT. 0) THEN
              VOLEQ = EQNUMW(LAST)
          ELSE
              VOLEQ = EQNUMW(DONE)   
          ENDIF

C     FORESTS IN WA
C     NEED TO FIND FOREST NUMBER FOR WA!!!
        IF(FORST.EQ.'03' .OR. FORST.EQ.'05' .OR. FORST.EQ.'09'
     >    .OR. FORST.EQ.'13' .OR. FORST.EQ.'17' .OR. FORST.EQ.'21')THEN
          IF(SPEC.EQ.72 .OR. SPEC.EQ.73)THEN
            VOLEQ='632TRFW073'
          ELSEIF(SPEC.EQ.122)THEN
            VOLEQ='632TRFW122'
          ELSEIF(SPEC.EQ.312 .OR. SPEC.EQ.361 .OR. SPEC.EQ.376
     >           .OR. SPEC.EQ.431 .OR. SPEC.EQ.492
     >           .OR. SPEC.EQ.542 .OR. SPEC.EQ.815)THEN
            VOLEQ='500DVEW351'
          ELSEIF(SPEC.EQ.351)THEN
            VOLEQ='616TRFW351'
          ENDIF
        ENDIF
C     EAST VARIANTS
      ELSE
C     GET EQUATION FROM EQNUME ARRAY
          DO 75, WHILE (DONE.EQ.0)
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
  75       CONTINUE 
      
          IF(DONE .LT. 0) THEN
              VOLEQ = EQNUME(LAST)
          ELSE
              VOLEQ = EQNUME(DONE)   
          ENDIF

C     FORESTS IN WA      
        IF(FORST.EQ.'03' .OR. FORST.EQ.'05' .OR. FORST.EQ.'09'
     >    .OR. FORST.EQ.'13' .OR. FORST.EQ.'17' .OR. FORST.EQ.'21')THEN
          IF(SPEC.EQ.351)THEN
            VOLEQ='616TRFW351'
          ELSEIF(SPEC.EQ.312 .OR. SPEC.EQ.361 .OR. SPEC.EQ.542
     &            .OR. SPEC.EQ.815 .OR. SPEC.EQ.818)THEN
            VOLEQ='500DVEW351'
          ENDIF
        ENDIF
      ENDIF
            
      RETURN
      END SUBROUTINE R6_PNWEQN
      