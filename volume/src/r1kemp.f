!== last modified  6-04-2008
      SUBROUTINE R1KEMP(VOLEQ,HTTOT,DBHOB,VOL,LIVE,PROD,TLOGS,errflag) 
   
C PURPOSE: THIS ROUTINE CALCULATES GROSS & NET BOARD & CUBIC 
C          VOLUMES IN ENGLISH OR METRIC INCLUDING PRODUCT & POLE VOL

C     INTERNAL SUBROUTINE R1LOGS 

      CHARACTER*1 LIVE
      CHARACTER*2 PROD
      CHARACTER*10 VOLEQ
      
      INTEGER ISPEC,JTAB,I,ERRFLAG,IWHEN,KLASS,NONSAW

      REAL VOL(15),HTTOT,DBHOB,D2H100,BFGRS,CBGRS,BFNET
      REAL BFVOL(15,5,4),CBVOLE(15,11),TLOGS,CBSAW

C****************************************
C             DATA STATEMENTS           *
C****************************************
C      BLOCK DATA
     
C INITIALIZATION OF VARIABLES IN COMMON BLOCKS
     
C MISCELLANEOUS

C SPECIES - 2 NAMES FOR EACH...

C      DATA NAMSPC/'QA','QA ','CW','CW ','GF ','GF ','AF','AF ','L ',
C     *'L  ','S  ','S  ','WB ','WB ','LP ','LP ','WP ','WP ','PP ','PP ',
C     *'DF ','DF ','C  ','C  ','MH ','MH ','TOT','TOT'/, IETFOR/1,2,8,9,
C     *11,12,15/, INDXP1/5,8,11,12/, INDXP2/4*0,1,2*0,2,2*0,3,4,0/,
C     &MNGLST/'NONE',19*' '/
     
     
C BOARD FOOT VOLUME - 01 TABLES     
     
C AS,CW,GF,AF,WL,S,WBP,LP,WP,PP,DF,C,H,J,PY - IN ORDER, ONE LINE EACH  
      DATA (BFVOL(1,1,I),I=1,4)/1.197,-18.544,1.216,-21.309/,     
     *(BFVOL(2,1,I),I=1,4)/1.046,-15.966, 1.140, -46.735/,  
     *(BFVOL(3,1,I),I=1,4)/1.293, -34.127, 1.218, 10.603/,  
     *(BFVOL(4,1,I),I=1,4)/1.011, -11.403, 0.694, 124.425/, 
     *(BFVOL(5,1,I),I=1,4)/0.997, -29.790, 0.841, 85.150/,  
     *(BFVOL(6,1,I),I=1,4)/1.149, -11.851, 1.158, 1.620/,   
     *(BFVOL(7,1,I),I=1,4)/1.208, -8.085, 1.103, 14.111/,   
     *(BFVOL(8,1,I),I=1,4)/1.208, -8.085, 1.103, 14.111/,   
     *(BFVOL(9,1,I),I=1,4)/1.189, -26.729, 1.181, -32.516/, 
     *(BFVOL(10,1,I),I=1,4)/1.201, -50.340, 1.595, -298.784/,     
     *(BFVOL(11,1,I),I=1,4)/1.003, -25.332, 1.011, -9.522/, 
     *(BFVOL(12,1,I),I=1,4)/0.878, -10.742, 0.799, -4.064/, 
     *(BFVOL(13,1,I),I=1,4)/1.203, -37.314, 1.306, -50.680/ 
     *(BFVOL(14,1,I),I=1,4)/1.208, -8.085, 1.103, 14.111/,   
     *(BFVOL(15,1,I),I=1,4)/0, 0, 0, 0/   
     
C BOARD FOOT 02 TABLES - WBP,LP,PP,DF - IN ORDER, ONE LINE EACH   
     
      DATA (BFVOL(7,2,I),I=1,2)/0.0135436, -33.632/, 
     *     (BFVOL(8,2,I),I=1,2)/0.0135436, -33.632/, 
     *     (BFVOL(10,2,I),I=1,3)/1.6399, -1.01249, -4.60157/,     
     *     (BFVOL(11,2,I),I=1,3)/0.848633, 0.000102364, -9.84419/ 
     
C BOARD FOOT 03 TABLES - LP,PP - IN ORDER, ONE LINE EACH    
     
      DATA (BFVOL(7,3,I),I=1,2)/1.1656, -13.5219/,   
     *     (BFVOL(8,3,I),I=1,2)/1.1656, -13.5219/,   
     *     (BFVOL(10,3,I),I=1,3)/1.8422, 1.11986, -2.78156/ 
     
C BOARD FOOT 04 TABLE - PP    
     
      DATA (BFVOL(10,4,I),I=1,3)/0.17148, -1.97703, 7.034/  
     
C BOARD FOOT 05 TABLE - PP    
     
      DATA (BFVOL(10,5,I),I=1,3)/0.13412, -1.65764, 7.4384/ 
     
C CUBIC FOOT VOLUME TABLES    
     
C AS  
      DATA (CBVOLE(1,I),I=1,8)/0.3482, -0.0384, 0.001427, -0.842503,  
     *0.224, -0.343, 0.217, 1.071/  
C CW  
      DATA (CBVOLE(2,I),I=1,8)/0.1064, -0.00778, 0.000176, -0.265342, 
     *0.204, -0.749, 0.194, 4.285/  
C GF  
      DATA (CBVOLE(3,I),I=1,11)/0.3386, -0.03359, 0.001109, -0.918645,
     *0.219, -0.563, 0.197, 9.969, .2153, -0.00167, .50/    
C AF  
      DATA (CBVOLE(4,I),I=1,11)/0.4529, -0.052, 0.002003, -1.113416,  
     *0.183, 1.449, 0.117, 26.222, .2153, .00167, .67/
C WL  
      DATA (CBVOLE(5,I),I=1,11)/0.4172, -0.04693, 0.001782, -1.086592,
     *0.17, -0.056, 0.132, 19.409, .1922, .09023, .35/
C S   
      DATA (CBVOLE(6,I),I=1,11)/0.2619, -0.02345, 0.000671, -0.716502,
     *0.214, 0.48, 0.174, 19.041, .2306, .14528, .35/ 
C WB  
      DATA (CBVOLE(7,I),I=1,11)/0.6808, -0.07974, 0.003113, -1.692512,
     *0.221, 1.052, .197, 5.369, .2306, .14528, .35/  
C LP  
      DATA (CBVOLE(8,I),I=1,11)/0.6808, -0.07974, 0.003113, -1.692512,
     *0.221, 1.052, 0.197, 5.369, .2306, .14528, .35/ 
C WP  
      DATA (CBVOLE(9,I),I=1,11)/0.4544, -0.05119, 0.001945, -1.14765, 
     *0.206, 0.166, 0.194, 4.508, .2306, .14528, .35/ 
C PP  
      DATA (CBVOLE(10,I),I=1,11)/0.4041, -0.04535, 0.001726,-1.054732,
     *0.203, -1.656, 0.218, -9.637, .2306, .14528, .25/     
C DF  
      DATA (CBVOLE(11,I),I=1,11)/0.5125, -0.05817, 0.002208,-1.320519,
     *0.178, 0.437, 0.165, 7.702, 0.1795, 0.16949, 0.47/    
C C   
      DATA (CBVOLE(12,I),I=1,11)/0.3349, -0.03565, 0.001273,-0.851441,
     *0.174, 1.141, 0.146, 8.931, 0.1922, 0.09023, 0.67/    
C H   
      DATA (CBVOLE(13,I),I=1,11)/0.2213, -0.01913, 0.000533,-0.635045,
     *0.209, -0.991, 0.210, 2.544, 0.2153, -0.00167, 0.43/  
C J   
      DATA (CBVOLE(14,I),I=1,11)/0.0, 0.0, 0.0, 0.0,
     *0.211, -0.597, 0.211,-0.597, 0.0, 0.0, 0.0/  
C PY   
      DATA (CBVOLE(15,I),I=1,11)/0.0, 0.0, 0.0, 0.0,
     *0.211, -0.597, 0.211,-0.597, 0.0, 0.0, 0.0/  
     
     
C BOARD FOOT POLE CONVERSIONS 
     
C WL  
C      DATA (POLEBF(1,I),I=1,17)/2*26.0, 2*63.0, 2*120.0, 2*240.0, 
C     *2*343.0, 2*441.0, 5*544.0/    
C LP  
C      DATA (POLEBF(2,I),I=1,17)/2*25.0, 2*57.0, 2*99.0, 11*234.0/ 
C DF  
C      DATA (POLEBF(3,I),I=1,17)/2*27.0, 2*80.0, 2*114.0, 2*216.0, 
C     *9*359.0/    
   
C      DATA (POLEBF(4,I),I=1,17)/22.0, 36.0, 50.0, 70.0, 101.0, 161.0,   
C     *239.0, 261.0, 304.0, 418.0, 462.0, 512.0, 595.0, 736.0, 792.0,    
C     *892.0, 929.0/     
     
     
C CUBIC FOOT POLE CONVERSIONS 
     
C WL  
C      DATA (POLECB(1,I),I=1,17)/2*9.0, 2*12.0, 2*25.0, 2*38.0, 2*65.0,  
C     *2*90.0, 5*120.0/  
C LP  
C      DATA (POLECB(2,I),I=1,17)/2*7.8, 2*10.9, 2*22.6, 11*32.1/   
C DF  
C      DATA (POLECB(3,I),I=1,17)/2*10.0, 2*17.0, 2*24.0, 2*32.0, 9*69.0/ 
   
C      DATA (POLECB(4,I),I=1,17)/7.4, 10.4, 14.1, 18.3, 25.4, 34.3, 46.0,
C     *54.8, 65.5, 75.0, 86.6, 96.7, 112.5, 120.8, 136.3, 150.0, 170.4/  
     
C--  SET ALL POTENTIAL VOLUMES AND AVG NUM OF TLOGS TO ZERO
      DO 10 I=1,15
        VOL(I)=0.0
 10   CONTINUE
      ERRFLAG = 0
       
C  ASSUMES CRUISES ARE FROM YR 95 ON
      IWHEN=9500000

C GO TO SECTION TO PROCESS METRIC OR ENGLISH VOLUMES
   
C      IF (MEASUR .EQ. 1) GO TO 2000        ! never implemented....


C SECTION FOR ENGLISH COMPUTATIONS  
   
      D2H100 = DBHOB ** 2 * HTTOT / 100

C********************************************************
C  DETERMINE SPECIES NUMBER AND TABLE NUMBER TO BE USED *
C********************************************************
      IF (VOLEQ(2:3).EQ.'02') THEN
        IF(VOLEQ(8:10).EQ.'746') THEN
          ISPEC = 1
        ELSEIF(VOLEQ(8:10).EQ.'740') THEN
          ISPEC = 2
        ELSEIF(VOLEQ(8:10).EQ.'017') THEN
          ISPEC = 3
        ELSEIF(VOLEQ(8:10).EQ.'019') THEN
          ISPEC = 4
        ELSEIF(VOLEQ(8:10).EQ.'070' .or. VOLEQ(8:10).EQ.'073') THEN
          ISPEC = 5
        ELSEIF(VOLEQ(8:10).EQ.'090'.or.VOLEQ(8:10).EQ.'093') THEN
          ISPEC = 6
        ELSEIF(VOLEQ(8:10).EQ.'101') THEN
          ISPEC = 7
        ELSEIF(VOLEQ(8:10).EQ.'108') THEN
          ISPEC = 8
        ELSEIF(VOLEQ(8:10).EQ.'119') THEN
          ISPEC = 9
        ELSEIF(VOLEQ(8:10).EQ.'122') THEN
          ISPEC = 10
        ELSEIF(VOLEQ(8:10).EQ.'202') THEN
          ISPEC = 11
        ELSEIF(VOLEQ(8:10).EQ.'240'.or.VOLEQ(8:10).EQ.'242') THEN
          ISPEC = 12
        ELSEIF(VOLEQ(8:10).EQ.'260'.or.VOLEQ(8:10).EQ.'263') THEN
          ISPEC = 13
        ELSEIF(VOLEQ(8:10).EQ.'060') THEN
          ISPEC = 14
        ELSEIF(VOLEQ(8:10).EQ.'106') THEN
          ISPEC = 15
        ELSE
         ERRFLAG = 1
          GO TO 2000
        ENDIF
        JTAB=1

      ELSEIF (VOLEQ(2:3).EQ.'03') THEN
        IF(VOLEQ(8:10).EQ.'101') THEN
          ISPEC = 7
        ELSEIF(VOLEQ(8:10).EQ.'108') THEN
          ISPEC = 8
        ELSEIF(VOLEQ(8:10).EQ.'122') THEN
          ISPEC = 10
        ELSEIF(VOLEQ(8:10).EQ.'202') THEN
          ISPEC = 11
        ELSE
         ERRFLAG = 1
          GO TO 2000
        ENDIF 
        JTAB=2

      ELSEIF (VOLEQ(2:3).EQ.'04') THEN
        IF(VOLEQ(8:10).EQ.'108') THEN
          ISPEC = 8
        ELSEIF(VOLEQ(8:10).EQ.'122') THEN
          ISPEC = 10
        ELSE
         ERRFLAG = 1
          GO TO 2000
        ENDIF
        JTAB=3

      ELSEIF (VOLEQ(2:3).EQ.'05') THEN
        ISPEC=10
        JTAB=4

      ELSEIF (VOLEQ(2:3).EQ.'06') THEN
        ISPEC=10
        JTAB=5
      ENDIF

C********************
C  DETERMINE KLASS  *
C********************
      IF (LIVE.EQ.'D'.AND.ISPEC.EQ.8) THEN
         KLASS=2
      ELSEIF (LIVE.EQ.'D') THEN 
         KLASS=1
      ELSEIF (PROD.EQ.'02') THEN
         KLASS=3
C     ELSEIF (PROD.EQ.?????) THEN
C        KLASS=8
      ELSE
         KLASS=4
      ENDIF
   
C CALCULATE GROSS BOARD FOOT VOLUME 
   
C      JTAB = NTABLE (ISPEC) 
      GO TO (300,400,500,600,700), JTAB 
C   
C COMPUTATIONS FOR BOARD FOOT 01 TABLES 
C   ! for all species
  300 IF (DBHOB .LT. 21.0) THEN       
         BFGRS = BFVOL (ISPEC,1,1) * D2H100 + BFVOL (ISPEC,1,2)
      ELSE
         BFGRS = BFVOL (ISPEC,1,3) * D2H100 + BFVOL (ISPEC,1,4)
      ENDIF
      GO TO 1000
C
C COMPUTATIONS FOR BOARD FOOT 02 TABLES
C! for all species
  400 IF (ISPEC .EQ. 8 .OR. ISPEC .EQ. 7) THEN  
         BFGRS = DBHOB**2 * HTTOT * BFVOL(ISPEC,2,1) + BFVOL(ISPEC,2,2)
      ELSE IF (ISPEC .EQ.10) THEN          
         BFGRS = 10**(BFVOL(10,2,1)*LOG10(DBHOB**2*HTTOT)+
     >   BFVOL(10,2,2)*LOG10(DBHOB) + BFVOL(10,2,3)) * 10.0
      ELSE IF (ISPEC .EQ.11) THEN          
         BFGRS = BFVOL(11,2,1) * D2H100 + BFVOL(11,2,2) * D2H100 **2 +
     *   BFVOL(11,2,3)
      ELSE                
         GO TO 300
      ENDIF
      GO TO 1000
C   
C COMPUTATIONS FOR BOARD FOOT 03 TABLES
C
  500 IF (ISPEC .EQ.10) THEN         
           BFGRS = 10.0**(BFVOL(10,3,1)*LOG10(DBHOB)+BFVOL(10,3,2)*
     *     LOG10(HTTOT) + BFVOL(10,3,3))
           BFGRS = BFGRS * 10.0
      ELSE IF (ISPEC .EQ. 8 .OR. ISPEC .EQ. 7) THEN     
           BFGRS = BFVOL(ISPEC,3,1) * D2H100 + BFVOL(ISPEC,3,2)
      ELSE             
           GO TO 300
      ENDIF
      GO TO 1000
C   
C COMPUTATIONS FOR BOARD FOOT 04 TABLES
C   
  600 IF (ISPEC .EQ.10) THEN     
           BFGRS = DBHOB**2 * BFVOL(10,4,1) + DBHOB * BFVOL(10,4,2) +
     *     BFVOL(10,4,3)
           BFGRS = BFGRS * 10.0
      ELSE                           
           GO TO 300
      ENDIF
      GO TO 1000
C
C COMPUTATIONS FOR BOARD FOOT 05 TABLES 
C   
  700 IF (ISPEC .EQ.10) THEN            
           BFGRS = DBHOB**2 * BFVOL(10,5,1) + DBHOB * BFVOL(10,5,2) +
     *     BFVOL(10,5,3)
           BFGRS = BFGRS * 10.0 
      ELSE                              
           GO TO 300
      ENDIF
 1000 CONTINUE
C
C DEDUCT 20 BF FOR DEAD LP OR DEAD WP (WP CRUISED BEFORE JAN 1,1994)
C
      IF (KLASS .EQ. 2 .AND.(ISPEC .EQ. 8 .OR. (ISPEC .EQ. 9 .AND.
     & IWHEN .LT. 9400000))) BFGRS = BFGRS - 20.0
C
C CHECK FOR MINIMUM BOARD  OOT VOLUME
C! 10 MIN EXCEPT FOR BELOW...
      IF (BFGRS .LT. 10.0) BFGRS = 10.0   
C
C SET MIN TO 30 FOR DEAD LP OR DEAD WP (WP CRUISED BEFORE JAN 1, 1994)
C
      IF ((ISPEC.EQ.8 .OR. (ISPEC .EQ. 9 .AND. IWHEN .LT. 9400000))
     & .AND. KLASS .EQ. 2 .AND.  BFGRS.LT.30.0) BFGRS = 30.0
C
C CALCULATE GROSS CUBIC FOOT VOLUME
C
      IF(ISPEC.EQ.14 .OR. ISPEC.EQ.15)THEN
         IF (DBHOB .LT. 5.0) THEN
            CBGRS = 0.0
         ELSE IF (DBHOB .LE. 20.5 .AND. DBHOB .GE. 5.0) THEN   
            CBGRS = CBVOLE(ISPEC,5) * D2H100 + CBVOLE(ISPEC,6)   
         ELSE  
            CBGRS = CBVOLE(ISPEC,7) * D2H100 + CBVOLE(ISPEC,8)
         ENDIF
      ELSE
         IF (DBHOB .LT. 5.0) THEN
            CBGRS = (CBVOLE(ISPEC,9)*D2H100+CBVOLE(ISPEC,10)) *
     *      CBVOLE(ISPEC,11)
      
         ELSE IF (DBHOB .LE. 9.5 .AND. DBHOB .GE. 5.0) THEN
            CBGRS = D2H100 * (CBVOLE(ISPEC,1)*DBHOB + CBVOLE(ISPEC,2) *
     *      DBHOB**2 + CBVOLE(ISPEC,3)*DBHOB**3 + CBVOLE(ISPEC,4))
         ELSE IF (DBHOB .LE. 20.5 .AND. DBHOB .GT. 9.5) THEN   
            CBGRS = CBVOLE(ISPEC,5) * D2H100 + CBVOLE(ISPEC,6)   
         ELSE  
            CBGRS = CBVOLE(ISPEC,7) * D2H100 + CBVOLE(ISPEC,8)
         ENDIF
      ENDIF
C
C DEDUCT FROM GROSS FOR DEAD LP AND WP (WP CRUISED BEFORE JAN 1, 1994)
C
      IF (KLASS .EQ. 2 .AND. (ISPEC .EQ. 8 .OR. (ISPEC .EQ. 9 .AND.
     & IWHEN .LT. 9400000))) CBGRS = CBGRS - 3.8
C
C SET CUBIC MINIMUMS FOR GROSS
C
      IF (KLASS .EQ. 2 .AND.(ISPEC .EQ. 8 .OR. (ISPEC .EQ. 9 .AND.
     & IWHEN .LT. 9400000))) THEN  
         IF (CBGRS .LT. 4.3) CBGRS = 4.3
      ELSE IF (KLASS .LE. 2) THEN        
         IF (CBGRS .LT. 1.6) CBGRS = 1.6
      ELSE IF (KLASS .EQ. 3) THEN        
         IF (CBGRS .LT. 2.4) CBGRS = 2.4
      ELSE                               
         IF (CBGRS .LT. 0.1) CBGRS = 0.1
      ENDIF
C
C CALCULATE NET BOARD FOOT VOLUME
C   
C     IDEF   = IDEF + ICB (ISPEC,KLASS+1)   ! ADD SEEN AND ADDITIONAL
c      IDEF=SDEF1+SDEF2
c      IF (IDEF   .GT. 100.0) IDEF   = 100.0
c      BFNET = BFGRS - IDEF   * BFGRS / 100.0

C
C CHECK FOR MINIMUM OF 10 BF NET
C
      IF (BFNET .LT. 10.0) BFNET = 10.0   
C
C MIN OF 30 FOR DEAD LP...
C
      IF (ISPEC.EQ.8.AND.KLASS.EQ.2.AND.BFNET.LT.30.0) BFNET = 30.0
C
C CALCULATE CUBIC SAWLOG VOLUME
C
C
C SET NON-SAWLOG PERCENT TO SUM OF BD FT DEFECTS
C
c      IF (KLASS .LE. 2 .OR. KLASS .EQ. 8) NONSAW = IDEF
c      nonsaw = idef
      IF (NONSAW .GT. 100) NONSAW = 100      
      CBSAW = CBGRS - NONSAW / 100.0 * CBGRS     
C
C SET CUBIC MINIMUMS FOR SAWLOG (NET)
C
      IF (KLASS .EQ. 2 .AND.(ISPEC .EQ. 8 .OR. (ISPEC .EQ. 9 .AND.
     & IWHEN .LT. 9400000))) THEN       
         IF (CBSAW .LT. 4.3) CBSAW = 4.3
      ELSE IF (KLASS .LE. 2) THEN        
         IF (CBSAW .LT. 0.5) CBSAW = 0.5
      ELSE IF (KLASS .EQ. 3) THEN            
         IF (CBSAW .LT. 2.4) CBSAW = 2.4
      ELSE                                   
         IF (CBSAW .LT. 0.1) CBSAW = 0.1
      ENDIF
C
C CALCULATE OTHER CUBIC VOLUME  

C       CBOTH = CBGRS * (IALLOC / 100.0) ! CUBIC ALLOCATED TO OTHER PRODS
C
C CALCULATE POLE VOLUME - BOARD FOOT + CUBIC
C   
C      BFPOLE = 0.0  
C      CBPOLE = 0.0
C      IF (LENPOL .NE. 0) THEN    ! TABLE LOOKUP BY HEIGHT CLASS
C           J = INDXP2(ISPEC)
C           I =  (LENPOL / 05 ) - 3  
C           IF (I .LT. 1) I = 1
C           IF (I .GT.17) I = 17 
C           BFPOLE = POLEBF (    J,I)
C           CBPOLE = POLECB (    J,I)
C      ENDIF
C***********************************
C  WRITE VOLUMES TO THE VOL ARRAY  *
C***********************************
      VOL(1)=CBGRS
      VOL(2)=BFGRS
      VOL(3)=BFNET
      VOL(4)=CBGRS
      VOL(5)=CBSAW

C     FIND NUMBER OF TLOGS
      if(ispec.lt.14) then
         CALL R1LOGS(ISPEC,DBHOB,HTTOT,KLASS,IWHEN,TLOGS)      
      endif

c     RETURN

C SECTION FOR METRIC COMPUTATIONS

 2000 CONTINUE  
   
      RETURN
      END


c**********************************************************
      SUBROUTINE R1LOGS (ISPEC,DBHOB,HTTOT,KLASS,IWHEN,TLOGS)
c**********************************************************
C    
C     THIS ROUTINE CALCULATES MECHANTABLE LENGTH FOR ALL TREES EXCEPT
C     FELLED TREES

C     VARIABLE DESCRIPTIONS
C        MEASUR = ENGLISH OR METRIC UNITS FLAG
C        AMERCH = ENGLISH MERCHATABLE LENGTH
C        AMRLNE = TALBE OF COEFFICIENTS FOR COMPUTING ENGLISH MERCH LENGTH
C        ISPEC =SPECIES INDEX
C        HTTOT = TREE HEIGHT
C        DBHOB = TREE DBHOB      
C
C AS,CW,GF,AF,WL,S,WBP,LP,WP,PP,DF,C,H - IN ORDER, ONE LINE EACH  
      REAL DBHOB,HTTOT,AMRLNE(13,3),AMERCH,TLOGS
      INTEGER ISPEC,MEASUR,KLASS,IWHEN

      DATA (AMRLNE(1,I),I=1,3)/0.0,0.0,0.0/
      DATA (AMRLNE(2,I),I=1,3)/0.0,0.0,0.0/
      DATA (AMRLNE(3,I),I=1,3)/0.769102, 8.17961, -39.81773/
      DATA (AMRLNE(4,I),I=1,3)/0.617455, 9.00450, -33.62005/
      DATA (AMRLNE(5,I),I=1,3)/0.767457, 11.0658, -53.23026/
      DATA (AMRLNE(6,I),I=1,3)/0.788744, 6.25054, -36.74818/
      DATA (AMRLNE(7,I),I=1,3)/0.722147, 17.8226, -67.2513/
      DATA (AMRLNE(8,I),I=1,3)/0.722147, 17.8226, -67.2513/
      DATA (AMRLNE(9,I),I=1,3)/0.596947, 14.6879, -49.13522/
      DATA (AMRLNE(10,I),I=1,3)/0.838089, 7.29928, -41.14457/
      DATA (AMRLNE(11,I),I=1,3)/0.745249, 6.12528, -31.79443/
      DATA (AMRLNE(12,I),I=1,3)/0.741795, 6.29675, -33.11715/
      DATA (AMRLNE(13,I),I=1,3)/0.716799, 10.5844, -46.78366/
      
      
      MEASUR = 0
      
      IF (MEASUR .EQ. 1) GO TO 200
C
C     COMPUTE ENGLISH MECHANTABLE LENGTH
C
      AMERCH = AMRLNE(ISPEC,1) * HTTOT + AMRLNE(ISPEC,2)*DBHOB**.5 +
     >         AMRLNE(ISPEC,3)
      
      IF(ISPEC .EQ. 1 .OR. ISPEC.EQ.2) AMERCH = 0.0
C
C     DEDUCT FOR DEAD WP (before 94) or dead lp
C
      IF ((ISPEC.EQ.8 .OR. (ISPEC.EQ.9 .AND. IWHEN.LT.9400000)) .AND.
     >     KLASS.EQ.2)  AMERCH = AMERCH - 16.5
     
      IF (AMERCH .LT. 0.0) AMERCH = 0.0     
      
      TLOGS = AMERCH/16.5
      
      RETURN
C
C     COMPUTE METRIC MERCHANTABLE LENGTH
C
  200 CONTINUE
  
      RETURN
      END
     
     
C VARIABLE DESCRIPTION:  
   
C MEASUR = ENGLISH OR METRIC UNITS FLAG 
C DBHOB = TREE DBHOB
C HTTOT = TREE HEIGHT
C ISPEC = SPECIES INDEX 
C NTABLE = ARRAY FOR VOLUME TABLE SELECTION (has been replaced with VOLEQ)
C BFVOL = TABLE OF BOARD FT VOLUME COEFFICIENTS 
C BFGRS = BOARD FT GROSS VOLUME 
C CBVOLE = TABLE OF CUBIC FT VOLUME COEFFICIENTS
C CBGRS = CUBIC    GROSS VOLUME (1 foot stump to 4" DIB)
C SDEF1 = TREE DEFECT
C ICB = was ARRAY OF ADD'L CULL+BREAK FAC. 
C       Change to single varible NATCRS' SDEF2 
C BFNET = BOARD FT NET VOLUME   
C CBSAW = CUBIC SAWLOG VOLUME   
C NONSAW = NON-SAWLOG VOLUME PERCENT
C IALLOC = PERCENT ALLOCATED TO OTHER PRODUCTS  
C CBOTH = OTHER CUBIC VOLUME
C BFPOLE = BOARD FT POLE VOLUME 
C CBPOLE = CUBIC    POLE VOLUME 
C LENPOL = POLE LENGTH  
C POLEBF = TABLE OF COEFFICIENTS FOR BOARD FT POLE VOLUMES  
C POLECB = TABLE OF COEFFICIENTS FOR CUBIC FT POLE VOLUMES  
