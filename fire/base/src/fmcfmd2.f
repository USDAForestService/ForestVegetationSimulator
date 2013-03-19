      SUBROUTINE FMCFMD2 (IYR, FMD)
      IMPLICIT NONE
C----------
C   **FMCFMD2 FIRE-BASE-DATE OF LAST REVISION: 01/08/08
C----------
*     CALLED FROM: FMBURN, FMPOFL
*     PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     AND THE CLOSEST FUEL MODELS (2) AND THEIR WEIGHTINGS FOR USE BY 
*     THE DYNAMIC FUEL MODEL.
*
*     THE LOGIC USED HERE MAY SELECT THE "NEW" SCOTT AND BURGAN FUEL MODELS
*     AS WELL AS THE ORIGINAL 13 DEPENDING ON THE FUEL MODEL SET SELECTED
*     BY THE USER WITH THE FIRECALC KEYWORD.  THESE RULES WERE DEVELOPED BY
*     JOE SCOTT, OCTOBER 2008.
*----------------------------------------------------------------------
*
*     CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*     LOCAL VARIABLE DEFINITIONS:
*     LARID:   TRUE IF YOU ARE IN AN ARID VARIANT;
*              FALSE IF YOU ARE IN A HUMID VARIANT
*     LFUELMON:  TRUE IF THE FUEL MODEL IS PART OF THE PICK LIST BASED 
*                ON JOE SCOTT'S LOGIC AND SELECTED FUEL MODEL SET (WHEREAS
*                IFUELMON HOLDS INFO ON WHETHER THE USER SET THE FUEL MODEL ON OR OFF)
*     IFCFT:   FIRE CARRYING FUEL TYPE (1=GR, 2=GS, 3=SH/TU, 4=TL/SB)
*     LIVEFRAC: RATIO OF LIVE FUEL LOAD TO THE FINE FUEL LOAD
*     HERBFRAC: RATIO OF HERB LOAD TO THE FINE FUEL LOAD
*     HERBRAT:  RATIO OF HERB LOAD TO THE LIVE WOODY LOAD
*     FBSAV - fuelbed SAV (1/ft)
*     FMSAV - fuelmodel SAV (1/ft)
*     FBBD - fuelbed bulk density (lbs/ft3)
*     FMBD - fuelmodel bulk density (lbs/ft3)
*     FDFL - fine dead fuel load for fuel bed (tons/acre)
*     FBFFL - fuelbed fine fuel load (tons/acre)
*     FMFFL - fuel model fine fuel load (tons/acre)
*     DIndex - departure index
*     CURRCWD(MXFLCL) - array with current surface fuel info (tons/acre)
*     WF - weighted factor used in calc of fuel bed bulk density
*     HERB - herb load (tons/acre)
*     WOODY - live woody load (tons/acre) - includes shrubs and foliage
*             and fine branchwood of trees less than CANMHT ft.(6 usually)
*     SURFAREA, WTFT, WEIGHTLIVE, WEIGHTDEAD, SAVDEAD, SAVLIVE, 
*     SALIVE, SADEAD - all used in weighted SAV calculations
***********************************************************************
C
C.... PARAMETER INCLUDE FILES.
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
C
C.... COMMON INCLUDE FILES.
C
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
C
C     LOCAL VARIABLE DECLARATIONS
C
      CHARACTER VVER*7
      INTEGER IYR,FMD,IFCFT, MYACT(1),NTODO,ITODO,NPRM,IACTK,JYR
      INTEGER I,J,K,L,IFM(MXDFMD), LFM(MXDFMD)
      REAL    LIVEFRAC, HERBFRAC, HERBRAT, LOWDI(MXDFMD), BOT, SUMWT
      REAL    CURRCWD(MXFLCL), WF, FDFL, FBFFL, HERB, WOODY
      REAL    FBSAV, FBBD, SURFAREA(5), SADEAD, PRMS(8)
      REAL    FMSAV(MXDFMD), FMBD(MXDFMD), FMFFL(MXDFMD), DIndex(MXDFMD)
      REAL    WTFT(5), WEIGHTLIVE, WEIGHTDEAD, SAVDEAD, SAVLIVE, SALIVE
      LOGICAL DEBUG, LARID, LFUELMON(MXDFMD)

C     BEGIN ROUTINE
C
      CALL DBCHK (DEBUG,'FMCFMD2',7,ICYC)

      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD2 CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

C     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

      IF (LUSRFM) RETURN

C     INITIALIZE SOME VARIABLES 

      DO I = 1,MXFMOD
        FMOD(I)  = 0
        FWT(I)   = 0.
      ENDDO

      DO I = 1,MXDFMD
        LFUELMON(I) = .FALSE. 
        FMSAV(I) = 0.
        FMBD(I) = 0.
        FMFFL(I) = 0.
        DIndex(I) = 9999.
        LFM(I) = 0
        IFM(I) = 0
        LOWDI(I) = 0.        
      ENDDO 

      DATA MYACT/2550/

C     CHECK WHETHER THE FMODLIST KEYWORD IS SCHEDULED FOR THIS YEAR 

      CALL OPFIND(1,MYACT(1),NTODO)
      IF (NTODO.GT.0) THEN
        DO 400 ITODO = 1,NTODO
          CALL OPGET(ITODO,2,JYR,IACTK,NPRM,PRMS)
CSB          IF (JYR .NE. IYR) GO TO 400 - ! TESTING NON ANNUAL TIMESTEP 
          CALL OPDONE (ITODO,IYR)
          IFUELMON(INT(PRMS(1))) = INT(PRMS(2))                    
  400   CONTINUE
      ENDIF

C     CALCULATE LIVE FRACTION, HERB FRACTION, AND HERB RATIO

      DO I = 1, MXFLCL
        CURRCWD(I) = 0.0
      ENDDO
       
C     Sum up CWD categories by size class
      
      DO I = 1, 2
         DO J = 1, MXFLCL
            DO K = 1, 2
               DO L = 1, 4
                  CURRCWD(J) = CURRCWD(J) + CWD(I,J,K,L)
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      HERB =  FLIVE(1)

C     FOR THE LIVE WOODY ESTIMATE, INCLUDE THE FOLIAGE AND HALF THE 0-.25"
C     BRANCHWOOD OF ALL LIVE TREES NOT INCLUDED IN THE CANOPY FUELS CALCULATIONS
C     ALSO INCLUDE THE SHRUBS POOL

      WOODY = 0
      DO I = 1,ITRN
        IF (HT(I) .LE. CANMHT) THEN
          WOODY = WOODY + (CROWNW(I,0) + 0.5*CROWNW(I,1))*FMPROB(I)*P2T
        ENDIF
      ENDDO
      WOODY = WOODY + FLIVE(2)  

      FBFFL = CURRCWD(1) + CURRCWD(10) + HERB + WOODY
      IF (FBFFL .LE. 0) THEN
        IFCFT = 4
        GOTO 90
      ENDIF
      LIVEFRAC = (HERB + WOODY) / FBFFL
      HERBFRAC = HERB/FBFFL
      IF (WOODY .GT. 0) THEN
        HERBRAT = HERB/WOODY
      ELSE
        HERBRAT = 10
      ENDIF     

C     CALCULATE IFCFT
C     IFCFT:   FIRE CARRYING FUEL TYPE (1=GR, 2=GS, 3=SH/TU, 4=TL/SB)

      IF (LIVEFRAC .LE. 0.20) THEN
        IFCFT = 4
      ELSE ! livefrac gt 0.20
        IF (HERBFRAC .GE. 0.75) THEN
          IFCFT = 1     
        ELSE ! herbfrac lt 0.75
          IF (HERBRAT .GT. 2.0) THEN   
            IFCFT = 1        
          ELSEIF (HERBRAT .GT. 0.25) THEN
            IFCFT = 2  
          ELSE
            IFCFT = 3  
          ENDIF
        ENDIF
      ENDIF

   90 CONTINUE
      IF (DEBUG) WRITE(JOSTND,2) HERB,WOODY,FBFFL,CURRCWD(2),CURRCWD(3),
     &                           IFCFT           
    2 FORMAT(' FMCFMD2 HERB= ',F7.2,' WOODY=',F7.2,' FBFFL=',F7.2,
     &       ' 10hr=',F7.2,' 100hr=',F7.2,' IFCFT=',I3)

C     CALCULATE WHETHER YOU ARE IN AN ARID OR HUMID VARIANT

      CALL VARVER(VVER)
      IF (VVER(1:2) .EQ. 'AK' .OR.
     &    VVER(1:2) .EQ. 'PN' .OR.
     &    VVER(1:2) .EQ. 'WC' .OR.
     &    VVER(1:2) .EQ. 'NE' .OR.
     &    VVER(1:2) .EQ. 'LS' .OR.
     &    VVER(1:2) .EQ. 'CS' .OR.
     &    VVER(1:2) .EQ. 'SN') THEN
        LARID = .FALSE.
      ELSE
        LARID = .TRUE.
      ENDIF

C     SET THE FUEL MODEL PICK LIST BASED ON ARID/HUMID AND FIRE CARRYING FUEL TYPE
C     (GR, GS, SH/TU, OR TL/SB).  MAKE SURE THE RIGHT FUEL MODEL SET (13, 40, OR 53)
C     IS BEING USED.


      IF (LARID) THEN !arid
        IF (IFCFT .EQ. 1) THEN !gr
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (1:3,101,102,104,107)
              LFUELMON(I) = .TRUE.                     
            END SELECT
          ENDDO         
        ELSEIF (IFCFT .EQ. 2) THEN  !gs
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (1:3,5,102,104,121,122,141,142)
              LFUELMON(I) = .TRUE.                  
            END SELECT
          ENDDO        
        ELSEIF (IFCFT .EQ. 3) THEN !sh/tu
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (2,4,5,7,10,141,142,145,147,161,164,165)
              LFUELMON(I) = .TRUE.                     
            END SELECT
          ENDDO        
        ELSE ! tl/sb
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (8,9,11:13,181:189,201:204)
              LFUELMON(I) = .TRUE.                   
            END SELECT
          ENDDO
        ENDIF      
      ELSE !humid
        IF (IFCFT .EQ. 1) THEN !gr
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (1:3,101,103,105,106,108,109)
              LFUELMON(I) = .TRUE.                      
            END SELECT
          ENDDO        
        ELSEIF (IFCFT .EQ. 2) THEN !gs
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (1:3,7,103,105,106,123,124,141,143,144)
              LFUELMON(I) = .TRUE.               
            END SELECT
          ENDDO        
        ELSEIF (IFCFT .EQ. 3) THEN !sh/tu
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (2,4,5,7,10,143,144,146,148,149,161:163)
              LFUELMON(I) = .TRUE.                      
            END SELECT
          ENDDO        
        ELSE !tl/sb
          DO I = 1,MXDFMD
            SELECT CASE (I)
            CASE (8,9,11:13,181:189,201:204)
              LFUELMON(I) = .TRUE.                    
            END SELECT
          ENDDO        
        ENDIF
      ENDIF

      IF (IFMSET .EQ. 0) THEN  ! use only original 13 fuel models
        DO I=101,MXDFMD
          LFUELMON(I) = .FALSE.
        ENDDO
      ELSEIF (IFMSET .EQ. 1) THEN ! use only 40 new fuel models
        DO I=1,13
          LFUELMON(I) = .FALSE.
        ENDDO      
      ENDIF

C     CALCULATE FUEL BED BULK DENSITY

      FDFL = CURRCWD(1) + CURRCWD(10)
      IF (FBFFL .GT. 0) THEN      
        WF = FDFL/FBFFL
        FBBD = UBD(1) + (WF*(UBD(2) - UBD(1)))   
      ELSE
        FBBD = 0
      ENDIF

C     CALCULATE SAV FOR THE FUEL BED

      SURFAREA(1) = USAV(1) * FDFL    !0-.25 and litter
      SURFAREA(2) = 109 * CURRCWD(2)  !.25 - 1 
      SURFAREA(3) = 30 * CURRCWD(3)   ! 1 - 3
      SURFAREA(4) = USAV(3) * WOODY   !woody
      SURFAREA(5) = USAV(2) * HERB    !herb
      SADEAD = SURFAREA(1) + SURFAREA(2) + SURFAREA(3)
      SALIVE = MAX(0.0000001,(SURFAREA(4) + SURFAREA(5)))
      
      DO I = 1,3
        IF (SADEAD .GT. 0) THEN
          WTFT(I) = SURFAREA(I)/SADEAD
        ELSE
          WTFT(I) = 0
        ENDIF
        IF (SALIVE .GT. 0) THEN
          IF (I .LT. 3) WTFT(I+3) = SURFAREA(I+3)/SALIVE
        ELSE
          IF (I .LT. 3) WTFT(I+3) = 0
        ENDIF
      ENDDO
      
      IF ((SALIVE .LE. 0) .AND. (SADEAD .LE. 0)) THEN
        FBSAV = 0
      ELSE
        WEIGHTDEAD = SADEAD/(SALIVE+SADEAD)
        WEIGHTLIVE = 1 - WEIGHTDEAD        
        SAVDEAD = USAV(1)*WTFT(1) + 109*WTFT(2) + 30*WTFT(3)        
        SAVLIVE = USAV(3)*WTFT(4) + USAV(2)*WTFT(5)
        FBSAV = SAVLIVE*WEIGHTLIVE + SAVDEAD*WEIGHTDEAD
      ENDIF

C      IF (DEBUG) WRITE(JOSTND,*) SAVLIVE, WEIGHTLIVE, SAVDEAD,
C     &           WEIGHTDEAD,USAV(1),USAV(2),USAV(3),
C     &           WTFT(1),WTFT(2),WTFT(3),WTFT(4),WTFT(5),
C     &           SALIVE, SADEAD, SURFAREA(1), SURFAREA(2), SURFAREA(3), 
C     &           SURFAREA(4), SURFAREA(5)

      IF (DEBUG) WRITE(JOSTND,3) FBSAV,FBBD,FBFFL          
    3 FORMAT(' FMCFMD2 FBSAV= ',F8.2,' FBBD=',F7.2,' FBFFL=',F7.2)

C     CALCULATE SAV FOR THE FUEL MODEL
  
      DO J = 1,MXDFMD
        IF ((.NOT. LFUELMON(J)) .AND. (IFUELMON(J) .NE. 0)) GOTO 390 
        SURFAREA(1) = SURFVL(J,1,1) * FMLOAD(J,1,1)/0.04591    
        SURFAREA(2) = SURFVL(J,1,2) * FMLOAD(J,1,2)/0.04591
        SURFAREA(3) = SURFVL(J,1,3) * FMLOAD(J,1,3)/0.04591  
        SURFAREA(4) = SURFVL(J,2,1) * FMLOAD(J,2,1)/0.04591
        SURFAREA(5) = SURFVL(J,2,2) * FMLOAD(J,2,2)/0.04591  
        SADEAD = SURFAREA(1) + SURFAREA(2) + SURFAREA(3)
        SALIVE = MAX(0.0000001,(SURFAREA(4) + SURFAREA(5)))        

        DO I = 1,3
          IF (SADEAD .GT. 0) THEN
            WTFT(I) = SURFAREA(I)/SADEAD
          ELSE
            WTFT(I) = 0
          ENDIF
          IF (SALIVE .GT. 0) THEN
            IF (I .LT. 3) WTFT(I+3) = SURFAREA(I+3)/SALIVE
          ELSE
            IF (I .LT. 3) WTFT(I+3) = 0
          ENDIF
        ENDDO

        IF ((SALIVE .LE. 0) .AND. (SADEAD .LE. 0)) THEN
          FMSAV(J) = 0
        ELSE        
          WEIGHTDEAD = SADEAD/(SALIVE+SADEAD)
          WEIGHTLIVE = 1 - WEIGHTDEAD
          
          SAVDEAD = SURFVL(J,1,1)*WTFT(1) + SURFVL(J,1,2)*WTFT(2) + 
     &              SURFVL(J,1,3)*WTFT(3)        
          SAVLIVE = SURFVL(J,2,1)*WTFT(4) + SURFVL(J,2,2)*WTFT(5)
          FMSAV(J) = SAVLIVE*WEIGHTLIVE + SAVDEAD*WEIGHTDEAD
        ENDIF
  390 CONTINUE
      ENDDO


C     CALCULATE BULK DENSITY AND FINE FUEL LOAD FOR THE FUEL MODELS
C     THEN CALCULATE THE DEPARTURE INDEX

      DO I = 1,MXDFMD
        IF ((.NOT. LFUELMON(I)) .AND. (IFUELMON(I) .NE. 0)) GOTO 490      
        FMFFL(I) = (FMLOAD(I,1,1)+FMLOAD(I,2,1)+FMLOAD(I,2,2))/0.04591
        FMBD(I) = (FMLOAD(I,1,1) + FMLOAD(I,1,2) + FMLOAD(I,1,3) + 
     &             FMLOAD(I,2,1) + FMLOAD(I,2,2)) / FMDEP(I)

        DIndex(I) = 0.25*((FBSAV - FMSAV(I))/405.2)**2 +
     &              0.25*((FBBD -  FMBD(I))/0.3992)**2 +
     &              0.50*((FBFFL - FMFFL(I))/3.051)**2 

      IF (DEBUG) WRITE(JOSTND,4) I,FMSAV(I),FMBD(I),FMFFL(I),DINDEX(I)       
    4 FORMAT(' FMCFMD2 FM= ',I4,' FMSAV= ',F8.2,' FMBD=',F7.2,
     &       ' FMFFL=',F7.2,' DINDEX=',F8.2)

  490 CONTINUE
      ENDDO 

C     FIGURE OUT WHICH TWO FUEL MODELS HAVE THE LOWEST DEPARTURE INDEX

      DO I = 1,MXDFMD
        DINDEX(I) = -1*DINDEX(I) !make them negative so the lowest ones sort out on top
      ENDDO      
      
      CALL RDPSRT(MXDFMD,DINDEX,IFM,.TRUE.)

      DO I = 1,MXDFMD
        DINDEX(I) = -1*DINDEX(I) !switch the values back
      ENDDO 

      J = 0
      DO I = 1,MXDFMD
        IF (LFUELMON(IFM(I)) .OR. (IFUELMON(IFM(I)) .EQ. 0)) THEN
          IF (IFUELMON(IFM(I)) .NE. 1) THEN
            J = J + 1
            LFM(J) = IFM(I)
            LOWDI(J) = DINDEX(IFM(I)) 
          ENDIF
        ENDIF
      ENDDO          
            
  590 CONTINUE

      IF (DEBUG) WRITE(JOSTND,5) (LFM(I),I=1,4), (LOWDI(I),I=1,4)      
    5 FORMAT(' FMCFMD2 LFM= ',4(1X,I4),' LOWDI=',4(1X,F8.2))
     
C     SET NFMODS, FMOD(I), FWT(I), AND FMD, DEPENDING ON WHETHER 
C     THE STATIC OR DYNAMIC FUEL MODEL OPTION IS TURNED ON

      J = MIN(J,2)
      
      IF (J .eq. 0) THEN ! no model is able to be picked
        FMD = 8
        FMOD(1) = 8
        FWT(1) = 1.
        NFMODS = 1

        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO AVAILABLE ',
     &  'FUEL MODELS:',
     &  /1X,'*** FUEL MODEL SET TO FM 8',/1X)")
        CALL RCDSET (2,.TRUE.)   

      ELSEIF ((LOWDI(1) .le. 0) .OR. (J .EQ. 1)) then  ! only one or this one matches exactly, give it 100% weight
        FMD = LFM(1)
        FMOD(1) = LFM(1)
        FWT(1) = 1.
        NFMODS = 1
      ELSE
        FMD = LFM(1)
        BOT = 0
        SUMWT = 0
        DO I = 1,J
          FMOD(I) = LFM(I)      
          BOT = BOT + 1/LOWDI(I)
        ENDDO          
        DO I = 1,J-1     
          FWT(I) = (1/LOWDI(I))/BOT
          SUMWT = SUMWT + FWT(I)
        ENDDO
        FWT(J) = 1 - SUMWT             
        NFMODS = J
      ENDIF
      
      IF (.NOT. LDYNFM) THEN
        FMOD(1) = FMD
        FWT(1)  = 1.0
        NFMODS  = 1
        DO I = 2,MXFMOD     
          FMOD(I) = 0
          FWT(I)  = 0.0
        ENDDO
      ENDIF

      IF (DEBUG) WRITE(JOSTND,6) FMD,NFMODS,FMOD(1),FWT(1),
     &                           FMOD(2),FWT(2),FMOD(3),FWT(3),
     &                           FMOD(4),FWT(4)      
    6 FORMAT(' FMCFMD2 FMD= ',I4,' NFMODS= ',I3,' FMOD1=',I4,
     &       ' FWT1=',F7.2,' FMOD2=',I4,' FWT2=',F7.2,
     &       ' FMOD3=',I4,' FWT3=',F7.2,' FMOD4=',I4,' FWT4=',F7.2)

      RETURN
      END
