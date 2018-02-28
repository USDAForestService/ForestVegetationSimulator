      SUBROUTINE FMSOILHEAT (IYR, LNMOUT)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     CALLED FROM: FMBURN
C     CALLS fm_fofem
C
C  PURPOSE:
C     THIS SUBROUTINE IS THE INTERFACE BETWEEN FFE AND THE FOFEM SOIL HEADING
C     MODEL.
C
C  CALL LIST DEFINITIONS:
C     IYR:  CURRENT YEAR
C     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
C     LNMOUT: TRUE IF NORMAL OUTPUT PROCESS, FALSE TO SUPPRESS ALL OUTPUTS
C
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      integer iyr
      logical lnmout
      character fofem_msg(101), VVER*7
      real fofem_input(32),fofem_output(30)
      integer fofem_rc
      logical debug
      integer i,jrout
      integer j,k,l,regnum,covnum,seasnum,moiscode
      real smcat(4), duffcat(4), algslp
      real MOIS1(2,5), MOIS2(2,5), MOIS3(2,5), MOIS4(2,5), soilmois

C if _WIN64
      !DEC$ ATTRIBUTES DLLIMPORT :: FM_FOFEM
C else
      !DEC$ ATTRIBUTES DLLIMPORT :: FM_FOFEM
      !DEC$ ATTRIBUTES ALIAS:'_FM_FOFEM' :: FM_FOFEM
C endif

      integer fm_fofem
      
      data SMCAT /5,10,15,25/      
C     check for debug

      call dbchk (debug,'FMSOILHEAT',10,icyc)
      if (debug) write(jostnd,7) icyc, iyr, isheatb, isheate, idsheat
    7 format(' fmsoilheat cycle=',i2,' iyr=',i5,' isheatb,isheate=',
     >         2I5,' idsheat=',I5)

      if (isheatb.eq.9999 .or. isheate.eq.9999) return
      if (iyr .lt. abs(isheatb) .or. iyr .ge. isheate) return

      if (idsheat.eq.0) call getid(idsheat)

            ! C subscript +1 = Fortran subscript
      fofem_input (  0 +1) = 100   ! maximum length of msg string
C
C     Zero the summation columns (index 1=3 and 4=5)
C
      DO I = 1, MXFLCL
         DO J = 1, 2
            CWD(3,I,J,5) = 0.0
         ENDDO
      ENDDO

C     Calculate summation in Pile categories

      DO I = 1, 2
         DO J = 1, MXFLCL
            DO K = 1, 2
               DO L = 1, 4
                  CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
C     This assumes that "hard" surface fuel is sound and "soft" surface
C     fuel is rotten.  It also assumes that half of the 6-12" stuff is
C     6-9" and the other half is 9-12".  
C
      fofem_input (  1 +1) = CWD(3,1,1,5) + CWD(3,1,2,5) ! 1 hour wood
      fofem_input (  2 +1) = CWD(3,2,1,5) + CWD(3,2,2,5) ! 10 hour wood
      fofem_input (  3 +1) = CWD(3,3,1,5) + CWD(3,3,2,5) ! 100 hour wood

      fofem_input (  4 +1) = CWD(3,4,2,5)   ! Sound Large Woods
      fofem_input (  5 +1) = 0.5*CWD(3,5,2,5)
      fofem_input (  6 +1) = 0.5*CWD(3,5,2,5) + CWD(3,6,2,5)
      fofem_input (  7 +1) = CWD(3,7,2,5)+ CWD(3,8,2,5)+ CWD(3,9,2,5)

      fofem_input (  8 +1) = CWD(3,4,1,5)    ! Rotten Large Wood
      fofem_input (  9 +1) = 0.5*CWD(3,5,1,5)
      fofem_input ( 10 +1) = 0.5*CWD(3,5,1,5) + CWD(3,6,1,5)
      fofem_input ( 11 +1) = CWD(3,7,1,5)+ CWD(3,8,1,5)+ CWD(3,9,1,5)

      fofem_input ( 12 +1) = CWD(3,11,1,5) + CWD(3,11,2,5) ! Duff load
      fofem_input ( 12 +1) = MAX (0.45, fofem_input ( 12 +1))

C     this duff depth assumes that duff is 12 tons/acre/inch

      ! Duff depth, inches
      fofem_input ( 13 +1) = (CWD(3,11,1,5) + CWD(3,11,2,5))/12 
      ! Litter Load
      fofem_input ( 14 +1) = CWD(3,10,1,5) + CWD(3,10,2,5)  
      fofem_input ( 15 +1) = FLIVE(1)  ! Herb load
      fofem_input ( 16 +1) = FLIVE(2)  ! Shrub

      ! Crown Foliage load
      fofem_input ( 17 +1) = 0.5*TCLOAD*P2T*43560
      ! Crown Branch load
      fofem_input ( 18 +1) = 0.5*TCLOAD*P2T*43560
      ! Percent of crown that will burn
      fofem_input ( 19 +1) = CRBURN*100

      fofem_input ( 20 +1) = MOIS(1,2)*100  ! 10 Hr Moisture
      fofem_input ( 21 +1) = MOIS(1,4)*100  ! 1000 Hr Moisture
      fofem_input ( 22 +1) = MOIS(1,5)*100  ! Duff Moisture

C     get default fuel moistures for this variant

      CALL FMMOIS(1, MOIS1)
      CALL FMMOIS(2, MOIS2)
      CALL FMMOIS(3, MOIS3)
      CALL FMMOIS(4, MOIS4)
      
C     use duff moisture to determine the soil moisture and moisture code

      duffcat(1) = mois1(1,5)
      duffcat(2) = mois2(1,5)
      duffcat(3) = mois3(1,5)
      duffcat(4) = mois4(1,5)

      soilmois=ALGSLP(MOIS(1,5),DUFFCAT,SMCAT,4)

      IF (MOIS(1,5) .LE. ((MOIS1(1,5) + MOIS2(1,5))/2)) THEN
        moiscode = 1
      ELSEIF (MOIS(1,5) .LE. ((MOIS2(1,5) + MOIS3(1,5))/2)) THEN
        moiscode = 2
      ELSEIF (MOIS(1,5) .LE. ((MOIS3(1,5) + MOIS4(1,5))/2)) THEN
        moiscode = 3
      ELSE
        moiscode = 4
      ENDIF

      ! Soil Moisture
      fofem_input ( 23 +1) = soilmois
      ! Moisture Condition, 1-VeryDry,2-Dry,3-Moderate,4-Wet
      fofem_input ( 24 +1) = moiscode
      ! Soil Type,  1-LoaSke,2-FinSil,3-Fin,4-CoaSil,5-CoaLoa
      fofem_input ( 25 +1) = SOILTP
      ! Duff Moisture Method, 1-Entire, 2-Low, 3-NFDR, 4-AdjNFDR
      fofem_input ( 26 +1) = 1

      IF (BURNSEAS .EQ. 4) THEN
        seasnum = 3
      ELSEIF (BURNSEAS .EQ. 3) THEN
        seasnum = 2
      ELSE
        seasnum = 1
      ENDIF

      ! Season, 1-Spring, 2-Summer, 3-Fall, 4-Winter
      fofem_input ( 27 +1) = seasnum

      CALL VARVER(VVER)
      IF (VVER(1:2) .EQ. 'NE' .OR.
     &    VVER(1:2) .EQ. 'LS' .OR.
     &    VVER(1:2) .EQ. 'CS') THEN
         regnum = 4
      ELSEIF (VVER(1:2) .EQ. 'SN') THEN
         regnum = 3
      ELSEIF (VVER(1:2) .EQ. 'PN' .OR.
     &        VVER(1:2) .EQ. 'WC' .OR.
     &        VVER(1:2) .EQ. 'OP' .OR.
     &        VVER(1:2) .EQ. 'NC') THEN
         regnum = 2
      ELSE
         regnum = 1
      ENDIF

      ! Region, 1-InteriorWest, 2-Pacific, 3-SE, 4-NE
      fofem_input ( 28 +1) = regnum
      ! Fuel Category, 1-Natural, 2-Pile, 3-Slash
      fofem_input ( 29 +1) = 1
      
      IF (VVER(1:2) .EQ. 'NE') THEN
         covnum = 8
      ELSEIF (VVER(1:2) .EQ. 'LS') THEN
         covnum = 7
      ELSE
         covnum = 5
      ENDIF

      ! Cover Group:
      ! 0-None, 1-Grass, 2-Sage, 3-Shrub,  4-Pocosin,
      ! 5-Ponderosa, 6-White Pine Hemlock , 7-Red Jack Pine,
      ! 8-Balsam, Blk Red Whit Spruce      
      fofem_input ( 30 +1) = covnum
      ! Output flag, 0=no output, 1=a file of output.
      fofem_input ( 31 +1) = 0

      if (debug) write (jostnd, 10) fofem_input
   10 format (/' FMSOILHEAT, FOFEM_INPUT:',32F7.2)

      fofem_rc = fm_fofem(fofem_input,fofem_output,fofem_msg)

      if (fofem_rc.gt.0) then
         write (jostnd, 20) fofem_msg(1:FOFEM_RC)
   20    format (/' ******** ERROR: FMSOILHEAT MESSAGE TEXT:', A)
         call rcdset (2,.true.)
         return
      endif

      call getlun (jrout)

      if (isheatb.lt.0) then
         isheatb=-isheatb
         write (jrout,30) idsheat,idsheat,idsheat,idsheat,nplt,
     >                    mgmid,idsheat,idsheat,(i,i=0,13),idsheat
   30    format (1x,i5/1x,i5,1x, 75('-')/1x,i5,t21,
     >     'FOFEM SOIL HEATING ESTIMATES FOR SIMULATED FIRES'/
     >     1x,i5,' STAND ID: ',A,' MGMT ID: ',A,T71,'DEPTH WHERE'/
     >     1x,i5,'       --TEMP IN CELCIUS AT THESE DEPTHS ',
     >               'BELOW SURFACE (CM)---  TEMP EXCEEDS'/
     >     1x,i5,' YEAR ',14i4,'   60  275 C'/
     >     1x,i5,1x, 75('-'))
      endif

      write (jrout,40) idsheat,iyr,
     >                 (int(fofem_output(i)),i=1,16)
   40 format (1x,i5,1x,i4,1x,14i4,2i5)


      return
      end
      
