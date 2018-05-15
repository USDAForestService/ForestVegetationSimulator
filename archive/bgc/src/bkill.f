      SUBROUTINE BKILL(FVS_CYC)
C------------------------------
C Subroutine to remove dead trees from live tree list. Called
C from TRNOVR.FOR -- DWC,5/16/94.
C Changed unit=12 file into permanent output file for mortality
C in GSV.FOR and eliminated REWIND(12) statement -- DWC,9/15/94.
C
C MORTALITY:  When a negative crown ratio occurs, the tree is
C   effectively killed. It is removed from the live tree list, and
C   the live tree list is compressed. The growth cycle then begins
C   a new year.  NOTE: Currently, there is no time lag between the
C   death of a tree and the crown ratio falling below zero.
C
C Added STAT(I) to new tree list, variable needed by BTRNOVR.  AJM 2/7/01
C
C Amended the WRITE(74,500) and WRITE(74,1000) to allow BA to be written
C F5.2 (was F3.2).  AJM 6/6/03
C amended 7/03.  this change corresponds to change in bentyload, bentyupdate, 
C and bupdate regarding parameter "A_", the shape parameter for the small tree
C bole shape parabolas.  This parameter needs to be written to the scratch file
C (file 69) and re-read in, below.
C
C-------------------------------

      INCLUDE 'ENTITY.F77'
      INTEGER FVS_CYC
      NDF=0
      NLF=0
      NLT=0

C read the current tree list and segregate into live and dead lists.
C Amended 10/00 ajm.  added fvs_cyc to output table, reformatted table.
C Amended output file again 10/24/00 ajm
C
C Ammended the new live tree list to include variable STAT, used in BTRNOVR.  It keeps track of the
C number of years of PSN > MRESP for each entity.  Without this, if x number of entities die, then
C STAT each entity J occuring--in a sequential list of entity numbers--after the removed (dead)
C entities will inherit the STAT variable from entity number J-n, where J = the entity's number
C before other entity mortality.  AJM 2/7/01
C
      DO 10 I=1,NB
        IF(Z1(1).EQ.1 .AND. I.EQ.1) WRITE(68,500)
        IF(IDEADFLG(I).EQ.1) THEN
          WRITE(68,1000) FVS_CYC,Z1(1),ID(I),TREENO(I),SPP(I),BD(I),
     +                   D(I),H(I),CR(I),TPH(I),! PCOVER(I),EXPAND(I),
     +                   BA(I),CCF(I),LA(I),LEAF(I),STEM(I),ROOT(I)
          NDF=NDF + 1
          IDEADFLG(I)=0
        ELSE
          WRITE(69,2000) BD(I), D(I), H(I), CR(I), TPH(I), EXPAND(I),
     +                   TREENO(I), BA(I), CCF(I), LA(I), PCOVER(I),
     +                   LEAF_ON(I), FROOT_ON(I), TLEAFON(I),
     +                   IBLB(I), BGCISP(I), ID(I), SPP(I),
     +                   LEAF(I), STEM(I), ROOT(I),
     +                   LEAFCF(I), STEMCF(I), FROOTCF(I),
     +                   SUMDG(I), SUMHG(I),
     +                   O_BD(I), O_D(I), O_H(I), O_CR(I), O_TPH(I),
     +                   O_BA(I), O_LA(I),
     +                   O_LEAF(I), O_STEM(I), O_ROOT(I),
     +                   STAT(I),A_(I)                          !Added 2/01 ajm
	  NLF=NLF + 1
          IF(ID(I).EQ.'T') NLT=NLT+1
        ENDIF
   10 CONTINUE
      REWIND(69)
      WRITE(*,*)
      WRITE(*,*) 'NUMBER LIVE PLANTS= ',NLF

C create a new live tree list for subsequent runs.
      DO 20 J=1,NLF
        READ(69,2000) BD(J), D(J), H(J), CR(J), TPH(J), EXPAND(J),
     +                TREENO(J), BA(J), CCF(J), LA(J), PCOVER(J),
     +                LEAF_ON(J), FROOT_ON(J), TLEAFON(J),
     +                IBLB(J), BGCISP(J), ID(J), SPP(J),
     +                LEAF(J), STEM(J), ROOT(J),
     +                LEAFCF(J), STEMCF(J), FROOTCF(J),
     +                SUMDG(J), SUMHG(J),
     +                O_BD(J), O_D(J), O_H(J), O_CR(J), O_TPH(J),
     +                O_BA(J), O_LA(J),
     +                O_LEAF(J), O_STEM(J), O_ROOT(J),
     +                STAT(J),A_(J)                          !Added 2/01 ajm
   20 CONTINUE

      REWIND(69)
      O_NB=NB
      NB=NLF
      NT=NLT
C  500 FORMAT(/,'FVS',T6,'YR',T9,'ID',T16,'TREE',T21,'SP',T26,'BD',
C     +         T32,'DBH',T39,'HT',T43,'CR',T48,'TPH',T54,'COV',T60,
C     +         'EXP',T65,'BA',T69,'CCF',T78,'LA',T84,
C     +         'LEAF',T92,'STEM',T100,'ROOT',/,'CYCLE',
C     +         T25,'(cm)',T31,'(cm)',T38,'(m)',T54,'(%)',T64,'(m2)',
C     +         T77,'(m2)',T83,'(kgC)',T91,'(kgC)',T99,'(kgC)',/,
C     +'=================================================================
C     +========================================')
C...Removed %cover and the redundant EXP.  AJM 10/24/00
C   Amendeding header again 6/03 to provide space for expanded BA number (see 1000FORMAT change
C
C  500 FORMAT(/,T2,'FVS',T6,'YR',T9,'ID',T16,'TREE',T21,'SP',T26,'BD',
C     +         T32,'DBH',T39,'HT',T43,'CR',T48,'TPH',
C     +         T54,'BA',T58,'CCF',T67,'LA',T73,
C     +         'LEAF',T81,'STEM',T89,'ROOT',/,T2,'CYC',
C     +         T25,'(cm)',T31,'(cm)',T38,'(m)',T53,'(m2)',
C     +         T66,'(m2)',T72,'(kgC)',T80,'(kgC)',T88,'(kgC)',/,
C     +'=================================================================
C     +============================')
  500 FORMAT(/,T2,'FVS',T6,'YR',T9,'ID',T16,'TREE',T21,'SP',T26,'BD',
     +         T32,'DBH',T39,'HT',T43,'CR',T48,'TPH',
     +         T54,'BA',T60,'CCF',T69,'LA',T75,
     +         'LEAF',T83,'STEM',T91,'ROOT',/,T2,'CYC',
     +         T25,'(cm)',T31,'(cm)',T38,'(m)',T53,'(m2)',
     +         T68,'(m2)',T74,'(kgC)',T82,'(kgC)',T90,'(kgC)',/,
     +'=================================================================
     +==============================')
C 1000 FORMAT(I4,F4.0,1X,A1,1X,I8,1X,A2,1X,3(F5.1,1X),F3.2,1X,F6.1,1X,
C     +       F3.2,1X,F4.1,1X,F7.2,3(1X,F7.2)) !Ammending 6/03 ajm.  BA is
C             inappropriately formatted (in this commented out block)
 1000 FORMAT(I4,F4.0,1X,A1,1X,I8,1X,A2,1X,3(F5.1,1X),F3.2,1X,F6.1,1X,
     +       F5.2,1X,F4.1,1X,F7.2,3(1X,F7.2))
 2000 FORMAT(6F14.8,I8,4F14.8,L7,L7,3I3,A1,A2,18F14.8,F4.1)

      RETURN
      END
