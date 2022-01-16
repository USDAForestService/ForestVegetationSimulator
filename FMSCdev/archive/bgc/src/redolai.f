        SUBROUTINE REDOLAI
C  This subroutine written 6/30/03 AJ McMahan, INTECS International, Inc for
C  FHTET, Ft Collins, CO.
C  This subroutine recalculates the initialized total leaf area (LA) of 
C  individual trees. Heretofore, the amount of LA assigned to each entity was
C  a function of bole volume and DBH.  No crown information was provided.
C  So, without this change, two trees--identical in Ht and DBH but with 
C  differing crowns--would be (unrealistically) assigned the SAME AMOUNT of 
C  total leaf area; resulting in a longer-crowned trees having a
C  disproportionately low foliage density, and shorter crowned trees having
C  disproportionately large foliage density.  Since LA diretly controls the
C  size of an entity's water bucket, this mis-representation of LA can result
C  (at least in a water limited system) in the shorter crowned trees having a
C  competative advantage over long-crowned trees over simulated time.
C  This subroutine attempts to ameliorate this behaviour by modifying how LA
C  is initialized.
C  In lieu of a better method to assign more leaf area to larger crowns, we
C  we make this relatively simple, minor adjustment.  The total LA assigned
C  to a stand (i.e. the sum for all of the [large tree*] entities) will not 
C  change from that assigned by the original, unchanged code.  What this 
C  routine does is re-distribute the leaf area in the stand in such a way 
C  that the amount of LA assigned to each entity is PROPORTIONAL TO EACH 
C  ENTITY'S CROWN VOLUME..
C
C  * Large trees in this sense are the tree entities that can access both
C  upper and lower water buckets, which are trees that are GE 1.3m tall.
C
C  Since this subroutine is called (from within BSTRUCTN) after GEOMETRY, CW
C  and CL info is available (for first DO-loop).  This subroutine is
C  necessarily called immediately before the call to LAIJ, where each
C  entity's crown layer's leaf areas are determined.
C
C Amended 7/7/03  Adding an alternative method for redistribution of LA.
C Instead of prorating LA by proportion of total crown VOLUME represented by
C entity's crown VOLUME; now use SURFACE AREA of entity's cone/crown. We'll
C use identical logic/loop structure as the crown-volume based logic.  User
C dictates which method to use via the B2(9) parameter in BETA.dat. [i.e. we
C maintain the "co-opt"ing of the BETA.dat file's B2(9) parameter.]
C If B2(9) = 1.0, use volume logic; if B2(9) = -1.0, we go to SA logic.
C Note: this "flagging" necessitates a change to BSTRUCTN.f subroutine as well.

C THis Version3 adjusts root biomass as a function of leaf biomass (as the 
C original code (without this subroutine) does.
C
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
C
C New Local Variable declarations:
C
      REAL VCRN(SIZE),TOTCRNVL, SACRN(SIZE),TOTCRNSA
      REAL TEMPLA1(SIZE),TEMPLA2(SIZE),TEMPLA3(SIZE)
C
C Begin
C Get each entity's crown volume, surface area, and accumulate total stand
C crown volume and surface areas.
c      IF (B2(9) .EQ. 1.0) THEN
      DO 10 I=1,NB
         IF ((ID(I) .EQ.'T').AND. (H(I) .GE. HTLIMIT)) THEN
            VCRN(I)=(1./3.)*3.1415*((CW(I)/2.)**2)*(CTOP(I)-CBOT(I))
            TOTCRNVL=TOTCRNVL+VCRN(I)
            SACRN(I)=3.1415*(CW(I)/2)*
     &               SQRT(((CW(I)/2.)**2)+((CTOP(I)-CBOT(I))**2))
            TOTCRNSA=TOTCRNSA+SACRN(I)
         ENDIF
   10 CONTINUE
C
C  Calculate new LAs.based on three methods, Volume(VOL), Surface Area (SA), 
C or hybrid (mean of VOL and SA)
C  Note: SUMLA_LG is from BINITIAL; it represents the total leaf area on an
C  entire hectare (i.e. the individual entity LAs have been expanded
C  that is, they have been multiplied by EXPAND(I)

      DO 20 I=1,NB
         IF ((ID(I) .EQ. 'T').AND.(H(I) .GE. HTLIMIT)) THEN
            TEMPLA1(I)=(SUMLA_LG * (VCRN(I)/TOTCRNVL))/EXPAND(I)
            TEMPLA2(I)=(SUMLA_LG * (SACRN(I)/TOTCRNSA))/EXPAND(I)
            TEMPLA3(I)=(TEMPLA1(I)+TEMPLA2(I))/2.0
      ENDIF
   20 CONTINUE
C      ENDIF  !END IF B2(9) = 1.0
C
C *************************************************************************
C
C NOW, BASED ON FLAG, ASSIGN APPROPRIATE NEW LA TO ENTITY
C
C METHOD USED DICTATED BY USER VIA B2(9) FLAG [1,-1, OR -3].
C
      IF (B2(9) .EQ. 1.) THEN
         DO 30 I=1,NB
         IF ((ID(I) .EQ. 'T').AND.(H(I) .GE. HTLIMIT)) THEN
            LA(I)=TEMPLA1(I)
            LEAF(I)=LA(I)/B1(13,1)
            ROOT(I)=2.0*LEAF(I)
         ENDIF
   30    CONTINUE

      ELSE IF (B2(9) .EQ. -1.) THEN
         DO 40 I=1,NB
         IF ((ID(I) .EQ. 'T').AND.(H(I) .GE. HTLIMIT)) THEN
            LA(I)=TEMPLA2(I)
            LEAF(I)=LA(I)/B1(13,1)
            ROOT(I)=2.0*LEAF(I)
         ENDIF
   40    CONTINUE

      ELSE IF (B2(9) .EQ. -3.) THEN
         DO 50 I=1,NB
         IF ((ID(I) .EQ. 'T').AND.(H(I) .GE. HTLIMIT)) THEN
            LA(I)=TEMPLA3(I)
            LEAF(I)=LA(I)/B1(13,1)
            ROOT(I)=2.0*LEAF(I)
         ENDIF
   50    CONTINUE
      ENDIF
      RETURN
      END