      SUBROUTINE ECIN(IRECNT, IREAD, JOSTND, NSP, ICYC, LKECHO, ISPGRP)
C----------
C **ECIN--ECON  DATE OF LAST REVISION: 03/03/2012
C----------
C  Author Fred Martin, WA DNR,

! Processes FVS/ECON keywords for each simulation.
!  Contains entry ECKEY (activityId, keyword) returns an activity code given a keyword name

! Called from initre.f whenever an ECON keyword is read.

! Variables from FVS
!  ICYC   - current cycle number
!  IREAD  - logical unit number from which keyword records are read
!  IRECNT - counter for number of keyword records processed
!  ISPGRP - array holding species that have been defined as a group, from CONTRL.F77
!  JOSTND - logical unit number for writing standard output file
!  LKECHO - echo keywords & parameter values to the main output file, true by default. Controled with echok and noecho keywords.
!  LMODE  - logical true if ECON scheduled via Event Monitor and false if options scheduled normally, set in call to addEvent
!  MAXSP  - maximum number of species codes used by a specific variant, from PRGPRM.F77
!  NSP    - array of species character codes used by a specific variant, includes both species and tree-value-class codes

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'

      character (len=5), dimension(2), parameter :: BOOLEAN(0:1) = 
     &                                              (/'FALSE', 'TRUE '/) 
      character (len=8), dimension(5), parameter :: KEYWORD_TABLE =      !Names of keywords/activities recognized by Event Monitor
     &   (/ 'PRETEND ', 'SEVSTART', 'SPECCST ', 'SPECRVN ', 'STRTECON'/) !Coded in OPLIST as 1601-1605

      character (len=8)                :: KEYWRD, keyword
      character (len=10)               :: tmpLabel
      character (len=10), dimension(7) :: charFields
      character (len=12)               :: warn = ' ********   '
      character (len=4), intent(in), dimension(MAXSP,3) :: NSP           !Dimension defined in PLOT.F77

      integer :: i, k, l, activityId, errCode, lastSpId, parmsField,
     &           rateCnt, spId, units, IRTNCD
      integer, intent(in out) :: ICYC, IREAD, IRECNT, JOSTND
cc      integer, save                         :: specCstCnt, specRvnCnt
      integer, dimension (MAX_RATES)        :: tmpDurations
      integer, intent(in), dimension(10,52) :: ISPGRP                    !See CONTRL.F77 for description & array dimensions
      integer, parameter :: badCycle    =1, badUoM      =2, badValue=3,
     &                      maxKeyWds   =4, revMaxKeyWds=5,
     &                      revDuplicate=6, lbsDupSp    =7

      logical                :: LMODE, LKECHO, LFLAG, hasError
      logical, dimension (7) :: isNotBlank

      real                        :: price
      real, dimension (7)         :: realFields
      real, dimension (MAX_RATES) :: tmpRates

      LFLAG    = .FALSE.

!    Read keywords, first keyword expected to be ECON
      readKeyWd: do                                                      !Exit on keyword=END or KEYRDR=EOF, errCode=2
         isEconToBe = .TRUE.
         CALL KEYRDR(IREAD, JOSTND, .FALSE., KEYWRD, isNotBlank,
     &           realFields, IRECNT, errCode, charFields, LFLAG, LKECHO) !KEYRDR fills realField w/ 0.0 unless actual number
         if (errCode == 2) then
           call ERRGRO(.FALSE., 2)                       !.FALSE. causes ERRGRO to flag error condition, 2 = EOF before END keyword
           CALL getfvsRtnCode(IRTNCD)
           IF (IRTNCD.NE.0) RETURN         
         endif
         parmsField = 0
         if (errCode < 0) parmsField = -errCode                          !Negative errCode used to return field containing "PARMS" keyword

         KEYWRD = adjustl(KEYWRD)
         select case (KEYWRD)
          
!        ====================== CASE ANNUCST ===========================
         case ('ANNUCST')
            if (annCostCnt == MAX_KEYWORDS) then
               call errMsg(maxKeywds)
               cycle readKeyWd                                           !go read next keyword
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

            annCostCnt = annCostCnt + 1
            annCostAmt(annCostCnt) = realFields(1)
            call ratesAndDurations(charFields(3), MAX_RATES, rateCnt,
     &                             annCostRate(annCostCnt,1:MAX_RATES),
     &                             annCostDur(annCostCnt,1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/, 1x, a8, "   ANNUAL COST OF $", F7.2,
     &              " WILL BE APPLIED FOR: ", a, ".")') KEYWRD,
     &              annCostAmt(annCostCnt), trim(adjustl(charFields(2)))
               do i = 1, rateCnt
                  write (JOSTND,'(T13, "APPRECIATION RATE: ", F6.1,
     &                                         "% FOR", i4, " YEARS.")')
     &               annCostRate(annCostCnt,i), annCostDur(annCostCnt,i)
               end do
            end if

!        ====================== CASE ANNURVN ===========================
         case ('ANNURVN')
            if (annRevCnt == MAX_KEYWORDS) then
               call errMsg(maxKeywds)
               cycle readKeyWd                                           !go read next keyword
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !go read next keyword
            end if

            annRevCnt = annRevCnt + 1
            annRevAmt(annRevCnt) = realFields(1)
            call ratesAndDurations(charFields(3), MAX_RATES, rateCnt,
     &                             annRevRate(annRevCnt, 1:MAX_RATES),
     &                             annRevDur(annRevCnt, 1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/,1x, a8, "   ANNUAL REVENUE OF $", F7.2,
     &                " WILL BE APPLIED FOR: ", a, ".")') KEYWRD,
     &                annRevAmt(annRevCnt), trim(adjustl(charFields(2)))
               do i = 1, rateCnt
                  write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                "% FOR", i4, " YEARS.")') annRevRate(annRevCnt,i),
     &                annRevDur(annRevCnt,i)
               end do
            end if

!        =======================  CASE BURNCST =========================
         case ('BURNCST')
            if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !go read next keyword
            end if

            burnCostAmt = realFields(1)
            call ratesAndDurations(charFields(2), MAX_RATES, rateCnt,
     &                             burnCostRate, burnCostDur)

            if (LKECHO) then
               write (JOSTND,'(/, 1x, a8, "   COST: $", f5.0,
     &                              " PER ACRE.")') KEYWRD, burnCostAmt
               do i = 1, rateCnt
                  write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                                   "% FOR ", i4, " YEARS.")')
     &                                   burnCostRate(i), burnCostDur(i)
               end do
            end if

!        ======================= CASE END ==============================
         case ("END")
            if (LKECHO)
     &         write (JOSTND,'(/, 1x, a8,3x,"END ECONOMIC EXTENSION ",
     &                                             "KEYWORDS.")') KEYWRD
         return                                                          !no more ECON keywords

!        ======================= CASE HRVFXCST =========================
         case ('HRVFXCST')
            if (fixHrvCnt == MAX_KEYWORDS) then
               call errMsg(maxKeywds)
               cycle readKeyWd                                           !go read next keyword
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !go read next keyword
            end if

            fixHrvCnt = fixHrvCnt + 1
            fixHrvAmt(fixHrvCnt) = realFields(1)
            call ratesAndDurations(charFields(3), MAX_RATES, rateCnt,
     &                             fixHrvRate(fixHrvCnt, 1:MAX_RATES),
     &                             fixHrvDur(fixHrvCnt, 1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/, 1x, a8, "   COST: $", f5.0,
     &                " PER ACRE, FOR: ", a, ".")') KEYWRD,
     &                fixHrvAmt(fixHrvCnt), trim(adjustl(charFields(2)))
               do i = 1, rateCnt
                  write (JOSTND,'(T13, "APPRECIATION RATE: ", F6.1,
     &                                        "% FOR ", i4, " YEARS.")')
     &                                        fixHrvRate(fixHrvCnt, i),
     &                                        fixHrvDur(fixHrvCnt, i)
               end do
            end if

!        ======================= CASE HRVVRCST =========================
         case ("HRVVRCST")
            if (varHrvCnt == MAX_KEYWORDS) then
               call errMsg(maxKeywds)
               cycle readKeyWd                                           !go read next keyword
            else if (.not.isNotBlank(1) .or. realFields(1) < 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !go read next keyword
            else if (.not.isCorrectUnit(KEYWRD,int(realFields(2)))) then
               call errMsg(badUoM)
               cycle readKeyWd                                           !go read next keyword
            end if

            varHrvCnt              = varHrvCnt + 1
            varHrvAmt(varHrvCnt)   = realFields(1)
            varHrvUnits(varHrvCnt) = int(realFields(2))

            varHrvDbhLo(varHrvCnt) = realFields(3)
            if (realFields(4)>0.0) varHrvDbhHi(varHrvCnt) =realFields(4) !Else default value, 999.0

            call ratesAndDurations(charFields(5), MAX_RATES, rateCnt,
     &                             varHrvRate(varHrvCnt, 1:MAX_RATES),
     &                             fixHrvDur(fixHrvCnt, 1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/1x, a8, "   HARVEST COST: $", f5.0,
     &             " PER ", a, ", FOR TREES >= ", f5.1, " AND < ", f5.1,
     &             " INCHES DBH.")') KEYWRD, varHrvAmt(varHrvCnt),
     &                    trim(UNITS_LABEL(varHrvUnits(varHrvCnt))),
     &                    varHrvDbhLo(varHrvCnt), varHrvDbhHi(varHrvCnt)
               do i = 1, rateCnt
                  write (JOSTND,'(T13, "APPRECIATION RATE: ", F6.1,
     &                                        "% FOR ", i4, " YEARS.")')
     &                                        varHrvRate(varHrvCnt, i),
     &                                        varHrvDur(varHrvCnt, i)
               end do
            end if

!        ======================= CASE HRVRVN ===========================
         case ('HRVRVN')
            if (.not.isNotBlank(1) .or. realFields(1) < 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            else if (.not.isCorrectUnit(KEYWRD,int(realFields(2)))) then
               call errMsg(badUoM)
               cycle readKeyWd                                           !Go read next keyword
            end if

!           Check for alpha species and decode to integer value
            call SPDECD(4, spId, NSP(1,1), JOSTND, IRECNT,
     &                                   KEYWRD, realFields, charFields) !4=field containing species, realFields & charFields passed but not altered
            if (spId == -999) cycle readKeyWd                            !-999 = species not found, error handled by SPDECD

            call ratesAndDurations(charFields(5), MAX_RATES, rateCnt, 
     &                            tmpRates, tmpDurations)

            units = int(realFields(2))                                   !Revenue units-of-measure
            price = realFields(1)
            select case (units)                                          !Correct price for units-of-measure
               case (FT3_100, FT3_100_LOG)
                  price = realFields(1) / 100.0
               case (BF_1000, BF_1000_LOG)
                  price = realFields(1) / 1000.0
            end select

!          Assign prices to speices, UoM, & DBH or DIB classes
            lastSpId = 0
            if (spId == 0) then                                          !Set values for all species not already set
               allSpecies: do i = 1, MAXSP
                  if (hrvRevCnt(i,units)>=MAX_KEYWORDS) then
                     call errMsg(revMaxKeyWds)
                     cycle allSpecies                                    !Try next species in loop
                  end if
                  if (.not.hasRevAmt(i,units)) then                      !Value not previously set by species specific keywords
                     do k = 1, hrvRevCnt(i,units)
                        if (hrvRevDia(i,units,k) == realFields(3)) then  !realFields(3) = diameter
                           call errMsg(revDuplicate)
                           cycle allSpecies                              !Try next species in loop
                        end if
                     end do
                     hrvRevCnt(i,units)        = hrvRevCnt(i,units) + 1
                     k                         = hrvRevCnt(i,units)      !Represents a diameter
                     hrvRevPrice(i,units,k)    = price
                     hrvRevDia(i,units,k)      = realFields(3)
                     hrvRevRate(i,units,k, : ) = tmpRates
                     hrvRevDur(i,units,k, : )  = tmpDurations
                     lastSpId                  = i                       !Indicates at least one species was set
                  end if
               end do allSpecies
               if (lastSpId == 0) cycle readKeyWd                        !All species were previously set, go read another keyword
            elseif (spId < 0) then                                       !A species group is specified
               groupSpecies: do l = 2, ISPGRP(-spId,1) + 1
                  i = ISPGRP(-spId,l)
                  if (hrvRevCnt(i,units)>=MAX_KEYWORDS) then
                     call errMsg(revMaxKeyWds)
                     cycle groupSpecies                                  !Try next species in loop
                  end if
                  do k = 1, hrvRevCnt(i,units)
                     if (hrvRevDia(i,units,k) == realFields(3)) then     !realFields(3) = diameter
                        call errMsg(revDuplicate)
                        cycle groupSpecies                               !Try next species in loop
                     end if
                  end do
                  hasRevAmt(i,units)        = .TRUE.                     !Marks that a single species or species group revenue keyword has been specified
                  hrvRevCnt(i,units)        = hrvRevCnt(i,units) + 1
                  k                         = hrvRevCnt(i,units)         !Represents a diameter
                  hrvRevPrice(i,units,k)    = price
                  hrvRevDia(i,units,k)      = realFields(3)
                  hrvRevRate(i,units,k, : ) = tmpRates
                  hrvRevDur(i,units,k, : )  = tmpDurations
                  lastSpId                  = i                          !Indicates at least one species was set
               end do groupSpecies
               if (lastSpId == 0) cycle readKeyWd                        !All species were previously set, go read another keyword
            else                                                         !Individual species specified
               if (hrvRevCnt(spId,units) >= MAX_KEYWORDS) then
                  call errMsg(revMaxKeyWds)
                  cycle readKeyWd                                        !Go read next keyword
               end if
               do k = 1, hrvRevCnt(spId,units)                           !Represents a diameter
                  if (hrvRevDia(spId,units,k) == realFields(3)) then     !realFields(3) = diameter
                     call errMsg(revDuplicate)
                     cycle readKeyWd                                     !Go read next keyword
                  end if
               end do
               hasRevAmt(spId,units)        = .TRUE.                     !Marks that a single species or species group revenue keyword has been specified
               hrvRevCnt(spId,units)        = hrvRevCnt(spId,units) + 1
               k                            = hrvRevCnt(spId,units)      !Represents a diameter
               hrvRevPrice(spId,units,k)    = price
               hrvRevDia(spId,units,k)      = realFields(3)
               hrvRevRate(spId,units,k, : ) = tmpRates
               hrvRevDur(spId,units,k, : )  = tmpDurations
            end if

            if (LKECHO) then
               write (JOSTND,'(/, 1x, a8, "   HARVEST PRICE: $", f5.0,
     &                      " PER ", a, " >=", f5.1, " INCHES DIB/DBH ",
     &                      "FOR SPECIES ", a, ".")') KEYWRD,
     &                      realFields(1), trim(UNITS_LABEL(units)),
     &                      realFields(3), trim(adjustl(charFields(4)))
               if (spId == 0) spId = lastSpId                            !Set species ID from ALL or group to real species - need for correct rate/duration values
               do l = 1, rateCnt
                  write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                                        "% FOR ", i4, " YEARS.")')
     &                                       hrvRevRate(spId,units,k,l),
     &                                       hrvRevDur(spId,units,k,l)
               end do
            end if

!        =================== CASE LBSCFV ===============================
         case('LBSCFV')
            if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

!          Check for alpha species and decode to integer value
            call SPDECD(2, spId, NSP(1,1), JOSTND, IRECNT,
     &                                   KEYWRD, realFields, charFields) !2=field containing species
            if (spId == -999) cycle readKeyWd                            !-999 = species not found, error handled by SPDECD

            if (spId == 0) then                                          !Set values for all species not already set
               do i = 1, MAXSP
                  if (lbsFt3Amt(i) <= 0.0) then                          !value not previously set
                     lbsFt3Amt(i) = realFields(1)
                     lastSpId     = i                                    !Indicates at least one species was set
                  end if
               end do
               if (lastSpId == 0) then
***                  write (JOSTND,'(/,1x,a12, a8, " - KEYWORD IGNORED, ",
***     &                  "ALL INDIVIDUAL SPECIES PREVIOUSLY SET, RECORD",
***     &                  i4)') warn, KEYWRD, IRECNT
***                  call RCDSET(1,.TRUE.)                                  !.TRUE. causes RCDSET to return, 
                  call errMsg(lbsDupSp)
                  cycle readKeyWd                                        !Go read next keyword
               end if
            else if (spId < 0) then                                      !Species group is specified
               do i = 2, ISPGRP(-spId,1) + 1
                  if (lbsFt3Amt(i) > 0.0) then                           !Value  previously set for this species
                  call errMsg(lbsDupSp)
                  cycle readKeyWd                                        !Go read next keyword
               end if
                  lbsFt3Amt(ISPGRP(-spId,i)) = realFields(1)
               end do
            else                                                         !Individual species specified
               if (lbsFt3Amt(spId) > 0.0) then                           !Value previously set for this species
                  call errMsg(lbsDupSp)
                  cycle readKeyWd                                        !Go read next keyword
               end if
               lbsFt3Amt(spId) = realFields(1)
            end if

            if (LKECHO)
     &          write (JOSTND,'(/, 1x, a8, "   BIOMASS CONVERSION FOR ",
     &                       "SPECIES: ", a, ", BASED ON: ", F5.0,
     &                       "POUNDS PER FT3.")') KEYWRD,
     &                       trim(adjustl(charFields(2))), realFields(1)

!        =================== CASE MECHCST ==============================
         case ('MECHCST')
            if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

            mechCostAmt = realFields(1)
            call ratesAndDurations(charFields(2), MAX_RATES, 
     &                             rateCnt, mechCostRate, mechCostDur)

            if (LKECHO) then
               write (JOSTND,'(/, 1x, a8, "   COST: $", f5.0,
     &                              " PER ACRE.")') KEYWRD, mechCostAmt

               do i = 1, rateCnt
                  write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                       "% FOR ", i4, " YEARS.")') mechCostRate(i),
     &                       mechCostDur(i)
               end do
            end if

!        =================== CASE NOTABLE  =============================
         case ('NOTABLE')
            if (int(realFields(1)) == 1) then
               noOutputTables  = .TRUE.
               noLogStockTable = .TRUE.
               if (LKECHO)
     &            write (JOSTND,'(/, 1x, a8,"   ALL ECON OUTPUT ",
     &                                "TABLES ARE SUPPRESSED.")') KEYWRD
            else if (int(realFields(1)) == 2) then
               noLogStockTable = .TRUE.
               if (LKECHO)
     &            write (JOSTND,'(/, 1x, a8,"   ECON LOG STOCK VOLUME/",
     &                            "VALUE TABLE IS SUPPRESSED.")') KEYWRD
            end if

!        =================== CASE PCTFXCST   ===========================
         case ('PCTFXCST')
            if (fixPctCnt == MAX_KEYWORDS) then
               call errMsg(maxKeyWds)
               cycle readKeyWd                                           !Go read next keyword
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

            fixPctCnt = fixPctCnt + 1
            fixPctAmt(fixPctCnt) = realFields(1)
            call ratesAndDurations(charFields(3), MAX_RATES, rateCnt,
     &                             fixPctRate(fixPctCnt,1:MAX_RATES),
     &                             fixPctDur(fixPctCnt,1:MAX_RATES))

            if (LKECHO) then
               write(JOSTND,'(/, 1x, a8, "   COST: $", f5.0,
     &                " PER ACRE FOR:", a, ".")') KEYWRD,
     &                fixPctAmt(fixPctCnt), trim(adjustl(charFields(2)))

               do i = 1, rateCnt
                   write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                                        "% FOR ", i4, " YEARS.")')
     &                                        fixPctRate(fixPctCnt, i),
     &                                        fixPctDur(fixPctCnt, i)
               end do
            end if

!        =================== CASE PCTSPEC ==============================
         case ('PCTSPEC')
            if (.not.isCorrectUnit(KEYWRD, int(realFields(2)))) then
               call errMsg(badUoM)
               cycle readKeyWd                                           !Go read next keyword
            end if

            pctMinUnits = int(realFields(2))
            tmpLabel = 'CUBIC-FEET'
            if (pctMinUnits == BF_1000) tmpLabel = 'BOARD-FEET'

            if (realFields(1) > 0.0) pctMinDbh = realFields(1)           !Else default value
            if (realFields(3) > 0.0) pctMinVolume = realFields(3)        !Else default value

            if (LKECHO)
     &         write (JOSTND,'(/, 1x, a8,"   PRE-COMMERCIAL THINNING ",
     &               " DEFINED AS REMOVALS <", F6.0, 1x, a,
     &               " VOLUME PER ACRE AND  QMD < ", F4.1, " INCHES.")')
     &               KEYWRD, pctMinVolume, tmpLabel, pctMinDbh

!        =================== CASE PCTVRCST =============================
         case ('PCTVRCST')
            if (varPctCnt == MAX_KEYWORDS) then
               call errMsg(maxKeyWds)
               cycle readKeyWd                                           !Go read next keyword
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            else if (.not.isCorrectUnit(KEYWRD,int(realFields(2)))) then
               call errMsg(badUoM)
               cycle readKeyWd                                           !Go read next keyword
            end if

            varPctCnt = varPctCnt + 1
            varPctUnits(varPctCnt) = int(realFields(2))
            varPctAmt(varPctCnt) = realFields(1)

            varPctDbhLo(varPctCnt) = realFields(3)
            if (realFields(4)>0.0) varPctDbhHi(varPctCnt) =realFields(4) !Else default value, 999.0
            
            call ratesAndDurations(charFields(4), MAX_RATES, rateCnt,
     &                             varPctRate(varPctCnt,1:MAX_RATES),
     &                             varPctDur(varPctCnt,1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/1x, a8, "   PCT COST: $", f5.0,
     &             " PER ", a, ", FOR TREES >= ", f5.1, " AND < ", f5.1,
     &             " INCHES DBH.")') KEYWRD, varPctAmt(varPctCnt),
     &                    trim(UNITS_LABEL(varPctUnits(varPctCnt))),
     &                    varPctDbhLo(varPctCnt), varPctDbhHi(varPctCnt)

               do i = 1, rateCnt
                  write (JOSTND,'(T13, "APPRECIATION RATE: ", F6.1,
     &                                        "% FOR ", i4, " YEARS.")')
     &                                        varPctRate(varPctCnt,i),
     &                                        varPctDur(varPctCnt,i)
               end do
            end if

!        =================== CASE PLANTCST =============================
         case ('PLANTCST')
            if (plntCostCnt == MAX_PLANT_COSTS) then                     !Following logic based on max of two correct PLANTCST keywords
               call errMsg(maxKeyWds)
               cycle readKeyWd                                           !Go read next keyword
            else if (plntCostCnt > 0) then                               !Plant cost keyword previously read
               if (plntCostUnits(1) == int(realFields(2))) then          !Identical cost units previously specified
                  write (JOSTND,'(1x, a8, "   PLANTING COSTS UNITS ",i2,
     &                      " PREVIOUSLY SET, RECORD # IGNORED: ", i4)')
     &                      KEYWRD, plntCostUnits(1), IRECNT
                  call RCDSET(1,.TRUE.)                                  !.TRUE. causes RCDSET to return, 
                  cycle readKeyWd                                        !Go read next keyword
               endif
            else if (realFields(1) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword

            else if (.not.isCorrectUnit(KEYWRD,int(realFields(2)))) then
               call errMsg(badUoM)
               cycle readKeyWd                                           !Go read next keyword
            end if

            plntCostCnt = plntCostCnt + 1
            plntCostAmt(plntCostCnt) = realFields(1)
            plntCostUnits(plntCostCnt) = int(realFields(2))
            call ratesAndDurations(charFields(3), MAX_RATES, rateCnt,
     &                            plntCostRate(plntCostCnt,1:MAX_RATES),
     &                            plntCostDur(plntCostCnt,1:MAX_RATES))

            if (LKECHO) then
               write (JOSTND,'(/,1x,a8,"   COST: $", f5.0, " PER ", a)')
     &                     KEYWRD, plntCostAmt(plntCostCnt),
     &                     trim(UNITS_LABEL(plntCostUnits(plntCostCnt)))

               do i = 1, rateCnt
                  write (JOSTND,'(T13,"APPRECIATION RATE: ", F6.1,
     &                                     "% FOR ",i4," YEARS.")')
     &                                     plntCostRate(plntCostCnt, i),
     &                                     plntCostDur(plntCostCnt, i)
              end do
            end if

!        =================== CASE PRETEND ==============================
         case('PRETEND')
            if (realFields(1) < 0.0) then
               call errMsg(badCycle)
               cycle readKeyWd                                           !Go read next keyword
            end if

            if (parmsField > 0) then                                     !Keyword set by Event Monitor with PARMS
               call loadParms(PRETEND_ACTIVITY, 2)                       !2= field wherein word PARMS must start
               cycle readKeyWd                                           !Go read next keyword
            endif

            if (.not.isNotBlank(2)) realFields(2) = 999.0                !Default duration for PRETEND keyword

!          Register PRETEND keyword on the Event Monitor
            call addEvent(PRETEND_ACTIVITY, 1, 1, 2)                     !Sets value of LMODE & errCode, error handled in addEvent
            if (errCode > 0) cycle readKeyWd                             !Go read next keyword

            if (LMODE .and. LKECHO) then
               write (JOSTND, '(/,1X, A8, "   PRETEND MODE DELAYED", i4,
     &                 " YEARS, CONTINUING FOR", i4, " YEARS")') KEYWRD,
     &                 int(realFields(1)), int(realFields(2))
            else if (LKECHO) then
               write (JOSTND, '(/,1X, A8, "   PRETEND MODE STARTS YEAR",
     &              "/CYCLE: ", i4, ", CONTINUING FOR", i4, " YEARS ")')
     &              KEYWRD, int(realFields(1)), int(realFields(2))
            end if

!        =================== CASE SPECCST ==============================
         case('SPECCST')
            if (realFields(1) < 0.0) then
               call errMsg(badCycle)
               cycle readKeyWd                                           !Go read next keyword
            end if

            if (parmsField > 0) then                                     !Keyword set by Event Monitor with PARMS
              call loadParms(SPEC_COST_ACTIVITY, 3)                      !3= field wherein word PARMS must start
              cycle readKeyWd                                            !Go read next keyword
            endif

            if (realFields(3) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

!          Register SPECCST keyword on the Event Monitor
            call addEvent(SPEC_COST_ACTIVITY, 0, 1, 3)                   !Sets value of LMODE & errCode, error handled in addEvent
            if (errCode > 0) cycle readKeyWd                             !Go read next keyword
cc            specCstCnt = specCstCnt + 1                                  !No known reason for this

            if (LKECHO)
     &         write (JOSTND,'(/,1x,a8, 3X, "$", f5.0, " SPECIAL COST ",
     &           "INCURRED FOR: ", a, ", IN YEAR/CYCLE: ", i4)') KEYWRD,
     &           realFields(3), trim(adjustl(charFields(2))),
     &           int(realFields(1))

!        =================== CASE SPECRVN ==============================
         case('SPECRVN')
            if (realFields(1) < 0.0) then
               call errMsg(badCycle)
               cycle readKeyWd                                           !Go read next keyword
            end if

            if (parmsField > 0) then                                     !Keyword set by Event Monitor with PARMS
              call loadParms(SPEC_REV_ACTIVITY, 3)                       !3= field wherein word PARMS must start
              cycle readKeyWd                                            !Go read next keyword
            endif

            if (realFields(3) <= 0.0) then
               call errMsg(badValue)
               cycle readKeyWd                                           !Go read next keyword
            end if

!          Register SPECRVN keyword on the Event Monitor
            call addEvent(SPEC_REV_ACTIVITY, 0, 1, 3)                    !Sets value of LMODE & errCode, error handled in addEvent
            if (errCode > 0) cycle readKeyWd                             !Go read next keyword
cc            specRvnCnt = specRvnCnt + 1                                  !No known reason for this

            if (LKECHO)
     &          write (JOSTND,'(/, 1x, a8, 3X, "$", f5.0, " SPECIAL ",
     &            "REVENUE ACCRUED FOR: ", a, ", IN YEAR/CYCLE: ", i4)')
     &            KEYWRD, realFields(3), trim(adjustl(charFields(2))),
     &            int(realFields(1))

!        ======================= CASE STRTECON=========================
         case ("STRTECON")
            if (realFields(1) < 0.0) then
               call errMsg(badCycle)
               cycle readKeyWd                                           !Go read next keyword
            end if

!          Register STRTECON keyword with the Event Monitor
            if (realFields(2) <= 0.0 .or. realFields(3) > 0.0) 
     &                                               realFields(4) = 0.0 !Set SEV compute to false if SEV is input or discount rate = 0 (SEV infinite)
            call addEvent(ECON_START_YEAR, 1, 3, 2)                      !Sets value of LMODE & errCode, error handled in addEvent
            if (errCode > 0) cycle readKeyWd                             !Go read next keyword
            
            econStartYear = 9999                                         !Used to indicate valid STRTECON keyword has been submitted

            if (realFields(3) <= 0.0) charFields(3) = " "                !Replace potential zero w/ blank

            if (LMODE .and. LKECHO) then
               write (JOSTND, '(/,1x, a8, "   ECON START YEAR DELAYED", 
     &          i4, " YEARS, DISCOUNT RATE:", f5.1, "%, Known SEV: $ ",
     &          a, ", SEV will be computed: ", a)') 
     &         KEYWRD, int(realFields(1)), realFields(2), 
     &         trim(adjustl(charFields(3))), 
     &         trim(BOOLEAN(int(realFields(4)))) 
           else if (LKECHO) then
               write (JOSTND, '(/, 1x, a8, "   ECON START YEAR/CYCLE: ",
     &         i4, ", DISCOUNT RATE:", f5.1, "%, Known SEV: $ ", a, 
     &         ", SEV will be computed: ", a)') 
     &         KEYWRD, int(realFields(1)), realFields(2), 
     &         trim(adjustl(charFields(3))),
     &         trim(BOOLEAN(int(realFields(4)))) 
            end if

!        ======================= CASE DEFAULT ==========================
         case default !keyword not found
            call ERRGRO(.TRUE.,1)                                        !.TRUE. causes ERRGRO to return, 1=invalid keyword was specified
            call RCDSET(1,.TRUE.)                                        !.TRUE. causes RCDSET to return, 
         end select
      end do readKeyWd
      return

!    Given an activityId, return the keyword name
      ENTRY ECKEY(activityId, keyword)                                  !activityId keyword index
         keyword = KEYWORD_TABLE(activityId)
      return

      contains


!    Checks that correct units-of-measure is found in the specified column
      logical function isCorrectUnit(keyword, units)
         implicit none
         character (len=8), intent(in) :: keyword
         integer, intent(in) :: units
         isCorrectUnit = .FALSE.
         select case (keyword)
         case ('PLANTCST')
           if (units==PER_ACRE .or. units==TPA_1000) then
              isCorrectUnit = .TRUE.
           end if
         case ('HRVVRCST', 'PCTVRCST')
            if (units==TPA .or. units==BF_1000 .or. units==FT3_100) then
               isCorrectUnit = .TRUE.
            end if
         case ('HRVRVN')
            if (units==TPA .or. units==BF_1000 .or. units==BF_1000_LOG
     &                 .or. units==FT3_100 .or. units==FT3_100_LOG) then
               isCorrectUnit = .TRUE.
            end if
         case ('PCTSPEC')
              if (units==BF_1000 .or. units==FT3_100) then
                 isCorrectUnit = .TRUE.
              end if
         case default
            isCorrectUnit = .FALSE.
         end select
         return
      end function isCorrectUnit

!    Determines if a supplemental record indicator is in the given field
      logical function isSupplemental(field)
         implicit none
         character (len=10), intent(in) :: field
         character :: test
         isSupplemental = .FALSE.
         test = trim(adjustl(field))
         if (test == '&') then
            isSupplemental = .TRUE.
         end if
         return
      end function isSupplemental


!    Reads appreciation/depreciation rates and durations from a supplemental record.
      subroutine ratesAndDurations(supplemental, numRates, rateCnt, 
     &                                                 rates, durations)
         implicit none
         character (len=80)                        :: record
         character (len=5)                         :: fieldC
         character (len=10), intent(in)            :: supplemental
         integer                                   :: i, ios, j
         integer, intent(in)                       :: numRates
         integer, intent(out)                      :: rateCnt
         integer, dimension(numRates), intent(out) :: durations
         real,    dimension(numRates), intent(out) :: rates
         real,    dimension(2*numRates)            :: fieldR

         rateCnt=0; rates=0.0; durations=0;

        if (.not. isSupplemental(supplemental)) return                   !No rate change records to be read

         read (IREAD,'(a80)',iostat=ios) record
         if (ios < 0) then                                               !EOF, should not happen before an END keyword
            write (JOSTND,'(/,1x,8x,"   ERROR READING SUPPLEMENTAL ",
     &                                     "RATES & DURATIONS RECORD")')
            call ERRGRO(.FALSE., 2)                                      !.FALSE. causes ERRGRO not to return, 1 = invalid keyword
            CALL getfvsRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN         
         end if

!       Read value rates and durations allowing for free-field format w/in 5-character fields
         j=1
         do i = 1, 2*numRates
            fieldC = adjustr(record(j:j+4))
            j      = j + 5
            read (fieldC,'(f5.0)',iostat=ios) fieldR(i)
            if (ios>0 .or. fieldR(i)<-100.0) then !Read error
               write (JOSTND,'(/,1x,8x,"   ILLEGAL ENTRY IN VALUE RATE",
     &                " CHANGE, RECORD:", i4, ", ALL RATE CHANGES ARE ",
     &                "IGNORED.")') IRECNT
               write (JOSTND,'(1x, 8x, "PARAMETERS ARE: ", a80)') record
               hasError = .TRUE.
               return
            end if
         end do

!       Set rates and durations on input arrays
         do i = 1, 2*numRates, 2                                         !i=index for rates
            j = i + 1                                                    !j= index for durations
            If (fieldR(j) >= 1.0) then                                   !Ignore fields with zero durations
               rateCnt            = rateCnt + 1
               rates(rateCnt)     = fieldR(i)
               durations(ratecnt) = int(fieldR(j))
            end if
         end do                                                          !End rate duration loop
         return
      end subroutine ratesAndDurations


!    Loads keyword parameters submitted via PARMS on the Event Monitor
      subroutine loadParms(activityCode, field)
         integer, intent(in) :: activityCode, field
         if (parmsField /= field) then                                   !Error, word PARMS does not begin in correct field
            call KEYDMP(JOSTND, IRECNT, KEYWRD, realFields, charFields)
            call ERRGRO(.TRUE.,25)                                       !.TRUE. causes ERRGRO to return, 25=incorrect use/placement of parms
         else
            call OPNEWC(errCode, JOSTND, IREAD, int(realFields(1)),
     &                  activityCode, KEYWRD, charFields, parmsField,
     &                  IRECNT, ICYC)
            CALL getfvsRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         end if
         return
      end subroutine loadPArms


!    Registers a keyword on the Event Monitor
      subroutine addEvent(activityCode, minDate, NPRMS, prmsIndex)
         integer, intent(in) :: activityCode, minDate, NPRMS, prmsIndex
         integer activityDate, waitTime

         call OPMODE(LMODE)                                              !OPMODE is entry in OPNEW.f, LMODE=.TRUE. if keyword set by Event Monitor
         if (LMODE) then                                                 !Submitted via IF_THEN_ELSE
            waitTime = int(realFields(1))
            call OPNEW(errCode, waitTime, activityCode, NPRMS,
     &                                            realFields(prmsIndex)) !Returns errCode
         else                                                            !Submitted normally, but check for minimum value for start year
            activityDate = max(minDate, int(realFields(1)))
            call OPNEW(errCode, activityDate, activityCode, NPRMS,
     &                                            realFields(prmsIndex)) !Returns errCode
         end if

         if (errCode > 0)  then
            write (JOSTND,'(/, a12, 1x, a8, "   KEYWORD FAILED TO ", 
     &             "REGISTER ON EVENT MONITOR, RECORD: ", i4)') 
     &             warn, KEYWRD, IRECNT
            call RCDSET(1,.TRUE.)                                        !.TRUE. causes RCDSET to return, 
         endif
         return
      end subroutine addEvent


!    Provides for repeatable error messages
      subroutine errMsg(msg)
         integer, intent(in) :: msg
         select case (msg)
         case (badCycle)
            write (JOSTND,'(/, a12, 1x, a8, "   KEYWORD IGNORED, ", 
     &             "CYCLE/YEAR NEGATIVE OR MISSING, RECORD: ",I4)') 
     &             warn, KEYWRD, IRECNT
         case (badUoM)
            write (JOSTND,'(/, a12, 1x, a8, "   KEYWORD IGNORED, ",
     &             "UNIT-OF-MEASURE INCORRECTLY SPECIFIED, RECORD: ", 
     &             i4)') warn, KEYWRD, IRECNT
         case (badValue)
            write (JOSTND,'(/, a12, 1x, a8, "   KEYWORD IGNORED, NO ",
     &             "OR NEGATIVE COST/PRICE/VALUE ENTERED, RECORD:", 
     &             i4)') warn, KEYWRD, IRECNT
         case (maxKeyWds)
            write (JOSTND,'(/,1x,a12, "# ", a8, " KEYWORDS ",
     &             "ENTERED EXCEEDS MAXIMUM, RECORD", i4, " IGNORED.")')
     &             warn, KEYWRD, IRECNT
         case (revMaxKeyWds)
            write(JOSTND,'(/, a12, 1x, 1x, a8,"   KEYWORD IGNORED, ", 
     &           "MAX # KEYWORDS EXCEEDED FOR THIS SPECIES: ", a, 
     &           ", UNITS-OF-MEASURE: ", a, ", & DIAMETER CLASS:", f5.1,
*     &           ", RECORD:", i4)')  warn, KEYWRD, NSP(i,1)(1:2),        !NSP(i,1)(1:2) is character species code not including tree value class
     &           ", RECORD:", i4)')  warn, KEYWRD, 
     &            trim(adjustl(charFields(4))),trim(UNITS_LABEL(units)),
     &            realFields(3), IRECNT
         case (revDuplicate)
            write(JOSTND,'(/, a12, 1x, 1x, a8, "   KEYWORD IGNORED, " 
     &            "VALUES PREVIOUSLY SET FOR THIS SPECIES: ", a, 
     &            ", UNITS-OF-MEASURE: ", a, ", & DIAMETER CLASS:",
*     &            f5.1, ", RECORD:", i4)') warn, KEYWRD, NSP(i,1)(1:2),  !NSP(i,1)(1:2) is character species code not including tree value class
     &            f5.1, ", RECORD:", i4)') warn, KEYWRD, 
     &            trim(adjustl(charFields(4))),trim(UNITS_LABEL(units)), 
     &            realFields(3), IRECNT
         case (lbsDupSp)
            write(JOSTND,'(/, a12, 1x, a8, "   KEYWORD IGNORED, ", 
     &            "VALUES PREVIOUSLY SET FOR THIS SPECIES: ", a, 
     &            ", RECORD:", I4)') warn, KEYWRD, 
     &            trim(adjustl(charFields(2))), IRECNT


         end select
         call RCDSET(1,.TRUE.)                                           !.TRUE. causes RCDSET to return, 
         return
      end subroutine errMsg


      end subroutine ECIN
