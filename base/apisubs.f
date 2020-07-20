C----------
C BASE $Id$
C----------
c     This is a collection of routines that provide an interface to 
c     the shared library version of FVS. Other routines that exist
c     inside FVS may also be called.

c     Created in late 2011 by Nick Crookston, RMRS-Moscow
c     In 2019 additional interface routines were added to allow for C functions
c     that are part of the API to call fortran functions with character strings. 
c     In 2019 additional interface routines were added to allow for C functions
c     that are part of the API to call fortran functions with character strings.
c     The fortran routines that can be called by C have an upper case C added to 
c     the routine name. See apisubs.c for the C functions that are used to call 
c     the fortran routines. 

      subroutine fvsDimSizes(ntrees,ncycles,nplots,maxtrees,maxspecies,
     -                       maxplots,maxcycles)
      implicit none
      include "PRGPRM.F77"
      include "CONTRL.F77"
      include "PLOT.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSDIMSIZES'::FVSDIMSIZES
!DEC$ ATTRIBUTES REFERENCE :: NTREES, NCYCLES, NPLOTS, MAXTREES
!DEC$ ATTRIBUTES REFERENCE :: MAXSPECIES, MAXPLOTS, MAXCYCLES

      integer :: ntrees,ncycles,nplots,maxtrees,maxspecies,maxplots,
     -           maxcycles
      ntrees    = ITRN
      ncycles   = NCYC
      nplots    = IPTINV
      maxtrees  = MAXTRE
      maxspecies= MAXSP 
      maxplots  = MAXPLT
      maxcycles = MAXCYC
      return
      end

      subroutine fvsSummary(summary,icycle,ncycles,maxrow,maxcol,
     -                      rtnCode)
      implicit none

      include "PRGPRM.F77"
      include "CONTRL.F77"
      include "OUTCOM.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSSUMMARY'::FVSSUMMARY
!DEC$ ATTRIBUTES REFERENCE :: SUMMARY, ICYCLE, NCYCLES, MAXROW
!DEC$ ATTRIBUTES REFERENCE :: MAXCOL, RTNCODE
      
      integer :: summary(20),icycle,ncycles,maxrow,maxcol,rtnCode
      
      maxrow = maxcy1
      maxcol = 20
      ncycles = ncyc
      if (icycle <= 0 .or. icycle > ncyc+1) then
        rtnCode = 1
      else
        summary = iosum(:maxcol,icycle)
        rtnCode = 0
      endif
      return
      end    

      subroutine fvsTreeAttrC(namei,nch,actioni,ntrees,attr,rtnCode)
     -           bind(c, name="fvsTreeAttrC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsTreeAttr where namei and actioni are 
c     C-bindings of their correspoinding arguments.

      integer(c_int), bind(c) :: nch,rtnCode,ntrees
      real(c_double), dimension(ntrees), bind(c) :: attr
      character(c_char), dimension(10), bind(c) :: namei,actioni
      character name*10,action*4
      integer i
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      action="get"
      if (actioni(1) == "s") action="set"
      call fvsTreeAttr(name,nch,action,ntrees,attr,rtnCode)
      return
      end

      subroutine fvsTreeAttr(name,nch,action,ntrees,attr,rtnCode)
      implicit none

c     set and/or gets the named tree attributes
c     name    = char string of the variable name, (case sensitive)
c     nch     = the number of characters in "name" 
c     action  = char string that is one of "set" or "get" (case sensitive)
c     ntrees  = the number of trees, length of data
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 1= "name" not found,
c               2= ntrees is greater than maxtrees, not all data transfered
c               3= there were more/fewer than ntrees.
c               4= the length of the "name" string was too big or small
c
      include "PRGPRM.F77"
      include "FMPARM.F77"
      include "FMCOM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"
      include "VARCOM.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSTREEATTR'::FVSTREEATTR
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ACTION, NTREES, ATTR, RTNCODE

      integer :: nch,rtnCode,ntrees
      real(kind=8)      :: attr(ntrees)
      character(len=10) :: name
      character(len=4)  :: action

      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif

      name=name(1:nch)
      action=action(1:3)

      rtnCode = 0
      if (ntrees > MAXTRE) then
        attr = 0
        rtnCode = 2
        return
      endif
      if (ntrees /= itrn) then
        attr = 0
        rtnCode = 3
        return
      endif
      select case(name)
      case ("tpa")
        if (action=="get") attr = prob(:itrn)
        if (action=="set") prob(:itrn) = real(attr,4)
      case ("mort")
        if (action=="get") attr = wk2(:itrn)
        if (action=="set") wk2(:itrn) = real(attr,4)
      case ("dbh")
        if (action=="get") attr = dbh(:itrn)
        if (action=="set") dbh(:itrn) = real(attr,4)
      case ("dg")
        if (action=="get") attr = dg(:itrn)
        if (action=="set") dg(:itrn) = real(attr,4)
      case ("ht")
        if (action=="get") attr = ht(:itrn)
        if (action=="set") ht(:itrn) = real(attr,4)
      case ("htg")
        if (action=="get") attr = htg(:itrn)
        if (action=="set") htg(:itrn) = real(attr,4)
      case ("crwdth")
        if (action=="get") attr = crwdth(:itrn)
        if (action=="set") crwdth(:itrn) = real(attr,4)
      case ("cratio")
        if (action=="get") attr = icr(:itrn)
        if (action=="set") icr(:itrn) = int(attr,4)
      case ("species")
        if (action=="get") attr = isp(:itrn)
        if (action=="set") isp(:itrn) = int(attr,4)
      case ("age")
        if (action=="get") attr = abirth(:itrn)
        if (action=="set") abirth(:itrn) = real(attr,4)
      case ("plot")
        if (action=="get") attr = itre(:itrn)
        if (action=="set") itre(:itrn) = int(attr,4)
      case ("tcuft")
        if (action=="get") attr = cfv(:itrn)
        if (action=="set") cfv(:itrn) = real(attr,4)
      case ("mcuft")
        if (action=="get") attr = wk1(:itrn)
        if (action=="set") wk1(:itrn) = real(attr,4)
      case ("bdft")
        if (action=="get") attr = bfv(:itrn)
        if (action=="set") bfv(:itrn) = real(attr,4)
      case ("defect")
        if (action=="get") attr = defect(:itrn)
        if (action=="set") defect(:itrn) = ifix(real(attr,4))
      case ("ptbalt")
        if (action=="get") attr = ptbalt(:itrn)
        if (action=="set") ptbalt(:itrn) = real(attr,4)
      case ("mgmtcd")
        if (action=="get") attr = imc(:itrn)
        if (action=="set") imc(:itrn) = int(attr,4)
      case ("plotsize")
        if (action=="get") attr = pltsiz(:itrn)
        if (action=="set") pltsiz(:itrn) = int(attr,4)
      case ("bapctile")
        if (action=="get") attr = pct(:itrn)
        if (action=="set") pct(:itrn) = int(attr,4)
      case ("crownwt0")
        if (action=="get") attr = crownw(:itrn,0)
        if (action=="set") crownw(:itrn,0) = real(attr,4)
      case ("crownwt1")
        if (action=="get") attr = crownw(:itrn,1)
        if (action=="set") crownw(:itrn,1) = real(attr,4)
      case ("crownwt2")
        if (action=="get") attr = crownw(:itrn,2)
        if (action=="set") crownw(:itrn,2) = real(attr,4)
      case ("crownwt3")
        if (action=="get") attr = crownw(:itrn,3)
        if (action=="set") crownw(:itrn,3) = real(attr,4)
      case ("crownwt4")
        if (action=="get") attr = crownw(:itrn,4)
        if (action=="set") crownw(:itrn,4) = real(attr,4)
      case ("crownwt5")
        if (action=="get") attr = crownw(:itrn,5)
        if (action=="set") crownw(:itrn,5) = real(attr,4)
      case ("id")
        if (action=="get") attr = idtree(:itrn)
        if (action=="set") idtree(:itrn) = int(attr,4)
      case default
        rtnCode = 1
        attr = 0
      end select

      return
      end


      subroutine fvsSpeciesAttrC(namei,nch,actioni,attr,rtnCode)
     -           bind(c, name="fvsSpeciesAttrC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsSpeciesAttr where namei and actioni are 
c     C-bindings of their correspoinding arguments.

      include "PRGPRM.F77"

      integer(c_int), bind(c) :: nch,rtnCode
      real(c_double), dimension(MAXSP), bind(c) :: attr
      character(c_char), dimension(10), bind(c) :: namei,actioni
      character name*10,action*4
      integer i
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      action="get"
      if (actioni(1) == "s") action="set"
      call fvsSpeciesAttr(name,nch,action,attr,rtnCode)
      return
      end

      subroutine fvsSpeciesAttr(name,nch,action,attr,rtnCode)
      implicit none

c     set and/or gets the named species attributes
c     name    = char string of the variable name, (case sensitive)
c     nch     = the number of characters in "name" 
c     action  = char string that is one of "set" or "get" (case sensitive)
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 1= "name" not found,
c               4= the length of the "name" string was too big or small
c
      include "PRGPRM.F77"
      include "PLOT.F77"
      include 'VOLSTD.F77'
      include 'CONTRL.F77'

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE:: FVSSPECIESATTR
!DEC$ ATTRIBUTES ALIAS:'FVSSPECIESATTR'::FVSSPECIESATTR
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ACTION, ATTR, RTNCODE

      integer :: nch,rtnCode
      real(kind=8)      :: attr(MAXSP)
      character(len=10) :: name
      character(len=4)  :: action

      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif

      name=name(1:nch)
      action=action(1:3)

      rtnCode = 0
      select case(name)
      case ("spccf")
        if (action=="get") attr = reldsp
        if (action=="set") reldsp = real(attr,4)
      case ("spsiteindx")
        if (action=="get") attr = sitear
        if (action=="set") sitear = real(attr,4)
      case ("spsdi")
        if (action=="get") attr = sdidef
        if (action=="set") sdidef = real(attr,4)

c     volume calculation-related vectors

      case ("bfmind")
        if (action=="get") attr = bfmind
        if (action=="set") bfmind = real(attr,4)
      case ("bftopd")
        if (action=="get") attr = bftopd
        if (action=="set") bftopd = real(attr,4)
      case ("bfstmp")
        if (action=="get") attr = bfstmp
        if (action=="set") bfstmp = real(attr,4)
      case ("frmcls")
        if (action=="get") attr = frmcls
        if (action=="set") frmcls = real(attr,4)
      case ("bfmeth")
        if (action=="get") attr = methb
        if (action=="set") methb = int(attr,4)
      case ("mcmind")
        if (action=="get") attr = dbhmin
        if (action=="set") dbhmin = real(attr,4)
      case ("mctopd")
        if (action=="get") attr = topd
        if (action=="set") topd = real(attr,4)
      case ("mcstmp")
        if (action=="get") attr = stmp
        if (action=="set") stmp = real(attr,4)
      case ("mcmeth")
        if (action=="get") attr = methc
        if (action=="set") methc = int(attr,4)
      case default
        rtnCode = 1
        attr = 0
      end select

      return
      end

      subroutine fvsEvmonAttrC(namei,nch,actioni,attr,rtnCode)
     -           bind(c, name="fvsEvmonAttrC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsSpeciesAttr where namei and actioni are 
c     C-bindings of their correspoinding arguments.

      integer(c_int), bind(c) :: nch,rtnCode
      real(c_double), bind(c) :: attr
      character(c_char), dimension(9), bind(c) :: namei,actioni
      character name*9,action*4
      integer i
      
      if (nch == 0 .or. nch > 9) then
        rtnCode = 4
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      action="get"
      if (actioni(1) == "s") action="set"
      call fvsEvmonAttr(name,nch,action,attr,rtnCode)
      return
      end


      subroutine fvsEvmonAttr(name,nch,action,attr,rtnCode)
      implicit none

c     set and/or gets the named tree attributes
c     name    = char string of the variable name, (case sensitive)
c     nch     = the number of characters in "name" 
c     action  = char string that is one of "set" or "get" (case sensitive)
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 1=action is "get" and variable
c               is known to be undefined. 2= "name" not found, 
c
      include "PRGPRM.F77"
      include "FMPARM.F77"
      include "FMCOM.F77"
      include "ARRAYS.F77"
      include "OPCOM.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSEVMONATTR'::FVSEVMONATTR
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ACTION, ATTR, RTNCODE

      integer :: nch,rtncode,iv,i
      real(kind=8)      :: attr
      character(len=9)  :: name
      character(len=4)  :: action
      character(len=8)  :: upname
     
      upname = ' '
      upname = name(1:nch)
      
      action=action(1:3)
      if (action=="get") attr = 0

      rtncode = 0
      select case(upname)
      case ("year")
        iv=101
      case ("age")
        iv=102
      case ("btpa")
        iv=103
      case ("btcuft")
        iv=104
      case ("bmcuft")
        iv=105
      case ("bbdft")
        iv=106
      case ("bba")
        iv=107
      case ("bccf")
        iv=108
      case ("btopht")
        iv=109
      case ("badbh")
        iv=110
      case ("yes")
        iv=111
      case ("no")
        iv=112
      case ("cycle")
        iv=113
      case ("numtrees")
        iv=114
      case ("bsdimax")
        iv=115
      case ("bsdi")
        iv=116
      case ("brden")
        iv=117
      case ("brden2")
        iv=118
      case ("habtype")
        iv=126
      case ("slope")
        iv=127
      case ("aspect")
        iv=128
      case ("elev")
        iv=129
      case ("sampwt")
        iv=130
      case ("invyear")
        iv=131
      case ("cendyear")
        iv=132
      case ("evphase")
        iv=133
      case ("smr")
        iv=134
      case ("site")
        iv=135
      case ("cut")
        iv=136
      case ("lat")
        iv=137
      case ("long")
        iv=138
      case ("state")
        iv=139
      case ("county")
        iv=140
      case ("fortyp")
        iv=141
      case ("sizcls")
        iv=142
      case ("stkcls")
        iv=143
      case ("propstk")
        iv=144
      case ("mai")
        iv=145
      case ("agecmp")
        iv=146
      case ("bdbhwtba")
        iv=147
      case ("silvahft")
        iv=148
      case ("fisherin")
        iv=149
      case ("bsdi2")
        iv=150
      case ("atpa")
        iv=201
      case ("atcuft")
        iv=202
      case ("amcuft")
        iv=203
      case ("abdft")
        iv=204
      case ("aba")
        iv=205
      case ("accf")
        iv=206
      case ("atopht")
        iv=207
      case ("aadbh")
        iv=208
      case ("rtpa")
        iv=209
      case ("rtcuft")
        iv=210
      case ("rmcuft")
        iv=211
      case ("rbdft")
        iv=212
      case ("asdimax")
        iv=213
      case ("asdi")
        iv=214
      case ("arden")
        iv=215
      case ("adbhwtba")
        iv=216
      case ("arden2")
        iv=217
      case ("asdi2")
        iv=218
      case ("acc")
        iv=301
      case ("mort")
        iv=302
      case ("pai")
        iv=303
      case ("dtpa")
        iv=305
      case ("dtpa%")
        iv=306
      case ("dba")
        iv=307
      case ("dba%")
        iv=308
      case ("dccf")
        iv=309
      case ("dccf%")
        iv=310
      case ("tm%stnd")
        iv=401
      case ("tm%df")
        iv=402
      case ("tm%gf")
        iv=403
      case ("mpbtpak")
        iv=404
      case ("bw%stnd")
        iv=405
      case ("mpbprob")
        iv=406
      case ("bsclass")
        iv=416
      case ("asclass")
        iv=417
      case ("bstrdbh")
        iv=418
      case ("astrdbh")
        iv=419
      case ("fire")
        iv=420
      case ("minsoil")
        iv=421
      case ("crownidx")
        iv=422
      case ("fireyear")
        iv=423
      case ("bcancov")
        iv=424
      case ("acancov")
        iv=425
      case ("crbaseht")
        iv=426
      case ("torchidx")
        iv=427
      case ("crbulkdn")
        iv=428
      case ("disccost")
        iv=430
      case ("discrevn")
        iv=431
      case ("forstval")
        iv=432
      case ("harvcost")
        iv=433
      case ("harvrevn")
        iv=434
      case ("irr")
        iv=435
      case ("pctcost")
        iv=436
      case ("pnv")
        iv=437
      case ("rprodval")
        iv=438
      case ("sev")
        iv=439
      case ("bmaxhs")
        iv=440
      case ("amaxhs")
        iv=441
      case ("bminhs")
        iv=442
      case ("aminhs")
        iv=443
      case ("bnumss")
        iv=444
      case ("anumss")
        iv=445
      case ("discrate")
        iv=446
      case ("undiscst")
        iv=447
      case ("undisrvn")
        iv=448
      case ("eccuft")
        iv=449
      case ("ecbdft")
        iv=450
      case ("shrubwt")
        if (action=="get") attr = flive(1)
        if (action=="set") flive(1) = real(attr,4)
        return        
      case ("herbwt")
        if (action=="get") attr = flive(2)
        if (action=="set") flive(2) = real(attr,4)                
        return
      case default
        iv=0
      end select
      
      if (iv == 0) then
        do i=1,nch
          call upcase(upname(i:i))
        enddo
        do i=1,ITST5
          if (ctstv5(i).eq.upname) then
            if (action=="get") then
              if (LTSTV5(i)) then
                attr = tstv5(i)
              else
                rtncode = 1
              endif
            elseif (action=="set") then 
              tstv5(i) = real(attr,4)
              LTSTV5(i) = .TRUE.
            else
              rtncode = 1
            endif
            return
          endif
        enddo
        if (action=="set" .and. ITST5.lt.MXTST5) then
          ITST5=ITST5+1
          LTSTV5(ITST5) = .TRUE.
          tstv5(ITST5) = real(attr,4)
          ctstv5(ITST5) = upname
        else
          rtnCode=1
        endif
        return
      endif
      
      i = mod(iv,100)
      iv = iv/100

      select case (iv)
      case (1)
        if (action=="get") attr = tstv1(i)
        if (action=="set") tstv1(i) = real(attr,4)
      case (2)
        if (action=="get") attr = tstv2(i)
        if (action=="set") tstv2(i) = real(attr,4)
      case (3)
        if (action=="get") attr = tstv3(i)
        if (action=="set") tstv3(i) = real(attr,4)
      case (4)
        if (action=="get") then
          if (ltstv4(i)) then
            attr = tstv4(i)
          else
            rtncode = 1
          endif
        endif
        if (action=="set") then
          tstv4(i) = real(attr,4)
          ltstv4(i)=.true.
        endif
      end select
      return
      end
      
      subroutine fvsAddTrees(in_dbh,in_species,in_ht,in_cratio,
     -                       in_plot,in_tpa,ntrees,rtnCode)
      implicit none

c     rtnCode = 0 when all is OK
c               1 when there is no room for the ntrees
c                 or when ntrees is zero
      
      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"
      include "OUTCOM.F77"
      include "PLOT.F77"
      include "VARCOM.F77"
      include "ESTREE.F77"
      include "STDSTK.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSADDTREES'::FVSADDTREES
!DEC$ ATTRIBUTES REFERENCE :: IN_DBH, IN_SPECIES, IN_HT, IN_CRATIO
!DEC$ ATTRIBUTES REFERENCE :: IN_PLOT, IN_TPA, NTREES, RTNCODE
      
      real(kind=8) :: in_dbh(ntrees),in_species(ntrees),
     -    in_ht(ntrees),in_cratio(ntrees),in_plot(ntrees),
     -    in_tpa(ntrees)
      real :: cw,crdum
      integer :: ntrees,rtnCode,i
      
      rtnCode = 0
      if (ntrees == 0 .or. ntrees+itrn > MAXTRE) then
        rtnCode = 1
        return
      endif

      do i=1,ntrees
        itrn=itrn+1
        dbh(itrn) = real (in_dbh    (i),4)
        isp(itrn) = int  (in_species(i),4)
        ht (itrn) = real (in_ht     (i),4)
        icr(itrn) = int  (in_cratio (i),4)
        itre(itrn)= int  (in_plot   (i),4)
        prob(itrn)= real (in_tpa    (i),4)
        imc(itrn)=2
        cfv(itrn)=0.0
        itrunc(itrn)=0
        normht(itrn)=0
        crdum = icr(itrn)
        call cwcalc(isp(itrn),prob(itrn),dbh(itrn),ht(itrn),crdum,
     &              icr(itrn),cw,0,jostnd)
        crwdth(itrn)=cw
c       
        dg(itrn)=0.0
        htg(itrn)=0.0
        pct(itrn)=0.0
        oldpct(itrn)=0.0
        wk1(itrn)=0.0
        wk2(itrn)=0.
        wk4(itrn)=0.
        bfv(itrn)=0.0
        iestat(itrn)=0
        ptbalt(itrn)=0.
        idtree(itrn)=10000000+icyc*10000+itrn
        call misputz(itrn,0)
c       
        abirth(itrn)=5
        defect(itrn)=0
        ispecl(itrn)=0
        oldrn(itrn)=0.
        ptocfv(itrn)=0.
        pmrcfv(itrn)=0.
        pmrbfv(itrn)=0.
        ncfdef(itrn)=0
        nbfdef(itrn)=0
        pdbh(itrn)=0.
        pht(itrn)=0.
        zrand(itrn)=-999.
      enddo

c     set the value of irec1 to point to the last location in the
c     treelist. call spesrt to reestablish the species order sort.

      irec1=itrn
      call spesrt

c     reestablish the diameter sort (include the new trees).

      call rdpsrt (itrn,dbh,ind,.true.)

c     set ifst=1 so that the sample tree records will be repicked
c     next time dist is called.

      ifst=1
      return
      end
      
      subroutine fvsSpeciesCode(fvs_code,fia_code,plant_code,
     -                          indx,nchfvs,nchfia,nchplant,rtnCode)
      implicit none

c     rtnCode = 0 when all is OK
c               1 when index is out of bounds
c     indx    = species index
     
      include "PRGPRM.F77"
      include "PLOT.F77"

!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'FVSSPECIESCODE'::FVSSPECIESCODE
!DEC$ ATTRIBUTES C,DECORATE :: FVSSPECIESCODE
!DEC$ ATTRIBUTES REFERENCE :: FVS_CODE, FIA_CODE, PLANT_CODE, INDX
!DEC$ ATTRIBUTES REFERENCE :: NCHFVS, NCHFIA, NCHPLANT, RTNCODE

      integer :: indx,nchfvs,nchfia,nchplant,rtnCode
      character(len=4) :: fvs_code
      character(len=4) :: fia_code
      character(len=6) :: plant_code
      
      if (indx == 0 .or. indx > MAXSP) then
        nchfvs  = 0
        nchfia  = 0
        nchplant= 0
        rtnCode = 1
      else
        fvs_code   = JSP   (indx)
        fia_code   = FIAJSP(indx)
        plant_code = PLNJSP(indx)
        nchfvs  = len_trim(JSP   (indx))
        nchfia  = len_trim(FIAJSP(indx))
        nchplant= len_trim(PLNJSP(indx))
        rtnCode = 0
      endif
      return
      end

      subroutine fvsSpeciesCodeC(fvs_code,fia_code,plant_code,indx) 
     -           bind(c, name="fvsSpeciesCodeC") 
      use iso_c_binding
      implicit none
                  
c     rtnCode = 0 when all is OK                                                            
c               1 when index is out of bounds
c     indx    = species index
     
      include "PRGPRM.F77"
      include "PLOT.F77"
                                                                                          
      integer(c_int), bind(c) :: indx
      integer i,nch
      character(c_char), dimension(5), bind(c) :: fvs_code
      character(c_char), dimension(5), bind(c) :: fia_code                                              
      character(c_char), dimension(7), bind(c) :: plant_code
      if (indx == 0 .or. indx > MAXSP) then
        fvs_code(1)  =char(0)
        fia_code(1)  =char(0)                             
        plant_code(1)=char(0)
      else
        nch=len_trim(JSP(indx))
        do i=1,nch
          fvs_code(i) = JSP(indx)(i:i)
        enddo
        fvs_code(nch+1) = char(0)
        nch=len_trim(FIAJSP(indx))                     
        do i=1,nch
          fia_code(i) = FIAJSP(indx)(i:i)
        enddo
        fia_code(nch+1) = char(0)                             
        nch=len_trim(PLNJSP(indx))
        do i=1,nch
          plant_code(i) = PLNJSP(indx)(i:i)
        enddo
        plant_code(nch+1) = char(0)
      endif
      return
      end
      
      subroutine fvsCutTrees(pToCut,ntrees,rtnCode)

      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSCUTTREES'::FVSCUTTREES
!DEC$ ATTRIBUTES REFERENCE :: PTOCUT, NTREES, RTNCODE

      integer :: ntrees,rtnCode
      double precision :: pToCut(ntrees)
      pToCut = 0.
      if (ntrees <= 0 .or. ntrees > maxtre) then
        rtnCode = 1
        return
      endif
      
      print *,"Not yet implemented"
      rtnCode = 1
      return
      end
      
      subroutine fvsStandIDC(sID,sCN,mID,mCase)
     -           bind(c, name="fvsStandIDC") 
      use iso_c_binding
      implicit none
                       
      include "PRGPRM.F77"
      include "PLOT.F77"
      include "DBSCOM.F77"
                                                                                          
      integer i,ncsID,ncCN,ncmID,ncCase
      character(c_char), dimension(len(NPLT  )+1), bind(c) :: sID
      character(c_char), dimension(len(DBCN  )+1), bind(c) :: sCN
      character(c_char), dimension(len(MGMID )+1), bind(c) :: mID
      character(c_char), dimension(len(CASEID)+1), bind(c) :: mCase

      ncsID  = len_trim(NPLT)
      ncCN   = len_trim(DBCN)
      ncmID  = len_trim(MGMID)
      ncCase = len_trim(CASEID)
      do i=1,ncsID
        sID(i)=NPLT(i:i)
      enddo
      sID(ncsID+1)=char(0)
      do i=1,ncCN
        sCN(i)=DBCN(i:i)
      enddo
      sCN(ncCN+1)=char(0)
      do i=1,ncmID
        mID(i)=MGMID(i:i)
      enddo
      mID(ncmID+1)=char(0)
      do i=1,ncCase
        mCase(i) = CASEID(i:i)
      enddo
      mCase(ncCase+1)=char(0)
      return
      end

      subroutine fvsStandID(sID,sCN,mID,ncsID,ncCN,ncmID)

      include "PRGPRM.F77"
      include "PLOT.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSSTANDID'::FVSSTANDID
!DEC$ ATTRIBUTES REFERENCE :: SID, SCN, MID, NCSID, NCCN, NCMID

      integer :: ncsID,ncCN,ncmID
      
      character(len=len(NPLT))  sID
      character(len=len(DBCN))  sCN
      character(len=len(MGMID)) mID

      sID = trim(NPLT)
      sCN = trim(DBCN)
      mID = trim(MGMID)
      ncsID = len_trim(NPLT)
      ncCN  = len_trim(DBCN)
      ncmID = len_trim(MGMID)
      return
      end

      subroutine fvsCloseFileC(filenamei,nch)
     -           bind(c, name="fvsCloseFileC") 
      use iso_c_binding
      implicit none
                       
      integer(c_int), bind(c) :: nch
      integer i
      character(c_char), dimension(nch), bind(c) :: filenamei
      character*255 filename

      do i=1,nch
        filename(i:i) = filenamei(i)
      enddo 
      call fvsCloseFile(filename,nch)
      return
      end

      subroutine fvsCloseFile(filename,nch)
      implicit none
      
!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'FVSCLOSEFILE'::FVSCLOSEFILE
!DEC$ ATTRIBUTES REFERENCE :: FILENAME, NCH

C     this routine closes "filename" if it is opened, it is not called
C     from within FVS. nch is the length of filename.

      integer nch,i
      character(len=nch) filename
      logical ls
      if (nch <= 0) return
      inquire(file=filename(:nch),opened=ls)
      if (ls) then
        inquire(file=filename(:nch),number=i)
        if (i > 0) close(unit=i)
      endif
      return
      end
      
      
      subroutine fvsAddActivity(idt,iactk,inprms,nprms,rtnCode)
      implicit none

C     add an activity to the schedule.

      include "PRGPRM.F77"
      include "CONTRL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'FVSADDACTIVITY'::FVSADDACTIVITY
!DEC$ ATTRIBUTES C,DECORATE :: FVSADDACTIVITY
!DEC$ ATTRIBUTES REFERENCE :: IDT, IACTK, INPRMS, NPRMS, RTNCODE
      
      integer :: i,idt,iactk,nprms,rtnCode,kode
      integer, parameter :: mxtopass=20
      real(kind=8) inprms(nprms)
      real(kind=4) prms(mxtopass)

      if (nprms > 0) then 
        do i=1,min(nprms,mxtopass)
          prms(i) = real(inprms(i),kind=4)
        enddo
      endif
      call opadd(idt,iactk,0,nprms,prms,kode)
      if (kode /= 0) then
        rtnCode = 1
      else
        call opincr (IY,ICYC,NCYC)
        rtnCode = 0
      endif
      return 
      end
      
      subroutine fvsSVSDimSizes(nsvsobjs,ndeadobjs,ncwdobjs,
     -                          mxsvsobjs,mxdeadobjs,mxcwdobjs)
      implicit none
      include "PRGPRM.F77"
      include "SVDATA.F77"
      include "SVDEAD.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSSVSDIMSIZES
!DEC$ ATTRIBUTES ALIAS:'FVSSVSDIMSIZES':: FVSSVSDIMSIZES
!DEC$ ATTRIBUTES REFERENCE :: NSVSOBJS,NDEADOBJS,NCWDOBJS
!DEC$ ATTRIBUTES REFERENCE :: MXSVSOBJS,MXDEADOBJS,MXCWDOBJS

      integer :: nsvsobjs,  ndeadobjs,  ncwdobjs,  
     -           mxsvsobjs, mxdeadobjs, mxcwdobjs 
      
      nsvsobjs   =  NSVOBJ
      ndeadobjs  =  NDEAD
      ncwdobjs   =  NCWD
      mxsvsobjs  =  MXSVOB
      mxdeadobjs =  MXDEAD
      mxcwdobjs  =  MXCWD
      return
      end

      subroutine fvsSVSObjDataC(namei,nch,actioni,nobjs,attr,rtnCode)
     -           bind(c, name="fvsSVSObjDataC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsSVSObjData 

      integer(c_int), bind(c) :: nch,rtnCode,nobjs
      real(c_double), dimension(nobjs), bind(c) :: attr
      character(c_char), dimension(10), bind(c) :: namei,actioni
      character name*10,action*4
      integer i
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      action="get"
      if (actioni(1) == "s") action="set"
      call fvsSVSObjData(name,nch,action,nobjs,attr,rtnCode)
      return
      end
     
      subroutine fvsSVSObjData(name,nch,action,nobjs,attr,rtnCode)
      implicit none

c     set and/or gets the named SVS object attributes
c     name    = char string of the variable name, (case sensitive)
c     nch     = the number of characters in "name" 
c     action  = char string that is one of "set" or "get" (case sensitive)
c     nobjs   = the number of objects, length of data
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 
c               1= "name" not found,
c               2= nobjs is greater than the corresponding max, no data transfered.
c               3= there were more/fewer than nobjs.
c               4= the length of the "name" string was too big or small

      include "PRGPRM.F77"
      include "CONTRL.F77"
      include "PLOT.F77"
      include "SVDATA.F77"
      include "SVDEAD.F77"
      
!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSSVSOBJDATA
!DEC$ ATTRIBUTES ALIAS:'FVSSVSOBJDATA':: FVSSVSOBJDATA
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ACTION, NOBJS, ATTR, RTNCODE

      integer :: nch,rtnCode,nobjs
      real(kind=8)      :: attr(nobjs)
      character(len=10) :: name
      character(len=4)  :: action
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
        
      name=name(1:nch)
      action=action(1:3)

      rtnCode = 0
      select case(name)
      
C     ALL object section (the locations, etc):
        
      case ("objtype")  
        if (nobjs > MXSVOB) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NSVOBJ) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = IOBJTP(:nsvobj)
        if (action=="set") IOBJTP(:nsvobj) = int(attr,4) 

      case ("objindex")  
        if (nobjs > MXSVOB) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NSVOBJ) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = IS2F(:nsvobj)
        if (action=="set") IS2F(:nsvobj) = int(attr,4) 

      case ("xloc")  
        if (nobjs > MXSVOB) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NSVOBJ) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = XSLOC(:nsvobj)
        if (action=="set") XSLOC(:nsvobj) = real(attr,4) 

      case ("yloc")  
        if (nobjs > MXSVOB) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NSVOBJ) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = YSLOC(:nsvobj)
        if (action=="set") YSLOC(:nsvobj) = real(attr,4) 
        
C     SNAG section:
        
      case ("snagdbh")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGDIA(:ndead)
        if (action=="set") SNGDIA(:ndead) = real(attr,4)
        
      case ("snaglen")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGLEN(:ndead)
        if (action=="set") SNGLEN(:ndead) = real(attr,4)

      case ("snagyear")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = IYRCOD(:ndead)
        if (action=="set") IYRCOD(:ndead) = int(attr,4)
        
        
      case ("snagspp")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = ISNSP(:ndead)
        if (action=="set") ISNSP(:ndead) = int(attr,4)
                
      case ("snagfdir")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = FALLDIR(:ndead)
        if (action=="set") FALLDIR(:ndead) = real(attr,4)
        
      case ("snagstat")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = ISTATUS(:ndead)
        if (action=="set") ISTATUS(:ndead) = int(attr,4) 

      case ("snagwt0")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGCNWT(:ndead,0)
        if (action=="set") SNGCNWT(:ndead,0) = real(attr,4) 

      case ("snagwt1")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGCNWT(:ndead,1)
        if (action=="set") SNGCNWT(:ndead,1) = real(attr,4) 

      case ("snagwt2")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGCNWT(:ndead,2)
        if (action=="set") SNGCNWT(:ndead,2) = real(attr,4) 

      case ("snagwt3")
        if (nobjs > MXDEAD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NDEAD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = SNGCNWT(:ndead,3)
        if (action=="set") SNGCNWT(:ndead,3) = real(attr,4) 

C     CWD section:

      case ("cwddia") 
        if (nobjs > MXCWD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NCWD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = cwddia(:ncwd)
        if (action=="set") cwddia(:ncwd) = real(attr,4) 
        
      case ("cwdlen") 
        if (nobjs > MXCWD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NCWD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = cwdlen(:ncwd)
        if (action=="set") cwdlen(:ncwd) = real(attr,4) 
        
      case ("cwdpil") 
        if (nobjs > MXCWD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NCWD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = cwdpil(:ncwd)
        if (action=="set") cwdpil(:ncwd) = real(attr,4) 
        
      case ("cwddir")
        if (nobjs > MXCWD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NCWD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = cwddir(:ncwd)
        if (action=="set") cwddir(:ncwd) = real(attr,4) 
        
      case ("cwdwt")  
        if (nobjs > MXCWD) then
          attr = 0
          rtnCode = 2
          return
        endif
        if (nobjs /= NCWD) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = cwdwt(:ncwd)
        if (action=="set") cwdwt(:ncwd) = real(attr,4) 
        
      case default
        rtnCode = 1
        attr = 0
      end select
      
      return
      end

      subroutine fvsFFEAttrsC(namei,nch,actioni,nobjs,attr,rtnCode)
     -           bind(c, name="fvsFFEAttrsC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsTreeAttr where namei and actioni are 
c     C-bindings of their correspoinding arguments.

      integer(c_int), bind(c) :: nch,rtnCode,nobjs
      real(c_double), dimension(nobjs), bind(c) :: attr
      character(c_char), dimension(10), bind(c) :: namei,actioni
      character name*10,action*4
      integer i
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      action="get"
      if (actioni(1) == "s") action="set"
      call fvsFFEAttrs(name,nch,action,nobjs,attr,rtnCode)
      return
      end     
      
      subroutine fvsFFEAttrs(name,nch,action,nobjs,attr,rtnCode)
      implicit none

c     set and/or gets the named FFE variables
c     name    = char string of the variable name, (case sensitive)
c     nch     = the number of characters in "name" 
c     action  = char string that is one of "set" or "get" (case sensitive)
c     nobjs   = the number of objects, length of data
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 
c               1= "name" not found,
c               2= nobjs is greater than the corresponding max, no data transfered.
c               3= there were more/fewer than nobjs.
c               4= the length of the "name" string was too big or small

      include "PRGPRM.F77"
      include 'FMPARM.F77'
      include 'FMCOM.F77'

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSFFEATTRS
!DEC$ ATTRIBUTES ALIAS:'FVSFFEATTRS':: FVSFFEATTRS
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, ACTION, NOBJS, ATTR, RTNCODE

      integer :: nch,rtnCode,nobjs
      real(kind=8)      :: attr(nobjs)
      character(len=10) :: name
      character(len=4)  :: action
      
      if (nch == 0 .or. nch > 10) then
        rtnCode = 4
        return
      endif
        
      name=name(1:nch)
      action=action(1:3)

      rtnCode = 0
      
      select case(name)

      case ("fallyrs0")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(1:MAXSP,0)
        if (action=="set") TFALL(1:MAXSP,0) = real(attr,4) 

      case ("fallyrs1")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(1:MAXSP,1)
        if (action=="set") TFALL(1:MAXSP,1) = real(attr,4) 

      case ("fallyrs2")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(1:MAXSP,2)
        if (action=="set") TFALL(1:MAXSP,2) = real(attr,4) 

      case ("fallyrs3")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(1:MAXSP,3)
        if (action=="set") TFALL(1:MAXSP,3) = real(attr,4) 

      case ("fallyrs4")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(:MAXSP,4)
        if (action=="set") TFALL(:MAXSP,4) = real(attr,4) 

      case ("fallyrs5")
        if (nobjs /= MAXSP) then
          attr = 0
          rtnCode = 3
          return
        endif
        if (action=="get") attr = TFALL(:MAXSP,5)
        if (action=="set") TFALL(:MAXSP,5) = real(attr,4) 

      case default
        rtnCode = 1
        attr = 0
      end select
      
      return
      end
      
      
      subroutine fvsUnitConversionC(namei,nch,value,rtnCode)
     -           bind(c, name="fvsUnitConversionC") 
      use iso_c_binding
      implicit none

c     C-callable version of fvsUnitConversionC where namei is a
c     C-bindings of name

      integer(c_int), bind(c) :: nch,rtnCode
      real(c_double), bind(c) :: value
      character(c_char), dimension(15), bind(c) :: namei
      character name*15
      integer i
      
      if (nch == 0 .or. nch > 15) then
        rtnCode = 1
        return
      endif
      name=" "
      do i=1,nch
        name(i:i) = namei(i)
      enddo
      call fvsUnitConversion(name,nch,value,rtnCode)
      return
      end

      subroutine fvsUnitConversion(name,nch,value,rtnCode)
      implicit none
      
c     get the named unit conversion
c     name    = char string of the variable name (case sensitive)
c     rtnCode = 0 is OK, 
c               1= "name" not found,

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSUNITCONVERSION
!DEC$ ATTRIBUTES ALIAS:'FVSUNITCONVERSION'::FVSUNITCONVERSION
!DEC$ ATTRIBUTES REFERENCE :: NAME, NCH, VALUE, RTNCODE

      include "METRIC.F77"
      
      integer :: nch,rtnCode
      real(kind=8)       :: value
      character(len=15)  :: name

      rtnCode = 0      
      if (nch == 0 .or. nch > 15) then
        rtnCode = 1
        return
      endif
        
      select case(name(1:nch))

      case ("CMtoIN")
        value = CMtoIN
      case ("CMtoFT")
        value = CMtoFT
      case ("MtoIN")
        value = MtoIN
      case ("MtoFT")
        value = MtoFT  
      case ("KMtoMI")
        value = KMtoMI 
      case ("M2toFT2")
        value = M2toFT2
      case ("HAtoACR")
        value = HAtoACR
      case ("M3toFT3")
        value = M3toFT3
      case ("KGtoLB")
        value = KGtoLB 
      case ("TMtoTI")
        value = TMtoTI 
      case ("CtoF1")
        value = CtoF1  
      case ("CtoF2")
        value = CtoF2  
      case ("INtoCM")
        value = INtoCM 
      case ("FTtoCM")
        value = FTtoCM 
      case ("INtoM")
        value = INtoM  
      case ("FTtoM")
        value = FTtoM  
      case ("MItoKM")
        value = MItoKM 
      case ("FT2toM2")
        value = FT2toM2 
      case ("ACRtoHA")
        value = ACRtoHA 
      case ("FT3toM3")
        value = FT3toM3 
      case ("LBtoKG")
        value = LBtoKG 
      case ("TItoTM")
        value = TItoTM 
      case ("FtoC1")
        value = FtoC1 
      case ("FtoC2")
        value = FtoC2  
      case ("BTUtoKJ")
        value = BTUtoKJ       
      case ("M2pHAtoFT2pACR")
        value = M2pHAtoFT2pACR
      case ("M3pHAtoFT3pACR")
        value = M3pHAtoFT3pACR
      case ("FT2pACRtoM2pHA")
        value = FT2pACRtoM2pHA
      case ("FT3pACRtoM3pHA")
        value = FT3pACRtoM3pHA 
      case default
        rtnCode = 1
      end select
      return
      end
      

