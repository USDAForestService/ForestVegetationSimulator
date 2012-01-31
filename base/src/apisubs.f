c     This is a collection of routines that provide an interface to 
c     the shared library version of FVS. Other routines that exist
c     inside FVS may also be called.

      subroutine fvsDimSizes(ntrees,ncycles,maxtrees,maxspecies,
     -                       maxplots,maxcycles)
      implicit none
      include "PRGPRM.F77"
      include "CONTRL.F77"
      integer :: ntrees,ncycles,maxtrees,maxspecies,maxplots,maxcycles
      ntrees    = ITRN
      ncycles   = NCYC
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
      
      subroutine fvsTreeAttr(name,nch,action,ntrees,attr,rtnCode)
      implicit none

c     set and/or gets the named tree attributes
c     name    = char string of the variable name,
c     nch     = the number of characters in "name" (case sensitive)
c     action  = char string that is one of "set" or "get" (case sensitive)
c     ntrees  = the number of trees, length of data
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 1= "name" not found,
c               2= ntrees is greater than maxtrees, not all data transfered
c               3= there were more/fewer than ntrees.
c
      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"

      integer :: nch,rtnCode,ntrees
      double precision  :: attr(ntrees)
      character(len=10) :: name
      character(len=4)  :: action

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
        if (action=="set") prob = attr
      case ("dbh")
        if (action=="get") attr = dbh(:itrn)
        if (action=="set") dbh = attr
      case ("dg")
        if (action=="get") attr = dg(:itrn)
        if (action=="set") dg = attr
      case ("ht")
        if (action=="get") attr = ht(:itrn)
        if (action=="set") ht = attr
      case ("htg")
        if (action=="get") attr = htg(:itrn)
        if (action=="set") htg = attr
      case ("crwdth")
        if (action=="get") attr = crwdth(:itrn)
        if (action=="set") ht = attr
      case ("cratio")
        if (action=="get") attr = icr(:itrn)
        if (action=="set") icr = attr
      case ("species")
        if (action=="get") attr = isp(:itrn)
        if (action=="set") isp = attr
      case ("age")
        if (action=="get") attr = abirth(:itrn)
        if (action=="set") abirth = attr
      case default
        rtnCode = 1
        attr = 0
      end select

      return
      end


      subroutine fvsEvmonAttr(name,nch,action,attr,rtnCode)
      implicit none

c     set and/or gets the named tree attributes
c     name    = char string of the variable name,
c     nch     = the number of characters in "name" (case sensitive)
c     action  = char string that is one of "set" or "get" (case sensitive)
c     attr    = a vector of length data, always "double"
c     rtnCode = 0 is OK, 1=action is "get" and variable
c               is known to be undefined. 2= "name" not found, 
c
      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "OPCOM.F77"

      integer :: nch,rtncode,iv,i
      double precision  :: attr
      character(len=10) :: name
      character(len=4)  :: action

      name=name(1:nch)
      action=action(1:3)

      rtncode = 0

      select case(name)
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
      case default
        iv=0
      end select
      
      if (iv == 0) then
        rtncode = 2
        return
      endif
      
      i = mod(iv,100)
      iv = iv/100
 
      select case (iv)
      case (1)
        if (action=="get") attr = tstv1(i)
        if (action=="set") tstv1(i) = attr
      case (2)
        if (action=="get") attr = tstv2(i)
        if (action=="set") tstv2(i) = attr
      case (3)
        if (action=="get") attr = tstv3(i)
        if (action=="set") tstv3(i) = attr
      case (4)
        if (action=="get") then
          if (ltstv4(i)) then
            attr = tstv4(i)
          else
            attr = 0
            rtncode = 1
          endif
        endif
        if (action=="set") then
          tstv4(i) = attr
          ltstv4(i)=.true.
        endif
      end select
      return
      end
      
      subroutine fvsAddTrees(in_dbh,in_species,in_ht,in_cratio,
     -                       in_plot,in_tpa,ntrees,rtnCode)
      implicit none

c     rtnCode = 0 when all is OK
c               1 when there isn't room for the ntrees
c                 or when ntrees is zero
      
      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"
      include "OUTCOM.F77"
      include "PLOT.F77"
      include "VARCOM.F77"
      include "ESTREE.F77"
      include "STDSTK.F77"

      double precision :: in_dbh(ntrees),in_species(ntrees),
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
        dbh(itrn) = in_dbh    (i)
        isp(itrn) = in_species(i)
        ht (itrn) = in_ht     (i)
        icr(itrn) = in_cratio (i)
        itre(itrn)= in_plot   (i)
        prob(itrn)= in_tpa    (i)
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
        defect(itrn)=0.
        ispecl(itrn)=0
        oldrn(itrn)=0.
        ptocfv(itrn)=0.
        pmrcfv(itrn)=0.
        pmrbfv(itrn)=0.
        ncfdef(itrn)=0
        nbfdef(itrn)=0
        pdbh(itrn)=0.
        pht(itrn)=0.
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
      
      
      subroutine fvsCutTrees(pToCut,ntrees,rtnCode)

      include "PRGPRM.F77"
      include "ARRAYS.F77"
      include "CONTRL.F77"
      
      integer :: ntrees,rtnCode
      double precision :: pToCut(ntrees)
      pToCut = 0.
      if (ntrees <= 0 .or. ntrees > maxtre) then
        rtnCode = 1
        return
      endif
      
      print *,"Not yet implemented"
      rntCode = 1
      return
      end
      
      subroutine fvsStandID(sID,mID,ncsID,ncmID)

      include "PRGPRM.F77"
      include "PLOT.F77"
      
      integer :: ncsID,ncmID
      character(len=26) sID
      character(len=4)  mID
      
      sID = NPLT
      mID = MGMID
      ncsID = len_trim(NPLT)
      ncmID = len_trim(MGMID)
      return
      end


      

