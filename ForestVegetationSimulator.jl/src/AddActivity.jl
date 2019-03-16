# subroutine fvsAddActivity(idt,iactk,inprms,nprms,rtnCode)
#    implicit none

#     integer :: i,idt,iactk,nprms,rtnCode,kode
#       integer, parameter :: mxtopass=>20
#       real(kind=>8) inprms(nprms)
#       real(kind=>4) prms(mxtopass)

function AddActivity(year,activity,parms)

 activity_list=Dict{String,Int32}(
    "BASE_TREELIST"=>  80, "BASE_CRNMULT" =>  81, "BASE_MANAGED" =>  82, "BASE_FIXCW"   =>  90,
    "BASE_BAIMULT" =>  91, "BASE_HTGMULT" =>  92, "BASE_REGHMULT"=>  93, "BASE_MORTMULT"=>  94,
    "ESTB_SPECMULT"=>  95, "BASE_REGDMULT"=>  96, "BASE_FIXMORT" =>  97, "BASE_FIXDG"   =>  98,
    "BASE_FIXHTG"  =>  99, "BASE_SYSTEM"  => 100, "DBIN_SQLIN"   => 101, "DBIN_SQLOUT"  => 102,
    "BASE_HTGSTOP" => 110, "BASE_TOPKILL" => 111, "BASE_SETSITE" => 120, "BASE_ATRTLIST"=> 198,
    "BASE_CUTLIST" => 199, "BASE_MINHARV" => 200, "BASE_SPECPREF"=> 201, "BASE_TCONDMLT"=> 202,
    "BASE_YARDLOSS"=> 203, "BASE_FVSSTAND"=> 204, "BASE_CRUZFILE"=> 205, "BASE_MCDEFECT"=> 215,
    "BASE_BFDEFECT"=> 216, "BASE_VOLUME"  => 217, "BASE_BFVOLUME"=> 218, "BASE_THINAUTO"=> 222,
    "BASE_THINBTA" => 223, "BASE_THINATA" => 224, "BASE_THINBBA" => 225, "BASE_THINABA" => 226,
    "BASE_THINPRSC"=> 227, "BASE_THINDBH" => 228, "BASE_SALVAGE" => 229, "BASE_THINSDI" => 230,
    "BASE_THINCC"  => 231, "BASE_THINHT"  => 232, "BASE_THINMIST"=> 233, "BASE_THINRDEN"=> 234,
    "BASE_THINPT"  => 235, "BASE_THINRDSL"=> 236, "BASE_SETPTHIN"=> 248, "BASE_PRUNE"   => 249,
    "BASE_COMPRESS"=> 250, "BASE_FERTILIZ"=> 260, "ESTB_TALLY"   => 427, "ESTB_TALLYONE"=> 428,
    "ESTB_TALLYTWO"=> 429, "ESTB_PLANT"   => 430, "ESTB_NATURAL" => 431, "ESTB_ADDTREES"=> 432,
    "ESTB_STOCKADJ"=> 440, "ESTB_HTADJ"   => 442, "BASE_RESETAGE"=> 443, "ESTB_SPROUT"  => 450,
    "ESTB_NATURAL" => 490, "ESTB_BURNPREP"=> 491, "ESTB_MECHPREP"=> 493, "COVR_COVER"   => 900,
    "MIST_MISTMULT"=>2001, "MIST_MISTPREF"=>2002, "MIST_MISTMORT"=>2003, "MIST_MISTHMOD"=>2004,
    "MIST_MISTGMOD"=>2005, "MIST_MISTPINF"=>2006, "MIST_MISTABLE"=>2007, "FIRE_SALVSP"  =>2501,
    "FIRE_SOILHEAT"=>2503, "FIRE_BURNREPT"=>2504, "FIRE_MOISTURE"=>2505, "FIRE_SIMFIRE" =>2506,
    "FIRE_FLAMEADJ"=>2507, "FIRE_POTFIRE" =>2508, "FIRE_SNAGOUT" =>2512, "FIRE_FUELOUT" =>2515,
    "FIRE_SALVAGE" =>2520, "FIRE_FUELINIT"=>2521, "FIRE_SNAGINIT"=>2522, "FIRE_PILEBURN"=>2523,
    "FIRE_FUELTRET"=>2525, "FIRE_FUELREPT"=>2527, "FIRE_MORTREPT"=>2528, "FIRE_DROUGHT" =>2529,
    "FIRE_FUELMOVE"=>2530, "FIRE_FUELMODL"=>2538, "FIRE_DEFULMOD"=>2539, "FIRE_CARBREPT"=>2544,
    "FIRE_CARBCUT" =>2545, "FIRE_CANFPROF"=>2547, "FIRE_FUELFOTO"=>2548, "FIRE_FIRECALC"=>2549,
    "FIRE_FMODLIST"=>2550, "FIRE_DWDVLOUT"=>2551, "FIRE_DWDCVOUT"=>2552, "FIRE_FUELSOFT"=>2553,
    "ECON_PRETEND" =>2605, "ECON_SEVSTART"=>2606, "ECON_SPECCST" =>2607, "ECON_SPECRVN" =>2608,
    "ECON_STRTECON"=>2609)

    typeof(activity) == String ?  #test
    try #true
        iatck =activities[activity]
        catch error
        if isa(error, KeyError)
            println("no match found for that activity code")
        end
    end
    : iatck = activity #false

    nprms=length(parms)
    rtnCode=0

    ccall(Libdl.dlsym(lib,:fvsaddactivity_), Cvoid,(
    Ref{Int32}, #year (aka idt)
    Ref{Int32}, #iatck
    Ref{Int32}, #parms(aka inprms)
    Ref{Int32}, #nprms
    Ref{Int32}, #rtnCode
    ),year,iatck,parms,nprms,rtnCode)

end
