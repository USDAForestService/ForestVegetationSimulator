function fvsSpeciesCode()
    fvs_code = repeat(" ", 4)
    fia_code = repeat(" ", 4)
    plant_code = repeat(" ", 6)
    indx = 0
    nchfvs = 0
    nchfia = 0
    nchplant = 0
    rtnCode = 0

    ccall(Libdl.dlsym(lib,:fvsspeciescode_), Void,(
    Cstring, #fvs_code
    Cstring, #fia_code
    Cstring, #plant_code
    Ref{Int32}, #indx
    Ref{Int32}, #nchfvs
    Ref{Int32}, #nchfia
    Ref{Int32}, #nchplant
    Ref{Int32}, #rtnCode
    ),fvs_code,fia_code,plant_code,indx,nchfvs,nchfia,nchplant,rtnCode)
    return[fvs_code,fia_code,plant_code,indx,nchfvs,nchfia,nchplant,rtnCode]
end