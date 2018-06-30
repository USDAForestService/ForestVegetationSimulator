function fvsSpeciesAttr(;action="get")
    name=repeat(" ", 8)
    nch=len(name)
    action=action
    attr = Vector{Float64}()
    rtnCode = 0
    
    ccall(Libdl.dlsym(lib,:fvsspeciesattr_), Void,(
    Cstring, #name
    Ref{Int32}, #nch
    Cstring, #action
    Ref{Float32}, #attr
    Ref{Int32}, #rtnCode
    ),name,nch,action,attr,rtnCode)
end