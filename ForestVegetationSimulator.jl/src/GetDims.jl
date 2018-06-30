function fvsDimSizes()

    ntrees    = Ref{Int32}(0)
    ncycles   = Ref{Int32}(0)
    nplots    = Ref{Int32}(0)
    maxtrees  = Ref{Int32}(0)
    maxspecies= Ref{Int32}(0)
    maxplots  = Ref{Int32}(0)
    maxcycles = Ref{Int32}(0)

    ccall(Libdl.dlsym(lib,:fvsdimsizes_),Void,(
        Ref{Int32}, #ntrees
        Ref{Int32}, #ncycles
        Ref{Int32}, #nplots
        Ref{Int32}, #maxtrees
        Ref{Int32}, #maxspecies
        Ref{Int32}, #maxplots
        Ref{Int32}, #maxcycles
    ),ntrees,ncycles,nplots,maxtrees,maxspecies,maxplots,maxcycles)

    return[ntrees[],ncycles[],nplots[],maxtrees[],maxspecies[],maxplots[],maxcycles[]]
end