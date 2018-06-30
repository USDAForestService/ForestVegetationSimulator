# subroutine fvsAddTrees(in_dbh,in_species,in_ht,in_cratio,
#                           in_plot,in_tpa,ntrees,rtnCode)

# real(kind=8) :: in_dbh(ntrees),in_species(ntrees),
#      -    in_ht(ntrees),in_cratio(ntrees),in_plot(ntrees),
#      -    in_tpa(ntrees)
#       real :: cw,crdum
#       integer :: ntrees,rtnCode,i

function fvsAddTrees(in_dbh,in_species,in_ht,in_cratio,
                          in_plot,in_tpa,ntrees,rtnCode)

##placeholder to be fixed later
    ccall(Libdl.dlsym(lib,:fvsaddtrees_), Void,(
    Ref{Float32}, #in_dbh
    Ref{Float32}, #in_species
    Ref{Float32}, #in_ht
    Ref{Float32}, #in_cratio
    Ref{Float32}, #in_plot
    Ref{Float32}, #in_tpa
    Ref{Int32}, #ntrees
    Ref{Int32}, #rtnCode
    ),in_dbh,in_species,in_ht,in_cratio,in_plot,in_tpa,ntrees,rtnCode)
end