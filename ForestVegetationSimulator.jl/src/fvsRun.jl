function fvsRun(stop_point_code,stop_point_year)
    ccall(Libdl.dlsym(lib,:fvssetstoppointcodes_),Cvoid,(
    Ref{Int32},
    Ref{Int32},
    ),stop_point_code,stop_point_year)

    ccall(Libdl.dlsym(lib,:fvs_),Cvoid,(
    Ref{Int32},
    Ref{Int32},
    ),stop_point_code,stop_point_year)
end
