function fvsRun(stop_point_code,stop_point_year)
    ccall(Libdl.dlsym(lib,:fvssetstoppointcodes_),Void,(
    Ref{Int32},
    Ref{Int32},
    ),stop_point_code,stop_point_year)

    ccall(Libdl.dlsym(lib,:fvs_),Void,(
    Ref{Int32},
    Ref{Int32},
    ),stop_point_code,stop_point_year)
end
