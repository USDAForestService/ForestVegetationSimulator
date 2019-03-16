function fvsSetCmdLine(inputstring)
    lengthCL=length(inputstring)
    rtncode=0
    ccall(Libdl.dlsym(lib,:fvssetcmdline_), Cvoid,(
    Cstring, #theCmdLine
    Ref{Int32}, #LenCL
    Ref{Int32}, #rtnCode
    ),inputstring,lengthCL,rtncode)
end


