function fvsLoad(fvsProgram,basedir="~/bin")
    const fvs = joinpath(basedir, fvsProgram)
    global lib = Libdl.dlopen(fvs)
    #return([basedir,fvs,lib]) #for debugging
end 

#if pwd() not in load path
#push!(DL_LOAD_PATH,pwd())