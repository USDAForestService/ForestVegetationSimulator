function fvsLoad(fvsProgram,basedir="~/bin")
    const fvs = joinpath(basedir, fvsProgram)
    global lib = Libdl.dlopen(fvs)
    #return([basedir,fvs,lib]) #for debugging
end 


# not sure if needed to load explicitly ? "libfvsSQL.dll"
#dllname=joinpath(basedir,split(fvsProgram,".")[1])
#working example
#fvsLoad("libFVS_iec.dll","C:\\Users\\Casey\\Documents\\SVN\\compiled")

#working hard coded example
#l=Libdl.dlopen("C:\\Users\\Casey\\Documents\\SVN\\compiled\\libFVS_iec.dll")