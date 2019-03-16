function fvsGetEventMonitorVariables(vars::Array{String}; action="get")
    out=[]
for i in vars
    name=i
    nch=length(name)
    action=action
    attr = zeros(Float64,length(vars))
    rtnCode = 0
    
    ccall(Libdl.dlsym(lib,:fvsevmonattr_), Cvoid,(
    Cstring, #name
    Ref{Int32}, #nch
    Cstring, #action
    Ptr{Float64}, #attr
    Ref{Int32}, #rtnCode
    ),name,nch,action,attr,rtnCode)
    push!(out,[name,nch,action,attr,rtnCode])
end
    return out
end