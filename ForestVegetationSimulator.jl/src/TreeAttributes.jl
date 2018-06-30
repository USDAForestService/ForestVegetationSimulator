#using DataFrames

function fvsTreeAttr(vars;action="get")
      out=[]
      for i in vars
            name    = i
            nch     = length(name)
            action  = action
            ntrees  = fvsDimSizes()[1]
            attr    = zeros(Float64,ntrees)
            rtnCode = 0

            ccall(Libdl.dlsym(lib,:fvstreeattr_), Void,(
            Cstring, #name
            Ref{Int32}, #nch
            Cstring, #action
            Ref{Int32}, # ntrees
            Ptr{Float64}, #attr
            Ref{Int32}, # rtnCode
            ),name,nch,action,ntrees,attr,rtnCode)
      push!(out,[name,nch,action,ntrees,attr,rtnCode])
      end
return out
end