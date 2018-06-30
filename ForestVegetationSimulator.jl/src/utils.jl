function meansd(a::Array{Float32})
n=length(a)
abar=Ref{Float64}(0)
var=Ref{Float64}(0)
std=Ref{Float64}(0)

ccall(Libdl.dlsym(lib,:meansd_),Void,(
Ptr{Float32},#A
Ref{Int32}, #N
Ref{Float64}, #ABAR
Ref{Float64}, #VAR
Ref{Float64},  #STD
),a,n,abar,var,std)

return [abar_ref[],var_ref[],std_ref[]]

end

function ch2num(ICYC::Int32)
C2=rep(" ",2)

ccall(Libdl.dlsym(lib,:ch2num_),Void,(
Cstring, #C2
Ref{Int32},#ICYC
),C2,ICYC)

end


function tvalue(N::Int32,P::Float32)

T=Ref{Float32}(0)
IERR=Ref{Int32}(0)

ccall(Libdl.dlsym(lib,:tvalue_),Void,(
Ref{Int32}, #N
Ref{Float32}, #P
Ref{Float32}, #T
Ref{Int32},#IERR
),N,P,T,IERR)

return T[]

end


function grdtim()

DAT = rep(" ",8)
TIM = rep(" ",8)

ccall(Libdl.dlsym(lib,:grdtim_),Void,(
Cstring, #DAT
Cstring, #TIM
),DAT,TIM)

return [DAT,TIM]

end

function bratio(IS,D,H)
  
    bratio=ccall(Libdl.dlsym(lib,:bratio_),Float32,(
    Ref{Int32}, #I
    Ref{Float32},#D
    Ref{Float32},#H
    ),IS,D,H)

    return bratio
end


function cmrang(ARR::Array{Float32}, INDX::Array{Float32})

    LEN=length(ARR)

    cmrang=ccall(Libdl.dlsym(lib,:cmrang_), Float32,(
    Ref{Int32}, #LEN
    Ptr{Int32}, #INDX
    Ptr{Float64}, #ARR
    ),LEN,INDX,ARR)

return cmrang

end
