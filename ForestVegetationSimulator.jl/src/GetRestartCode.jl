function fvsGetRestartCode()
restart_code=0
ccall((:fvsgetrestartcode_ , lib), Cvoid,(
Ref{Int32},
), restart_code)
end