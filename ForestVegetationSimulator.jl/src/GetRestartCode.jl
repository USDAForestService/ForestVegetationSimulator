function fvsGetRestartCode()
restart_code=0
ccall((:fvsgetrestartcode_ , "libFVS_iec"), Void,(
Ref{Int32},
), restart_code)
end