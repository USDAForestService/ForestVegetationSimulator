module ForestVegetationSimulator

using DataFrames
##structure if needed for METADATA Julia pkg
# const fvs = joinpath(dirname(@__FILE__), "..", "deps", fvsProgram)

#The name __init__ is reserved as a name for a function in a module that is automatically run when the module is loaded,
#function __init__()
#    global lib = Libdl.dlopen(dllname) 

include("AddActivity.jl")
include("AddTrees.jl")
include("fvsRun.jl")
include("GetDims.jl")
include("GetEventMonitorVariables.jl")
include("GetRestartCode.jl")
include("GetSpeciesCodes.jl")
include("GetStandIDs.jl")
include("LoadFVS.jl")
include("SetCmdLine.jl")
include("SpeciesAttributes.jl")
include("TreeAttributes.jl")
include("utils.jl")

export AddActivity
export fvsAddTrees
export fvsRun
export fvsDimSizes
export fvsGetEventMonitorVariables
export fvsGetRestartCode
export fvsSpeciesCode
export fvsGetStandIDs
export fvsLoad
export fvsSetCmdLine
export fvsSpeciesAttr
export fvsTreeAttr

#utils
export meansd
export ch2num
export tvalue
export grdtim
export bratio 
export cmrang





end