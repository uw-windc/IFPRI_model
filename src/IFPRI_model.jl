module IFPRI_model

using CSV
using DataFrames
using NamedArrays
using JSON
using MPSGE


include("load_data.jl")

export load_data, @extract_to_local_scope

include("model.jl")

export IFPRI_mpsge

end # module IFPRI_model
