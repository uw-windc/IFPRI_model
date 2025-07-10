module IFPRI_model

using CSV
using DataFrames
using NamedArrays
using JSON


include("load_data.jl")

export load_data, @extract_to_local_scope


end # module IFPRI_model
