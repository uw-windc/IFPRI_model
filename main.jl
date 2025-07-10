using IFPRI_model
using CSV
using DataFrames
using NamedArrays
using JSON

dir_path = "data/pre_model"



value_info =  open(joinpath(dir_path, "value_info.json")) do f
    JSON.parse(read(f, String))
end

sets = Dict{Symbol, Vector}()
for (set_name, aliases) in value_info["sets"]
    df = CSV.read(joinpath(dir_path, "$set_name.csv"), DataFrame, stringtype = String)
    sets[Symbol(set_name)] = collect(df[!, 1])
end




function extract_set_from_domain_element(element::String, parameter::DataFrame, sets::Dict{Symbol, Vector})
    if element == "uni"
        return unique(parameter[!, element])
    else
        return sets[Symbol(element)]
    end
end



parameters = Dict{Symbol, Union{NamedArray, Number}}()
for (param_name, domain) in value_info["parameters"]
    path = joinpath(dir_path, "$param_name.csv")
    if !isfile(path)
        domain = extract_set_from_domain_element.(domain, Ref(DataFrame()), Ref(sets))
        if length(domain) == 1
            out = NamedArray(zeros(length.(domain)...), domain...)
        else
            out = NamedArray(zeros(length.(domain)...), domain)
        end
        parameters[Symbol(param_name)] = out
        
        continue
    end

    df = CSV.read(joinpath(dir_path, "$param_name.csv"), DataFrame, stringtype = String)
    if length(domain) == 0
        parameters[Symbol(param_name)] = df[1,1]
        continue
    else
        full_domain = extract_set_from_domain_element.(domain, Ref(df), Ref(sets))
        if length(full_domain) == 1
            parameters[Symbol(param_name)] = NamedArray(zeros(length.(full_domain)...), full_domain...)
        else
            parameters[Symbol(param_name)] = NamedArray(zeros(length.(full_domain)...), full_domain)
        end
    end

    for row in eachrow(df)
        value = row[:value]
        element = [row[element] for element in domain]
        parameters[Symbol(param_name)][element...] = value
    end
        

end

parameters[:d0]["agr"]