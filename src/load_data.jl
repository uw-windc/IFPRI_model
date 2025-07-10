"""
    @extract_to_local_scope(data, block)

This macro will extract the variables in the `block` from `data`.

## Example

```julia
@extract_to_local_scope(data, begin
    set_i
    set_g
end)
```
"""
macro extract_to_local_scope(data, block)
    code = quote end
    last_line = block.args[1]
    for X in block.args
        if X isa LineNumberNode
            last_line = X
            continue
        end
        if Meta.isexpr(X, :call)
            element = X.args[2]
            name = X.args[3]
        else
            element = X
            name = X
        end

        push!(code.args, :($(esc(name)) = $(esc(data))[$(QuoteNode(element))]))
        
    end
    return code
end



function extract_set_from_domain_element(element::String, parameter::DataFrame, sets::Dict{Symbol, Any})
    if element == "uni"
        return unique(parameter[!, element])
    else
        return sets[Symbol(element)]
    end
end

function load_data(dir_path::String = "@__dir__/../data/pre_model")

    value_info =  open(joinpath(dir_path, "value_info.json")) do f
        JSON.parse(read(f, String))
    end

    all_values = Dict{Symbol, Any}()

    for (set_name, aliases) in value_info["sets"]
        df = CSV.read(joinpath(dir_path, "$set_name.csv"), DataFrame, stringtype = String)
        all_values[Symbol(set_name)] = collect(df[!, 1])
    end

    for (param_name, domain) in value_info["parameters"]
        path = joinpath(dir_path, "$param_name.csv")
        if !isfile(path)
            domain = extract_set_from_domain_element.(domain, Ref(DataFrame()), Ref(all_values))
            if length(domain) == 1
                out = NamedArray(zeros(length.(domain)...), domain...)
            else
                out = NamedArray(zeros(length.(domain)...), domain)
            end
            all_values[Symbol(param_name)] = out
            
            continue
        end

        df = CSV.read(joinpath(dir_path, "$param_name.csv"), DataFrame, stringtype = String)
        if length(domain) == 0
            all_values[Symbol(param_name)] = df[1,1]
            continue
        else
            full_domain = extract_set_from_domain_element.(domain, Ref(df), Ref(all_values))
            if length(full_domain) == 1
                all_values[Symbol(param_name)] = NamedArray(zeros(length.(full_domain)...), full_domain...)
            else
                all_values[Symbol(param_name)] = NamedArray(zeros(length.(full_domain)...), full_domain)
            end
        end

        for row in eachrow(df)
            value = row[:value]
            element = [row[element] for element in domain]
            all_values[Symbol(param_name)][element...] = value
        end
            

    end

    return all_values

end



