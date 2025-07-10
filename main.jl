using IFPRI_model
using MPSGE
using DataFrames

data = load_data();

M = IFPRI_mpsge(data);

solve!(M)#, cumulative_iteration_limit=0)
benchmark_value = generate_report(M)


MPSGE.aux_constraints(M)

#set_value!.(M[:ta], .25)

M_f = IFPRI_mpsge(data; 
                    flexiblePFX = 1,
                    eqyield = 0,
                    fixedGS = 1
        );

solve!(M_f)

df = generate_report(M_f);


value(M[:GSAV])

value(M_f[:GSAV])

value.(M[:Y]) .- value.(M_f[:Y])



P = production(M[:A]["agr"])
cost_function(P.input, virtual=:partial)

cost_function(P.input.children[1])

cost_function(M[:A]["agr"], :m)