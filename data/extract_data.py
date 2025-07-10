"""
Quick script to extract sets and parameters from the IFPRI GAMS model. Not clean
and not meant to be used in production, but useful for quick exploration of the
model data.

I could not get this to work with a basic Python installation, I needed to use Conda
to install the GAMS Python API. The requirements file is here if you need to replicate
the environment. I requires a GAMS installation.
"""


#%%

import gams.transfer as gt
import pandas
import json

# %%


w = gt.Container(r"ifpri.gdx")




# %%

value_info = {}


sets = {}
parameters = {}
aliases = {}
for name, X in w:
    if isinstance(X, gt.Set):
        sets[name] = X
    elif isinstance(X, gt.Parameter):
        parameters[name] = X
    elif isinstance(X, gt.Alias):
        aliases[name] = X



used_sets = [
    "s",
    "g",
    "h",
    "f",
]

sets = {s: sets[s] for s in sets if s in used_sets}
aliases = {s: aliases[s] for s in aliases if s in used_sets} # s is aliased with G


value_info["sets"] = {}
for s in sets.values():
    value_info["sets"][s.name] = []
for a in aliases.values():
    value_info["sets"][a.name] = [a._alias_with.name]



for name, S in sets.items():
    S.records.to_csv(f"pre_model/{name}.csv", index=False)

for name, A in aliases.items():
    A.records.to_csv(f"pre_model/{name}.csv", index=False)



used_parameters = [
    "a0",
    "c0",
    "cm0",
    "cn0",
    "d0",
    "dtax0",
    "esub",
    "esubv",
    "eta",
    "fd0",
    "fe0",
    "fsav0",
    "g0",
    "govdef",
    "gsav0",
    "hp0",
    "hs0",
    "i0",
    "id0",
    "m0",
    "mdd0",
    "mdm0",
    "pm0",
    "s0",
    "sh0",
    "sigma",
    "stk0",
    "ta",
    "tm",
    "trn0",
    "tva",
    "tva0",
    "x0",
]

parameters = {p: parameters[p] for p in parameters if p in used_parameters}


def domain_name(d):
    if isinstance(d, gt.Set) or isinstance(d, gt.Alias):
        return d.name
    if d=="*":
        return "uni"
    return None

def get_domain(p):
    """Get the domain of a parameter."""
    return [domain_name(d) for d in p.domain] if p.domain else []


value_info["parameters"] = {}
for p in parameters.values():
    domain = get_domain(p)
    if domain:
        value_info["parameters"][p.name] = domain
    else:
        value_info["parameters"][p.name] = []


with open("pre_model/value_info.json", "w") as f:
    json.dump(value_info, f)


for name, P in parameters.items():
    if P.records is not None:
        P.records.to_csv(f"pre_model/{name}.csv", index=False)


# %%


fe_domain = parameters["fe0"].records["uni"].unique()

h = sets["h"].records["uni"].unique()

# %%

[a for a in fe_domain if a not in h]


# %%
dtax = parameters["dtax0"].records["uni"].unique()
[a for a in dtax if a not in h]



# %%
