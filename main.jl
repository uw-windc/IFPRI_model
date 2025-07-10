using IFPRI_model
using MPSGE


data = load_data();


@extract_to_local_scope(data, begin
    s => S
    g => G
    h => H
    f => F
    a0
    c0
    cm0
    cn0
    d0
    dtax0
    esub
    esubv
    eta
    fd0
    fe0
    fsav0
    g0
    govdef
    gsav0
    hp0
    hs0
    i0
    id0
    m0
    mdd0
    mdm0
    pm0
    s0
    sh0
    sigma
    stk0
    ta
    tm
    trn0
    tva
    tva0
    x0
end)

