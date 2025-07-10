function IFPRI_mpsge(data; 
                    flexiblePFX = 1,
                    eqyield = 1,
                    fixedGS = 1
        )

    @extract_to_local_scope(data, begin
        s => S
        g => G
        h => H
        f => F
        mg => MG
        gs => GS
        a0
        c0
        cm0
        cn0
        d0
        dtax0
        esub => ESUB
        esubv => ESUBV
        eta => ETA
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
        sigma => SIGMA
        stk0
        ta => TA
        tm => TM
        trn0
        tva0
        x0
    end)
    M = MPSGEModel()


    @parameters(M, begin
        esub[s=S], ESUB[s], (description = "Elasticity of substitution between goods in sector s",) #ai description
        esubv[s=S], ESUBV[s], (description = "Elasticity of substitution between goods in sector s and value added",)
        eta[g=G], ETA[g], (description = "Elasticity of transformation between domestic and export supply of good g",)
        tva[s=S], tva0[s], (description = "Tax on value added in sector s",)
        sigma[g=G], SIGMA[g], (description = "Elasticity of substitution between Armington goods in sector g",)
        ta[g=G], TA[g], (description = "Tax on Armington supply in sector g",)
        tm[g=G], TM[g], (description = "Tax on Armington supply in sector mg",)
    end)

    @sectors(M, begin
            Y[s = S],        (description = "Sectoral production",)
            X[g = G],        (description = "Supply of goods",)
            A[g = G],        (description = "Armington supply",)
            CD[h = H],       (description = "Household consumption",)
            ID,              (description = "Investment demand",)
            GOV,             (description = "Government demand",)
    end)

    @commodities(M, begin
            PA[g = G],           (description = "Armington price",)
            PC[h = H],           (description = "Household consumption",)
            PY[g = G],           (description = "Goods output",)
            PN[s = S],           (description = "Non-market output",)
            PD[g = G],           (description = "Domestic market price",)
            PINV,            (description = "Price of investment",)
            PENT,            (description = "Return to enterprise rents",)
            PF[f = F],           (description = "Factor price",)
            PG,              (description = "Price of public goods",)
            PFX,             (description = "Foreign exchange",)
    end)

    @consumers(M, begin
            RA[h = H],           (description = "Household",)
            ENT,             (description = "Enterprises",)
            GOVT,            (description = "Government",)
            ROW,             (description = "Foreign factor holdings",)
    end)

    @auxiliaries(M, begin
            FSAV,            (description = "Foreign saving", start = 1,)
            GSAV,            (description = "Government saving", start = 1,)
            DTAX,            (description = "Direct taxes", start = 1,)
            CAPFLOW,         (description = "Foreign capital inflow", start = 1,)
            DEMAND,          (description = "Aggregate demand (level of savings and public expenditure)", start = 1,)
            TOTABS,          (description = "Total absorption", start = 1,)
    end)

    @production(M, X[g=G], [t=0, s=eta[g]], begin
        @output(PFX, x0[g], t)
        @output(PD[g], d0[g], t)
        @input(PY[g], x0[g]+ d0[g], s)
    end)

    @production(M, Y[s=S], [t=4, s=esub[s], fd=>s=esubv[s]], begin
        @output(PY[g=G], s0[s,g], t)
        @output(PN[s], sh0[s], t)
        @input(PA[g=G], id0[g,s], s)
        @input(PF[f=F], fd0[f,s], fd, reference_price=1+tva0[s], taxes=[Tax(GOVT, tva[s])])
    end)


    @production(M, A[g=G], [t=0, s=sigma[g], m=>s=0, d=>s=0], begin
        @output(PA[g], a0[g], t, taxes=[Tax(GOVT, ta[g])])
        @input(PA[mg=MG], mdd0[g], d)
        @input(PD[g], d0[g], d)
        @input(PA[mg=MG], mdm0[g], m)
        @input(PFX, m0[g], m, reference_price=pm0[g], taxes=[Tax(GOVT, tm[g])])
    end)

    @production(M, GOV, [t=0, s=0], begin
        @output(PG, sum(g0[g] for g in G), t)
        @input(PA[g=G], g0[g], s)
    end)

    @production(M, CD[h=H], [t=0, s=1, nest[g=G]=>s=4], begin
        @output(PC[h], c0[h], t)
        [@input(PN[s_element], cn0[s_element,h], nest[s_element]) for s_element in S if s_element in GS]...
        [@input(PN[s_element], cn0[s_element,h], s)               for s_element in S if s_element ∉ GS]...
        @input(PA[g=G], cm0[g,h], nest[g])
    end)

    @production(M, ID, [t=0, s=0], begin
        @output(PINV, sum(i0[g] for g in G), t)
        @input(PA[g=G], i0[g], s)
    end)


    @demand(M, RA[h=H], begin
        @final_demand(PC[h], sum(fe0[h,f] for f in F) + hp0[h] + trn0[h] - hs0[h]*DEMAND - dtax0[h]*DTAX)
        @endowment(PF[f=F], fe0[h,f])
        @endowment(PENT, hp0[h])
        @endowment(PFX, trn0[h])
        @endowment(PINV, -hs0[h]*DEMAND)
        @endowment(PG, -dtax0[h]*DTAX)
    end)

    @demand(M, ENT, begin
        @final_demand(PENT, sum(fe0["ent", f] for f in F) + trn0["ent"] - hs0["ent"]*DEMAND - dtax0["ent"]*DTAX)
        @endowment(PF[f=F], fe0["ent", f])
        @endowment(PFX, trn0["ent"])
        @endowment(PINV, -hs0["ent"]*DEMAND)
        @endowment(PG, -dtax0["ent"]*DTAX)
    end)

    @demand(M, ROW, begin
        @final_demand(PFX, sum(fe0["row", f] for f in F))
        @endowment(PF[f=F], fe0["row", f])
    end)

    @demand(M, GOVT, begin
        @final_demand(PG, 
                -sum(stk0[g] for g in G) + 
                sum(fe0["govt", f] for f in F) +
                -gsav0*GSAV +
                dtax0["govt"]*DTAX +
                -fsav0*FSAV +
                fsav0*CAPFLOW + govdef
                )
        @endowment(PA[g=G], -stk0[g])
        @endowment(PF[f=F], fe0["govt", f])
        @endowment(PINV, -gsav0*GSAV)
        @endowment(PG, dtax0["govt"]*DTAX)
        @endowment(PINV, -fsav0*FSAV)
        @endowment(PFX, fsav0*CAPFLOW + govdef)

    end)

    @aux_constraint(M, TOTABS, 
        TOTABS - sum(a0[g]*A[g] for g in G)/sum(a0[g] for g in G)
    )
            
        #(fsav0 * FSAV * PINV - (trn0("row")*PFX + sum(f, fe0("ROW",f)*PF(f))) )
    @aux_constraint(M, FSAV,
        ifelse(flexiblePFX != 0,
            fsav0 * FSAV * PINV - trn0["row"]*PFX - sum(fe0["row",f]*PF[f] for f in F),
            PFX * sum(c0[h] for h in H) - sum(PC[h]*c0[h] for h in H)
        )
    )

    @aux_constraint(M, CAPFLOW,
        CAPFLOW*PFX - FSAV*PINV
    )

    @aux_constraint(M, GSAV,
        ifelse(eqyield !=0,
            GSAV - 1,
            GOV - DEMAND
        )
    )

    @aux_constraint(M, DTAX,
        ifelse(eqyield != 0,
            GOV - DEMAND,
            DTAX - 1
        )
    )

    @aux_constraint(M, DEMAND,
        DEMAND - ifelse(fixedGS !=0, 1, TOTABS)
    )

    # ?? Bug in MPSGE?
    set_start_value(GOVT, 29.5062)

    return M

end