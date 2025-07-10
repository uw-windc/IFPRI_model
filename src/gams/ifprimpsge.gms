$title  The Standard IFPRI Model Implemented in MPSGE

*       This GAMS program illustrates the implementation and analysis of an
*       open-economy general equilibrium model based on the IFPRI standard model.
*       This is a small open economy model for Peru which is formulated as
*       a nonlinear complementarity problem using MPSGE for generation of the
*       underlying system of equations.

*       This program reads the social accounting matrix for Peru, 2004 from an
*       Excel worksheet, extracts relevant submatrices, and replicates the base
*       year equilibrium.

*       Model application is illustrated through a typical counterfactual simulation
*       in which all tariffs are set to zero.   We compute this scenario under eight
*       alternative model closures.

*       Documentation of the 2004 Peru SAM is provided in English (PeruSAM.PDF) and  
*       Spanish (PeruSAM.doc).

*       GAMS code and associated data and template files are in ifprimpsge.zip (360K).

*       A zipped copy of the resulting report file is in uft.zip (151K).

*       This work has been funded through the Inter-American Development Bank
*       program Foreign Trade Policies Development Program", loan contract 
*       No. 1442/OC-PE under the task, "Development of a set of computable
*       general equilibrium models to analyze economic, trade and social
*       policies".  This work has been conduct in collaboration with the
*       Office of Economic Research, Vice Ministry of Foreign Trade. Expert
*       assistance has been provided by Gabriela Cuadra and Claudia Sanchez.
*       Errors remain my own.

*       Thomas F. Rutherford
*       December, 2006

*       Uncomment the following statement to generate the unilateral
*       free trade simultion:

* $set uftsim 

set     r       Set of SAM rows  /1*61/;

scalar  

*       The following three parameter determine the model "closure", reflecting
*       alternative assumptions about the macroeconomic environment:

        flexiblePFX     Logical flag (1=flexible exchange rate) /1/,

*       flexiblePFX determines the "Rest of World" constraint in the
*       standard IFPRI model.  When flexiblePFX=1, we assume that foreign
*       savings in Peru are fixed and the real exchange rate is flexibl.
*       When flexiblePFX=0,  we assume that foreign savings in Peru
*       adjust endogenously so that the real exchange rate remains fixed.

        eqyield         Logical flag (1=tax replacement)        /1/,

*       eqyield determines the "Government" constraint in the standard
*       IFPRI model.  When eqyield=1, we assume that government savings
*       are fixed and direct tax rates on households and enterprises
*       are adjusted to balance the public budget.  When eqyield=0, we
*       assume that direct tax rates are fixed and government savings
*       adjusts proportionally to private savings and investment.

        fixedGS         Logical flag (1=fixed investment and public demand) /1/;

*       fixedGS determines the "Savings-Investment" constraint in the 
*       standard IFPRI model.  When "fixedGS=1" we assume fixed private savings 
*       and investment.  When fixedGS=0, we assume that savings (and investment)
*       adjusts in proportion to total absorption.

        
*       SAM rows and columns are the same set:

alias (r,c);

*       Pull the social accounting data from the source Excel file:

parameter       sam(r,c)        Social accounts;
$if not exist ../../data/sam2004.gdx $call 'gdxxrw i=../../data/SAM2004.xls o=../../data/sam2004.gdx par=sam rdim=1 cdim=1 rng=MICROSAM!G12'
$gdxin ../../data/sam2004.gdx
$load sam

*       Computation works best when numbers are around unity (one), so we scale the SAM data accordingly:

sam(r,c) = sam(r,c)/1e9;


set     rc(r)   Rows in the SAM to be checked /1*60/;

parameter       chksam  Cross check on totals;
chksam(r,"row") = sum(rc(c), sam(r,c));
chksam(r,"col61") = sam(r,"61");
chksam(r,"col") = sum(rc(c), sam(c,r));
chksam(r,"row61") = sam("61",r);
chksam(r,"diff") = chksam(r,"row") - chksam(r,"col");
chksam(r,"diff%")$chksam(r,"row") = chksam(r,"diff")/chksam(r,"row");
display chksam;

set f Factores de produccion /
        urbusk  "Urbano Asalariado no calificado",
        urbskl  "Urbano Asalariado calificado",
        urbsusk "Urbano Independiente no calificado",
        urbsskl "Urbano Independiente calificado",
        rurusk  "Rural no calificado",
        rurskl  "Rural calificado",
        cap     "Capital"/
alias (f,ff);

set h Hogares /
        costau          "Hogar costa urbana",    
        sierrau         "Hogar sierra urbana",   
        selvau          "Hogar selva urbana",    
        lima            "Hogar Lima metropolitana",
        costar          "Hogar costa rural",     
        sierrar         "Hogar sierra rural",    
        selvar          "Hogar selva rural"/;
alias (h,hh);

set g   Actividades y Bienes /
        agr     "Agropecuaria, caza, silvicultura",
        fsh     "Pesca, preservacion, harina y aceite de pescado",
        min     "Extraccion de minerales, extraccion de petroleo",
        foo     "Bebidas y tabaco, lacteos y otros productos alimenticios, molineria y panaderia, azucar",
        txt     "Productos textiles, prendas de vestir, cuero y articulos de cuero, calzado",
        mac     "Maquinaria y equipo, material de transporte, muebles de madera y metal, productos de metal, caucho y plastico y otros manufacturados",
        chm     "Productos quimicos basicos, abonos, y productos farmaceuticos, productos de papel, impresion y edicion",
        con     "Construccion",
        com     "Comercio",
        trn     "Transportes y comunicaciones",
        hea     "Salud",
        edu     "Educacion",
        gov     "Servicios gubernamentales",
        srv     "Servicios financieros y de seguros, servicios a empresas, servicios mercantes a hogares",
        htl     "Restaurantes y hoteles, servicios no mercantes a hogares, alquiler de vivienda"/;
        
alias (g,s,gg,ss);

set     mapr(r,*)       Mapping from SAM accounts to labels /

*       Factores de produccion

        1.urbusk        "Urbano Asalariado no calificado",
        2.urbskl        "Urbano Asalariado calificado",
        3.urbsusk       "Urbano Independiente no calificado",
        4.urbsskl       "Urbano Independiente calificado",
        5.rurusk        "Rural no calificado",
        6.rurskl        "Rural calificado",
        7.cap           "Capital"

*       Instituciones

        8.costau        "Hogar costa urbana",
        9.costar        "Hogar costa rural",
        10.sierrau      "Hogar sierra urbana",
        11.sierrar      "Hogar sierra rural",
        12.selvau       "Hogar selva urbana",
        13.selvar       "Hogar selva rural",
        14.lima         "Hogar Lima metropolitana",
        15.ent          "Empresas (enterprises)",
        16.gvt          "Gobierno general (central government)",
        17.edu          "Educacion (education)",
        18.hea          "Salud (health)",
        19.trn          "Asistencia alimentaria (transportation)",

        20.soc          "Gasto social (social costs)",
        21.pub          "Capital publico (public capital)",
        22.cca          "Cuenta de capitales combinada (combined capital account)",
        23.inv          "Variacion de existencias (change in stock)",

*       Actividades             

        24.agr          "Agropecuaria, caza, silvicultura",
        25.fsh          "Pesca, preservacion, harina y aceite de pescado",
        26.min          "Extraccion de minerales, extraccion de petroleo",
        27.foo          "Bebidas y tabaco, lacteos y otros productos alimenticios, molineria y panaderia, azucar",
        28.txt          "Productos textiles, prendas de vestir, cuero y articulos de cuero, calzado",
        29.mac          "Maquinaria y equipo, material de transporte etc.",
        30.chm          "Productos quimicos basicos, abonos, y productos farmaceuticos etc.",
        31.con          "Construccion",
        32.com          "Comercio",
        33.trn          "Transportes y comunicaciones",
        34.hea          "Salud",
        35.edu          "Educacion",
        36.gov          "Servicios gubernamentales",
        37.srv          "Servicios financieros y de seguros, servicios a empresas, servicios mercantes a hogares",
        38.htl          "Restaurantes y hoteles, servicios no mercantes a hogares, alquiler de vivienda",

*       Bienes          

        39.agr          "Agropecuaria, caza, silvicultura",
        40.fsh          "Pesca, preservacion, harina y aceite de pescado",
        41.min          "Extraccion de minerales, extraccion de petroleo",
        42.foo          "Bebidas y tabaco, lacteos y otros productos alimenticios, molineria y panaderia, azucar",
        43.txt          "Productos textiles, prendas de vestir, cuero y articulos de cuero, calzado",
        44.mac          "Maquinaria y equipo, material de transporte etc.",
        45.chm          "Productos quimicos basicos, abonos, y productos farmaceuticos etc.",
        46.con          "Construccion",
        47.com          "Comercio",
        48.trn          "Transportes y comunicaciones",
        49.hea          "Salud",
        50.edu          "Educacion",
        51.gov          "Servicios gubernamentales",
        52.srv          "Servicios financieros y de seguros, servicios a empresas, servicios mercantes a hogares",
        53.htl          "Restaurantes y hoteles, servicios no mercantes a hogares, alquiler de vivienda",

*       Comercializacion

        54.dom          "Bienes domesticos",
        55.imp          "Bienes importados",

        56.dtax         "Ingresos por impuestos directos        (direct taxes)",
        57.itax         "Ingresos por impuestos indirectos (indirect taxes)",
        58.tar          "Derechos de importacion (tariffs)",
        59.row          "Cuenta combinada del resto del mundo (current account)",
        60.ext          "Deuda externa (external debt)"/;

set bienes(r)/39*53/, actividades(c)/24*38/, factores(r)/1*7/, hogares(r)/8*14/;

*       Display numerical values from select SAM accounts:

parameter       firms0  Activities of enterprises;
alias (u,*);
loop(mapr(r,u),
        firms0(u,"payment") = sam(r,"15");
        firms0(u,"receipt") = sam("15",r);
);
display firms0;


parameter       capital0        Elements of the combined capital account;
alias (u,*);
loop(mapr(r,u)$(not actividades(r)),
        capital0(u,"payment") = sam(r,"22");
        capital0(u,"receipt") = sam("22",r);
);
display capital0;

parameter       rent0   Elements of capital earnings;
alias (u,*);
loop(mapr(r,u)$(not bienes(r)),
        rent0(u,"payment") = sam(r,"7");
        rent0(u,"receipt") = sam("7",r);
);
display rent0;

parameter       direct0 Direct tax payments;
loop(mapr(r,u)$(not actividades(r)),
        direct0(u,"payment") = sam(r,"56");
        direct0(u,"receipt") = sam("56",r);
);
display direct0;

*       Decleare submatrices which collectively constitute the 
*       social accounting matrix:

parameters
        sh0(s)          Sectoral output supplied directly to households
        s0(s,g)         Sectoral supply of goods
        gd0(g)          Government demand
        gs0(g)          Government social demand
        gi0(g)          Demand for goods into government investment
        i0(g)           Private investment demand
        stk0(g)         Stock and inventory demand
        msd0(g)         Margin supply for domestic market
        msm0(g)         Margin supply for import market
        x0(g)           Exports
        d0(g)           Domestic supply
        y0(s)           Aggregate output
        ym0(g)          Marketed output
        m0(g)           Imports (cif)
        tm0(g)          Import tariffs
        ta0(g)          Indirect taxes
        mdd0(g)         Margins (on domestic supply)
        mdm0(g)         Margins (on imported supply)
        cn0(g,h)        Household demand for non-market sectoral output,
        cm0(g,h)        Household demand for market output,
        id0(g,s)        Intermediate demand
        fd0(f,s)        Factor demand
        fe0(*,f)        Factor endowments
        hs0(*)          Household and enterprise saving
        hp0(h)          Household payments by enterprises
        es0             Enterprise saving
        a0(g)           Armington supply (aggregate demand)
        d0(g)           Domestic supply
        dtax0(*)        Direct taxes;

*       Extract these submatrices, setting elements of the SAM to zero
*       as data are extracted:

loop((g,s,r,c)$(bienes(r) and actividades(c) and mapr(r,g) and mapr(c,s)),
        id0(g,s) = sam(r,c); sam(r,c) = 0;
);

loop((f,s,r,c)$(factores(r) and actividades(c) and mapr(r,f) and mapr(c,s)),
        fd0(f,s) = sam(r,c); sam(r,c) = 0;
);
loop((h,f,r,c)$(hogares(r) and factores(c) and mapr(r,h) and mapr(c,f)),
        fe0(h,f) = sam(r,c); sam(r,c) = 0;
);
loop((f,r)$(factores(r) and mapr(r,f)), 
        fe0("row",f) = -sam(r,"59"); sam(r,"59") = 0;
);

loop((s,h,r,c)$(actividades(r) and hogares(c) and mapr(r,s) and mapr(c,h)),
        cn0(s,h) = sam(r,c); sam(r,c) = 0;
);
sh0(s) = sum(h, cn0(s,h));
loop((s,g,r,c)$(actividades(r) and bienes(c) and mapr(r,s) and mapr(c,g)),
        s0(s,g) = sam(r,c); sam(r,c) = 0;
);
loop((g,h,r,c)$(bienes(r) and hogares(c) and mapr(r,g) and mapr(c,h)),
        cm0(g,h) = sam(r,c); sam(r,c) = 0;
);
loop((g,r)$(bienes(r) and mapr(r,g)),
        gd0(g) = sam(r,"16"); sam(r,"16") = 0;
        gi0(g) = sam(r,"21"); sam(r,"21") = 0;
        gs0(g) = sam(r,"20"); sam(r,"20") = 0;
        i0(g) = sam(r,"22"); sam(r,"22") = 0;
        stk0(g) = sam(r,"23"); sam(r,"23") = 0;
        msd0(g) = sam(r,"54"); sam(r,"54") = 0;
        msm0(g) = sam(r,"55"); sam(r,"55") = 0;
        x0(g) = sam(r,"59"); sam(r,"59") = 0;
        m0(g) = sam("59",r); sam("59",r) = 0;
        tm0(g) = sam("58",r); sam("58",r) = 0;
        ta0(g) = sam("57",r); sam("57",r) = 0;
        mdd0(g) = sam("54",r); sam("54",r) = 0;
        mdm0(g) = sam("55",r); sam("55",r) = 0;
);

*       Display the SAM elements which have not been extracted:

display "Unextracted SAM data:", sam;

*       We have some tariff revenue for the SRV sector which
*       has no imports.  Recategorize these data as indirect taxes:

ta0(g)$(m0(g) = 0) = ta0(g) + tm0(g);
tm0(g)$(m0(g) = 0) = 0;

*       Domestic supply equals production less exports:

d0(g) = sum(s,s0(s,g)) - x0(g);

*       Aggregate domestic absorption:

a0(g) = gd0(g) + gi0(g) + gs0(g) + i0(g) + stk0(g) + msd0(g) + msm0(g) + sum(s,id0(g,s)) + sum(h,cm0(g,h));

parameter       profit(s,*)     Profit check for production and aggregate supply;

profit(s,"y") = sh0(s) + sum(g, s0(s,g)) - sum(g, id0(g,s)) - sum(f, fd0(f,s));
profit(g,"a") = a0(g) - m0(g) - d0(g) - tm0(g) - ta0(g) - mdd0(g) -mdm0(g);
display profit;

parameter       market  Market clearance;

$eolcom !

market(g,"s0") = sum(s,s0(s,g));        ! Sectoral supply
market(g,"x0") = x0(g);                 ! Exports
market(g,"m0") = m0(g);                 ! Imports (cif)
market(g,"tm0") = tm0(g);               ! Tariff revenue
market(g,"md") = mdd0(g)+mdm0(g);       ! Margin demand (domestic and imported goods)
market(g,"ms") = msd0(g)+msm0(g);       ! Margin supply
market(g,"ta0") = ta0(g);               ! Indirect taxes
market(g,"id0") = sum(s, id0(g,s));     ! Intermediate demand
market(g,"cm0") = sum(h, cm0(g,h));     ! Consumer demand (market)
market(g,"i0") = i0(g);                 ! Investment
market(g,"gd0") = gd0(g);               ! Government demand
market(g,"gs0") = gs0(g);               ! Government social demand
market(g,"gi0") = gi0(g);               ! Government investment demand
market(g,"stk0") = stk0(g);             ! Changes in stock

*       Generate a cross-check of consistency:

market(g,"chk") = sum(s,s0(s,g))-x0(g) + m0(g) + tm0(g) + ta0(g) + mdd0(g)+mdm0(g)
        - sum(h, cm0(g,h)) - (msd0(g)+msm0(g)) - sum(s, id0(g,s)) - i0(g) - (gd0(g)+gs0(g)+gi0(g)) - stk0(g);

*       Report the totals for each of these accounts:

set acct /s0,m0,x0,tm0,md,ms,ta0,id0,cm0,i0,gs0,gd0,gi0,stk0/;
market("total",acct) = sum(g,market(g,acct));
display market;

parameter       g0(g)   Aggregate public demand;
g0(g) = gd0(g)+gs0(g)+gi0(g);

*       Enterprise capital earnings:

fe0("ent","cap") = rent0("ent","payment");

*       Household receipts from firms:

hp0(h) = firms0(h,"payment");

*       Household savings:

hs0(h) = capital0(h,"receipt");

*       Enterprise savings:

hs0("ent") = capital0("ent","receipt");

*       Marketed output by commodity:

ym0(g) = sum(s, s0(s,g));

*       Sectoral output (market and non-market):

y0(s) = sum(g,s0(s,g)) + sh0(s);

parameter       esub(s)         Elasticity of substitution
                esubv(s)        Elasticity of substitution (value-added)
                govdef          Government deficit
                eta(g)          Elasticity of transformation
                sigma(g)        Armington elasticity
                trn0(*)         Net transfers 
                gsav0           Government saving
                fsav0           Foreign saving
                c0(h)           Consumption demand
                pm0(g)          Reference import price
                tm(g)           Import tariff rate
                ta(g)           Indirect tax rate
                tva(s)          Indirect tax levied on production
                tva0(s)         Base year rate;

*       Define some default elasticities:

esub(s) = 0;
esubv(s) = 1;
eta(g)  = 5;
sigma(g) = 5;

*       Foreign savings:

fsav0 = capital0("row","receipt");

*       Government savings is defined as a residual:

gsav0 = sum(g,i0(g)) - fsav0 - sum(h, hs0(h)) - hs0("ent");
display gsav0;



*       No sectoral value-added taxes in this dataset:

tva(s) = 0;
tva0(s) = 0;

*       Scale indirect taxes based on tax revenue and tax base:

ta(g)$ta0(g) = ta0(g)/a0(g);
tm(g)$tm0(g) = tm0(g)/m0(g);

*       Direct tax payments and receipts:

dtax0(h) = direct0(h,"receipt");
dtax0("ent") = direct0("ent","receipt");
dtax0("govt") = dtax0("ent") + sum(h,dtax0(h));
display dtax0;



*       Household consumption of market and non-market goods:

c0(h) = sum(g,cn0(g,h)+cm0(g,h));

*       Transfers are defined implicitly:

trn0(h) = c0(h) + hs0(h) + dtax0(h) - sum(f, fe0(h,f)) - hp0(h);
trn0("ent") = sum(h,hp0(h)) + dtax0("ent") + hs0("ent") - sum(f, fe0("ent",f));
trn0("row") = fsav0 - sum(f, fe0("row",f));

*       Government factor endowments are likewise defined implicitly:

fe0("govt",f) = round(sum(s, fd0(f,s)) - sum(h, fe0(h,f)) - fe0("ent",f) - fe0("row",f),6);

*       Government deficit:

govdef = sum(g,g0(g)+stk0(g))+gsav0-sum(g,m0(g)*tm(g)+a0(g)*ta(g))- dtax0("govt")-sum(f, fe0("govt",f));

set             mg(g)           Margin commodity;
mg(g) = yes$(msd0(g)+msm0(g));
display mg;


abort$(card(mg)>1) "Dataset must have a single margin commodity.";
pm0(g) = 1 + tm(g);

set             gs(*)   Sectors which have associated goods;
gs(g) = yes;

*       Produce an echo-print of benchmark values:

parameter       sstats          Base year sectoral statistics
                mstats          Market statistics:
                gstats          Government budget statistics;

sstats(s,"nmkt%") = sh0(s)/(sh0(s)+sum(g,s0(s,g)));
sstats(s,"export%") = 100 * sum(g,s0(s,g)*x0(g)/ym0(g)) / (sh0(s)+sum(g,s0(s,g)));
sstats(s,"id%") = 100 * sum(g,id0(g,s))/(sh0(s)+sum(g,s0(s,g)));
sstats(s,f)$fd0(f,s) = 100 * fd0(f,s)/sum(ff,fd0(ff,s));
sstats(s,"tva%") = 100 * tva(s);
option sstats:1;
display sstats,
        "nmkt%          Non-market supply as percentage of sectoral output",
        "export%        Percentage of sectoral output which is exported",
        "id%            Intermediate demand as percentage of sectoral output",
        "K,L            Factor share of value-added",
        "tva%           Value-added tax rate";


mstats(g,"export%") = 100 * x0(g)/ym0(g);
mstats(g,"import%") = 100 * (m0(g)*pm0(g)+mdm0(g))/(a0(g)*(1-ta(g)));
mstats(g,"ta%") = 100 * ta(g);
mstats(g,"tm%") = 100 * tm(g);
mstats(g,"mdd%")$(mdd0(g)+d0(g)) = mdd0(g)/(mdd0(g)+d0(g));
mstats(g,"mdm%")$(mdm0(g)+m0(g)*pm0(g)) = mdm0(g)/(mdm0(g)+m0(g)*pm0(g));
mstats(g,"c%") = 100 * sum(h,cm0(g,h)) / sum((gg,h),cm0(gg,h));
mstats(g,"x%") = 100 * x0(g)/sum(gg,x0(gg));
mstats(g,"m%") = 100 * m0(g)/sum(gg,m0(gg));
mstats(g,"m/c%")$sum(h,cm0(g,h)+cn0(g,h)) = 100 * sum(h,cm0(g,h))/sum(h,cm0(g,h)+cn0(g,h));
option mstats:1;
display mstats,
        "export%        Percentage of domestic supply which is exported",
        "import%        Percentage of domestic aborption which is imported",
        "ta%            Commodity tax rate",
        "tm%            Import tariff rate",
        "mdd%           Distribution margin on domestic goods",
        "mdm%           Distribution margin on imported goods",
        "c%             Commodity share of household market demand",
        "x%             Commodity share of total exports",
        "m%             Commodity share of total imports",
        "m/c%           Market share of final demand including non-market supply";


parameter       gincome Components of government income;
gincome("ent","dtax") = dtax0("ent");
gincome(h,"dtax") = dtax0(h);
gincome(g,"ta") = ta(g)*a0(g);
gincome(s,"tva") = tva(s)*sum(f,fd0(f,s));
gincome(g,"tm") = tm(g)*m0(g);
gincome(f,"fendow") = fe0("govt",f);
gincome(g,"stock") = stk0(g);
gincome("row","def") = govdef + sum(f,fe0("row",f));
display gincome;

$ontext
$MODEL:IFPRI

$SECTORS:
        Y(s)$y0(s)      ! Sectoral production
        X(g)            ! Supply of goods
        A(g)            ! Armington supply
        CD(h)           ! Household consumption
        ID              ! Investment demand
        GOV             ! Government demand

$COMMODITIES:
        PA(g)           ! Armington price
        PC(h)           ! Household consumption
        PY(g)           ! Goods output
        PN(s)$sh0(s)    ! Non-market output
        PD(g)$d0(g)     ! Domestic market price
        PINV            ! Price of investment
        PENT            ! Return to enterprise rents
        PF(f)           ! Factor price
        PG              ! Price of public goods
        PFX             ! Foreign exchange

$CONSUMERS:
        RA(h)           ! Household
        ENT             ! Enterprises
        GOVT            ! Government
        ROW             ! Foreign factor holdings

$AUXILIARY:
        FSAV            ! Foreign saving
        GSAV            ! Government saving
        DTAX            ! Direct taxes
        CAPFLOW         ! Foreign capital inflow
        DEMAND          ! Aggregate demand (level of savings and public expenditure)
        TOTABS          ! Total absorption

*       Commodity supply to domestic and export markets governed by a constant
*       elasticity of transformation supply function:

$prod:X(g)  eta(g)
        o:PFX           q:x0(g)         
        o:PD(g)         q:d0(g)         
        i:PY(g)         q:(x0(g)+d0(g))
        

*       Sectoral production combines primary factors and intermediate inputs
*       to produce goods and services which are both marketed and distributed
*       directly to households.  Market and non-market output are allocated
*       according to a constant elasticity of transformation function.
*       A nested constant elasticity of substitution cost function characterizes
*       the trade-off between intermediate inputs and primary factor inputs:

$prod:Y(s)$y0(s)  t:4 s:esub(s)  fd:esubv(s)
        o:PY(g)         q:s0(s,g)
        o:PN(s)         q:sh0(s)        
        i:PA(g)         q:id0(g,s)
        i:PF(f)         q:fd0(f,s)  p:(1+tva0(s))       a:govt  t:tva(s)   fd:

*       Supply of domestic and imported goods involves collection of import
*       tariffs, the application of distribution margins and the collection
*       of other indirect taxes:

$prod:A(g)  s:sigma(g)  m:0 d:0
        o:PA(g)         q:a0(g)         a:GOVT  t:ta(g)
        i:PA(mg)        q:mdd0(g)       d:
        i:PD(g)         q:d0(g)         d:
        i:PA(mg)        q:mdm0(g)       m:
        i:PFX           q:m0(g)         m: p:pm0(g) a:GOVT  t:tm(g)

*       Generate report variables corresponding to exports and imports:

$report:
        v:EXPT(g)$x0(g) o:PFX   prod:X(g)
        v:IMPT(g)$m0(g) i:PFX   prod:A(g)

*       Government demand (corresponding to current public expenditures,
*       social expenditures and public investment):

$prod:GOV  s:0
        o:PG            q:(sum(g,g0(g)))
        i:PA(g)         q:g0(g)

*       Household demand for market and non-market goods and services:

$prod:CD(h) s:1  g.tl:4
        o:PC(h)         q:c0(h)
        i:PN(s)         q:cn0(s,h) s.tl:$gs(s)
        i:PA(g)         q:cm0(g,h) g.tl:

*       Private investment:

$prod:ID  s:0
        o:PINV          q:(sum(g,i0(g)))
        i:PA(g)         q:i0(g)


*       Household income and expenditure:

$demand:RA(h)  
        d:PC(h)
        e:PF(f)         Q:fe0(h,f)                      ! Factor income
        e:PENT          Q:hp0(h)                        ! Household claims on enterprise profits
        e:PFX           Q:trn0(h)                       ! Net transfers 
        e:PINV          Q:(-hs0(h))     R:DEMAND        ! Private savings (proportional to DEMAND)
        e:PG            Q:(-dtax0(h))   R:DTAX          ! Direct tax payments (proportional to DTAX)

*       Enterprises:

$demand:ENT
        d:PENT                                          ! Net income is distributed through the PENT market
        e:PF(f)         Q:fe0("ent",f)                  ! Factor income
        e:PFX           Q:trn0("ent")                   ! Net transfers
        e:PINV          Q:(-hs0("ent")) R:DEMAND        ! Enterprise savings (proportional to DEMAND)
        e:PG            Q:(-dtax0("ent")) R:DTAX        ! Direct tax payments (proportional to DTAX)

*       Foreign workers:

$demand:ROW
        d:PFX                                           ! Assume full remittance of earnings.
        e:PF(f)         Q:fe0("row",f)                  ! Factor income

$demand:GOVT
        d:PG                                            ! Demand for public goods and services
        e:PA(g)         Q:(-stk0(g))                    ! Stock
        e:PF(f)         Q:fe0("govt",f)                 ! Factor income
        e:PINV          Q:(-gsav0)      R:GSAV          ! Government savings (proportional to GSAV)
        e:PG            Q:dtax0("govt") R:DTAX          ! Income from direct tax payments (proportional to DTAX)
        e:PINV          Q:(-fsav0)      R:FSAV          ! Investment demand from foreign savings (proportional to FSAV)
        e:PFX           Q:fsav0         R:CAPFLOW       ! Current account inflows (proportional to FSAV)
        e:PFX           Q:govdef                        ! Government deficit is fixed
        
*       Define an index of total absorption based on value of 
*       market supply at base year prices:

$constraint:TOTABS
        TOTABS =E= sum(g, a0(g)*A(g)) / sum(g,a0(g));

*       Index of the level of foreign savings:

$constraint:FSAV

*       If the exchange rate is flexible, then foreign savings are equal to the fixed level
*       of transfers plus the level of foreign factor earnings:

        (fsav0 * FSAV * PINV - (trn0("row")*PFX + sum(f, fe0("ROW",f)*PF(f))) )$(flexiblePFX) +

*       If the exchange rates is fixed, FSAV is determined as an equilibrium variable which
*       assures that the real exchange rate (defined here as a basket of consumer price indices)
*       is fixed exogenously:

        (PFX * sum(h, c0(h)) - sum(h, PC(h)*c0(h)) )$(not flexiblePFX)  =e= 0;

*       Assume that any change in foreign savings is financed through capital flows:

$constraint:CAPFLOW
        CAPFLOW*PFX =E= FSAV*PINV;

*       Index of the level of government saving:

$constraint:GSAV
        (GOV   - DEMAND)$(not eqyield) + 
        (GSAV  -      1)$eqyield =e= 0; 

*       Index of the level of direct taxes:

$constraint:DTAX
        (GOV   - DEMAND)$eqyield +
        (DTAX  -      1)$(not eqyield) =e= 0;

*       Aggregate investment and public demand are either equal to the index
*       of total absorption or they remain fixed at the base year level:

$constraint:DEMAND
        DEMAND =e= TOTABS$(not fixedGS) + 1$fixedGS;

$OFFTEXT
$SYSINCLUDE MPSGESET IFPRI

GSAV.LO = -INF;
DTAX.LO = -INF;
DEMAND.LO = -INF;
FSAV.LO = -INF;
CAPFLOW.LO = -INF;

CAPFLOW.L = 1;
FSAV.L = 1;
GSAV.L = 1;
DTAX.L = 1;
DEMAND.L = 1;
TOTABS.L = 1;




IFPRI.iterlim = 0;
$include IFPRI.gen
solve IFPRI using mcp;
IFPRI.iterlim = 2000;

$exit


$if not set uftsim $exit

*       Evaluate the impact of tariff elimination:

tm(g) = 0;

set             pfxcl   Exchange rate regime /"Fixed ER", "Flexible ER"/,  
                invcl   Investment closure /"I Fixed", "I=TotAbs"/,
                govcl   Government closure /"G Fixed", "G=TotAbs"/;

parameter       consum                          Real consumption level (% change)
                gdp0                            Benchmark gdp,
                cpi                             Consumer price index
                gdp                             Real GDP (% change)
                output                          Sectoral output (% change),
                fprice                          Factor price effectgs (% change),
                cprice                          Household consumption prices (% change),
                gprice                          Commodity prices (% change)
                exports                         Commodity exports,
                imports                         Commodity imports,
                decomp(pfxcl,govcl,invcl,*,*)   Decomposition of static welfare effects;

*       Base year GDP equals: C + I + G + X - M

gdp0 = sum(h, c0(h)) + sum(g,g0(g)) + sum(g,i0(g)) + sum(f,fe0("row",f))
        - (sum(h, trn0(h)) + trn0("ent") + govdef + fsav0);

eqyield = 1;

*       Consider all possible closures:



loop((pfxcl,invcl,govcl),

        flexiblePFX = 1$sameas(pfxcl,"Flexible ER");
        fixedGS     = 1$sameas(invcl,"I Fixed");
        eqyield     = 1$sameas(govcl,"G Fixed");

$include IFPRI.gen
        solve IFPRI using mcp;

*       Percentage change in consumption level for household h: 

        consum(h,pfxcl,govcl,invcl) = 100 * (CD.L(h)-1);

*       Consumer price index based on value-weight consumer price indices:

        cpi = sum(h, PC.L(h)*c0(h))/sum(h,c0(h));

*       Compute percentage change in real GDP:

        gdp(pfxcl,govcl,invcl) = 100 * (
                (( sum(h, PC.L(h)*CD.L(h)*c0(h)) + PG.L*GOV.L*(sum(g,g0(g))) + PINV.L*ID.L*sum(g,i0(g)) 
                + ROW.L - PFX.L * (sum(h, trn0(h)) + trn0("ent") + govdef + CAPFLOW.L*fsav0) ) / cpi)/gdp0-1);

*       Report percentage change in output, exports, imports, factor prices, and commodity prices:

        output(s,pfxcl,govcl,invcl,"%")    = 100 * (Y.L(s)-1);
        output(s,pfxcl,govcl,invcl,"value")  = (x0(s)+d0(s)) * (Y.L(s)-1);
        exports(g,pfxcl,govcl,invcl,"%")$x0(g) = 100 * (EXPT.L(g)/x0(g)-1);
        exports(g,pfxcl,govcl,invcl,"value")$x0(g) = EXPT.L(g)-x0(g);
        imports(g,pfxcl,govcl,invcl,"%")$m0(g) = 100 * (IMPT.L(g)/m0(g)-1);
        imports(g,pfxcl,govcl,invcl,"value")$m0(g) = IMPT.L(g)-m0(g);
        fprice(f,pfxcl,govcl,invcl)        = 100 * (PF.L(f)/cpi-1);

        cprice(h,pfxcl,govcl,invcl)        = 100 * (PC.L(h)/cpi-1);
        cprice("PFX",pfxcl,govcl,invcl)    = 100 * (PFX.L/cpi-1);

        gprice(g,pfxcl,govcl,invcl,"pd")$d0(g) = 100 * (PD.L(g)/cpi-1);
        gprice(g,pfxcl,govcl,invcl,"pa")$a0(g) = 100 * (PA.L(g)/cpi-1);

*       Produce a first-order decomposition of welfare impacts:

        decomp(pfxcl,govcl,invcl,h,f)        = 100 * (PF.L(f)/cpi-1) * fe0(h,f) / c0(h);
        decomp(pfxcl,govcl,invcl,h,"trn")    = 100 * (PFX.L/cpi-1) * trn0(h)/c0(h);
        decomp(pfxcl,govcl,invcl,h,"dtax")   = 100 * (1-PG.L*DTAX.L/cpi) * dtax0(h)/c0(h);
        decomp(pfxcl,govcl,invcl,h,"saving") = 100 * (1-PINV.L/cpi*DEMAND.L) * hs0(h)/c0(h);
        decomp(pfxcl,govcl,invcl,h,"profit") = 100 * (PENT.L/cpi-1)*hp0(h)/c0(h);
        decomp(pfxcl,govcl,invcl,h,"pc")     = 100 * sum(g,cm0(g,h)*(1-PA.L(g)/cpi)+cn0(g,h)*(1-PN.L(g)/cpi))/c0(h);

        decomp(pfxcl,govcl,invcl,h,"EV") = 100 * (CD.L(h)-1 );
        decomp(pfxcl,govcl,invcl,h,"EV$") = (CD.L(h)-1 )*c0(h);
        decomp(pfxcl,govcl,invcl,"total","EV") = 100 * (sum(h,c0(h)*CD.L(h))/sum(h,c0(h))-1 );
        decomp(pfxcl,govcl,invcl,"total","EV$") = sum(h,(CD.L(h)-1)*c0(h));

        decomp(pfxcl,govcl,invcl,h,"checksum") = 100 * (CD.L(h)-1)
                - sum(f,decomp(pfxcl,govcl,invcl,h,f))
                - decomp(pfxcl,govcl,invcl,h,"trn")
                - decomp(pfxcl,govcl,invcl,h,"dtax")
                - decomp(pfxcl,govcl,invcl,h,"saving")
                - decomp(pfxcl,govcl,invcl,h,"profit")
                - decomp(pfxcl,govcl,invcl,h,"pc");
);
option consum:1:1:2;
display gdp, consum;
option decomp:3:1:1;
display decomp;

$label pivotdata

$onecho >gdxxrw.rsp
par=decomp  rng="decomp!a2" cdim=0 
par=output  rng="output!a2" cdim=0 
par=exports rng="exports!a2" cdim=0
par=imports rng="imports!a2" cdim=0
par=fprice  rng="fprice!a2" cdim=0
par=cprice  rng="cprice!a2" cdim=0
par=gprice  rng="gprice!a2" cdim=0
$offecho
execute_unload 'pivotdata.gdx',decomp,output,exports,imports,fprice,cprice,gprice;

execute 'copy uft_template.xls uft.xls';
execute 'gdxxrw i=pivotdata.gdx o=uft.xls @gdxxrw.rsp';
