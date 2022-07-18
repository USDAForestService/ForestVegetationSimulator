.tables
.mode csv
PRAGMA table_info(FVS_ATRTList);
select count(distinct standid) from FVS_ATRTList;
select standid, count(*) as tree_records from FVS_ATRTList group by StandID order by StandID;

PRAGMA table_info(FVS_BurnReport);
select count(distinct standid) from FVS_BurnReport;
select standid, slope, round(midflame_wind, 2), round(flame_length, 2) from FVS_BurnReport order by standid;

PRAGMA table_info(FVS_CalibStats);
select standid, speciesfvs, treesize, numtrees, scalefactor from FVS_CalibStats order by standid, speciesfvs, treesize;

PRAGMA table_info(FVS_CanProfile);
select count(distinct standid) from FVS_CanProfile where year = 2021;
select standid, year, round(sum(canopy_fuel_lbs_acre_ft), 2) as total_weight 
    from FVS_CanProfile
    group by standid, year 
    order by standid, year;

PRAGMA table_info(FVS_Carbon);
select count(distinct standid) from FVS_Carbon;
select standid, total_stand_carbon from FVS_Carbon where year = 2014 order by standid;

PRAGMA table_info(FVS_Compute);
select standid, year, myvar from FVS_Compute order by standid, year;

PRAGMA table_info(FVS_Consumption);
select standid, year, min_soil_exp, total_consumption from FVS_Consumption order by standid, year;

PRAGMA table_info(FVS_CutList);
select standid from FVS_CutList order by standid;
select standid, year, speciesplants, round(sum(tpa), 2) as tpa from FVS_CutList 
    group by standid, year, speciesplants 
    order by standid, year, speciesplants;

PRAGMA table_info(FVS_DM_Spp_Sum);
select standid, count(distinct year) as years from FVS_DM_Spp_Sum group by standid order by standid;
select standid, year, speciesplants, mean_dmr, mean_dmi, inf_tpa, mort_tpa, inf_tpa_pct, mort_tpa_pct from FVS_DM_Spp_Sum 
    order by standid, year, speciesplants;

PRAGMA table_info(FVS_DM_Stnd_Sum);
select standid, count(distinct year) as years from FVS_DM_Stnd_Sum group by standid order by standid;
select standid, year, stnd_tpa, inf_tpa, mort_tpa, inf_tpa_pct, mort_tpa_pct from FVS_DM_Stnd_Sum 
    order by standid, year;

PRAGMA table_info(FVS_Down_Wood_Cov);
select standid, year, DWD_Cover_Total_Hard, DWD_Cover_Total_Soft
    from FVS_Down_Wood_Cov 
    where year = '2021'  
    order by standid;

PRAGMA table_info(FVS_Down_Wood_Vol);
select standid, year, DWD_Volume_Total_Hard, DWD_Volume_Total_Soft
    from FVS_Down_Wood_Vol 
    where year = '2021'  
    order by standid;

PRAGMA table_info(FVS_EconHarvestValue);

PRAGMA table_info(FVS_EconSummary);
select standid, count(year) 
    from FVS_EconSummary 
    group by standid 
    order by standid;

PRAGMA table_info(FVS_Fuels);
select standid, year, surface_total, standing_total, total_consumed 
    from FVS_Fuels 
    where total_consumed > 0 
    order by standid, year;

PRAGMA table_info(FVS_Hrv_Carbon);
select standid, year, products, landfill, energy, emissions, merch_carbon_stored 
    from FVS_Hrv_Carbon 
    where products > 0 
    order by standid, year;

PRAGMA table_info(FVS_Mortality);
select standid, year, speciesfvs, bakill 
    from FVS_Mortality 
    where bakill > 0 and speciesfvs != 'ALL'
    order by standid, year, speciesfvs;

PRAGMA table_info(FVS_PotFire);
select standid, year, tot_flame_sev, tot_flame_mod, mortality_BA_sev, mortality_BA_mod 
    from FVS_PotFire 
    where year = 2021 
    order by standid;

PRAGMA table_info(FVS_Regen_HabType);
select standid, series, habitattype, numplots 
    from FVS_Regen_HabType 
    order by standid, series, habitattype;

PRAGMA table_info(FVS_Regen_Ingrow);
select standid, year, sum(ingrowthTpa) 
    from FVS_Regen_Ingrow 
    group by standid, year 
    order by standid, year;

PRAGMA table_info(FVS_Regen_SitePrep);
select standid, yearburn, pcntmech, pcntburn from FVS_Regen_SitePrep order by standid, yearburn;

PRAGMA table_info(FVS_Regen_Sprouts);
select standid, year, speciesplants, sprttpa from FVS_Regen_Sprouts order by standid, year, speciesplants;

PRAGMA table_info(FVS_Regen_Tally);
select standid, year, speciesplants, tpaall, pctoftotalall from FVS_Regen_Tally 
    where speciesplants != 'ALL' order by standid, year, speciesplants;

PRAGMA table_info(FVS_SnagDet);
select standid, year, speciesplants, year_died, avg(total_volume), avg(density_total) 
    from FVS_SnagDet 
    group by standid, year, speciesplants, year_died 
    order by standid, year, speciesplants;

PRAGMA table_info(FVS_SnagSum);
select standid, year, hard_snags_total, soft_snags_total 
    from FVS_SnagSum order by standid, year;

PRAGMA table_info(FVS_Stats_Species);
select standid, year, speciesplants, boardfeet, cubicfeet, treesperacre, basalarea  
    from FVS_Stats_Species order by standid, year, speciesplants;

PRAGMA table_info(FVS_Stats_Stand);
select standid, year, characteristic, average, standard_dev, coeff_of_var, samp_error_percent 
    from FVS_Stats_Stand order by standid, year, characteristic;

PRAGMA table_info(FVS_StrClass);
select standid, year, removal_code, stratum_1_dbh, stratum_1_nom_ht, stratum_1_speciesfvs_1
    from FVS_StrClass order by standid, year, removal_code;

PRAGMA table_info(FVS_Summary2);
select standid, year, rmvCode, tpa, ba, sdi, ccf, qmd
    from FVS_Summary2 order by standid, year, rmvCode;

PRAGMA table_info(FVS_TreeList);
select standid, year, count(distinct speciesplants) 
    from FVS_TreeList 
    group by standid, year 
    order by standid, year;
