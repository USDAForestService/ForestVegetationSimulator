.tables
.mode csv
select 'Table: FVS_ATRTList' as '';
PRAGMA table_info(FVS_ATRTList);
select count(distinct standid) from FVS_ATRTList;
select standid, count(*) as tree_records from FVS_ATRTList group by StandID order by StandID;

select 'Table: FVS_BurnReport' as '';
PRAGMA table_info(FVS_BurnReport);
select count(distinct standid) from FVS_BurnReport;
select standid, slope, round(midflame_wind, 2), round(flame_length, 2) from FVS_BurnReport order by standid;

select 'Table: FVS_CalibStats' as '';
PRAGMA table_info(FVS_CalibStats);
select standid, speciesfvs, treesize, numtrees, round(scalefactor,2) from FVS_CalibStats order by standid, speciesfvs, treesize;

select 'Table: FVS_CanProfile' as '';
PRAGMA table_info(FVS_CanProfile);
select count(distinct standid) from FVS_CanProfile where year = 2021;
select standid, year, round(sum(canopy_fuel_lbs_acre_ft), 2) as total_weight 
    from FVS_CanProfile
    group by standid, year 
    order by standid, year;

select 'Table: FVS_Carbon' as '';
PRAGMA table_info(FVS_Carbon);
select count(distinct standid) from FVS_Carbon;
select standid, round(total_stand_carbon,2) from FVS_Carbon where year = 2014 order by standid;

select 'Table: FVS_Compute' as '';
PRAGMA table_info(FVS_Compute);
select standid, year, round(myvar,2) from FVS_Compute order by standid, year;

select 'Table: FVS_Consumption' as '';
PRAGMA table_info(FVS_Consumption);
select standid, year, round(min_soil_exp,2), round(total_consumption,2) from FVS_Consumption order by standid, year;

select 'Table: FVS_CutList' as '';
PRAGMA table_info(FVS_CutList);
select standid from FVS_CutList order by standid;
select standid, year, speciesplants, round(sum(tpa), 2) as tpa from FVS_CutList 
    group by standid, year, speciesplants 
    order by standid, year, speciesplants;

select 'Table: FVS_DM_Spp_Sum' as '';
PRAGMA table_info(FVS_DM_Spp_Sum);
select standid, count(distinct year) as years from FVS_DM_Spp_Sum group by standid order by standid;
select standid, year, speciesplants, round(mean_dmr,2), round(mean_dmi,2), inf_tpa, mort_tpa, inf_tpa_pct,
       mort_tpa_pct, stnd_tpa_pct from FVS_DM_Spp_Sum 
    order by standid, year, speciesplants;

select 'Table: FVS_DM_Stnd_Sum' as '';
PRAGMA table_info(FVS_DM_Stnd_Sum);
select standid, count(distinct year) as years from FVS_DM_Stnd_Sum group by standid order by standid;
select standid, year, stnd_tpa, inf_tpa, mort_tpa, inf_tpa_pct, mort_tpa_pct from FVS_DM_Stnd_Sum 
    order by standid, year;

select 'Table: FVS_Down_Wood_Cov' as '';
PRAGMA table_info(FVS_Down_Wood_Cov);
select standid, year, round(DWD_Cover_Total_Hard,2), round(DWD_Cover_Total_Soft,2)
    from FVS_Down_Wood_Cov 
    where year = '2021'  
    order by standid;

select 'Table: FVS_Down_Wood_Vol' as '';
PRAGMA table_info(FVS_Down_Wood_Vol);
select standid, year, round(DWD_Volume_Total_Hard,2), round(DWD_Volume_Total_Soft,2)
    from FVS_Down_Wood_Vol 
    where year = '2021'  
    order by standid;

select 'Table: FVS_EconHarvestValue' as '';
PRAGMA table_info(FVS_EconHarvestValue);

select 'Table: FVS_EconSummary' as '';
PRAGMA table_info(FVS_EconSummary);
select standid, count(year) 
    from FVS_EconSummary 
    group by standid 
    order by standid;

select 'Table: FVS_Fuels' as '';
PRAGMA table_info(FVS_Fuels);
select standid, year, round(surface_total,2), round(standing_total,2), total_consumed 
    from FVS_Fuels 
    where total_consumed > 0 
    order by standid, year;

select 'Table: FVS_Hrv_Carbon' as '';
PRAGMA table_info(FVS_Hrv_Carbon);
select standid, year, round(products,2), round(landfill,2), round(energy,2), round(emissions,2), round(merch_carbon_stored,2)
    from FVS_Hrv_Carbon 
    where products > 0 
    order by standid, year;

select 'Table: FVS_Mortality' as '';
PRAGMA table_info(FVS_Mortality);
select standid, year, speciesfvs, round(bakill,2)
    from FVS_Mortality 
    where bakill > 0 and speciesfvs != 'ALL'
    order by standid, year, speciesfvs;

select 'Table: FVS_PotFire' as '';
PRAGMA table_info(FVS_PotFire);
select standid, year, round(tot_flame_sev,2), round(tot_flame_mod,2), round(mortality_BA_sev,2), round(mortality_BA_mod,2) 
    from FVS_PotFire 
    where year = 2021 
    order by standid;

select 'Table: VS_Regen_HabType' as '';
PRAGMA table_info(FVS_Regen_HabType);
select standid, series, habitattype, numplots 
    from FVS_Regen_HabType 
    order by standid, series, habitattype;

select 'Table: FVS_Regen_Ingrow' as '';
PRAGMA table_info(FVS_Regen_Ingrow);
select standid, year, round(sum(ingrowthTpa),2) 
    from FVS_Regen_Ingrow 
    group by standid, year 
    order by standid, year;

select 'Table: FVS_Regen_SitePrep' as '';
PRAGMA table_info(FVS_Regen_SitePrep);
select standid, yearburn, pcntmech, pcntburn from FVS_Regen_SitePrep order by standid, yearburn;

select 'Table: FVS_Regen_Sprouts' as '';
PRAGMA table_info(FVS_Regen_Sprouts);
select standid, year, speciesplants, sprttpa from FVS_Regen_Sprouts order by standid, year, speciesplants;

select 'Table: FVS_Regen_Tally' as '';
PRAGMA table_info(FVS_Regen_Tally);
select standid, year, speciesplants, tpaall, pctoftotalall from FVS_Regen_Tally 
    where speciesplants != 'ALL' order by standid, year, speciesplants;

select 'Table: FVS_SnagDet' as '';
PRAGMA table_info(FVS_SnagDet);
select standid, year, speciesplants, year_died, round(avg(total_volume),2), round(avg(density_total),2)
    from FVS_SnagDet 
    group by standid, year, speciesplants, year_died 
    order by standid, year, speciesplants;

select 'Table: FVS_SnagSum' as '';
PRAGMA table_info(FVS_SnagSum);
select standid, year, round(hard_snags_total,2), round(soft_snags_total,2) 
    from FVS_SnagSum order by standid, year;

select 'Table: FVS_Stats_Species' as '';
PRAGMA table_info(FVS_Stats_Species);
select standid, year, speciesplants, round(boardfeet,2), round(cubicfeet,2), round(treesperacre,2), round(basalarea,2) 
    from FVS_Stats_Species order by standid, year, speciesplants;

select 'Table: FVS_Stats_Stand' as '';
PRAGMA table_info(FVS_Stats_Stand);
select standid, year, characteristic, round(average,2), round(standard_dev,2), round(coeff_of_var,2), round(samp_error_percent,2)
    from FVS_Stats_Stand order by standid, year, characteristic;

select 'Table: FVS_StrClass' as '';
PRAGMA table_info(FVS_StrClass);
select standid, year, removal_code, round(stratum_1_dbh,2), stratum_1_nom_ht, stratum_1_speciesfvs_1
    from FVS_StrClass order by standid, year, removal_code;

select 'Table: FVS_Summary2' as '';
PRAGMA table_info(FVS_Summary2);
select standid, year, rmvCode, round(tpa,2), round(ba,2), sdi, ccf, round(qmd,2)
    from FVS_Summary2 order by standid, year, rmvCode;

select 'Table: FVS_TreeList' as '';
PRAGMA table_info(FVS_TreeList);
select standid, year, count(distinct speciesplants) 
    from FVS_TreeList 
    group by standid, year 
    order by standid, year;
