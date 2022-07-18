.tables
.mode csv
select "Cases", StandID, RunTitle, KeywordFile, SamplingWt, Variant from FVS_Cases;
select "BurnReport", Duff_Moisture from FVS_BurnReport;
select "Consumption", round(Consumption_6to12,2) from FVS_Consumption;
select "Hrv_Carbon", round(Products,2) from FVS_Hrv_Carbon;
select "SnagSum", round(Hard_snags_class2,2) from FVS_SnagSum;
select "CanProfile", round(Height_ft,2) from FVS_CanProfile;
select "Down_Wood_Cov", round(DWD_Cover_Total_Hard,1) from FVS_Down_Wood_Cov;
select "Mortality", SpeciesFVS from FVS_Mortality;
select "Carbon", round(Aboveground_Merch_Live,2) from FVS_Carbon;
select "Down_Wood_Vol", round(DWD_Volume_Total_Hard,2) from FVS_Down_Wood_Vol;
select "PotFire", round(Surf_Flame_Sev,2)  from FVS_PotFire;
select "Fuels", round(Surface_ge12,2)  from FVS_Fuels;
select "SnagDet", round(Total_Volume,2)  from FVS_SnagDet;
