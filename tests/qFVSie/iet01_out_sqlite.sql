.tables
.mode csv
select StandID, RunTitle, KeywordFile, SamplingWt, Variant from FVS_Cases;
select StandID, Year, TPA, BA, TCuFt, RTPA, Mort from FVS_Summary;
select Count(*) from FVS_TreeList;
select Count(*) from FVS_CutList;
select Count(*) from FVS_ATRTList;
select Count(*) from FVS_DM_Spp_Sum;
select Count(*) from FVS_DM_Stnd_Sum;
select Count(*) from FVS_DM_Sz_Sum;
select Count(*) from FVS_Regen_HabType;
select Count(*) from FVS_Regen_Ingrow;
select Count(*) from FVS_Regen_SitePrep;
select Count(*) from FVS_Regen_Tally;
select Count(*) from FVS_Stats_Species;
select Count(*) from FVS_Stats_Stand;
select Count(*), Stratum_1_SpeciesFVS_1, Stratum_1_SpeciesFVS_2 from FVS_StrClass;
