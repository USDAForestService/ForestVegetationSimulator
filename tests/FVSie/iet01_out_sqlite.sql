.tables
.mode csv
select StandID, RunTitle, KeywordFile, SamplingWt, Variant from FVS_Cases;
select StandID, Year, TPA, BA, TCuFt, RTPA, Mort from FVS_Summary;
select 'FVS_TreeList--------', Count(*) from FVS_TreeList;
select 'FVS_CutList---------', Count(*) from FVS_CutList;
select 'FVS_ATRTList--------', Count(*) from FVS_ATRTList;
select 'FVS_DM_Spp_Sum------', Count(*) from FVS_DM_Spp_Sum;
select 'FVS_DM_Stnd_Sum-----', Count(*) from FVS_DM_Stnd_Sum;
select 'FVS_DM_Sz_Sum-------', Count(*) from FVS_DM_Sz_Sum;
select 'FVS_Regen_HabType---', Count(*) from FVS_Regen_HabType;
select 'FVS_Regen_Ingrow----', Count(*) from FVS_Regen_Ingrow;
select 'FVS_Regen_SitePrep--', Count(*) from FVS_Regen_SitePrep;
select 'FVS_Regen_Tally-----', Count(*) from FVS_Regen_Tally;
select 'FVS_Stats_Species---', Count(*) from FVS_Stats_Species;
select 'FVS_Stats_Stand-----', Count(*) from FVS_Stats_Stand;
select 'FVS_StrClass--------', Stratum_1_SpeciesFVS_1, Stratum_1_SpeciesFVS_2 from FVS_StrClass;
