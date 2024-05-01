.tables
.mode csv
select 'Cases', StandID, RunTitle, KeywordFile, SamplingWt, Variant 
  from FVS_Cases;
select 'Compute', StandID, Year, round(MYBBA,2), round(MYABA,2), round(MYBAR,2), 
       round(newDHt,2), round(newMyBBA,2), round(X1,2)
  from FVS_Compute;
select 'Treelist_count', Count(*) from FVS_TreeList;
select 'Summary', StandID, Year, Age, TopHt, BA, TPA from FVS_Summary;
select 'Summary2', StandID, Year, Age, TopHt, round(BA,0), round(TPA,0) 
  from FVS_Summary2;
select 'DM_Stnd_Sum', StandID, Year, round(Inf_TPA,0) 
  from FVS_DM_Stnd_Sum;
select 'DM_Spp_Sum', StandID, Year, SpeciesFVS, round(Inf_TPA,0) 
  from FVS_DM_Spp_Sum;
