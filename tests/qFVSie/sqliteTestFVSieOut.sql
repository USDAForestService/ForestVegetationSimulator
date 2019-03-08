.tables
.mode csv
select StandID, RunTitle, KeywordFile, SamplingWt, Variant from FVS_Cases;
select StandID, Year, round(MYBBA,2), round(MYABA,2), round(MYBAR,2), 
  round(newDHt,2), round(newMyBBA,2), round(X1,2)
  from FVS_Compute;
select "Treelist count" as cnt,Count(*) from FVS_TreeList;
select "from summary " as frm,StandID, Year, Age, TopHt, BA, TPA from FVS_Summary;
select "from summary2" as frm,StandID, Year, Age, TopHt, round(BA,0), round(TPA,0) from FVS_Summary2;
select StandID, Year, round(Inf_TPA,0) as Inf_TPA from FVS_DM_Stnd_Sum;
select StandID, Year, Spp, round(Inf_TPA,0) as Inf_TPA from FVS_DM_Spp_Sum;
