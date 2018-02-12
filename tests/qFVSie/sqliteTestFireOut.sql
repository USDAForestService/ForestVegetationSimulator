.tables
.mode csv
select StandID, RunTitle, KeywordFile, SamplingWt, Variant from FVS_Cases;
select Count(*),Duff_Moisture from FVS_BurnReport;     
select Count(*),round(Consumption_6to12,2) from FVS_Consumption;   
select Count(*),round(Products,2) from FVS_Hrv_Carbon;    
select Count(*),round(Hard_snags_class2,2) from FVS_SnagSum;      
select Count(*),round(Height_ft,2) from FVS_CanProfile;     
select Count(*),round(DWD_Cover_Total_Hard,1)  from FVS_Down_Wood_Cov;  
select Count(*),Species from FVS_Mortality;   
select Count(*),round(Aboveground_Merch_Live,2)  from FVS_Carbon;        
select Count(*),round(DWD_Volume_Total_Hard,2)  from FVS_Down_Wood_Vol;  
select Count(*),round(Surf_Flame_Sev,2)  from FVS_PotFire;      
select Count(*),round(Surface_ge12,2)  from FVS_Fuels;
select Count(*),round(Total_Volume,2)  from FVS_SnagDet;   
