# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : StandStructure YSM-SkyRanch

StandStructure :
	@echo "Test StandStructure (4 stands)"
	rm -f FVS-BC.Stand.Structure.Out.db
	rm -f StandStructure.out
	rm -f StandStructure.sng
	-../../bin/FVSbc --keywordfile=StandStructure.key
	grep -v "\-999" StandStructure.sum      > StandStructure.tmp
	grep -v "\-999" StandStructure.sum.save > StandStructure.tmp.save
	-diff -w StandStructure.tmp StandStructure.tmp.save
	rm StandStructure.tmp StandStructure.tmp.save
	@echo "StandStructure output DB dump"
	rm -f StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".tables" > StandStructureDBdump.txt
	@echo "FVS_Cases" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID \
   from FVS_Cases" >> StandStructureDBdump.txt
	@echo "FVS_Compute" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year,round(SMVOL,2),round(MDVOL,2),round(LGVOL,2) \
   from FVS_Compute" >> StandStructureDBdump.txt
	@echo "FVS_Down_Wood_Vol_Metric" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year,round(DWD_Volume_Total_Hard,2), \
   round(DWD_Volume_Total_Soft,2) \
   from FVS_Down_Wood_Vol_Metric" >> StandStructureDBdump.txt
	@echo "FVS_Fuels_Metric" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year,round(Surface_Litter,2), \
   round(Surface_Duff,2),round(Surface_Total,2), \
   round(Standing_Total,2),round(Total_Biomass,2),round(Total_Consumed,2), \
   round(Biomass_Removed,2) \
   from FVS_Fuels_Metric" >> StandStructureDBdump.txt
	@echo "FVS_PotFire" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year,round(Surf_Flame_Sev,2),round(Surf_Flame_Mod,2), \
   round(Tot_Flame_Sev,2),round(Tot_Flame_Mod,2), \
   Fire_Type_Sev,Fire_Type_Mod, \
   round(PTorch_Sev,2),round(PTorch_Mod,2), \
   round(Torch_Index,2),round(Crown_Index,2), \
   Canopy_Ht,round(Canopy_Density,2), \
   Fuel_Mod1,Fuel_Mod2,Fuel_Mod3,Fuel_Mod4, \
   Fuel_Wt1,Fuel_Wt2,Fuel_Wt3,Fuel_Wt4 \
   from FVS_PotFire_Metric" >> StandStructureDBdump.txt
	@echo "FVS_PotFire_Cond" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Fire_Condition,round(Wind_Speed,2),Temperature, \
   round(One_Hr_Moisture,2),round(Ten_Hr_Moisture,2), \
   round(Hundred_Hr_Moisture,2),round(Thousand_Hr_Moisture,2), \
   round(Duff_Moisture,2),round(Live_Woody_Moisture,2), \
   round(Live_Herb_Moisture,2) \
   from FVS_PotFire_Cond_Metric" >> StandStructureDBdump.txt
	@echo "FVS_SnagDet" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,DBH_Class, \
   round(Death_DBH,2), \
   round(Current_Ht_Hard,2),round(Current_Ht_Soft,2), \
   round(Current_Vol_Hard,2),round(Current_Vol_Soft,2), \
   round(Total_Volume,2),Year_Died, \
   round(Density_Hard,2),round(Density_Soft,2),round(Density_Total,2) \
   from FVS_SnagDet_Metric" >> StandStructureDBdump.txt
	@echo "FVS_SnagSum" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select StandID,Year, \
   round(Hard_snags_class1,2), \
   round(Hard_snags_class2,2), \
   round(Hard_snags_class3,2), \
   round(Hard_snags_class4,2), \
   round(Hard_snags_class5,2), \
   round(Hard_snags_class6,2), \
   round(Hard_snags_total,2), \
   round(Soft_snags_class1,2), \
   round(Soft_snags_class2,2), \
   round(Soft_snags_class3,2), \
   round(Soft_snags_class4,2), \
   round(Soft_snags_class5,2), \
   round(Soft_snags_class6,2), \
   round(Soft_snags_total,2), \
   round(Hard_soft_snags_total,2) \
   from FVS_SnagSum_Metric" >> StandStructureDBdump.txt
	@echo "FVS_Summary_Metric" >> StandStructureDBdump.txt
	-sqlite3 FVS-BC.Stand.Structure.Out.db ".header on" ".mode csv" \
  "select count(*) \
   from FVS_Summary_Metric" >> StandStructureDBdump.txt
	-diff -w StandStructureDBdump.txt StandStructureDBdump.txt.save

	@echo "Test StandStructure (4 stands) ended"

YSM-SkyRanch :
	@echo "Test YSM-SkyRanch (15 stands)"
	rm -f FVS-BC.YSM-SkyRanch.Out.db
	rm -f YSM-SkyRanch.out YSM-SkyRanch.sum
	-../../bin/FVSbc --keywordfile=YSM-SkyRanch.key
	grep -v "\-999" YSM-SkyRanch.sum      > YSM-SkyRanch.tmp
	grep -v "\-999" YSM-SkyRanch.sum.save > YSM-SkyRanch.tmp.save
	-diff -w YSM-SkyRanch.tmp YSM-SkyRanch.tmp.save
	rm YSM-SkyRanch.tmp YSM-SkyRanch.tmp.save
	@echo "YSM-SkyRanch output DB dump"
	rm -f YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".tables" > YSM-SkyRanchDBdump.txt
	@echo "FVS_Cases" >> YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".header on" ".mode csv" \
  "select StandID \
   from FVS_Cases" >> YSM-SkyRanchDBdump.txt
	@echo "FVS_DM_Spp_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".header on" ".mode csv" \
  "select StandID,Year,SpeciesFVS,round(Mean_DMR,2),round(Mean_DMI,2), \
   Inf_TPH,Mort_TPH,Inf_TPH_Pct,Mort_TPH_Pct,Stnd_TPH_Pct \
   from FVS_DM_Spp_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	@echo "FVS_DM_Stnd_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".header on" ".mode csv" \
  "select StandID,Year,Age,Stnd_TPH,Stnd_BA,Stnd_Vol,Inf_TPH,Inf_BA, \
   Mort_TPH,Mort_BA,round(Mean_DMR,2),round(Mean_DMI,2) \
   from FVS_DM_Stnd_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	@echo "FVS_DM_Sz_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".header on" ".mode csv" \
  "select StandID,Year,Type, \
   round('0-5cm',2),round('5-10cm',2),round('10-15cm',2),round('15-20cm',2), \
   round('20-25cm',2),round('25-30cm',2),round('30-35cm',2),round('35-40cm',2), \
   round('40-45cm',2),round(gt45cm,2) \
   from FVS_DM_Sz_Sum_Metric" >> YSM-SkyRanchDBdump.txt
	@echo "FVS_Summary_Metric" >> YSM-SkyRanchDBdump.txt
	-sqlite3 FVS-BC.YSM-SkyRanch.Out.db ".header on" ".mode csv" \
  "select StandID,Year,Age,TPH,BA,SDI,CCF,round(QMD,2),round(MAI,2) \
   from FVS_Summary_Metric" >> YSM-SkyRanchDBdump.txt
	-diff -w YSM-SkyRanchDBdump.txt YSM-SkyRanchDBdump.txt.save

	@echo "Test YSM-SkyRanch (15 stands) ended"

