
all : crt01 crt02 CalibStats climdb CR_FFE_Reg_db

crt01 : 
	@echo Test crt01, basic functionality 
	-$(WINE) ../../bin/qFVScr --keywordfile=crt01.key
	grep -v "\-999" crt01.sum      > crt01.tmp
	grep -v "\-999" crt01.sum.save > crt01.tmp.save
	-diff -w crt01.tmp crt01.tmp.save
	rm crt01.tmp crt01.tmp.save
	@echo Test crt01 ended

crt02 : 
	@echo Test crt02, stop/restart, output should match crt01
	-$(WINE) ../../bin/qFVScr --keywordfile=crt01.key --stoppoint=2,2040,crt.stop
	-$(WINE) ../../bin/qFVScr --restart=crt.stop
	grep -v "\-999" crt01.sum      > crt01.tmp
	grep -v "\-999" crt01.sum.save > crt01.tmp.save
	-diff -w crt01.tmp crt01.tmp.save
	rm crt01.tmp crt01.tmp.save
	@echo Test crt02 ended

CalibStats :
	@echo Test FVS_CalibStats read and write
	rm -f CalibStats.db
	-$(WINE) ../../bin/qFVScr --keywordfile=CalibStats.key 
	@echo FVS_CalibStats/ > CalibStats_dump.txt
	-sqlite3 CalibStats.db ".header on" "select StandID,TreeSize,SpeciesFVS,NumTrees,round(ScaleFactor,3),round(StdErrRatio,2) from FVS_CalibStats" >> CalibStats_dump.txt
	@echo FVS_Stats_Species/ >> CalibStats_dump.txt
	-sqlite3 CalibStats.db ".header on" "select StandID,Year,SpeciesFVS,round(BoardFeet,2),round(CubicFeet,2),round(TreesPerAcre,2),round(BasalArea,2) from FVS_Stats_Species" >> CalibStats_dump.txt
	@echo FVS_Stats_Stand/ >> CalibStats_dump.txt
	-sqlite3 CalibStats.db ".header on" "select StandID,Year,Characteristic,round(Average,3),round(Standard_Dev,3),round(Coeff_of_Var,3),Sample_Size,Conf_Level_Percent,round(CI_LB,3),round(CI_UB,3),round(Samp_Error_Percent,3),round(Samp_Error_Units,3) from FVS_Stats_Stand" >> CalibStats_dump.txt
	-diff -w CalibStats_dump.txt CalibStats_dump.txt.save
	@echo Test CalibStats ended

climdb : 
	@echo climate database read and write
	rm -f ClimOut.db
	-$(WINE) ../../bin/qFVScr --keywordfile=Clim.key  
	-sqlite3 ClimOut.db "Select SpeciesFVS,Count(SpeciesFVS) from FVS_Climate group by SpeciesFVS;" > ClimOut.db.txt 
	-diff -w ClimOut.db.txt ClimOut.db.txt.save
	@echo Test climdb ended

CR_FFE_Reg_db : 
	@echo FFE Regen Output database write
	rm -f CR_FFE_Reg_db_Out.db
	-$(WINE) ../../bin/qFVScr --keywordfile=CR_FFE_Reg_db.key  
	-echo Record counts in tables...  > CR_FFE_Reg_db_Out.txt
	@echo FVS_ATRTList/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_ATRTList ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_BurnReport/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_BurnReport ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Carbon/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Carbon ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Cases/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Cases ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Consumption/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Consumption ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_CutList/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_CutList ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Down_Wood_Cov/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Down_Wood_Cov ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Down_Wood_Vol/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Down_Wood_Vol ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Fuels/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Fuels ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Hrv_Carbon/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Hrv_Carbon ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Mortality/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Mortality ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_PotFire/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_PotFire ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Regen_Sprouts/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Regen_Sprouts ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Regen_Tally/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Regen_Tally ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_SnagSum/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_SnagSum ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Stats_Species/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Stats_Species ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Stats_Stand/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Stats_Stand ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_StrClass/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_StrClass ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_Summary/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_Summary ;" >> CR_FFE_Reg_db_Out.txt
	@echo FVS_TreeList/ >> CR_FFE_Reg_db_Out.txt
	@sqlite3 CR_FFE_Reg_db_Out.db "Select Count(*) from FVS_TreeList ;" >> CR_FFE_Reg_db_Out.txt
	-diff -w CR_FFE_Reg_db_Out.txt CR_FFE_Reg_db_Out.txt.save
	@echo Test CR_FFE_Reg_db ended

