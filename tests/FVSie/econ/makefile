
# $Id: makefile 767 2013-04-10 22:29:22Z rhavis@msn.com $
# to execute run following from tests/fvsiec/econ directory: make -f makefile

all : AnnualValueOfTrees ClearCutPretend Example1A Example1B Example2SEV\
 Example3Forest Example4DBS Example4SEV Example5Reprod MinHarvValueOfTrees \
 TestAnnualCosts TestDataBase TestIRR_Decadal TestIRR_Yearly TestPoundsPerCubicFoot \
 TestSpecialCostAndRevenue TestSpecialCostAndRevenueThin TestSpeciesGroupHrvRvn \
 TestValueRateChanges

AnnualValueOfTrees :
	@echo Test ECON AnnualValueOfTrees
	-../../../bin/FVSiec --keywordfile=AnnualValueOfTrees.key
	-diff -w AnnualValueOfTrees.out AnnualValueOfTrees.out.save
	@echo Test ECON AnnualValueOfTrees ended

ClearCutPretend :
	@echo Test ECON ClearCutPretend
	-../../../bin/FVSiec --keywordfile=ClearCutPretend.key
	-diff -w ClearCutPretend.out ClearCutPretend.out.save
	@echo Test ECON ClearCutPretend ended

Example1A :
	@echo Test ECON Example1A
	-../../../bin/FVSiec --keywordfile=Example1A.key
	-diff -w Example1A.out Example1A.out.save
	@echo Test ECON Example1A ended

Example1B :
	@echo Test ECON Example1B
	-../../../bin/FVSiec --keywordfile=Example1B.key
	-diff -w Example1B.out Example1B.out.save
	@echo Test ECON Example1B ended

Example2SEV :
	@echo Test ECON Example2SEV
	-../../../bin/FVSiec --keywordfile=Example2SEV.key
	-diff -w Example2SEV.out Example2SEV.out.save
	@echo Test ECON Example2SEV ended

Example3Forest :
	@echo Test ECON Example3Forest
	-../../../bin/FVSiec --keywordfile=Example3Forest.key
	-diff -w Example3Forest.out Example3Forest.out.save
	@echo Test ECON Example3Forest ended

Example4DBS :
	@echo Test ECON Example4DBS
	-../../../bin/FVSiec --keywordfile=Example4DBS.key
	-diff -w Example4DBS.out Example4DBS.out.save
	@echo Test ECON Example4DBS ended

Example4SEV :
	@echo Test ECON Example4SEV
	-../../../bin/FVSiec --keywordfile=Example4SEV.key
	-diff -w Example4SEV.out Example4SEV.out.save
	@echo Test ECON Example4SEV ended

Example5Reprod :
	@echo Test ECON Example5Reprod
	-../../../bin/FVSiec --keywordfile=Example5Reprod.key
	-diff -w Example5Reprod.out Example5Reprod.out.save
	@echo Test ECON Example5Reprod ended

MinHarvValueOfTrees :
	@echo Test ECON MinHarvValueOfTrees
	-../../../bin/FVSiec --keywordfile=MinHarvValueOfTrees.key
	-diff -w MinHarvValueOfTrees.out MinHarvValueOfTrees.out.save
	@echo Test ECON MinHarvValueOfTrees ended

TestAnnualCosts :
	@echo Test ECON TestAnnualCosts
	-../../../bin/FVSiec --keywordfile=TestAnnualCosts.key
	-diff -w TestAnnualCosts.out TestAnnualCosts.out.save
	@echo Test ECON TestAnnualCosts ended

TestDataBase :
	@echo Test ECON TestDataBase
	-../../../bin/FVSiec --keywordfile=TestDataBase.key
	-diff -w TestDataBase.out TestDataBase.out.save
	@echo Test ECON TestDataBase ended

TestIRR_Decadal :
	@echo Test ECON TestIRR_Decadal
	-../../../bin/FVSiec --keywordfile=TestIRR_Decadal.key
	-diff -w TestIRR_Decadal.out TestIRR_Decadal.out.save
	@echo Test ECON TestIRR_Decadal ended

TestIRR_Yearly :
	@echo Test ECON TestIRR_Yearly
	-../../../bin/FVSiec --keywordfile=TestIRR_Yearly.key
	-diff -w TestIRR_Yearly.out TestIRR_Yearly.out.save
	@echo Test ECON TestIRR_Yearly ended

TestPoundsPerCubicFoot :
	@echo Test ECON TestPoundsPerCubicFoot
	-../../../bin/FVSiec --keywordfile=TestPoundsPerCubicFoot.key
	-diff -w TestPoundsPerCubicFoot.out TestPoundsPerCubicFoot.out.save
	@echo Test ECON TestPoundsPerCubicFoot ended

TestSpecialCostAndRevenue :
	@echo Test ECON TestSpecialCostAndRevenue
	-../../../bin/FVSiec --keywordfile=TestSpecialCostAndRevenue.key
	-diff -w TestSpecialCostAndRevenue.out TestSpecialCostAndRevenue.out.save
	@echo Test ECON TestSpecialCostAndRevenue ended

TestSpecialCostAndRevenueThin :
	@echo Test ECON TestSpecialCostAndRevenueThin
	-../../../bin/FVSiec --keywordfile=TestSpecialCostAndRevenueThin.key
	-diff -w TestSpecialCostAndRevenueThin.out TestSpecialCostAndRevenueThin.out.save
	@echo Test ECON TestSpecialCostAndRevenueThin ended

TestSpeciesGroupHrvRvn :
	@echo Test ECON TestSpecialCostAndRevenueThin
	-../../../bin/FVSiec --keywordfile=TestSpeciesGroupHrvRvn.key
	-diff -w TestSpeciesGroupHrvRvn.out TestSpeciesGroupHrvRvn.out.save
	@echo Test ECON TestSpeciesGroupHrvRvn ended

TestValueRateChanges :
	@echo Test ECON TestValueRateChanges
	-../../../bin/FVSiec --keywordfile=TestValueRateChanges.key
	-diff -w TestValueRateChanges.out TestValueRateChanges.out.save
	@echo Test ECON TestValueRateChanges ended
