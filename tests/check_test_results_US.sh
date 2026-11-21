#! /bin/bash
#
# check_test_results_US.sh
#
# This script goes through the US varients tests and compares
# the .save files with the latest run. It still requires a
# manual inspection of the output.
#
# James I. Garrett, Jr.
# 20260206
#
echo "check_test_results_US.sh"
pwd
date
##ls -1
##APIviaR
##check_test_results_US.sh
echo "---------------------------------------------------------------------------------------------------------------"
#FVSak
cd FVSak
pwd
ls *.save
diff -w -b -Z -E -s akt01.sum.save akt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSbm
cd FVSbm
pwd
ls *.save
diff -w -b -Z -E -s bmt01.sum.save bmt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSca
cd FVSca
pwd
ls *.save
diff -w -b -Z -E -s cat01.sum.save cat01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSbc -- Omit currently 20260226
#cd FVSbc
#pwd
#ls *.save
#echo "# No file -- OK diff -w -b -Z -E -s StandStructure.sum.save StandStructure.sum"
#diff -w -b -Z -E -s StandStructureDBdump.txt.save StandStructureDBdump.txt
#echo "# No file -- OK diff -w -b -Z -E -s YSM-SkyRanch.sum.save YSM-SkyRanch.sum"
#diff -w -b -Z -E -s YSM-SkyRanchDBdump.txt.save YSM-SkyRanchDBdump.txt
#cd ..
#echo "---------------------------------------------------------------------------------------------------------------"
#FVSci
cd FVSci
pwd
ls *.save
diff -w -b -Z -E -s cit01.sum.save cit01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVScr
cd FVScr
pwd
ls -1 *.save
diff -w -b -Z -E -s CalibStats_dump.txt.save CalibStats_dump.txt
diff -w -b -Z -E -s ClimOut.db.txt.save ClimOut.db.txt
diff -w -b -Z -E -s CR_FFE_Reg_db_Out.txt.save CR_FFE_Reg_db_Out.txt
diff -w -b -Z -E -s crt01.sum.save crt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVScs
cd FVScs
pwd
ls *.save
diff -w -b -Z -E -s cst01.sum.save cst01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSec
cd FVSec
pwd
ls *.save
diff -w -b -Z -E -s ect01.sum.save ect01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSem
cd FVSem
pwd
ls *.save
diff -w -b -Z -E -s emt01.sum.save emt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSie
cd FVSie
pwd
ls -1 *.save
diff -w -b -Z -E -s calibStats_dump.txt.save calibStats_dump.txt
diff -w -b -Z -E -s ccadj.sum.save ccadj.sum
diff -w -b -Z -E -s Clim.db.txt.save Clim.db.txt
diff -w -b -Z -E -s climate.sum.save climate.sum
diff -w -b -Z -E -s compress.out.save compress.out
diff -w -b -Z -E -s DBReportTest_dump.txt.save DBReportTest_dump.txt
diff -w -b -Z -E -s EconDB_Out.dump.save EconDB_Out.dump
echo "## No file -- OK diff -w -b -Z -E -s ffe_svs_007.svs.save ffe_svs_007.svs"
echo "## No file -- OK diff -w -b -Z -E -s ffe_svs_index.svs.save ffe_svs_index.svs"
diff -w -b -Z -E -s fireTest_db_dump.txt.save fireTest_db_dump.txt
diff -w -b -Z -E -s ie_db_dump.txt.save ie_db_dump.txt
diff -w -b -Z -E -s iet01.sum.save iet01.sum
diff -w -b -Z -E -s iet01_db_dump.txt.save iet01_db_dump.txt
diff -w -b -Z -E -s iet02_db_dump.txt.save iet02_db_dump.txt
diff -w -b -Z -E -s iet03.sum.save iet03.sum
diff -w -b -Z -E -s NestedAddFile.sum.save NestedAddFile.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSkt
cd FVSkt
pwd
ls *.save
diff -w -b -Z -E -s ktt01.sum.save ktt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSls
cd FVSls
pwd
ls *.save
diff -w -b -Z -E -s lst01.sum.save lst01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSnc
cd FVSnc
pwd
ls *.save
diff -w -b -Z -E -s ffe.sum.save ffe.sum
diff -w -b -Z -E -s nct01.sum.save nct01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSne
cd FVSne
pwd
ls *.save
diff -w -b -Z -E -s net01.sum.save net01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSoc
cd FVSoc
pwd
ls *.save
diff -w -b -Z -E -s oct01.sum.save oct01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSon
#cd FVSon
#pwd
#ls *.save
#diff -w -b -Z -E -s FVSOutHardwoodDBdump.txt.save FVSOutHardwoodDBdump.txt
#echo "## No file -- OK diff -w -b -Z -E -s Hardwood.sum.save Hardwood.sum"
#cd ..
#echo "---------------------------------------------------------------------------------------------------------------"
#FVSop
cd FVSop
pwd
ls *.save
diff -w -b -Z -E -s opt01.sum.save opt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSpn
cd FVSpn
pwd
ls *.save
diff -w -b -Z -E -s pnt01.sum.save pnt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSsn
cd FVSsn
pwd
ls *.save
diff -w -b -Z -E -s snout.txt.save snout.txt
diff -w -b -Z -E -s snt01.sum.save snt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSso
cd FVSso
pwd
ls *.save
diff -w -b -Z -E -s sot01.sum.save sot01.sum
diff -w -b -Z -E -s sot03.sum.save sot03.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVStt
cd FVStt
pwd
ls *.save
diff -w -b -Z -E -s ttt01.sum.save ttt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSut
cd FVSut
pwd
ls *.save
diff -w -b -Z -E -s utt01.sum.save utt01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSwc
cd FVSwc
pwd
ls *.save
diff -w -b -Z -E -s wct01.sum.save wct01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSws
cd FVSws
pwd
ls *.save
diff -w -b -Z -E -s wst01.sum.save wst01.sum
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
date
##makefile
##test.py
##testSetFromFMSC
