#! /bin/bash
#
# run_tests_US.sh
#
# This script goes through the US varients and run the test suite.
#
# After the execution the check_test_results_US.sh is used to 
# compare the .save files with the latest run. It still requires a
# manual inspection of the output.
#
#
# James I. Garrett, Jr.
# 20261026
#
# Example of Use:
#   sh run_tests_US.sh 2>&1 | tee run_results_US.txt
#
# Example of use for Run and Check:
#
# $ clear
#
# $ date
#
# $ cd /c/Users/james.garrett/Documents/ForestVegetationSimulator/tests
#
# $ sh run_tests_US.sh 2>&1 | tee run_results_US_YYYYMMDD.txt
#
# $ sh check_test_results_US.sh > check_test_results_US_YYYYMMDD.txt
#
# $ diff -wi check_test_results_US.txt.save check_test_results_US_YYYYMMDD.txt | tee compare_test_results_US_YYYYMMDD.txt
#
# $ date
#
echo "run_tests_US.sh"
pwd
date
echo "---------------------------------------------------------------------------------------------------------------"
#FVSak
cd FVSak
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSbm
cd FVSbm
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSca
cd FVSca
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSci
cd FVSci
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVScr
cd FVScr
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVScs
cd FVScs
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSec
cd FVSec
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSem
cd FVSem
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSie
cd FVSie
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSkt
cd FVSkt
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSls
cd FVSls
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSnc
cd FVSnc
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSne
cd FVSne
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSoc
cd FVSoc
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSop
cd FVSop
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSpn
cd FVSpn
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSsn
cd FVSsn
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSso
cd FVSso
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVStt
cd FVStt
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSut
cd FVSut
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSwc
cd FVSwc
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
#FVSws
cd FVSws
pwd
make clean
make
cd ..
echo "---------------------------------------------------------------------------------------------------------------"
date