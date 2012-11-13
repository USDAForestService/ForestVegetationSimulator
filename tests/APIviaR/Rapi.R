# $Id$

## R code to test the FVS api

# find and get the R code
cwd = getwd()
while(TRUE)
{
  if (length(dir(pattern="rFVS")) > 0) break
  setwd("..")
  if (nchar(getwd()) < 4) {setwd(cwd);stop("Cannot find R code.")}
}
setwd("rFVS/R")

# fetching R code from
getwd()
for (rf in dir ()) source (rf)
setwd(cwd)

# load the FVS library
fvsLoad("FVSiec","../../bin")


# define tree attribute list names
treeAttrs = c("id","species","tpa","dbh","dg","ht",
      "htg","crwdth","cratio","age","plot",
      "tcuft","mcuft","bdft","plotsize","mgmtcd")
      
# no cycles, plots, or trees yet
fvsGetDims()

# should be return an empty list
fvsGetTreeAttrs(treeAttrs) 

# the species codes
fvsGetSpeciesCodes()

## first run
fvsSetCmdLine("--keywordfile=base.key")

fvsRun(2,2030)
fvsGetStandIDs()

# get and output some event monitor vars
fvsGetEventMonitorVariables(c("year","atpa","aba"))

# get and output tree attributes
fvsGetTreeAttrs(treeAttrs)

# finish the run
fvsRun()

# get and output summary statistics
fvsGetSummary()

# list supported activity codes
fvsGetSummary()

## next run, use the same keywords
fvsSetCmdLine("--keywordfile=base.key")

fvsRun(2,1993)
addtrees <- fvsGetTreeAttrs(treeAttrs) 
addtrees <- subset(addtrees,dbh<2)[,c("dbh","species","ht","cratio","plot","tpa")]

# these trees will be added to the run at 2013
addtrees

# add a yearloss and thindbh for 1993
fvsAddActivity(1993,"base_yardloss",c(0.50, 0.70, 0.50))
fvsAddActivity(1993,"base_thindbh",c(0.00,12.00,1.00,0.00,0.00))

# continue the run
fvsRun(6,2013)

# add the trees and output the current trees
fvsAddTrees(addtrees)
fvsGetTreeAttrs(treeAttrs)

# continue the run
fvsRun()

#get and output summary statistics
fvsGetSummary()


