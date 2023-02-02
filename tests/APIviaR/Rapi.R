## R code to test the FVS api

library(rFVS)

# load the FVS library
fvsLoad("FVSie","../../bin")

# define tree attribute list names
treeAttrs = c("id","species","mort","tpa","dbh","dg","ht",
      "htg","crwdth","cratio","age","plot",
      "tcuft","mcuft","bdft","plotsize","mgmtcd")
      
# no cycles, plots, or trees yet
fvsGetDims()

# should be return an empty list
fvsGetTreeAttrs(treeAttrs) 

# the species codes
fvsGetSpeciesCodes()
# list supported activity codes
fvsAddActivity()

## first run
fvsSetCmdLine("--keywordfile=base.key")

fvsRun(7,0)
fvsRun(2,2030)
fvsGetStandIDs()

# get and output some event monitor vars
fvsGetEventMonitorVariables(c("year","atpa","aba","mybba","myaba"))
fvsSetEventMonitorVariables(c("myaba"=100,"another"=40))
fvsGetEventMonitorVariables(c("myaba","another"))

# get and output tree attributes
fvsGetTreeAttrs(treeAttrs)

fvsSetSpeciesAttrs(list(baimult=rep(1.1,fvsGetDims()["maxspecies"]),
                        mortmult=rep(.9,fvsGetDims()["maxspecies"])))

# get and set some species attributes
spAttrs = fvsGetSpeciesAttrs(c("baimult","spsdi","spccf","spsiteindx"))
spAttrs

rtn = fvsSetSpeciesAttrs(spAttrs)
cat ("rtn = ",rtn,"\n")

# run to 2060 stop prior to adding increments
fvsRun(5,2060)

trees=fvsGetTreeAttrs(treeAttrs)
#set mortality and growth to zero
trees$mort = 0
trees$htg  = 0
trees$dg   = 0
fvsSetTreeAttrs(trees[,c(3,6,8)])

# finish the run
fvsRun(0,0)

# get and output summary statistics
fvsGetSummary() #year 2060 and 2070 should be equal

# run the next stand in the set, no stoping. 
fvsRun()

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
fvsRun(0,0)

#get and output summary statistics
fvsGetSummary()

# continue the run for the next stand.
fvsRun()

# test fvsInteractRun
fvsSetCmdLine("--keywordfile=base.key") 
testInteract <- function(msg) 
{
  cat("msg=",msg," ids=",unlist(fvsGetStandIDs()),         
      " year=",fvsGetEventMonitorVariables("year"),"\n")
  fvsGetRestartcode()
}
rtn = fvsInteractRun(
        BeforeEM1  = 'testInteract("BeforeEM1  ")', 
        AfterEM1   = 'testInteract("AfterEM1   ")', 
        BeforeEM2  = 'testInteract("BeforeEM2  ")', 
        AfterEM2   = 'testInteract("AfterEM2   ")', 
        BeforeAdd  = 'testInteract("BeforeAdd  ")', 
        BeforeEstab= 'testInteract("BeforeEstab")', 
        SimEnd     = 'testInteract("SimEnd     ")')



