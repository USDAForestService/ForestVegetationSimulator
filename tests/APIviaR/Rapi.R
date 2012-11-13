
# find and get the R code
cwd = getwd()
while(TRUE)
{
  if (length(dir(pattern="rFVS")) > 0) break
  setwd("..")
}
setwd("rFVS/R")
cat ("fetching R code from",getwd(),"\n")
for (rf in dir ()) source (rf)
setwd(cwd)
# load the FVS library
fvsLoad("FVSiec","../../bin")

# define a function to get tree some trees in a given cycle.
fetchTrees <- function (captureYears) 
{
  curYear <- fvsGetEventMonitorVariables("year") 
  if (is.na(match(curYear,captureYears))) NULL else 
      fvsGetTreeAttrs(c("id","dbh","species","ht","cratio","plot",
                           "tpa","tcuft","bdft","mgmtcd"))  
}
fvsSetCmdLine("--keywordfile=base.key")
base <- fvsInteractRun(BeforeEstab="fetchTrees(c(2040))",
                       AfterEM1="fetchTrees(c(2050))")
baseSum <- fvsGetSummary()

# in this function, the tree records are fetched and then added
# to the tree list as duplicates. The number of trees is divided
# in half for all the trees so that the result is the same as
# not adding the trees. 
repTrees <- function (captureYears) 
{
  curYear <- fvsGetEventMonitorVariables("year") 
  if (is.na(match(curYear,captureYears))) return (NULL)
  ctrees=fvsGetTreeAttrs(c("id","dbh","species","ht","cratio","plot",
                           "tpa","tcuft","bdft","mgmtcd"))  
  fvsAddTrees(ctrees)
  ctrees=rbind(ctrees,ctrees)
  ctrees$tpa=ctrees$tpa*.5
  fvsSetTreeAttrs(ctrees)
  fvsGetTreeAttrs(c("id","dbh","species","ht","cratio","plot",
                     "tpa","tcuft","bdft","mgmtcd"))  
}

fvsSetCmdLine("--keywordfile=base.key")
wRep <- fvsInteractRun(BeforeEstab="repTrees(c(2040))",
                       AfterEM1="fetchTrees(c(2050))")
wRepSum <- fvsGetSummary()

wRepSum-baseSum
w=wRep[[1]][[1]]

w[1:27,]-w[28:54,]


