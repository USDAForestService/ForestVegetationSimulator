#Follows same test pattern as Rapi.R in trunk/tests/APIviaR

#working example
fvsLoad("libFVS_iec","C:\\Users\\Casey\\Documents\\SVN\\compiled")

treeAttrs = ["id","species","mort","tpa","dbh","dg","ht",
      "htg","crwdth","cratio","age","plot","tcuft","mcuft",
      "bdft","plotsize","mgmtcd"]

# check before run, no cycles, plots, or trees yet
fvsDimSizes()

#check before run should return nothing
#not working yet, needs a loop 
fvsTreeAttr(treeAttrs)

# the species codes
fvsSpeciesCode()

#not working yet
fvsAddActivity()

#set command line;
fvsSetCmdLine(raw"--keywordfile=C:\Users\Casey\Documents\SVN\compiled\base.key")
#doesnt wokr without raw?
#fvsSetCmdLine("--keywordfile=C:\Users\Casey\Documents\SVN\compiled\base.key")

#run for a few cycles and stop
fvsRun(2,2030)

#return some standIDs
fvsGetStandIDs()

# get and output some event monitor vars
 fvsGetEventMonitorVariables(["year","atpa","aba"])
 
 #for testing if needed
#     name="btpa"
#     nch=length(name)
#     action="get"
#     attr=zeros(Float64,1)
#     rtnCode = 0
    
#     ccall(Libdl.dlsym(lib,:fvsevmonattr_), Void,(
#     Cstring, #name
#     Ref{Int32}, #nch
#     Cstring, #action
#     Ref{Float64}, #attr
#     Ref{Int32}, #rtnCode
#     ),name,nch,action,attr,rtnCode)



# get and output tree attributes

 fvsTreeAttr(treeAttrs)


# #from R

# # get and set some species attributes
# spAttrs = fvsGetSpeciesAttrs(c("spsdi","spccf","spsiteindx"))

# run to 2060 stop prior to adding increments
fvsRun(5,2060)


#store tree info in var 
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
