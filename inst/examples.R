cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), oracle.username, oracle.password, "PTRAN")
#whole folders
test <- clamQC(cwFolder = "C:/Path/Original CW Log Data/2023/2023BC Q1&Q2/",
       logFolder = "C:/Path/HailedOnboardLogData/2023/BC/",
       resultsFolder = "C:/Path/exampleOutput/", 
       cxn = cxn, offline=F, debug=F,
       gpkgName = "example.gpkg")



#single CW file examples
productFile <- clamCW_QC_file(file = "C:/Path/Original Log Data/2022/2022AE Q1&Q2/Product.xlsx", rightNow = Sys.time(), layerPrefix = "Mike")
recordFile  <- clamCW_QC_file(file = "C:/Path/Original Log Data/2022/2022AE Q1&Q2/Record.xlsx", rightNow = Sys.time(), layerPrefix = "Mike")

#single Log file
logFile <- clamLog_QC_file(file ="C:/Path/HailedOnboardLogData/2022/AR/AR Cdn Clam DFO Log- Jan-5-8 2023.xlsx" )


test <- clamQC(cwFolder = "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/!group_OffshoreClams/QCChecks/Original CW Log Data/2023/2023AE Q3&Q4/",
               logFolder = "C:/Users/mcmahonm/OneDrive - DFO-MPO/Support/!group_OffshoreClams/QCChecks/HailedOnboardLogData/2023/AE/",
               resultsFolder = "C:/Users/McMahonM/OneDrive - DFO-MPO/Support/!group_OffshoreClams/test1/",
               cxn = cxn, offline=F, debug=F,
               gpkgName = "example.gpkg")