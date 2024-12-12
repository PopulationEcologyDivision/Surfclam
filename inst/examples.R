#whole folders
test <- clamQC(cwFolder = "C:/Path/Original CW Log Data/2023/2023BC Q1&Q2/",
       logFolder = "C:/Path/HailedOnboardLogData/2023/BC/",
       resultsFolder = "C:/Path/exampleOutput/", 
       clam.username= oracle.username, clam.password=oracle.password, clam.dsn="ptran", usepkg="roracle", offline=F, debug=F,
       gpkgName = "example.gpkg")



#single CW file examples
productFile <- clamCW_QC_file(file = "C:/Path/Original Log Data/2022/2022AE Q1&Q2/Product.xlsx", rightNow = Sys.time(), layerPrefix = "Mike")
recordFile  <- clamCW_QC_file(file = "C:/Path/Original Log Data/2022/2022AE Q1&Q2/Record.xlsx", rightNow = Sys.time(), layerPrefix = "Mike")

#single Log file
logFile <- clamLog_QC_file(file ="C:/Path/HailedOnboardLogData/2022/AR/AR Cdn Clam DFO Log- Jan-5-8 2023.xlsx" )
