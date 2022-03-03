# The folders where the results from different databases are stores
resultFolders <- c()

# The folder where the combined result files will be written:
outputFolder <- "combShiny"

# Combine yearly and monthly results from different databases
dataYr <- data.frame()
dataMth <- data.frame()
for(i in resultFolders) {
  dataYr_i <- readRDS(file.path(getwd(), i, "Shiny", "dataByYear.RDS"))
  dataMth_i <- readRDS(file.path(getwd(), i, "Shiny", "dataByMonth.RDS"))

  dataYr <- rbind(dataYr, dataYr_i)
  dataMth <- rbind(dataMth, dataMth_i)
}

# Save the combined results
if(!file.exists(file.path(getwd(), outputFolder))) dir.create(file.path(getwd(), outputFolder))
saveRDS(dataYr, file.path(getwd(), outputFolder, "dataByYear.RDS"))
saveRDS(dataMth, file.path(getwd(), outputFolder, "dataByMonth.RDS"))

# Launch Shiny
shinyDataFolder <- file.path(getwd(), outputFolder)
.GlobalEnv$shinySettings <- list(dataFolder = shinyDataFolder)
shinyFolder <- file.path(getwd(), "inst", "shiny")
shiny::runApp(shinyFolder)
