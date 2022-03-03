library(CERVELLOPrevalence)

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
outputFolder <- file.path("")

# Details for connecting to the server:
dbName <- ''

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "",
  server = '',
  user = '',
  password = '',
  pathToDriver = ''
)


# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema<-''

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- ''
cohortTable <- "Cevello"

# Some meta-information that will be used by the export function:
databaseName <- ''



execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        databaseName = databaseName,
        createCohorts = TRUE,
        runPrevalence = TRUE,
        yearStartDate = "2016-01-01",
        yearEndDate = "2021-07-01",
        monthStartDate = "2016-01-01",
        monthEndDate = "2021-07-01",
        maxCores = maxCores,
        minCellCount= 0)

# Launch Shiny
shinyDataFolder <- file.path(getwd(), outputFolder, "Shiny")
.GlobalEnv$shinySettings <- list(dataFolder = shinyDataFolder)
shinyFolder <- file.path("inst", "shiny")
shiny::runApp(shinyFolder)
