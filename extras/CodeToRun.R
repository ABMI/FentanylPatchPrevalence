library(FentanylPatchPrevalence)

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
outputFolder <- file.path("FentanylPatchPrevalenceResults")

# Details for connecting to the server:
dbName <- 'MydbName'

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'oracle',
  server = 'myserver',
  user = 'joe',
  password = 'secret',
  pathToDriver = 'S:/jdbcDrivders'
)


# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema<-'CDM_mydb.dbo'

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- 'mydb.dbo'
cohortTable <- "FentanylPatchPrevalence"

# Some meta-information that will be used by the export function:
databaseName <- dbName



execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        databaseName = databaseName,
        createCohorts = TRUE,
        runPrevalence = TRUE,
        yearStartDate = "2012-01-01",
        yearEndDate = "2021-12-31",
        monthStartDate = "2012-01-01",
        monthEndDate = "2021-12-31",
        maxCores = maxCores,
        minCellCount= 0)

# Launch Shiny
shinyDataFolder <- file.path(getwd(), outputFolder, "Shiny")
.GlobalEnv$shinySettings <- list(dataFolder = shinyDataFolder)
shinyFolder <- file.path("inst", "shiny")
shiny::runApp(shinyFolder)
