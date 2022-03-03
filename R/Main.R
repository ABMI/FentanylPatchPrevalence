# This file is part of CervelloPrevalence


#' Execute the Study
#'
#' @details
#' This function executes the CervelloPrevalence Study.
#'
#' The \code{createCohorts}, \code{createAllCohorts}, \code{computePrevalence} arguments
#' are intended to be used to run parts of the full study at a time, but none of the parts are considered to be optional.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the exposure and outcome cohorts used in this
#'                             study.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param runPrevalence          Perform the prevalence analyses?
#' @param yearStartDate        Start date for yearly prevalence, has to be on or later than 2016-01-01
#' @param yearEndDate          End date for yearly prevalence, has to be on or earlier than the latest date in the database
#' @param monthStartDate       Start date for monthly prevalence, has to be on or later than 2016-01-01
#' @param monthEndDate         End date for monthly prevalence, has to on or earlier than the latest date in the database
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param minCellCount         The minimum number of subjects can be reported.
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "redshift",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         outputFolder = "c:/temp/study_results",
#'         maxCores = 4)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = cohortTable,
                    outputFolder,
                    databaseName = "Unknown",
                    createCohorts = TRUE,
                    runPrevalence = TRUE,
                    yearStartDate = as.Date("2016-01-01"),
                    yearEndDate = as.Date("2021-07-01"),
                    monthStartDate = as.Date("2016-01-01"),
                    monthEndDate = as.Date("2021-07-01"),
                    maxCores = 4,
                    minCellCount = 0) {

  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)

  `%>%` <- magrittr::`%>%`
  if (createCohorts) {
    ParallelLogger::logInfo("Creating Disease, Drug, and Whole Cohort Table")
    createAllCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  outputFolder = outputFolder,
                  yearStartDate=yearStartDate,
                  yearEndDate=yearEndDate)
  }


  if (runPrevalence) {
    ParallelLogger::logInfo("Running incidence and prevalence calculation")
    computePrevalence(yearStartDate = yearStartDate,
                      yearEndDate = yearEndDate,
                      monthStartDate = monthStartDate,
                      monthEndDate = monthEndDate,
                      databaseName = databaseName,
                      outputFolder = outputFolder,
                      minCellCount = minCellCount)
  }

  invisible(NULL)
}
