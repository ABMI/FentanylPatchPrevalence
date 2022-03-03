# This file is part of CERVELLOPrevalence

createCohorts <- function(connection,
                          cdmDatabaseSchema,
                          vocabularyDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          outputFolder) {

  # Create study cohort table structure:
  sql <- SqlRender::readSql(file.path("inst", "sql", "CreateCohortTable.sql"))
  sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)


  # Instantiate cohorts:
  cohortsToCreate <- read.csv(file.path("inst", "settings", "CohortsToCreate.csv"))
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Creating", cohortsToCreate$cohortType[i], "cohort:", cohortsToCreate$name[i]))
    sql <- SqlRender::readSql(file.path("inst", "sql", paste0(cohortsToCreate$name[i], ".sql")))
    sql <- SqlRender::render(sql,
                              cdm_database_schema = cdmDatabaseSchema,
                              vocabulary_database_schema = cdmDatabaseSchema,
                              target_database_schema = cohortDatabaseSchema,
                              target_cohort_table = cohortTable,
                              target_cohort_id = cohortsToCreate$cohortId[i])
    sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
  }

  # Fetch cohort counts:
  sql <- "SELECT cohort_definition_id, COUNT(distinct subject_id) AS incidencecounts FROM @cohort_database_schema.@cohort_table GROUP BY cohort_definition_id"
  sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  incidencecounts <- DatabaseConnector::querySql(connection, sql)
  names(incidencecounts) <- SqlRender::snakeCaseToCamelCase(names(incidencecounts))
  counts <-  merge(data.frame(cohortDefinitionId = cohortsToCreate$cohortId, cohortName  = cohortsToCreate$name),
                   incidencecounts, by = "cohortDefinitionId")
  write.csv(counts, file.path(outputFolder, "CohortCounts.csv"), row.names = F)
}




