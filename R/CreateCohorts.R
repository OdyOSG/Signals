.createCohorts <- function(connection,
                           cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           oracleTempSchema,
                           outputFolder,
                           indicationId) {

  # Create study cohort table structure:
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "Signals",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

  # # Instantiate cohorts:
  # pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Signals")
  # cohortsToCreate <- read.csv(pathToCsv)
  # cohortsToCreate <- cohortsToCreate[cohortsToCreate$indicationId == indicationId, ]
  # for (i in 1:nrow(cohortsToCreate)) {
  #   writeLines(paste("Creating cohort:", cohortsToCreate$name[i]))
  #   sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortsToCreate$name[i], ".sql"),
  #                                            packageName = "Signals",
  #                                            dbms = attr(connection, "dbms"),
  #                                            oracleTempSchema = oracleTempSchema,
  #                                            cdm_database_schema = cdmDatabaseSchema,
  #                                            vocabulary_database_schema = vocabularyDatabaseSchema,
  #                                            target_database_schema = cohortDatabaseSchema,
  #                                            target_cohort_table = cohortTable,
  #                                            target_cohort_id = cohortsToCreate$cohortId[i])
  #   DatabaseConnector::executeSql(connection, sql)
  # }
}
