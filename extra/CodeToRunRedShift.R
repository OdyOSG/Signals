rJava::.jinit(parameters="-Xmx100g", force.init = TRUE)
options(java.parameters = c("-Xms200g", "-Xmx200g"))

library(Signals)

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="d:/Drivers")
options(andromedaTempFolder = "E:/andromedaTemp")
oracleTempSchema <- NULL


cdmDatabaseSchema <- "cdm_truven_mdcd_v2321" 
serverSuffix <- "truven_mdcd"
cohortDatabaseSchema <- "scratch_fbu2"
databaseId<- "MDCD"
databaseName <- "IBM Health MarketScan® Multi-State Medicaid Database"
databaseDescription <- "IBM MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data."
tablePrefix <- "signals_mdcd"
outputFolder <- "E:/SignalsOutput_mdcd_drug2"


cdmDatabaseSchema <- "cdm_optum_extended_dod_v2323" 
serverSuffix <- "optum_extended_dod"
cohortDatabaseSchema <- "scratch_fbu2"
databaseId <- "OptumDod"
databaseName <- "Optum Clinformatics Extended Data Mart - Date of Death (DOD)"
databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006).  The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years.  It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum.  This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."
tablePrefix <- "signals_optum_dod"
outputFolder <- "E:/SignalsOutput_optum_dod_drug2"


conn <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("epi_server"), "/", !!serverSuffix),
  port = 5439,
  user = keyring::key_get("redshiftUser"),
  password = keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
  pathToDriver = 'D:/Drivers')

# ## DO NOT RUN: connecting to database
# connection = DatabaseConnector::connect(conn)
# DatabaseConnector::disconnect(connection)

# Feasibility assessment ---------------------------------------------------------
assessPhenotypes(connectionDetails = conn,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 outputFolder = outputFolder,
                 tablePrefix = tablePrefix,
                 indicationId = 'drug',
                 databaseId = databaseId,
                 databaseName = databaseName,
                 databaseDescription = databaseDescription,
                 createExposureCohorts = TRUE,
                 runExposureCohortDiagnostics = TRUE,
                 createOutcomeCohorts = TRUE,
                 runOutcomeCohortDiagnostics = FALSE) #TRUE)

assessPropensityModels(connectionDetails = conn,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       tablePrefix = tablePrefix,
                       indicationId = 'drug',
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       outputFolder = outputFolder,
                       databaseId = databaseId,
                       maxCores = 16)


# Cohort Explorer
# to check one example cohort
# CohortDiagnostics::launchCohortExplorer(connectionDetails = conn,
#                                         cdmDatabaseSchema = cdmDatabaseSchema,
#                                         cohortDatabaseSchema = cohortDatabaseSchema,
#                                         cohortTable = paste0(tablePrefix,'_',
#                                                              'dpp4i','_cohort'),
#                                         cohortId = 111100000)

# check out cohort table rows first to make sure things are there
# connection = connect(conn)
# sql = 'SELECT *
#       FROM scratch_fbu2.signals_mdcr_dpp4i_cohort'
# sql <- SqlRender::translate(sql, targetDialect = connection@dbms)
# allCohorts <- DatabaseConnector::querySql(connection, sql)


## test code? only run one section
# execute(connectionDetails = connectionDetails,
#         cdmDatabaseSchema = cdmDatabaseSchema,
#         oracleTempSchema = oracleTempSchema,
#         cohortDatabaseSchema = cohortDatabaseSchema,
#         outputFolder = outputFolder,
#         indicationId = "class",
#         databaseId = databaseId,
#         databaseName = databaseName,
#         databaseDescription = databaseDescription,
#         tablePrefix = tablePrefix,
#         createExposureCohorts = FALSE,
#         createOutcomeCohorts = FALSE,
#         fetchAllDataFromServer = FALSE,
#         generateAllCohortMethodDataObjects = FALSE,
#         runCohortMethod = FALSE,
#         runSections = c(1),
#         computeCovariateBalance = FALSE,
#         exportToCsv = TRUE,
#         maxCores = 16)

## full-on execution of CES; run all sections of analyses
execute(connectionDetails = conn,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        outputFolder = outputFolder,
        indicationId = "drug",
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        minCohortSize = 1000,
        tablePrefix = tablePrefix,
        createExposureCohorts = FALSE,
        createOutcomeCohorts = FALSE,
        fetchAllDataFromServer = FALSE,
        generateAllCohortMethodDataObjects = FALSE,
        runCohortMethod = TRUE,
        runSections = c(1:6),
        computeCovariateBalance = TRUE,
        exportToCsv = TRUE,
        maxCores = 10)

# re-run compute covariate and results export
exportSettings = Signals:::createExportSettings(exportAnalysisInfo = FALSE,
                                                   exportStudyResults = FALSE,
                                                   exportStudyDiagnostics = TRUE,
                                                   exportDateTimeInfo = FALSE,
                                                   exportBalanceOnly = TRUE)
execute(
  connectionDetails = conn,
  cdmDatabaseSchema = cdmDatabaseSchema,
  oracleTempSchema = oracleTempSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  outputFolder = outputFolder,
  indicationId = "drug",
  databaseId = databaseId,
  databaseName = databaseName,
  databaseDescription = databaseDescription,
  tablePrefix = tablePrefix,
  createExposureCohorts = FALSE,
  createOutcomeCohorts = FALSE,
  fetchAllDataFromServer = FALSE,
  generateAllCohortMethodDataObjects = FALSE,
  runCohortMethod = FALSE,
  runSections = c(1:6),
  computeCovariateBalance = TRUE,
  exportToCsv = TRUE,
  exportSettings = exportSettings,
  maxCores = 16
)


#### test staged execution code on a big JnJ data source ----
# prepareStagedExecution(originalOutputFolder = outputFolder,
#                        outputFolderHeader = outputFolder,
#                        indicationId = "drug",
#                        stages = 10)
#
# ## try this out (only run the first 1/10 of target-comparator pairs):
# newOutputFolder1 = file.path(paste0(outputFolder, "-1"))
#
# indicationId = "drug"
#
# execute(connectionDetails = conn,
#         cdmDatabaseSchema = cdmDatabaseSchema,
#         oracleTempSchema = oracleTempSchema,
#         cohortDatabaseSchema = cohortDatabaseSchema,
#         outputFolder = newOutputFolder1,
#         indicationId = indicationId,
#         databaseId = databaseId,
#         databaseName = databaseName,
#         databaseDescription = databaseDescription,
#         tablePrefix = tablePrefix,
#         createExposureCohorts = FALSE,
#         createOutcomeCohorts = FALSE,
#         createPairedExposureSummary = FALSE,
#         fetchAllDataFromServer = FALSE,
#         generateAllCohortMethodDataObjects = TRUE,
#         runCohortMethod = TRUE,
#         computeCovariateBalance = TRUE,
#         exportToCsv = TRUE,
#         maxCores = 16)
