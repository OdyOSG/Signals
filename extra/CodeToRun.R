renv::restore(packages = "renv")
install.packages("checkmate")
renv::status()
renv::restore()
library(Signals)

#Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="s:/DatabaseDrivers")

# Run-once: set-up your database driver
#DatabaseConnector::downloadJdbcDrivers(dbms = "postgresql")

# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "C:/AndromedaTemp")

# Maximum number of cores to be used:
maxCores <- min(4, parallel::detectCores()) # Or more depending on your hardware

# Minimum cell count when exporting data:
minCellCount <- 5

# Patch for Oracle (if necessary)
oracleTempSchema <- NULL

# The folder where the study intermediate and result files will be written:
outputFolder <- "results"

# Details for connecting to the server:
# See ?DatabaseConnector::createConnectionDetails for help
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = "ohdsi",
  password = "ohdsi",
  server = "testnode.arachnenetwork.com/synpuf_110k",
  port = 5441
)

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "cdm_531"

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "jmt_signals.dbo"
tablePrefix <- "signals_study"

# Some meta-information that will be used by the export function:
databaseId <- "Synpuf"
databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."

# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
options(sqlRenderTempEmulationSchema = NULL)

# Feasibility assessment ---------------------------------------------------------
Signals::assessPhenotypes(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 outputFolder = outputFolder,
                 tablePrefix = tablePrefix,
                 #indicationId = 'glp1ra',
                 databaseId = databaseId,
                 databaseName = databaseName,
                 databaseDescription = databaseDescription,
                 createExposureCohorts = TRUE,
                 runExposureCohortDiagnostics = TRUE,
                 createOutcomeCohorts = TRUE,
                 runOutcomeCohortDiagnostics = TRUE)

#uploadPhenotypeResults(cohort = "class",
#                       outputFolder, privateKeyFileName = "s:/private.key", userName = "signals")
#uploadPhenotypeResults(cohort = "outcome",
#                       outputFolder, privateKeyFileName = "s:/private.key", userName = "signals")

CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = file.path(outputFolder, "class/cohortDiagnosticsExport"))
SignalsCohortExplorer::launchCohortExplorer(cohorts = "class",
                                               dataFolder = file.path(outputFolder, "class/cohortDiagnosticsExport"))

CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = file.path(outputFolder, "outcome/cohortDiagnosticsExport"))
SignalsCohortExplorer::launchCohortExplorer(cohorts = "outcome",
                                               dataFolder = file.path(outputFolder, "outcome/cohortDiagnosticsExport"))

assessPropensityModels(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       outputFolder = outputFolder,
                       indicationId = "class",
                       tablePrefix = tablePrefix,
                       databaseId = databaseId,
                       maxCores = maxCores)

uploadPsAssessmentResults(cohort = "class",
                          outputFolder, privateKeyFileName = "s:/private.key", userName = "signals")

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        outputFolder = outputFolder,
        indicationId = "class",
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        tablePrefix = tablePrefix,
        createExposureCohorts = FALSE, # It is not necessary to re-generate the exposure cohorts
        createOutcomeCohorts = TRUE, # It is necessary to re-generate the outcome cohorts
        fetchAllDataFromServer = TRUE,
        generateAllCohortMethodDataObjects = TRUE,
        runCohortMethod = TRUE,
        computeCovariateBalance = TRUE,
        exportToCsv = TRUE,
        maxCores = maxCores)
