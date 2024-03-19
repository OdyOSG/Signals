# It is recommended to specify the environmental variable:
#
#    DATABASECONNECTOR_JAR_FOLDER=<folder-of-your-choice>
#
# in `.Renviron` located in the user's home directory.
# Then to install the required `postgresql` JDBC drivers, use:
#
#    DatabaseConnector::downloadJdbcDrivers(dbms = "postgresql")
#
# OHDSI shinydb signals read-only credentials
appConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste(keyring::key_get("signalsServer"),
                 keyring::key_get("signalsDatabase"),
                 sep = "/"),
  user = keyring::key_get("signalsUser"),
  password = keyring::key_get("signalsPassword"))

CohortDiagnostics::launchDiagnosticsExplorer(connectionDetails = appConnectionDetails,
                                      resultsDatabaseSchema = "signals_class_diagnostics")

CohortDiagnostics::launchDiagnosticsExplorer(connectionDetails = appConnectionDetails,
                                      resultsDatabaseSchema = "signals_outcome_diagnostics")
