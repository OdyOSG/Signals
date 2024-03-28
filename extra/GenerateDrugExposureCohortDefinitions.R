# generates drug-level exposure cohorts
# derived from `GenerateExposureCohortDefinitions.R`

# Generates:
#   [className]CohortsToCreate.csv
#   [className]TcosOfInterest.csv
#
# Requires at input:
#   classGeneratorList.csv
#   ExposuresOfInterest.csv
library(dplyr)
# load in base cohort
baseCohort <- jsonlite::read_json("inst/settings/baseCohort.json")
# start from drug-level permutations and exposures
permutations <- read.csv("extra/classGeneratorList.csv")
exposuresOfInterestTable <- readr::read_csv("inst/settings/ExposuresOfInterest.csv")
## DEBUG: fill in `includedConceptIds` for drug-level exposure cohorts
exposuresOfInterestTable <- exposuresOfInterestTable %>%
  mutate(includedConceptIds = if_else(type == "Drug", as.character(conceptId), includedConceptIds))
permutations <- inner_join(permutations, exposuresOfInterestTable %>%
                             select(cohortId, shortName),
                           by = c("targetId" = "cohortId"))
# helper function to create cohort names
makeName <- function(permutation) {
  paste0(permutation$shortName, ": ",
         permutation$tar, ", ",
         permutation$met, " prior met, ",
         permutation$age, " age, ",
         permutation$sex, " sex, ",
         permutation$obesity, " obesity")
}
# another helper function to generate `shortName` (used as `atlasName` for creating cohort)
makeShortName <- function(permutation) {
  paste0(permutation$shortName,
         ifelse(permutation$age == "any" &
                  permutation$sex == "any" &
                  permutation$obesity == "any", " main", ""),
         ifelse(permutation$tar == "ot2", " ot2", ""),
         ifelse(permutation$met == "no", " no-met", ""),
         ifelse(permutation$age != "any", paste0(" ", permutation$age, "-age"), ""),
         ifelse(permutation$sex != "any", paste0(" ", permutation$sex), ""),
         ifelse(permutation$obesity != "any", paste0(" ", permutation$obesity, "-obesity"), ""))
}

# function to create permute drug-level target-comparator pairs----
createPermutationsForDrugs <- function(classId){
  drugsForClass <- exposuresOfInterestTable %>%
    filter(cohortId > classId, cohortId < (classId + 10)) %>%
    mutate(classId = classId)
  permutationsForDrugs <- drugsForClass %>%
    left_join(permutations, by = c("classId" ="targetId")) %>%
    mutate(targetId = cohortId.x,
           cohortId = cohortId.y,
           includedConceptIds = conceptId,
           shortName = name) %>%
    select(-type, -shortName.x, -order, -includedConceptIds, -conceptId, -cohortId.x, -cohortId.y, -shortName.y) %>%
    rowwise() %>%
    mutate(cohortId = as.integer(sub(paste0("^",classId), targetId, cohortId))) %>%
    mutate(name = paste0("ID", as.integer(cohortId)))
}
# cohort = baseCohort
# permutation = permutationsForDrugs[1,] 
# ingredientLevel = TRUE
# another function to actually permute the target-comparator pairs----
permuteTC <- function(cohort, permutation, ingredientLevel = FALSE) {
  c1Id <- floor(permutation$comparator1Id / 10)
  c2Id <- floor(permutation$comparator2Id / 10)
  c3Id <- floor(permutation$comparator3Id / 10)
  delta <- 0
  cohort$ConceptSets[[17]] <- cohort$ConceptSets[[12]]

  if (ingredientLevel) {

    targetId <- permutation$targetId
    classId <- floor(targetId / 10)
    classSet <- cohort$ConceptSets[[classId]]
    targetSet <- classSet
    excludeSet <- classSet
    drugInfo <- exposuresOfInterestTable %>% filter(cohortId == targetId)
    name <- drugInfo %>% pull(name)
    conceptId <- drugInfo %>% pull(conceptId)
    targetSet$name <- name
    excludeSet$name <- paste(excludeSet$name, "excluding", name)
    excludeSet$id <- 17
    targetSet$expression$items <- plyr::compact(
      lapply(targetSet$expression$items, function(item) {
        if (item$concept$CONCEPT_ID == conceptId) {
          item
        } else {
          NULL
        }
      }))
    excludeSet$expression$items <- plyr::compact(
      lapply(excludeSet$expression$items, function(item) {
        if (item$concept$CONCEPT_ID != conceptId) {
          item
        } else {
          NULL
        }
      }))
    cohort$ConceptSets[[classId]] <- targetSet
    cohort$ConceptSets[[12]] <- excludeSet
    #cohort$ConceptSets[[13]] <- excludeSet
    tId <- classId
    cohort$InclusionRules[[1]]$expression$CriteriaList[[1]]$Criteria$DrugExposure$CodesetId <- 17
  } else {
    cohort$InclusionRules[[1]] <- NULL
    delta <- delta + 1
    cohort$ConceptSets[[15]] <- NULL
    tId <- floor(permutation$targetId / 10)
  }
  cohort$PrimaryCriteria$CriteriaList[[1]]$DrugExposure$CodesetId <- tId
  target <- 2 - delta - 1
  cohort$InclusionRules[[target + tId]] <- NULL
  cohort$EndStrategy$CustomEra[1] <- tId
  # print(2)

  delta <- delta + 1
  age <- 7 - delta
  if (permutation$age == "younger") {
    cohort$InclusionRules[[age]]$name <- "Lower age group"
    cohort$InclusionRules[[age]]$description <- NULL
    cohort$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]]$Age$Op <- "lte"
    cohort$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]]$Age$Value <- 55
  } else if (permutation$age == "older") {
    cohort$InclusionRules[[age]]$expression$name <- "Older age group"
    cohort$InclusionRules[[age]]$expression$description <- NULL
    cohort$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]]$Age$Op <- "gt"
    cohort$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]]$Age$Value <- 55
  } else if (permutation$age == "any") {
    cohort$InclusionRules[[age]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown age type")
  }
  # print(3)
  sex <- 8 - delta
  if (permutation$sex == "female") {
    cohort$InclusionRules[[sex]]$name <- "Female stratum"
    cohort$InclusionRules[[sex]]$description <- NULL
    cohort$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_ID <- 8532
    cohort$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_NAME <- "female"
  } else if (permutation$sex == "male") {
    cohort$InclusionRules[[sex]]$name <- "Male stratum"
    cohort$InclusionRules[[sex]]$description <- NULL
    cohort$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_ID <- 8507
    cohort$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_NAME <- "male"
  } else if (permutation$sex == "any") {
    cohort$InclusionRules[[sex]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown sex type")
  }
  # print(4)
  obesity <- 12 - delta
  if (permutation$obesity == "without") {
    cohort$InclusionRules[[obesity]]$name <- "Without obesity"
    cohort$InclusionRules[[obesity]]$description <- 'no info about obesiy all days before index'
    cohort$InclusionRules[[obesity]]$expression$Type <- 'ALL'
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Type <- 0
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Count <- 0

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[2]]$Occurrence$Type <- 0
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[2]]$Occurrence$Count <- 0

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[3]]$Occurrence$Type <- 0
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[3]]$Occurrence$Count <- 0

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[4]]$Occurrence$Type <- 0
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[4]]$Occurrence$Count <- 0

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[5]]$Occurrence$Type <- 0
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[5]]$Occurrence$Count <- 0

  } else if (permutation$obesity == "with") {
    cohort$InclusionRules[[obesity]]$name <- "with obesity"
    cohort$InclusionRules[[obesity]]$description <- 'info about obesiy all days before index'

    cohort$InclusionRules[[obesity]]$expression$Type <- 'ANY'
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Type <- 2
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Count <- 1

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[2]]$Occurrence$Type <- 2
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[2]]$Occurrence$Count <- 1

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[3]]$Occurrence$Type <- 2
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[3]]$Occurrence$Count <- 1

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[4]]$Occurrence$Type <- 2
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[4]]$Occurrence$Count <- 1

    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[5]]$Occurrence$Type <- 2
    cohort$InclusionRules[[obesity]]$expression$CriteriaList[[5]]$Occurrence$Count <- 1

  } else if (permutation$obesity == "any") {
    cohort$InclusionRules[[obesity]] <- NULL
    delta <- delta  + 1
  } else {
    stop("Unknown obesity type")
  }
  # print(5)
  #
  met <- 10 - delta
  if (permutation$met == "with") {
    # Do nothing
    cohort$InclusionRules[[met]]$description <- NULL
    cohort$InclusionRules[[met + 1]] <- NULL
    delta <- delta + 1
  } else if (permutation$met == "no") {
    cohort$InclusionRules[[met + 1]]$description <- NULL
    cohort$InclusionRules[[met]] <- NULL
    delta <- delta + 1
  } else if (permutation$met == "test") {
    cohort$InclusionRules[[met]]$description <- NULL
    cohort$InclusionRules[[met]]$expression$Type <- "AT_MOST"
    cohort$InclusionRules[[met]]$expression$Count <- 0
    cohort$InclusionRules[[met + 1]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown metformin type")
  }
  # print(6)
  insulin <- 11 - delta
  cohort$InclusionRules[[insulin]]$description <- NULL

  if (permutation$tar == "ot1") {
    cohort$CensoringCriteria <- list()
  } else if (permutation$tar == "ot2") {

    includedConcepts <- as.numeric(unlist(strsplit(exposuresOfInterestTable %>%
                                                     filter(cohortId == permutation$targetId) %>%
                                                     pull(includedConceptIds),
                                                   ";")))
    items <- cohort$ConceptSets[[17]]$expression$items
    tmp <-
      lapply(items, function(item) {
        if (item$concept$CONCEPT_ID %in% includedConcepts) {
          NULL
        } else {
          item
        }
      })
    cohort$ConceptSets[[17]]$expression$items <- plyr::compact(tmp)
  } else {
    stop("Unknown TAR")
  }
  cohort$name <- makeName(permutation)
  return(cohort)
}



# !! #------
# specify drug class here
# e.g., GLP1RAs:
#classIds = c(20)

# or, DPP4i
#classIds = c(10)

# also, for SU
#classIds = c(40)

# DEBUG: test with SGLT2i
classIds = c(10)

# then create permutations for the desired drug class
permutationsForDrugs <- lapply(classIds, createPermutationsForDrugs) %>%
  bind_rows()
# permutationsForDrugs <- permutationsForDrugs[1:10, ]
# cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[1,], ingredientLevel = TRUE)
# generate the needed JSON and SQL files to create the drug-level exposure cohorts
permutationsForDrugs$json <-
  do.call("rbind",
          lapply(1:nrow(permutationsForDrugs), function(i) {
            cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[i,], ingredientLevel = TRUE)
            cohortJson <- RJSONIO::toJSON(cohortDefinition, indent = 2, digits = 10)
            print(i)
            return(cohortJson)
          }))

generateOptions <- CirceR::createGenerateOptions(generateStats = TRUE)

# Build the SQL query from the cohort definition
#cohortSql <- CirceR::buildCohortQuery(cohortJson, generateOptions)


permutationsForDrugs$sql <-
  do.call("rbind",
          lapply(1:nrow(permutationsForDrugs), function(i) {
            print(i)
            cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[i,], ingredientLevel = TRUE)
            cohortSql <- CirceR::buildCohortQuery(
              as.character(RJSONIO::toJSON(cohortDefinition)),
              CirceR::createGenerateOptions(generateStats = TRUE))
            
            return(cohortSql)
          }))
# save SQL and JSON files under class name (e.g., "DPP4I") folder
## need to create the directory for this class first
this.class = tolower(permutationsForDrugs[1,]$class)
if(!dir.exists(file.path("inst/sql/sql_server", this.class))){
  dir.create(file.path("inst/sql/sql_server", this.class))
}
if(!dir.exists(file.path("inst/cohorts", this.class))){
  dir.create(file.path("inst/cohorts", this.class))
}
## and then save JSON and SQL files
for (i in 1:nrow(permutationsForDrugs)) {
  row <- permutationsForDrugs[i,]
  sqlFileName <- file.path("inst/sql/sql_server", tolower(row$class), paste(row$name, "sql", sep = "."))
  SqlRender::writeSql(row$sql, sqlFileName)
  jsonFileName <- file.path("inst/cohorts", tolower(row$class), paste(row$name, "json", sep = "."))
  print(jsonFileName)
  SqlRender::writeSql(row$json, jsonFileName)
}
# save drug-level cohorts to [className]cohortsToCreate.csv file
this.class = permutationsForDrugs$class[1] %>% tolower() # this line defines name of drug class
permutationsForDrugs$atlasName <- makeShortName(permutationsForDrugs) # add `atlasName` as short cohort name
drugCohortsToCreate <- permutationsForDrugs %>%
  mutate(atlasId = cohortId,
         name = sprintf('%s/%s',this.class,name)) %>%
  select(atlasId, atlasName, cohortId, name) # creates the cohortsToCreate table
filePath = "inst/settings/"
fileName = sprintf('%sCohortsToCreate.csv', this.class) # file path
readr::write_csv(drugCohortsToCreate,
                 file.path(filePath, fileName)) # write the file as `[className]cohortsToCreate.csv`
# check out some example cohort definitions
## PLEASE UPDATE INGREDIENT NAME FOR EACH DRUG-CLASS!
## (examples here are within the GLP1RA class)
# #permutationsForDrugs$atlasName <- makeShortName(permutationsForDrugs)
# printCohortDefinitionFromNameAndJson(name = "albiglutide main",
#                                      json = permutationsForDrugs$json[1])
# printCohortDefinitionFromNameAndJson(name = "albiglutide younger-age",
#                                      json = permutationsForDrugs$json[2])
# print out an example for SGLT2i
cohortName = "canagliflozin main ot2"
jsonFile = drugCohortsToCreate %>% filter(atlasName == cohortName) %>% pull(name)
# printCohortDefinitionFromNameAndJson(name = "canagliflozin main ot2",
#                                      json = SqlRender::readSql(paste0("inst/cohorts/", jsonFile, ".json")))
# generate drug-level TCOs-----
# function to create TCO triplets
makeTCOsDrug <- function(
    tarId,
    metId,
    ageId,
    sexId,
    obesityId
    ) {
  baseTs <- permutationsForDrugs %>%
    filter(
      tar == tarId,
      age == ageId,
      sex == sexId,
      obesity == obesityId,
      met == metId
      )
  tab <- as.data.frame(t(combn(baseTs$cohortId, m = 2)))
  names(tab) <- c("targetId", "comparatorId")
  tab$outcomeIds <- -1
  tab$excludedCovariateConceptIds <- NA
  tab <- tab %>%
    inner_join(permutationsForDrugs %>%
                 select(cohortId, atlasName) %>%
                 rename(targetId = cohortId),
                            by = "targetId") %>%
    rename(targetName = atlasName)
  tab <- tab %>%
    inner_join(
      permutationsForDrugs %>%
        select(cohortId, atlasName) %>%
        rename(comparatorId = cohortId),
                            by = "comparatorId") %>%
    rename(comparatorName = atlasName)
  return(tab)
}
# use the function to create all TCOs
drugTcos <- rbind(
  # Order: tar, met, age, sex, obesity
  #
  # OT1
  # Main
  makeTCOsDrug("ot1", "with", "any", "any",  "any"),
  # Age
  makeTCOsDrug("ot1", "with", "younger", "any",  "any"),
  makeTCOsDrug("ot1", "with", "older", "any",  "any"),
  # Sex
  makeTCOsDrug("ot1", "with", "any", "female",  "any"),
  makeTCOsDrug("ot1", "with", "any", "male",  "any"),
  # obesity dz
  makeTCOsDrug("ot1", "with", "any", "any", "without"),
  makeTCOsDrug("ot1", "with", "any", "any",  "with"),
  #
  # OT2
  # Main
  makeTCOsDrug("ot2", "with", "any", "any",  "any"),
  # Age
  makeTCOsDrug("ot2", "with", "younger", "any",  "any"),
  makeTCOsDrug("ot2", "with", "older", "any",  "any"),
  # Sex
  makeTCOsDrug("ot2", "with", "any", "female",  "any"),
  makeTCOsDrug("ot2", "with", "any", "male",  "any"),
  # obesity dz
  makeTCOsDrug("ot2", "with", "any", "any",  "without"),
  makeTCOsDrug("ot2", "with", "any", "any",  "with")
)
# save TCOs for the desired drug class
this.class = tolower(permutationsForDrugs$class[1])
filePath = "inst/settings/"
fileName = sprintf('%sTcosOfInterest.csv', this.class) # file path
readr::write_csv(drugTcos, file.path(filePath, fileName)) # save it

