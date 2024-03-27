# Generates:
#   classCohortsToCreate.csv
#   classTcosOfInterest.csv
#
# Requires at input:
#   classGeneratorList.csv
#   ExposuresOfInterest.csv
# 
# install.packages('https://github.com/OHDSI/ROhdsiWebApi/archive/refs/tags/v1.3.0.tar.gz')
# install.packages('https://github.com/OHDSI/FeatureExtraction/archive/refs/tags/v3.3.2.tar.gz')
# install.packages('https://github.com/OHDSI/CohortMethod/archive/refs/tags/v5.2.1.tar.gz')
# install.packages('https://github.com/OHDSI/MethodEvaluation/archive/refs/tags/v2.3.0.tar.gz')
# install.packages('https://github.com/OHDSI/CohortGenerator/archive/refs/tags/v0.6.0.tar.gz')
# install.packages('https://github.com/OHDSI/CohortDiagnostics/archive/refs/tags/v3.0.1.tar.gz')
# install.packages('https://github.com/OHDSI/CirceR/archive/refs/tags/v1.2.0.tar.gz')
# install.packages('https://github.com/OHDSI/MethodEvaluation/archive/refs/tags/v2.3.0.tar.gz')
# library(dplyr)
library(Signals)


# set ATLAS web API link
baseUrl = "https://atlas.odysseusinc.com/WebAPI"

keyring::key_set_with_value('baseUrl', password = baseUrl)
baseUrlWebApi <- keyring::key_get("baseUrl")

# code to query the Atlas Web API to get the base cohort (based on pre-defined ATLAS cohort)
# Make sure you download the right cohort
#baseCohort = baseCohort_orig
   baseCohort <- ROhdsiWebApi::getCohortDefinition(677, baseUrl = baseUrl)
   baseCohortJson <- RJSONIO::toJSON(baseCohort$expression, indent = 2, digits = 50)
   SqlRender::writeSql(baseCohortJson, targetFile = "inst/settings/baseCohort.json")
   saveRDS(baseCohort, file = "inst/settings/baseCohort.rds")

# Inclusion rules: Age == 1, Sex == 2, Race == 3, CVD == 4, obesity == 5, PriorMet == 6, NoMet == 7

baseCohort <- readRDS("inst/settings/baseCohort.rds")

generateStats <- TRUE

permutations <- readr::read_csv("extra/classGeneratorList.csv")
exposuresOfInterestTable <- readr::read_csv("inst/settings/ExposuresOfInterest.csv")
permutations <- inner_join(permutations, exposuresOfInterestTable %>% select(cohortId, shortName), by = c("targetId" = "cohortId"))

makeName <- function(permutation) {
  paste0(permutation$shortName, ": ", permutation$tar, ", ", permutation$met, " prior met, ",
         permutation$age, " age, ", permutation$sex, " sex, ", 
         permutation$obesity, " obesity")
}

makeShortName <- function(permutation) {
  paste0(permutation$shortName,
         ifelse(permutation$age == "any" &
                  permutation$sex == "any" &
                  permutation$obesity == "any", " main", ""),
         ifelse(permutation$tar == "ot2", " ot2", ""),
         ifelse(permutation$met == "no", " no-met", ""),
         ifelse(permutation$age != "any", paste0(" ", permutation$age, "-age"), ""),
         ifelse(permutation$sex != "any", paste0(" ", permutation$sex), ""),
         ifelse(permutation$obesity != "any", paste0(" ", permutation$obesity, "-obe"), ""))
}
#cohort  = baseCohort_orig 

permuteTC <- function(cohort, permutation, ingredientLevel = FALSE) {
  c1Id <- floor(permutation$comparator1Id / 10)
  c2Id <- floor(permutation$comparator2Id / 10)
  c3Id <- floor(permutation$comparator3Id / 10)
  delta <- 0
  
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
    cohort$ConceptSets[[13]] <- excludeSet
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
    items <- cohort$ConceptSets[[14]]$expression$items
    
    tmp <-
      lapply(items, function(item) {
        if (item$concept$CONCEPT_ID %in% includedConcepts) {
          NULL
        } else {
          item
        }
      })
    cohort$ConceptSets[[14]]$expression$items <- plyr::compact(tmp)
  } else {
    stop("Unknown TAR")
  }
  cohort$name <- makeName(permutation)
  return(cohort)
}



allCohortsSql <-
  do.call("rbind",
          lapply(1:nrow(permutations), function(i) {
            cohortDefinition <- permuteTC(baseCohort, permutations[i,])
            cohortSql <- ROhdsiWebApi::getCohortSql(cohortDefinition,
                                                    baseUrlWebApi,
                                                    generateStats = generateStats)
            return(cohortSql)
          }))

allCohortsJson <-
  do.call("rbind",
          lapply(1:nrow(permutations), function(i) {
            cohortDefinition <- permuteTC(baseCohort, permutations[i,])
            cohortJson <- RJSONIO::toJSON(cohortDefinition$expression, indent = 2, digits = 12)
            return(cohortJson)
          }))


permutations$json <- allCohortsJson
permutations$sql <- allCohortsSql

permutations <- permutations %>%
  mutate(name = paste0("ID", as.integer(cohortId)))

permutations$atlasName <- makeShortName(permutations)


# write class-level SQL and JSON files----
for (i in 1:nrow(permutations)) {
  row <- permutations[i,]
  sqlFileName <- file.path("inst/sql/sql_server/class", paste(row$name, "sql", sep = "."))
  SqlRender::writeSql(row$sql, sqlFileName)
  jsonFileName <- file.path("inst/cohorts/class", paste(row$name, "json", sep = "."))
  SqlRender::writeSql(row$json, jsonFileName)
}

# write class-level CohortsToCreate.csv file----
classCohortsToCreate <- permutations %>%
  mutate(atlasId = cohortId,
         name = paste0("class/", name)) %>%
  select(atlasId, atlasName, cohortId, name)

readr::write_csv(classCohortsToCreate, "inst/settings/classCohortsToCreate.csv")
 # tarId = "ot1"
 # metId = "with"
 # ageId = "any"
 # sexId = "any"
 # obesityId = "any"
# Make classTcosOfInterest.csv ----
makeTCOs <- function(tarId, metId, ageId, sexId, obesityId) {

  baseTs <- permutations %>%
    filter(tar == tarId,
           age == ageId, sex == sexId,
           obesity == obesityId, met == metId)

  tab <- as.data.frame(t(combn(baseTs$cohortId, m = 2)))
  names(tab) <- c("targetId", "comparatorId")
  tab$outcomeIds <- -1
  tab$excludedCovariateConceptIds <- NA

  tab <- tab %>% inner_join(permutations %>% select(cohortId, atlasName) %>% rename(targetId = cohortId),
                            by = "targetId") %>%
    rename(targetName = atlasName)

  tab <- tab %>% inner_join(permutations %>% select(cohortId, atlasName) %>% rename(comparatorId = cohortId),
                            by = "comparatorId") %>%
    rename(comparatorName = atlasName)

  return(tab)
}

classTcos <- rbind(
  # Order: tar, met, age, sex, obesity
  #
  # OT1
  # Main
  makeTCOs("ot1", "with", "any", "any",
           "any"),
  # Age
  makeTCOs("ot1", "with", "younger", "any",  "any"),
  makeTCOs("ot1", "with", "older", "any","any"),
  # Sex
  makeTCOs("ot1", "with", "any", "female", "any"),
  makeTCOs("ot1", "with", "any", "male", "any"),
  # obesity dz
  makeTCOs("ot1", "with", "any", "any",  "with"),
  #
  # OT2
  # Main
  makeTCOs("ot2", "with", "any", "any","any"),
  # Age
  makeTCOs("ot2", "with", "younger", "any",  "any"),
  makeTCOs("ot2", "with", "older", "any",  "any"),
  # Sex
  makeTCOs("ot2", "with", "any", "female",  "any"),
  makeTCOs("ot2", "with", "any", "male", "any"),

  # obesity dz
  makeTCOs("ot2", "with", "any", "any", "with")
)
readr::write_csv(classTcos, "inst/settings/classTcosOfInterest.csv")


# readr::write_csv(classCohortsToCreate, "inst/settings/testCohortsToCreate.csv")
# TODO Move to separate file






# WE NEED TO RUN THIS!
# Generate ingredient-level cohorts----
# NOTE: need to use `permutations` generated from previous class-level code!
permutations <- readr::read_csv("extra/classGeneratorList.csv")
exposuresOfInterestTable <- readr::read_csv("inst/settings/ExposuresOfInterest.csv")
#permutations <- inner_join(permutations, exposuresOfInterestTable %>%
#                             select(cohortId, shortName), by = c("targetId" = "cohortId"))

# permutations <- permutations %>% filter(cohortId == 101100000)
 classId <- 10

createPermutationsForDrugs <- function(classId){
  drugsForClass <- exposuresOfInterestTable %>% filter(cohortId > classId, cohortId < (classId + 10)) %>% mutate(classId = classId)
  permutationsForDrugs <- drugsForClass %>% left_join(permutations, by = c("classId" ="targetId")) %>%
    mutate(targetId = cohortId.x,
           cohortId = cohortId.y,
           includedConceptIds = conceptId,
           shortName = name) %>%
#    select(-type, -shortName.x, -order, -includedConceptIds, -conceptId, -cohortId.x, -cohortId.y, -shortName.y) %>%
    select(-type, -order, -includedConceptIds, -conceptId, -cohortId.x, -cohortId.y) %>%
        rowwise() %>%
    mutate(cohortId = as.integer(sub(paste0("^",classId), targetId, cohortId))) %>%
    mutate(name = paste0("ID", as.integer(cohortId)))
}

# try DPP4 I first...
#classIds <- c(10, 20, 30, 40)
#classIds = c(10)
# shift to SGLT2I
#classIds = c(30)

# March 2023 GLP1RAs:
classIds <- c(10,20,30,40)
#classId <- 10
permutationsForDrugs <- lapply(classIds, createPermutationsForDrugs) %>% bind_rows()

permutationsForDrugs$json <-
  do.call("rbind",
          lapply(1:nrow(permutationsForDrugs), function(i) {
            cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[i,], ingredientLevel = TRUE)
            cohortJson <- RJSONIO::toJSON(cohortDefinition$expression, indent = 2, digits = 10)
            return(cohortJson)
            #return(cohortDefinition)
          }))

permutationsForDrugs$sql <-
  do.call("rbind",
          lapply(1:nrow(permutationsForDrugs), function(i) {
            cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[i,], ingredientLevel = TRUE)
            cohortSql <- ROhdsiWebApi::getCohortSql(cohortDefinition,
                                                    baseUrlWebApi,
                                                    generateStats = generateStats)
          }))

 # cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[1,])

# save SQL and JSON files under class name (e.g., "DPP4I") folder
## need to create the directory for this class first

value = tolower(unique(permutationsForDrugs$class)[1])
perutationsForClass = permutationsForDrugs[tolower(permutationsForDrugs$class) == value,]
for (value in tolower(unique(permutationsForDrugs$class))){
    #if (!file.exists(file.path("inst/sql/sql_server", value))) {dir.create(file.path("instsql/sql_server", value))}
    #if (!file.exists(file.path("inst/sql/cohorts", value))) {dir.create(file.path("inst/sql/cohorts", value))}
    perutationsForClass = permutationsForDrugs[(permutationsForDrugs$class) == value,]
    for (i in 1:nrow(permutationsForDrugs)) {
      row <- permutationsForDrugs[i,]
      sqlFileName <- file.path("inst/sql/sql_server", tolower(row$class), paste(row$name, "sql", sep = "."))
      SqlRender::writeSql(row$sql, sqlFileName)
      jsonFileName <- file.path("inst/cohorts", tolower(row$class), paste(row$name, "json", sep = "."))
      SqlRender::writeSql(row$json, jsonFileName)
    }

}
# # sanity check --- inspect json and sql concept set IDs
# cohortID = 2
# conceptSetJson = CohortDiagnostics:::extractConceptSetsJsonFromCohortJson(permutationsForDrugs$json[cohortID])
# conceptSetSql = CohortDiagnostics:::extractConceptSetsSqlFromCohortSql(permutationsForDrugsc $sql[cohortID])

# save drug-level cohorts to cohortsToCreate.csv file
# only do this for DPP4I for now
# do it for SGLT2I
#this.class = permutationsForDrugs$class[1] %>% tolower() # this line defines name of drug class
# 3/24 do
classNames = unique(permutationsForDrugs$class)
permutationsForDrugs$atlasName = permutationsForDrugs$shortName
for (value in classNames){
  drugCohortsToCreate <- permutationsForDrugs %>% 
    filter(class == value) %>%
    mutate(atlasId = cohortId,
           name = sprintf('%s/%s',value,name)) %>%
      select(atlasId, atlasName, cohortId, name)


  filePath = "inst/settings/"
  fileName = sprintf('%sCohortsToCreate.csv', value)
  readr::write_csv(drugCohortsToCreate,
                   file.path(filePath, fileName))  
}


# this creates CohortsToCreate.csv file; can eye-ball to check

# check out some example cohort definitions
# (updating ingredient name for each drug class)
# permutationsForDrugs$atlasName <- makeShortName(permutationsForDrugs)
# printCohortDefinitionFromNameAndJson(name = "canagliflozin main",
#                                      json = permutationsForDrugs$json[1]) # change this to one GLP1RA name instead!
# printCohortDefinitionFromNameAndJson(name = "canagliflozin younger-age",
#                                      json = permutationsForDrugs$json[2])

tarId   ="ot1"
metId   ="with"
ageId   ="any"
sexId   ="any"
obesityId ="any"
#make drug-level TCOs
makeTCOsDrug <- function(tarId, metId, ageId, sexId, obesityId) {

  baseTs <- permutationsForDrugsclass %>%
    filter(tar == tarId,
           age == ageId, sex == sexId, obesity == obesityId, met == metId)

  tab <- as.data.frame(t(combn(baseTs$cohortId, m = 2)))
  names(tab) <- c("targetId", "comparatorId")
  tab$outcomeIds <- -1
  tab$excludedCovariateConceptIds <- NA

  tab <- tab %>% inner_join(permutationsForDrugsclass %>% select(cohortId, atlasName) %>% rename(targetId = cohortId),
                            by = "targetId") %>%
    rename(targetName = atlasName)

  tab <- tab %>% inner_join(permutationsForDrugsclass %>% select(cohortId, atlasName) %>% rename(comparatorId = cohortId),
                            by = "comparatorId") %>%
    rename(comparatorName = atlasName)

  return(tab)
}
for (this.class in unique(permutationsForDrugs$class)){
  permutationsForDrugsclass = permutationsForDrugs %>% filter(tolower(class) == tolower(this.class)) 
  drugTcos <- rbind(
    # Order: tar, met, age, sex, obesity
    # OT1
    # Main
    makeTCOsDrug("ot1", "with", "any", "any", "any"),
    # Age
    makeTCOsDrug("ot1", "with", "younger", "any", "any"),
    makeTCOsDrug("ot1", "with", "older", "any", "any"),
    # Sex
    makeTCOsDrug("ot1", "with", "any", "female", "any"),
    makeTCOsDrug("ot1", "with", "any", "male", "any"),
    # obesity dz
    makeTCOsDrug("ot1", "with", "any", "any",  "with"),
    makeTCOsDrug("ot1", "with", "any", "any", "without"),
    #
    # OT2
    # Main
    makeTCOsDrug("ot2", "with", "any", "any",  "any"),
    # Age
    makeTCOsDrug("ot2", "with", "younger", "any",  "any"),
    makeTCOsDrug("ot2", "with", "older", "any", "any"),
    # Sex
    makeTCOsDrug("ot2", "with", "any", "female",  "any"),
    makeTCOsDrug("ot2", "with", "any", "male", "any"),
  
    # obesity dz
    makeTCOsDrug("ot2", "with", "any", "any", "with"),
    makeTCOsDrug("ot2", "with", "any", "any", "without")
  )
  
  # save TCOs 
  
    
  
  drugTcos$targetId = as.character(drugTcos$targetId)
  grepstring = paste0("^",this.class)
  drugs <- drugTcos[grepl(grepstring,drugTcos$targetId),]
  filePath = "inst/settings/"
  fileName = sprintf('%sTcosOfInterest.csv', this.class)
  
  readr::write_csv(drugTcos, file.path(filePath, fileName))
}
