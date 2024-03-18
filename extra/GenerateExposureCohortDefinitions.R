# Generates:
#   classCohortsToCreate.csv
#   classTcosOfInterest.csv
#
# Requires at input:
#   classGeneratorList.csv
#   ExposuresOfInterest.csv

install.packages('https://github.com/OHDSI/ROhdsiWebApi/archive/refs/tags/v1.3.0.tar.gz')
install.packages('https://github.com/OHDSI/FeatureExtraction/archive/refs/tags/v3.3.2.tar.gz')
install.packages('https://github.com/OHDSI/CohortMethod/archive/refs/tags/v5.2.1.tar.gz')
install.packages('https://github.com/OHDSI/MethodEvaluation/archive/refs/tags/v2.3.0.tar.gz')
install.packages('https://github.com/OHDSI/CohortGenerator/archive/refs/tags/v0.6.0.tar.gz')
install.packages('https://github.com/OHDSI/CohortDiagnostics/archive/refs/tags/v3.0.1.tar.gz')
install.packages('https://github.com/OHDSI/CirceR/archive/refs/tags/v1.2.0.tar.gz')
install.packages('https://github.com/OHDSI/MethodEvaluation/archive/refs/tags/v2.3.0.tar.gz')
library(dplyr)
library(Signals)


# set ATLAS web API link
baseUrl = "https://atlas.odysseusinc.com/WebAPI"

keyring::key_set_with_value('baseUrl', password = baseUrl)
baseUrlWebApi <- keyring::key_get("baseUrl")

# code to query the Atlas Web API to get the base cohort (based on pre-defined ATLAS cohort)
# Make sure you download the right cohort
#baseCohort = baseCohort_orig
   baseCohort <- ROhdsiWebApi::getCohortDefinition(674, baseUrl = baseUrl)
   baseCohortJson <- RJSONIO::toJSON(baseCohort$expression, indent = 2, digits = 50)
   SqlRender::writeSql(baseCohortJson, targetFile = "inst/settings/baseCohort.json")
   saveRDS(baseCohort, file = "inst/settings/baseCohort.rds")

# Inclusion rules: Age == 1, Sex == 2, Race == 3, CVD == 4, obese == 5, PriorMet == 6, NoMet == 7

baseCohort <- readRDS("inst/settings/baseCohort.rds")

generateStats <- TRUE

permutations <- readr::read_csv("extra/classGeneratorList.csv")
exposuresOfInterestTable <- readr::read_csv("inst/settings/ExposuresOfInterest.csv")
permutations <- inner_join(permutations, exposuresOfInterestTable %>% select(cohortId, shortName), by = c("targetId" = "cohortId"))

makeName <- function(permutation) {
  paste0(permutation$shortName, ": ", permutation$tar, ", ", permutation$met, " prior met, ",
         permutation$age, " age, ", permutation$sex, " sex, ", 
         permutation$obese, " obese")
}

makeShortName <- function(permutation) {
  paste0(permutation$shortName,
         ifelse(permutation$age == "any" &
                  permutation$sex == "any" &
                  permutation$obese == "any", " main", ""),
         ifelse(permutation$tar == "ot2", " ot2", ""),
         ifelse(permutation$met == "no", " no-met", ""),
         ifelse(permutation$age != "any", paste0(" ", permutation$age, "-age"), ""),
         ifelse(permutation$sex != "any", paste0(" ", permutation$sex), ""),
         ifelse(permutation$obese != "any", paste0(" ", permutation$obese, "-rdz"), ""))
}
cohort  = baseCohort_orig 
permutation = permutations[1,] 
permuteTC <- function(cohort, permutation, ingredientLevel = FALSE) {
  
  c1Id <- floor(permutation$comparator1Id / 10)
  c2Id <- floor(permutation$comparator2Id / 10)
  c3Id <- floor(permutation$comparator3Id / 10)
  
  delta <- 0
  
  # Remove unused alternative within-class
  if (ingredientLevel) {
    
    targetId <- permutation$targetId
    classId <- floor(targetId / 10)
    
    classSet <- cohort$expression$ConceptSets[[classId]]
    targetSet <- classSet
    excludeSet <- classSet
    
    drugInfo <- exposuresOfInterestTable %>% filter(cohortId == targetId)
    name <- drugInfo %>% pull(name)
    conceptId <- drugInfo %>% pull(conceptId)
    
    targetSet$name <- name
    excludeSet$name <- paste(excludeSet$name, "excluding", name)
    excludeSet$id <- 15
    
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
    
    cohort$expression$ConceptSets[[classId]] <- targetSet
    cohort$expression$ConceptSets[[15]] <- excludeSet
    
    tId <- classId
    
  } else {
    cohort$expression$InclusionRules[[1]] <- NULL
    delta <- delta + 1
    cohort$expression$ConceptSets[[15]] <- NULL
    tId <- floor(permutation$targetId / 10)
  }
  
  cohort$expression$PrimaryCriteria$CriteriaList[[1]]$DrugExposure$CodesetId <- tId
  
  # cohort$expression$AdditionalCriteria$CriteriaList[[1]]$Criteria$DrugExposure$CodesetId <- c1Id
  # cohort$expression$AdditionalCriteria$CriteriaList[[2]]$Criteria$DrugExposure$CodesetId <- c2Id
  # cohort$expression$AdditionalCriteria$CriteriaList[[3]]$Criteria$DrugExposure$CodesetId <- c3Id
  
  target <- 2 - delta - 1
  cohort$expression$InclusionRules[[target + tId]] <- NULL
  cohort$expression$EndStrategy$CustomEra[1] <- tId
  delta <- delta + 1
  
  # AdditionalCriteria: [1,2,3] other drug classes
  # [4]: codesetId 12 (type 2 diabetes mellitus)
  # [5]: codesetId 11 (type 1 diabetes mellitus)
  # [6]: codesetId 10 (2nd diabetes mellitus)
  # [7]: codesetId 5 (other anti-diabetes)
  # [8]: codesetId 15 (other drugs in class) if not null
  # end ...
  
  
  # Want to move: [1,2,3,7,8]
  
  # moveList <- c(cohort$expression$AdditionalCriteria$CriteriaList[[1]],
  #               cohort$expression$AdditionalCriteria$CriteriaList[[2]],
  #               cohort$expression$AdditionalCriteria$CriteriaList[[3]],
  #               cohort$expression$AdditionalCriteria$CriteriaList[[7]])
  #
  # if (length(cohort$expression$AdditionalCriteria$CriteriaList) == 8) {
  #   moveList <- c(moveList, cohort$expression$AdditionalCriteria$CriteriaList[[8]])
  # }
  #
  # cohort$expression$AdditionalCriteria$CriteriaList <- list(cohort$expression$AdditionalCriteria$CriteriaList[[4]],
  #                                                           cohort$expression$AdditionalCriteria$CriteriaList[[5]],
  #                                                           cohort$expression$AdditionalCriteria$CriteriaList[[6]])
  
  age <- 7 - delta
  if (permutation$age == "younger") {
    cohort$expression$InclusionRules[[age]]$name <- "Lower age group"
    cohort$expression$InclusionRules[[age]]$description <- NULL
    cohort$expression$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]]$Age$Op <- "lt"
    cohort$expression$InclusionRules[[age]]$expression$DemographicCriteriaList[[2]] <- NULL
    # cohort$expression$InclusionRules[[age]]$expression$DemographicCriteriaList[[2]]$Age$Op <- ""
  } else if (permutation$age == "older") {
    cohort$expression$InclusionRules[[age]]$name <- "Older age group"
    cohort$expression$InclusionRules[[age]]$description <- NULL
    cohort$expression$InclusionRules[[age]]$expression$DemographicCriteriaList[[2]]$Age$Op <- "gte"
    cohort$expression$InclusionRules[[age]]$expression$DemographicCriteriaList[[1]] <- NULL
  } else if (permutation$age == "any") {
    cohort$expression$InclusionRules[[age]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown age type")
  }
  
  sex <- 8 - delta
  if (permutation$sex == "female") {
    cohort$expression$InclusionRules[[sex]]$name <- "Female stratum"
    cohort$expression$InclusionRules[[sex]]$description <- NULL
    cohort$expression$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_ID <- 8532
    cohort$expression$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_NAME <- "female"
  } else if (permutation$sex == "male") {
    cohort$expression$InclusionRules[[sex]]$name <- "Male stratum"
    cohort$expression$InclusionRules[[sex]]$description <- NULL
    cohort$expression$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_ID <- 8507
    cohort$expression$InclusionRules[[sex]]$expression$DemographicCriteriaList[[1]]$Gender[[1]]$CONCEPT_NAME <- "male"
  } else if (permutation$sex == "any") {
    cohort$expression$InclusionRules[[sex]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown sex type")
  }
  

  obesity <- 11 - delta
  if (permutation$obese == "without") {
    cohort$expression$InclusionRules[[obesity]]$name <- "Without obesity "
    cohort$expression$InclusionRules[[obesity]]$description <- NULL
    cohort$expression$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Type <- 0
    cohort$expression$InclusionRules[[obesity]]$expression$CriteriaList[[1]]$Occurrence$Count <- 0
  } else if (permutation$obese == "with") {
    cohort$expression$InclusionRules[[obesity]]$name <- "obesity"
    cohort$expression$InclusionRules[[obesity]]$description <- NULL
  } else if (permutation$obese == "any") {
    cohort$expression$InclusionRules[[obesity]] <- NULL
    delta <- delta  + 1
  } else {
    stop("Unknown obesity type")
  }
  
  met <- 12 - delta
  if (permutation$met == "with") {
    # Do nothing
    cohort$expression$InclusionRules[[met]]$description <- NULL
    cohort$expression$InclusionRules[[met + 1]] <- NULL
    delta <- delta + 1
  } else if (permutation$met == "no") {
    cohort$expression$InclusionRules[[met + 1]]$description <- NULL
    cohort$expression$InclusionRules[[met]] <- NULL
    delta <- delta + 1
  } else if (permutation$met == "test") {
    cohort$expression$InclusionRules[[met]]$description <- NULL
    cohort$expression$InclusionRules[[met]]$expression$Type <- "AT_MOST"
    cohort$expression$InclusionRules[[met]]$expression$Count <- 0
    cohort$expression$InclusionRules[[met + 1]] <- NULL
    delta <- delta + 1
  } else {
    stop("Unknown metformin type")
  }
  
  insulin <- 13 - delta
  cohort$expression$InclusionRules[[insulin]]$description <- NULL
  
  if (permutation$tar == "ot1") {
    cohort$expression$CensoringCriteria <- list()
  } else if (permutation$tar == "ot2") {
    
    includedConcepts <- as.numeric(unlist(strsplit(exposuresOfInterestTable %>%
                                                     filter(cohortId == permutation$targetId) %>%
                                                     pull(includedConceptIds),
                                                   ";")))
    items <- cohort$expression$ConceptSets[[14]]$expression$items
    
    tmp <-
      lapply(items, function(item) {
        if (item$concept$CONCEPT_ID %in% includedConcepts) {
          NULL
        } else {
          item
        }
      })
    cohort$expression$ConceptSets[[14]]$expression$items <- plyr::compact(tmp)
  } else {
    stop("Unknown TAR")
  }
  
  cohort$name <- makeName(permutation)
  # cohort$name <- paste0(cohort$name, " T: ", permutation$shortName, " CVD: ", permutation$cvd, " Age: ", permutation$age)
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

# Make classTcosOfInterest.csv ----
makeTCOs <- function(tarId, metId, ageId, sexId, obeseId) {

  baseTs <- permutations %>%
    filter(tar == tarId,
           age == ageId, sex == sexId,
           obese == obeseId, met == metId)

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
  # Order: tar, met, age, sex, obese
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
  # Race
  makeTCOs("ot1", "with", "any", "any",  "any"),
  # CV risk
  makeTCOs("ot1", "with", "any", "any",  "any"),
  makeTCOs("ot1", "with", "any", "any","any"),
  # obese dz
  makeTCOs("ot1", "with", "any", "any",  "without"),
  makeTCOs("ot1", "with", "any", "any", "with"),
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

  # obese dz
  makeTCOs("ot2", "with", "any", "any","without"),
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
# classId <- 10

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

cohortDefinition <- permuteTC(baseCohort, permutationsForDrugs[1,])

# save SQL and JSON files under class name (e.g., "DPP4I") folder
## need to create the directory for this class first
this.class = tolower(permutationsForDrugs[1,]$class)
dir.create(file.path("inst/sql/sql_server", this.class))
dir.create(file.path("inst/cohorts", this.class))

classNames = unique(permutationsForDrugs$class)

for (value in classNames){
    class = value
    perutationsForClass = permutationsForDrugs[permutationsForDrugs$class == value,]
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
# conceptSetSql = CohortDiagnostics:::extractConceptSetsSqlFromCohortSql(permutationsForDrugs$sql[cohortID])

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
permutationsForDrugs$atlasName <- makeShortName(permutationsForDrugs)
printCohortDefinitionFromNameAndJson(name = "canagliflozin main",
                                     json = permutationsForDrugs$json[1]) # change this to one GLP1RA name instead!
printCohortDefinitionFromNameAndJson(name = "canagliflozin younger-age",
                                     json = permutationsForDrugs$json[2])

#make drug-level TCOs
makeTCOsDrug <- function(tarId, metId, ageId, sexId, obeseId) {

  baseTs <- permutationsForDrugs %>%
    filter(tar == tarId,
           age == ageId, sex == sexId, obese == obeseId, met == metId)

  tab <- as.data.frame(t(combn(baseTs$cohortId, m = 2)))
  names(tab) <- c("targetId", "comparatorId")
  tab$outcomeIds <- -1
  tab$excludedCovariateConceptIds <- NA

  tab <- tab %>% inner_join(permutationsForDrugs %>% select(cohortId, atlasName) %>% rename(targetId = cohortId),
                            by = "targetId") %>%
    rename(targetName = atlasName)

  tab <- tab %>% inner_join(permutationsForDrugs %>% select(cohortId, atlasName) %>% rename(comparatorId = cohortId),
                            by = "comparatorId") %>%
    rename(comparatorName = atlasName)

  return(tab)
}

drugTcos <- rbind(
  # Order: tar, met, age, sex, obese
  # OT1
  # Main
  makeTCOsDrug("ot1", "with", "any", "any", "any"),
  # Age
  makeTCOsDrug("ot1", "with", "younger", "any", "any"),
  makeTCOsDrug("ot1", "with", "older", "any", "any"),
  # Sex
  makeTCOsDrug("ot1", "with", "any", "female", "any"),
  makeTCOsDrug("ot1", "with", "any", "male", "any"),
  # obese dz
  makeTCOsDrug("ot1", "with", "any", "any",  "without"),
  makeTCOsDrug("ot1", "with", "any", "any",  "with"),
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

  # obese dz
  makeTCOsDrug("ot2", "with", "any", "any", "without"),
  makeTCOsDrug("ot2", "with", "any", "any", "with")
)

# save TCOs 
for (x in 1:4){
  
this.class = classNames[x]
drugTcos$targetId = as.character(drugTcos$targetId)
grepstring = paste0("^",x)
drugs <- drugTcos[grepl(grepstring,drugTcos$targetId),]
filePath = "inst/settings/"
fileName = sprintf('%sTcosOfInterest.csv', this.class)

readr::write_csv(drugTcos, file.path(filePath, fileName))
}
