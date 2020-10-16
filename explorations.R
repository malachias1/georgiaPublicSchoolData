source("getData.R")

getSchoolMetadata <- function(districtName, schoolType) {
  districtRecord <- read.csv(districtDataFile(enrollmentDir())) %>%
    filter(grepl(districtName, school_district_name, ignore.case = TRUE))
  districtId <- districtRecord$school_district_id[1]
  districtSchools <- read.csv(institutionDataFile(enrollmentDir())) %>%
    filter(school_district_id==districtId)
  if (schoolType == "high") {
    school_ids <- read.csv(gradesServedDataFile()) %>%
      filter(G09 | G10 | G11 | G12, institution_id %in% districtSchools$institution_id) %>%
      distinct(institution_id)
  } else if (schoolType == "middle") {
    school_ids <- read.csv(gradesServedDataFile()) %>%
      filter(G06 | G08 | G08, institution_id %in% districtSchools$institution_id) %>%
      distinct(institution_id)
  } else {
    school_ids <- read.csv(gradesServedDataFile()) %>%
      filter(KK | G01 | G02 | G03 | G04 | G05, institution_id %in% districtSchools$institution_id) %>%
      distinct(institution_id)
  }
  districtSchools %>%
    filter(institution_id %in% school_ids$institution_id) %>%
    select(school_district_id, institution_id, institution_name)
}

getSchoolEnrollmentTotals <- function(districtName, schoolType) {
  schoolMetadata <- getSchoolMetadata(districtName, schoolType)
  enrollment <- read.csv(enrollmentDataFile()) %>%
    filter(institution_id %in% schoolMetadata$institution_id, year=="2018-19", period=="Fall") %>%
    group_by(institution_id) %>%
    summarize(total = sum(count))
  merge(enrollment, schoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getSchoolTeacherSpend <- function(districtName, schoolType) {
  schoolMetadata <- getSchoolMetadata(districtName, schoolType)
  teacherSpend <- read.csv(teachersDataFile()) %>%
    filter(institution_id %in% schoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalTeacherSpend = avg_annual_salary*count)
  merge(teacherSpend, schoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getSchoolAdministratorSpend <- function(districtName, schoolType) {
  schoolMetadata <- getSchoolMetadata(districtName, schoolType)
  administratorSpend <- read.csv(administratorsDataFile()) %>%
    filter(institution_id %in% schoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalAdministratorSpend = avg_annual_salary*count)
  merge(administratorSpend, schoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getSchoolSupportSpend <- function(districtName, schoolType) {
  schoolMetadata <- getSchoolMetadata(districtName, schoolType)
  supportSpend <- read.csv(supportPersonnelDataFile()) %>%
    filter(institution_id %in% schoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalSupportSpend = avg_annual_salary*count)
  merge(supportSpend, schoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getInvestmentPerStudent <- function(districtName, schoolType) {
  teacherSpend <- getSchoolTeacherSpend(districtName, schoolType)
  supportSpend <- getSchoolSupportSpend(districtName, schoolType)
  administratorSpend <- getSchoolAdministratorSpend(districtName, schoolType)
  totalEnrollment <- getSchoolEnrollmentTotals(districtName, schoolType)
  investment <- merge(merge(merge(totalEnrollment, teacherSpend), supportSpend), administratorSpend) %>%
    mutate(investmentPerSudent=(totalTeacherSpend + totalSupportSpend + totalAdministratorSpend)/total, teacherInvestmentPerStudent=(totalTeacherSpend)/total)
}