source("getData.R")

getHighSchoolMetadata <- function(districtName) {
  districtRecord <- read.csv(districtDataFile(enrollmentDir())) %>%
    filter(grepl(districtName, school_district_name, ignore.case = TRUE))
  districtId <- districtRecord$school_district_id[1]
  districtSchools <- read.csv(institutionDataFile(enrollmentDir())) %>%
    filter(school_district_id==districtId)
  high_school_ids <- read.csv(gradesServedDataFile()) %>%
    filter(G09 | G10 | G11 | G12, institution_id %in% districtSchools$institution_id) %>%
    distinct(institution_id)
  districtSchools %>%
    filter(institution_id %in% high_school_ids$institution_id) %>%
    select(school_district_id, institution_id, institution_name)
}

getHighSchoolEnrollmentTotals <- function(districtName) {
  highSchoolMetadata <- getHighSchoolMetadata(districtName)
  enrollment <- read.csv(enrollmentDataFile()) %>%
    filter(institution_id %in% highSchoolMetadata$institution_id, year=="2018-19", period=="Fall") %>%
    group_by(institution_id) %>%
    summarize(total = sum(count))
  merge(enrollment, highSchoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getHighSchoolTeacherSpend <- function(districtName) {
  highSchoolMetadata <- getHighSchoolMetadata(districtName)
  teacherSpend <- read.csv(teachersDataFile()) %>%
    filter(institution_id %in% highSchoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalTeacherSpend = avg_annual_salary*count)
  merge(teacherSpend, highSchoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getHighSchoolAdministratorSpend <- function(districtName) {
  highSchoolMetadata <- getHighSchoolMetadata(districtName)
  administratorSpend <- read.csv(administratorsDataFile()) %>%
    filter(institution_id %in% highSchoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalAdministratorSpend = avg_annual_salary*count)
  merge(administratorSpend, highSchoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getHighSchoolSupportSpend <- function(districtName) {
  highSchoolMetadata <- getHighSchoolMetadata(districtName)
  supportSpend <- read.csv(supportPersonnelDataFile()) %>%
    filter(institution_id %in% highSchoolMetadata$institution_id, year=="2018-19") %>%
    group_by(institution_id) %>%
    summarize(totalSupportSpend = avg_annual_salary*count)
  merge(supportSpend, highSchoolMetadata %>% select(institution_id, institution_name), 
        by.x=c("institution_id")) %>%
    relocate(institution_name, .after=institution_id)
}

getInvestmentPerStudent <- function(districtName) {
  teacherSpend <- getHighSchoolTeacherSpend(districtName)
  supportSpend <- getHighSchoolSupportSpend(districtName)
  administratorSpend <- getHighSchoolAdministratorSpend(districtName)
  totalEnrollment <- getHighSchoolEnrollmentTotals(districtName)
  investment <- merge(merge(merge(totalEnrollment, teacherSpend), supportSpend), administratorSpend) %>%
    mutate(investmentPerSudent=(totalTeacherSpend + totalSupportSpend + totalAdministratorSpend)/total, teacherInvestmentPerStudent=(totalTeacherSpend)/total)
}