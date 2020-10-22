source("initialize.R")
source("pathes.R")

forSchoolsAndYear <- function(data, myYear, schoolIds=NULL) {
  result <- as_tibble(data %>% filter(year==myYear))
  if (!is.null(schoolIds)) {
    result <- result %>% filter(school_id %in% schoolIds)
  }
}

getAll <- function() {
  assertthat::assert_that("633" == getDistrictId(2018, "Cobb County"), msg="Cobb County district id not 633")
  assertthat::assert_that(213 == nrow(getDistricts(2018)), msg="Number of districts not 213")
  assertthat::assert_that(111 == nrow(getSchools(2018, districtName="Cobb County")), msg="Number of Cobb County Schools not 111")
  assertthat::assert_that(111 == nrow(getSchools(2018, districtId=633)), msg="Number of Cobb County Schools not 111")
  schools <- getSchools(2018, districtName="Cobb County")
  administrators <- getAdministrators(2018, schools$school_id)
  averageExperience <- getAverageExperience(2018, schools$school_id)
  enrollmentBySubgroup <- getEnrollmentBySubgroup(2018, schools$school_id)
  eoC <- getMilestoneEoC(2018, schools$school_id)
  eoG <- getMilestoneEoG(2018, schools$school_id)
  teachers <- getTeachers(2018, schools$school_id)
  support <- getSupport(2018, schools$school_id)
}

getAdministrators <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(administratorsDataFile(), 
                             colClasses = c(school_id="character")), 
                    myYear, schoolIds)
}

getAverageExperience <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(experienceAvgDataFile(), 
                             colClasses = c(school_id="character")), 
                    myYear, schoolIds)
}

getDistricts <- function(myYear) {
  as_tibble(read.csv(districtDataFile(), colClasses = c(school_district_name="character", 
                                                        school_district_id="character"))) %>%
    filter(year==myYear)
}

getDistrictId <- function(myYear, districtName) {
  districtRecord <- getDistricts(myYear) %>%
    filter(grepl(districtName, school_district_name, ignore.case = TRUE))
  districtRecord$school_district_id[1]
}

# getEconomic <- function(myYear, schools=NULL) {
#   forSchoolsAndYear(read.csv(economicDataFile(),
#                              colClasses = c(school_id="character")), 
#                     myYear, schools)
# }

getEnrollmentBySubgroup <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(enrollmentBySubgroupDataFile(), 
                             colClasses = c(school_id="character")),
                    myYear, schoolIds)
}

getSchools <- function(myYear, myPeriod="Fall", districtName=NULL, districtId=NULL) {
  if (!is.null(districtName) & !is.null(districtId)) {
    stop("You can specify a district id or a district name, but not both.")
  }
  
  if (!is.null(districtName)) {
    districtId <- getDistrictId(myYear, districtName)
  }
  
  schools <- as_tibble(read.csv(schoolDataFile(), 
                                colClasses = c(school_district_id="character",
                                               school_id="character",
                                               school_code="character"))) %>% 
    filter(year==myYear)
  
  if(!is.null(districtId)) {
    if (class(districtId) == "numeric" | class(districtId) == "integer") {
      districtId <- sprintf("%d", districtId)
    }
    schools <- schools %>% filter(school_district_id==districtId)
  }
  
  totalEnrollment <- getTotalEnrollment(myYear, myPeriod, schools$school_id)
  merge(schools, totalEnrollment)
}

getMilestoneEoC <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(milestoneEoCDataFile(), 
                             colClasses = c(school_id="character")) %>%
                      mutate(grade_level=as.factor(grade_level)),
                    myYear, schoolIds)
}

getMilestoneEoG <- function(myYear, schools=NULL) {
  forSchoolsAndYear(read.csv(milestoneEoGDataFile(),
                             colClasses = c(school_id="character")) %>%
                      mutate(grade_level=as.factor(grade_level)),
                    myYear, schools)
}

getTotalEnrollment <- function(myYear, myPeriod="Fall", schoolIds=NULL) {
  forSchoolsAndYear(read.csv(totalEnrollmentDataFile(),
                             colClasses = c(school_id="character")),
                    myYear, schoolIds) %>%
    filter(period==myPeriod)
}

getTeachers <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(teachersDataFile(),
                             colClasses = c(school_id="character")),
                    myYear, schoolIds)
}

getSupport <- function(myYear, schoolIds=NULL) {
  forSchoolsAndYear(read.csv(supportPersonnelDataFile(),
                             colClasses = c(school_id="character")),
                    myYear, schoolIds)
}

