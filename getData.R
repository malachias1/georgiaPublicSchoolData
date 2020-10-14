#
# Ensure required libraries are available
#
ensureLibraries <- function() {
  if (!require(data.table)) {
    stop("The data.table package is required, but the package does not exist.")
  }
  
  if (!require(dplyr)) {
    stop("The dplyr package is required, but the package does not exist.")
  }
  
  if (!require(tidyr)) {
    stop("The tidyr package is required, but the package does not exist.")
  }
}

createDir <- function(dirPath) {
  if (!dir.exists(dirPath)) {
    if (file.exists(dirPath) ) {
      stop(sprintf("Cannot create \"%s\" directory because a file by that name is in the way.", dirPath))
    }
    dir.create(dirPath)
  }
}

#
# Return a relative path to the data directory, 
# i.e., "data".
#
dataDir <- function() {
  path <- file.path(".", "data")
  createDir(path)
  path
}

rawCpiDir <- function() {
  path <- file.path(dataDir(), "rawcpi")
  createDir(path)
  path
}

cpiDir <- function() {
  path <- file.path(dataDir(), "cpi")
  createDir(path)
  path
}

rawEnrollmentDir <- function() {
  path <- file.path(dataDir(), "rawenrollment")
  createDir(path)
  path
}

rawEnrollmentBySubgroupDir <- function() {
  path <- file.path(dataDir(), "rawenrollmentBySubgroup")
  createDir(path)
  path
}

enrollmentDir <- function() {
  path <- file.path(dataDir(), "enrollment")
  createDir(path)
  path
}

districtDataFile <- function(rootDir) {
  file.path(rootDir, "district.csv")
}

institutionDataFile <- function(rootDir) {
  file.path(rootDir, "institution.csv")
}

certificateLevelDataFile <- function() {
  file.path(cpiDir(), "certificateLevel.csv")
}

certifiedPersonnelDataFile <- function() {
  file.path(cpiDir(), "certifiedPersonnel.csv")
}

genderDataFile <- function() {
  file.path(cpiDir(), "gender.csv")
}

employmentStatusDataFile <- function() {
  file.path(cpiDir(), "employeeStatus.csv")
}

administratorsDataFile <- function() {
  file.path(cpiDir(), "administrators.csv")
}

teachersDataFile <- function() {
  file.path(cpiDir(), "teachers.csv")
}

supportPersonnelDataFile <- function() {
  file.path(cpiDir(), "supportPersonnel.csv")
}

experienceDataFile <- function() {
  file.path(cpiDir(), "experience.csv")
}

experienceAvgDataFile <- function() {
  file.path(cpiDir(), "experienceAvg.csv")
}

raceEthnicityDataFile <- function() {
  file.path(cpiDir(), "raceEthnicity.csv")
}

enrollmentDataFile <- function() {
  file.path(enrollmentDir(), "enrollment.csv")
}

gradesServedDataFile <- function() {
  file.path(enrollmentDir(), "gradesServed.csv")
}

downloadCPI <- function() {
  if (!dir.exists(rawCpiDir())) {
    dir.create(rawCpiDir())
  }
  
  URLS <- c("https://download.gosa.ga.gov/2019/Certified_Personnel_Data_2019_Dec2nd_2019.csv",
            "https://download.gosa.ga.gov/2018/Certified_Personnel_Data_2018_DEC_10th_2018.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawCpiDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}

downloadEnrollment <- function() {
  if (!dir.exists(rawEnrollmentDir())) {
    dir.create(rawEnrollmentDir())
  }
  
  URLS <- c("https://download.gosa.ga.gov/2019/Enrollment_By_Grade_Level_2019_Dec2nd_2019.csv",
            "https://download.gosa.ga.gov/2018/Enrollment_By_Grade_Level_2018_DEC_10th_2018.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawEnrollmentDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}

downloadEnrollmentBySubgroup <- function() {
  if (!dir.exists(rawEnrollmentBySubgroupDir())) {
    dir.create(rawEnrollmentBySubgroupDir())
  }
  
  URLS <- c("https://download.gosa.ga.gov/2019/Enrollment_by_Subgroups_Programs_2019_Dec2nd_2019.csv",
            "https://download.gosa.ga.gov/2018/Enrollment_by_Subgroups_Programs_2018_DEC_10th_2018.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawEnrollmentBySubgroupDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}

rawCPI <- function() {
  cpiData <- NULL
  for (f in list.files(rawCpiDir())) {
    dt <- fread(file.path(rawCpiDir(), f))
    if (is.null(cpiData)) {
      cpiData <- dt
    } else {
      cpiData <- rbind(cpiData, dt)
    }
  }
  cpiData
}

rawEnrollment <- function() {
  enrollmentData <- NULL
  for (f in list.files(rawEnrollmentDir())) {
    dt <- fread(file.path(rawEnrollmentDir(), f))
    if (is.null(enrollmentData)) {
      enrollmentData <- dt
    } else {
      enrollmentData <- rbind(enrollmentData, dt)
    }
  }
  enrollmentData
}

rawEnrollmentBySubgroup <- function() {
  enrollmentData <- NULL
  for (f in list.files(rawEnrollmentBySubgroupDir())) {
    dt <- fread(file.path(rawEnrollmentBySubgroupDir(), f))
    if (is.null(enrollmentData)) {
      enrollmentData <- dt
    } else {
      enrollmentData <- rbind(enrollmentData, dt)
    }
  }
  enrollmentData
}


repairName <- function(n) {
  gsub("average", "avg", tolower(gsub(" ", "_", n)))
}

tidyCPI <- function() {
  ensureLibraries()
  downloadCPI()
  data <- rawCPI() %>%
    mutate(institution_id = paste0(SCHOOL_DSTRCT_CD, INSTN_NUMBER)) %>%
    filter(INSTN_NUMBER != "ALL")
  districtData <- distinct(data, school_district_id=SCHOOL_DSTRCT_CD, school_district_name=SCHOOL_DSTRCT_NM)
  institutionData <- data %>% 
    distinct(institution_id, school_district_id=SCHOOL_DSTRCT_CD, institution_code=INSTN_NUMBER, institution_name=INSTN_NAME)
  
  write.csv(districtData, districtDataFile(cpiDir()), row.names=FALSE)
  write.csv(institutionData, institutionDataFile(cpiDir()), row.names=FALSE)
  
  certificateLevel <- data %>%
    filter(DATA_CATEGORY == "Certificate Level") %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, certificate_level=DATA_SUB_CATEGORY, employee_type=EMPLOYEE_TYPE, count=MEASURE)
  certifiedPersonnel <- data %>%
    filter(DATA_CATEGORY == "Certified Personnel") %>%
    mutate(provisional=(DATA_SUB_CATEGORY=="Provisional")) %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, provisional, employee_type=EMPLOYEE_TYPE, count=MEASURE)
  gender <- data %>%
    filter(DATA_CATEGORY == "Gender") %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, gender=DATA_SUB_CATEGORY, employee_type=EMPLOYEE_TYPE, count=MEASURE)
  employmentStatus <- data %>%
    filter(DATA_CATEGORY == "Personnel") %>%
    mutate(fulltime=(DATA_SUB_CATEGORY=="Full-time")) %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, fulltime, employee_type=EMPLOYEE_TYPE, count=MEASURE)
  adminstrators <- data %>%
    filter(DATA_CATEGORY == "Positions", EMPLOYEE_TYPE == "Administrators") %>%
    pivot_wider(names_from = DATA_SUB_CATEGORY, values_from = MEASURE, names_repair=repairName) %>%
    select(institution_id, year=long_school_year, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  teachers <- data %>%
    filter(DATA_CATEGORY == "Positions", EMPLOYEE_TYPE == "PK-12 Teachers") %>%
    pivot_wider(names_from = DATA_SUB_CATEGORY, values_from = MEASURE, names_repair=repairName) %>%
    select(institution_id, year=long_school_year, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  supportPersonnel <- data %>%
    filter(DATA_CATEGORY == "Positions", EMPLOYEE_TYPE == "Support Personnel") %>%
    pivot_wider(names_from = DATA_SUB_CATEGORY, values_from = MEASURE, names_repair=repairName) %>%
    select(institution_id, year=long_school_year, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  
  experience <- data %>%
    filter(DATA_CATEGORY == "Years Experience", DATA_SUB_CATEGORY != "Average") %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, experience=DATA_SUB_CATEGORY, employee_type=EMPLOYEE_TYPE, count=MEASURE)

  experienceAvg <- data %>%
    filter(DATA_CATEGORY == "Years Experience", DATA_SUB_CATEGORY == "Average") %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, experience=DATA_SUB_CATEGORY, employee_type=EMPLOYEE_TYPE, average=MEASURE)
  
  raceEthnicity <- data %>%
    filter(DATA_CATEGORY == "Race/Ethnicity") %>%
    select(institution_id, year=LONG_SCHOOL_YEAR, race_ethnicity=DATA_SUB_CATEGORY, employee_type=EMPLOYEE_TYPE, count=MEASURE)

  write.csv(certificateLevel, certificateLevelDataFile(), row.names=FALSE)
  write.csv(certifiedPersonnel, certifiedPersonnelDataFile(), row.names=FALSE)
  write.csv(gender, genderDataFile(), row.names=FALSE)
  write.csv(employmentStatus, employmentStatusDataFile(), row.names=FALSE)
  write.csv(adminstrators, administratorsDataFile(), row.names=FALSE)
  write.csv(teachers, teachersDataFile(), row.names=FALSE)
  write.csv(supportPersonnel, supportPersonnelDataFile(), row.names=FALSE)
  write.csv(experience, experienceDataFile(), row.names=FALSE)
  write.csv(experienceAvg, experienceAvgDataFile(), row.names=FALSE)
  write.csv(raceEthnicity, raceEthnicityDataFile(), row.names=FALSE)
}

gradesServedMask <- function(desc) {
  mask = c()
  for (g in c("PK", "KK", "01", "02", "03", "04","05", "06", "07", "08", "09", "10", "11", "12")) {
    mask <- append(mask, grepl(g, desc))
  }
  names(mask) <- c("PK", "KK", "G01", "G02", "G03", "G04","G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12")
  row.names(mask) <- NULL
  mask
}

tidyEnrollment <- function() {
  ensureLibraries()
  downloadEnrollment()
  downloadEnrollmentBySubgroup()
  
  data <- rawEnrollment()  %>%
    rename_with(tolower) %>%
    filter(detail_lvl_desc=="School") %>%
    mutate(institution_id = paste0(school_dstrct_cd, instn_number))
  
  districtData <- distinct(data, school_district_id=school_dstrct_cd, school_district_name=school_dstrct_nm)
  institutionData <- data %>% 
    distinct(institution_id, school_district_id=school_dstrct_cd, institution_code=instn_number, institution_name=instn_name)

  write.csv(districtData, districtDataFile(enrollmentDir()), row.names=FALSE)
  write.csv(institutionData, institutionDataFile(enrollmentDir()), row.names=FALSE)

  enrollment <- data %>%
    select(institution_id, year=long_school_year, period=enrollment_period, grade_level, count=enrollment_count)
  
  gradesServedTbl <- tibble::as_tibble(t(sapply(data$grades_served_desc, gradesServedMask)))
  rownames(gradesServedTbl) <- NULL
  data <- bind_cols(data, gradesServedTbl)
  gradesServed <- data %>%
    select(institution_id, year=long_school_year, PK, KK, G01, G02, G03, G04, G05, G06, G07, G08, G09, G10, G11, G12)
  
  write.csv(enrollment, enrollmentDataFile(), row.names=FALSE)
  write.csv(gradesServed, gradesServedDataFile(), row.names=FALSE)
}