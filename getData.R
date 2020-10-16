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
  
  if (!require(stringr)) {
    stop("The stringr package is required, but the package does not exist.")
  }
  
  if (!require(readr)) {
    stop("The readr package is required, but the package does not exist.")
  }
  
  if (!require(xlsx)) {
    stop("The readr package is required, but the package does not exist.")
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

rawEconomicDir <- function() {
  path <- file.path("rawEconomic")
  createDir(path)
  path
}

economicDir <- function() {
  path <- file.path(dataDir(), "economic")
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

economicDataFile <- function() {
  file.path(enrollmentDir(), "economic.csv")
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

rawEconomic <- function() {
  # first row of 2018 has a note in so I need start at row 2.
  f2019 <- read.xlsx(file.path(rawEconomicDir(), "2019_directly_certified.xls"), sheetIndex = 1)
  f2018 <- read.xlsx(file.path(rawEconomicDir(), "2018_directly_certified.xls"), sheetIndex = 1, startRow = 2)
  rbind(f2019, f2018)
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
    rename_with(tolower) %>%
    mutate(institution_id = paste0(school_dstrct_cd, instn_number)) %>%
    mutate(long_school_year=substr(long_school_year, 1, 4)) %>%
    filter(instn_number != "ALL")
  districtData <- data %>%
    distinct(year=long_school_year, school_district_id=school_dstrct_cd, school_district_name=school_dstrct_nm)
  institutionData <- data %>% 
    distinct(year=long_school_year, institution_id, school_district_id=school_dstrct_cd, institution_code=instn_number, institution_name=instn_name)
  
  write.csv(districtData, districtDataFile(cpiDir()), row.names=FALSE)
  write.csv(institutionData, institutionDataFile(cpiDir()), row.names=FALSE)
  
  certificateLevel <- data %>%
    filter(data_category == "Certificate Level") %>%
    select(year=long_school_year, institution_id, certificate_level=data_sub_category, employee_type=employee_type, count=measure)
  certifiedPersonnel <- data %>%
    filter(data_category == "Certified Personnel") %>%
    mutate(provisional=(data_sub_category=="Provisional")) %>%
    select(year=long_school_year, institution_id, provisional, employee_type=employee_type, count=measure)
  gender <- data %>%
    filter(data_category == "Gender") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=function(n) {tolower(n)}) %>%
    select(year=long_school_year, institution_id, employee_type=employee_type, male_count=male, female_count=female)
  employmentStatus <- data %>%
    filter(data_category == "Personnel") %>%
    mutate(fulltime=(data_sub_category=="Full-time")) %>%
    select(year=long_school_year, institution_id, fulltime, employee_type=employee_type, count=measure)
  adminstrators <- data %>%
    filter(data_category == "Positions", employee_type == "Administrators") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year=long_school_year, institution_id, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  teachers <- data %>%
    filter(data_category == "Positions", employee_type == "PK-12 Teachers") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year=long_school_year, institution_id, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  supportPersonnel <- data %>%
    filter(data_category == "Positions", employee_type == "Support Personnel") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year=long_school_year, institution_id, avg_annual_salary, avg_contract_days,
           avg_daily_salary, count=number)
  
  experience <- data %>%
    filter(data_category == "Years Experience", data_sub_category != "Average") %>%
    select(year=long_school_year, institution_id, experience=data_sub_category, employee_type=employee_type, count=measure)

  experienceAvg <- data %>%
    filter(data_category == "Years Experience", data_sub_category == "Average") %>%
    select(year=long_school_year, institution_id, employee_type=employee_type, average=measure)
  
  raceEthnicity <- data %>%
    filter(data_category == "Race/Ethnicity") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=function(n) {tolower(gsub(" ", "_", n))}) %>%
    select(year=long_school_year, institution_id, employee_type=employee_type, asian_count=asian, black_count=black, 
           hispanic_count=hispanic, multiracial_count=multiracial, 
           native_american_count=native_american, white_count=white)

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
    mutate(long_school_year=substr(long_school_year, 1, 4)) %>%
    filter(detail_lvl_desc=="School") %>%
    mutate(institution_id = paste0(school_dstrct_cd, instn_number))
  
  districtData <- distinct(data, school_district_id=school_dstrct_cd, school_district_name=school_dstrct_nm)
  institutionData <- data %>% 
    distinct(institution_id, school_district_id=school_dstrct_cd, institution_code=instn_number, institution_name=instn_name)

  write.csv(districtData, districtDataFile(enrollmentDir()), row.names=FALSE)
  write.csv(institutionData, institutionDataFile(enrollmentDir()), row.names=FALSE)

  enrollment <- data %>%
    select(year=long_school_year, institution_id, period=enrollment_period, grade_level, count=enrollment_count)
  
  gradesServedTbl <- tibble::as_tibble(t(sapply(data$grades_served_desc, gradesServedMask)))
  rownames(gradesServedTbl) <- NULL
  data <- bind_cols(data, gradesServedTbl)
  gradesServed <- data %>%
    select(year=long_school_year, institution_id, PK, KK, G01, G02, G03, G04, G05, G06, G07, G08, G09, G10, G11, G12)
  
  write.csv(enrollment, enrollmentDataFile(), row.names=FALSE)
  write.csv(gradesServed, gradesServedDataFile(), row.names=FALSE)
}

tidyEconomic <- function() {
  ensureLibraries()
  if(!file.exists(economicDataFile())) {
    data <- rawEconomic()  %>%
      rename_with(tolower) %>%
      mutate(institution_id=sprintf("%3d%04d", system_id, school_id), school_district_id=sprintf("%d", system_id))
    
    economic <- data %>%
      select(year=fiscal_year, school_district_id, institution_id, institution_name=school_name, direct_certified_percent=direct_cert_perc)
    
    write.csv(economic, economicDataFile(), row.names=FALSE)
  }
}

getDistrictId <- function(districtName) {
  districtRecord <- read.csv(districtDataFile(cpiDir())) %>%
    filter(grepl(districtName, school_district_name, ignore.case = TRUE))
  districtRecord$school_district_id[1]
}

getDistrictInstitutions <- function(districtName) {
  districtId <- getDistrictId(districtName)
  districtInstitutions <- read.csv(institutionDataFile(enrollmentDir())) %>%
    filter(school_district_id==districtId) %>%
    mutate(school_district_id=as.factor(school_district_id), institution_id=as.factor(institution_id), 
           institution_code=as.factor(institution_code))
}