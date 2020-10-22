source("initialize.R")
source("pathes.R")
source("downloading.R")

# Load raw Certified Personnel Info
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

# Load raw Directly Certified Info
# These are xml files that I cannot download
# programmatically. There are not consistent
# with one another. One has a note in the first row.
#
rawEconomic <- function() {
  # first row of 2018 has a note in so I need start at row 2.
  f2019 <- read.xlsx(file.path(rawEconomicDir(), "2019_directly_certified.xls"), sheetIndex = 1)
  f2018 <- read.xlsx(file.path(rawEconomicDir(), "2018_directly_certified.xls"), sheetIndex = 1, startRow = 2)
  rbind(f2019, f2018)
}

# Load raw Enrollment Info. I use this
# data to generate: districts, schools, grade level
# enrollement per school, and total enrollment
# per school for the fall period.
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

# Load raw Enrollment by subgroup Info.
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

# Load raw Milestone End of Course Info.
rawMilestoneEoC <- function() {
  milestoneData <- NULL
  for (f in list.files(rawMilestoneEoCDir())) {
    dt <- fread(file.path(rawMilestoneEoCDir(), f))
    if (is.null(milestoneData)) {
      milestoneData <- dt
    } else {
      milestoneData <- rbind(milestoneData, dt)
    }
  }
  milestoneData
}

# Load raw Milestone End of Grade Info.
rawMilestoneEoG <- function() {
  milestoneData <- NULL
  for (f in list.files(rawMilestoneEoGDir())) {
    dt <- fread(file.path(rawMilestoneEoGDir(), f))
    if (is.null(milestoneData)) {
      milestoneData <- dt
    } else {
      milestoneData <- rbind(milestoneData, dt)
    }
  }
  milestoneData
}

#
# Convert a grade level description like KK,01,02" to a
# list of grades like: "KK", "01", "02".
#
gradeLevelDescriptionToVectorOfGrades <- function(gradeLevelDesciption) {
  drop(sapply(strsplit(gradeLevelDesciption, ","), str_trim, USE.NAMES = FALSE))
}

#
# Return TRUE if one or more of the grades in the description
# is in grades, where description looks like "PK,KK,01..." and 
# grades looks like c("PK", "KK", "01", ...).  The grades argument
# lists the grades typically in a particular category of schools, e.g.,
# a middle school has grades 6th, 7th, and 8th).
#
looksLike <- function(gradeLevelDesciption, grades) {
  length(intersect(gradeLevelDescriptionToVectorOfGrades(gradeLevelDesciption),
                   grades)) > 0
}

#
# Return a mask (of length 14, Pk-12) of grades served 
# (e.g., [1] is TRUE if PK is in grades_served_description).  T
#
getGradesServed <- function(grades_served_description) {
  grades <- c("PK", "KK", "01", "02", "03", 
              "04", "05", "06", "07", 
              "08", "09", "10", "11", "12")
  grades %in% gradeLevelDescriptionToVectorOfGrades(grades_served_description)
}

#
# Return a school category (e.g., middle school) given
# a grades served description.
#
getSchoolCategory <- function(gradeLevelDesciption) {
  e <- c("PK", "KK", "01", "02", "03", "04","05")
  m <- c("06", "07", "08")
  h <- c("09", "10", "11", "12")
  
  isElementary <- looksLike(gradeLevelDesciption, e)
  isMiddle <- looksLike(gradeLevelDesciption, m)
  isHigh <- looksLike(gradeLevelDesciption, h)
  
  if (isElementary & !isMiddle & !isHigh) {
    return ("Elementary School")
  }
  
  if (!isElementary & isMiddle & !isHigh) {
    return ("Middle School")
  }
  
  if (!isElementary & !isMiddle & isHigh) {
    return ("High School")
  }
  
  if (isElementary & isMiddle & !isHigh) {
    return ("Mixed Elementary & Middle School Grades")
  }
  
  if (!isElementary & isMiddle & isHigh) {
    return ("Mixed Middle & High School Grades")
  }
  
  if (isElementary & !isMiddle & isHigh) {
    return ("Mixed Elementary & High School Grades")
  }
  
  if (isElementary & !isMiddle & isHigh) {
    return ("Mixed Elementary & High School Grades")
  }
  
  return ("Mixed Pk - 12th Grades")
}

#
# Return a canonical institution id give a district id
# and institution number.
#
makeInstitutionId <- function(districtId, institutionNumber) {
  paste0(districtId, institutionNumber)
}

#
# Long school year is of the form, YYYY-YY.  The
# Fiscal year is the first 4 digits.
#
longSchoolYearToFiscalYear <- function(longSchoolYear) {
  strtoi(substr(longSchoolYear, 1, 4))
}

#
# Return a name that is lowercase, spaces are replaced 
# with underscores, average is replaced with avg.
#
repairName <- function(n) {
  tolower(gsub(" ", "_", n))
}

tidyCPI <- function() {
  downloadCPI()
  data <- rawCPI() %>%
    rename_with(function(n) { tolower(gsub(" ", "_", n))}) %>%
    rename(school_district_id=school_dstrct_cd, 
           school_district_name=school_dstrct_nm,
           school_name=instn_name,
           school_code=instn_number) %>%
    filter(school_code != "ALL") %>%
    mutate(school_id = makeInstitutionId(school_district_id, school_code)) %>%
    mutate(year=longSchoolYearToFiscalYear(long_school_year))
    
  certificateLevel <- data %>%
    filter(data_category == "Certificate Level") %>%
    rename(certificate_level=data_sub_category)  %>%
    select(year, school_id, certificate_level, employee_type, count=measure)
  certifiedPersonnel <- data %>%
    filter(data_category == "Certified Personnel") %>%
    mutate(provisional=(data_sub_category=="Provisional")) %>%
    select(year, school_id, provisional, employee_type, count=measure)
  gender <- data %>%
    filter(data_category == "Gender") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=function(n) {tolower(n)}) %>%
    select(year, school_id, employee_type, male_count=male, female_count=female)
  employmentStatus <- data %>%
    filter(data_category == "Personnel") %>%
    mutate(fulltime=(data_sub_category=="Full-time")) %>%
    select(year, school_id, fulltime, employee_type, count=measure)
  adminstrators <- data %>%
    filter(data_category == "Positions", employee_type == "Administrators") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year, school_id, average_annual_salary, average_contract_days,
           average_daily_salary, count=number)
  teachers <- data %>%
    filter(data_category == "Positions", employee_type == "PK-12 Teachers") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year, school_id, average_annual_salary, average_contract_days,
           average_daily_salary, count=number)
  supportPersonnel <- data %>%
    filter(data_category == "Positions", employee_type == "Support Personnel") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year, school_id, average_annual_salary, average_contract_days,
           average_daily_salary, count=number)
  
  experience <- data %>%
    filter(data_category == "Years Experience", data_sub_category != "Average") %>%
    select(year, school_id, experience=data_sub_category, employee_type, count=measure)
  
  experienceAvg <- data %>%
    filter(data_category == "Years Experience", data_sub_category == "Average") %>%
    select(year, school_id, employee_type, average=measure)
  
  raceEthnicity <- data %>%
    filter(data_category == "Race/Ethnicity") %>%
    pivot_wider(names_from = data_sub_category, values_from = measure, names_repair=repairName) %>%
    select(year, school_id, employee_type, asian_count=asian, black_count=black, 
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

CANONICAL_ACADEMIC_LEVEL_MAP <- c("PK"="Pre-K", "K"="Kindergarten", "KK"="Kindergarten", 
                                  "01"="1st", "02"="2nd", "03"="3rd", 
                                  "1"="1st", "2"="2nd", "3"="3rd", 
                                  "04"="4th", "05"="5th", "06"="6th", "07"="7th", 
                                  "4"="4th", "5"="5th", "6"="6th", "7"="7th", 
                                  "08"="8th", "09"="9th", "10"="10th", "11"="11th", "12"="12th",
                                  "8"="8th", "9"="9th")

stringToCanonicalGradeLevel <- function(academicLevel) {
  gradeLevel <- CANONICAL_ACADEMIC_LEVEL_MAP[academicLevel]
  if (is.na(gradeLevel)) {
    stop(sprintf("Grade level %s, not recognized", academicLevel))
  }
  academicLevel
}
#
# Enrollment data should be dealt with first because
# I create district and institution tables from it.
# See Code Book for details.
# 
tidyEnrollment <- function() {
  downloadEnrollment()
  
  #
  # Read in the raw enrollment data.
  # First make all column names lower case.
  # Filter for school institutions.  There are others, like
  # central office. Convert the long school year to
  # a fiscal year.  Create and school_id (I only have schools) that
  # combines the school district id and
  # institution number.
  #
  data <- rawEnrollment()  %>%
    rename_with(tolower) %>%
    filter(detail_lvl_desc=="School") %>%
    rename(school_district_id=school_dstrct_cd, 
           school_district_name=school_dstrct_nm,
           school_name=instn_name,
           school_code=instn_number,
           period=enrollment_period, 
           count=enrollment_count) %>%
    mutate(year=longSchoolYearToFiscalYear(long_school_year),
           school_id = makeInstitutionId(school_district_id, school_code))
  
  # Build an enrollment table with column 
  # name changes, selected columns, and a total
  # enrollment.
  enrollment <- data %>%
    select(year, school_district_id, school_district_name, 
           school_id, school_code, school_name,
           period, grades_served_desc, grade_level, 
           count)
  
  # Build grade level enrollment table.
  gradeLevelEnrollment <- data %>%
    distinct(year, school_id, grade_level, count)

  
  # Build total enrollment table.
  totalEnrollment <- data %>%
    group_by(year, school_id, period) %>%
    summarize(total = sum(count)) %>%
    ungroup()
  
  # Build a grades served table. There is a logical column for each grade.
  # If the school serves a particular grade that grade column will be true;
  # otherwise, it will be false.
  gradesServedDesc <- data %>%
    distinct(year, school_id, desc=grades_served_desc)
  gradeLevelInfo <- bind_cols(gradesServedDesc %>%
                                select(year, school_id),
                              as_tibble(t(sapply(gradesServedDesc$desc, getGradesServed, USE.NAMES = FALSE)),
                                        .name_repair = function(ignore) {c("PK", "KK", "G01", "G02", "G03", 
                                                                           "G04", "G05", "G06", "G07", 
                                                                           "G08", "G09", "G10", "G11", "G12")}))
  # names(gradeLevelInfo) <- c("year", "school_id",
  #                            "PK", "KK", "G01", "G02", "G03", 
  #                            "G04", "G05", "G06", "G07", 
  #                            "G08", "G09", "G10", "G11", "G12")
  
  # Build a category table that assigns a category to each school, e.g.,
  # Middle School.
  gradeLevelInfo$category <- sapply(gradesServedDesc$desc, getSchoolCategory)
  
  # Build a district table
  districts <- data %>%
    distinct(year, school_district_id, school_district_name)
  
  # Build a schools table
  schools <- merge(data %>%
                     distinct(year, school_district_id, school_id, 
                              school_code, school_name),
                   gradeLevelInfo,
                   by=c("school_id", "year"))
  
  write.csv(districts, districtDataFile(), row.names=FALSE)
  write.csv(schools, schoolDataFile(), row.names=FALSE)
  write.csv(totalEnrollment, totalEnrollmentDataFile(), row.names=FALSE)
  write.csv(gradeLevelEnrollment, gradeLevelEnrollmentDataFile(), row.names=FALSE)
}

getTotalEnrollmentData <- function() {
  read.csv(totalEnrollmentDataFile(), colClasses = c(school_id="character"))
}


tidyEnrollmentBySubgroup <- function() {
  downloadEnrollmentBySubgroup()
  
  data <- rawEnrollmentBySubgroup()  %>%
    rename_with(tolower) %>%
    rename(school_district_id=school_dstrct_cd, 
           school_code=instn_number) %>%
    mutate(year=longSchoolYearToFiscalYear(long_school_year)) %>%
    filter(detail_lvl_desc=="School") %>%
    mutate(school_id = makeInstitutionId(school_district_id, school_code))
  
  
  totalEnrollment <- getTotalEnrollmentData() %>% 
    filter(period=="Fall")
  
  enrollmentBySubgroup <- merge(data, totalEnrollment) %>%
    mutate(minority_count=enroll_percent_native*total/100.0 + enroll_percent_black*total/100.0 + 
             enroll_percent_hispanic*total/100.0 + enroll_percent_multiracial*total/100.0, 
           minority_percent = minority_count/total) %>%
    select(year, school_id, total, minority_count, minority_percent, 
           enroll_percent_asian,enroll_percent_native,enroll_percent_black,enroll_percent_hispanic,
           enroll_percent_multiracial,enroll_percent_white,enroll_percent_migrant,
           enroll_percent_ed,enroll_percent_swd,enroll_percent_lep,enroll_count_remedial_gr_6_8,
           enroll_pct_remedial_gr_6_8,enroll_count_eip_k_5,
           enroll_percent_eip_k_5,enroll_count_remedial_gr_9_12,enroll_pct_remedial_gr_9_12,
           enroll_count_special_ed_k12,enroll_pct_special_ed_k12,enroll_count_esol,
           enroll_pct_esol,enroll_count_special_ed_pk,enroll_pct_special_ed_pk,
           enroll_count_vocation_9_12,enroll_pct_vocation_9_12,enroll_count_alt_programs,
           enroll_pct_alt_programs,enroll_count_gifted,enroll_pct_gifted,
           enroll_percent_male,enroll_percent_female)
  
  write.csv(enrollmentBySubgroup, enrollmentBySubgroupDataFile(), row.names=FALSE)
}


tidyEconomic <- function() {
  if(!file.exists(economicDataFile())) {
    data <- rawEconomic()  %>%
      rename_with(tolower) %>%
      mutate(school_id=sprintf("%3d%04d", system_id, school_id), school_district_id=sprintf("%d", system_id))
    
    economic <- data %>%
      select(year=fiscal_year, school_district_id, school_id, school_name=school_name, direct_certified_percent=direct_cert_perc)
    
    write.csv(economic, economicDataFile(), row.names=FALSE)
  }
}

tidyGeorgiaSchools <- function() {
  tidyEnrollment()
  tidyEnrollmentBySubgroup()
  tidyMilestoneEoC()  
  tidyMilestoneEoG()  
  tidyCPI()
}

tidyMilestoneEoC <- function() {
  downloadMilestoneEoC()
  
  data <- rawMilestoneEoC()  %>%
    rename_with(tolower) %>%
    rename(school_district_id=school_distrct_cd, 
           school_code=instn_number,
           grade_level=acdmc_lvl) %>%
    mutate(year=longSchoolYearToFiscalYear(long_school_year)) %>%
    mutate(school_id = makeInstitutionId(school_district_id, school_code)) %>%
    mutate(grade_level=sapply(grade_level, stringToCanonicalGradeLevel))
  
  milestone <- data %>%
    select(year, school_id, grade_level,
           subgroup_name, test_cmpnt_typ_nm, num_tested_cnt, begin_cnt, developing_cnt, proficient_cnt,
           distinguished_cnt, begin_pct, developing_pct, proficient_pct, distinguished_pct)
  
  write.csv(milestone, milestoneEoCDataFile(), row.names=FALSE)
  
}

tidyMilestoneEoG <- function() {
  downloadMilestoneEoG()
  
  data <- rawMilestoneEoG()  %>%
    rename_with(tolower) %>%
    rename(school_district_id=school_distrct_cd, 
           school_code=instn_number,
           grade_level=acdmc_lvl) %>%
    mutate(year=longSchoolYearToFiscalYear(long_school_year)) %>%
    mutate(school_id = makeInstitutionId(school_district_id, school_code)) %>%
    mutate(grade_level=sapply(grade_level, stringToCanonicalGradeLevel))
  
  milestone <- data %>%
    select(year, school_id, grade_level,
           subgroup_name, test_cmpnt_typ_nm, num_tested_cnt, begin_cnt, developing_cnt, proficient_cnt,
           distinguished_cnt, begin_pct, developing_pct, proficient_pct, distinguished_pct)
  
  write.csv(milestone, milestoneEoGDataFile(), row.names=FALSE)
}
