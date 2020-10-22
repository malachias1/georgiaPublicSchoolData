source("pathes.R")

rawCpiDir <- function() {
  createDir(dataDir("rawcpi"))
}

rawEconomicDir <- function() {
  createDir(file.path("rawEconomic"))
}

rawEnrollmentDir <- function() {
  createDir(dataDir("rawenrollment"))
}

rawEnrollmentBySubgroupDir <- function() {
  createDir(dataDir("rawenrollmentBySubgroup"))
}

rawMilestoneEoCDir <- function() {
  createDir(dataDir("rawmilestoneEoC"))
}

rawMilestoneEoGDir <- function() {
  createDir(dataDir("rawmilestoneEoG"))
}

downloadCPI <- function() {
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
  URLS <- c("https://download.gosa.ga.gov/2019/Enrollment_by_Subgroups_Programs_2019_Dec2nd_2019.csv",
            "https://download.gosa.ga.gov/2018/Enrollment_by_Subgroups_Programs_2018_DEC_10th_2018.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawEnrollmentBySubgroupDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}

downloadMilestoneEoC <- function() {
  URLS <- c("https://download.gosa.ga.gov/2019/EOC_2019_By_Grad_FEB_24_2020.csv",
            "https://download.gosa.ga.gov/2018/EOC_2018_By_Grad_FEB_24_2020.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawMilestoneEoCDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}

downloadMilestoneEoG <- function() {
  URLS <- c("https://download.gosa.ga.gov/2019/EOG_2019_By_Grad_FEB_24_2020.csv",
            "https://download.gosa.ga.gov/2018/EOG_2018_By_Grad_FEB_24_2020.csv")
  
  for (url in URLS) {
    localPath <- file.path(rawMilestoneEoGDir(), basename(url))
    if (!file.exists(localPath)) {
      download.file(url, localPath, method="curl")
    }
  }
}
