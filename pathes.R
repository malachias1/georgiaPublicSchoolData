createDir <- function(dirPath) {
  if (!dir.exists(dirPath)) {
    if (file.exists(dirPath) ) {
      stop(sprintf("Cannot create \"%s\" directory because a file by that name is in the way.", dirPath))
    }
    dir.create(dirPath)
  }
  dirPath
}

#
# Return a relative path to the data directory, 
#
dataDir <- function(name) {
  path <- file.path(".", "data")
  createDir(path)
  file.path(path, name)
}

cpiDir <- function(name) {
  file.path(createDir(dataDir("cpi")), name)
}

economicDir <- function(name) {
  file.path(createDir(dataDir("economic")), name)
}

enrollmentDir <- function(name) {
  file.path(createDir(dataDir("enrollment")), name)
}

milestoneDir <- function(name) {
  file.path(createDir(dataDir("milestone")), name)
}

certificateLevelDataFile <- function() {
  cpiDir("certificateLevel.csv")
}

certifiedPersonnelDataFile <- function() {
  cpiDir("certifiedPersonnel.csv")
}

administratorsDataFile <- function() {
  cpiDir("administrators.csv")
}

districtDataFile <- function() {
  dataDir("district.csv")
}

economicDataFile <- function() {
  enrollmentDir("economic.csv")
}

employmentStatusDataFile <- function() {
  cpiDir("employeeStatus.csv")
}

enrollmentBySubgroupDataFile <- function() {
  enrollmentDir("enrollmentBySubgroup.csv")
}

enrollmentDataFile <- function() {
  enrollmentDir("enrollment.csv")
}

experienceAvgDataFile <- function() {
  cpiDir("experienceAvg.csv")
}

experienceDataFile <- function() {
  cpiDir("experience.csv")
}

genderDataFile <- function() {
  cpiDir("gender.csv")
}

gradeLevelEnrollmentDataFile <- function() {
  enrollmentDir("gradeLevelEnrollment.csv")
}

gradesServedDataFile <- function() {
  enrollmentDir("gradesServed.csv")
}

milestoneEoCDataFile <- function() {
  milestoneDir("eoc.csv")
}

milestoneEoGDataFile <- function() {
  milestoneDir("eog.csv")
}

raceEthnicityDataFile <- function() {
  cpiDir("raceEthnicity.csv")
}

schoolDataFile <- function() {
  dataDir("schools.csv")
}

supportPersonnelDataFile <- function() {
  cpiDir("supportPersonnel.csv")
}

teachersDataFile <- function() {
  cpiDir("teachers.csv")
}

totalEnrollmentDataFile <- function() {
  enrollmentDir("totalEnrollment.csv")
}
