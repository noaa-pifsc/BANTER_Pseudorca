library(tuneR)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Function to calculate seconds of recordings
get_duration <- function(f) {
  tryCatch({
    wav <- readWave(f, header = TRUE)
    duration_sec <- wav$samples / wav$sample.rate
    return(duration_sec)
  },
  error = function(e) {
    warning(paste("Skipping unreadable file:", f))
    return(NA)
  })
}


# Folder containing your recordings
# path <- "\\\\Piccrpnas\\crp2\\PICEAS_2011_TowedArray\\Leg_1\\Recordings\\MF_Recordings"
# path <- "\\\\Piccrpnas\\crp2\\MACS_2015_TowedArray_Recording\\HF_Recordings"
# path <- "\\\\Piccrpnas\\crp2\\MACS_2015_TowedArray_Recording\\MF_Recordings"
path <- "\\\\Piccrpnas\\crp\\pifsc-1\\towed_array\\2018_MACS_1803"
path <- "\\\\Piccrpnas\\crp3\\pifsc-1\\towed_array\\2021_MACS_2102"
path <- "\\\\Piccrpnas\\crp3\\pifsc-1\\towed_array\\2023_HICEAS_2401" #lasker
path <- "\\\\Piccrpnas\\crp3\\pifsc-1\\towed_array\\2024_SCOPE_2404" #lasker


# Look through folder and subfolders
files <- list.files(path, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)
# files2 <- list.files(path, pattern = "\\.wav$", full.names = TRUE, recursive = FALSE)

# Any problem files?
check_files <- sapply(files, function(f) {
  tryCatch({
    readWave(f, header = TRUE)
    TRUE
  }, error = function(e) FALSE)
})

bad_files <- files[!check_files]
bad_files

# Use only good files
valid_files <- files[check_files]

durations <- sapply(valid_files, function(f) {
  wav <- readWave(f, header = TRUE)
  wav$samples / wav$sample.rate
})

total_hours <- sum(durations) / 3600
print(total_hours)

