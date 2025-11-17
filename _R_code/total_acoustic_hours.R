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
path <- "\\\\Piccrpnas\\crp2\\WHICEAS_2020_TowedArray_Recording\\Recording"

# Look through folder and subfolders
files <- list.files(path, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)


# Apply to all files
durations_sec <- sapply(files, get_duration)

total_hours <- sum(durations_sec) / 3600
print(total_hours)







get_duration <- function(f) {
  tryCatch({
    wav <- readWave(f, header = TRUE)
    wav$samples / wav$sample.rate   # duration in seconds
  
    },
  error = function(e) NA)           # skip bad files
}

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



df <- data.frame(file = basename(files)) %>%
  tidyr::extract(file, into = "timestamp",
                 regex = ".*_(\\d{8}_\\d{6})\\.wav") %>%
  mutate(datetime = ymd_hms(timestamp))




library(tuneR)

df$duration_sec <- sapply(files, function(f) {
  wav <- readWave(f, header = TRUE)
  wav$samples / wav$sample.rate
})

total_hours <- sum(df$duration_sec) / 3600
print(total_hours)
