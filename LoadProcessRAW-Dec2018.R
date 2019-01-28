# New load and process raw files in R
# This was made on August 21, 2018 bc we ran just the files we know are good
# We are assuming every file here has the correct bregma
# But we still need to double check and remove files that don't match the final
# dataset from the last time we ran the whole thing
# 
# The goal here is to output a file that can be run into the PrepForKK.R
#
## Adapted from the original LoadProcessRAW-May2018.R



# I. 
# Load the original raw data
csvfiles = list.files(path = "All AGE 10 series counts-12122018-again/csv/", full.names = TRUE)
raw = do.call("rbind", lapply(csvfiles, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
    round = basename(file)
  )
}
)
)

# make the column names easier to read by removing the 'X.'
colnames(raw) <- gsub('X.', '', colnames(raw), fixed = TRUE)



# IV. Merge in animal details
animal_details = read.csv("animal_details3.csv", fileEncoding="UTF-8-BOM")
animal_details$Mouse.ID. = as.factor(animal_details$Mouse.ID.)
raw = merge(animal_details, raw, by = "Mouse.ID.")

# change everything else to numeric
for (i in c(21:32, 37:57)) {
  raw[,i] = as.numeric(as.character(raw[,i]))
}

# Convert time (hh:mm) to minutes 
raw$DELTA.T = sapply(strsplit(as.character(raw$DELTA.T), ":"), function(x)
{
  x = as.numeric(x)
  x[1] * 60 + x[2]
}
)

source("SplitZLevels.R")

