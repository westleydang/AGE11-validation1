# New load and process raw files in R
## Will be adding in the new revision files here
## Also excluding the files
## Adapted from the original LoadProcessRAW.R

# I. 
# Load the original raw data
csvfiles = list.files(path = "All AGE 10 series counts-03232018/Counts10/", full.names = TRUE)
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



# II. 
# Exclusions

# KK's usual file naming pattern is:
# seq _ z _ bregma _ animal _ flags .tif
# So find num_num_num_num_ with optional '-' for negative bregma
root_pat = "^[:digit:]{1,3}_[:digit:]{1,3}_[-]?.{1,3}_[:xdigit:]{1,4}_"

# Load in the image metadata and clean it
exfiles = list.files(path = "All AGE 10 series counts-03232018\\Rotations or Exclusions - From Manual Validations", full.names = TRUE)
ex = do.call("rbind", lapply(exfiles, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
  round = basename(file) )}
  ))

# Clean the 'ex' dataframe
clean_shellshit = function(string, pattern)
{
  new = str_extract(string, pattern)
  string = new
}
pat_shell = "[:digit:]{1,4}"
ex$Width = mapply(clean_shellshit, string = ex$Width, pattern = pat_shell)
ex$Height = mapply(clean_shellshit, string = ex$Height, pattern = pat_shell) 
ex$Width = as.numeric(ex$Width)
ex$Height = as.numeric(ex$Height)


# Flag the excluded files in the RAW dataframe
# 1) Work on each filename in the RAW
# 2) Compare root names with exclusion list
# 3) If same, then label every observation under that file to be excluded
for (filename in levels(factor(raw$File.Name.)))
{
  a1 = str_extract(filename, root_pat)
  a2 = str_extract(ex$Name[ex$Width < ex$Height], root_pat) # list of portrait images (bad)
  if (any(a1 %in% a2)) # if it's a bad image
  {
    raw$is_excluded[raw$File.Name. == filename] = TRUE # then flag exclude TRUE
  } else raw$is_excluded[raw$File.Name. == filename] = FALSE
}




# III. 
# Source the revisions
revfiles = list.files(path = "All AGE 10 series counts-03232018\\Revisions - From Manual Validations", full.names = TRUE)
rev = do.call("rbind", lapply(revfiles, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
    round = basename(file),
    is_excluded = TRUE)}
))
colnames(rev) <- gsub('X.', '', colnames(rev), fixed = TRUE)
raw_revised = raw[raw$is_excluded==F,]
raw_revised = rbind(raw_revised, rev)


# IV. Merge in animal details
animal_details = read.csv("animal_details3.csv", fileEncoding="UTF-8-BOM")
animal_details$Mouse.ID. = as.factor(animal_details$Mouse.ID.)
raw_revised = merge(animal_details, raw_revised, by = "Mouse.ID.")

# change everything else to numeric
for (i in c(21:32, 37:57)) {
  raw_revised[,i] = as.numeric(as.character(raw_revised[,i]))
}

# Convert time (hh:mm) to minutes 
raw_revised$DELTA.T = sapply(strsplit(as.character(raw_revised$DELTA.T), ":"), function(x)
{
  x = as.numeric(x)
  x[1] * 60 + x[2]
}
)

raw0 = raw
raw = raw_revised

# Make the Z levels accurate
source("SplitZLevels.R")

# Useful plotting shortcuts
rx = theme(axis.text.x = element_text(angle = 90, hjust = 1))


