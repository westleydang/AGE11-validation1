

# This script is to do the following
library(stringr)
# Get two folders to compare:
# 1 - the ARCHIVE folder
# 2 - the COUNTS folder


folder.archive = "E:\\WestleyAGE - July 2018\\round 2\\MACRO_ANALYSIS\\03_ARCHIVE"
folder.counts = "E:\\WestleyAGE - July 2018\\round 2\\MACRO_ANALYSIS\\02_COUNTS"
folder.output = "E:\\WestleyAGE - July 2018\\round 2\\MACRO_ANALYSIS\\FILES_DIDNT_COUNTS"

# Get a list of the files in each folder

# get a list of subfolders there in archive
subfolder.list.archive = c()
subfolder.list.archive = append(paste(folder.archive, list.files(folder.archive), sep="\\"), subfolder.list.archive)

subfolder.list.counts = c()
subfolder.list.counts = append(paste(folder.counts, list.files(folder.counts), sep="\\"), subfolder.list.counts)


files.archive = c()
for (subdir in subfolder.list.archive) {
  files.archive = append(list.files(subdir), files.archive)
}

files.counts = c()
for (subdir in subfolder.list.counts) {
  files.counts = append(list.files(subdir), files.counts)
}


# Separate out the flags so you can compare them

files.archive = str_replace(files.archive, "CLEAN.tif", "")
files.counts = str_replace(files.counts, "COUNTS.tif", "")

# Get me a list of the ARCHIVE files that aren't in COUNTS
difference = setdiff (files.archive, files.counts)


# Move them somewhere else

difference = data.frame(difference)
names(difference) = c("diff")
difference$diff = as.character(difference$diff)
difference$animal = str_split(difference$diff, "_", simplify=T)[,4]
difference$path = paste(folder.archive, difference$animal, paste(difference$diff, "CLEAN.tif", sep=""), sep="\\")
difference$file = paste(difference$diff, "CLEAN.tif", sep="")

dir.create(folder.output)
file.copy( paste(folder.archive, difference$animal, difference$file, sep="\\"),
           paste(folder.output, difference$file, sep="\\"))
