# REQUIRES: nothing

# Load the raw file, exclude and revise, and refactor for KK's macro

source("LoadLibs.R")
source("LoadProcessRAW-Sep2018.R")

# Get the variables and their order from a file that was FOR SURE
# worked with KK's Matlab script in the past. This is to ensure
# that the variables/order are identical
a = read.csv("OUTPUT/wes_rawz1b.csv", header=T)
# names(a[2:55])

# Reorder the dataframe variables so that they match
rawz1.toexport = raw.z1[,names(a[2:55])]

# Save the csv files
write.csv(rawz1.toexport, "OUTPUT/WES_rawz1_sep06.csv")
