

# Load the raw file, exclude and revise, and refactor for KK's macro

source("LoadLibs.R")
source("LoadProcessRAW-May2018.R")

a = read.csv("OUTPUT/wes_rawz1b.csv", header=T)
names(a[2:55])
rawz1.toexport = raw.z1[,names(a[2:55])]
write.csv(rawz1.toexport, "OUTPUT/WES_rawz1_jun1.csv")