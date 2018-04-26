# Loading and processing the data
# This script is sourced from downstream notebooks
# as a common source point for processed data. 



# Load libraries
library(ggplot2)
library(dplyr)
library(plotrix)
library(GGally)
library(reshape2)

# Load the data
kk = read.csv(file="All AGE 10 series counts-03232018/Wes_SUBROI_CLN_STACKED.csv", header=TRUE, sep=",")
kk$idBysubroi = as.factor(kk$idBysubroi)

# Remove the non-transgenics
k2 = kk %>% filter(! ID %in% c("8351", "8352"))


# Merge animal details
animal_details = read.csv("animal_details.csv")
animal_details$Mouse.ID. = as.factor(animal_details$ID)
colnames(animal_details)[9] = "idBysubroi"
kk = merge(animal_details, kk, by = "idBysubroi")
colnames(kk)[2] = "GROUP"




# Merge group details
group_details = read.csv("group_details.csv")
names(group_details)[1] = "GROUP" # because for some reason it messes up
kk = merge(group_details, kk, by="GROUP")

replace = read.csv("replacements.csv")
names(replace) = c("pre", "post") # because for some reason the names mess up

# Rename the age groups
kk$EXPT = as.factor(as.character(replace$post[match(kk$EXPT, replace$pre)]))

# normalize the OL counts
kk$OLmm2.norm = kk$OLmm2
kk$OLmm2.norm = kk$OLmm2/(kk$Ch2mm2 + kk$Ch4mm2 - kk$OLmm2)

# Rename all variable names of the channels
names(kk)[15:20] = as.character(replace[1:6,2])

attach(kk)

# Transform rawkk to a long format
kk.long = melt(kk, id.vars=names(kk[1:14]), variable.name = "CHANNEL", value.name="DENSITY")

