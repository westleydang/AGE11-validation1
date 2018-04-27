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


# Merge animal details
animal_details = read.csv("animal_details.csv")
animal_details$Mouse.ID. = as.factor(animal_details$ID)
colnames(animal_details)[9] = "idBysubroi"
kk = merge(animal_details, kk, by = "idBysubroi")
colnames(kk)[2] = "GROUP"

# Remove the non-transgenics
kk = kk %>% filter(! idBysubroi %in% c("8351", "8352"))

# Merge group details
group_details = read.csv("group_details.csv")
names(group_details)[1] = "GROUP" # because for some reason it messes up
kk = merge(group_details, kk, by="GROUP")

# Replace the CHANNEL names
replace_details = read.csv("replacements.csv")
names(replace_details) = c("pre", "post") # because for some reason the names mess up
names(kk)[15:19] = as.character(replace_details[1:5,2])

# Normalize the OL counts
kk$NormOL = kk$OL/(kk$ArcIHC + kk$H2BGFP - kk$OL)

# Rename the age groups
kk$EXPT = as.factor(as.character(replace_details$post[match(kk$EXPT, replace_details$pre)]))

# Merge BATCH details
batch_details = read.csv("batch_details.csv")
kk$GRAIN = paste(kk$GROUP, kk$EXPT)
colnames(batch_details) = c("GRAIN", "BATCH")
kk = merge(kk, batch_details, by="GRAIN")

attach(kk)

# Transform kk to a long format
kk.long = melt(kk, id.vars=names(kk[c(1:15,22)]), variable.name = "CHANNEL", value.name="DENSITY")

rx = theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
