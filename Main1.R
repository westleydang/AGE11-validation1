

# load some libs 
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(stringr)


# load the data
csvfiles = list.files(path = "All AGE 10 series counts-03232018/CombinedCounts/", full.names = TRUE)
raw = do.call("rbind", lapply(csvfiles, FUN = function(file) read.csv(file, header = TRUE, sep = ",")))

# make the column names easier to read by removing the 'X.'
colnames(raw) <- gsub('X.', '', colnames(raw), fixed = TRUE)

# Change Mouse ID to a factor, not a numeric
raw$Mouse.ID. = as.factor(raw$Mouse.ID.)

# Merge in animal details
animal_details = read.csv("animal_details.csv")
animal_details$Mouse.ID. = as.factor(animal_details$ID)
raw = merge(animal_details, raw, by = "Mouse.ID.")

# Convert time (hh:mm) to minutes 
raw$DELTA.T = sapply(strsplit(as.character(raw$DELTA.T), ":"), function(x)
{
  x = as.numeric(x)
  x[1] * 60 + x[2]
}
)



# show the number of different values per ROI variables
factor(levels(raw$System.)) # 17
factor(levels(raw$Major.ROI.)) # 69
factor(levels(raw$Sub.ROI)) # 82

# Useful plotting shortcuts
rx = theme(axis.text.x = element_text(angle = 90, hjust = 1))
