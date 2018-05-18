# Loading the new data
# New data was aggregated by KK
# Also reflects only Z1

library(reshape2)
library(dplyr)

knew = read.csv("SubroiAGGR.csv")
colnames(knew)[1] = "ID"

knew = knew %>% filter(! ID %in% c("8351", "8352"))

# Melt the data
knew.molten = melt(knew, id.vars = names(knew)[c(1:13, 34:39)])

# Display the variable levels
levels(knew.molten$variable)

    
# Combine all the transformations

knew.molten = melt(knew, id.vars = names(knew)[c(1:13, 34:39)])
units.counts = levels(knew.molten$variable)[c(1:20,41:60)]
units.density = levels(knew.molten$variable)[c(21:40)]

knew.molten = knew.molten %>%
  mutate(UNITS = ifelse(variable %in% units.counts, "counts", "density"))

normalized.yes = levels(knew.molten$variable)[41:60]
knew.molten = knew.molten %>%
  mutate(NORMD = ifelse(variable %in% normalized.yes, "yes", "no"))

brightness.low = levels(knew.molten$variable)[seq(1,60,4)]
brightness.mid = levels(knew.molten$variable)[seq(2,60,4)]
brightness.hi = levels(knew.molten$variable)[seq(3,60,4)]
brightness.total = levels(knew.molten$variable)[seq(4,60,4)]

knew.molten = knew.molten %>%
  mutate(BRIGHTNESS = 
           case_when(
             variable %in% brightness.low ~ "low",
             variable %in% brightness.mid ~ "mid",
             variable %in% brightness.hi ~ "hi",
             variable %in% brightness.total ~ "total"
           ))

chan1 = levels(knew.molten$variable)[grep("Ch1", levels(knew.molten$variable))]
chan2 = levels(knew.molten$variable)[grep("Ch2", levels(knew.molten$variable))]
chan3 = levels(knew.molten$variable)[grep("Ch3", levels(knew.molten$variable))]
chan4 = levels(knew.molten$variable)[grep("Ch4", levels(knew.molten$variable))]
chan5 = levels(knew.molten$variable)[grep("OL", levels(knew.molten$variable))]

knew.molten = knew.molten %>%
  mutate(CHANNEL = 
           case_when(
             variable %in% chan1 ~ "DAPI",
             variable %in% chan2 ~ "H2BGFP",
             variable %in% chan3 ~ "GFAP",
             variable %in% chan4 ~ "ArcIHC",
             variable %in% chan5 ~ "OL"
           ))
knew.molten$CHANNEL = as.factor(knew.molten$CHANNEL)


# Rename the EXPT details
knew.molten = knew.molten %>%
  mutate(EXPT = 
           case_when(
             EXPT == 11 ~ "OLD",
             EXPT == 10 ~ "YOUNG"
           ))
knew.molten$GRAIN = paste(knew$EXPT, knew$GROUP)


rx = theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
channel.list = c("DAPI", "H2BGFP", "ArcIHC", "GFAP", "OL", "NormOL")
