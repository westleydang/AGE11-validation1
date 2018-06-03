# Loading the new data
# New data was aggregated by KK
# Also reflects only Z1

library(reshape2)
library(dplyr)

knew = read.csv("SubroiAGGR - jun 3.csv")
source("MakeNormOL.R")
# colnames(knew)[1] = "ID"
# 
# knew = knew %>% filter(! ID %in% c("8351", "8352")) # formerly MouseID

# Melt the data, all measurements in one variable
# names.not.measurements = as.character(read.csv("OUTPUT/names_not_measurements.csv", header=T)$x)
# names.measurements = as.character(read.csv("OUTPUT/names_measurements.csv", header=T)$x)
knew.molten = melt(knew, id.vars = names.not.measurements)

# Display the variable levels (all the measurements)
levels(knew.molten$variable)

# Combine all the transformations
units.counts = levels(knew.molten$variable)[c(1:24,49:72)]
units.density = levels(knew.molten$variable)[c(25:48)]
normalized.yes = levels(knew.molten$variable)[49:72]

# write.csv(units.counts, "OUTPUT/units_count.csv")
# write.csv(units.density, "OUTPUT/units_density.csv")
# write.csv(normalized.yes, "OUTPUT/normalized_yes.csv")

knew.molten = knew.molten %>%
  mutate(UNITS = ifelse(variable %in% units.counts, "counts", "density"),
         NORMD = ifelse(variable %in% normalized.yes, "yes", "no")
         )


brightness.low = names.measurements[seq(1,72,4)]
brightness.mid = names.measurements[seq(2,72,4)]
brightness.hi = names.measurements[seq(3,72,4)]
brightness.total = names.measurements[seq(4,72,4)]

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
chan6 = levels(knew.molten$variable)[grep("NormOL", levels(knew.molten$variable))]

knew.molten = knew.molten %>%
  mutate(CHANNEL = 
           case_when(
             variable %in% chan1 ~ "DAPI",
             variable %in% chan2 ~ "H2BGFP",
             variable %in% chan3 ~ "GFAP",
             variable %in% chan4 ~ "ArcIHC",
             variable %in% chan5 ~ "OL",
             variable %in% chan6 ~ "NormOL"
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

# knew.molten$ID = as.factor(knew.molten$ID)
# knew.molten$EXPT = as.factor(knew.molten$EXPT)
# knew.molten$BRIGHTNESS = as.factor(knew.molten$BRIGHTNESS)


rx = theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
channel.list = c("DAPI", "H2BGFP", "ArcIHC", "GFAP", "OL", "NormOL")

# --- EXCLUDE SubROI with low representation across all mice

pop.means = read.csv("SubroiPopMEANS - jun 3.csv")
exclude.low.pop.means = pop.means$SubROI[pop.means$CountPerROI<0.5*max(pop.means$CountPerROI)]

knew = knew[!(knew$SubROI_SubroiAGGR %in% exclude.low.pop.means),]
knew.molten= knew.molten[!(knew.molten$SubROI_SubroiAGGR %in% exclude.low.pop.means),]
