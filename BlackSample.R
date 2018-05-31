# Excluding ROIs with large zero pixels

# This is a little confusing because non-zero-pixels means how many of them are signal
# so we need to find the ones with very LOW non zero pixels

# --- 

# INPUT = 'raw' dataframe
# set as working dataframe
blacks = raw

# plot general distribution
hist(blacks$..non.zero.pixels.)
# most are in the 0-20% range
# check them by batch
# RESULT: most are from the B batch
ggplot(blacks) + aes(..non.zero.pixels.) +
  geom_density() + facet_wrap(~BATCH)

# check whether they were excluded
# RESULT: the ones blacks were mostly excluded
ggplot(blacks) + aes(..non.zero.pixels.) + 
  geom_density() + facet_wrap(~is_excluded)

# check where the images/areas are supposedly all black
# sample some images/areas from there
library(dplyr)
sample_blacks = blacks %>%
  filter(..non.zero.pixels. < 1) %>%
  sample_n(10)

# save the file 
write.csv(sample_blacks[,c(1:15,34:37)], "Sample Images to check for zero pixels.csv")


# check whether the LA is black in the B batch
c = blacks %>%
  filter(BATCH=="B") %>%
  filter(Sub.ROI.=="LA")

ggplot(c) + aes(..non.zero.pixels., OL.Total.) + geom_jitter()
