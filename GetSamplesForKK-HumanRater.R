
# This R script is to find a sample of images for KK's human/computer expt
# First you get the Z1 image files
# Then filter for only the ones in the bregma coordinates KK wants
# then for each bregma, sample 3 images
# Record that data into a csv file and save it somewhere

# This script assumes that we have already loaded the raw data and then filtered for Z1
# PrepforKK.R has the raw data
a = read.csv("OUTPUT/wes_rawz1b.csv", header=T)

# Here are the list of bregma coordinates that she wants



KK.wants.bregma = c(-3, -2, -1.5, -1, 1, 1.5, 2)

KK.wants.images = a %>%
  filter(Bregma. %in% KK.wants.bregma) %>%
  group_by(Bregma., File.Name.)  %>%
  summarize(n = n())


KK.wants.files = KK.wants.images %>%
  group_by(Bregma.) %>%
  sample_n(3)

write.csv(KK.wants.files, "KK_HumanRatingSamples.csv")

