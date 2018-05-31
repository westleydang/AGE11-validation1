# Select random images


nrow(raw)

# take raw z1 only

fileseq = sapply(as.character(raw$File.Name.), function(x) strsplit(x, "_")[[1]][1])
fileseq = as.numeric(fileseq)
raw$fseq = fileseq

raw$Z.Level. = ((fileseq - 1 ) %% 3) + 1
raw$Z.Level. = as.factor(raw$Z.Level.)

raw %>% group_by(Z.Level.) %>% dplyr::summarize(avg_intensity = mean(Ch3.Mean.), avg_count = mean(Ch3.Total.))

raw.z1 = raw %>% filter(Z.Level. == '1')

# then sample

library(dplyr)
sample.images = raw.z1 %>%
  mutate(grain = paste(EXPT, GROUP)) %>%
  group_by(grain) %>%
  do(sample_n(., 4))

sample.images$File.Name. 

write.csv(sample.images$File.Name., "Sample Images for Manual Validation.csv")


