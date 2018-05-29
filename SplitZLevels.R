# Makes sure the Z levels are accurate

library(dplyr)

fileseq = sapply(as.character(raw$File.Name.), function(x) strsplit(x, "_")[[1]][1])
fileseq = as.numeric(fileseq)
raw$fseq = fileseq

raw$Z.Level. = ((fileseq - 1 ) %% 3) + 1
raw$Z.Level. = as.factor(raw$Z.Level.)

raw %>% group_by(Z.Level.) %>% summarize(avg_intensity = mean(Ch3.Mean.), avg_count = mean(Ch3.Total.))

ggplot(raw) + aes(Z.Level., Ch3.Total.) + geom_violin() + geom_boxplot() 

raw.z1 = raw %>% filter(Z.Level. == '1')
raw.z2 = raw %>% filter(Z.Level. == '2')
raw.z3 = raw %>% filter(Z.Level. == '3')