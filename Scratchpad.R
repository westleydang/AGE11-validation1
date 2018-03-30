



# Put all the channels variables into lists
raw.channels = raw[17:24]
raw.channels.totalCounts = raw[25:28]
raw.channels.segmentedCounts = raw[34:53]
raw.allcounts = raw[c(17:24, 25:28, 34:53)]



raw.allcounts.table = cbind(lapply(raw.allcounts, function(x) mean(x)))
raw.allcounts = raw.allcounts/raw$ROI.Area..px..
raw.allcounts.table = cbind(raw.allcounts.table, lapply(raw.allcounts, function(x) mean(x)))



# I want to figure out how to separate the z appropriately
# Do divisibility rules where (x%%3)+1 = z
# 1. Find x per filename by parsing via regex
raw$fileseq = sapply(as.character(raw$File.Name.), function(x) strsplit(x, "_")[[1]][1])
raw$fileseq = as.numeric(raw$fileseq)
raw$Z.Level. = (raw$fileseq %% 3) + 1
raw$Z.Level. = as.factor(raw$Z.Level.)

