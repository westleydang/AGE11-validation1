

# how many unique filenames are there per animal?

# go to raw dataset

source("LoadProcessRAW-Dec2018.R")

z1.count = raw.z1 %>%
  mutate(GRAIN = paste(EXPT, VALENCE, CONTEXT, sep = "-")) %>%
  group_by(Mouse.ID., GRAIN, File.Name.) %>%
  summarize(n()) %>%
  group_by(Mouse.ID., GRAIN) %>%
  summarize(n = n())

# see histogram
hist(z1.count$n)

# see ID by count
z1.count %>%
  filter(n < 20) %>%
  ggplot() + aes(reorder(Mouse.ID., n), n, fill = GRAIN) + geom_bar(stat = "identity") + rx




## BUT WHAT IF YOU ASK HOW MANY AREAS THEY REPRESENT?

z1.count2 = raw.z1 %>%
  mutate(GRAIN = paste(EXPT, VALENCE, CONTEXT, sep = "-")) %>%
  group_by(Mouse.ID., GRAIN, Sub.ROI.) %>%
  summarize(n()) %>%
  group_by(Mouse.ID., GRAIN) %>%
  summarize(n = n())
# see histogram
hist(z1.count2$n)

# see ID by count
z1.count2 %>%
  # filter(n < 20) %>%
  ggplot() + aes(reorder(Mouse.ID., n), n, fill = GRAIN) + geom_bar(stat = "identity") + 
  ylab("num of subrois") + theme_minimal() + geom_hline(yintercept=54) + rx + xlab("Animal") + 
  ggtitle("Number of regions sampled per mouse")

# see ID by count under 60
z1.count2 %>%
  filter(n < 60) %>%
  ggplot() + aes(reorder(Mouse.ID., n), n, fill = GRAIN) + geom_bar(stat = "identity") + rx +
  ylab("n of subrois")

# if we had to remove on the basis of number of subrois represented...
qc.roi = z1.count2 %>% filter(n < 60) %>% pull(Mouse.ID.) %>% as.vector()


