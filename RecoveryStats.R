


# how many are there?
length(unique(raw$File.Name.[raw$is_excluded==T]))
library(dplyr)
library(broom)
s = raw %>%
  group_by(round) %>%
  summarize(
    excluded = length(unique(File.Name.[is_excluded==T])), 
    total = length(unique(File.Name.)),
    excluded.pct = 100*excluded/total)

ggplot(s) + aes(round, excluded.pct) + geom_bar(stat="identity") + coord_flip() + ylim(0,100) +
  ggtitle("% excluded from dataset because of bad image or bad bregma")


t = raw_revised %>%
  group_by(round) %>%
  summarize(
    excluded = length(unique(File.Name.[is_excluded==T])), 
    total = length(unique(File.Name.)),
    excluded.pct = 100*excluded/total)


summarize.revisions = raw_revised %>%
  group_by(Mouse.ID.) %>%
  summarize(
    revised = length(unique(File.Name.[is_excluded==T])), 
    total_post_rev = length(unique(File.Name.)),
    revised.pct = 100*revised/total_post_rev)

summarize.exclusions = raw %>%
  group_by(Mouse.ID.) %>%
  summarize(
    excluded = length(unique(File.Name.[is_excluded==T])), 
    total_post_ex = length(unique(File.Name.)),
    revised.pct = 100*excluded/total_post_ex)

summary.validation = merge(summarize.exclusions, summarize.revisions, by="Mouse.ID.")

summary.validation = summary.validation %>%
  mutate(lost.forever = total_post_ex - total_post_rev,
         unrevisable = excluded - revised,
         recovery.pct = 100*revised/excluded)

summary.validation %>%
  ggplot() + aes(as.factor(Mouse.ID.), recovery.pct) + geom_bar(stat="identity") + coord_flip() +
  ggtitle("How much was recovered after exclusions (%)") + ylim(0,100)
