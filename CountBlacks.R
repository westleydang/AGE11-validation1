
# Find images with the most number of black pixels

# for each image, find the number of ROI with all black pixels

count.blacks = raw %>% 
  mutate(blackROI = ifelse(..non.zero.pixels. < 0.1, "black", "full")) %>%
  group_by(File.Name.) %>%
  summarize(num.black = length(blackROI[blackROI == "black"]),
            num.full = length(blackROI[blackROI == "full"]),
            total = length(..non.zero.pixels.),
            sum = num.black + num.full,
            mean.black = mean(..non.zero.pixels.[blackROI == "black"]),
            mean.full = mean(..non.zero.pixels.[blackROI == "full"]))

attach(count.blacks)
count.blacks = count.blacks[order(-num.black),] # sort descending
detach(count.blacks)

