# EXCLUSIONS STATS


# MAKE LIST OF FILES
# READ EXCLUSION FILES
csvfiles.excluded = list.files(path = "All AGE 10 series counts-03232018/Rotations or Exclusions - From Manual Validations/", full.names = TRUE)
excluded.files = do.call("rbind", lapply(csvfiles.excluded, FUN = function(file) read.csv(file, header = TRUE, sep = ",")))




# GSUB THE DIMENSIONS

excluded.files$Width = gsub("\\?", "", excluded.files$Width)
excluded.files$Height = gsub("\\?", "", excluded.files$Height)

excluded.files$Width = gsub(" pixels", "", excluded.files$Width)
excluded.files$Height = gsub(" pixels", "", excluded.files$Height)

excluded.files$Width = as.numeric(excluded.files$Width)
excluded.files$Height = as.numeric(excluded.files$Height)


# NEW VECT EXCLUDED?

library(dplyr)
excluded.files = excluded.files %>%
  mutate(
    rotated = ifelse(Width < Height, 1, 0)
  )

# DO AVERAGE PER SET
mean(excluded.files$rotated)

