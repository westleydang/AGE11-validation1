
# get stats on the images and automation method


# load the data, these are pre-exclusion
csvfiles = list.files(path = "All AGE 10 series counts-03232018/CombinedCounts/", full.names = TRUE)

raw = do.call("rbind", lapply(csvfiles, FUN = function(file) read.csv(file, header = TRUE, sep = ",")))
# make the column names easier to read by removing the 'X.'
colnames(raw) <- gsub('X.', '', colnames(raw), fixed = TRUE)


# get number of files per each bregma 
validate.bregmas = raw %>% dplyr::select(1:26) %>%
  dcast(Mouse.ID. + Bregma. + File.Name. ~ Sub.ROI., value.var="Ch1.Mean.", fun.aggregate=length) %>%
  group_by(Mouse.ID., Bregma., File.Name) %>%
  summarize(n())

validate.bregmas$Bregma. = as.numeric(as.character(validate.bregmas$Bregma.))

ggplot(validate.bregmas) + aes(Bregma.) + geom_density() + theme_minimal() + 
  ggtitle("Bregmas density plot, all animals, pre-exclusion")


# now get it after it was revised


# I. 
# Load the original raw data
csvfiles2 = list.files(path = "All AGE 10 series counts-12122018-again/csv/", full.names = TRUE)
raw2 = do.call("rbind", lapply(csvfiles2, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
    round = basename(file)
  )
}
)
)

# make the column names easier to read by removing the 'X.'
colnames(raw2) <- gsub('X.', '', colnames(raw2), fixed = TRUE)
# get number of files per each bregma 
validate.bregmas2 = raw2 %>% dplyr::select(1:26) %>%
  dcast(Mouse.ID. + Bregma. + File.Name. ~ Sub.ROI., value.var="Ch1.Mean.", fun.aggregate=length) %>%
  group_by(Mouse.ID., Bregma.) %>%
  summarize(n())
validate.bregmas2$Bregma. = as.numeric(as.character(validate.bregmas2$Bregma.))
ggplot(validate.bregmas2) + aes(Bregma.) + geom_density() + theme_minimal() + 
  ggtitle("Bregmas density plot, all animals, pre-exclusion")

ggplot() + 
  geom_density(data=validate.bregmas, aes(Bregma.))  + 
  geom_density(data=validate.bregmas2, aes(Bregma., color="red")) +
  theme_minimal() + theme(legend.position = "none") + 
  ggtitle("Bregma population size, all animals, before and after (red) corrections")



## get the bregma errors

# II. 
# Exclusions

# KK's usual file naming pattern is:
# seq _ z _ bregma _ animal _ flags .tif
# So find num_num_num_num_ with optional '-' for negative bregma
root_pat = "^[:digit:]{1,3}_[:digit:]{1,3}_[-]?.{1,3}_[:xdigit:]{1,4}_"

# Load in the image metadata and clean it
exfiles = list.files(path = "All AGE 10 series counts-03232018/Rotations or Exclusions - From Manual Validations", full.names = TRUE)


ex = do.call("rbind", lapply(exfiles, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
    round = basename(file) )}
))


# Flag the excluded files in the RAW dataframe
# 1) Work on each filename in the RAW
# 2) Compare root names with exclusion list
# 3) If same, then label every observation under that file to be excluded

ex = ex %>% mutate(error = ifelse(Height > Width, "ERROR", "Safe"))

ex = ex %>% mutate(fileroot = str_extract(as.character(Name), root_pat))
ex.errors = ex %>% filter(error == 'ERROR') %>% pull(fileroot)

raw = raw %>% mutate(f_root = str_extract(as.character(File.Name.), root_pat),
                     is_excluded = ifelse(f_root %in% ex.errors, "yes", "no"))
raw$is_excluded = as.factor(raw$is_excluded)
summary(raw$is_excluded)

# get the bregmas of all the wrong ones
a = raw %>% filter(is_excluded=='yes') %>% 
  dcast(Mouse.ID. + Bregma. + File.Name. ~ Sub.ROI., value.var="Ch1.Mean.", fun.aggregate=length) %>%
  group_by(Mouse.ID., Bregma., File.Name.) %>%
  summarize(n()) 

a$Bregma. = as.numeric(as.character(a$Bregma.))
a %>% 
  ggplot() + aes(Bregma.) + geom_density()


# III. 
# Source the revisions
revfiles = list.files(path = "All AGE 10 series counts-03232018/Revisions - From Manual Validations", full.names = TRUE)
rev = do.call("rbind", lapply(revfiles, FUN = function(file) {
  data.frame(
    read.csv(file, header = TRUE, sep = ",", fileEncoding="UTF-8-BOM"), 
    round = basename(file),
    is_excluded = TRUE)}
))
colnames(rev) <- gsub('X.', '', colnames(rev), fixed = TRUE)

rev = rev %>% 
  dcast(Mouse.ID. + Bregma. + File.Name. ~ Sub.ROI., value.var="Ch1.Mean.", fun.aggregate=length) %>%
  group_by(Mouse.ID., Bregma., File.Name.) %>%
  summarize(n()) %>%
  mutate(rev_root = str_extract(File.Name., root_pat),
         img = str_split(rev_root, "_", simplify=T)[1],
         img_grain = paste(Mouse.ID., img))


raw = raw %>%
  mutate(img = str_split(File.Name., "_", simplify=T)[1],
         img_grain = paste(Mouse.ID., img))
raw = raw %>% 
  mutate(was_revised = ifelse(img_grain %in% rev$img_grain, "yes", "no"))
raw$was_revised = as.factor(raw$was_revised)

raw = raw %>%
  dcast(Mouse.ID. + Bregma. + File.Name. + is_excluded + was_revised~ Sub.ROI., value.var="Ch1.Mean.", fun.aggregate=length) %>%
  group_by(Mouse.ID., Bregma., File.Name., is_excluded, was_revised) %>%
  summarize(n())

raw = raw %>% mutate(lost = case_when(
  is_excluded=='yes' & was_revised=='no' ~ "lost", 
  is_excluded=='no' ~ "ok",
  is_excluded=='yes' & was_revised=='yes' ~ "recovered"))

raw$lost = as.factor(raw$lost)

# ok now get stats
summary(raw$is_excluded)
summary(raw$was_revised)
summary(raw$lost)

# ok now see if you can see which bregma changed
rev.to.merge = rev %>% dplyr::select(c(img_grain, File.Name.))
rev.to.merge = rev.to.merge[-c(1:2)]
names(rev.to.merge)[2] = "rev_fn"
names(rev.to.merge)

raw$Bregma. = as.numeric(as.character(raw$Bregma.))
raw$File.Name. = as.character(raw$File.Name.)
raw$rev_fn = as.character(raw$rev_fn)

raw %>%
  mutate(img_grain = paste(Mouse.ID., str_split(File.Name., "_", simplify=T)[1])) %>%
  merge(., rev.to.merge, by="img_grain") %>%
  mutate(
    old.bregma = str_split(as.character(File.Name.), "_", simplify=T)[3],
    new.bregma = str_split(as.character(rev_fn), "_", simplify=T)[3]
  )



chk = raw %>% dplyr::select(is_excluded, was_revised)
chk = chk %>% mutate(lost = ifelse(is_excluded=='yes' & was_revised=='no', "lost", "recovered"))
chk$lost = as.factor(chk$lost)
summary(chk$lost)

rev %>% mutate(grain = paste(Mouse.ID., Image))
ggplot(rev) + aes(Bregma.) + geom_density()
rev.bregmas = as.numeric(as.character(rev$Bregma.))







