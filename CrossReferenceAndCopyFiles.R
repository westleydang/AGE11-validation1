

# ----

# Variables and aliases

# home = "C:\\Users\\westley\\OneDrive - The Scripps Research Institute\\Samples for KK Human Ratings\\Counted"
output.folder = "E:/WestleyAGE - July 2018/"
jpv2 = read.csv("OUTPUT/for_jeanne.csv", header=T, fileEncoding="UTF-8-BOM")


# Get list of dir
list.dirs1 = read.csv("list_input_images_J2.csv", header=T, fileEncoding="UTF-8-BOM")

# Get their sub-dir
final.dirs = c()
for (parentDir in list.dirs1$Directory) {
  final.dirs = append(paste(parentDir, list.files(parentDir), sep="\\"), final.dirs)
}


# put into final list of dirs
for (dir in final.dirs) {
  counter_copied = 0
  counter_total = 0
  
  # For each dir, get list of tif files
  lf = list.files(dir)
  lf = lf[grepl(".tif$", lf)==T]
  
  # For each tif file, cross reference image sequence and animal name
  for (file in lf) {
    counter_total = counter_total+1
    
    id = strsplit(file, "_")[[1]][4]
    image.sequence = strsplit(file, "_")[[1]][1]
    
    # Temp df to match animal
    temp.find = jpv2 %>% filter(ID == id)
    
    # within animal temp, copy if image sequence matches
    if( image.sequence %in% temp.find$sequence.no ) {
      file.copy( paste(dir, file, sep="\\") , output.folder)
      print(paste("COPIED",  file, "to", output.folder))
      counter_copied = counter_copied + 1
      
    } else print(paste("DOES NOT MATCH", file))
    
    print(paste(counter_copied, "out of", counter_total, "copied so far"))
  }
  
}

# ---

# go to the folder and then reorganize the files
# output.folder = "C:\\Users\\westley\\OneDrive - The Scripps Research Institute\\Samples for KK Human Ratings\\Raw images"

library(stringr)
output.folder ="E:/WestleyAGE - July 2018/round 2/MACRO_ANALYSIS/03-archive-new"
output.files = list.files(output.folder)

# get all the animal names unique value
output.animals = str_split(output.files, "_", simplify=T)[,4]
output.animals.unique = unique(output.animals)

# make directory for each animal
for (animals in output.animals.unique) {
  to.create = paste(output.folder, animals, sep="/")
  dir.create( to.create )
  print( paste("directory created: ", to.create) )
}

# for each image, move to their folder
for (img in output.files) {
  id = strsplit(img, "_")[[1]][4]
  move.from = paste(output.folder, img, sep="/")
  move.to = paste(output.folder, id, img, sep="/")
  file.rename( move.from , move.to)
}



