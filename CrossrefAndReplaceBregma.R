

# Load libraries
library(dplyr)
library(stringr)


# Variables and aliases
#

output.folder = "E:/TestDir/New 02_COUNTS/"
jpv2 = read.csv("OUTPUT/for_jeanne.csv", header=T, fileEncoding="UTF-8-BOM")
output.log = paste(output.folder, 'sink2.csv', sep="")


# Get list of dirs AND sub-dirs
# FOR USER:
# MAKE A CSV FILE OF ALL YOUR PARENT FOLDERS YOU WANT TO RUN
# (WHICH IS THE FOLDER THAT HOUSES ALL THE ANIMAL FOLDERS)
# PUT THE FIRST ROW AS THE HEADER "Directory"
list.dirs1 = read.csv("Dirs_For_TestDirsRun.csv", header=T, fileEncoding="UTF-8-BOM")

# Get the subdirs list
final.dirs = c()
for (parentDir in list.dirs1$Directory) {
  final.dirs = append(paste(parentDir, list.files(parentDir), sep="\\"), final.dirs)
}

counter_copied = 0
counter_total = 0
counter_renamed = 0

# put into final merged list of sub-dirs
for (dir in final.dirs) {
  # dir.create(output.folder)
  # file.create(output.log)
  # sink(output.log, append=T)
  
  # For each sub-dir, get list of tif files
  lf = list.files(dir)
  lf = lf[grepl(".tif$", lf)==T]
  
  # For each tif file, cross reference image sequence and animal name
  for (file in lf) {
    counter_total = counter_total+1
    
    # get the tif file info
    image.sequence = strsplit(file, "_")[[1]][1]
    zlevel = strsplit(file, "_")[[1]][2]
    breg = strsplit(file, "_")[[1]][3]
    id = strsplit(file, "_")[[1]][4]
    hemi = strsplit(file, "_")[[1]][5]
    flag = strsplit(file, "_")[[1]][6]
    
    # match tif file animal in temp df
    temp.find = jpv2 %>% filter(ID == id)
    
    # get the hemispheres...
    temp.find$File.Name. = as.character(temp.find$File.Name.)
    temp.find$hemisphere = str_split(temp.find$File.Name., "_", simplify=T)[,5]
    
    
    # Match the image sequence AND the hemisphere in the temp.find table
    temp.find = temp.find[temp.find$sequence.no == image.sequence 
                          & temp.find$hemisphere == hemi,]
    print(temp.find)
    # If there's a match
    if (dim(temp.find)[1] == 1) {
      print('FOUND')
      # Get that new bregma
      new.bregma = temp.find$bregma[temp.find$sequence.no == image.sequence 
                                    & temp.find$hemisphere == hemi]
      # Put that new bregma into the new file name
      new.filename = paste(image.sequence, zlevel, new.bregma, id, hemi, flag, sep="_")
      
      # Copy the original file to the output folder
      file.copy( paste(dir, file, sep="\\") , output.folder)
      print(paste("COPIED:",  file, "to", output.folder))
      
      # If the bregmas don't match then rename the file
      if (breg != new.bregma) {
        file.rename( paste(output.folder, file, sep="/"), 
                     paste(output.folder, new.filename, sep="/") )
        
        print( paste( "Bregma was renamed from", 
                      breg,
                      "to", 
                      new.bregma) )
        counter_renamed = counter_renamed + 1

      } 
      print( "NO NAME CHANGE: Bregma already matched records. " ) 
      
      counter_copied = counter_copied + 1
      
    } else print(paste("NO MATCH: File does not match records.", file))
    
  }
  print(paste(counter_copied, "out of", counter_total, "copied so far"))
  print(paste(counter_renamed, "out of", counter_copied, "copied were renamed"))
  print("---")
  # sink()
}

# ---

# go to the folder and then reorganize the files
# output.folder = "C:\\Users\\westley\\OneDrive - The Scripps Research Institute\\Samples for KK Human Ratings\\Raw images"
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



