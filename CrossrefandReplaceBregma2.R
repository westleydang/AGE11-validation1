## PURPOSE:
## The goal of this macro is to change bregma of filenames
## to match the ones that have already been corrected before. 

## Here the output is to replace the name,
## instead of copying a new file. Hopefully this works! 


# Load libraries
library(dplyr)
library(stringr)


# Variables and aliases


output.folder = "E:/WestleyAGE - July 2018/All New 02_COUNTS copied/"

# for_jeanne is where all the CORRECTED BREGMAS are
jpv2 = read.csv("OUTPUT/for_jeanne.csv", header=T, fileEncoding="UTF-8-BOM")

output.log = paste(output.folder, 'renameLOG-dec16b.csv', sep="")

dir.create(output.folder)


# Get list of dirs AND sub-dirs
# FOR USER:
# MAKE A CSV FILE OF ALL YOUR PARENT FOLDERS YOU WANT TO RUN
# (WHICH IS THE FOLDER THAT HOUSES ALL THE ANIMAL FOLDERS)
# PUT THE FIRST ROW AS THE HEADER "Directory"
list.dirs1 = read.csv("Dirs_For_AgeReRunDec16.csv", header=T, fileEncoding="UTF-8-BOM")

# Get the subdirs paths
final.dirs = c()
for (parentDir in list.dirs1$Directory) {
  final.dirs = append(paste(parentDir, list.files(parentDir), sep="\\"), final.dirs)
}

counter_copied = 0
counter_total = 0
counter_renamed = 0

log.df = data.frame(matrix(ncol=4))
names(log.df) = c("old fn ", "new fn", "old b", "new b")

# put into final merged list of sub-dir paths
for (dir in final.dirs) {
  # dir.create(output.folder)
  # file.create(output.log)
  sink(output.log, append=T)
  
  # For each sub-dir, get list of tif files
  lf = list.files(dir) # these are not paths! just the filenames
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
      
      # If the bregmas don't match then rename the file
      if (breg != new.bregma) {
        file.rename( paste(dir, file, sep="/"), 
                     paste(dir, new.filename, sep="/") )
        
        print( paste( "Bregma was renamed from", 
                      breg,
                      "to", 
                      new.bregma) )
        counter_renamed = counter_renamed + 1
        
      } 
       else print( "NO NAME CHANGE: Bregma already matched records. " ) 
      counter_copied = counter_copied + 1
    } else print(paste("NO MATCH: File does not match records.", file))
    
    log.df = rbind(log.df, c(file, new.filename, breg, new.bregma))
    
  } # end file loop
  print(paste(counter_copied, "out of", counter_total, "copied so far"))
  print(paste(counter_renamed, "out of", counter_copied, "copied were renamed"))
  print("---")
  sink()
} # end subdir loop
