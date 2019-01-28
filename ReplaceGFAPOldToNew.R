## Transfer old GFAP to replace new GFAP
## from Sep6 to Dec18

# load the files
data1 = read.csv("SubroiAGGR - Sep6.csv")
data2 = read.csv("SubroiAGGR - dec18.csv")

# see the means first
mean(data1$Ch3Total) #633.5088
mean(data2$Ch3Total) #9529.914

# make the matches
match_data1 = paste(data1$MouseID, data1$SubROI_SubroiAGGR)
match_data2 = paste(data2$MouseID, data2$SubROI_SubroiAGGR)

# all gfap variable columns
where_gfap_columns = (grep("Ch3", names(data2)))

# loop it
for (each in where_gfap_columns) {
  colname = colnames(data2[each])
  data2[,colname] = data1[match(match_data2, match_data1), colname]
}


mean(na.omit(data2$Ch3Total)) #652.8121

abc = data2[is.na(data2$Ch3m05sd),]

data2 = data2[!(data2$Ch3Total %in% abc$Ch3Total),]

write.csv(data2, "SubroiAGGR - dec18-oldGFAP.csv")