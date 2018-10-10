# creates the normalized OL variables

ol = function(OL, A, B) {
  NormOL = OL / (A + B - OL)
  NormOL
}  
ol(.5, 1, .5)


knew = knew %>%
  mutate(
    # counts, not normd
    NormOLm05sd = ol(OLm05sd, Ch2m05sd, Ch4m05sd), 
    NormOLm05sdm05sd = ol(OLm05sdm05sd, Ch2m05sdm05sd, Ch4m05sdm05sd),
    NormOLm05sd1 = ol(OLm05sd1, Ch2m05sd1, Ch4m05sd1),
    NormOLTotal = ol(OLTotal, Ch2Total, Ch4Total),
    # densities
    NormOLm05sdperMM2 = ol(OLm05sdperMM2, Ch2m05sdperMM2, Ch4m05sdperMM2), 
    NormOLm05sdm05sdperMM2 = ol(OLm05sdm05sdperMM2, Ch2m05sdm05sdperMM2, Ch4m05sdm05sdperMM2),
    NormOLm05sd1perMM2 = ol(OLm05sd1perMM2, Ch2m05sd1perMM2, Ch4m05sd1perMM2),
    NormOLTotalperMM2 = ol(OLTotalperMM2, Ch2TotalperMM2, Ch4TotalperMM2),
    # counts, yes normd
    NormOLm05sd_COUNTnormd = ol(OLm05sd_COUNTnormd, Ch2m05sd_COUNTnormd, Ch4m05sd_COUNTnormd), 
    NormOLm05sdm05sd_COUNTnormd = ol(OLm05sdm05sd_COUNTnormd, Ch2m05sdm05sd_COUNTnormd, Ch4m05sdm05sd_COUNTnormd),
    NormOLm05sd1_COUNTnormd = ol(OLm05sd1_COUNTnormd, Ch2m05sd1_COUNTnormd, Ch4m05sd1_COUNTnormd),
    NormOLTotal_COUNTnormd = ol(OLTotal_COUNTnormd, Ch2Total_COUNTnormd, Ch4Total_COUNTnormd)
  )

refactored.col = names(knew)[c(1:38, 86:89, 39:65, 90:93, 66:85, 94:97)]
# write.csv(refactored.col, "OUTPUT/refactored_columns_for_knew.csv")
knew = knew[, refactored.col]

# define the names
names.not.measurements = refactored.col[c(1:18, 43:49)]
names.measurements = refactored.col[c(19:42, 50:97)]
