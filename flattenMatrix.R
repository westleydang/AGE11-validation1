flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

compare_sigs = function(mat1, mat2) {
  flat1 = flattenCorrMatrix(mat1$r, mat1$P) %>% mutate(comp = paste(row, column))
  flat2 = flattenCorrMatrix(mat2$r, mat2$P) %>% mutate(comp = paste(row, column))
  flat.both = merge(flat1, flat2 %>% select(p, comp), by="comp")
  flat.both = flat.both %>% mutate(p.delta = case_when(
    p.x < 0.05 & p.y < 0.05 ~ "s-s",
    p.x < 0.05 & p.y > 0.05 ~ "s-ns",
    p.x > 0.05 & p.y < 0.05 ~ "ns-s",
    p.x > 0.05 & p.y > 0.05 ~ "ns-ns")
  )
  results = summary(as.factor(flat.both$p.delta))
  results
  
  barplot(results)
}
compare_sigs(av1, av2)
compare_sigs(arc.age1, arc.age2)
compare_sigs(arcodd, arceven)

a0 = a


a = merge(a, animal_details, by="ID")
rownames(a) = a$GROUP
heatmap(as.matrix(a[,2:16]), labRow = paste(a$FREEZE))
heatmap(as.matrix(a[,2:16]))
