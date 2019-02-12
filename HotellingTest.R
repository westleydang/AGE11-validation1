

data.manova = arc %>% dcast(ID + EXPT + GROUP + VALENCE + CONTEXT ~ System, value.var = "weighted")
data.manova = complete(mice(data.manova, m=5, seed=25))
response = as.matrix((data.manova[,6:18]))
g = as.vector(data.manova[,"GROUP"])
e = as.vector(data.manova[,"EXPT"])
v = as.vector(data.manova[,"VALENCE"])
c = as.vector(data.manova[,"CONTEXT"])
m = manova(response ~ e * v * c)
summary(m, test="Hotelling")
