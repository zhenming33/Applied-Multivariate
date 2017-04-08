datimp <- read.csv("cdat.csv",header = T)
datimp2 <- read.csv("driveMICE.csv", header = T)

library(psych)
fa.parallel(datimp[,5:32], fa = "fa", n.iter = 100, show.legend = FALSE)
efa <- fa(datimp[,5:32], nfactors = 3, rotate = "varimax", scores = T, fm = "pa")
factor.plot(efa, labels = rownames(efa$loadings))
fa.diagram(efa)

library(qgraph)
qgraph(cor(datimp[,5:32]))
qgraph.pca(cor(datimp[,5:32]), rotation = "varimax", factors=3, factorCors=T)

dist.e <- dist(efa$scores, method = 'euclidean')
# Hierarchical clustering
## Ward??s minimum variance criterion minimizes the total within-cluster variance
model1 <- hclust(dist.e, method = 'ward.D')
result <- cutree(model1, k=3)
plot(model1)
rect.hclust(model1, k=3, border = "red")

model2 <- kmeans(efa$scores, centers = 3, nstart = 100)


voles.mds <- cmdscale(dist.e, k = 13, eig = T)
sum(abs(voles.mds$eig[1:2])) / sum(abs(voles.mds$eig))
sum((voles.mds$eig[1:2]) ^ 2) / sum((voles.mds$eig) ^ 2)

library(cluster)
clusplot(efa$scores, result, color = TRUE, shade = TRUE, labels = 2, lines = 0)
clusplot(efa$scores, model2$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

library(rgl)
plot3d(efa$scores[,1:3], col=model2$cluster, main="k-means clusters")
plot3d(efa$scores[,1:3], col=result, main="Hierarchical clusters")
# try to plot gender and cluster at same time

plot3d(efa$scores[datimp$GENDER==1,1:3], col=colours[datimp$GENDER==1],
       main="Hierarchical clusters", type = "p")
plot3d(efa$scores[datimp$GENDER==0,1:3], col=colours[datimp$GENDER==0], 
       type = "s", add = TRUE)
colours <- result
colours[result==1] <- "green"
colours[result==2] <- "blue"
colours[result==3] <- "red"


a <- with(model2, table(cluster,datimp$GENDER))
a /750
a/(c(545,545,545,211,211,211)) # get percentages of each gender
with(model2, table(cluster,datimp$DRIVERLICENSE1))
with(model2, table(cluster,datimp$AGE_LIST))
