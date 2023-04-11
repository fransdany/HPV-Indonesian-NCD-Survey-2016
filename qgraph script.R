library(igraph)
data <- read.table(file="admat2.txt", header=TRUE)
newdata <- data[,-1]
adj_mat <- as.matrix(newdata)
hpv_net <- graph.adjacency(adj_mat, mode = c("undirected"), weighted = TRUE, diag = TRUE)
plot(hpv_net)

library(qgraph)
require(qgraph)
gr <- structure(list(Low_Risk = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     High_Risk = c(13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                     Mix = c(27)))
qgraph(adj_mat,directed=FALSE, groups=gr, color=c("grey","violet","wheat"),
       mode="strength", parallelEdge=TRUE, borders = FALSE, layout="spring", vsize = 7,
       labels=colnames(newdata), esize=17, edge.color="darkgreen")