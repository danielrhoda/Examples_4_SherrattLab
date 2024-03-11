
library(phytools)
data(mammal.data)
data(mammal.tree)

# x is the trait data of the tips of a phylogeny
# tree is a rooted phylogeny you want to project onto morphospace
# alpha is the % transparency you want
# this is a geomorph internal function that I've modified to plot onto a plot, rather than only inside a geomorph function 
addtree2 <- function(x, tree, col = "black", alpha = 1, lwd = 1){

anc.x <- cbind(fastAnc(tree = tree, x = x[,1]), fastAnc(tree = tree, x = x[,2]))
pts <- x[,1:2]
anc <- anc.x[,1:2]
colnames(anc) <- colnames(x)
ind <- match(tree$tip.label, rownames(pts))
pts <- pts[ind, ]
z <- rbind(pts,anc)
# coordinates of each tip and node in the tree
edges <- as.matrix(tree$edge)
# edges is two column matrix where the first row is the index of the ancestor node
  for (i in 1:NROW(edges)) {
          pts <- z[edges[i, ], ]
          points(pts, type = "l", col = addTrans(color = col,trans = alpha), lwd = lwd, lty = 1)}
}

mammal.data[,1] <- log(mammal.data[,1])

# a phylomorphospace of mass x range data
plot(mammal.data)
addtree2(x = mammal.data, tree = mammal.tree)


# a prettier version of the same plot

# first, open a plot, then add the tree, then plot the points
# this way, the phylogeny is 'behind' the points and anything else you'd want to plot
# makes plotting morphospaces much more malleable in my opinion!
plot(NA, xlim = range(mammal.data[,1]), ylim = range(mammal.data[,2]), 
     xlab = "log(body mass)", ylab = "home range")
addtree2(x = mammal.data, tree = mammal.tree, col = 'gray', alpha = 0.8, lwd = 2)
points(mammal.data, pch = 19, cex = 1.5)

