library(geomorph)
library(plotly)


data("plethodon")

pleth.gpa <- gpagen(plethodon$land)
pleth.pca <- gm.prcomp(pleth.gpa$coords)

pleth.df <- data.frame(PC1 = pleth.pca$x[,1], PC2 = pleth.pca$x[,2], 
                       site = plethodon$site, species = plethodon$species, 
                       size = log(pleth.gpa$Csize))


# interactive plot of the plethodon morphospace
# color == site 
# hovertext == plethodon species
# point size == specimen size
plot_ly(pleth.df, x=~PC1, y=~PC2, color=~site, text=~species, size=~size*10,
        type = "scatter", mode = "markers", hoverinfo = "text")


# now colored by species
# text == size
plot_ly(pleth.df, x=~PC1, y=~PC2, text=~site, color=~species, size=~size*5,
        type = "scatter", mode = "markers", hoverinfo = "text")






