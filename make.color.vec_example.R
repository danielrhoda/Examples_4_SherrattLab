# make.color.vec example

# maps a color palette to a vector of continuous variables


# takes a vector and scales and returns a color palette accordingly
make.color.vec <- function(x, n = 50, pallette = NULL, rev = F, breaks = NULL){
  if(is.null(pallette)){pallette <- colorRampPalette(GetColors(256, scheme = "sunset"))} # sets the default color palette
  if(is.null(breaks)){breaks <- n}    # if you'd like to manually define the scale
  x1 <- cut(as.numeric(x),breaks=breaks)  # converts the vector of continuous variables into intervals which define which color it should take on
if(!rev){idcolor<-pallette(n)}else{id.color<-rev(pallette(n))}
x.cols <- 1:length(x1)
for(i in 1:length(x.cols)){x.cols[[i]] <- idcolor[as.numeric(x1[i])]}
names(x.cols) <- names(x)
return(x.cols)
}
mcv <- make.color.vec


# a bunch of random normally distributed points 
XY <- matrix(rnorm(2000), nrow=1000, ncol=2)

# using color to represent the X value
plot(XY, pch = 19, xlab = "X", ylab ="Y",
     col = mcv(XY[,1]))

# using color to represent the Y value
plot(XY, pch = 19, xlab = "X", ylab ="Y",
     col = mcv(XY[,2]))



# same plot as the first, but with expanded limits on the area that's plotted, to demonstrate how the breaks work
plot(XY, pch = 19, xlab = "X", ylab ="Y", xlim = c(-6, 3), ylim = c(-3,3),
     col = mcv(XY[,1]))

# now, defining the breaks so that -6 should be the 'coolest' color, instead of the minimum X value
new.breaks <- seq(-6,3, length.out = 50)
plot(XY, pch = 19, xlab = "X", ylab ="Y", xlim = c(-6, 3), ylim = c(-3,3),
     col = mcv(XY[,1], breaks = new.breaks))




# addTrans 
  # adds transparency to color values
  # not my function, but incredibly convenient
  # stole it from: https://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r

# adds transparency to a color code
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  trans <- trans*255
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
} 

# the trans value is the % solid (the alpha level) you want the color to be

# same plot as the first, but with expanded limits on the area that's plotted, to demonstrate how the breaks work
# 25% transparency
plot(XY, pch = 19, xlab = "X", ylab ="Y", xlim = c(-3, 3), ylim = c(-3,3),
     col = mcv(XY[,1]) %>% addTrans(., 0.25))

# 50% transparency
plot(XY, pch = 19, xlab = "X", ylab ="Y", xlim = c(-3, 3), ylim = c(-3,3),
     col = mcv(XY[,1]) %>% addTrans(., 0.5))


# same plot but I'm pulling the color argument out so that it's easier to digest

cols <- addTrans(mcv(XY[,1]), 0.75)
plot(XY, pch = 19, xlab = "X", ylab ="Y", xlim = c(-3, 3), ylim = c(-3,3),
     col = cols)
