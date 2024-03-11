# animations example
library(gifski)

# okay, we're going to animate:
  # 1 - a point bouncing back and forth from a starting and stopping point
  # 2 - a random walk of a lineage


# first, the bounding animation
# the starting & ending coordinates
start <- c(-4, -9)
end <- c(7,7)

# the number of frames
nstep <- 50
  

# use the 'seq' function to make a sequence of coordinates between the starting and stopping point for both the X and Y axes
# then store the sequences in a matrix 
xseq <- seq(start[1], end[1], length.out = nstep)
yseq <- seq(start[2], end[2], length.out = nstep)
coords <-  matrix(c(xseq, yseq), nrow = nstep, ncol = 2)



# with this matrix of points for each frame, using a for loop we're going to plot each row 
# and automatically export each plot to a folder 
# then stitch them together into a gif using the 'gifski' function

par(mar=c(5,6,5,2)) # define the margins of the plot
png_path <- file.path("C:/Users/drhod/Desktop/People/Sherratt_Lab/Ex1/", "frame%03d.png") # the file path of the folder you want to store each frame in. the "frame%03d.png" part ensures that each file is named something different and in order (idk exactly why this works to be honest)
png(png_path, width = 1024, height = 768) #'starts' a PNG file. 1024x768 is the standard size of a powerpoint slide  # usually, I make the width and height of the plot multiples of 1024x768, so that when I stitch it back into a gif with the size 1024x768, it looks very high-definition
par(ask = FALSE) # ask=FALSE so that R doesn't ask for a new input each time the for loop plots a plot

for(i in 1:nstep){
  plot(coords[c(i,i),], xlim = c(-10,10), ylim = c(-10,10), cex = 3, pch = 19, cex.lab = 2) 
  # base plot won't let you just plot a single point, so I'm plotting the same i-th point twice
}

dev.off() # tells R that you're done writing plots to .png files 
png_files <- sprintf(png_path, 1:nstep) # makes a vector of all of the file names that you just plotted
gif_file <- "C:/Users/drhod/Desktop/People/Sherratt_Lab/Ex1/Ex1.gif"
gifski::gifski(c(png_files,rev(png_files)), gif_file, delay = 1/nstep, width = 1024, height = 768) # 

# delay: how long, in seconds, you want each frame to last
# because I want the point to bounce back and forth, with the first argument I'm telling the function to stitch together the frames in reverse order after doing so in their normal order


####
####
####
####
####


# random walk:
nstep <- 250

# make a matrix where each row is the coordinates of a step of a random walk
# two dimensions X & Y
# starts at origin, x=0, y=0
mu <- c(0,0)
step.mat <- matrix(mu, nrow=nstep, ncol=2)
for(i in 1:nstep){
  if(i==1){step.mat[i,] <- rnorm(2)}else{       # the first iteration starts from the origin. rnorm(2) because you're adding x & y coordinates, two points
    step.mat[i,] <- step.mat[i-1,]+rnorm(2)     # all subsequent iterations start from the previous step, so we're adding random values onto the previous coordinates
  }
}

# add c(0,0) to the top of the matrix so that the animation starts from the origin
step.mat <- rbind(c(0,0), step.mat) 

#range(step.mat) 
# so that you know what you should set the limits as: xlim=c(-25,25)=ylim

par(mar=c(5,6,5,2))
png_path <- file.path("C:/Users/drhod/Desktop/People/Sherratt_Lab/Ex2/", "frame%03d.png")
png(png_path, width = 1024, height = 768) #'starts' a PNG file
par(ask = FALSE)

for(i in 1:nstep){
  plot(step.mat[c(1:i+1),], xlim = c(-25,25), ylim = c(-25,25), xlab = "axis 1", ylab = "axis 2",
       lwd = 3, type='l', cex.lab = 2)
  }

dev.off()
png_files <- sprintf(png_path, 1:nstep)
gif_file <- "C:/Users/drhod/Desktop/People/Sherratt_Lab/Ex2/Ex2.gif"
gifski::gifski(c(png_files), gif_file, delay = 3/nstep, width = 1024, height = 768)

# so, the main idea is the same for both animations
# you write some code to define the values of whatever you're plotting at EACH STEP of the animation
# and then you're plotting the frames/steps with a for loop
# the for loop is bracketed on either side by a couple of lines of code that define where you want to export each PNG file (of each step) to
# and then a line that stitches together each frame using the 'gifski' function

# possibilities are endless!!!






