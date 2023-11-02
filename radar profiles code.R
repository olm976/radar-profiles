#make sure to have the parameters listed in alphabetical or numerical order in your data table
#check the range of your data: how low goes the smallest values, how high is the largest?

#load the packages
library(scales)
library(tidyverse)

#import your data 
radardata <- read.table("clipboard", h=T)

#obtain straight polygon lines
coord_radar <- function (theta = "x", start = - pi / 2, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto(
    "CordRadar", CoordPolar, theta = theta, r = r, start = start,
    direction = sign(direction),
    is_linear = function(coord) TRUE)
}

#plot the radar in which stimulationX lists the average value of each parameter during stimulation, and upper and lower list mean + or -SEM, respectively
radarplot <- ggplot(radardata, aes(x=parameter, y=stimulation, group=1))+
    geom_polygon(aes(y=upper), fill="darkgrey", alpha=0.5)+
    geom_polygon(aes(y=lower), fill="white", alpha=0.7)+
    geom_polygon(fill=NA,colour="darkgrey", linewidth=1)+
    geom_polygon(aes(y=baseline), fill=NA, colour="black", linewidth=1)+
    theme_light()+
    theme(panel.grid.minor=element_blank())+
    coord_radar()+
    ylim(-0.25,0.2)+
    xlab("Facial  profile response to stimulation X")+
    ylab("Proportional change from baseline")+
    theme(axis.title.y = element_text(size = 15))

radarplot

#you can see in the ggplot that you plot the data from your dable "dataFN", 
#with column "parameter" in abscissae(or the equivalent in a radar plot)
#with column "formalin" as your dependant variables value
#"upper" and "lower" columns make for the shaded areas
#"colour" in geom_polygon defines the color of the main line (maybe match with those you will use in your regular plots for SN, FN, SH and FH? but not necessary)
#"ylim" gives the range of the ordinate, try to use the same range for each of your plots to make them easily visually comparable