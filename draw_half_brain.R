## Draw half-head R Code. This is the code for drawing a half head pic with data list on locations of channels.
# It needs to work with these files together:
# half-head-transparent.png : The background head outline
# plot.csv                  : The CSV file includes both the x,y coordinates and the data to be plotted.
# To plot a new half-head pic, add a new column to the plot.csv file, with the title of the pic as the column name. Then insert the numeric data or character data into the rows that are corresponding to their channel locations. Save the plot.csv and run the draw_half_brain.R file and you will get output in the format of "[column_name]-half-head.png". Examples of plot.csv and outputs are available in the same folder.
# These libraries are needed.
library(ggplot2)
library(dplyr)
library(png)
library(ggpubr)

# Here is the section for setting
# Here is the cutoff for bold font. By default, any value larger than cutoff will be bold. Not bold font for non-numeric data.
cutoff <- 5
output_dir <- "./figures/"
working_dir <- "./"



setwd(working_dir)
img <- readPNG('half-head-transparent.png')
no_margins <- theme(
    axis.line =         element_blank(),
    axis.text.x =       element_blank(),
    axis.text.y =       element_blank(),
    axis.ticks =        element_blank(),
    axis.title.x =      element_blank(),
    axis.title.y =      element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.border =      element_blank(),
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
#    plot.title =        element_blank(),
    plot.margin =       unit(c(0, 0, 0, 0), "lines"),
    legend.position =   "none",
)
data <- read.csv('plot.csv')
pics <- colnames(data)
pics <- pics[4:length(pics)]
for (pic in pics)
{
    if (is.numeric(data[10,pic])) {
        data[,pic] <- as.numeric(data[,pic])
        bold_data <- data[data[,pic]>cutoff,]
        rest_data <- data[data[,pic]<=cutoff,]
        anchor_data <- data[data[,pic]==0,]
        p <-  ggplot(mapping = aes_string(x = 'x', y = 'y', label = pic)) + ggtitle(pic) + geom_text(data = bold_data, size=15, fontface = 'bold') + geom_text(data = rest_data, size=15) + geom_text(data = anchor_data, aes(color='white', size = 1)) + background_image(img) + no_margins + theme(plot.title = element_text(hjust = 0.5, size = 80, face = "bold")) + xlim(1,7) + ylim(1,4)
    } else {
        p <-  ggplot(mapping = aes_string(x = 'x', y = 'y', label = pic)) + ggtitle(pic) + geom_text(data = data, size=15) + background_image(img) + no_margins + theme(plot.title = element_text(hjust = 0.5, size = 80, face = "bold")) +
            xlim(1,7) + ylim(1,4)
    }
    fileName <- paste(output_dir,pic,sep='') %>% paste('-half-head.png',sep = '')
    ggsave(fileName,
           p, height = 430, width = 600, units = 'mm')
}
