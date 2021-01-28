library(ggplot2)
library(dplyr)

#load in csv
cshours <- read.csv("C:/R/CS_Hours_Tracking-LessonsLearnedPaper.csv", header = TRUE)

#create bar chart of volunteer hours by type
vol_hours_graph <- ggplot(cshours) + 
  geom_col(aes(x = reorder(Type, -Hours, FUN = sum), y = Hours, fill=Type)) + 
  scale_fill_manual(values = c("Arbimon" = "#8c510a", "Data Upload" = "#d8b365", 
                               "Volunteer Deploy" = "#01665e", "GIS/Map-making" = "#c7eae5", 
                               "Mail Deploy" = "#5ab4ac", "Misc Project Work" = "#f6e8c3")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=13),axis.title.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks=seq(0,3000,500), 
                     labels = c("0","", "1000", "", "2000", "", "3000")) +   
  xlab("") + ylab("Number of Hours") + 
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()


#save volunteer hours bar chart
png(filename="C:/R/temp", 
    units="in", width=6.5, height=3, res=600)
print(vol_hours_graph)
dev.off()