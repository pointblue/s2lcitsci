## author: Rose L Snyder
## This code reads a csv with citizen scientist recruitment data
## Generates bar graph summarizing volunteers by recruitment type using ggplot
## Generates bar graph summarizing number of bird call validations by recruitment type using ggplot
## Arranges bar graphs side-by-side using ggarrange

library(ggplot2)
library(ggpubr)

#load in csv
recruit_data <- read.csv("C:/R/recruitment_data-arbimon_work.csv", header = TRUE)


#bar graph of CS count by recruitment type

CS_recruit <- ggplot(recruit_data, aes(x=Count_of_Name, y=reorder(Recruitment, -Count_of_Name, FUN = sum), 
                                    fill = Recruitment)) +
  geom_col()+
  scale_fill_manual(values = c("Unknown" = "#8c510a", "Social Media" = "#d8b365", 
                               "Student" = "#01665e", "VolunteerMatch" = "#c7eae5", 
                               "Word-of-mouth" = "#5ab4ac", "SciStarter" = "#f6e8c3")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15),axis.title.y=element_text(size=14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +   
  xlab("Number of Citizen Scientists") + ylab("") +
  labs(title="a)") +
  theme(plot.title = element_text(size=16, hjust=(0)))

#bar graph of validations by recruitment type

valid_recruit <- ggplot(recruit_data, aes(x=vals_total, y=reorder(Recruitment, -vals_total, FUN = sum), 
                                          fill = Recruitment)) +
  geom_col()+
  scale_fill_manual(values = c("Unknown" = "#8c510a", "Social Media" = "#d8b365", 
                               "Student" = "#01665e", "VolunteerMatch" = "#c7eae5", 
                               "Word-of-mouth" = "#5ab4ac", "SciStarter" = "#f6e8c3")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=15)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, .1)), 
                     labels = c("0","100,000", "", "300,000", "", "500,000", "")) +   
  xlab("Validations Completed") + ylab("") +
  labs(title="b)") +
  theme(plot.title = element_text(size=16, hjust=(0)))

#combine bar graphs into one graphic using ggpubr
recruit_graphic <- ggarrange(CS_recruit, valid_recruit, widths = c(1.35,1))

#save graphic to file

png(filename="C:/R/temp/filename", 
    units="in", width=9, height=3.5, res=600)
print(recruit_graphic)
dev.off()

