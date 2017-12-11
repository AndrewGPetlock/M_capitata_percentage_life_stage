rm(list=ls()) #removes all prior information in global environment

library(ggplot2) #loads library


setwd("~/Desktop/Putnam_Lab/") #set working directory

CP <- read.csv("M_capitata_larvae_percentages.csv", stringsAsFactors = TRUE) #read in data
CP

#calculate proportion of embryos at each stage
CP$ST1.per <- (CP$Sum.life.stage.1/CP$Embryo.Sum)*100 
CP$ST2.per <- (CP$Sum.life.stage.2/CP$Embryo.Sum)*100
CP$ST4.per <- (CP$Sum.life.stage.4/CP$Embryo.Sum)*100
CP$ST8.per <- (CP$Sum.life.stage.8/CP$Embryo.Sum)*100
CP$ST8p.per <- (CP$Sum.life.stage.8./CP$Embryo.Sum)*100



stg.avgs <- aggregate(ST1.per ~ Offspring.treatment, data=CP, FUN=mean)
ST2.per <- aggregate(ST2.per ~ Offspring.treatment, data=CP, FUN=mean)
ST4.per <- aggregate(ST4.per ~ Offspring.treatment, data=CP, FUN=mean)
ST8.per <- aggregate(ST8.per ~ Offspring.treatment, data=CP, FUN=mean)
ST8p.per <- aggregate(ST8p.per ~ Offspring.treatment, data=CP, FUN=mean)
stg.avgs <- cbind(stg.avgs,ST2.per[,2],ST4.per[,2], ST8.per[,2], ST8p.per[,2])
colnames(stg.avgs) <- c("Treatment", "ST1", 'ST2', "ST4", "ST8", "ST8+")

stg.avgs

props <- melt(stg.avgs)

props #graphing average proportions of coral life stages

colnames(props) <- c("Treatment", "Stage", "Proportion")

ggplot(props, aes(x = Stage, y = Proportion, fill = Treatment)) +
  geom_bar(stat = "identity", aes(fill = Treatment), position = "dodge") +
  theme_bw()+
  theme(plot.background = element_blank())+ ggtitle("Averages of M.capitata life stages")+theme(plot.title = element_text(hjust = 0.5))



