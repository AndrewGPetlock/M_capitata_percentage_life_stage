rm(list=ls()) #removes all prior information in global environment
library(ggplot2) #loads library
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

setwd("~/Desktop/Putnam_Lab/") #set working directory

CP <- read.csv("M_capitata_larvae_percentages.csv", stringsAsFactors = TRUE) #read in data
CP

#calculate proportion of embryos at each stage
CP$ST1.per <- (CP$Sum.life.stage.1/CP$Embryo.Sum)*100 
CP$ST2.per <- (CP$Sum.life.stage.2/CP$Embryo.Sum)*100
CP$ST4.per <- (CP$Sum.life.stage.4/CP$Embryo.Sum)*100
CP$ST8.per <- (CP$Sum.life.stage.8/CP$Embryo.Sum)*100
CP$ST8p.per <- (CP$Sum.life.stage.8./CP$Embryo.Sum)*100


STx_y_avg <- CP[1:6,c(4,18:22)]  #grabbing rows 1-6 of CP, taking columns 4 (treatment) and 18-22 (lifestages 1 - 8p) of CP 

stg.avgs_x_y <- aggregate(ST8p.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean) #agregating and averaging proportions
ST8.per <- aggregate(ST8.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST4.per <- aggregate(ST4.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST2.per <- aggregate(ST2.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST1.per <- aggregate(ST1.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
stg.avgs_x_y <- cbind(stg.avgs_x_y,ST8.per[,2],ST4.per[,2], ST2.per[,2], ST1.per[,2])
colnames(stg.avgs_x_y) <- c("Treatment", "ST8+", 'ST8', "ST4", "ST2", "ST1") #building the bars that are plotted


props_x_y <- melt(stg.avgs_x_y)  #melt converts from a horizontal represention to a vertical representation / builds the bars that are plotted
colnames(props_x_y) <- c("Treatment", "Stage", "Proportion")

p5 <- ggplot() + geom_bar(aes(y = Proportion, x = Treatment, fill = Stage), data = props_x_y, #plot without data labels
                          stat="identity")

stg.avgs_x_y <- aggregate(ST1.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean) #building the proportions for plotting
ST2.per <- aggregate(ST2.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST4.per <- aggregate(ST4.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST8.per <- aggregate(ST8.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
ST8p.per <- aggregate(ST8p.per ~ Offspring.treatment, data=STx_y_avg, FUN=mean)
stg.avgs_x_y <- cbind(stg.avgs_x_y,ST2.per[,2],ST4.per[,2], ST8.per[,2], ST8p.per[,2])
colnames(stg.avgs_x_y) <- c("Treatment", "ST1", 'ST2', "ST4", "ST8", "ST8+") 

props_x_y <- melt(stg.avgs_x_y)  #melt converts from a horizontal represention to a vertical representation
colnames(props_x_y) <- c("Treatment", "Stage", "Proportion")


props_x_y <- ddply(props_x_y, .(Treatment),
                   transform, pos = cumsum(Proportion) - (0.5 * Proportion)) #positioning the data labels

p5 <- p5 + geom_text(data=props_x_y, aes(x = Treatment, y = pos, label = paste0(Proportion,"%")), #plotting the labels
                     size=4)

p5 <- p5 + ggtitle("M.Capitata embryo life stages in 111*85 cross (%)") #adding tittle

p5
