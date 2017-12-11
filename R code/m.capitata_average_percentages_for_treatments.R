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


CP$fert1 <- 100 - CP$ST1.per #fertilized embryos????? 
CP$fert2 <- 100 - CP$ST2.per
CP$fert4 <- 100 - CP$ST4.per
CP$fert8 <- 100 - CP$ST8.per
CP$fert8p <- 100 - CP$ST8p.per


sum(CP[1,18:22]) #check if first row sums to 100
sum(CP[,18:22]) #check if sums to 100 x number of row

sum(CP$ST1.per, CP$fert1) #check to see that sum of fert1 and non-fert (ST1) equals 100 <- i know this line's statement is valid
sum(CP$ST2.per, CP$fert2) #check to see that sum of fert2 and non-fert (ST2) equals 100
sum(CP$ST4.per, CP$fert4) #check to see that sum of fert4 and non-fert (ST4) equals 100
sum(CP$ST8.per, CP$fert8) #check to see that sum of fert8 and non-fert (ST8) equals 100
sum(CP$ST8p.per, CP$fert8p) #check to see that sum of fert8p and non-fert (ST8p) equals 100

# boxplot(CP$ST1.per ~ CP$Offspring.treatment) # **how to make a boxplot for life stage 1 data in proportion to offstring treatment**

#### unfertilized embryo proportions graph

stg.avgs <- aggregate(ST1.per ~ Offspring.treatment, data=CP, FUN=mean)
ST2.per <- aggregate(ST2.per ~ Offspring.treatment, data=CP, FUN=mean)
ST4.per <- aggregate(ST4.per ~ Offspring.treatment, data=CP, FUN=mean)
ST8.per <- aggregate(ST8.per ~ Offspring.treatment, data=CP, FUN=mean)
ST8p.per <- aggregate(ST8p.per ~ Offspring.treatment, data=CP, FUN=mean)
stg.avgs <- cbind(stg.avgs,ST2.per[,2],ST4.per[,2], ST8.per[,2], ST8p.per[,2])
colnames(stg.avgs) <- c("Treatment", "ST1", 'ST2', "ST4", "ST8", "ST8+")

props <- melt(stg.avgs)

props #graphing average proportions of coral life stages

colnames(props) <- c("Treatment", "Stage", "Proportion")

ggplot(props, aes(x = Stage, y = Proportion, fill = Treatment)) +
geom_bar(stat = "identity", aes(fill = Treatment), position = "dodge") +
  theme_bw()+
  theme(plot.background = element_blank())+ ggtitle("Averages of M.capitata life stages")+theme(plot.title = element_text(hjust = 0.5))



#### fertilized embryo proportions graph

stg2.avgs <- aggregate(fert1 ~ Offspring.treatment, data=CP, FUN=mean)
fert2 <- aggregate(fert2 ~ Offspring.treatment, data=CP, FUN=mean)
fert4 <- aggregate(fert4 ~ Offspring.treatment, data=CP, FUN=mean)
fert8 <- aggregate(fert8 ~ Offspring.treatment, data=CP, FUN=mean)
fert8p <- aggregate(fert8p ~ Offspring.treatment, data=CP, FUN=mean)
stg2.avgs <- cbind(stg2.avgs,fert2[,2],fert4[,2], fert8[,2], fert8p[,2])
colnames(stg2.avgs) <- c("Treatment", "ST1", 'ST2', "ST4", "ST8", "ST8+")


props2 <- melt(stg2.avgs)

props2

colnames(props2) <- c("Treatment", "Stage", "Proportion")

ggplot(props2, aes(x = Stage, y = Proportion, fill = Treatment)) +
  geom_bar(stat = "identity", aes(fill = Treatment), position = "dodge") +
  theme_bw()+
  theme(plot.background = element_blank())+ ggtitle("Fertilized")+theme(plot.title = element_text(hjust = 0.5))


