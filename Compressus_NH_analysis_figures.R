#F. compressus Natural History & Ecology: Analysis + Figures
#Starting Analysis 10.26.22

#Load in packages
library(scales)
library(car)
library(AICcmodavg)
library(lme4)
library(lmerTest)
library(nlme)
library(bbmle)
library(MuMIn)
library(ggpubr)
library(performance)
library(see)
library(patchwork)
library(tidyverse)

#Read in data
data <- read.table("burrow_data.csv", fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
attach(data)
detach(data)

#check data
summary(data)
str(data)
data

###                                ###
#Burrow Density Figures + Analysis####
###                                ###

#change site.number to a facotr for use as a random effect
data$site.number <- as.factor(data$site.number)
str(data)

#Burrow Number Figure
#tiff('Burrow_Number_Habitat.tiff', units="in", width=10, height=8, res=300, compression = 'lzw')

p<-ggplot(data, aes(x=habitat.type, y=burrow.number, fill = habitat.type))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(color= alpha("black",0.75), aes(fill=habitat.type, group=site.number), size=3, position=position_dodge(0.14)) +
  theme_classic() +
  labs(y = expression ("Burrow Density per 0.25 m"^2), x = expression ("Macrohabitat Type")) +
  theme(legend.position="none")+
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

p + scale_fill_manual(breaks = data1$habitat.type,
                      values = c("tan1", "tomato3","darkgray"))

#dev.off()

#Burrow Number and Habitat Analysis
Cand.models <- list( )

Cand.models[[1]] <- lmer(data$burrow.number ~ data$habitat.type + (1|data$site.number) ,  na.action="na.omit", REML = FALSE)
Cand.models[[2]] <- lmer(data$burrow.number ~ data$burrow.depth + (1|data$site.number), na.action="na.omit", REML = FALSE)
Cand.models[[3]] <- lmer(data$burrow.number ~ data$burrow.depth + data$habitat.type + (1|data$site.number), na.action="na.omit", REML = FALSE)
Cand.models[[4]] <- lmer(data$burrow.number ~ data$burrow.depth*data$habitat.type + (1|data$site.number), na.action="na.omit", REML = FALSE)
Cand.models[[5]] <- lmer(data$burrow.number ~ 1 + (1|data$site.number), na.action="na.omit", REML = FALSE)

Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

aictab(cand.set = Cand.models, modnames = Modnames, second.ord = TRUE, nobs = NULL, sort = TRUE) 

summary(Cand.models[[1]])


###                      ###
#Burrow Number and Depth####
###                      ###
m1 <- lmer(data$burrow.number ~ data$burrow.depth + (1|data$site.number) ,  na.action="na.omit", REML = FALSE)
summary(m1)
confint(m1)

summary.aov(m1) 

par(mfrow=c(1,1))
par(mar=c(5,6,4,1)+.1)
shapes = c(19)
colors <- c(alpha('black',0.55))
y1 <- expression(Burrow~Density~~per~0.25~m^2)#y-axis title

tiff('Burrow_Number_Depth.tiff', units="in", width=10, height=8, res=300, compression = 'lzw')

plot(data$burrow.depth,data$burrow.number, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1,
     xlab="Estimated Water Depth (cm)", ylab=y1)

curve(((6.24967) + (0.14514*x)), col=alpha('tomato3', 0.75),lwd=3,lty=2, add=T)

dev.off()

###                                                                ###
#F. compressus Collected  Per Habitat + Body Size per Habitat    ####
###                                                                 ###
#Read in data
data1 <- read.table("body_size_data_per_habitat.csv", fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
attach(data1)
detach(data1)

#check data
summary(data1)
str(data1)
data1

###
#F. compressus by habitat
###
#tiff('Compressus_by_habitat.tiff', units="in", width=10, height=8, res=300, compression = 'lzw')

x<-ggplot(data1, aes(x=habitat, y=compressus.collected, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values = c("tan1", "tomato3"))+
  geom_point(position=position_jitterdodge(0.14), size = 3, color= alpha("black",0.75))+
  theme_classic() +
  labs(y = expression(paste("Number of ", italic(" F. compressus"), " collected")), x = expression ("Macrohabitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

x + guides(fill=guide_legend(title="Substrate depth"))

#dev.off()

#F. compressus collected and Habitat Analysis
Cand.models <- list( )

Cand.models[[1]] <- lmer(data1$compressus.collected ~ data1$habitat*data1$location + (1|data1$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[2]] <- lmer(data1$compressus.collected ~ data1$habitat + data1$location + (1|data1$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[3]] <- lmer(data1$compressus.collected ~ data1$location + (1|data1$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[4]] <- lmer(data1$compressus.collected ~ data1$habitat + (1|data1$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[5]] <- lmer(data1$compressus.collected ~ 1 + (1|data1$site), na.action="na.omit", REML = FALSE)

Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

aictab(cand.set = Cand.models, modnames = Modnames, second.ord = TRUE, nobs = NULL, sort = TRUE) 
#Model selection based on AICc:
#  
#  K    AICc Delta_AICc AICcWt Cum.Wt      LL
#mod 2 6 1109.90       0.00   0.86   0.86 -548.73
#mod 1 8 1113.90       4.00   0.12   0.98 -548.57
#mod 3 4 1117.05       7.15   0.02   1.00 -554.42
#mod 4 5 1131.34      21.44   0.00   1.00 -560.51
#mod 5 3 1137.06      27.16   0.00   1.00 -565.47

summary(Cand.models[[2]])
confint(Cand.models[[2]])
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)           4.6919     0.6682  82.6327   7.022 5.64e-10 ***
#  data1$habitatriffle  -2.0000     0.5924 165.0000  -3.376 0.000916 ***
#  data1$habitatrun     -1.3182     0.5924 165.0000  -2.225 0.027421 *  
#  data1$locationtop     2.4343     0.4837 165.0000   5.033 1.25e-06 ***
#  ---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


###
#F. compressus Average Body Size
###

#tiff('Compressus_CL_by_habitat.tiff', units="in", width=10, height=8, res=300, compression = 'lzw')

o<-ggplot(data1, aes(x=habitat, y=avg.compressus.size, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values = c("tan1", "tomato3"))+
  geom_point(position=position_jitterdodge(0.14), size = 3, color= alpha("black",0.75))+
  theme_classic() +
  labs(y = expression(paste("Mean Carapace Length (mm) of ", italic(" F. compressus"))), x = expression ("Macrohabitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

o + guides(fill=guide_legend(title="Location"))

#dev.off()

#F. compressus body size and Habitat Analysis
#Loading in new data because I need to remove NA's and they woudlnt remove normally
data2 <- read.table("body_size_data_per_habitat_naremoved.csv", fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")


Cand.models <- list( )

Cand.models[[1]] <- lmer(data2$avg.compressus.size ~ data2$habitat*data2$location + (1|data2$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[2]] <- lmer(data2$avg.compressus.size ~ data2$habitat + data2$location + (1|data2$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[3]] <- lmer(data2$avg.compressus.size ~ data2$location + (1|data2$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[4]] <- lmer(data2$avg.compressus.size ~ data2$habitat + (1|data2$site) ,  na.action="na.omit", REML = FALSE)
Cand.models[[5]] <- lmer(data2$avg.compressus.size ~ 1 + (1|data2$site), na.action="na.omit", REML = FALSE)

Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

aictab(cand.set = Cand.models, modnames = Modnames, second.ord = TRUE, nobs = NULL, sort = TRUE) 

#Model selection based on AICc:
#  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#mod 2 6 798.96       0.00   0.48   0.48 -393.23
#mod 4 5 800.57       1.61   0.22   0.70 -395.11
#mod 1 8 801.20       2.24   0.16   0.86 -392.16
#mod 3 4 802.12       3.16   0.10   0.96 -396.94
#mod 5 3 803.83       4.87   0.04   1.00 -398.84

summary(Cand.models[[2]])
confint(Cand.models[[2]])
#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)           9.69580    0.37579 131.45788  25.801   <2e-16 ***
#  data2$habitatriffle   0.96919    0.41398 140.70982   2.341   0.0206 *  
#  data2$habitatrun     -0.04951    0.41048 141.01709  -0.121   0.9042    
#data2$locationtop    -0.66353    0.33873 143.08936  -1.959   0.0521 .


###                                                                           ###
#Other Species Per Habitat + Body Size per Habitat (not reproted in paper)    ####
###                                                                           ###

###
#F. durelli by habitat
###
z<-ggplot(data1, aes(x=habitat, y=durelli.collected, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Number of F. durelli Collected"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

z + scale_fill_manual(breaks = data1$location,
                      values = c("green", "orange","green","orange","green","orange"))

###
#F. placidus by habitat
###
c<-ggplot(data1, aes(x=habitat, y=placidus.collected, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Number of F. placidus Collected"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

c + scale_fill_manual(breaks = data1$location,
                      values = c("purple", "yellow","purple","yellow","purple","yellow"))


###
#C. polypilosus by habitat
###
v<-ggplot(data1, aes(x=habitat, y=polypilosus.collected, fill = location))+
  geom_violin()+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Number of C. polypilosus Collected"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

v + scale_fill_manual(breaks = data1$location,
                      values = c("pink", "red","pink","red","pink","red"))

###
#F. durelli Average Body Size
###
m<-ggplot(data1, aes(x=habitat, y=avg.durreli.size, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Average Body Size of F. durelli"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

m + scale_fill_manual(breaks = data1$location,
                      values = c("green", "orange","green","orange","green","orange"))


###
#F. placidus Average Body Size
###
n<-ggplot(data1, aes(x=habitat, y=avg.placidus.size, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Average Body Size of F. placidus"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

n + scale_fill_manual(breaks = data1$location,
                      values = c("purple", "yellow","purple","yellow","purple","yellow"))


###
#C. polypilosus Average Body Size
###
c<-ggplot(data1, aes(x=habitat, y=avg.polypilosus.size, fill = location))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitterdodge(0.14))+
  theme_classic() +
  labs(y = expression ("Average Body Size of C. polypilosus"), x = expression ("Habitat Type")) +
  theme(text = element_text(size = 15)) +
  theme(axis.title.x = element_text(vjust = - 0.5))

c + scale_fill_manual(breaks = data1$location,
                      values = c("pink", "red","pink","red","pink","red"))
