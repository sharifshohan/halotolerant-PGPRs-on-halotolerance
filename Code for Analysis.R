###Figure 1
dat<- read.csv("/Users/mohammadumersharifshohan/Desktop/PBT_LAb/dola/Analysis/Data_figure_1.csv", header = TRUE)

head(dat)
install.packages("ggplot2",dependencies = TRUE)
install.packages('Rmisc', dependencies = TRUE)
install.packages("ggpubr", dependencies = T)
install.packages("gridExtra", dependencies = T)

# you need to run these commands. Called library calling. 
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(Rmisc)
library(dplyr)
dat2<- dat%>% filter(dat$Condition == "40 mM Salt") 
#View(dat2)
anova <- aov(Total_Chlorophyll~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat$Condition <- factor(dat$Condition, levels = c ("Control","40 mM Salt","80 mM Salt"))
str(dat)
dat$Bacteria <- factor(dat$Bacteria, levels = c("No Bacteria","A","B","C","D"))


dat3 <- summarySE(dat,measurevar="Total_Chlorophyll", groupvars= c("Bacteria","Condition"))


View(dat3)
ggplot(dat3, aes(x = dat3$Bacteria, y = dat3$Total_Chlorophyll, fill = Bacteria))+
      facet_grid(~dat3$Condition)+
      geom_errorbar(aes(ymin=Total_Chlorophyll-se,
                        ymax=Total_Chlorophyll+se),
                    width= 0.2, size=0.6,position=position_dodge())+
      geom_bar(position=position_dodge(0.9), stat="identity", width = 0.5)+
      theme(axis.text.x = element_text(face = "bold", size = 8, angle = 0, vjust = 0.5)) +
      theme(axis.text.y = element_text(face = "bold"))+
      theme(strip.text.x = element_text(face = "bold", size = NULL, colour = NULL, angle = 0))+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle("Analysis of Total Chlorophyll Content") +
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Isolates") + ylab("Total Chlorophyll")+
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00))+
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90))+
      labs(fill='Isolates') 

ggsave("Figure1.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')

### Figure 3

dat<- read.csv("Data_figure_3.csv", header = TRUE)
str(dat)
dat2<- dat%>% filter(dat$Condition == "80 mM Salt") 
#View(dat2)
anova <- aov(Yield.gm.plant.~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat$Condition <- factor(dat$Condition, levels = c ("Control","40 mM Salt","80 mM Salt"))
dat$Bacteria <- factor(dat$Bacteria, levels = c("No Bacteria","A","B","C","D"))

dat2<- dat%>% filter(dat$Condition == "40 mM Salt") 
#View(dat2)
anova <- aov(Yield.gm.plant.~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat3 <- summarySE(dat,measurevar="Yield.gm.plant.", groupvars= c("Bacteria","Condition"))

ggplot(dat3, aes(x = dat3$Bacteria, y = dat3$Yield.gm.plant., fill = Bacteria))+
      facet_grid(~dat3$Condition)+
      geom_errorbar(aes(ymin=Yield.gm.plant.-se,
                        ymax=Yield.gm.plant.+se),
                    width= 0.2, size=0.6,position=position_dodge())+
      geom_bar(position=position_dodge(0.9), stat="identity", width = 0.5)+
      theme(axis.text.x = element_text(face = "bold", size = 8, angle = 0, vjust = 0.5)) +
      theme(axis.text.y = element_text(face = "bold"))+
      theme(strip.text.x = element_text(face = "bold", size = NULL, colour = NULL, angle = 0))+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle("Analysis of Yield") +
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Isolates") + ylab("yield (gm/plant)")+
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00))+
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90))+
      labs(fill='Isolates') 

ggsave("Figure3.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')

##Figure6

dat<- read.csv("Data_figure_6.csv", header = TRUE)
tail(dat)


dat2<- dat%>% filter(dat$Condition == "Control") 
#View(dat2)
anova <- aov(Total_Chlorophyll~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat$Condition <- factor(dat$Condition, levels = c ("Control","40 mM Salt","80 mM Salt"))
str(dat)
dat$Bacteria <- factor(dat$Bacteria, levels = c("No Bacteria","I","J"))


dat3 <- summarySE(dat,measurevar="Total_Chlorophyll", groupvars= c("Bacteria","Condition"))


View(dat3)
ggplot(dat3, aes(x = dat3$Bacteria, y = dat3$Total_Chlorophyll, fill = Bacteria))+
      facet_grid(~dat3$Condition)+
      geom_errorbar(aes(ymin=Total_Chlorophyll-se,
                        ymax=Total_Chlorophyll+se),
                    width= 0.2, size=0.6,position=position_dodge())+
      geom_bar(position=position_dodge(0.9), stat="identity", width = 0.5)+
      theme(axis.text.x = element_text(face = "bold", size = 8, angle = 0, vjust = 0.5)) +
      theme(axis.text.y = element_text(face = "bold"))+
      theme(strip.text.x = element_text(face = "bold", size = NULL, colour = NULL, angle = 0))+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle("Analysis of Total Chlorophyll Content") +
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Isolates") + ylab("Total Chlorophyll")+
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00))+
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90))+
      labs(fill='Isolates') 
ggsave("Figure6.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')


##Figure 7
dat<- read.csv("Data_figure_7.csv", header = TRUE)
str(dat)
dat2<- dat%>% filter(dat$Condition == "80 mM Salt") 
#View(dat2)
anova <- aov(Yield~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat$Condition <- factor(dat$Condition, levels = c ("Control","40 mM Salt","80 mM Salt"))
dat$Bacteria <- factor(dat$Bacteria, levels = c("No Bacteria","I","J"))

dat2<- dat%>% filter(dat$Condition == "Control") 
#View(dat2)
anova <- aov(Yield~ Bacteria, data = dat2)
#summary(anova)
#dat2$Bacteria
TukeyHSD(anova)
dat3 <- summarySE(dat,measurevar="Yield", groupvars= c("Bacteria","Condition"))

ggplot(dat3, aes(x = dat3$Bacteria, y = dat3$Yield, fill = Bacteria))+
      facet_grid(~dat3$Condition)+
      geom_errorbar(aes(ymin=Yield-se,
                        ymax=Yield+se),
                    width= 0.2, size=0.6,position=position_dodge())+
      geom_bar(position=position_dodge(0.9), stat="identity", width = 0.5)+
      theme(axis.text.x = element_text(face = "bold", size = 8, angle = 0, vjust = 0.5)) +
      theme(axis.text.y = element_text(face = "bold"))+
      theme(strip.text.x = element_text(face = "bold", size = NULL, colour = NULL, angle = 0))+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle("Analysis of Yield") +
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("Isolates") + ylab("yield (gm/plant)")+
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00))+
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90))+
      labs(fill='Isolates') 

ggsave("Figure7.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')



