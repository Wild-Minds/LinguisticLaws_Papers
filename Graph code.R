#Figure 1
library(ggplot2)
library(ggpol)
GLMMData<-read.csv(url("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMData.csv"))
               
#Boxplot with jitter points - Figure 1 ----
library(forcats)
my_y_title <- expression(paste("Duration (s)"))
my_x_title<- expression(paste("Gesture Type"))
tiff("GesturesSummary", units='in', width=10, height=7, res=300)
ggplot(GLMMData, aes(x=fct_reorder(Gesture, Duration,, .desc=T), y=Duration, fill=Category))+ theme_classic()+
  geom_boxjitter(jitter.shape = 21, jitter.color = NULL,
                 outlier.colour = NULL, outlier.shape = 1,
                 errorbar.draw = T,
                 errorbar.length = 0.2, show.legend = F) +
  scale_fill_manual(values=c("#FFC20A", "#0C7BDC")) +
  theme_classic() + theme(axis.text.x = element_text(angle = 45,  vjust = 0.9, hjust=0.9))+
  labs(x=my_x_title, y=my_y_title)
dev.off()

#Zipf's law figure - Figure 2 ----
library(dplyr)
Aggregate2<-GLMMData %>% 
  group_by(Gesture) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    sd=sd(Duration),
    d=mean(Duration),)

my_x_title<- expression(paste("Frequency"))
my_y_title<- expression(paste("Duration (s)"))
GestureTypes<-left_join(GestureTypes,Aggregate2, by='Gesture')
library(ggplot2)
tiff(filename="ZipfPlot", res=300, height = 1400, width = 2400) # perhaps width/height as well
AllZipf<-ggplot(Aggregate2, aes(x=F, y=d)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=14))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm, se=F, color='black') +
  scale_x_continuous(breaks = c(0,50,100,150,200,250), limits = c(-2,270))+ 
  scale_y_continuous(breaks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7, 8))
dev.off()

#Same for Duane
library(dplyr)
Aggregate3<-Duane %>% 
  group_by(Gesture) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    sd=sd(Duration),
    d=mean(Duration),)

my_x_title<- expression(paste("Frequency"))
my_y_title<- expression(paste("Duration (s)"))
library(ggplot2)
tiff(filename="ZipfPlotDuane", res=300, height = 1400, width = 2400) # perhaps width/height as well
DuaneZipf<-ggplot(Aggregate3, aes(x=F, y=d)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=14))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm, se=F, color='black') +
  scale_x_continuous(breaks = c(0,50,100,150,200, 250), limits = c(-2,270))+ 
  scale_y_continuous(breaks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7, 8))
dev.off()

#arrange graphs
install.packages("ggpubr")
library(ggpubr)
tiff(filename="Figure 2", res=300, height = 2400, width = 2400) 
ggarrange(AllZipf, DuaneZipf, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
dev.off()

#Menzerath's graph _figure 3----
library(dplyr)
library(ggplot2)
GLMMData$SequenceSize<-as.factor(GLMMData$SequenceSize)
GLMMDataGraph<-GLMMData%>%
  mutate(Individual = case_when(
    Signaller=="Duane" ~ "Duane",
    Signaller !="Duane" ~ "Other") )

my_y_title <- expression(paste("Duration (s)"))
my_x_title<- expression(paste("Sequence Size"))
my_legend_title<- expression(paste("Individual"))
GLMMDataGraph$SequenceSize<-as.factor(GLMMDataGraph$SequenceSize)

tiff("Menzerath's Plot", units='in', width=7, height=5, res=300)
A<-ggplot(GLMMDataGraph, aes(x=as.numeric(SequenceSize), y=Duration))+ 
  theme_classic()+ 
  geom_boxplot(aes(x=as.factor(SequenceSize), y=Duration),position= position_nudge(x=-0.15), width=0.3, outlier.shape=NA) + 
  geom_jitter(position = position_jitter(width = 0.18),
              aes(x = as.numeric(SequenceSize) + .20, fill=Individual, shape=Individual),
              size =1.5) + scale_fill_manual(values=c("#00AFBB", "White")) +
  geom_smooth(method = "lm", se=F, color='blue')+
  scale_shape_manual(values=c(21, 21)) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position="top")  +
  labs(x=my_x_title, y=my_y_title) 
dev.off()
A


#Menzerath's graph Duane _ Figure B
Duane$SequenceSize<-as.factor(Duane$SequenceSize)
tiff("Duane's Plot", units='in', width=6, height=5, res=300)
B<-ggplot(Duane, aes(x=as.numeric(SequenceSize), y=Duration))+ theme_classic()+ 
  geom_boxplot(aes(x=as.factor(SequenceSize), y=Duration), position= position_nudge(x=-0.15), width=0.3, outlier.shape=NA) + 
  geom_jitter(position = position_jitter(width = 0.18),
              aes(x = as.numeric(SequenceSize) + .20),
              size =1.5, fill="#00AFBB", shape=21)+ 
  geom_smooth(method = "lm", se=F, color='blue') +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.title = element_text(size=14)) +
  labs(x=my_x_title, y=my_y_title)
dev.off()
B
#merge graphs on pane
install.packages("ggpubr")
library(ggpubr)
tiff(filename="Figure 3", res=300, height = 3000, width = 2400) 
ggarrange(A, B, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
dev.off()


#loose vs fixed graphs----
GLMMData <- read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMData.csv")
MenzData<- read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMDataMenz.csv")

GLMMData$F<-GLMMData$P*560
MenzData<-MenzData%>%select(Sequence_ID,PWB)
MenzDataModel<-right_join(GLMMData, MenzData, by = c("SequenceID"="Sequence_ID"))

ZipfData<- GLMMData%>%
  group_by(Gesture)%>%
  summarise(median=median(Duration),
            F=n()) 

ZipfData$Type<-c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0)

Summary<-ZipfData%>%
  group_by(Type)%>%
  summarise(sum=sum(F))

ZipfData%>%
  select(Gesture, Type)
MenzDataModel<-MenzDataModel%>%
  left_join(ZipfData, by ='Gesture')


SequenceSummary<-MenzDataModel%>%
  group_by(SequenceID)%>%
  summarise(Proportion=mean(Type))


MenzDataModel<-MenzDataModel%>%
  left_join(SequenceSummary, by ='SequenceID')
MenzDataModel<-MenzDataModel %>% mutate(ProportionCat =
                                          case_when(Proportion >= 1 ~ "All Fixed", 
                                                    Proportion >= 0.1 & Proportion <= 0.9 ~"Mixed",
                                                    Proportion <= 0 ~ "All Loose"))


FixedSeq<-MenzDataModel%>%
  subset(Proportion==1)

SummaryFixed<-FixedSeq%>%
  group_by(SequenceSize)%>%
  summarise(n=n())

#Graph for distribution of duration of gestures within sequences formed of only fixed duration gestures 
FixedSeq$SequenceSize<-as.factor(FixedSeq$SequenceSize)
ggplot(FixedSeq, aes(x=SequenceSize, y=Duration)) + geom_boxplot() +
  theme_classic() + labs(x="Sequence Size", y="Duration (s)") +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=14))

#Graph for distribution of sequence structure across sequence sizes
MenzDataModel$SequenceSize<-as.factor(MenzDataModel$SequenceSize)
ggplot(MenzDataModel, aes(x=SequenceSize, fill=ProportionCat)) + geom_bar(position=position_dodge()) +
  theme_classic() + scale_fill_hue(h = c(180, 300)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(0.9)) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
  labs(x="Sequence Size", y="Count") +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=14), 
        legend.title = element_text(size=14), legend.text = element_text(size=10)) +
  guides(fill = guide_legend(title = "Sequence gestures"))



#Menzertah's law plot for each individual----
library(ggpol)
tiff(filename="16IndPlot", res=300, height = 2080, width = 3200) # perhaps width/height as well
ggplot(MenzDataModel, aes(x=SequenceSize, y=Duration, group=SequenceSize))+
  facet_wrap(~Signaller) +
  geom_boxjitter(jitter.size=0.5, outlier.shape = NA) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
  labs(x="Sequence Size", y="Duration (s)")
dev.off()








       