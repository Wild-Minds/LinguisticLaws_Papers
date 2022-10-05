
install.packages('ggpol')
library(ggplot2)
library(ggpol)


#Boxplot with jitter points 
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
  theme_classic() + theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=1))+
  labs(x=my_x_title, y=my_y_title)
dev.off()


#Menzerath's graph
library(dplyr)
GLMMData$SequenceSize<-as.factor(GLMMData$SequenceSize)
GLMMData<-GLMMData%>%
  mutate(Duane = case_when(
    Signaller=="Duane" ~ "Duane",
    Signaller !="Duane" ~ "Other") )

my_y_title <- expression(paste("Duration (s)"))
my_x_title<- expression(paste("Sequence Size"))

Duane$SequenceSize<-as.factor(Duane$SequenceSize)

tiff("Fig_1", units='in', width=5, height=4, res=300)
ggplot(Duane, aes(x=SequenceSize, y=Duration))+ theme_classic()+
  geom_boxjitter(jitter.shape = 21, jitter.fill=NULL,
                 outlier.shape= NA, 
                 errorbar.draw = T,
                 errorbar.length = 0.2, show.legend = T) +  
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14), 
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))  +
  labs(x=my_x_title, y=my_y_title)

#Menzerath's graph Duane
ggplot(Duane, aes(x=SequenceSize, y=Duration, group=SequenceSize,))+ theme_classic()+
  geom_boxjitter(jitter.shape = 21, jitter.fill = NULL,
                 outlier.colour = NULL, outlier.shape = 1,
                 errorbar.draw = T,
                 errorbar.length = 0.2, show.legend = F)  + scale_color_manual(aes(color=Duane))
  theme_classic()


