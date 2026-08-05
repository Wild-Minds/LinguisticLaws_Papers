#load data
library(dplyr)
library(lme4)
library(car)
library(lmerTest)
library(sjPlot)
Zipf_MAU<-read.csv( '/Users/as493/Documents/GitHub/PhD_Thesis/Data/ZipfData.csv')

nlevels(as.factor(Zipf_MAU$Signaller))
#anonimise data
new_names <- setNames(paste0("ID_", seq_along(unique(Zipf_MAU$Signaller))), unique(Zipf_MAU$Signaller))
Zipf_MAU$SigID <- new_names[Zipf_MAU$Signaller]
new_gesture_code<- setNames(paste0("G_action_", seq_along(unique(Zipf_MAU$Gesture_record))), unique(Zipf_MAU$Gesture_record))
Zipf_MAU$Gesture_actionID<- new_gesture_code[Zipf_MAU$Gesture_record]
new_morph_code<- setNames(paste0("Morph_", seq_along(unique(Zipf_MAU$morph))), unique(Zipf_MAU$morph))
Zipf_MAU$MorphID<- new_morph_code[Zipf_MAU$morph]
new_goal_code<- setNames(paste0("Goal_", seq_along(unique(Zipf_MAU$Goal))), unique(Zipf_MAU$Goal))
Zipf_MAU$GoalID<- new_goal_code[Zipf_MAU$Goal]
Mau_data<-Zipf_MAU%>%select(-c(Signaller, Gesture_record, X.1, morph, X, Goal, Social_unit))
write.csv(Mau_data,'/Users/as493/Documents/GitHub/PhD_Thesis/Data/Mau_data_anon.csv')

Mau_morph_data<-Mau_data%>%select(SigID, log, rFq_morph, Community, GoalID, Mau_duration, MorphID)%>%na.omit()


Mau_data<-read.csv( '/Users/as493/Documents/GitHub/PhD_Thesis/Data/Mau_data_anon.csv')
Mau_morph_data<-Mau_data%>%select(SigID, log, rFq_morph, Community, GoalID, Mau_duration, MorphID)%>%na.omit()

#MAU-Morph-----
#Check for collinearity
MAU_morph.col<-lmer(log~ rFq_morph + Community + (1 | SigID) + (1|MorphID) + (1|GoalID),data= Mau_morph_data )
vif(MAU_morph.col)#Check for multicollinearity - anything below 2 is acceptable

#Check for model assumptions
Mau_morph<-lmer(log ~ rFq_morph*Community +(1 | SigID) + (1|MorphID) + (1|Goal),data= Mau_morph_data)
par(mfrow = c(2,2))
qqnorm(resid(Mau_morph)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(Mau_morph))
hist(resid(Mau_morph)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(Mau_morph), resid(Mau_morph)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

# Null model
Mau_morph_null<-lmer(log ~1+(1 | SigID) + (1|MorphID) + (1|Goal),data= Mau_morph_data)
anova(Mau_morph, Mau_morph_null) #full-null
drop1(Mau_morph, test = "Chisq" ) #dropping 
modified_tab_model <- function(...) {
  sjPlot::tab_model(...,show.p = T, show.se = T)
}
Mau_model_output<-modified_tab_model(Mau_morph) #output table

# change the reference level of the predictor to "Sonso" to check Sonso-Waibira difference
Mau_morph_data$Community <-as.factor(Mau_morph_data$Community)
Mau_morph_data$Community <- relevel(Mau_morph_data$Community, ref = "Bossou")

# fit the model again with the new reference level
Mau_morph_rev<-lmer(log ~ rFq_morph*Community +(1 | SigID) + (1|MorphID) + (1|Goal),data= Mau_morph_data)
modified_tab_model(Mau_morph_rev)

# recompute models per community to test whether the relationship is significant
Sonso.MAU<-Mau_morph_data%>%filter(Community=="Sonso")
Mau_morph_Sonso<-lmer(log ~ rFq_morph +(1 | SigID) + (1|MorphID) + (1|Goal),data= Sonso.MAU)
Mau_morph_Sonso_null<-lmer(log ~ 1 +(1 | SigID) + (1|MorphID) + (1|Goal),data= Sonso.MAU)
lrt1 <- anova(Mau_morph_Sonso, Mau_morph_Sonso_null)
modified_tab_model(Mau_morph_Sonso)

#Now repeat for Waibira
Waibira.MAU<-Mau_morph_data%>%filter(Community=="Waibira")
Mau_morph_Waibira<-lmer(log ~ rFq_morph +(1 | SigID) + (1|MorphID) + (1|Goal),data= Waibira.MAU)
Mau_morph_Waibira_null<-lmer(log ~ 1 + (1 | SigID) + (1|MorphID) + (1|Goal),data= Waibira.MAU)
lrt2 <- anova(Mau_morph_Waibira, Mau_morph_Waibira_null)
modified_tab_model(Mau_morph_Waibira)

#Now repeat for Bosspu
Bossou.MAU<-Mau_morph_data%>%filter(Community=="Bossou")
Mau_morph_Bossou<-lmer(log ~  rFq_morph +(1 | SigID) + (1|MorphID) + (1|Goal),data= Bossou.MAU)
Mau_morph_Bossou_null<-lmer(log ~ 1 +(1 | SigID) + (1|MorphID) + (1|Goal),data= Bossou.MAU)
lrt3 <- anova(Mau_morph_Bossou, Mau_morph_Bossou_null)
lrt3

#MAU-GA----
#Check for collinearity
MAU_action.col<-lmer(log~ rFq_action + Community + (1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Mau_data )
vif(MAU_action.col)#Check for multicollinearity - anything below 2 is acceptable

#Check for model assumptions
Mau_action<-lmer(log~ rFq_action*Community + (1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Mau_data )
par(mfrow = c(2,2))
qqnorm(resid(Mau_action)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(Mau_action))
hist(resid(Mau_action)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(Mau_action), resid(Mau_action)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

# Null model
Mau_action_null<-lmer(log ~1+(1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Mau_data)
anova(Mau_action, Mau_action_null) #full-null
drop1(Mau_action, test = "Chisq" ) #dropping 
modified_tab_model(Mau_action) #output table

# change the reference level of the predictor to "Sonso" to check Sonso-Waibira difference
Mau_data$Community <-as.factor(Mau_data$Community)
Mau_data$Community <- relevel(Mau_data$Community, ref = "Bossou")

# fit the model again with the new reference level
Mau_action_rev<-lmer(log~ rFq_action*Community + (1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Mau_data)
modified_tab_model(Mau_action_rev)

# recompute models per community to test whether the relationship is significant
Sonso.MAU_action<-Mau_data%>%filter(Community=="Sonso")
Mau_action_Sonso<-lmer(log ~ rFq_action + (1|SigID) + (1|Gesture_actionID) + (1|Goal),data= Sonso.MAU_action)
Mau_action_Sonso_null<-lmer(log ~ 1 + (1|SigID) + (1|Gesture_actionID) + (1|Goal),data= Sonso.MAU_action)
lrt1 <- anova(Mau_action_Sonso, Mau_action_Sonso_null)
lrt1

#Now repeat for Waibira
Waibira.MAU_action<-Mau_data%>%filter(Community=="Waibira")
Mau_action_Waibira<-lmer(log ~ rFq_action +(1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Waibira.MAU_action)
Mau_action_Waibira_null<-lmer(log ~ 1 + (1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Waibira.MAU_action)
lrt2 <- anova(Mau_action_Waibira, Mau_action_Waibira_null)
lrt2

#Now repeat for Bosspu
Bossou.MAU_action<-Mau_data%>%filter(Community=="Bossou")
Mau_action_Bossou<-lmer(log ~  rFq_action +(1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Bossou.MAU_action)
Mau_action_Bossou_null<-lmer(log ~ 1 +(1 | SigID) + (1|Gesture_actionID) + (1|Goal),data= Bossou.MAU_action)
lrt3 <- anova(Mau_action_Bossou, Mau_action_Bossou_null)
lrt3
modified_tab_model(Mau_action_Bossou)


#Mau graphs ----
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
Aggregate1<-Mau_morph_data %>% 
  group_by(MorphID, Community) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    p=mean(rFq_morph),
    sd=sd(Mau_duration),
    d=mean(Mau_duration),
    logm=mean(log),
    logsd=sd(log))

my_x_title<- expression(paste("Morph relative frequency - rFq_morph"))
my_y_title<- expression(paste("MAU duration (s)"))
library(ggplot2)
Graph_mau_morph<-ggplot(Aggregate1, aes(x=p, y=d, color=Community)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.text = element_text(size = 10))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm,aes(fill = Community) ) +
  scale_x_continuous(breaks = c(0,0.025,0.05,0.075,0.1), limits = c(0,0.08))+ 
  scale_y_continuous(breaks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7, 8))+
  scale_color_manual(values =colorBlindBlack8[c(4,2,3)] ) +  scale_fill_manual(values =colorBlindBlack8[c(4,2,3)] )
Graph_mau_morph


Aggregate2<-Mau_data %>% 
  group_by(Gesture_actionID, Community) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    t=mean(rFq_action),
    sd=sd(Mau_duration),
    d=mean(Mau_duration),
    logm=mean(log),
    logsd=sd(log))

my_x_title<- expression(paste("Gesture Action relative frequenc - rFq_action"))
my_y_title<- expression(paste("MAU duration (s)"))
Graph_mau_action<-ggplot(Aggregate2, aes(x=t, y=d, color=Community)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.text = element_text(size = 10))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm, aes(fill=Community)) +
  scale_x_continuous(breaks = c(0,0.025,0.05,0.075,0.1), limits = c(0,0.08))+ 
  scale_y_continuous(breaks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7, 8))+
  scale_color_manual(values =colorBlindBlack8[c(4,2,3)] )+
  scale_fill_manual(values =colorBlindBlack8[c(4,2,3)] )
Graph_mau_action

#Subset graphs
# Create a vector of unique levels of 'Community'
communities <- unique(Aggregate2$Community)
# Define a color map for the communities
community_colors <- colorBlindBlack8[c(4,2,3)]  # Adjust the color palette as needed
names(community_colors) <- communities  # Assign names to the colors corresponding to each community
# Initialize a list to store the graphs
graph_list <- list()

# Loop through each level of 'Community' and create a plot
for (community in communities) {
  # Filter data for the current community
  data_filtered <- Aggregate2[Aggregate2$Community == community, ]
  # Create the plot for the current community
  graph <- ggplot(data_filtered, aes(x=t, y=d)) +
    geom_point(size=2.5, color = community_colors[community]) +  # Set the color explicitly
    theme_classic() +
    labs(x=my_x_title, y=my_y_title, title = paste("Community:", community)) +  # Add title to distinguish the plots
    theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.text = element_text(size = 10)) + 
    geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3, color = community_colors[community]) +  # Set color explicitly
    geom_smooth(method=lm, fill = community_colors[community], color = community_colors[community], se = TRUE) +  # Set both color and fill
    scale_x_continuous(breaks = c(0,0.025,0.05,0.075,0.1), limits = c(0,0.08)) +  
    scale_y_continuous(breaks=c(0,1.0,2.0,3.0,4.0,5.0,6.0,7,8)) +
    scale_color_manual(values = community_colors) +  # Ensure manual colors match
    scale_fill_manual(values = community_colors)     # Ensure fill matches colors
    graph_list[[community]] <- graph
}
graph_list[["Waibira"]]  # Replace "Community1" with the actual name


#PAU-morph----
#Load data
Pau_data<-read.csv( '/Users/as493/Documents/GitHub/PhD_Thesis/Data/Pau_data_anon.csv')
Pau_morph_data<-Pau_data%>%select(SigID, log, rFq_morph, Community, GoalID, Pau_duration, MorphID)%>%na.omit()

#Test for collinearity
Pau_morph_model_vif<-lmer(log ~ rFq_morph+Community +(1 | SigID) + (1|MorphID) + (1|GoalID),data= Pau_morph_data)
vif(Pau_morph_model_vif)#Check for multicollinearity - anything below 2 is acceptable

#Check model assumptions
Pau_morph_model<-lmer(log ~rFq_morph*Community +(1 | SigID) + (1|MorphID) + (1|GoalID),data= Pau_morph_data)
par(mfrow = c(2,2))
qqnorm(resid(Pau_morph_model)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(Pau_morph_model))
hist(resid(Pau_morph_model)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(Pau_morph_model), resid(Pau_morph_model)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

# Null model
Pau_morph_model_null<-lmer(log ~1+(1 | SigID) + (1|MorphID) + (1|GoalID),data= Pau_morph_data)
anova(Pau_morph_model, Pau_morph_model_null)
drop1(Pau_morph_model, test = "Chisq" ) #No significance
Pau_morph_model<-lmer(log ~ rFq_morph+Community +(1 | SigID) + (1|MorphID) + (1|GoalID),data= Pau_morph_data)
modified_tab_model(Pau_morph_model)

# change the reference level of the predictor to "Sonso" to check Sonso-Waibira difference
Pau_morph_data$Community <-as.factor(Pau_morph_data$Community)
Pau_morph_data$Community <- relevel(Pau_morph_data$Community, ref = "Bossou")

# fit the model again with the new reference level
Pau_morph_model_rel<-lmer(log ~ rFq_morph+Community +(1 | SigID) + (1|MorphID) + (1|GoalID),data= Pau_morph_data)modified_tab_model(model3)
modified_tab_model(Pau_morph_model_rel)


#PAU-action----
#Load data
Pau_data<-read.csv( '/Users/as493/Documents/GitHub/PhD_Thesis/Data/Pau_data_anon.csv')
#Test for collinearity
Pau_action_model_vif<-lmer(log ~ rFq_action+Community +(1 | SigID) + (1|Gesture_actionID) + (1|GoalID),data= Pau_data)
vif(Pau_action_model_vif)#Check for multicollinearity - anything below 2 is acceptable

#Check model assumptions
Pau_action_model<-lmer(log ~ rFq_action*Community +(1 | SigID) + (1|Gesture_actionID) + (1|GoalID),data= Pau_data)
par(mfrow = c(2,2))
qqnorm(resid(Pau_action_model)) # The qqnorm() and qqline() functions are used to check the normality assumption of the residuals.
qqline(resid(Pau_action_model))
hist(resid(Pau_action_model)) #The hist() function is used to check the symmetry of the residuals. 
plot(fitted(Pau_action_model), resid(Pau_action_model)) #he plot() function is used to check the homoscedasticity assumption of the residuals. 

# Null model
Pau_action_model_null<-lmer(log ~1+(1 | SigID) + (1|Gesture_actionID) + (1|GoalID),data= Pau_data)
anova(Pau_action_model, Pau_action_model_null)
drop1(Pau_action_model, test = "Chisq" ) #No significance
Pau_action_model<-lmer(log ~ rFq_action+Community +(1 | SigID) + (1|Gesture_actionID) + (1|GoalID),data= Pau_data)
modified_tab_model(Pau_action_model)

# change the reference level of the predictor to "Sonso" to check Sonso-Waibira difference
Pau_data$Community <-as.factor(Pau_data$Community)
Pau_data$Community <- relevel(Pau_data$Community, ref = "Bossou")

# fit the model again with the new reference level
Pau_action_model_rel<-lmer(log ~ rFq_action+Community +(1 | SigID) + (1|Gesture_actionID) + (1|GoalID),data= Pau_data)
modified_tab_model(Pau_action_model_rel)


#Pau Graphs ----
library(dplyr)
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
Aggregate3<-Pau_morph_data %>% 
  group_by(MorphID, Community) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    p=mean(rFq_morph),
    sd=sd(Pau_duration),
    d=mean(Pau_duration),
    logm=mean(log),
    logsd=sd(log))

my_x_title<- expression(paste("Morph relative frequency - rFq_morph"))
my_y_title<- expression(paste("PAU duration (s)"))
library(ggplot2)
Graph_pau_morph<-ggplot(Aggregate3, aes(x=p, y=d, color=Community)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.text = element_text(size = 10))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm,aes(fill = Community) ) +
  scale_x_continuous(breaks = c(0,0.025,0.05,0.075,0.1), limits = c(0,0.10))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30))+coord_cartesian(xlim = c(0,0.10), ylim = c(-2, 30))+  
  scale_color_manual(values =colorBlindBlack8[c(4,2,3)] ) +  scale_fill_manual(values =colorBlindBlack8[c(4,2,3)] )
Graph_pau_morph


Aggregate4<-Pau_data %>% 
  group_by(Gesture_actionID, Community) %>% # our group
  summarise( # summarise operation by group
    F=n(),
    t=mean(rFq_action),
    sd=sd(Pau_duration),
    d=mean(Pau_duration),
    logm=mean(log),
    logsd=sd(log))

my_x_title<- expression(paste("Relative frequency of Gesture Actions - rFq_action"))
my_y_title<- expression(paste("PAU duration (s)"))
Graph_pau_action<-ggplot(Aggregate4, aes(x=t, y=d, color=Community)) +
  geom_point(size=2.5) + 
  theme_classic() +
  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.text = element_text(size = 10))  +
  geom_errorbar(aes(ymin=d-sd, ymax=d+sd), width=3)  +  
  geom_smooth(method=lm, aes(fill=Community)) +
  scale_x_continuous(breaks = c(0,0.025,0.05,0.075), limits = c(0,0.08))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30))+coord_cartesian(xlim = c(0,0.08), ylim = c(-2, 30))+
  scale_color_manual(values =colorBlindBlack8[c(4,2,3)] )+
  scale_fill_manual(values =colorBlindBlack8[c(4,2,3)] )
Graph_pau_action

#Difference in duration boxplots
my_x_title<- expression(paste("Community"))
my_y_title<- expression(paste("log(PAU duration (s))"))
boxplots<-ggplot(Pau_data, aes(x=Community, y=log, group=Community)) +
  theme_classic() +  labs(x=my_x_title, y=my_y_title) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12), legend.position = 'none')  + geom_jitter(aes(color=Community))+ geom_boxplot(alpha=0.2, outlier.shape = NA)+ 
  scale_color_manual(values =colorBlindBlack8[c(4,2,3)] )
# Add significance lines
boxplots_sig<-boxplots + geom_segment(aes(x = 1, xend = 3, y=5.5, yend=5.5),
                         color = "black", size = 0.4) + 
  geom_segment(aes(x = 1, xend = 2, y=5, yend=5),
               color = "black", size = 0.4) + 
  annotate("text", x = 2,
           y = 5.6, label = "***", size = 6, color = "black")+
  annotate("text", x = 1.5,
           y = 5.1, label = "***", size = 6, color = "black")
boxplots_sig
