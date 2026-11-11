#Load packages ----
library(dplyr)
library(sjPlot)
library(readr)
library(brms)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(modelr)
library(rstan)
library(MuMIn)
library(RCurl)
library(bayestestR)


#Zipf's law correlation analysis----

#Correlation anaylsis between f and d
GLMMData <- read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMData.csv")
GestureTypes<-GLMMData%>%
  group_by(Gesture)%>%
  summarise(D=sum(Duration),
            d=mean(Duration),
            F=n(),
            Category=first(Category))

GestureTypes$p<-GestureTypes$F/sum(GestureTypes$F)


cor.test(GestureTypes$F, GestureTypes$d, method = 'spearman', alternative='greater') #correlation between f and d
cor.test(GestureTypes$F, GestureTypes$D, method = 'spearman') #Control correlation between D and f


#Is L significnatly small for gesture types? from Heesen et al. (2019)
reps <- 100000
results <- rep(0, reps)
x <- c(GestureTypes$p)
y <- c(GestureTypes$d)
L <- sum(x*y)
print (c("real L is", L))
sortvector <- 1:length(x)
for (i in 1:reps)
{
  sortvector <- sample(sortvector, replace = F)
  xtemp <- x[sortvector]
  L_temp <- sum(xtemp *y)
  results[i] <- L_temp
}
is_small <- sum(results <L) #changed check if it is significiantly big swap '<' with '>'
print(c("P of being so small is estimated as ", is_small/reps)) 

#Histogram
L<-results
higher <- quantile(x = results,probs = .95)
lower<-quantile(x = results,probs = .05)
jpeg('histogramZipf.jpg')
hist(L)
abline(v=c(lower, 2.39, higher), col=c("Black", "Red"), lwd=2, lty=c(2,1))
dev.off()

#Is L significnatly small in manual gestures? 
ManualTypes<-subset(GestureTypes, Category =="Manual")
reps <- 100000
results <- rep(0, reps)
x <- c(ManualTypes$p)
y <- c(ManualTypes$d)
L <- sum(x*y)
print (c("real L is", L))
sortvector <- 1:length(x)
for (i in 1:reps)
{
  sortvector <- sample(sortvector, replace = F)
  xtemp <- x[sortvector]
  L_temp <- sum(xtemp *y)
  results[i] <- L_temp
}
is_small <- sum(results <L) #changed check if it is significiantly big swap '<' with '>'
print(c("P of being so small is estimated as ", is_small/reps))

#Histogram
lower <- quantile(x = results,probs = .05)
upper <-quantile (x=results, probs = .95)
L<-results
jpeg('histogramManual.jpg')
hist(L)
abline(v=c(lower, 2.26, upper), col=c("Black", "Red", "black"), lwd=2, lty=c(2,1,2))
dev.off()

#Is L significnatly small in whole-body gestures? 
BodyTypes<-subset(GestureTypes, Category !="Manual")
reps <- 100000
results <- rep(0, reps)
x <- c(BodyTypes$p)
y <- c(BodyTypes$d)
L <- sum(x*y)
print (c("real L is", L))
sortvector <- 1:length(x)
for (i in 1:reps)
{
  sortvector <- sample(sortvector, replace = F)
  xtemp <- x[sortvector]
  L_temp <- sum(xtemp *y)
  results[i] <- L_temp
}
is_small <- sum(results <L) #changed check if it is significiantly big swap '<' with '>'
print(c("P of being so small is estimated as ", is_small/reps))

#Histogram
lower <- quantile(x = results,probs = .05)
upper <-quantile (x=results, probs = .95)
L<-results
jpeg('histogramWholebody.jpg')
hist(L)
abline(v=c(lower, 0.13, upper), col=c("Black", "Red", "black"), lwd=2, lty=c(2,1,2))
dev.off()



#Menzerath's law correlation analysis ----
MenzData<-read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMDataMenz.csv")

cor.test(MenzData$Size, MenzData$t, method = 'spearman') #Correlation between sequence size and mean gesture duration per each sequence
cor.test(MenzData$Size, MenzData$T, method = 'spearman', alternative='greater') #Control correlation between T and n


#is M significanlty small?
reps <- 100000
results <- rep(0, reps)
x <- c(MenzData$Size)
y <- c(MenzData$t)
M <- sum(x*y)
print (c("real M is", M))
sortvector <- 1:length(x)
for (i in 1:reps)
{
  sortvector <- sample(sortvector, replace = F)
  xtemp <- x[sortvector]
  M_temp <- sum(xtemp *y)
  results[i] <- M_temp
}
hist(results)
is_small <- sum(results 
                <M)
print(c("P of being so small is estimated as ", is_small/reps))

#Histogram
lower <- quantile(x = results,probs = .05)
higher<- quantile(x=results, probs=0.95)
M<-results
jpeg('histogramMenz.jpg')
hist(M)
abline(v=c(lower, 1300.67, higher), col=c("Black", "Red"), lwd=2, lty=c(2,1))
dev.off()


#Data distribution analysis----
hist(GLMMData$Duration)
#Test distribution data
#install.packages("fitdistrplus")
library(fitdistrplus)
citation('fitdistrplus')
#I start by plotting the empirical distribution
plotdist(GLMMData$Duration, histo=TRUE, demp=T)

#now I will look at the skewness and kurtosis of the distribution
par(mfrow = c(1,1))
descdist(GLMMData$Duration, boot=1000)

#fitting first weibull distribution
fitW <- fitdist(GLMMData$Duration, "weibull")
summary(fitW)

#fitting second gamma distribution
fitG <- fitdist(GLMMData$Duration, "gamma")
summary(fitG)

#try also lognormal
fitN <- fitdist(GLMMData$Duration, "lnorm")
summary(fitN)

#create more plots to compare the fit via weibull and gamma
par(mfrow = c(1,1))
plot.legend <- c("Weibull", "gamma", "lognorm")
denscomp(list(fitW, fitG, fitN))
qqcomp(list(fitW,fitG, fitN))
cdfcomp(list(fitW,fitG, fitN))
ppcomp(list(fitW,fitG, fitN))

#more goodness of fit tests:
gofstat(list(fitW, fitG, fitN), fitnames = c("weibull", "gamma", "lognormal"))

#Transformation of data
GLMMData$logduration<-log(GLMMData$Duration) #transform duration in log


#Bayesian GLMM analysis  ----
#Get data

GLMMData <- read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMData.csv")
MenzData<- read.csv("https://raw.githubusercontent.com/Wild-Minds/LinguisticLaws_Papers/master/Data/GLMMDataMenz.csv")

GLMMData$F<-GLMMData$P*560
MenzData<-MenzData%>%select(Sequence_ID,PWB)
MenzDataModel<-right_join(GLMMData, MenzData, by = c("SequenceID"="Sequence_ID"))

Duane<-GLMMData%>%
  subset(Signaller == "Duane")
count(Duane)

DuaneMenz<-MenzDataModel%>%
  subset(Signaller == "Duane")
count(Duane)

NotDuane <- GLMMData%>%
  subset(!Signaller == "Duane")
count(NotDuane)

NotDuaneMenz<-MenzDataModel%>%
  subset(!Signaller == "Duane")
count(NotDuane)


#create dataset with 26 gesture types for whole data and then for Duane - use median.
ZipfData<- GLMMData%>%
  group_by(Gesture)%>%
  summarise(median=median(Duration),
            F=n()) #If we do this way we cannot really control for individual ID, sequence ID and the other random factors we have included. 
GLMMData <- GLMMData %>%
  select(Gesture, Category)
ZipfData <- right_join(ZipfData, GLMMData, by = "Gesture") #Added manual vs whole body categroy 
ZipfData<-ZipfData %>% distinct()

#Same for Duane
DuaneZipf<- Duane%>%
  group_by(Gesture)%>%
  summarise(median=median(Duration),
            F=n())

GLMMData <- GLMMData %>%
  select(Gesture, Category)
DuaneZipf <- inner_join(DuaneZipf, GLMMData, by = "Gesture") #Added manual vs whole body categroy 
DuaneZipf<-DuaneZipf %>% distinct()

#Descriptives
Recipient<-GLMMData%>%
  group_by(Recipient)%>%
  summarise( x=n())

Signaller<-GLMMData%>%
  group_by(Signaller)%>%
  summarise( x=n())
median(Signaller$x)

# run model 1 for full dataset (Zifp's Law of brevity)
Zipf.model <- brm(logduration ~ 
                    F + 
                    Category + 
                    (1|Signaller)+
                    (1|SequenceID) +
                    (1|Gesture),
                  family = gaussian, # family is Gaussian/normal
                  prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                  data = GLMMData,
                  chains = 3, 
                  cores = 4, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))



# the null model is one that contains the same random effects structure but none of the fixed effects, to test whether the fixed effects themselves have any impact at all
Zipf.model.null <- brm(logduration~
                         Category +
                         (1|Signaller)+
                         (1|SequenceID) +
                         (1|Gesture),
                       family = gaussian,
                       data=GLMMData,
                       chains = 3, 
                       cores = 4, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
Zipf.model <- add_criterion(Zipf.model, "loo")
Zipf.model.null <- add_criterion(Zipf.model.null, "loo")
loo_compare(Zipf.model.null, Zipf.model) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information

#Get model summary
summary(Zipf.model)
rhat(Zipf.model)
effective_sample(Zipf.model)

# run model for full dataset using the median of gesture types including all individuals (26 types)
Zipf.model.ts <- brm(median ~ 
                    F + 
                    Category,
                  family = lognormal(), 
                  prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                  data = ZipfData,
                  chains = 3, 
                  cores = 4, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))

# the null model 
Zipf.model.null.ts <- brm(median ~ 
                    Category,
                  family = lognormal(),
                  prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                  data = ZipfData,
                  chains = 3, 
                  cores = 4, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
Zipf.model.ts <- add_criterion(Zipf.model.ts, "loo")
Zipf.model.null.ts <- add_criterion(Zipf.model.null.ts, "loo")
loo_compare(Zipf.model.null.ts, Zipf.model.ts) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information

#Get model summary
summary(Zipf.model.ts)


# run model 1 for Duane data (Zipf's law of brevity)
duane.model <- brm(logduration ~ # formula is same as in lmer()
                     P + 
                     Category + 
                     Date +
                     (1|SequenceID) +
                     (1|Gesture),#Added date to control for within-individual variation
                   family = gaussian, # family is Gaussian/normal
                   prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                   data = Duane,
                   chains = 3, 
                   cores = 4, 
                   warmup = 1000, # tells R how many cycles to run
                   iter = 2000, 
                   control = list(adapt_delta = 0.99))


# the null model 
duane.model.null <- brm(logduration~
                          Category +
                          Date+
                          (1|SequenceID)+
                          (1|Gesture),
                        family = gaussian,
                        data=Duane,
                        prior=prior(cauchy(0,1), class="b"),
                        chains = 3, 
                        cores = 4, 
                        warmup = 1000, 
                        iter = 2000, 
                        control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
duane.model <- add_criterion(duane.model, "loo")
duane.model.null <- add_criterion(duane.model.null, "loo")
loo_compare(duane.model.null, duane.model) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information

# run model for full dataset using the medians of gesture types  only on Duane data (15 types)
Zipf.model.D <- brm(median ~ 
                       F + 
                       Category,
                     family = lognormal(), 
                     prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                     data = DuaneZipf,
                     chains = 3, 
                     cores = 4, 
                     warmup = 1000, 
                     iter = 2000, 
                     control = list(adapt_delta = 0.99))

# the null model 
Zipf.model.null.D <- brm(median ~ 
                            Category,
                          family = lognormal(),
                          prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                          data = DuaneZipf,
                          chains = 3, 
                          cores = 4, 
                          warmup = 1000, 
                          iter = 2000, 
                          control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
Zipf.model.D <- add_criterion(Zipf.model.D, "loo")
Zipf.model.null.D <- add_criterion(Zipf.model.null.D, "loo")
loo_compare(Zipf.model.null.D, Zipf.model.D) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information

#Get model summary
summary(Zipf.model.D)

#Run additional analysis on non-Duane data
Zipf.model.ND <- brm(logduration ~ 
                       F + 
                       Category + 
                       (1|Signaller)+
                       (1|SequenceID) +
                       (1|Gesture),
                     family = gaussian, # family is Gaussian/normal
                     prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                     data = NotDuane,
                     chains = 3, 
                     cores = 4, 
                     warmup = 1000, 
                     iter = 2000, 
                     control = list(adapt_delta = 0.99))

# the null model 
Zipf.model.null.ND <- brm(logduration ~ 
                            Category + 
                            (1|Signaller)+
                            (1|SequenceID) +
                            (1|Gesture),
                          family = gaussian(), # family is Gaussian/normal
                          prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                          data = NotDuane,
                          chains = 3, 
                          cores = 4, 
                          warmup = 1000, 
                          iter = 2000, 
                          control = list(adapt_delta = 0.99))

#compare models
Zipf.model.ND <- add_criterion(Zipf.model.ND, "loo", moment_match=T)
Zipf.model.null.ND <- add_criterion(Zipf.model.null.ND, "loo")
loo_compare(Zipf.model.null.ND, Zipf.model.ND)
summary(Zipf.model.ND)


# all the same for the Menz model
full.menz.model <- brm(logduration ~ 
                         SequenceSize +
                         PWB+
                         (1|Signaller)+
                         (1|SequenceID) 
                        ,
                       data=MenzDataModel,
                       family = gaussian,
                       prior = prior(cauchy(0,1), class="b"),
                       chains = 3, 
                       cores = 4, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))

full.menz.model.null <- brm(logduration ~ 
                              PWB +
                              (1|Signaller)+
                              (1|SequenceID),
                            
                            
                            data=MenzDataModel,
                            prior = prior(cauchy(0,1), class = "b"),
                            family = gaussian,
                            chains = 3, 
                            cores = 4, 
                            warmup = 1000, 
                            iter = 2000, 
                            control = list(adapt_delta = 0.99))



full.menz.model <- add_criterion(full.menz.model, "loo")
full.menz.model.null <- add_criterion(full.menz.model.null, "loo")
loo_compare(full.menz.model.null, full.menz.model)

summary(full.menz.model)

# all the same for the Duane Menz model
menz.model.Duane <- brm(logduration ~ 
                    SequenceSize +
                    PWB+
                      Date+
                    (1|SequenceID),
                  data=DuaneMenz,
                  family = gaussian,
                  prior = prior(cauchy(0,1), class = "b"),
                  chains = 3, 
                  cores = 4, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))
 pairs(menz.model.Duane)

menz.model.Duane.null <- brm(logduration ~ 
                         PWB+
                           Date +
                         (1|SequenceID),
                       data=DuaneMenz,
                       prior = prior(cauchy(0,1), class = "b"),
                       family = gaussian,
                       chains = 3, 
                       cores = 4, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))


menz.model.Duane <- add_criterion(menz.model.Duane, "loo")
menz.model.Duane.null <- add_criterion(menz.model.Duane.null, "loo")
loo_compare(menz.model.Duane.null, menz.model.Duane)
summary(menz.model.Duane)


#Run additional analysis on non-Duane data on Menzerath law
Menz.model.ND <- brm(logduration ~ 
                       SequenceSize +
                       PWB+
                       (1|Signaller)+
                       (1|SequenceID),
                     family = gaussian, # family is Gaussian/normal
                     prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                     data = NotDuaneMenz,
                     chains = 3, 
                     cores = 4, 
                     warmup = 1000, 
                     iter = 2000, 
                     control = list(adapt_delta = 0.99))

# the null model 
MEnz.model.null.ND <- brm(logduration ~ 
                            Category + 
                            (1|Signaller)+
                            (1|SequenceID),
                          family = gaussian(), # family is Gaussian/normal
                          prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                          data = NotDuaneMenz,
                          chains = 3, 
                          cores = 4, 
                          warmup = 1000, 
                          iter = 2000, 
                          control = list(adapt_delta = 0.99))

#compare models
Menz.model.ND <- add_criterion(Menz.model.ND, "loo")
ME.model.null.ND <- add_criterion(MEnz.model.null.ND, "loo")
loo_compare(ME.model.null.ND, Menz.model.ND)
summary(Menz.model.ND)


