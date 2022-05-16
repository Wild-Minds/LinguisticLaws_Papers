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
lower <- quantile(x = results,probs = .95)
jpeg('histogramZipf.jpg')
hist(results)
abline(v=c(lower, 2.39), col=c("Black", "Red"), lwd=2, lty=c(2,1))
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
jpeg('histogramManual.jpg')
hist(results)
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
jpeg('histogramWholebody.jpg')
hist(results)
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
jpeg('histogramMenz.jpg')
hist(results)
abline(v=c(lower, 1300.67), col=c("Black", "Red"), lwd=2, lty=c(2,1))
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

MenzData<-MenzData%>%select(Sequence_ID,PWB)
GLMMData<-left_join(GLMMData, MenzData, by = c("SequenceID"="Sequence_ID"))

Duane<-GLMMData%>%
  subset(Signaller == "Duane")

Recipient<-GLMMData%>%
  group_by(Recipient)%>%
  summarise( x=n())

Signaller<-GLMMData%>%
  group_by(Signaller)%>%
  summarise( x=n())
median(Signaller$x)

# run model for full dataset
Zipf.model <- brm(logduration ~ 
                    P + 
                    Category + 
                    (1|Signaller)+
                    (1|SequenceID) +
                    (1|Gesture),
                  family = gaussian, # family is Gaussian/normal
                  prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                  data = GLMMData,
                  chains = 3, 
                  cores = 1, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))

tab_model(all.model)
# the null model is one that contains the same random effects structure but none of the fixed effects, to test whether the fixed effects themselves have any impact at all
Zipf.model.null <- brm(logduration~
                         Category +
                         (1|Signaller)+
                         (1|SequenceID) +
                         (1|Gesture),
                       family = gaussian,
                       data=GLMMData,
                       chains = 3, 
                       cores = 10, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
all.model <- add_criterion(Zipf.model, "loo")
all.model.null <- add_criterion(Zipf.model.null, "loo")
loo_compare(Zipf.model.null, Zipf.model) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information



# run model for Duane
duane.model <- brm(logduration ~ # formula is same as in lmer()
                     P + 
                     Category + 
                     (1|SequenceID) +
                     (1|Gesture),
                   family = gaussian, # family is Gaussian/normal
                   prior = prior(cauchy(0,1), class = "b"), # prior is set to students t distribution around 0. Simply means that we do not a priori expect a positive or negative effect, but we constrain the estimates to be more or less normally distributed ('minimally informative prior')
                   data = Duane,
                   chains = 3, 
                   cores = 1, 
                   warmup = 1000, # tells R how many cycles to run
                   iter = 2000, 
                   control = list(adapt_delta = 0.99))


# the null model is one that contains the same random effects structure but none of the fixed effects, to test whether the fixed effects themselves have any impact at all
duane.model.null <- brm(logduration~
                          Category +
                          
                          (1|SequenceID) +
                          (1|Gesture),
                        family = gaussian,
                        data=Duane,
                        prior=prior(cauchy(0,1), class="b"),
                        chains = 3, 
                        cores = 10, 
                        warmup = 1000, 
                        iter = 2000, 
                        control = list(adapt_delta = 0.99))

# we compare those two models using the Leave-One-Out Information Criterion (LOO)
duane.model <- add_criterion(duane.model, "loo")
duane.model.null <- add_criterion(duane.model.null, "loo")
loo_compare(duane.model.null, duane.model) # if difference (elpd_diff) is two times the SE (se_diff), then we would consider the two models to be different. If it is not, the fixed effects don't add much information


# all the same for the Menz model

full.menz.model <- brm(logduration ~ 
                         SequenceSize +
                         PWB+
                         (1|Signaller)+
                         (1|SequenceID)  ,
                       
                       
                       data=GLMMData,
                       family = gaussian,
                       prior = prior(cauchy(0,1), class="b"),
                       chains = 3, 
                       cores = 10, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))

full.menz.model.null <- brm(logduration ~ 
                              PWB +
                              (1|Signaller)+
                              (1|SequenceID),
                            
                            
                            data=GLMMData,
                            prior = prior(cauchy(0,1), class = "b"),
                            family = gaussian,
                            chains = 3, 
                            cores = 10, 
                            warmup = 1000, 
                            iter = 2000, 
                            control = list(adapt_delta = 0.99))



full.menz.model <- add_criterion(full.menz.model, "loo")
full.menz.model.null <- add_criterion(full.menz.model.null, "loo")
loo_compare(full.menz.model.null, full.menz.model)


# all the same for the duane Menz model

menz.model <- brm(logduration ~ 
                    SequenceSize +
                    PWB+
                    (1|SequenceID),
                  data=Duane,
                  family = gaussian,
                  prior = prior(cauchy(0,1), class = "b"),
                  chains = 3, 
                  cores = 10, 
                  warmup = 1000, 
                  iter = 2000, 
                  control = list(adapt_delta = 0.99))

menz.model.null <- brm(logduration ~ 
                         PWB+
                         (1|SequenceID),
                       
                       data=Duane,
                       prior = prior(cauchy(0,1), class = "b"),
                       family = gaussian,
                       chains = 3, 
                       cores = 10, 
                       warmup = 1000, 
                       iter = 2000, 
                       control = list(adapt_delta = 0.99))


menz.model <- add_criterion(menz.model, "loo")
menz.model.null <- add_criterion(menz.model.null, "loo")
loo_compare(menz.model.null, menz.model)

tab_model(menz.model)
