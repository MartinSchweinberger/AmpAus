##################################################################
# Titel: The Amplifier System of Australian English
# Part 4
# R version 3.4.1 (2017-06-30) -- "Single Candle"
# Autor: Martin Schweinberger
# Date: 2018-10-21
# Contact: martin.schweinberger.hh@gmail.com
###############################################################
# If you have questions,suggestions or you found errors
# or in case you would to provide feedback, questions
# write an email to
# CITATION
# If you use this script or results thereof, please cite it as:
# Schweinberger, Martin. 2018. "The Amplifier System of Australian English",
# unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpAusE")
# load libraries
library(ggplot2)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"images"
###############################################################
# load data
reallyaus <- read.table("ampaus05_statz.txt", sep = "\t", header = T)
# inspect data
nrow(reallyaus); str(reallyaus); head(reallyaus)

###############################################################
#                    DATA VALIDATION
# check for NAs
sum(table(reallyaus$Occupation)) 
nrow(reallyaus)

# check distribution of Occupation
table(reallyaus$Occupation)

# plot really : Occupation
pOcc <- ggplot(reallyaus, aes(Occupation, really*100, color = Occupation)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Occupation", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyOccupation.png",sep="/"), width = 14)
pOcc

# remove Occupation: low N of sml (10) and no difference between Occ Types
reallyaus$Occupation <- NULL
# check if FileSpeaker can be used as a random effect or whether it should be removed
table(reallyaus$FileSpeaker)
# plot frequency of slots per speaker
hist(table(reallyaus$FileSpeaker), xlab = "Frequency of Adjectives per Speaker", 
     main = "", col = "lightgrey", breaks = 1:max(table(reallyaus$FileSpeaker)),
     ylim = c(0,100))
box()
# the vast majority of speakers occur only once .> FileSpeaker cannot be used as random effect!
###########################################################################
#                     TABULARIZATION
# load library
library(dplyr)
# factorize variables
clfct <- c("Age", "Adjective","FileSpeaker",  "Emotionality",  "Function", "Priming", "Gender", 
           "ConversationType", "AudienceSize", "Gradabilty", "SemanticCategory")
reallyaus[clfct] <- lapply(reallyaus[clfct], factor)
# count adj. slots by gender and age
tb4 <- reallyaus %>% 
  group_by(Age, Gender) %>%
  summarise(Freq = n())
# count speakers
NSpk <- reallyaus %>% 
  group_by(Age, Gender,FileSpeaker) %>%
  summarise(Freq = n())
f1725 <- nrow(NSpk[NSpk$Age == "17-25" & NSpk$Gender == "Female",])
m1725 <- nrow(NSpk[NSpk$Age == "17-25" & NSpk$Gender == "Male",])
f2640 <- nrow(NSpk[NSpk$Age == "26-40" & NSpk$Gender == "Female",])
m2640 <- nrow(NSpk[NSpk$Age == "26-40" & NSpk$Gender == "Male",])
f4180 <- nrow(NSpk[NSpk$Age == "41-80" & NSpk$Gender == "Female",])
m4180 <- nrow(NSpk[NSpk$Age == "41-80" & NSpk$Gender == "Male",])
tb4$Speakers <- c(f1725, m1725, f2640, m2640, f4180, m4180)
# count really
Nreally <- reallyaus %>% 
  group_by(Age, Gender, really) %>%
  summarise(Freq = n())
# add really
tb4$really <- Nreally$Freq[Nreally$really == 1]
# add percent
tb4$Percent <- round(tb4$really/tb4$Freq*100, 2)
tb4 <- data.frame(tb4$Age, tb4$Gender, tb4$Speakers, tb4$Freq, tb4$really, tb4$Percent)
colnames(tb4) <- c("Age", "Gender", "Speakers (N)", "Adjective Slots (N)", "really (N)", "Percent")
tb4

sum(tb4$`Speakers (N)`)
# remove FileSpeaker
reallyaus$FileSpeaker <- NULL
###########################################################################
#                     VIZUALIZATION
# prepare plot data (pd) 
pd <- reallyaus
pd$really <- pd$really *100
# convert Age column
Agelbs <- names(table(pd$Age))
pd$Age <- ifelse(pd$Age == "17-25", 3,
                 ifelse(pd$Age == "26-40", 2, 
                        ifelse(pd$Age == "41-80", 1, pd$Age)))
pd$Age<- as.numeric(pd$Age)
# start plotting
pdummy <- ggplot(pd, aes(Gender, really, color = Gender)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.25) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Gender", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
pdummy

# really : Gender
p10 <- ggplot(pd, aes(Gender, really, color = Gender)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Gender", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyGender.png",sep="/"))
p10

# really: Age
p11 <- ggplot(pd, aes(x = Age, y = really)) +
  geom_smooth(aes(y = really), size=.5, col = "gray30", lty = "longdash", se = F) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent of Amplification") +
  theme_light(base_size = 10) +
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imageDirectory,"ReallyAge.png",sep="/"))
p11

# really: Freq
p12 <- ggplot(pd, aes(x = Freq, y = really)) +
  geom_smooth(aes(y = really), size=.5, col = "gray30", lty = "longdash", se = F) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Frequency of Adjective", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  theme_light(base_size = 10) 
ggsave(file = paste(imageDirectory,"ReallyFreq.png",sep="/"))
p12

# really : Priming
p13 <- ggplot(pd, aes(Priming, really, color = Priming)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Priming", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyPriming.png",sep="/"))
p13

# really : Adjective
p14d <- pd
p14d$Adjective <- factor(p14d$Adjective, levels = names(table(p14d$Adjective))[order(tapply(p14d$really, p14d$Adjective, mean))])
p14 <- ggplot(p14d, aes(Adjective, really, color = Adjective)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Adjective", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50", "grey50", "grey50", "grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyAdjective.png",sep="/"))
p14

# really : Function
p15 <- ggplot(pd, aes(Function, really, color = Function)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Syntactic function", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyFunction.png",sep="/"))
p15

# really : Emotionality
p16 <- ggplot(pd, aes(Emotionality, really, color = Emotionality)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Emotionality of Adj.", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyEmotionality.png",sep="/"))
p16

# really : Gradabilty
p17 <- ggplot(pd, aes(Gradabilty, really, color = Gradabilty)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Gradability of Adj.", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyGrad.png",sep="/"))
p17

# really : SemanticCategory
p18d <- pd
p18d$SemanticCategory <- factor(p18d$SemanticCategory, 
                                levels = names(table(p18d$SemanticCategory))[order(tapply(p18d$really, 
                                                                                          p18d$SemanticCategory, mean))])
p18 <- ggplot(p18d, aes(SemanticCategory, really, color = SemanticCategory)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Semantic class of Adj.", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50", "grey50", "grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallySem.png",sep="/"), width = 14)
p18

# really : ConversationType
p19 <- ggplot(pd, aes(ConversationType, really, color = ConversationType)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Conversation type.", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyConvTyp.png",sep="/"), width = 14)
p19

# really : AudienceSize
p20 <- ggplot(pd, aes(AudienceSize, really, color = AudienceSize)) +
  stat_summary(fun.y = mean, geom = "point", size = 2) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_light(base_size = 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position="none") +
  labs(x = "Conversation Type", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  scale_color_manual(values = c("grey50", "grey50"))
ggsave(file = paste(imageDirectory,"ReallyAudienceSize.png",sep="/"), width = 14)
p20

# interaction plots
# p100
pd100 <- data.frame(pd$Age, pd$Gender, pd$really)
colnames(pd100) <- gsub("pd.", "", colnames(pd100))
pd100 <- na.omit(pd100)
p100 <- ggplot(pd100, aes(Age, really, colour = Gender)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_set(theme_light(base_size = 10)) +
  #  theme(legend.position="none") +
  labs(x = "Gender", y = "Percent (REALLY in Ampl. Adj. Slots)", colour = "Gender") +
  scale_color_manual(values = c("grey40", "grey60", "grey80")) +   
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imageDirectory,"ReallyAgeGender.png",sep="/"))
p100

# prepare data p101
p101d <- tapply(pd$really, list(pd$Age, pd$Gender), mean)
p101d <- data.frame(rownames(p101d), p101d)
colnames(p101d) <- c("Age", "Women", "Men")
p101d$Age <- as.numeric(ifelse(p101d$Age == "17-25", 3,
                               ifelse(p101d$Age == "26-40", 2, 
                                      ifelse(p101d$Age == "41-80", 1, p101d$Age))))
# p101
p101 <- ggplot(p101d, aes(jitter(Age), jitter(Women))) +
  geom_smooth(aes(y = jitter(Women), color = "Women", linetype = "Women"), size=1, se = F) +
  geom_smooth(aes(y = jitter(Men), color = "Men", linetype = "Men"), size=1, se = F) +
  geom_smooth(aes(y = jitter(Women), color = "Women (Reg. Line)", linetype = "Women (Reg. Line)"), method='lm', se = FALSE, size=1) +
  geom_smooth(aes(y = jitter(Men), color = "Men (Reg. Line)", linetype = "Men (Reg. Line)"), method='lm', se = FALSE, size=1) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","dotted", "solid", "dotted"),
                        name="Gender",
                        breaks = c("Women", "Men", "Women (Reg. Line)", "Men (Reg. Line)"), 
                        labels = c("Women", "Men", "Women (Reg. Line)", "Men (Reg. Line)")) +
  scale_colour_manual(values=c("grey40", "blue", "grey40", "red"),
                      name="Gender", 
                      breaks=c("Women", "Men", "Women (Reg. Line)", "Men (Reg. Line)"), 
                      labels = c("Women", "Men", "Women (Reg. Line)", "Men (Reg. Line)")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imageDirectory,"ReallyAgeGenderSmooth.png",sep="/"))
p101

# prepare data p102
p102d <- tapply(pd$really, list(pd$Age, pd$Function), mean)
p102d <- data.frame(rownames(p102d), p102d)
colnames(p102d) <- c("Age", "Attributive", "Predicative")
p102d$Age <- as.numeric(ifelse(p102d$Age == "17-25", 3,
                               ifelse(p102d$Age == "26-40", 2, 
                                      ifelse(p102d$Age == "41-80", 1, p102d$Age))))
# p102
p102 <- ggplot(p102d, aes(jitter(Age), jitter(Attributive))) +
  geom_smooth(aes(y = jitter(Attributive), color = "Attributive", linetype = "Attributive"), size=1, se = F) +
  geom_smooth(aes(y = jitter(Predicative), color = "Predicative", linetype = "Predicative"), size=1, se = F) +
  geom_smooth(aes(y = jitter(Attributive), color = "Attributive (Reg. Line)", linetype = "Attributive (Reg. Line)"), method='lm', se = FALSE, size=1) +
  geom_smooth(aes(y = jitter(Predicative), color = "Predicative (Reg. Line)", linetype = "Predicative (Reg. Line)"), method='lm', se = FALSE, size=1) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed","dotted", "solid", "dotted"),
                        name="Gender",
                        breaks = c("Attributive", "Predicative", "Attributive (Reg. Line)", "Predicative (Reg. Line)"), 
                        labels = c("Attributive", "Predicative", "Attributive (Reg. Line)", "Predicative (Reg. Line)")) +
  scale_colour_manual(values=c("grey40", "blue", "grey40", "red"),
                      name="Gender", 
                      breaks=c("Attributive", "Predicative", "Attributive (Reg. Line)", "Predicative (Reg. Line)"), 
                      labels = c("Attributive", "Predicative", "Attributive (Reg. Line)", "Predicative (Reg. Line)")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imageDirectory,"ReallyAgeFunctionrSmooth.png",sep="/"))
p102

# p103
pd103 <- data.frame(pd$ConversationType, pd$Priming, pd$really)
colnames(pd103) <- gsub("pd.", "", colnames(pd103))
pd103 <- na.omit(pd103)
p103 <- ggplot(pd103, aes(ConversationType, really, colour = Priming)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  coord_cartesian(ylim = c(0, 103)) +
  theme_set(theme_light(base_size = 10)) +
  #  theme(legend.position="none") +
  labs(x = "Priming", y = "Percent (REALLY in Ampl. Adj. Slots)", colour = "Gender") +
  scale_color_manual(values = c("grey40", "grey80")) +   
  ggsave(file = paste(imageDirectory,"ReallyConversationTypePriming.png",sep="/"))
p103

# prepare data p104
p104d <- data.frame(pd$really, pd$ConversationType, pd$Freq)
colnames(p104d) <- c("really", "ConversationType", "Freq")
p104d$ConversationType <- as.factor(p104d$ConversationType)
# p104
p104 <- ggplot(p104d, aes(Freq, really, group = ConversationType)) +
  geom_smooth(aes(y = really, color = "ConversationType", linetype = "ConversationType"), size=1, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "solid"),
                        name="ConversationType",
                        breaks = c("MixedSex", "SameSex"), 
                        labels = c("MixedSex", "SameSex")) +
  scale_colour_manual(values=c("grey40", "grey80"),
                      name="ConversationType", 
                      breaks=c("MixedSex", "SameSex"), 
                      labels = c("MixedSex", "SameSex")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Frequency of Adjective", y = "Percent (REALLY in Ampl. Adj. Slots)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
ggsave(file = paste(imageDirectory,"ReallyFreqConversationType.png",sep="/"))
p104

###########################################################################
#           MIXED EFFECTS BIONOMIAL LOGISTIC REGRESSION
# load library
library(rms)
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
reallyaus.dist <- datadist(reallyaus)
options(datadist = "reallyaus.dist")
# generate initial minimal regression model 
m0.glm = glm(really ~ 1, family = binomial, data = reallyaus) # baseline model glm
m0.lrm = lrm(really ~ 1, data = reallyaus, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# load library
library(lme4)
# create model with a random intercept for token
m0.lmer <- lmer(really ~ (1|Adjective), data = reallyaus, family = binomial)
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(really ~ (1|Adjective), data = reallyaus, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glmer object is smaller which shows that including the random
# intercepts is justified

# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) # sig m0.glmer better than m0.glm

# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up procedure
# we need to add "control = glmerControl(optimizer = "bobyqa")" 
# because otherwise R fails to converge
#	manual modelfitting
m0.glmer <- glmer(really ~ 1+ (1|Adjective), family = binomial, data = reallyaus, 
                  control=glmerControl(optimizer="bobyqa"))
# add Age
m1.glmer <- update(m0.glmer, .~.+Age)
m1.glm <- update(m0.glm, .~.+Age)
vif(m1.glm) # VIFs ok
anova(m0.glmer, m1.glmer, test = "Chi") # not sig!

# add Function
ftable(reallyaus$Function, reallyaus$really)
m2.glmer <- update(m0.glmer, .~.+Function)
m2.glm <- update(m0.glm, .~.+Function)
vif(m2.glm) # VIFs ok
anova(m0.glmer, m2.glmer, test = "Chi") # mar sig (p=0.05292)

# AIC: AIC(m0.glmer)-AIC(m2.glmer) -> -1.75), BIC (BIC(m0.glmer)-BIC(m2.glmer -> 2: no reason to include!

# add Priming
ftable(reallyaus$Priming, reallyaus$really)
m3.glmer <- update(m0.glmer, .~.+Priming)
m3.glm <- update(m0.glm, .~.+Priming)
vif(m3.glm) # VIFs ok
anova(m0.glmer, m3.glmer, test = "Chi") # mar sig (p=0.05934)

# AIC: AIC(m0.glmer)-AIC(m3.glmer) -> -1.55), BIC (BIC(m0.glmer)-BIC(m3.glmer) -> 2.19: no reason to include!

# add Gender
ftable(reallyaus$Gender, reallyaus$really)
m4.glmer <- update(m0.glmer, .~.+Gender)
m4.glm <- update(m0.glm, .~.+Gender)
vif(m4.glm) # VIFs ok
anova(m0.glmer, m4.glmer, test = "Chi") # SIG (p=0.00515 **)

# add ConversationType
ftable(reallyaus$ConversationType, reallyaus$really)
m5.glmer <- update(m4.glmer, .~.+ConversationType)
m5.glm <- update(m4.glm, .~.+ConversationType)
vif(m5.glm) # VIFs ok
anova(m4.glmer, m5.glmer, test = "Chi") # mar sig. (p=0.05265) 

# AIC: AIC(m4.glmer)-AIC(m5.glmer) -> -1.76), BIC: BIC(m4.glmer)-BIC(m5.glmer) -> 2.00: no reason to include!

# add AudienceSize
ftable(reallyaus$AudienceSize, reallyaus$really)
m6.glmer <- update(m4.glmer, .~.+AudienceSize)
m6.glm <- update(m4.glm, .~.+AudienceSize)
vif(m6.glm) # VIFs ok
anova(m4.glmer, m6.glmer, test = "Chi") # not sig (p=0.144)

# add Freq
ftable(reallyaus$Freq, reallyaus$really)
m7.glmer <- update(m4.glmer, .~.+Freq)
m7.glm <- update(m4.glm, .~.+Freq)
vif(m7.glm) # VIFs ok
anova(m4.glmer, m7.glmer, test = "Chi") # SIG (p=0.01441*)

# add Gradabilty
ftable(reallyaus$Gradabilty, reallyaus$really)
m8.glmer <- update(m7.glmer, .~.+Gradabilty)
m8.glm <- update(m7.glm, .~.+Gradabilty)
vif(m8.glm) # VIFs ok
anova(m7.glmer, m8.glmer, test = "Chi") # not sig (p=0.1379)

# add SemanticCategory
ftable(reallyaus$SemanticCategory, reallyaus$really)
m9.glmer <- update(m7.glmer, .~.+SemanticCategory)
m9.glm <- update(m7.glm, .~.+SemanticCategory)
vif(m9.glm) # VIFs ok
anova(m7.glmer, m9.glmer, test = "Chi") # mar sig (p=0.07522)

# AIC: AIC(m9.glmer)-AIC(m7.glmer) -> -0.49), BIC: BIC(m7.glmer)-BIC(m9.glmer) -> 14.51: no reason to include!

# add Emotionality
ftable(reallyaus$Emotionality, reallyaus$really)
m10.glmer <- update(m7.glmer, .~.+Emotionality)
m10.glm <- update(m7.glm, .~.+Emotionality)
vif(m10.glm) # VIFs ok
anova(m7.glmer, m10.glmer, test = "Chi") # mar sig (p=0.0866)

# AIC: AIC(m10.glmer)-AIC(m7.glmer) -> -0.8930034), BIC: BIC(m7.glmer)-BIC(m10.glmer) -> 6.605783: no reason to include!

###########################################################################
# find all 2-way interactions
library(utils)
colnames(reallyaus)

vars <- c("Age", "Function", "Priming", "Gender", "ConversationType", "AudienceSize",
          "Freq", "Gradabilty", "SemanticCategory", "Emotionality")
intac <- t(combn(vars, 2))
intac

# add Age*Function
ftable(reallyaus$Age, reallyaus$Function, reallyaus$really) 
m11.glmer <- update(m7.glmer, .~.+Age*Function)
m11.glm <- update(m7.glm, .~.+Age*Function)
vif(m11.glm) # VIFs ok
anova(m7.glmer, m11.glmer, test = "Chi") # SIG (p=0.0133) BUT BIC inflated! 

# AIC: AIC(m7.glmer)-AIC(m11.glmer) -> -4.39), BIC: BIC(m7.glmer)-BIC(m11.glmer) -> 14.36: DO NOT INCLUDE!

# add Age*Priming
ftable(reallyaus$Age, reallyaus$Priming, reallyaus$really) # not possible

# add Age*Gender
ftable(reallyaus$Age, reallyaus$Gender, reallyaus$really) 
m12.glmer <- update(m7.glmer, .~.+Age*Gender)  
m12.glm <- update(m7.glm, .~.+Age*Gender)
vif(m12.glm) # VIFs ok
anova(m12.glmer, m7.glmer, test = "Chi") # not sig (p=0.4088)

# add Age*ConversationType
ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$really)
m13.glmer <- update(m7.glmer, .~.+Age*ConversationType)  
m13.glm <- update(m7.glm, .~.+Age*ConversationType)
vif(m13.glm) # VIFs ok
anova(m13.glmer, m7.glmer, test = "Chi") # not sig (p=0.1048)

# add Age*AudienceSize
ftable(reallyaus$Age, reallyaus$AudienceSize, reallyaus$really) 
m14.glmer <- update(m7.glmer, .~.+Age*AudienceSize)  
m14.glm <- update(m7.glm, .~.+Age*AudienceSize)
vif(m14.glm) # VIFs ok
anova(m14.glmer, m7.glmer, test = "Chi") # not sig (p=0.3414)

# add Age*Freq
m15.glmer <- update(m7.glmer, .~.+Age*Freq)  
m15.glm <- update(m7.glm, .~.+Age*Freq)
vif(m15.glm) # VIFs ok
anova(m15.glmer, m7.glmer, test = "Chi") # not sig (p=0.3655)

# add Age*Gradabilty
ftable(reallyaus$Age, reallyaus$Gradabilty, reallyaus$really) # not possible

# add Age*SemanticCategory
ftable(reallyaus$Age, reallyaus$SemanticCategory, reallyaus$really) # not possible

# add Age*Emotionality
ftable(reallyaus$Age, reallyaus$Emotionality, reallyaus$really) 
m16.glmer <- update(m7.glmer, .~.+Age*Emotionality)  
m16.glm <- update(m7.glm, .~.+Age*Emotionality)
vif(m16.glm) # VIFs  unacceptable!

# add Function*Priming
ftable(reallyaus$Function, reallyaus$Priming, reallyaus$really) 
m17.glmer <- update(m7.glmer, .~.+Function*Priming)  
m17.glm <- update(m7.glm, .~.+Function*Priming)
vif(m17.glm) # VIFs ok
anova(m17.glmer, m7.glmer, test = "Chi") # mar sig (p=0.0685)

# AIC: AIC(m17.glmer)-AIC(m7.glmer) -> -1.11), BIC: BIC(m7.glmer)-BIC(m17.glmer) -> 10.14: no reason to include!

# add Function*Gender
ftable(reallyaus$Function, reallyaus$Gender, reallyaus$really) 
m18.glmer <- update(m7.glmer, .~.+Function*Gender)  
m18.glm <- update(m7.glm, .~.+Function*Gender)
vif(m18.glm) # VIFs ok
anova(m18.glmer, m7.glmer, test = "Chi") # not sig (p=0.1026)

# add Function*ConversationType
ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$really)
m19.glmer <- update(m7.glmer, .~.+Function*ConversationType)  
m19.glm <- update(m7.glm, .~.+Function*ConversationType)
vif(m19.glm) # VIFs ok
anova(m19.glmer, m7.glmer, test = "Chi") # mar sig (p=0.05836)

# AIC: AIC(m19.glmer)-AIC(m7.glmer) -> -1.47), BIC: BIC(m19.glmer)-BIC(m7.glmer) -> 9.77: no reason to include!


# add Function*AudienceSize
ftable(reallyaus$Function, reallyaus$AudienceSize, reallyaus$really) 
m20.glmer <- update(m7.glmer, .~.+Function*AudienceSize)  
m20.glm <- update(m7.glm, .~.+Function*AudienceSize)
vif(m20.glm) # VIFs ok
anova(m20.glmer, m7.glmer, test = "Chi") # not sig (p=0.07207)

# AIC: AIC(m20.glmer)-AIC(m7.glmer) -> -0.99), BIC: BIC(m20.glmer)-BIC(m7.glmer) -> 10.25: no reason to include!

# add Function*Freq
m21.glmer <- update(m7.glmer, .~.+Function*Freq)  
m21.glm <- update(m7.glm, .~.+Function*Freq)
vif(m21.glm) # VIFs ok
anova(m21.glmer, m7.glmer, test = "Chi") # SIG (p=0.01019)

# AIC: AIC(m21.glmer)-AIC(m7.glmer) -> -5.17), BIC: BIC(m21.glmer)-BIC(m7.glmer) -> 2.32: no reason to include!

# add Function*Gradabilty
ftable(reallyaus$Function, reallyaus$Gradabilty, reallyaus$really) # not possible

# add Function*SemanticCategory
ftable(reallyaus$Function, reallyaus$SemanticCategory, reallyaus$really)
m22.glmer <- update(m7.glmer, .~.+Function*SemanticCategory)  
m22.glm <- update(m7.glm, .~.+Function*SemanticCategory)
vif(m22.glm) # VIFs  unacceptable!

# add Function*Emotionality
ftable(reallyaus$Function, reallyaus$Emotionality, reallyaus$really) 
m23.glmer <- update(m7.glmer, .~.+Function*Emotionality)  
m23.glm <- update(m7.glm, .~.+Function*Emotionality)
vif(m23.glm) # VIFs  unacceptable!

# add Priming*Gender
ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$really) 
m24.glmer <- update(m7.glmer, .~.+Priming*Gender)  
m24.glm <- update(m7.glm, .~.+Priming*Gender)
vif(m24.glm) # VIFs ok
anova(m24.glmer, m7.glmer, test = "Chi") # not sig (p=0.1716)

# add Priming*ConversationType
ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$really)
m25.glmer <- update(m7.glmer, .~.+Priming*ConversationType)  
m25.glm <- update(m7.glm, .~.+Priming*ConversationType)
vif(m25.glm) # VIFs ok
anova(m25.glmer, m7.glmer, test = "Chi") # SIG (p=0.04956) BUT BIC inflated! 

# AIC: AIC(m25.glmer)-AIC(m7.glmer) -> -1.83), BIC: BIC(m25.glmer)-BIC(m7.glmer) -> 9.41: DO NOT INCLUDE!

# add Priming*AudienceSize
ftable(reallyaus$Priming, reallyaus$AudienceSize, reallyaus$really) 
m26.glmer <- update(m7.glmer, .~.+Priming*AudienceSize)  
m26.glm <- update(m7.glm, .~.+Priming*AudienceSize)
vif(m26.glm) # VIFs ok
anova(m26.glmer, m7.glmer, test = "Chi") # not sig (p=0.2124)

# add Priming*Freq
m27.glmer <- update(m7.glmer, .~.+Priming*Freq)  
m27.glm <- update(m7.glm, .~.+Priming*Freq)
vif(m27.glm) # VIFs ok
anova(m27.glmer, m7.glmer, test = "Chi") # not sig (p=0.3574)

# add Priming*Gradabilty
ftable(reallyaus$Priming, reallyaus$Gradabilty, reallyaus$really) # not possible

# add Priming*SemanticCategory
ftable(reallyaus$Priming, reallyaus$SemanticCategory, reallyaus$really)
m29.glmer <- update(m7.glmer, .~.+Priming*SemanticCategory)  
m29.glm <- update(m7.glm, .~.+Priming*SemanticCategory)
vif(m29.glm) # VIFs unacceptable

# add Priming*Emotionality
ftable(reallyaus$Priming, reallyaus$Emotionality, reallyaus$really) 
m30.glmer <- update(m7.glmer, .~.+Priming*Emotionality)  
m30.glm <- update(m7.glm, .~.+Priming*Emotionality)
vif(m30.glm) # VIFs unacceptable

# add Gender*ConversationType
ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$really)
m31.glmer <- update(m7.glmer, .~.+Gender*ConversationType)  
m31.glm <- update(m7.glm, .~.+Gender*ConversationType)
vif(m31.glm) # VIFs ok
anova(m31.glmer, m7.glmer, test = "Chi") # mar sig (p=0.07753) BUT BIC INFLATION!

# AIC: AIC(m31.glmer)-AIC(m7.glmer) -> -1.11), BIC: BIC(m31.glmer)-BIC(m7.glmer) -> 6.38: DO NOT INCLUDE!

# add Gender*AudienceSize
ftable(reallyaus$Gender, reallyaus$AudienceSize, reallyaus$really) 
m32.glmer <- update(m7.glmer, .~.+Gender*AudienceSize)  
m32.glm <- update(m7.glm, .~.+Gender*AudienceSize)
vif(m32.glm) # VIFs ok
anova(m32.glmer, m7.glmer, test = "Chi") # not sig (p=0.3034)

# add Gender*Freq
m33.glmer <- update(m7.glmer, .~.+Gender*Freq)  
m33.glm <- update(m7.glm, .~.+Gender*Freq)
vif(m33.glm) # VIFs ok
anova(m33.glmer, m7.glmer, test = "Chi") # not sig (p=0.3343)

# add Gender*Gradabilty
ftable(reallyaus$Gender, reallyaus$Gradabilty, reallyaus$really) 
m34.glmer <- update(m7.glmer, .~.+Gender*Gradabilty)  
m34.glm <- update(m7.glm, .~.+Gender*Gradabilty)
vif(m34.glm) # VIFs unacceptable

# add Gender*SemanticCategory
ftable(reallyaus$Gender, reallyaus$SemanticCategory, reallyaus$really)
m35.glmer <- update(m7.glmer, .~.+Gender*SemanticCategory)  
m35.glm <- update(m7.glm, .~.+Gender*SemanticCategory)
vif(m35.glm) # VIFs unacceptable

# add Gender*Emotionality
ftable(reallyaus$Gender, reallyaus$Emotionality, reallyaus$really) 
m36.glmer <- update(m7.glmer, .~.+Gender*Emotionality)  
m36.glm <- update(m7.glm, .~.+Gender*Emotionality)
vif(m36.glm) # VIFs unacceptable

# add ConversationType*AudienceSize
ftable(reallyaus$ConversationType, reallyaus$AudienceSize, reallyaus$really) 
m37.glmer <- update(m7.glmer, .~.+ConversationType*AudienceSize)  
m37.glm <- update(m7.glm, .~.+ConversationType*AudienceSize)
vif(m37.glm) # VIFs ok
anova(m37.glmer, m7.glmer, test = "Chi") # SIG (p=0.01695) BUT BIC INFLATION!

# AIC: AIC(m37.glmer)-AIC(m7.glmer) -> -4.29), BIC: BIC(m37.glmer)-BIC(m7.glmer) -> 7.05: DO NOT INCLUDE!

# add ConversationType*Freq
m38.glmer <- update(m7.glmer, .~.+ConversationType*Freq)  
m38.glm <- update(m7.glm, .~.+ConversationType*Freq)
vif(m38.glm) # VIFs ok
anova(m38.glmer, m7.glmer, test = "Chi") # SIG (p=0.03645) BUT BIC INFLATION!

# AIC: AIC(m38.glmer)-AIC(m7.glmer) -> -2.62), BIC: BIC(m38.glmer)-BIC(m7.glmer) -> 4.88: DO NOT INCLUDE!

# add ConversationType*Gradabilty
ftable(reallyaus$ConversationType, reallyaus$Gradabilty, reallyaus$really) 
m39.glmer <- update(m7.glmer, .~.+ConversationType*Gradabilty)  
m39.glm <- update(m7.glm, .~.+ConversationType*Gradabilty)
vif(m39.glm) # VIFs unacceptable

# add ConversationType*SemanticCategory
ftable(reallyaus$ConversationType, reallyaus$SemanticCategory, reallyaus$really)
m40.glmer <- update(m7.glmer, .~.+ConversationType*SemanticCategory)  
m40.glm <- update(m7.glm, .~.+ConversationType*SemanticCategory)
vif(m40.glm) # VIFs unacceptable

# add ConversationType*Emotionality
ftable(reallyaus$ConversationType, reallyaus$Emotionality, reallyaus$really) 
m41.glmer <- update(m7.glmer, .~.+ConversationType*Emotionality)  
m41.glm <- update(m7.glm, .~.+ConversationType*Emotionality)
vif(m41.glm) # VIFs unacceptable

# add AudienceSize*Freq
m42.glmer <- update(m7.glmer, .~.+AudienceSize*Freq)  
m42.glm <- update(m7.glm, .~.+AudienceSize*Freq)
vif(m42.glm) # VIFs ok
anova(m42.glmer, m7.glmer, test = "Chi") # mar sig (p=0.1233)

# AIC: AIC(m42.glmer)-AIC(m7.glmer) -> -1.32), BIC: BIC(m42.glmer)-BIC(m7.glmer) -> 6.18: no reason to include!

# add AudienceSize*Gradabilty
ftable(reallyaus$AudienceSize, reallyaus$Gradabilty, reallyaus$really) 
m43.glmer <- update(m7.glmer, .~.+AudienceSize*Gradabilty) # Model failed to converge 

# add AudienceSize*SemanticCategory
ftable(reallyaus$AudienceSize, reallyaus$SemanticCategory, reallyaus$really)
m44.glmer <- update(m7.glmer, .~.+AudienceSize*SemanticCategory)  
m44.glm <- update(m7.glm, .~.+AudienceSize*SemanticCategory)
vif(m44.glm) # VIFs unacceptable

# add AudienceSize*Emotionality
ftable(reallyaus$AudienceSize, reallyaus$Emotionality, reallyaus$really) 
m45.glmer <- update(m7.glmer, .~.+AudienceSize*Emotionality)  
m45.glm <- update(m7.glm, .~.+AudienceSize*Emotionality)
vif(m45.glm) # VIFs ok
anova(m45.glmer, m7.glmer, test = "Chi") # not sig (p=0.1089)

# add Freq*Gradabilty
m46.glmer <- update(m7.glmer, .~.+Freq*Gradabilty) # Model is nearly unidentifiable 

# add Freq*SemanticCategory
m47.glmer <- update(m7.glmer, .~.+Freq*SemanticCategory)  
m47.glm <- update(m7.glm, .~.+Freq*SemanticCategory)
vif(m47.glm) # VIFs unacceptable

# add Freq*Emotionality
m48.glmer <- update(m7.glmer, .~.+Freq*Emotionality)  
m48.glm <- update(m7.glm, .~.+Freq*Emotionality)
vif(m48.glm) # VIFs unacceptable

# add Gradabilty*SemanticCategory
ftable(reallyaus$Gradabilty, reallyaus$SemanticCategory, reallyaus$really)
m49.glmer <- update(m7.glmer, .~.+Gradabilty*SemanticCategory) # Hessian is numerically singular 

# add Gradabilty*Emotionality
ftable(reallyaus$Gradabilty, reallyaus$Emotionality, reallyaus$really) 
m50.glmer <- update(m7.glmer, .~.+Gradabilty*Emotionality) # Model is nearly unidentifiable 

# add SemanticCategory*Emotionality
ftable(reallyaus$SemanticCategory, reallyaus$Emotionality, reallyaus$really)                         
m51.glmer <- update(m7.glmer, .~.+SemanticCategory*Emotionality) # fixed-effect model matrix is rank deficient  

###########################################################################
# find all 3-way interactions
#install.packages("utils")
library(utils)
vars <- c("Age", "Function", "Priming", "Gender", "ConversationType", "AudienceSize",
          "Freq", "Gradabilty", "SemanticCategory", "Emotionality")
intac <- t(combn(vars, 3))
intac

# check if interaction is possible
AgeFunctionPriming <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$Priming)) 
AgeFunctionGender <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$Gender)) 
AgeFunctionConversationType <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$ConversationType))
AgeFunctionAudienceSize <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$AudienceSize)) 
AgeFunctionFreq <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$Freq)) 
AgeFunctionGradabilty <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$Gradabilty)) 
AgeFunctionSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$SemanticCategory))
AgeFunctionEmotionality <- min(ftable(reallyaus$Age, reallyaus$Function, reallyaus$Emotionality)) 
AgePrimingGender <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$Gender)) 
AgePrimingConversationType <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$ConversationType))
AgePrimingAudienceSize <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$AudienceSize)) 
AgePrimingFreq <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$Freq)) 
AgePrimingGradabilty <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$Gradabilty)) 
AgePrimingSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$SemanticCategory))
AgePrimingEmotionality <- min(ftable(reallyaus$Age, reallyaus$Priming, reallyaus$Emotionality)) 
AgeGenderConversationType <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$ConversationType))
AgeGenderAudienceSize <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$AudienceSize)) 
AgeGenderFreq <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$Freq)) 
AgeGenderGradabilty <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$Gradabilty)) 
AgeGenderSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$SemanticCategory))
AgeGenderEmotionality <- min(ftable(reallyaus$Age, reallyaus$Gender, reallyaus$Emotionality)) 
AgeConversationTypeAudienceSize <- min(ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$AudienceSize)) 
AgeConversationTypeFreq <- min(ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$Freq)) 
AgeConversationTypeGradabilty <- min(ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$Gradabilty)) 
AgeConversationTypeSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$SemanticCategory))
AgeConversationTypeEmotionality <- min(ftable(reallyaus$Age, reallyaus$ConversationType, reallyaus$Emotionality)) 
AgeAudienceSizeFreq <- min(ftable(reallyaus$Age, reallyaus$AudienceSize, reallyaus$Freq)) 
AgeAudienceSizeGradabilty <- min(ftable(reallyaus$Age, reallyaus$AudienceSize, reallyaus$Gradabilty)) 
AgeAudienceSizeSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$AudienceSize, reallyaus$SemanticCategory))
AgeAudienceSizeEmotionality <- min(ftable(reallyaus$Age, reallyaus$AudienceSize, reallyaus$Emotionality)) 
AgeFreqGradabilty <- min(ftable(reallyaus$Age, reallyaus$Freq, reallyaus$Gradabilty)) 
AgeFreqSemanticCategory <- min(ftable(reallyaus$Age, reallyaus$Freq, reallyaus$SemanticCategory))
AgeFreqEmotionality <- min(ftable(reallyaus$Age, reallyaus$Freq, reallyaus$Emotionality)) 
AgeGradabiltySemanticCategory <- min(ftable(reallyaus$Age, reallyaus$Gradabilty, reallyaus$SemanticCategory))
AgeGradabiltyEmotionality <- min(ftable(reallyaus$Age, reallyaus$Gradabilty, reallyaus$Emotionality)) 
AgeSemanticCategoryEmotionality <- min(ftable(reallyaus$Age, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
FunctionPrimingGender <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$Gender)) 
FunctionPrimingConversationType <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$ConversationType))
FunctionPrimingAudienceSize <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$AudienceSize)) 
FunctionPrimingFreq <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$Freq)) 
FunctionPrimingGradabilty <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$Gradabilty)) 
FunctionPrimingSemanticCategory <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$SemanticCategory))
FunctionPrimingEmotionality <- min(ftable(reallyaus$Function, reallyaus$Priming, reallyaus$Emotionality)) 
FunctionGenderConversationType <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$ConversationType))
FunctionGenderAudienceSize <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$AudienceSize)) 
FunctionGenderFreq <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$Freq)) 
FunctionGenderGradabilty <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$Gradabilty)) 
FunctionGenderSemanticCategory <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$SemanticCategory))
FunctionGenderEmotionality <- min(ftable(reallyaus$Function, reallyaus$Gender, reallyaus$Emotionality)) 
FunctionConversationTypeAudienceSize <- min(ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$AudienceSize)) 
FunctionConversationTypeFreq <- min(ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$Freq)) 
FunctionConversationTypeGradabilty <- min(ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$Gradabilty)) 
FunctionConversationTypeSemanticCategory <- min(ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$SemanticCategory))
FunctionConversationTypeEmotionality <- min(ftable(reallyaus$Function, reallyaus$ConversationType, reallyaus$Emotionality)) 
FunctionAudienceSizeFreq <- min(ftable(reallyaus$Function, reallyaus$AudienceSize, reallyaus$Freq)) 
FunctionAudienceSizeGradabilty <- min(ftable(reallyaus$Function, reallyaus$AudienceSize, reallyaus$Gradabilty)) 
FunctionAudienceSizeSemanticCategory <- min(ftable(reallyaus$Function, reallyaus$AudienceSize, reallyaus$SemanticCategory))
FunctionAudienceSizeEmotionality <- min(ftable(reallyaus$Function, reallyaus$AudienceSize, reallyaus$Emotionality)) 
FunctionFreqGradabilty <- min(ftable(reallyaus$Function, reallyaus$Freq, reallyaus$Gradabilty)) 
FunctionFreqSemanticCategory <- min(ftable(reallyaus$Function, reallyaus$Freq, reallyaus$SemanticCategory))
FunctionFreqEmotionality <- min(ftable(reallyaus$Function, reallyaus$Freq, reallyaus$Emotionality)) 
FunctionGradabiltySemanticCategory <- min(ftable(reallyaus$Function, reallyaus$Gradabilty, reallyaus$SemanticCategory))
FunctionGradabiltyEmotionality <- min(ftable(reallyaus$Function, reallyaus$Gradabilty, reallyaus$Emotionality)) 
FunctionSemanticCategoryEmotionality <- min(ftable(reallyaus$Function, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
PrimingGenderConversationType <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$ConversationType))
PrimingGenderAudienceSize <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$AudienceSize)) 
PrimingGenderFreq <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$Freq)) 
PrimingGenderGradabilty <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$Gradabilty)) 
PrimingGenderSemanticCategory <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$SemanticCategory))
PrimingGenderEmotionality <- min(ftable(reallyaus$Priming, reallyaus$Gender, reallyaus$Emotionality)) 
PrimingConversationTypeAudienceSize <- min(ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$AudienceSize)) 
PrimingConversationTypeFreq <- min(ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$Freq)) 
PrimingConversationTypeGradabilty <- min(ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$Gradabilty)) 
PrimingConversationTypeSemanticCategory <- min(ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$SemanticCategory))
PrimingConversationTypeEmotionality <- min(ftable(reallyaus$Priming, reallyaus$ConversationType, reallyaus$Emotionality)) 
PrimingAudienceSizeFreq <- min(ftable(reallyaus$Priming, reallyaus$AudienceSize, reallyaus$Freq)) 
PrimingAudienceSizeGradabilty <- min(ftable(reallyaus$Priming, reallyaus$AudienceSize, reallyaus$Gradabilty)) 
PrimingAudienceSizeSemanticCategory <- min(ftable(reallyaus$Priming, reallyaus$AudienceSize, reallyaus$SemanticCategory))
PrimingAudienceSizeEmotionality <- min(ftable(reallyaus$Priming, reallyaus$AudienceSize, reallyaus$Emotionality)) 
PrimingFreqGradabilty <- min(ftable(reallyaus$Priming, reallyaus$Freq, reallyaus$Gradabilty)) 
PrimingFreqSemanticCategory <- min(ftable(reallyaus$Priming, reallyaus$Freq, reallyaus$SemanticCategory))
PrimingFreqEmotionality <- min(ftable(reallyaus$Priming, reallyaus$Freq, reallyaus$Emotionality)) 
PrimingGradabiltySemanticCategory <- min(ftable(reallyaus$Priming, reallyaus$Gradabilty, reallyaus$SemanticCategory))
PrimingGradabiltyEmotionality <- min(ftable(reallyaus$Priming, reallyaus$Gradabilty, reallyaus$Emotionality)) 
PrimingSemanticCategoryEmotionality <- min(ftable(reallyaus$Priming, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
GenderConversationTypeAudienceSize <- min(ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$AudienceSize)) 
GenderConversationTypeFreq <- min(ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$Freq)) 
GenderConversationTypeGradabilty <- min(ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$Gradabilty)) 
GenderConversationTypeSemanticCategory <- min(ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$SemanticCategory))
GenderConversationTypeEmotionality <- min(ftable(reallyaus$Gender, reallyaus$ConversationType, reallyaus$Emotionality)) 
GenderAudienceSizeFreq <- min(ftable(reallyaus$Gender, reallyaus$AudienceSize, reallyaus$Freq)) 
GenderAudienceSizeGradabilty <- min(ftable(reallyaus$Gender, reallyaus$AudienceSize, reallyaus$Gradabilty)) 
GenderAudienceSizeSemanticCategory <- min(ftable(reallyaus$Gender, reallyaus$AudienceSize, reallyaus$SemanticCategory))
GenderAudienceSizeEmotionality <- min(ftable(reallyaus$Gender, reallyaus$AudienceSize, reallyaus$Emotionality)) 
GenderFreqGradabilty <- min(ftable(reallyaus$Gender, reallyaus$Freq, reallyaus$Gradabilty)) 
GenderFreqSemanticCategory <- min(ftable(reallyaus$Gender, reallyaus$Freq, reallyaus$SemanticCategory))
GenderFreqEmotionality <- min(ftable(reallyaus$Gender, reallyaus$Freq, reallyaus$Emotionality)) 
GenderGradabiltySemanticCategory <- min(ftable(reallyaus$Gender, reallyaus$Gradabilty, reallyaus$SemanticCategory))
GenderGradabiltyEmotionality <- min(ftable(reallyaus$Gender, reallyaus$Gradabilty, reallyaus$Emotionality)) 
GenderSemanticCategoryEmotionality <- min(ftable(reallyaus$Gender, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
ConversationTypeAudienceSizeFreq <- min(ftable(reallyaus$ConversationType, reallyaus$AudienceSize, reallyaus$Freq)) 
ConversationTypeAudienceSizeGradabilty <- min(ftable(reallyaus$ConversationType, reallyaus$AudienceSize, reallyaus$Gradabilty)) 
ConversationTypeAudienceSizeSemanticCategory <- min(ftable(reallyaus$ConversationType, reallyaus$AudienceSize, reallyaus$SemanticCategory))
ConversationTypeAudienceSizeEmotionality <- min(ftable(reallyaus$ConversationType, reallyaus$AudienceSize, reallyaus$Emotionality)) 
ConversationTypeFreqGradabilty <- min(ftable(reallyaus$ConversationType, reallyaus$Freq, reallyaus$Gradabilty)) 
ConversationTypeFreqSemanticCategory <- min(ftable(reallyaus$ConversationType, reallyaus$Freq, reallyaus$SemanticCategory))
ConversationTypeFreqEmotionality <- min(ftable(reallyaus$ConversationType, reallyaus$Freq, reallyaus$Emotionality)) 
ConversationTypeGradabiltySemanticCategory <- min(ftable(reallyaus$ConversationType, reallyaus$Gradabilty, reallyaus$SemanticCategory))
ConversationTypeGradabiltyEmotionality <- min(ftable(reallyaus$ConversationType, reallyaus$Gradabilty, reallyaus$Emotionality)) 
ConversationTypeSemanticCategoryEmotionality <- min(ftable(reallyaus$ConversationType, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
AudienceSizeFreqGradabilty <- min(ftable(reallyaus$AudienceSize, reallyaus$Freq, reallyaus$Gradabilty)) 
AudienceSizeFreqSemanticCategory <- min(ftable(reallyaus$AudienceSize, reallyaus$Freq, reallyaus$SemanticCategory))
AudienceSizeFreqEmotionality <- min(ftable(reallyaus$AudienceSize, reallyaus$Freq, reallyaus$Emotionality)) 
AudienceSizeGradabiltySemanticCategory <- min(ftable(reallyaus$AudienceSize, reallyaus$Gradabilty, reallyaus$SemanticCategory))
AudienceSizeGradabiltyEmotionality <- min(ftable(reallyaus$AudienceSize, reallyaus$Gradabilty, reallyaus$Emotionality)) 
AudienceSizeSemanticCategoryEmotionality <- min(ftable(reallyaus$AudienceSize, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
FreqGradabiltySemanticCategory <- min(ftable(reallyaus$Freq, reallyaus$Gradabilty, reallyaus$SemanticCategory))
FreqGradabiltyEmotionality <- min(ftable(reallyaus$Freq, reallyaus$Gradabilty, reallyaus$Emotionality)) 
FreqSemanticCategoryEmotionality <- min(ftable(reallyaus$Freq, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
GradabiltySemanticCategoryEmotionality <- min(ftable(reallyaus$Gradabilty, reallyaus$SemanticCategory, reallyaus$Emotionality)) 
# test which interactions are possible
testpos3intact <- c(AgeFunctionPriming, AgeFunctionGender, AgeFunctionConversationType, AgeFunctionAudienceSize, 
                    AgeFunctionFreq, AgeFunctionGradabilty, AgeFunctionSemanticCategory, AgeFunctionEmotionality, 
                    AgePrimingGender, AgePrimingConversationType, AgePrimingAudienceSize, AgePrimingFreq, 
                    AgePrimingGradabilty, AgePrimingSemanticCategory, AgePrimingEmotionality, AgeGenderConversationType, 
                    AgeGenderAudienceSize, AgeGenderFreq, AgeGenderGradabilty, AgeGenderSemanticCategory, 
                    AgeGenderEmotionality, AgeConversationTypeAudienceSize, AgeConversationTypeFreq, 
                    AgeConversationTypeGradabilty, AgeConversationTypeSemanticCategory, AgeConversationTypeEmotionality, 
                    AgeAudienceSizeFreq, AgeAudienceSizeGradabilty, AgeAudienceSizeSemanticCategory, 
                    AgeAudienceSizeEmotionality, AgeFreqGradabilty, AgeFreqSemanticCategory, AgeFreqEmotionality, 
                    AgeGradabiltySemanticCategory, AgeGradabiltyEmotionality, AgeSemanticCategoryEmotionality, 
                    FunctionPrimingGender, FunctionPrimingConversationType, FunctionPrimingAudienceSize, 
                    FunctionPrimingFreq, FunctionPrimingGradabilty, FunctionPrimingSemanticCategory, 
                    FunctionPrimingEmotionality, FunctionGenderConversationType, FunctionGenderAudienceSize, 
                    FunctionGenderFreq, FunctionGenderGradabilty, FunctionGenderSemanticCategory, 
                    FunctionGenderEmotionality, FunctionConversationTypeAudienceSize, FunctionConversationTypeFreq, 
                    FunctionConversationTypeGradabilty, FunctionConversationTypeSemanticCategory, 
                    FunctionConversationTypeEmotionality, FunctionAudienceSizeFreq, FunctionAudienceSizeGradabilty, 
                    FunctionAudienceSizeSemanticCategory, FunctionAudienceSizeEmotionality, FunctionFreqGradabilty, 
                    FunctionFreqSemanticCategory, FunctionFreqEmotionality, FunctionGradabiltySemanticCategory, 
                    FunctionGradabiltyEmotionality, FunctionSemanticCategoryEmotionality, PrimingGenderConversationType, 
                    PrimingGenderAudienceSize, PrimingGenderFreq, PrimingGenderGradabilty, PrimingGenderSemanticCategory, 
                    PrimingGenderEmotionality, PrimingConversationTypeAudienceSize, PrimingConversationTypeFreq, 
                    PrimingConversationTypeGradabilty, PrimingConversationTypeSemanticCategory, 
                    PrimingConversationTypeEmotionality, PrimingAudienceSizeFreq, PrimingAudienceSizeGradabilty, 
                    PrimingAudienceSizeSemanticCategory, PrimingAudienceSizeEmotionality, PrimingFreqGradabilty, 
                    PrimingFreqSemanticCategory, PrimingFreqEmotionality, PrimingGradabiltySemanticCategory, 
                    PrimingGradabiltyEmotionality, PrimingSemanticCategoryEmotionality, GenderConversationTypeAudienceSize, 
                    GenderConversationTypeFreq, GenderConversationTypeGradabilty, GenderConversationTypeSemanticCategory, 
                    GenderConversationTypeEmotionality, GenderAudienceSizeFreq, GenderAudienceSizeGradabilty, 
                    GenderAudienceSizeSemanticCategory, GenderAudienceSizeEmotionality, GenderFreqGradabilty, 
                    GenderFreqSemanticCategory, GenderFreqEmotionality, GenderGradabiltySemanticCategory, 
                    GenderGradabiltyEmotionality, GenderSemanticCategoryEmotionality, ConversationTypeAudienceSizeFreq, 
                    ConversationTypeAudienceSizeGradabilty, ConversationTypeAudienceSizeSemanticCategory, 
                    ConversationTypeAudienceSizeEmotionality, ConversationTypeFreqGradabilty, 
                    ConversationTypeFreqSemanticCategory, ConversationTypeFreqEmotionality, 
                    ConversationTypeGradabiltySemanticCategory, ConversationTypeGradabiltyEmotionality, 
                    ConversationTypeSemanticCategoryEmotionality, AudienceSizeFreqGradabilty, 
                    AudienceSizeFreqSemanticCategory, AudienceSizeFreqEmotionality, AudienceSizeGradabiltySemanticCategory, 
                    AudienceSizeGradabiltyEmotionality, AudienceSizeSemanticCategoryEmotionality, 
                    FreqGradabiltySemanticCategory, FreqGradabiltyEmotionality, FreqSemanticCategoryEmotionality, 
                    GradabiltySemanticCategoryEmotionality)
names(testpos3intact) <- c("AgeFunctionPriming", "AgeFunctionGender", "AgeFunctionConversationType", 
                           "AgeFunctionAudienceSize", "AgeFunctionFreq", "AgeFunctionGradabilty", 
                           "AgeFunctionSemanticCategory", "AgeFunctionEmotionality", "AgePrimingGender", 
                           "AgePrimingConversationType", "AgePrimingAudienceSize", "AgePrimingFreq", 
                           "AgePrimingGradabilty", "AgePrimingSemanticCategory", "AgePrimingEmotionality", 
                           "AgeGenderConversationType", "AgeGenderAudienceSize", "AgeGenderFreq", 
                           "AgeGenderGradabilty", "AgeGenderSemanticCategory", "AgeGenderEmotionality", 
                           "AgeConversationTypeAudienceSize", "AgeConversationTypeFreq", 
                           "AgeConversationTypeGradabilty", "AgeConversationTypeSemanticCategory", 
                           "AgeConversationTypeEmotionality", "AgeAudienceSizeFreq", "AgeAudienceSizeGradabilty", 
                           "AgeAudienceSizeSemanticCategory", "AgeAudienceSizeEmotionality", "AgeFreqGradabilty", 
                           "AgeFreqSemanticCategory", "AgeFreqEmotionality", "AgeGradabiltySemanticCategory", 
                           "AgeGradabiltyEmotionality", "AgeSemanticCategoryEmotionality", "FunctionPrimingGender", 
                           "FunctionPrimingConversationType", "FunctionPrimingAudienceSize", "FunctionPrimingFreq", 
                           "FunctionPrimingGradabilty", "FunctionPrimingSemanticCategory", "FunctionPrimingEmotionality", 
                           "FunctionGenderConversationType", "FunctionGenderAudienceSize", "FunctionGenderFreq", 
                           "FunctionGenderGradabilty", "FunctionGenderSemanticCategory", "FunctionGenderEmotionality", 
                           "FunctionConversationTypeAudienceSize", "FunctionConversationTypeFreq", 
                           "FunctionConversationTypeGradabilty", "FunctionConversationTypeSemanticCategory", 
                           "FunctionConversationTypeEmotionality", "FunctionAudienceSizeFreq", 
                           "FunctionAudienceSizeGradabilty", "FunctionAudienceSizeSemanticCategory", 
                           "FunctionAudienceSizeEmotionality", "FunctionFreqGradabilty", "FunctionFreqSemanticCategory", 
                           "FunctionFreqEmotionality", "FunctionGradabiltySemanticCategory", "FunctionGradabiltyEmotionality", 
                           "FunctionSemanticCategoryEmotionality", "PrimingGenderConversationType", "PrimingGenderAudienceSize", 
                           "PrimingGenderFreq", "PrimingGenderGradabilty", "PrimingGenderSemanticCategory", 
                           "PrimingGenderEmotionality", "PrimingConversationTypeAudienceSize", "PrimingConversationTypeFreq", 
                           "PrimingConversationTypeGradabilty", "PrimingConversationTypeSemanticCategory", 
                           "PrimingConversationTypeEmotionality", "PrimingAudienceSizeFreq", "PrimingAudienceSizeGradabilty", 
                           "PrimingAudienceSizeSemanticCategory", "PrimingAudienceSizeEmotionality", "PrimingFreqGradabilty", 
                           "PrimingFreqSemanticCategory", "PrimingFreqEmotionality", "PrimingGradabiltySemanticCategory", 
                           "PrimingGradabiltyEmotionality", "PrimingSemanticCategoryEmotionality", 
                           "GenderConversationTypeAudienceSize", "GenderConversationTypeFreq", 
                           "GenderConversationTypeGradabilty", "GenderConversationTypeSemanticCategory", 
                           "GenderConversationTypeEmotionality", "GenderAudienceSizeFreq", "GenderAudienceSizeGradabilty", 
                           "GenderAudienceSizeSemanticCategory", "GenderAudienceSizeEmotionality", "GenderFreqGradabilty", 
                           "GenderFreqSemanticCategory", "GenderFreqEmotionality", "GenderGradabiltySemanticCategory", 
                           "GenderGradabiltyEmotionality", "GenderSemanticCategoryEmotionality", 
                           "ConversationTypeAudienceSizeFreq", "ConversationTypeAudienceSizeGradabilty", 
                           "ConversationTypeAudienceSizeSemanticCategory", "ConversationTypeAudienceSizeEmotionality", 
                           "ConversationTypeFreqGradabilty", "ConversationTypeFreqSemanticCategory", 
                           "ConversationTypeFreqEmotionality", "ConversationTypeGradabiltySemanticCategory", 
                           "ConversationTypeGradabiltyEmotionality", "ConversationTypeSemanticCategoryEmotionality", 
                           "AudienceSizeFreqGradabilty", "AudienceSizeFreqSemanticCategory", "AudienceSizeFreqEmotionality", 
                           "AudienceSizeGradabiltySemanticCategory", "AudienceSizeGradabiltyEmotionality", 
                           "AudienceSizeSemanticCategoryEmotionality", "FreqGradabiltySemanticCategory", 
                           "FreqGradabiltyEmotionality", "FreqSemanticCategoryEmotionality", 
                           "GradabiltySemanticCategoryEmotionality")
tstintact3 <- names(testpos3intact)[which(testpos3intact >= 1)]
tstintact3; length(tstintact3)


# add AgeFunctionGender
m52.glmer <- update(m7.glmer, .~.+Age*Function*Gender) # Hessian is numerically singular

# add AgeFunctionConversationType
m53.glmer <- update(m7.glmer, .~.+Age*Function*ConversationType) # Model failed to converge'

# add AgeFunctionAudienceSize
m54.glmer <- update(m7.glmer, .~.+Age*Function*AudienceSize) # Model is nearly unidentifiable

# add AgeFunctionEmotionality
m55.glmer <- update(m7.glmer, .~.+Age*Function*Emotionality) # Model failed to converge

# add AgeGenderConversationType
m56.glmer <- update(m7.glmer, .~.+Age*Gender*ConversationType) # Model failed to converge

# add AgeGenderAudienceSize
m57.glmer <- update(m7.glmer, .~.+Age*Gender*AudienceSize) # Hessian is numerically singular

# add AgeGenderEmotionality
m58.glmer <- update(m7.glmer, .~.+Age*Gender*Emotionality) # Hessian is numerically singular

# add AgeConversationTypeAudienceSize
m59.glmer <- update(m7.glmer, .~.+Age*ConversationType*AudienceSize) # Model is nearly unidentifiable

# add AgeConversationTypeEmotionality
m60.glmer <- update(m7.glmer, .~.+Age*ConversationType*Emotionality) # Model is nearly unidentifiable

# add AgeAudienceSizeEmotionality
m61.glmer <- update(m7.glmer, .~.+Age*AudienceSize*Emotionality) # Model failed to converge

# add FunctionPrimingGender
m62.glmer <- update(m7.glmer, .~.+Function*Priming*Gender)
m62.glm <- update(m7.glm, .~.+Function*Priming*Gender)
vif(m62.glm) # VIFs ok
anova(m62.glmer, m7.glmer, test = "Chi") # not sig (p=0.2001)

# add FunctionPrimingConversationType
m63.glmer <- update(m7.glmer, .~.+Function*Priming*ConversationType)
m63.glm <- update(m7.glm, .~.+Function*Priming*ConversationType)
vif(m63.glm) # VIFs unacceptable

# add FunctionPrimingAudienceSize
m64.glmer <- update(m7.glmer, .~.+Function*Priming*AudienceSize)
m64.glm <- update(m7.glm, .~.+Function*Priming*AudienceSize)
vif(m64.glm) # VIFs unacceptable

# add FunctionPrimingEmotionality
m65.glmer <- update(m7.glmer, .~.+Function*Priming*Emotionality)
m65.glm <- update(m7.glm, .~.+Function*Priming*Emotionality)
vif(m65.glm) # VIFs unacceptable

# add FunctionGenderConversationType
m66.glmer <- update(m7.glmer, .~.+Function*Gender*ConversationType)
m66.glm <- update(m7.glm, .~.+Function*Gender*ConversationType)
vif(m66.glm) # VIFs unacceptable

# add FunctionGenderAudienceSize
m67.glmer <- update(m7.glmer, .~.+Function*Gender*AudienceSize)
m67.glm <- update(m7.glm, .~.+Function*Gender*AudienceSize)
vif(m67.glm) # VIFs unacceptable

# add FunctionGenderGradabilty
m68.glmer <- update(m7.glmer, .~.+Function*Gender*Gradabilty) # Model failed to converge

# add FunctionGenderSemanticCategory
m69.glmer <- update(m7.glmer, .~.+Function*Gender*SemanticCategory) # Model is nearly unidentifiable

# add FunctionGenderEmotionality
m70.glmer <- update(m7.glmer, .~.+Function*Gender*Emotionality)
m70.glm <- update(m7.glm, .~.+Function*Gender*Emotionality)
vif(m70.glm) # VIFs unacceptable

# add FunctionConversationTypeAudienceSize
m71.glmer <- update(m7.glmer, .~.+Function*ConversationType*AudienceSize)
m71.glm <- update(m7.glm, .~.+Function*ConversationType*AudienceSize)
vif(m71.glm) # VIFs unacceptable

# add FunctionConversationTypeGradabilty
m72.glmer <- update(m7.glmer, .~.+Function*ConversationType*Gradabilty) # Model failed to converge

# add FunctionConversationTypeEmotionality
m73.glmer <- update(m7.glmer, .~.+Function*ConversationType*Emotionality)
m73.glm <- update(m7.glm, .~.+Function*ConversationType*Emotionality)
vif(m73.glm) # VIFs unacceptable

# add FunctionAudienceSizeGradabilty
m74.glmer <- update(m7.glmer, .~.+Function*AudienceSize*Gradabilty) # Model failed to converge

# add FunctionAudienceSizeSemanticCategory
m75.glmer <- update(m7.glmer, .~.+Function*AudienceSize*SemanticCategory) # Model is nearly unidentifiable

# add FunctionAudienceSizeEmotionality
m76.glmer <- update(m7.glmer, .~.+Function*AudienceSize*Emotionality)
m76.glm <- update(m7.glm, .~.+Function*AudienceSize*Emotionality)
vif(m76.glm) # VIFs unacceptable

# add FunctionSemanticCategoryEmotionality
m77.glmer <- update(m7.glmer, .~.+Function*SemanticCategory*Emotionality) # Model is nearly unidentifiable

# add PrimingGenderConversationType
m78.glmer <- update(m7.glmer, .~.+Priming*Gender*ConversationType) # Hessian is numerically singular

# add PrimingGenderAudienceSize
m79.glmer <- update(m7.glmer, .~.+Priming*Gender*AudienceSize)
m79.glm <- update(m7.glm, .~.+Priming*Gender*AudienceSize)
vif(m79.glm) # VIFs unacceptable

# add PrimingGenderSemanticCategory
m80.glmer <- update(m7.glmer, .~.+Priming*Gender*SemanticCategory) # Model is nearly unidentifiable

# add PrimingGenderEmotionality
m81.glmer <- update(m7.glmer, .~.+Priming*Gender*Emotionality)
m81.glm <- update(m7.glm, .~.+Priming*Gender*Emotionality)
vif(m81.glm) # VIFs unacceptable

# add PrimingConversationTypeAudienceSize
m82.glmer <- update(m7.glmer, .~.+Priming*ConversationType*AudienceSize)
m82.glm <- update(m7.glm, .~.+Priming*ConversationType*AudienceSize)
vif(m82.glm) # VIFs unacceptable

# add PrimingConversationTypeEmotionality
m83.glmer <- update(m7.glmer, .~.+Priming*ConversationType*Emotionality)
m83.glm <- update(m7.glm, .~.+Priming*ConversationType*Emotionality)
vif(m83.glm) # VIFs unacceptable

# add PrimingAudienceSizeEmotionality
m84.glmer <- update(m7.glmer, .~.+Priming*AudienceSize*Emotionality)
m84.glm <- update(m7.glm, .~.+Priming*AudienceSize*Emotionality)
vif(m84.glm) # VIFs unacceptable

# add GenderConversationTypeAudienceSize
m85.glmer <- update(m7.glmer, .~.+Gender*ConversationType*AudienceSize)
m85.glm <- update(m7.glm, .~.+Gender*ConversationType*AudienceSize)
vif(m85.glm) # VIFs unacceptable

# add GenderConversationTypeGradabilty
m86.glmer <- update(m7.glmer, .~.+Gender*ConversationType*Gradabilty) # Model failed to converge

# add GenderConversationTypeSemanticCategory
m87.glmer <- update(m7.glmer, .~.+Gender*ConversationType*SemanticCategory) # Model failed to converge

# add GenderConversationTypeEmotionality
m88.glmer <- update(m7.glmer, .~.+Gender*ConversationType*Emotionality)
m88.glm <- update(m7.glm, .~.+Gender*ConversationType*Emotionality)
vif(m88.glm) # VIFs unacceptable  

# add GenderAudienceSizeGradabilty
m89.glmer <- update(m7.glmer, .~.+Gender*AudienceSize*Gradabilty) #  Model is nearly unidentifiable

# add GenderAudienceSizeSemanticCategory
m90.glmer <- update(m7.glmer, .~.+Gender*AudienceSize*SemanticCategory) # Model failed to converge

# add GenderAudienceSizeEmotionality
m91.glmer <- update(m7.glmer, .~.+Gender*AudienceSize*Emotionality)
m91.glm <- update(m7.glm, .~.+Gender*AudienceSize*Emotionality)
vif(m91.glm) # VIFs unacceptable

# add ConversationTypeAudienceSizeGradabilty
m92.glmer <- update(m7.glmer, .~.+ConversationType*AudienceSize*Gradabilty) # Model failed to converge

# add ConversationTypeAudienceSizeSemanticCategory
m93.glmer <- update(m7.glmer, .~.+ConversationType*AudienceSize*SemanticCategory) # Hessian is numerically singular

# add ConversationTypeAudienceSizeEmotionality
m94.glmer <- update(m7.glmer, .~.+ConversationType*AudienceSize*Emotionality)
m94.glm <- update(m7.glm, .~.+ConversationType*AudienceSize*Emotionality)
vif(m94.glm) # VIFs unacceptable

# add ConversationTypeGradabiltyEmotionality
m95.glmer <- update(m7.glmer, .~.+ConversationType*Gradabilty*Emotionality) # Model failed to converge

# add ConversationTypeSemanticCategoryEmotionality
m96.glmer <- update(m7.glmer, .~.+ConversationType*SemanticCategory*Emotionality) #  Model failed to converge

# add AudienceSizeGradabiltyEmotionality
m97.glmer <- update(m7.glmer, .~.+AudienceSize*Gradabilty*Emotionality) # Model is nearly unidentifiable

#########################################
# load function for regression table summary
source("D:\\R/meblr.summary.R")
# set up summary table
meblrm_ampaus <- meblrm.summary(m0.glm, m7.glm, m0.glmer, m7.glmer, reallyaus$really) #
meblrm_ampaus

# save results to disc
write.table(meblrm_ampaus, "meblrm_ampaus.txt", sep="\t")

# load function
library(car)
meblrm_ampaus_Anova <- Anova(m7.glmer, type = "III", test = "Chi")
meblrm_ampaus_Anova

# save results to disc
write.table(meblrm_ampaus_Anova, "meblrm_ampaus_Anova.txt", sep="\t")

effectgender <- anova(m0.glmer, m4.glmer, test = "Chi")

effectfrequency <- anova(m4.glmer, m7.glmer, test = "Chi")

# use customized model comparison function
# create comparireallyns
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi")
m2.m0 <- anova(m2.glmer, m0.glmer, test = "Chi")
m3.m0 <- anova(m3.glmer, m0.glmer, test = "Chi")
m4.m0 <- anova(m4.glmer, m0.glmer, test = "Chi")
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi")
m6.m4 <- anova(m6.glmer, m4.glmer, test = "Chi")
m7.m4 <- anova(m7.glmer, m4.glmer, test = "Chi")
m8.m7 <- anova(m8.glmer, m7.glmer, test = "Chi")
m9.m7 <- anova(m9.glmer, m7.glmer, test = "Chi")
m10.m7 <- anova(m10.glmer, m7.glmer, test = "Chi")
m11.m7 <- anova(m11.glmer, m7.glmer, test = "Chi") # SIG (p=0.0133) BUT BIC inflated! 
m12.m7 <- anova(m12.glmer, m7.glmer, test = "Chi") # not sig (p=0.4088)
m13.m7 <- anova(m13.glmer, m7.glmer, test = "Chi") # not sig (p=0.1048)
m14.m7 <- anova(m14.glmer, m7.glmer, test = "Chi") # not sig (p=0.3414)
m15.m7 <- anova(m15.glmer, m7.glmer, test = "Chi") # not sig (p=0.3655)
m17.m7 <- anova(m17.glmer, m7.glmer, test = "Chi") # mar sig (p=0.4289)
m18.m7 <- anova(m18.glmer, m7.glmer, test = "Chi") # not sig (p=0.1026)
m19.m7 <- anova(m19.glmer, m7.glmer, test = "Chi") # mar sig (p=0.05836)
m20.m7 <- anova(m20.glmer, m7.glmer, test = "Chi") # not sig (p=0.07207)
m21.m7 <- anova(m21.glmer, m7.glmer, test = "Chi") # SIG (p=0.01019)
m24.m7 <- anova(m24.glmer, m7.glmer, test = "Chi") # not sig (p=0.1716)
m25.m7 <- anova(m25.glmer, m7.glmer, test = "Chi") # SIG (p=0.04956) BUT BIC inflated! 
m26.m7 <- anova(m26.glmer, m7.glmer, test = "Chi") # not sig (p=0.2124)
m27.m7 <- anova(m27.glmer, m7.glmer, test = "Chi") # not sig (p=0.3574)
m31.m7 <- anova(m31.glmer, m7.glmer, test = "Chi") # mar sig (p=0.07753) BUT BIC INFLATION!
m32.m7 <- anova(m32.glmer, m7.glmer, test = "Chi") # not sig (p=0.3034)
m33.m7 <- anova(m33.glmer, m7.glmer, test = "Chi") # not sig (p=0.3343)
m37.m7 <- anova(m37.glmer, m7.glmer, test = "Chi") # SIG (p=0.01695) BUT BIC INFLATION!
m38.m7 <- anova(m38.glmer, m7.glmer, test = "Chi") # SIG (p=0.03645) BUT BIC INFLATION!
m42.m7 <- anova(m42.glmer, m7.glmer, test = "Chi") # mar sig (p=0.1233)
m45.m7 <- anova(m45.glmer, m7.glmer, test = "Chi") # not sig (p=0.1415)


# create a list of the model comparireallyns
mdlcmp <- list(m1.m0, m2.m0, m3.m0, m4.m0, m5.m4, m6.m4, m7.m4, m8.m7, m9.m7, m10.m7, 
               m11.m7, m12.m7, m13.m7, m14.m7, m15.m7, m17.m7, m18.m7, m19.m7, 
               m20.m7, m21.m7,  m24.m7, m25.m7, m26.m7, m27.m7,  m31.m7, 
               m32.m7, m33.m7,  m37.m7, m38.m7,  m42.m7, m45.m7)
# load function
source("D:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
# apply function
mdl.cmp.glmersc.swsu.dm <- mdl.fttng.swsu(mdlcmp)
# inspect output
mdl.cmp.glmersc.swsu.dm

write.table(mdl.cmp.glmersc.swsu.dm, "mdl_cmp_glmersc_swsu_reallyaus.txt", sep="\t")
###########################################################
# Post-hoc analysis
library (multcomp)
summary(glht(m7.glmer, mcp(Gender="Tukey")))

################################################################
#                 IMPORTANT OBJECTS
################################################################
# inspect very important objects
head(reallyaus)

# glmer
effectgender

effectfrequency

meblrm_ampaus

meblrm_ampaus_Anova

###############################################################
###              END PART 4
###############################################################

