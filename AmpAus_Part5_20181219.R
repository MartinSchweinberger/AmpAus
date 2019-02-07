##################################################################
# Titel:      The Amplifier System of Australian English - Part 5
# R version:  3.4.1 (2017-06-30) -- "Single Candle"
# Autor:      Martin Schweinberger
# Date:       2018-11-06
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. The Amplifier System of Australian English, Part 5,
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpAusE")
# load libraries
library(Rling)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.prAmplified=10000)
# define imAge dausctors
imAgeDirectory<-"images"
###############################################################
# load data
reallyaus <- read.table("ampaus05_statz.txt", sep = "\t", header = T)
# inspect data
str(reallyaus)

# remove superfluous columns
reallyaus$FileSpeaker <- NULL
reallyaus$Occupation <- NULL
reallyaus$very <- NULL
# inspect colnames
colnames(reallyaus)

# define vector for data inspection
clfct <- c("Age", "Adjective", "Function", "Priming", "Gender", "ConversationType", 
          "AudienceSize", "really", "Gradabilty", "SemanticCategory", "Emotionality")
# factorize data
reallyaus[clfct] <- lapply(reallyaus[clfct], factor)
# inspect data
str(reallyaus)

################################################################
#               CONDITION INFERENCE TREES
library(partykit)
# create data
citd <- reallyaus
# set.seed
set.seed(111) 
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*14))
# create initial conditional inference tree model
citd.ctree <- ctree(really ~ Age + Adjective + Function + Priming + Gender + 
                      ConversationType + AudienceSize +  Freq + 
                      Gradabilty + SemanticCategory + Emotionality,
                    data = citd)
# plot final ctree
png("images/final_ctree.png",  width = 680, height = 480) 
plot(citd.ctree, gp = gpar(fontsize = 8))
dev.off()
# test prediction accuracy
ptb <- table(predict(citd.ctree), citd$really)
(((ptb[1]+ptb[4])+(ptb[2]+ptb[3]))/sum(table(predict(citd.ctree), citd$really)))*100
##100

# determine baseline
(table(citd$really)[[2]]/sum(table(citd$really)))*100
## 41.08

###############################################################
#                   RANDOM FOREST I
# prepare data
rfd <- reallyaus
# convert really into a factor
rfd$really <- as.factor(rfd$really)
# start with random forest
# set seed
set.seed(222)
# partition data for evaluating rf 
id <- sample(2, nrow(rfd), replace = T, prob = c(.7, .3))
train <- rfd[id == 1, ]
test <- rfd[id == 2,]
# load library
library(randomForest)
# create initial model
reallyaus_rf1 <- randomForest(really~., data = train)
# inspect model
print(reallyaus_rf1)

# inspect attibutes
attributes(reallyaus_rf1)

# start model evaluation
# install package
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                      "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)

# load caret library
library(caret) # because initially caret did not work, the libraries above had to be installed
# extract prediction for training data
ptrain1 <- predict(reallyaus_rf1, train)
# inspect predictions
head(ptrain1); head(train$really)

# create confusionMatrix
confusionMatrix(ptrain1, train$really)

# extract prediction for test data
ptest1 <- predict(reallyaus_rf1, test)
# create confusionMatrix
confusionMatrix(ptest1, test$really)

# determine errorrate of random forest model
plot(reallyaus_rf1, main = "")

# tune model
reallyaus_rf2 <- tuneRF(train[, !colnames(train)== "really"], train[, colnames(train)== "really"], 
                        stepFactor = .5, # for most values 6 appears to be optimal
                        plot = T,
                        ntreeTry = 200,
                        trace = T,
                        improve = .05
)
# create improved model
reallyaus_rf2 <- randomForest(really~., data = train, 
                              ntree = 200,
                              ntry = 6,
                              importance= T,
                              proximity = T)
# inspect model
print(reallyaus_rf2)

# predict based on improved model
ptrain2 <- predict(reallyaus_rf2, train)
# create confusionMatrix
confusionMatrix(ptrain2, train$really)

# extract prediction for test data
ptest2 <- predict(reallyaus_rf2, test)
# create confusionMatrix
confusionMatrix(ptest2, test$really)

# inspect number of nodes for trees
hist(treesize(reallyaus_rf2), main = "", col = "lightgray")

# check variable importance
png("images/RandomForestVariableImportance.png",  width = 680, height = 480) 
varImpPlot(reallyaus_rf2, main = "", pch = 20) 
dev.off()
# left plot (Accuracy): how much accuracy decreases if factor is left out
# left plot (Gini/Pureness): how much more unpure (ambigious) the distributions become if fector is left out
# extract variable importance values
importance(reallyaus_rf2)

#which variables have been used in the trees
varUsed(reallyaus_rf2)

# partial dependence plot
partialPlot(reallyaus_rf2, train, Freq, 1)

partialPlot(reallyaus_rf2, train, ConversationType, 1)

partialPlot(reallyaus_rf2, train, Function, 1)

partialPlot(reallyaus_rf2, train, SemanticCategory, 1)

partialPlot(reallyaus_rf2, train, Gender, 1)


# extract tree
getTree(reallyaus_rf2, 1, labelVar = T)

# mds plot
MDSplot(reallyaus_rf2, test$really)

###############################################################
#                   RANDOM FOREST II
# detach partykit
detach("package:partykit", unload=TRUE)
# load package party
library(party)
# prepare data
rfd <- reallyaus
# set seed
set.seed(333)

# create initial model
reallyaus.rf <- cforest(really ~ Age + Adjective + Function + Priming + Gender + 
                          ConversationType + AudienceSize +  Freq + 
                          Gradabilty + SemanticCategory + Emotionality,
                        data = rfd, controls = cforest_unbiased(ntree = 50, mtry = 3))
# determine importance of factors
reallyaus.varimp <- varimp(reallyaus.rf, conditional = T)
round(reallyaus.varimp, 3)

# plot result
png("images/RandemForest2FactorImportance.png",  width = 680, height = 480) 
dotchart(sort(reallyaus.varimp), pch = 20, main = "Conditional importance of variables")
dev.off()
# load library
library(Hmisc)
# evaluate random forst
reallyaus.rf.pred <- unlist(treeresponse(reallyaus.rf))[c(FALSE,TRUE)]
somers2(reallyaus.rf.pred, as.numeric(rfd$really) - 1)
##     C         Dxy           n     Missing 
##0.8119422   0.6238843 314.0000000   0.0000000 
###############################################################
#                     RANDOM FOREST III
# load library
library(party)
# create data
randomforestdata <- reallyaus

cf1 <- cforest(really ~ . , data= randomforestdata, control=cforest_unbiased(mtry=2,ntree=100)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy

varimp(cf1, conditional=TRUE) # conditional=True, adjusts for correlations between predict

varimpAUC(cf1)  # more robust towards class imbalance.
png("images/RandemForest3FactorImportance.png",  width = 680, height = 480) 
par(mar = c(5, 8, 4, 2) + 0.1)
plot(y = 1:length(varimpAUC(cf1)), x = varimpAUC(cf1)[order(varimpAUC(cf1))], 
     axes = F, ann = F, pch = 20, xlim = c(-0.01, 0.05), main = "Predictor Importance")
axis(1, at = seq(-0.01, 0.05, 0.005), seq(-0.01, 0.05, 0.005))
axis(2, at = 1:length(varimpAUC(cf1)), names(varimpAUC(cf1))[order(varimpAUC(cf1))], las = 2)
grid()
box()
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
###############################################################
#                  BORUTA
# load library
library(Boruta)
# create dada for boruta
borutadata <- reallyaus
# run 1
boruta.ampaus <- Boruta(really~.,data=borutadata)
print(boruta.ampaus)

getConfirmedFormula(boruta.ampaus)

png("images/BorutaAmpaus1.png",  width = 1500, height = 300)
plot(boruta.ampaus, cex = .75)
dev.off()
plot(boruta.ampaus)

png("images/BorutaAmpausHistory1.png",  width = 680, height = 480)
plotImpHistory(boruta.ampaus)
dev.off()
plotImpHistory(boruta.ampaus)

# remove superfluous variables
borutadata$Emotionality <- NULL
borutadata$Priming <- NULL
borutadata$Age <- NULL
borutadata$SemanticCategory <- NULL
# run2
boruta.ampaus <- Boruta(really~.,data=borutadata)
print(boruta.ampaus)

getConfirmedFormula(boruta.ampaus)

png("images/BorutaAmpaus2.png",  width = 1200, height = 300)
plot(boruta.ampaus, cex = .75)
dev.off()
plot(boruta.ampaus)

png("images/BorutaAmpausHistory2.png",  width = 680, height = 480)
plotImpHistory(boruta.ampaus)
dev.off()
plotImpHistory(boruta.ampaus)

getConfirmedFormula(boruta.ampaus)
png("images/BorutaAmpaus.png",  width = 1500, height = 750)
par(mar = c(18, 8, 4, 2) + 0.1)
plot(boruta.ampaus, cex.axis=2, las=2, xlab="", ylab = "", cex = 2, 
     col = c("grey50", "grey50", "grey50",  "grey50", "grey50", "grey50", "grey50","grey90","grey90","grey90"))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 16, at = 7, cex = 3)
mtext("Control", 1, line = 16, at = 2, cex = 3)
mtext("Importance", 2, line = 2.5, at = 2.5, cex = 3, las = 0)
dev.off()
par(mar = c(5, 4, 4, 2) + 0.1)
plot(boruta.ampaus)

png("images/BorutaAmpausHistory.png",  width = 680, height = 480)
plotImpHistory(boruta.ampaus)
dev.off()
plotImpHistory(boruta.ampaus)

###############################################################
#                   END PART 5
###############################################################

