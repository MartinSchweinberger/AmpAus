##################################################################
# Titel:      The Amplifier System of Australian English - Part 3
# R version:  3.4.1 (2017-06-30) -- "Single Candle"
# Autor:      Martin Schweinberger
# Date:       2018-11-06
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. "The Amplifier System of Australian English, Part 3",
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
ampaus <- read.table("ampaus04_clean.txt", sep = "\t", header = T)
# inspect data
str(ampaus)

# recode text type
ampaus$Genre <- factor(ampaus$Genre, levels = c("PrivateDialogue", "PublicDialogue",
                                                "UnscriptedMonologue", "ScriptedMonologue"))
# factorize variables
clfct <- c("Priming","Emotionality", "Gender", "AgeOriginalClassification", "nationality", "birthplace", 
           "Education", "Occupation", "L1", "ConversationType", "AudienceSize", 
           "Gradabilty", "SemanticCategory")
ampaus[clfct] <- lapply(ampaus[clfct], factor)
# check if variable levels need to be collapsed (only PrivateDialogue data)
tstdt <- ampaus[ampaus$Genre == "PrivateDialogue",]
FileSpeakerAgetfb <- ftable(tstdt$FileSpeaker, tstdt$Age)
FileSpeakerAgetfb <- apply(FileSpeakerAgetfb, 1, function(x) ifelse(x > 1, 1, x)) 
rowSums(FileSpeakerAgetfb)

# recode age
ampaus$Age <- ifelse(ampaus$Age == "26-30", "26-40",
                     ifelse(ampaus$Age == "31-40", "26-40",
                            ifelse(ampaus$Age == "41-50", "41-80",
                                   ifelse(ampaus$Age == "51-80", "41-80", ampaus$Age))))
###############################################################
#              SEMANTIC VECTOR SPACE MODEL 1
# tabulate data
t1 <- tapply(ampaus$Amplified, list(ampaus$Adjective, ampaus$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3aus <- t3
t3aus <- t3aus[, 2: ncol(t3aus)]
# remove Adjectives that were not Amplifiedensified
t3aus <- t3aus[rowSums(t3aus) > 0, ]
# save row and column names
colnamesaus <- colnames(t3aus)
rownamesaus <- rownames(t3aus)
# turn dataframe Amplifiedo matrix
svsmaus <- as.matrix(t3aus)
# convert token frequency to type frequency
#svsmaus <- apply(svsmaus, 1, Function(x) { x <- ifelse(x > 1, 1, x) } )
svsmaus <- t(svsmaus)
#svsmaus <- svsmaus[, colSums(svsmaus) >= 2]
#svsmaus <- svsmaus[rowSums(svsmaus) >= 2, ]
svsmaus

# compute expected values
svsmaus.exp <- chisq.test(svsmaus)$expected
# calculate PMI and PPMI
svsmaus.PMI <- log2(svsmaus/svsmaus.exp)
svsmaus.PPMI <- ifelse(svsmaus.PMI < 0, 0, svsmaus.PMI)
# calculate cosine similarity
svsmaus.tmp1 <- svsmaus.PPMI
svsmaus.cos <- cossim(svsmaus.tmp1)
#round(svsmaus.cos, 2)
###############################################################
#               CLUSTER SEMANTIC VECTORS
# load library
library(cluster)
# find max value that is not 1
svsmaus.cos.test <- apply(svsmaus.cos, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(svsmaus.cos.test)
# create distance matrix
svsmaus.dist <- 1 - (svsmaus.cos/maxval)
clustd <- as.dist(svsmaus.dist)
# create distance matrix
clustd <- dist(svsmaus.cos, method = "manhattan") 
# alternative methods
# eucledian - not good when dealing with many dimensions
# manhattan - most popular choice
# method - here the difference between poAmplifieds dominates
# canberra - for count data
# binary - for binary data only!
# minkowski - is not a true distance measure

# find optimal number of clusters
asw <- as.vector(unlist(sapply(2:nrow(svsmaus)-1, function(x) pam(clustd, k = x)$silinfo$avg.width)))
# determine the optimal number of clusters (max width is optimal)
optclust <- which(asw == max(asw))+1 # optimal number of clusters

# inspect clustering with optimal number of clusters
svsmaus.clust <- pam(clustd, optclust)
svsmaus.clust$clustering

# create cluster object
# alternative methods: "single", "ward.D2", "averAge", "mcquitty", "median", "centroid"
ampaushclust <- hclust(clustd, method="ward.D")    
# plot cluster solution
png("images/Clustaus.png",  width = 680, height = 480) # save plot
plot(ampaushclust, main = "", xlab = "", ylab = "")
rect.hclust(ampaushclust, k = optclust, border= "orange")
dev.off()
# load libraries for nicer dendrograms
library(factoextra)
library(dendextend)
# plot with colored clusters
opar <- par(mar = c(5, 4, 4, 2) + 0.1)      # make a copy of current settings
npar <- par(mar = c(3, 1, 1, 20))
png("images/Clustaus2.png",  width = 400, height = 800) # save plot
npar <- par(mar = c(3, 1, 1, 20))
# plot with colored clusters
fviz_dend(ampaushclust, k = optclust, cex = 2, horiz = T,  type = "rectangle",
          k_colors = c("grey60", "grey60", "grey60", "grey20"), 
          rect_border = "white",#c("orange", "orange", "orange", "grey30"), 
          rect_fill = F, main = "", labels_track_height=10, rect = T, margin = c(5,20))
dev.off()
par(opar)          # restore original settings 
# plot as unrooted tree
png("images/PhyClustAmpaus.png",  width = 680, height = 480) 
fviz_dend(ampaushclust, k = optclust, color_labels_by_k = T, type = "phylogenic", repel = TRUE, cex = .9,
          k_colors = c("grey90", "grey70", "grey50", "grey30"))
dev.off()
###############################################################
# Unrooted clustering
# library ape
library(ape)
# convert 'hclust' to 'phylo' object
phylo_tree = as.phylo(ampaushclust)
# get edges
graph_edges = phylo_tree$edge
# library igraph
library(igraph)
# get graph from edge list
graph_net = graph.edgelist(graph_edges)
# extract layout (x-y coords)
graph_layout = layout.auto(graph_net)
# number of observations
nobs = nrow(svsmaus.cos)
# save plot
png("images/UClustAmpaus.png",  width = 680, height = 480) 
# start plot
plot(graph_layout[,1], graph_layout[,2], type = "n", axes = FALSE,
     xlab = "", ylab = "")
# draw tree branches
segments(
  x0 = graph_layout[graph_edges[,1],1], 
  y0 = graph_layout[graph_edges[,1],2],
  x1 = graph_layout[graph_edges[,2],1],
  y1 = graph_layout[graph_edges[,2],2],
  col = "gray90", lwd = 2
)
# add labels
text(graph_layout[1:nobs,1], graph_layout[1:nobs,2],
     phylo_tree$tip.label, cex = .9, xpd = TRUE, font = 1)
dev.off()
###############################################################
#                 WARNING
#             DATA REDUCTION
# exclude amplifiers that are very dissimilar to main group of amplifiers
rmvamp <- c("totally", "completely", "much", "extremely", "total", "real", "significant")
nrow(ampaus)

ampaus <- ampaus[!ampaus$Variant %in% rmvamp, ]
nrow(ampaus)

###############################################################
# prepare data for plotting
# create data frame with relevant variables
pd <- data.frame(ampaus$Age, ampaus$Genre, ampaus$Function, ampaus$Amplified, ampaus$Variant)
# clean col names
colnames(pd) <- gsub("ampaus.", "", colnames(pd))
# convert Age column
Agelbs <- names(table(pd$Age))
pd$Age <- ifelse(pd$Age == "17-25", 3,
                 ifelse(pd$Age == "26-40", 2, 
                        ifelse(pd$Age == "41-80", 1, pd$Age)))
# multiply Amplified * 100 to get percent for Variant
pd$Amplified <- ifelse(pd$Amplified == 1, 100, 0)
# convert Age and Amplified Amplifiedo a numeric variables
clnm <- c("Age", "Amplified")
pd[clnm] <- lapply(pd[clnm], as.numeric)
famps <- names(table(pd$Variant))[which(table(pd$Variant) > 20)]
# reclassify Adjectives - infreq. Adjectives are collapsed Amplifiedo category other
pd$Variant <- ifelse(pd$Variant  %in% famps, pd$Variant , "other")
# create variables 
pd$other <- ifelse(pd$Variant == "other", 100, 0)
pd$pretty <- ifelse(pd$Variant == "pretty", 100, 0) 
pd$really <- ifelse(pd$Variant == "really", 100, 0) 
pd$so <- ifelse(pd$Variant == "so", 100, 0) 
pd$very <- ifelse(pd$Variant == "very", 100, 0)
pd$zero <- ifelse(pd$Variant == "0", 100, 0)
###############################################################
# p1
p1d <- pd

# start plot: Amplified
p1 <- ggplot(p1d, aes(x = Age, y = Amplified)) +
  geom_smooth(aes(y = Amplified), size=.5, col = "gray30", lty = "longdash", se = F) +
  facet_grid(vars(Function), vars(Genre)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent of Amplification") +
  theme_light(base_size = 10) +
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"AmplifiedGenreFunction.png",sep="/"))
p1

###############################################################
# p2
p2d <- pd
# remove non-amplified instances
p2d <- p2d[p2d$Amplified != 0,]
# start plot: all
p2 <- ggplot(p2d, aes(x = Age, y = very)) +
  facet_grid(vars(Function), vars(Genre)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.25, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.25, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.25, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size=.25, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.25, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("longdash","twodash", "dashed", "dotdash", "solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("gray50", "goldenrod2", "gray70", "indianred4", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"VariantGenreFunction.png",sep="/"))
p2

###############################################################
# p3
p3d <- pd
# remove non-amplified instances
p3d <- p3d[p3d$Amplified != 0,]
# start plot: all with zero
p3 <- ggplot(p3d, aes(x = Age, y = very)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.25, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.25, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.25, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size=.25, se = F) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.25, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("longdash","twodash", "dashed", "dotdash", "solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_colour_manual(values=c("gray50", "goldenrod2", "gray70", "indianred4", "grey30"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very"), 
                      labels = c("other", "pretty", "really", "so", "very")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"VariantPdFunction.png",sep="/"), width = 10, height = 10, units = c("cm"),  dpi = 320)
p3

###############################################################
# p4
p4d <- pd
p4d <- subset(p4d, Genre == "PrivateDialogue")
# start plot: all
p4 <- ggplot(p4d, aes(x = Age, y = zero)) +
  facet_grid(vars(Function)) +
  geom_smooth(aes(y = other, color = "other", linetype = "other"), size=.25, se = F) +
  geom_smooth(aes(y = pretty, color = "pretty", linetype = "pretty"), size=.25, se = F) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.25, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.25, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.25, se = F) +
  geom_smooth(aes(y = zero, color = "zero", linetype = "zero"), size=.25, se = F) +  
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("longdash","twodash", "dashed", "dotdash", "solid", "dotted"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very", "zero"), 
                        labels = c("other", "pretty", "really", "so", "very", "zero")) +
  scale_colour_manual(values=c("gray50", "goldenrod2", "gray70", "indianred4", "grey30", "gray50"),
                      name="Variant", 
                      breaks=c("other", "pretty", "really", "so", "very", "zero"), 
                      labels = c("other", "pretty", "really", "so", "very", "zero")) +
  theme_set(theme_light(base_size = 10)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age", y = "Percent of Amplification") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"VariantwzGenre.png",sep="/"), width = 10, height = 10, units = c("cm"),  dpi = 320)
p4

###############################################################
#            WARNING: DATA REDUCTION
###############################################################
#ampaus <- ampaus[ampaus$Function == "Attributive",]
ampaus <- ampaus[ampaus$Genre == "PrivateDialogue",]
###############################################################
#            WARNING: DATA REDUCTION
###############################################################
# recode adjectives
ntfrqadj <- names(table(ampaus$Adjective))[which(table(ampaus$Adjective) <= 10)]
ampaus$Adjective <- ifelse(ampaus$Adjective %in% ntfrqadj, "other", ampaus$Adjective)
###############################################################
###             TABULARIZATION
###############################################################
# tb 1
Varianttbaus <- table(ampaus$Variant)
Varianttbaus <- Varianttbaus[order(table(ampaus$Variant), decreasing = T)]
Variantnames <- as.vector(names(Varianttbaus))
Variantn <- as.vector(Varianttbaus)
Variantprcnt <- round(Variantn/sum(Variantn)*100, 2)
Variantprcnt2 <-  c(0, round(Variantn[2:length(Variantn)]/sum(Variantn[2:length(Variantn)])*100, 2))
Varianttbaus <- data.frame(Variantnames, Variantn, Variantprcnt, Variantprcnt2)
colnames(Varianttbaus) <- c("Amplifiedensifier", "TokenFrequency", "PercentAgeSlots", "PercentAgeAmplifiedensifiers")
Varianttbaus <- rbind(Varianttbaus, c("Total", sum(as.vector(Varianttbaus$TokenFrequency)), "", ""))
rownames(Varianttbaus) <- NULL
# inspect data
head(Varianttbaus)

# save data to disc
write.table(Varianttbaus, "Varianttbaus.txt", sep = "\t", row.names = F)
###############################################################
# tb 2
ampausVariantdf <- ampaus[ampaus$Variant != "0",]
tbAdjective <- table(ampausVariantdf$Adjective)
tbAdjective <- tbAdjective[order(tbAdjective, decreasing = T)]
tbAdjective <- tbAdjective[which(tbAdjective >= 5)]
freqAdjective <- names(tbAdjective)
dffAdjective <- ampausVariantdf[ampausVariantdf$Adjective %in% freqAdjective,]
dffAdjective <- dffAdjective[, c(1, 14, 2)] # Age Variant Adjective
tst5 <- table(ampaus$Variant)
infreqAmplified <- names(tst5[which(tst5 < 15)])
dffAdjective$Variant <- ifelse(dffAdjective$Variant %in% infreqAmplified, "other", dffAdjective$Variant)
dffAdjective <- dffAdjective[dffAdjective$Variant != "0",]
# tabulate by Age
ampausVariantdf <- ftable(dffAdjective$Adjective, dffAdjective$Variant, dffAdjective$Age)
ampausVariantdf

# save data to disc
write.table(ampausVariantdf, "AmplifiedAdjectivedateabs.txt", sep = "\t", row.names = T)
###############################################################
# tb 3
# reorder data
relfreqtb_abs <- ftable(dffAdjective$Adjective, dffAdjective$Age, dffAdjective$Variant)
relfreqtb_rel <- round(prop.table(relfreqtb_abs, 1)*100, 1) # row percentAges
# replave NA with 0
relfreqtb_relwona <- apply(relfreqtb_rel, 1, function(x) ifelse(is.na(x) == T, 0, x))
# convert Amplifiedo matrix
relfreqmx <- matrix(relfreqtb_relwona, ncol = 3, byrow = F)
# convert Amplifiedo data frame
relfreqdf <- as.data.frame(relfreqmx)
# create a vector with Adjectives
Adjective <- rep(unlist(attr(relfreqtb_rel, "row.vars")[1]), each =  
                   (nrow(relfreqmx)/length(unlist(attr(relfreqtb_rel, "row.vars")[1])))) # Age groups
# create a vector with amplifiers
amp <- rep(unlist(attr(relfreqtb_rel, "col.vars")[1]),  
           (nrow(relfreqmx)/length(unlist(attr(relfreqtb_rel, "col.vars")[1])))) # Age groups
# create a data frame
relfreqdf <- data.frame(Adjective, amp, relfreqdf)
# add colnames
colnames(relfreqdf) <- c("Adjective", "amp", unlist(attr(relfreqtb_rel, "row.vars")[2]))
# inspect data
str(relfreqdf); head(relfreqdf)

# save data to disc
write.table(relfreqdf, "iampAdjectiveampAgepcnt.txt",  sep = "\t", row.names = T)
###############################################################
#              SEMANTIC VECTOR SPACE MODEL 2
# evaluation how strongly really and very correlate
# tabulate data
t1 <- tapply(ampaus$Amplified, list(ampaus$Adjective, ampaus$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3aus <- t3
#t3aus <- t3aus[, 2: ncol(t3aus)]
# remove Adjectives that were not Amplifiedensified
t3aus <- t3aus[rowSums(t3aus) > 0, ]
# save row and column names
colnamesaus <- colnames(t3aus)
rownamesaus <- rownames(t3aus)
# turn dataframe Amplifiedo matrix
svsmaus <- as.matrix(t3aus)
# convert token frequency to type frequency
#svsmaus <- apply(svsmaus, 1, Function(x) { x <- ifelse(x > 1, 1, x) } )
svsmaus <- t(svsmaus)
#svsmaus <- svsmaus[, colSums(svsmaus) >= 2]
#svsmaus <- svsmaus[rowSums(svsmaus) >= 2, ]
svsmaus

# determine overall n in data
n_aus <- sum(svsmaus)
n_aus

# correlate amplifiers based on collocation
r_aus <- cor(t(svsmaus))
r_aus

# extract correlation coefficient r for really and very
r_reallyveryaus <- r_aus[which(attr(r_aus, "dimnames")[[1]] == "very"), which(attr(r_aus, "dimnames")[[2]] == "really")]
r_reallyveryaus

# load required library
library(psych)
z_reallyveryaus <- fisherz(r_reallyveryaus)
z_reallyveryaus

# the z value can be tested for significance using the r-test from the psych library
#r.test(n=100,r12=.5,r34=.4, n2=80) 
###############################################################
#               PLOTTING LEXICAL DIVESRITY
# Function for extracting lexdiv values
lexdiv <- function(x){
  Varianttokentbaus <- table(x$Variant, x$Adjective)
  Varianttokentbaus <- Varianttokentbaus[2:nrow(Varianttokentbaus), ]
  #Varianttokentbaus <- Varianttokentbaus[rowSums(Varianttokentbaus) > 1, ]
  # extract typefrequency of tokenectives
  Varianttokentbaustyp <- t(apply(Varianttokentbaus, 1, function(x) ifelse(x > 1, 1, x)  ))
  # claculate lexical diversity measure
  lexdivaus <- rowSums(Varianttokentbaustyp)/rowSums(Varianttokentbaus)
  lexdivaus <- lexdivaus[order(lexdivaus)]
  return(lexdivaus)
}
# apply Function to data
lexdivaus <- lexdiv(ampaus)

# ggplot2 p5
lexdivdf <- data.frame(1:length(lexdivaus), names(lexdivaus), round(lexdivaus, 2))
colnames(lexdivdf) <- c("id", "amp", "lexdiv")

# example extraction
x <- ampaus
Varianttokentbaus <- table(x$Variant, x$Adjective)
Varianttokentbaus <- Varianttokentbaus[2:nrow(Varianttokentbaus), ]
Varianttokentbaustyp <- t(apply(Varianttokentbaus, 1, function(x) ifelse(x > 1, 1, x)  ))
lexdivaus <- rowSums(Varianttokentbaustyp)/rowSums(Varianttokentbaus)


# start plot: Amplified
p5 <- ggplot(lexdivdf, aes(x = jitter(id), y = lexdiv, label = lexdivdf$lexdiv), size = 8) +
  geom_line(aes(y = lexdiv), col = "gray30", lwd = .5) + 
  geom_smooth(aes(y = lexdiv), size=.5, col = "gray70", lty = "dashed", se = F) +
  geom_text(label = lexdivdf$lexdiv, hjust = 0.1, nudge_y = -0.1, size = 2) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Amplifier Type", y = "Lexical Diversity") +
  theme_light(base_size = 8) +
  scale_x_continuous(name = "Amplifier Type",
                     breaks = c(1:length(lexdivdf$amp)),
                     labels=lexdivdf$amp)
ggsave(file = paste(imAgeDirectory,"LexDivAll.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p5

###############################################################
#            PLOT LEXICAL DIVERSITY OVER TIME
# recode variant for lex div analysis
ldd <- ampaus
vld <- c("really", "so", "very", "0")
ldd$Variant <- ifelse(ldd$Variant %in% vld, ldd$Variant, "other")
# extract tokenfrequency of Amplifiedensifiers
a1725 <- subset(ldd, Age == "17-25")
a2640 <- subset(ldd, Age == "26-40")
a4180 <- subset(ldd, Age == "41-80")
# apply Function to data sets
lexdiv1725 <- lexdiv(a1725)
lexdiv2640 <- lexdiv(a2640)
lexdiv4180 <- lexdiv(a4180)
# find common items
cmnamps <- Reduce(intersect, list(names(lexdiv1725),names(lexdiv2640),names(lexdiv4180)))
# extract lex div values for amps which occur in all Age groups
lexdivvls <- data.frame(lexdiv1725[which(names(lexdiv1725) %in% cmnamps)][order(names(lexdiv1725[which(names(lexdiv1725) %in% cmnamps)]))], 
                        lexdiv2640[which(names(lexdiv2640) %in% cmnamps)][order(names(lexdiv2640[which(names(lexdiv2640) %in% cmnamps)]))], 
                        lexdiv4180[which(names(lexdiv4180) %in% cmnamps)][order(names(lexdiv4180[which(names(lexdiv4180) %in% cmnamps)]))])
# transpose data
lexdivvlst <- t(lexdivvls)
# combine lexdiv tables
p6d <- data.frame(1:nrow(lexdivvlst), names(table(ampaus$Age)), lexdivvlst)
colnames(p6d)[1:2] <- c("id", "Age")
rownames(p6d) <- 1:nrow(p6d)
Agelbs <- names(table(p6d$Age))
p6d$Age <- ifelse(p6d$Age == "17-25", 3,
                  ifelse(p6d$Age == "26-40", 2, 
                         ifelse(p6d$Age == "41-80", 1, p6d$Age)))
p6d$Age <- as.numeric(p6d$Age)
# start plot: Amplified
p6 <- ggplot(p6d, aes(x = Age, y = other, label = Age), size = 8) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  geom_smooth(aes(y = really, color = "really", lty = "really"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", lty = "very"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", lty = "so"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("longdash", "dashed","dotted", "solid"),
                        name="",
                        breaks = c("other", "really", "so",  "very"), 
                        labels = c("other", "really", "so",  "very")) +
  scale_colour_manual(values=c("grey40", "grey40", "grey40", "grey40"),
                      name="", 
                      breaks=c("other", "really",  "so",  "very"), 
                      labels = c("other", "really",  "so",  "very")) +
  theme(legend.position="top") +
  theme_light(base_size = 10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Amplifier Type", y = "Lexical Diversity") +
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"LexDivAge.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p6

###############################################################
#       CORRESPONDENCE ANALYSIS
# load packAges for CA
library("factoextra")
#library("gplots")
# convert the data as a table
catbaus <- t3aus[rowSums(t3aus) >= 5, ]
catbaus <- catbaus[, colSums(catbaus) >= 2]
# only variable items
catbaus2 <- t(apply(catbaus, 1, function(x){ x <- ifelse(x > 1, 1, x)})) 
catbaus <- catbaus[, colSums(catbaus2) >= 2]
# convert data Amplifiedo table
dt <- as.table(as.matrix(catbaus))
# check wehther rows and columns correlate significantly
chisq <- chisq.test(catbaus)
# use only if X2 is significant
chisq$p.value

# load library
library("FactoMineR")
# create correspondence object
res.ca <- CA(catbaus, graph = FALSE)
# extract eigencvalues
eig.val <- get_eigenvalue(res.ca)
# show percentAge of explained variances per dimension
#fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 60))

# show percentAge of explained variances per dimension with threshold
#fviz_screeplot(res.ca) +  
#geom_hline(yAmplifiedercept=mean(eig.val), linetype=2, color="red")

# Contributions of variables to PC1
#fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)

# Contributions of variables to PC2
#fviz_contrib(res.ca, choice = "col", axes = 2, top = 10)

# plot correspondence analysis results
png("images/CAFactorMapFvisAmpaus.png",  width = 680, height = 480) 
fviz_ca_biplot(res.ca, repel = T, col.ind = "cos2", col.row = "gray", col.var = "darkgray")
dev.off()
# alternative plot
png("images/CAFactorMapAmpaus.png",  width = 680, height = 480) 
plot(res.ca, shadow = T, cex = 1, selectRow = "cos2 0.1", selectCol = "cos2 0.1", col.row = "gray50", title = "")
dev.off()
# add dentrogram of results
res <- hcut(catbaus, k = 6, stand = TRUE)
# Visualize
#fviz_dend(res, rect = TRUE, cex = 0.5, k_colors = c("indianred4","goldenrod2", "grey30", "indianred4", "goldenrod2", "grey30"))

# Optimal number of clusters for k-means
my_data <- scale(catbaus)
fviz_nbclust(my_data, kmeans, method = "gap_stat")
###############################################################
#           COVARYING COLLEXEME ANALYSIS
# collex Function
collex <- function(data = data, cv1 = cv1){
  # set up rslttb
  rslttb <- matrix(c("amp", "Adjective", "namp", "nAdjective", "obs", "exp", "prob", "cs", "or", "p"), ncol = 10)
  colnames(rslttb) <- c("Amp", "Adjective", "N(Amp)", "N(Adjective)", "OBS", "EXP", 
                        "Probability", "CollStrength", "OddsRatio", "p")
  rvs <- 1:nrow(t3)
  # define column values
  cv0 <- 1
  # set up table
  sapply(rvs, function(x){
    # extract values
    obs <- t3[x,cv1] # freq Adjective with amp
    fAdjective <- sum(t3[x,]) # freq Adjective
    n <- sum(t3[,cv1]) # freq amp
    fall <- sum(t3) # freq amps and Adjectives
    # calculate exp
    exp <- (fAdjective*n)/fall
    prob <- exp/n
    # create table to extract odds ratios
    m <- matrix(c(obs, (n-obs), (fAdjective-obs), (fall-fAdjective)), nrow = 2, byrow = T)
    o <- fisher.test(m)
    # perform binomial test
    rslt <- binom.test(obs, n, prob)
    # set up table with results
    rslttb <- list(c(colnames(data)[cv1], 
                     rownames(data)[x], 
                     n, 
                     fAdjective,
                     obs,
                     round(exp, 1),
                     round(prob, 2),
                     round(abs(log(as.vector(unlist(rslt$p.value, 10)), 10)), 2), 
                     round(as.vector(unlist(o$estimate)), 2),
                     round(as.vector(unlist(rslt$p.value)), 6)
    ))
    # return results
    return(rslttb)
  } )
}
###############################################################
#                 CCLA ON ALL Age GROUPS COMBINED
# rename data
cclad <- ampaus
# define infrequent adjectives
ntfrqadj <- names(table(cclad$Adjective))[which(table(cclad$Adjective) <= 10)]
# recode adjectives
cclad$Adjective <- ifelse(cclad$Adjective %in% ntfrqadj, "other", cclad$Adjective)

# create table
t1 <- tapply(cclad$Amplified, list(cclad$Adjective, cclad$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex Function (Amplifieds >= 250: 
colnames(t3)[which(colSums(t3) >= 10)]

which(colSums(t3) >= 10)

Amplifiers <- names(which(colSums(t3) >= 10))

pretty  <- collex(data = t3, cv1 = which(colnames(t3) == "pretty"))
really  <- collex(data = t3, cv1 = which(colnames(t3) == "really"))
so  <- collex(data = t3, cv1 = which(colnames(t3) == "so"))
very  <- collex(data = t3, cv1 = which(colnames(t3) == "very"))
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=10,byrow=TRUE)
really <- matrix(unlist(really),ncol=10,byrow=TRUE)
so <- matrix(unlist(so),ncol=10,byrow=TRUE)
very <- matrix(unlist(very),ncol=10,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty,  really, so, very)
# write Function to process collextab (input = collextab)
collextbedit <- function(collextab){
  # convert Amplifiedo data frame
  collexdf <- as.data.frame(collextab)
  # add colnames
  colnames(collexdf) <- c("Amp", "Adjective", "N(Amp)", "N(Adjective)", "OBS", "EXP", 
                          "Probability", "CollStrength", "OddsRatio", "p")
  # add attraction column
  collexdf$attr <- ifelse(as.numeric(collexdf$OBS) > as.numeric(collexdf$EXP), "attr", "repel")
  # modify CollStrength column 
  collexdf$CollStrength <- ifelse(collexdf$attr == "repel", 
                                  paste("-", collexdf$CollStrength, sep =""), collexdf$CollStrength)
  # perform bonferroni correction
  corr05 <- 0.05/nrow(collexdf)
  collexdf$corr05 <- rep(corr05, nrow(collexdf))
  corr01 <- 0.01/nrow(collexdf)
  collexdf$corr01 <- rep(corr01, nrow(collexdf))
  corr001 <- 0.001/nrow(collexdf)
  collexdf$corr001 <- rep(corr001, nrow(collexdf))
  # calculate corrected significance status
  collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
    x <- ifelse(x <= corr001, "p<.001",
                ifelse(x <= corr01, "p<.01",
                       ifelse(x <= corr001, "p<.001", "n.s."))) } )))
  return(collexdf)
}
# apply collextbedit Function
collex_all <- collextbedit(collextab)
# inspect results
subset(collex_all, sig != "n.s.")

###############################################################
#                  CVCLA : 17-25
# collocation analysis Age 17-25
df1725 <- cclad[cclad$Age == "17-25",]
t1 <- tapply(df1725$Amplified, list(df1725$Adjective, df1725$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex Function (colnames(t3)[which(colSums(t3) >= 10)]
which(colnames(t3) %in% Amplifiers)
pretty  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[1])
really  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[2])
so  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[3])
very  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[4])
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=10,byrow=TRUE)
really <- matrix(unlist(really),ncol=10,byrow=TRUE)
so <- matrix(unlist(so),ncol=10,byrow=TRUE)
very <- matrix(unlist(very),ncol=10,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, really, so, very)
# apply collextbedit Function
collex1725 <- collextbedit(collextab)
# inspect results
subset(collex1725, sig != "n.s.")

###############################################################
#                  CVCLA: 26-40
df2640 <- cclad[cclad$Age == "26-40",]
t1 <- tapply(df2640$Amplified, list(df2640$Adjective, df2640$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex Function (colnames(t3)[which(colSums(t3) >= 3)]
which(colnames(t3) %in% Amplifiers)
pretty  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[1])
really  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[2])
so  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[3])
very  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[4])
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=10,byrow=TRUE)
really <- matrix(unlist(really),ncol=10,byrow=TRUE)
so <- matrix(unlist(so),ncol=10,byrow=TRUE)
very <- matrix(unlist(very),ncol=10,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, really, so, very)
# apply collextbedit Function
collex2640 <- collextbedit(collextab)
# inspect results
subset(collex2640, sig != "n.s.")

###############################################################
#                  CVCLA: 41-80
df4180 <- cclad[cclad$Age == "41-80",]
t1 <- tapply(df4180$Amplified, list(df4180$Adjective, df4180$Variant), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex Function (colnames(t3)[which(colSums(t3) >= 3)]
which(colnames(t3) %in% Amplifiers)
really  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[1])
so  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[2])
very  <- collex(data = t3, cv1 = which(colnames(t3) %in% Amplifiers)[3])
# extract informaltion
really <- matrix(unlist(really),ncol=10,byrow=TRUE)
so <- matrix(unlist(so),ncol=10,byrow=TRUE)
very <- matrix(unlist(very),ncol=10,byrow=TRUE)
# set up table with results
collextab <- rbind(really, so, very)
# apply collextbedit Function
collex4180 <- collextbedit(collextab)
# inspect results
subset(collex4180, sig != "n.s.")

###########################################################################
# combine covar collex data frames
collexdfAge <- rbind(collex1725[,c(1:11,15)], collex2640[,c(1:11,15)], 
                     collex4180[,c(1:11,15)])
Age <- c(rep("1725", nrow(collex1725)), rep("2640", nrow(collex2640)), 
         rep("4180", nrow(collex4180)))
# create data frame
covarcoldf <- data.frame(Age, collexdfAge)
#convert Amplifiedo numeric
covarcoldf$Probability <- as.numeric(covarcoldf$Probability)
covarcoldf$CollStrength <- as.numeric(covarcoldf$CollStrength)
covarcoldf$OddsRatio <- as.numeric(covarcoldf$OddsRatio)
# inspect data
str(covarcoldf); head(covarcoldf); summary(covarcoldf$CollStrength)

###########################################################################
# rename data
p10d <- covarcoldf[, c(1:3, 9)] # Age, Amp, Adjective, CollStrength
#extract Adjectives present in all ages
allagesAdjective <- which(rowSums(ftable(p10d$Amp, p10d$Adjective, p10d$Age)) == 3)
ampAdjectiveageftb <- ftable(p10d$Amp, p10d$Adjective, p10d$Age)
amp <- unlist(attr(ampAdjectiveageftb, "row.vars")[1])
Adjective <- unlist(attr(ampAdjectiveageftb, "row.vars")[2])
Age <- unlist(attr(ampAdjectiveageftb, "col.vars")[1])
Adjectiver <- rep(Adjective, length(amp))
ampr <- rep(amp, each = length(Adjective))
freqAdjectives1 <- unique(Adjectiver[allagesAdjective])
freqAdjectives2 <- names(table(cclad$Adjective)[order(table(cclad$Adjective), decreasing = T)])[1:20]
freqAdjectives <- intersect(freqAdjectives1, freqAdjectives2)
# use only data with Adjectives that are present among all amps and all age groups
p10d <- subset(p10d, Adjective %in% freqAdjectives)
# create new id variable
p10d$AgeAdjective <- paste(p10d$Age, "_", p10d$Adjective, sep = "")
# reorder data frame
p10tb <- reshape(p10d, idvar = "AgeAdjective", timevar = "Amp",direction = "wide")
# select relevant column
# age, Adjective, collstrength:quite, collstrength:so, collstrength:really, collstrength:very
p10tb <- p10tb[, c(5:7, 10, 13)] 
colnames(p10tb) <- c("Age", "Adjective",  "really", "so", "very")
p10tb$Adjective <- as.factor(p10tb$Adjective)
# recode Age
p10tb$Age <- ifelse(p10tb$Age == "1725", 3, p10tb$Age)
p10tb$Age <- ifelse(p10tb$Age == "2640", 2, p10tb$Age)
p10tb$Age <- ifelse(p10tb$Age == "4180", 1, p10tb$Age)
p10tb$Age <- as.numeric(p10tb$Age)
# create vector with Age groups
Agelbs <- c("17-25", "26-40", "41-80")
head(p10tb)

# start plot: all
p10 <- ggplot(p10tb, aes(x = jitter(Age), y = really)) +
  facet_wrap(vars(Adjective)) +
  geom_smooth(aes(y = really, color = "really", linetype = "really"), size=.5, se = F) +
  geom_smooth(aes(y = so, color = "so", linetype = "so"), size=.5, se = F) +
  geom_smooth(aes(y = very, color = "very", linetype = "very"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("longdash", "dotted", "solid"),
                        name="Variant",
                        breaks = c("really", "so", "very"), 
                        labels = c("really", "so", "very")) +
  scale_colour_manual(values=c("grey80","gray60", "grey40"),
                      name="Variant", 
                      breaks=c("really", "so", "very"), 
                      labels = c("really", "so", "very")) +
  theme_set(theme_light(base_size = 8)) +
  theme(legend.position="top") +
  coord_cartesian(ylim = c(-2.5, 2.5)) +
  labs(x = "Year", y = "Collocation Strength (LOG(p), 10)") +
  guides(size = FALSE)+
  guides(alpha = FALSE)+
  #  guides(linetype = FALSE)+
  #theme(legend.title=element_blank())+
  scale_x_continuous(name = "Age",
                     breaks = c(1, 2, 3),
                     labels=rev(Agelbs))
ggsave(file = paste(imAgeDirectory,"CovarcollAmpausFreqAdjective1.png",sep="/"), width = 15, height = 10, units = c("cm"),  dpi = 320)
p10

###########################################################################
#                  CHANGES IN Adjective FREQ
# tabulate data
ftbadjaus <- ftable(cclad$Adjective, cclad$Age)
rwnms <- as.vector(unlist(attr(ftbadjaus, "row.vars")))
ftbadjaus <- ftbadjaus[2:nrow(ftbadjaus),]
rownames(ftbadjaus) <- rwnms[2:length(rwnms)]
svrwnms <- as.vector(unlist(attr(ftbadjaus, "dimnames")))[which(rowSums(ftbadjaus) >= 45)]
ftbadjaus <- ftbadjaus[which(rowSums(ftbadjaus) >= 45),]
rownames(ftbadjaus) <- svrwnms
colnames(ftbadjaus) <- names(table(cclad$Age))
ptbadjaus <- prop.table(ftbadjaus, margin=2)*100
#ptbampaus <- ptbampaus[rowSums(ptbampaus) > 1, ]
ptbadjaus

# save data to disc
write.table(ptbadjaus, "ptbadjaus.txt", sep = "\t", row.names = F)
###########################################################################
p11d <- cclad
famp <- names(table(p11d$Adjective))[which(table(p11d$Adjective) > 45)]
p11d$Adjective <- ifelse(p11d$Adjective %in% famp, p11d$Adjective, "other")
table(p11d$Adjective)[order(table(p11d$Adjective), decreasing = T)]

# create vars for Variant
p11d$other <- ifelse(p11d$Adjective == "other", 100, 0)
p11d$big <- ifelse(p11d$Adjective == "big", 100, 0)
p11d$good <- ifelse(p11d$Adjective == "good", 100, 0)
p11d$great <- ifelse(p11d$Adjective == "great", 100, 0)
p11d$little <- ifelse(p11d$Adjective == "little", 100, 0)
p11d$nice <- ifelse(p11d$Adjective == "nice", 100, 0)
p11d$true <- ifelse(p11d$Adjective == "true", 100, 0)
# recode Age
p11d$Age <- ifelse(p11d$Age == "17-25", 3, p11d$Age)
p11d$Age <- ifelse(p11d$Age == "26-40", 2, p11d$Age)
p11d$Age <- ifelse(p11d$Age == "41-80", 1, p11d$Age)
p11d$Age <- as.numeric(p11d$Age)
# create vector with Age groups
Agelbs <- c("17-25", "26-40", "41-80")
str(p11d)

table(p11d$Adjective)[order(table(p11d$Adjective), decreasing = T)]

# start plot: Adjective
p11 <- ggplot(p11d, aes(x = jitter(Age), y = other), size = 8, se = F) +
  geom_smooth(aes(y = other, color = "other", lty = "other"), size=.5, se = F) +
  geom_smooth(aes(y = good, color = "good", lty = "good"), size=.5, se = F) +
  geom_smooth(aes(y = nice, color = "nice", lty = "nice"), size=.5, se = F) +
  geom_smooth(aes(y = big, color = "big", lty = "big"), size=.5, se = F) +
  geom_smooth(aes(y = little, color = "little", lty = "little"), size=.5, se = F) +
  geom_smooth(aes(y = true, color = "true", lty = "true"), size=.5, se = F) +
  geom_smooth(aes(y = great, color = "great", lty = "great"), size=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(values=c("dashed", "longdash", "twodash","dotdash", "dashed","longdash", "solid"),
                        name="",
                        breaks = c("other", "good", "nice", "big", "little", "true", "great"), 
                        labels = c("other", "good", "nice", "big", "little", "true", "great")) +
  scale_colour_manual(values=c("grey30", "grey60", "goldenrod2",  "indianred4", "grey30", "goldenrod2", "grey60"),
                      name="", 
                      breaks=c("other", "good", "nice", "big", "little", "true", "great"), 
                      labels = c("other", "good", "nice", "big", "little", "true", "great")) +
  theme(legend.position="top") +
  theme_light(base_size = 8) +
  coord_cartesian(ylim = c(0, 70)) +
  labs(x = "Decade", y = "Percent of Adjectives") +
  scale_x_continuous(name = "Age",
                     breaks = 3:1,
                     labels=Agelbs)
ggsave(file = paste(imAgeDirectory,"AdjectivefreqAge.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p11

###########################################################################
AdjectiveAge <- 1:3
Adjectivelm <- ptbadjaus
head(Adjectivelm)

str(Adjectivelm)

nrow(Adjectivelm)
sigAdjective <- apply(Adjectivelm, 1, function(x){
  x <- lm(x ~ AdjectiveAge)
  x <- summary(x)[4][[1]][[8]]})

sigAdjectives <- which(sigAdjective < .05)
sigAdjectives

###########################################################################
#                  REGRESSION DATA SET
###########################################################################
# only amplified instances
reallyaus <- ampaus[ampaus$Amplified == 1,]
# inspect data
str(reallyaus); colnames(reallyaus)

# remove superfluous columns
reallyaus$ID <- NULL
reallyaus$File <- NULL
reallyaus$Subfile <- NULL
reallyaus$Speaker <- NULL
reallyaus$SpeechUnit <- NULL
reallyaus$CleanSpeechUnit <- NULL
reallyaus$PosTaggedSpeechUnit <- NULL
reallyaus$OriginalString <- NULL
reallyaus$PreContext <- NULL
reallyaus$PostContext <- NULL
reallyaus$PreContextLong <- NULL
reallyaus$AgeOriginalClassification <- NULL
reallyaus$Genre <- NULL
reallyaus$Amplified <- NULL
reallyaus$so <- NULL
reallyaus$pretty <- NULL
reallyaus$Variant <- NULL 
# define vector for data inspection
cltb <- c("Age", "Adjective", "Emotionality", "Function", "Priming", "Gender",  
          "Occupation",  "Date", "ConversationType", "AudienceSize", 
          "very", "really", "Gradabilty", "SemanticCategory")
# tabulate data
lapply(reallyaus[cltb], table)

# check data
reallyaus[which(reallyaus$SemanticCategory == "NoSemType"),]

# remove superfluous columns
reallyaus$L1 <- NULL               # invar: only 1 token
reallyaus$Education <- NULL        # invar: only college
reallyaus$birthplace <- NULL       # only 10 tokens
reallyaus$nationality <- NULL      # invar: only 20 tokens, all Australian
reallyaus$Date <- NULL             # all data collected in 92 and 93
# recategorzie Adjective
fradj <- names(table(reallyaus$Adjective))[which(table(reallyaus$Adjective) > 10)]
reallyaus$Adjective <- ifelse(reallyaus$Adjective %in% fradj, reallyaus$Adjective, "other")
# recategorzie Gender
reallyaus$Gender <- ifelse(reallyaus$Gender == "M", "Men",
                           ifelse(reallyaus$Gender == "F", "Women", reallyaus$Gender))
# recategorzie AudienceSize
reallyaus$AudienceSize <- as.numeric(reallyaus$AudienceSize)
reallyaus$AudienceSize <- ifelse(reallyaus$AudienceSize > 2, "MultipleInterlocutors", "Dyad")
reallyaus$AudienceSize <- as.factor(reallyaus$AudienceSize)
# recategorzie Occupation
reallyaus$Occupation <- ifelse(reallyaus$Occupation == "sml", "SkilledLabor",
                               ifelse(reallyaus$Occupation == "acmp", "AcademicManagerialProfessionals", reallyaus$Occupation))
# recategorzie Priming
reallyaus$Priming <- ifelse(reallyaus$Priming == "noprime", "NoPrime",
                               ifelse(reallyaus$Priming == "prime", "Prime", reallyaus$Priming))
# recategorzie SemanticCategory
reallyaus$SemanticCategory <- as.character(reallyaus$SemanticCategory)
reallyaus$SemanticCategory <- ifelse(reallyaus$SemanticCategory == "Age", "NoSemType",
                                     ifelse(reallyaus$SemanticCategory == "Color", "NoSemType", 
                                            ifelse(reallyaus$SemanticCategory == "Difficulty", "NoSemType",reallyaus$SemanticCategory)))
# recategorzie ConversationType
reallyaus$ConversationType <- ifelse(reallyaus$ConversationType == "mixedsex", "MixedSex",
                        ifelse(reallyaus$ConversationType == "samesex", "SameSex", reallyaus$ConversationType))
# factorize variables
clfct <- c("Age", "Adjective", "Emotionality", "FileSpeaker", "Function", "Priming", "Gender", 
           "Occupation", "ConversationType", "AudienceSize", "Gradabilty", "SemanticCategory")
reallyaus[clfct] <- lapply(reallyaus[clfct], factor)
# define vector for data inspection
cltb <- c("Age", "Adjective", "Emotionality", "FileSpeaker", "Function", "Priming", "Gender", 
          "Occupation", "ConversationType", "AudienceSize", "very", "really", 
          "Gradabilty", "SemanticCategory")
# tabulate data
lapply(reallyaus[cltb], table)

# inspect data
nrow(reallyaus); str(reallyaus); colnames(reallyaus)

###############################################################
write.table(reallyaus, "ampaus05_statz.txt", row.names= F, sep = "\t")
###############################################################
#                   END PART 3
###############################################################
