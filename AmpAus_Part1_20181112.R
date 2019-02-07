##################################################################
# Titel:      The Amplifier System of Australian English - Part 1
# R version:  3.4.1 (2017-06-30) -- "Single Candle"
# Autor:      Martin Schweinberger
# Date:       2018-11-06
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. "The Amplifier System of Australian English, Part 1",
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpAusE")
# load packages
library(stringr)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
# define image directory
imageDirectory<-"images"
# specify path to corpra
corpus.aus <- "D:\\Uni\\Korpora\\Original\\ICE Australia"
bio.path <- "D:\\Uni\\Korpora\\Metadata\\ICE Australia biodata/biodataiceaus.txt"
###############################################################
# define files to load
corpus.files = list.files(path = corpus.aus, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# load corpus and start processing
corpus.tmp <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  x <- paste(x, sep = " ", collapse = " ")
  x <- strsplit(gsub("(<I>)", "~\\1", x), "~" )
  x <- unlist(x)
  x <- x[2:length(x)]
  x <- str_replace_all(x, fixed("\\"), "")
  x <- gsub("<I> ", "", x)
  } )
# extract subfiles
sf <- as.vector(unlist(sapply(corpus.tmp, function(x){
  x <- length(x)
  x <- str_replace_all(x, "2","1 2")
  x <- str_replace_all(x, "3","1 2 3")
  x <- str_replace_all(x, "4","1 2 3 4")
  x <- str_replace_all(x, "5","1 2 3 4 5")
  x <- strsplit(x, " ")
  })))
# unlist corpusfiles
corpus.tmp01 <- unlist(corpus.tmp)
# extract file names
fl <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
  x <- gsub(">.*", ">", x)
  x <- x[1]
  })
nsf <- as.vector(unlist(sapply(corpus.tmp, function(x){
  x <- length(x)}))) 
fl <- rep(fl, nsf)
# create file:subfile$speaker ids
crpdf <- cbind(fl, sf, corpus.tmp01)
crpdf[,1] <- str_replace_all(crpdf[,1], fixed(">"),"")
crpdf[,1] <- str_replace_all(crpdf[,1], fixed("<"),"")
crpdf1 <- apply(crpdf, 1, function(x){
  x <- str_replace_all(x[3], fixed("$"), paste(paste(x[1], x[2], sep = ":"), "$", sep = "")) })
# create list of spekaers with utterances
flsbspksus <- lapply(crpdf1, function(x){
  x <- strsplit(gsub("([A-Z][0-9][A-Z])", "~\\1", x), "~" )
  x <- unlist(x)
  x <- x[2:length(x)]
  } )
flsbspksusv <- as.vector(unlist(flsbspksus))
fl <- gsub(":.*", "", flsbspksusv)
sf <- gsub(".*:", "", flsbspksusv)
sf <- str_replace_all(sf, fixed("$"), "qwertz")
sf <- gsub("qwertz.*", "", sf)
spk <- gsub(">.*", "", flsbspksusv)
spk <- str_replace_all(spk, fixed("$"), "qwertz")
spk <- gsub(".*qwertz", "", spk)
su <- gsub("([A-Z][0-9][A-Z]-[0-9]{3,3}:[0-9]\\$[A-Z]{0,2}?{0,1}>)", "", flsbspksusv)
su <- gsub("<$", "", su)
su <- str_trim(su, side = "both")
flsfspk <- paste("<", fl, ":", sf, "$", spk, ">", sep = "")
su <- strsplit(gsub("(<#>)", "~\\1", su), "~" )
n <- as.vector(unlist(sapply(su, function(x){
  x <- length(x)})))
suv <- as.vector(unlist(su))
suv <- gsub("\"", "", suv)
id <- rep(flsfspk, n)
iceaus <- cbind(id, suv)
iceaus <- iceaus[iceaus[,2] != "",]
# inspect data
#str(iceaus); head(iceaus)

# create a clean vector of the speech units
sucl <- gsub("<#>", " ", iceaus[,2])
sucl <- gsub("<&>.*</&>", " ", sucl)
sucl <- gsub("<unclear>.*</unclear>", " ", sucl)
sucl <- gsub("<O>.*</O>", " ", sucl)
sucl <- gsub("<@>.*</@>", " ", sucl)
sucl <- gsub("<.{0,1}/{0,1}.{0,1}[A-Z]{0,1}[a-z]{1,}>", " ", sucl)
sucl <- gsub("<,{1,3}>", " ", sucl)
sucl <- gsub("\"", " ", sucl)
sucl <- gsub("</{0,1}\\{[0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}[0-9]{0,1}\\[{0,1}\\{{0,1}[0-9]{0,2}>", " ", sucl)
sucl <- gsub("<\\[/[0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}\\[[0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}\\}[0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}\\][0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}\\.[0-9]{0,2}>", " ", sucl)
sucl <- gsub("</{0,1}[A-Z]{0,2}[0-9]{0,2}>", " ", sucl)
sucl <- gsub("[A-Z]{0,1}[0-9]{0,1}[A-Z]-[0-9]{1,3} {0,1}[a-z]{0,1} {0,1}[0-9]{0,5}", " ", sucl)
sucl <- gsub(" {2,}", " ", sucl)
sucl <- str_trim(sucl, side = "both")
# additional cleaning
sucl <- gsub("<&> laughter <&>", " ", sucl)
sucl <- gsub("#", " ", sucl)
sucl <- gsub("=", " ", sucl)
sucl <- gsub("<", " ", sucl)
sucl <- gsub("&", " ", sucl)
sucl <- gsub(">", " ", sucl)
sucl <- gsub("\"", " ", sucl)
sucl <- gsub("/", "", sucl)
sucl <- gsub("[", " ", sucl, fixed = T)
sucl <- gsub("{", " ", sucl, fixed = T)
#  sucl <- gsub("-", "", sucl, fixed = T)
sucl <- gsub(" {2,}", " ", sucl)
sucl <- str_trim(sucl, side = "both")
corpusausdf <- data.frame(iceaus, sucl)
# remove empty speech units
corpusausdf <- corpusausdf[corpusausdf$sucl != "",]
# inspect data
#head(corpusausdf)
flidn <- corpusausdf$id
flnn <- gsub(":.*", "", corpusausdf$id)
flnn <- gsub("<", "", flnn)
sfnn <- gsub("\\$.*", "", corpusausdf$id)
sfnn <- gsub(".*:", "", sfnn)
spkn  <- gsub(".*\\$", "", corpusausdf$id)
spkn  <- gsub(">", "", spkn)
sunn <- corpusausdf$suv
sucl <- corpusausdf$sucl
corpusausdf <- data.frame(flidn, flnn, sfnn, spkn, sunn, sucl)
# check data
#head(corpusausdf)

###############################################################
# save data to disc
write.table(corpusausdf, "iceausamp01_raw.txt", sep = "\t", row.names = F, col.names = T, quote = T)
corpusausdf <- read.table("iceausamp01_raw.txt", sep = "\t", skipNul = T, header = T, fill = T)
###############################################################
# split data into smaller chunks
pos01 <- corpusausdf$sucl[1:5000]
pos02 <- corpusausdf$sucl[5001:10000]
pos03 <- corpusausdf$sucl[10001:15000]
pos04 <- corpusausdf$sucl[15001:20000]
pos05 <- corpusausdf$sucl[20001:25000]
pos06 <- corpusausdf$sucl[25001:30000]
pos07 <- corpusausdf$sucl[30001:35000]
pos08 <- corpusausdf$sucl[35001:40000]
pos09 <- corpusausdf$sucl[40001:45000]
pos10 <- corpusausdf$sucl[45001:50000]
pos11 <- corpusausdf$sucl[50001:nrow(corpusausdf)]
# reload libraries
source("D:\\R/POStagObject.R") # for pos-tagging objects in R
library(NLP)
library(openNLP)
library(openNLPmodels.en)
# pos tagging data
auspos01 <- POStag(object = pos01)
auspos01 <- as.vector(unlist(auspos01))
writeLines(auspos01, con = "auspos01.txt", sep = "\n", useBytes = FALSE)
# chunk 2
auspos02 <- POStag(object = pos02)
auspos02 <- as.vector(unlist(auspos02))
writeLines(auspos02, con = "auspos02.txt", sep = "\n", useBytes = FALSE)
# chunk 2
auspos02 <- POStag(object = pos02)
auspos02 <- as.vector(unlist(auspos02))
writeLines(auspos02, con = "auspos02.txt", sep = "\n", useBytes = FALSE)
# chunk 03
auspos03 <- POStag(object = pos03)
auspos03 <- as.vector(unlist(auspos03))
writeLines(auspos03, con = "auspos03.txt", sep = "\n", useBytes = FALSE)
# chunk 04
auspos04 <- POStag(object = pos04)
auspos04 <- as.vector(unlist(auspos04))
writeLines(auspos04, con = "auspos04.txt", sep = "\n", useBytes = FALSE)
# chunk 05
auspos05 <- POStag(object = pos05)
auspos05 <- as.vector(unlist(auspos05))
writeLines(auspos05, con = "auspos05.txt", sep = "\n", useBytes = FALSE)
# chunk 06
auspos06 <- POStag(object = pos06)
auspos06 <- as.vector(unlist(auspos06))
writeLines(auspos06, con = "auspos06.txt", sep = "\n", useBytes = FALSE)
# chunk 07
auspos07 <- POStag(object = pos07)
auspos07 <- as.vector(unlist(auspos07))
writeLines(auspos07, con = "auspos07.txt", sep = "\n", useBytes = FALSE)
# chunk 08
auspos08 <- POStag(object = pos08)
auspos08 <- as.vector(unlist(auspos08))
writeLines(auspos08, con = "auspos08.txt", sep = "\n", useBytes = FALSE)
# chunk 09
auspos09 <- POStag(object = pos09)
auspos09 <- as.vector(unlist(auspos09))
writeLines(auspos09, con = "auspos09.txt", sep = "\n", useBytes = FALSE)
# chunk 10
auspos10 <- POStag(object = pos10)
auspos10 <- as.vector(unlist(auspos10))
writeLines(auspos10, con = "auspos10.txt", sep = "\n", useBytes = FALSE)
# chunk 11
auspos11 <- POStag(object = pos11)
auspos11 <- as.vector(unlist(auspos11))
writeLines(auspos11, con = "auspos11.txt", sep = "\n", useBytes = FALSE)
# list pos tagged elements
postag.files = c("auspos01.txt", "auspos02.txt", "auspos03.txt",  "auspos04.txt", "auspos05.txt",
                 "auspos06.txt",  "auspos07.txt", "auspos08.txt", "auspos09.txt",  "auspos10.txt",
                 "auspos11.txt")
# load pos tagged elements
auspos <- sapply(postag.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
})
# unlist pos tagged elements
corpusausdf$auspos <- unlist(auspos)
###############################################################
# extract number of adjs per line
pstggd <- corpusausdf$auspos
lpstggd <- strsplit(pstggd, " ")
nlpstggd <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("[A-Z]{0,1}[a-z]{1,}\\/JJ[A-Z]{0,1}", x)]
  x <- length(x) } )))
rp <- nlpstggd
rp <- ifelse(rp == 0, 1, rp)
# load function for concordancing
source("D:\\R/ConcR_2.3_loadedfiles.R")
# set parameters for concordancing
pattern <- "[A-Z]{0,1}[a-z]{1,}\\/JJ[A-Z]{0,1}"
context <- 50
# extract all adjectives (concordance)
concjjaus <- ConcR(corpusausdf$auspos, pattern, context, all.pre = FALSE)
# repeat rows in data frame as often as there are adjectives in it (if 0 adj, repeat once)
corpusausadjdf <- corpusausdf[rep(seq(nrow(corpusausdf)), rp),]
# combine data sets
corpusausadj <- data.frame(1:nrow(corpusausadjdf), corpusausadjdf, concjjaus)
# remove rows without Tokens
ampaus <- corpusausadj[is.na(corpusausadj$Token) == F,]
# add clean column names
colnames(ampaus) <- c("ID", "FileSpeaker", "File", "Subfile", "Speaker", "SpeechUnit", "CleanSpeechUnit",
                      "PosTaggedSpeechUnit", "OriginalString", "PreContext", "Adjective", "PostContext")
# clean adjectives
ampaus$Adjective <- str_replace_all(ampaus$Adjective, fixed("/JJ"), "")
# add Vraiant column
ampaus$Variant <- gsub(".* ", "", str_trim(ampaus$PreContext, side = "both")) 
# inspect data
#nrow(ampaus); head(ampaus)

###############################################################
# define amplifiers
amplifiers <- c("absolutely", "actually", "aggressively", 
                "amazingly", "appallingly", "awful", "awfully", 
                "badly", "bloody", "certainly", "clearly",
                "complete", "dead", "completely", "considerably", 
                "crazy", "decidedly", "definitely",  "distinctly", 
                "dreadfully", "enormously", "entirely", "especially", 
                "exactly", "exceedingly", "exceptionally", 
                "excruciatingly", "extraordinarily", "extremely",
                "fiercely", "firmly", "frightfully", "fucking", 
                "fully", "genuinely", "greatly",
                "grossly", "heavily", "highly", "hopelessly", 
                "horrendously", "hugely",
                "immediately", "immensely", "incredibly", 
                "infinitely", "intensely", "irrevocably",
                "mad", "mega", "mighty", "most", "much", 
                "obviously", "openly", "overwhelmingly", "particularly", 
                "perfectly", "plenty", "positively", "precisely", 
                "pretty", "profoundly", "purely", 
                #"quite", 
                "real", "really", "remarkably", "seriously", 
                "shocking",   "significant", "significantly", "so", 
                "specially", "specifically", "strikingly",
                "strongly", "substantially", "super", "surely", 
                "terribly", "terrifically", 
                #"too",
                "total", "totally", "traditionally", "true", 
                "truly", "ultra", "utterly", "very",
                "viciously", 
                #"well", 
                "wholly", "wicked", "wildly")
# clean ice aus data
ampaus$Function <- str_trim(ampaus$PostContext, side = "both")
ampaus$Function <- tolower(ampaus$Function)
ampaus$Function <- gsub(" {2,}", " ", ampaus$Function)
ampaus$Function <- gsub(".* ", "", ampaus$Function)
ampaus$Function <- gsub(".*/n.*", "Attributive", ampaus$Function)
ampaus$Function <- ifelse(ampaus$Function == "", "Predicative", ampaus$Function)
ampaus$Function <- ifelse(ampaus$Function == "Attributive" | ampaus$Function == "Predicative", ampaus$Function, "remove")
# remove items for which Function could not be clearly determined
ampaus <- ampaus[ampaus$Function != "remove",]
# register
ampaus$Genre <- gsub("_.*", "", ampaus$File)
ampaus$Genre <- gsub("-.*", "", ampaus$Genre)
ampaus$Genre <- ifelse(ampaus$Genre == "S1A", "PrivateDialogue", ampaus$Genre)
ampaus$Genre <- ifelse(ampaus$Genre == "S1B", "PublicDialogue", ampaus$Genre)
ampaus$Genre <- ifelse(ampaus$Genre == "S2A", "UnscriptedMonologue", ampaus$Genre)
ampaus$Genre <- ifelse(ampaus$Genre == "S2B", "ScriptedMonologue", ampaus$Genre)
ampaus$Genre <- ifelse(ampaus$Genre == "PrivateDialogue" | ampaus$Genre == "PublicDialogue" |  
                         ampaus$Genre == "UnscriptedMonologue" | ampaus$Genre == "ScriptedMonologue", ampaus$Genre, "remove")
ampaus <- ampaus[ampaus$Genre != "remove",]
# shorten post Context
ampaus$PostContext <- substr(ampaus$PostContext, 1, ifelse((nchar(ampaus$PostContext)+25) <25, maampaus(nchar(ampaus$PostContext)), 25))
# pre Context
ampaus$PreContext <- str_trim(ampaus$PreContext, side = "both")
ampaus$PreContextLong <- ampaus$PreContext
ampaus$PreContextLong <- substr(ampaus$PreContextLong, ifelse(nchar(ampaus$PreContextLong)-25 <=0, 1, 
                                                              nchar(ampaus$PreContextLong)-25), nchar(ampaus$PreContextLong))
ampaus$PreContext <- gsub(".* ", "", ampaus$PreContext)
# amplifier variant
ampaus$PreContext <- gsub("\\/.*", "", ampaus$PreContext)
ampaus$Variant <- ifelse(ampaus$PreContext %in% amplifiers, ampaus$PreContext, "0")
# amplified y/n
ampaus$Amplified <- ifelse(ampaus$Variant == "0", 0, 1) 
# adjective
ampaus$Adjective <- tolower(ampaus$Adjective)
# inspect data
nrow(ampaus); head(ampaus); table(ampaus$Variant)

# define forms that require removal
sups <- c(".*most.*", ".*more.*") 
negs <- c(".*not.*", ".*never.*", ".*n't.*")
downtoners <- c(".*sort/.*", ".*kind/.*", ".* bit/.*", ".*somewhat.*", ".*fairly.*", 
                ".*rather.*", ".*reasonably.*", ".*slightly.*", ".*comparatively.*", ".*semi.*", 
                ".*relatively.*", ".*little.*", ".*somehow.*", ".*almost.*", ".*partly.*", 
                ".*hardly.*", ".* less.*", ".*barely.*", ".* just/.*")
specialforms <- c(".* too.*", ".*quite.*")
PostContextdowntoners <- c(".*enough.*")
nonpropadj <- c("only", "much", "many", "cheaper", "cheaperr", "bests", "larger")
# check length of dataset
str(ampaus); head(ampaus); nrow(ampaus)#; table(ampaus$pint); head(ampaus$PreContextLong); head(ampaus$PreContextLong)

# find items to be removed
supsidx <- unique(grep(paste(sups,collapse="|"), ampaus$PreContextLong, value=F))
negsidx <- unique(grep(paste(negs,collapse="|"), ampaus$PreContextLong, value=F))
downtonersidx <- unique(grep(paste(downtoners,collapse="|"), ampaus$PreContextLong, value=F))
specialformsidx <- unique(grep(paste(specialforms,collapse="|"), ampaus$PreContextLong, value=F))
PostContextdowntonersidx <- unique(grep(paste(PostContextdowntoners,collapse="|"), ampaus$PostContext, value=F))
nonpropadjidx <- unique(grep(paste(nonpropadj,collapse="|"), ampaus$Adjective, value=F))
# combine indices
idxs <- unique(c(supsidx, negsidx, downtonersidx, specialformsidx, PostContextdowntonersidx, nonpropadjidx))
# remove forms that require removal
ampaus <- ampaus[-idxs,]
# remove empty values
ampaus <- ampaus[!ampaus$Variant == "", ]
###############################################################
# save raw data to disc
write.table(ampaus, "ampaus02_wo_neg.txt", sep = "\t", row.names = F)
###############################################################
# code priming
prim1 <- c(rep(0, 1), ampaus$Variant[1:length(ampaus$Variant)-1])
prim2 <- c(rep(0, 2), ampaus$Variant[1:(length(ampaus$Variant)-2)])
prim3 <- c(rep(0, 3), ampaus$Variant[1:(length(ampaus$Variant)-3)])
primtb <- cbind(ampaus$Variant, prim1, prim2, prim3)

ampaus$Priming <- as.vector(unlist(apply(primtb, 1, function(x){
  x <- ifelse(x[1]== "0" , "noprime",
              ifelse(x[1] == x[2] | x[1] == x[3] | x[1] == x[4], "prime", "noprime"))
})))
# remove items that were not intensified by a minimum of 2 intensifier variants
nrow(ampaus)

pintadjtb <- table(ampaus$Adjective, ampaus$Variant)
#pintadjtb <- pintadjtb[2:nrow(pintadjtb),]
pintadjtb <- pintadjtb[,2:ncol(pintadjtb)]
pintadjtb2 <- apply(pintadjtb, 1, function(x){
  x <- ifelse(x > 1, 1, x)})
pintadjtb3 <- colSums(pintadjtb2)
pintadjschildes <- names(pintadjtb3)[which(pintadjtb3 >=2 )]
ampaus <- ampaus[ampaus$Adjective %in% pintadjschildes, ]
nrow(ampaus)

# inspect adjectives
names(table(ampaus$Adjective))

# create vector with false adjectives
rmvadj <- c("uhr")
ampaus$remove <- ifelse(ampaus$Adjective %in% rmvadj, "remove", ampaus$Adjective)
ampaus <- ampaus[ampaus$remove != "remove",]
ampaus$remove <- NULL
# inspecta data
nrow(ampaus); length(table(ampaus$Adjective)); head(ampaus)

###############################################################
# save raw data to disc
write.table(ampaus, "ampaus03_semiclean.txt", sep = "\t", row.names = F)
###############################################################
#                        END PART 1
###############################################################
