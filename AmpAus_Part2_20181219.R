##################################################################
# Titel:      The Amplifier System of Australian English - Part 2
# R version:  3.4.1 (2017-06-30) -- "Single Candle"
# Autor:      Martin Schweinberger
# Date:       2018-11-06
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. "The Amplifier System of Australian English, Part 2",
#             unpublished R script, The University of Queensland.
###############################################################
# clean current workspace
rm(list=ls(all=T))
# set wd
setwd("D:\\Uni\\Projekte\\02-Intensification\\AmpAusE")
# load library
library(plyr)
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"images"
# specify path to metadata
bio.path <- "D:\\Uni\\Korpora\\Metadata\\ICE Australia biodata/biodataiceaus.txt"
###############################################################
# read in data
ampaus <- read.table("ampaus03_semiclean.txt", sep = "\t", header=TRUE)
###############################################################
# add biodata
bio <- read.table(bio.path, sep = "\t", header=TRUE, quote = "")
# clean column names
colnames(bio) <- gsub("X.", "", colnames(bio))
colnames(bio) <- gsub("\\.", "", colnames(bio))
# homogenize column names
colnames(bio)[c(1, 2, 4)] <- c("fl", "sf", "spk")
# inspect data
#head(bio)

# add conversation type (same vs mixed) and audience size (number of interlocutors)
# add info on samesex vs diffsex files
# create a "file plus speaker" variable
bio$txtspk <- paste(bio$fl, "$", bio$spk, sep = "")
sstb <- table(bio$txtspk, bio$gender)
fl <- gsub("\\$.*", "", rownames(sstb))
spk <- gsub(".*\\$([A-Z]{0,1}\\?{0,1}).*", "\\1", rownames(sstb))
fml <- as.vector(unlist(sapply(sstb[,1], function(x){
  x <- ifelse(x >0, 1, 0) } )))
ml <- as.vector(unlist(sapply(sstb[,2], function(x){
  x <- ifelse(x >0, 1, 0) } )))
# set up data frame
ssdf <- data.frame(rownames(sstb), fl, spk, fml, ml)
colnames(ssdf) <- c("full", "file", "spk", "female", "male")
# clean speakers
ssdf$spk <- gsub("?", "", ssdf$spk, fixed = T)
ssdf <- ssdf[ssdf$spk != "", ]
# inspect data
#head(ssdf)

ssdf <- ssdf[rowSums(ssdf[, c(4:5)]) > 0, ]
ssdf$sex <- as.vector(unlist(sapply(ssdf[, 5], function(x){
  x <- ifelse(x == 1, "male", "female") } )))
# inspect data
#head(ssdf)

tst <- table(ssdf$file, ssdf$sex)
intloc <- apply(tst, 1, function(x){
  x <- rowSums(tst) } )
intloc <- intloc[,1]
tst2 <- t(apply(tst, 1, function(x){
  x <- ifelse(x > 0, 1, 0) } ))
tst2 <- as.data.frame(tst2)
rwsm <- as.vector(unlist(rowSums(tst2)))
tst2$ConversationType <- as.vector(unlist(sapply(rwsm, function(x){
  x <- ifelse(x == 1, "samesex", "mixedsex") } )))
tst2$text.id <- rownames(tst2)
tst2$ints <- intloc
tst3 <- data.frame(tst2$ConversationType, tst2$text.id, tst2$ints)
colnames(tst3) <- gsub("tst2.", "", colnames(tst3))
# chnage colnames so that they match the bio colnames
colnames(tst3) <- c("ConversationType", "File", "AudienceSize")
# inspect data
#head(tst3)

# homogenize colnames
colnames(bio)[c(1,2,4,5,6,7)] <- c("File", "Subfile", "Speaker", "Gender", "AgeOriginalClassification", "Age")

# combine number of interlocutors and emoire data frame
bio <- join(bio, tst3, by = c("File"), type = "left")
# inspect data
#str(bio); head(bio)

###############################################################
# join data
ampaus <- join(ampaus, bio, by = c("File", "Subfile", "Speaker"), type = "left")
# inspect data
#str(ampaus); head(ampaus); nrow(ampaus)

# clean colnames
colnames(ampaus) <- ifelse(colnames(ampaus) == "education", "Education",
                           ifelse(colnames(ampaus) == "occupation", "Occupation",
                                  ifelse(colnames(ampaus) == "mothertongue", "L1",
                                         ifelse(colnames(ampaus) == "date_of_recording", "Date", 
                                                colnames(ampaus)))))
# remove superfluous columns
ampaus$number_of_subtexts <- NULL
ampaus$otherlanguages <- NULL
ampaus$surname <- NULL
ampaus$forename <- NULL
ampaus$no_of_participants <- NULL
ampaus$audience <- NULL
ampaus$audience_size <- NULL
ampaus$no_of_speakers <- NULL
ampaus$wordcount <- NULL
ampaus$place_of_recording <- NULL
ampaus$category <- NULL
ampaus$subject <- NULL
ampaus$version <- NULL
ampaus$free_comments <- NULL
ampaus$consent <- NULL
ampaus$communicative_situation <- NULL  
ampaus$tape_number <- NULL
ampaus$dubbed <- NULL
ampaus$transcribed <- NULL
ampaus$proof_allocation <- NULL
ampaus$proofed_i <- NULL
ampaus$recorder <- NULL
ampaus$organising_body <- NULL
ampaus$video_number <- NULL
ampaus$program <- NULL
ampaus$channel <- NULL
ampaus$mode <- NULL
ampaus$tv_radio <- NULL
ampaus$source_title <- NULL
ampaus$comments <- NULL
ampaus$address_1 <- NULL
ampaus$free_comments_1 <- NULL
ampaus$txtspk <- NULL
# inspect data
#str(ampaus); head(ampaus); nrow(ampaus)

###############################################################
# clean education
college <- c("Accountancy", "Arts/law Degree", "B.Comm", "BA", "BA Dip Ed", 
             "BA pend", "BA(ANU), MA(Alberta), MTCP(Syd), DipEd(UNSW)", "BA(Hons)", 
             "BA(Hons)(Oxon)", "BA(Hons)pending", "BA(Qld), Dip.Agric(Gratton)", 
             "BA(Qld), DipT (Nth Brisbane CAE)", "BA, LLB(Hons)(Melb), MA (Oxon)", 
             "BA,LLB(Hons)(Melb), MA (Oxon)", "BA/BBuspend", "BA/LLBpend", 
             "Bachelor of Nursing", "BADipEd", "BAHons", "BAHonspend", "BALLB", 
             "BApend", "BBuspend", "BCommLLBpe", "BCommpend", "BE(ChemHons)", "BEc", 
             "BEc(com), AASA, CPA", "BEc(Syd), DipEd(Sydney CAE), CPA", "BEc(UNE)", 
             "BEc(WAust), DipAg(Roseworthy SA)", "BEc,LLB(Monash)", "BEcpend", 
             "BEd Primary", "BEng", "BHHons(pend)", "BJuris, LLB(WA)", "BSc", 
             "BScpend", "BSocStud", "BusCollege", "College", "Degree", 
             "Dip.Phty, MAPA, LTCL", "Diploma", "Diploma in Design", "Educated SA", 
             "Engineering Diploma", "GradDippend", "GraDippend", "Higher degree", 
             "Home Economics Cert", 
             "Hon in Govt (Qld) Rhodes Schollar 1979, Hon Politics & Economics (Oxford)", 
             "Hon in Govt (Qld) Rhodes Schollar 1979,Hon Politics & Economics (Oxford)", 
             "HonDLitt", "HSC", "HSC & Dip Acting", "HSC & TAFE", "HSC&TAFE", "HSCpend", 
             "InterCert", "Intermediate Cert", "LeavCert?", "Leaving", "Leaving Certificate", 
             "LLB", "Llb pend", "LLBdropout", "LLBpend", "MA", "MA(hons)", "MA(Hons)", 
             "Matriculation", "MEd", "MScpend", "Not known", "Nursing Degree", "P/6 Level", 
             "Phd", "PhD", "PhDpend", "SA Matric.", "SC+TAFE management", "Solicitor", 
             "Tafe", "TAFE", "TAFE + part BSc", "Tafe Cert.", "TAFE&ManCert", "TeachCert", 
             "Teacher", "Teaching", "Tertiary", "Trade Certificate", "various", "Year 10", 
             "Yr11")
ampaus$Education <- ifelse(ampaus$Education %in% college, "college", ampaus$Education)
# recode occupation
# Academic, clerical, managerial professions
acmp <- c("Academic", "Accountant", "Actor", "Adm Assistant", 
          "Admin.Asst. Secretary", "Architect", "Artist", "Audio-visual Installer", 
          "BA Student", "Bank Teller", "Barrister", "Barrister/journalist", 
          "Burgular Alarm Surveyor", "Campus worker", "Cashier", "Cattle judge", 
          "Chef/Scientific Officer", "Chief librarian of ABC reference library.", 
          "Clergy", "Clerical Admin", "Clerk", "Co-ordinator, Sports Medicine Federation", 
          "Commemtator", "Commentator", "Commissioner for Federal Human Rights Commission.", 
          "Commonwealth Bank Manager", "Computer Analyst", "Computer Operator", 
          "Computer System Manager", "Consultant", 
          "Coordinator of course at Monash Law School", "Dental Assistant", "Dentist", 
          "Desktop Publisher", "Director", "Doctor", "Editor", "Editorial Officer", 
          "Education Consultant", "Electrial Linesman", "Electrical engineer", "Engineer", 
          "Entrepreneur", "Ex-Governor-General", "Ex-Prime Minister", 
          "Exec Dir, Aust School Sports Council", "Executive Secretary", 
          "Federal Treasurer (Member for Fremantle WA) (ALP)", "Financial Manager", 
          "Financial Planner", "First Aid instructor", "Head,Careers Service", 
          "High School Teacher", "Home duties", "Home Duties", "Home Economist", 
          "Home Maker", "House Wife", "Housewife", "Industrial Designer", 
          "Insurance contractor", "International Lawyer", "Investment advisor", 
          "Journalist", "Jurnalist", "Lawyer", "Leader of Opposition", "Lecturer", 
          "Legal Secretary", "Legal Tutor", "Manager", "Manager Finalcial Services", 
          "Mass Comm Student", "Member for Blaxland NSW (ALP) - Prime Minister", 
          "Member for Burdekin Qld (NP)", "Member for Cairns Qld (ALP)", 
          "Member for Caloundra Qld (LP)", "Member for Chatsworth Qld (ALP)", 
          "Member for Currumbin (ALP)", "Member for Drummoyne NSW (ALP)", 
          "Member for Flinders Vic (LP)", "Member for Gregory Qld (NP)", 
          "Member for Kalgoorlie WA (ALP)", "Member for Lismore NSW (NP)", 
          "Member for McKellar NSW (LP)", "Member for McPherson Qld (LP)", 
          "Member for Mirani Qld (NP)", "Member for Moreton Qld (ALP)", 
          "Member for Mundingburra (Qld)", "Member for Tamworth NSW (Ind)", 
          "NSW Employers Association", "NSW Labour Council", 
          "Nurse", "Nursing Student", "Park Ranger", "Pensioner", "Personel Manager", 
          "Pre-school Teacher", 
          "President of Australian Federation of Organisations for Aids.", 
          "Primary School Teacher", "Prime Minister", "Producer's assistant", 
          "Professor", "Psychologist", "Public Servant", "Remedial Therapist", 
          "Reporter", "Reporter.", "Res Asst/Pilot", "Research Assistant", 
          "Research Assitant", "Researcher", "Retired manager", 
          "Sales Manager", "School student", "School studnet", "Secretary", 
          "Senator", "Senator (Qld) (NP)", "Senator (W.A.) (LP)", "Senator (WA) (ALP)", 
          "Senator Vic (Minister for Foreign Affairs) (ALP)", "Senior Lecturer", 
          "Show host", "Show host.", "Snr Technical Officer", "Solicitor", 
          "Solicitor General", "Speaker of the House - Member for Cunningham NSW (ALP)", 
          "Sport Co-ordinator, Australian Sports Commission", "Stucent", "student", 
          "Student", "Student and sales assistant", 
          "Student/Assistant Accountant", "Student/Parttime Sales Assistant", 
          "Students", "TAFE Teacher", "Teacher", "Trainee accountant", 
          "Travel Agent", "Tutor", "Unemployed ex Agent-General", 
          "Waitress/Millinery Student", "Wine grower", "Writer")
# Skilled Manual Labour
sml <- c("Hairdresser", "Policemen", "Student and Kitchen Hand", "Student nurse", 
         "Telephonist", "Trainer", "Wildlife conservationist", 
         "Wood Machinest", "Zoo keeper.")
# unclassifiable
NAN <- c("Not known", "Retired", "Unemployed")
# reclassify
ampaus$Occupation <- ifelse(ampaus$Occupation %in% acmp, "acmp",
              ifelse(ampaus$Occupation %in% sml, "sml", NA))
# create columns with intensifier freqs
ampaus$very <- ifelse(ampaus$Variant == "very", 1, 0) 
ampaus$really <- ifelse(ampaus$Variant == "really", 1, 0) 
ampaus$so <- ifelse(ampaus$Variant == "so", 1, 0) 
ampaus$pretty <- ifelse(ampaus$Variant == "pretty", 1, 0) 
# inspect results
head(ampaus)

###############################################################
# code freq of adj type by age group
frqadjtb <- table(ampaus$Age, ampaus$Adjective)
relfreqadjtb <- round(prop.table(frqadjtb, margin = 1)*100, 5)
relfreqadjdf <- as.data.frame(relfreqadjtb)
colnames(relfreqadjdf)[1:2] <- c("Age", "Adjective")
# add freq by date to data
ampaus <- merge(ampaus, relfreqadjdf, by=c("Age", "Adjective"))
# reorder data
ampaus <- ampaus[order(ampaus$ID),]
# inspect data
head(ampaus)

###############################################################
# code gradability
# gradability - manual classification
# done by trained grad students
# add gradability
ngrd_manual <- c("abject", "able", "abrasive", "abstract", "absurd", "abundant", "abusive",
                 "accurate", "acrimonious", "active", "advanced", "adverse", "affectionate", "afraid",
                 "aged", "aggressive", "agile", "agitated", "aimless", "airy", "alert", "alleged",
                 "allusive", "amazing", "ambitious", "amused", "amusing", "ancient", "angry",
                 "annoying", "anxious", "appalling", "apparent", "appealing", "applicable", "applied",
                 "appreciative", "apprehensive", "approachable", "appropriate", "approving",
                 "arduous", "arrogant", "ashamed", "associated", "astute", "athletic", "atrocious",
                 "attitudinal", "attractive", "authentic", "authoritarian   ", "authoritative",
                 "available", "aware", "awesome", "awful", "awkward", "awry", "bad", "bare", "base",
                 "battered", "beautiful", "beloved", "benevolent", "benign", "besetting", "bitter",
                 "bizarre", "bleak", "bleary", "bloody", "blotchy", "bold", "boppy", "bored",
                 "boring", "bossy", "brave", "brief", "bright", "brilliant", "broad", "browsing",
                 "brutal", "bubbly", "burly", "buzzy", "callous", "calm", "campy", "candid",
                 "capable", "careful", "careless", "casual", "cautious", "ceremonial", "challenging",
                 "changed", "charismatic", "charming", "cheap", "circumspect", "civic", "civil",
                 "civilised", "classy", "clever", "cocky", "cold", "collective", "colossal", "colourful",
                 "comfortable", "commandeered", "committed", "compatible", "compelling", "competent",
                 "competitive", "complex", "complicated", "conceivable", "concentrated", "concerned",
                 "confident", "confidential", "confused", "confusing", "considerable", "constructive",
                 "consultative", "contrived", "controversial", "convenient", "conventional", "converted",
                 "convinced", "cool", "corrupt", "cosy", "coy", "cramped", "crass", "crazy", "creative",
                 "criminal", "crippling", "critical", "cross", "crowded", "crucial", "cruel", "cumbersome",
                 "curious", "cushy", "cute", "cynical", "damaged", "damaging", "damp", "dangerous",
                 "daring", "darkened", "darn", "daunting", "dear", "debatable", "decent", "dedicated",
                 "deep", "defective", "defensive", "delicate", "delicious", "delighted", "delightful",
                 "dense", "dependent", "depressed", "desirable", "despairing", "desperate", "despicable",
                 "despondent", "destructive", "detailed", "detrimental", "devilish", "difficult",
                 "dirty", "disabled", "disadvantaged", "disappointed", "disappointing", "disastrous",
                 "disenchanted", "disgraceful", "disgusting", "dishonest", "disparaging", "distant",
                 "distinguished   ", "distorted", "distressed", "disturbed", "disturbing", "dizzy",
                 "dodgy", "dominant", "dotty", "double", "doubtful", "downhill", "dramatic", "dreadful",
                 "driving", "drunk", "drunken", "ductile", "dull", "dumb", "dusty", "dylan", "dynamic",
                 "dynamical", "eager", "early", "earnest", "earthy", "easterly", "eastern", "easy",
                 "eccentric", "economic", "edible", "effective", "efficient", "elderly", "elegant",
                 "eligible", "elitist", "elusive", "embarrassed", "embarrassing", "emergent", "eminent",
                 "emotional", "emotive", "encouraging", "energetic", "enlightening", "enormous",
                 "entertaining", "enthusiastic", "epic", "erudite", "estimated", "estranged", "everyday",
                 "evil", "exact", "exceptional", "excessive", "excited", "exciting", "expensive",
                 "experienced", "expert", "explicit", "express", "expressive", "extended", "extensive",
                 "extraordinary", "extravagant", "extroverted", "fabulous", "facile", "factual", "faint",
                 "familiar", "famous", "fanatic", "fancy", "fantastic", "fascinating", "fast", "fastidious",
                 "fat", "favourable", "favoured", "fearful", "feisty", "fergal", "ferocious", "fertile",
                 "fierce", "fiery", "filthy", "fine", "finished", "finite", "firm", "fitting", "fizzy",
                 "flexible", "fluffy", "fluttering", "foggy", "foolish", "forceful", "formalised",
                 "formidable", "fortunate", "frank", "frantic", "fraudulent", "fraught", "frenzied",
                 "frequent", "friendly", "frightening", "frightful", "frustrated", "frustrating",
                 "fulsome", "fun", "funny", "furious", "generous", "gentle", "giant", "gifted",
                 "gigantic", "glad", "glib", "glorious", "glossy", "good", "goodhearted", "gorgeous",
                 "gracious", "gradual", "grand", "grandiose", "grateful", "grave", "greasy", "great",
                 "grim", "groggy", "groovy", "gross", "grubby", "guilty", "gutless", "habitual",
                 "handsome", "handy", "hapless", "happy", "hard", "hardy", "harmful", "harmless",
                 "harmonic", "harsh", "hazardous", "hazy", "heavy", "hectic", "helpful", "hideous",
                 "high", "hilarious", "holy", "honest", "honorable", "honorary", "honourable",
                 "hooked", "hopeful", "hopeless", "horrendous", "horrible", "horrific", "hostile",
                 "hot", "huge", "humble", "humorous", "hungry", "hurt", "hysterical", "idealistic",
                 "igneous", "ignorant", "imaginative", "immature", "immediate", "immense", "imperative",
                 "important", "impotent", "impressive", "inane", "incompetent", "inconsistent",
                 "incorporate", "incorporated", "increased", "incredible", "incredulous", "indecent",
                 "independent", "individual", "individualistic ", "ineffective", "ineffectual",
                 "inept", "inevitable", "inexorable", "inexpensive", "inexperienced", "infamous",
                 "infertile", "informal", "infuriating", "injured", "innovative", "insatiable",
                 "insecure", "insidious", "inspirational   ", "inspired", "instructive", "insuperable",
                 "integrated", "intellectual", "intelligent", "intense", "intensive", "intimate",
                 "intolerant", "invaluable", "inventive", "ironic", "irresponsible", "irritable",
                 "irritating", "itchy", "jealous", "joyful", "justified", "justifying", "keen",
                 "labour", "ladylike", "lame", "large", "late", "layered", "lazy", "lean", "legitimate",
                 "leisurely", "less", "liberal", "liberating", "light", "likely", "limp", "little",
                 "loath", "locating", "lone", "lonely", "long", "loony", "loud", "lousy", "lovely",
                 "low", "loyal", "lucky", "lumbering", "luminous", "lumpy", "lunatic", "lush",
                 "mad", "magic", "magnificent", "major", "mandatory", "manipulated", "marginal",
                 "marvellous", "massive", "matrimonial", "mean", "meaningful", "measurable", "medical",
                 "medicinal", "mediocre", "mere", "mighty", "mild", "minatory", "minded", "minor",
                 "minted", "miraculous", "miscellaneous", "misleading", "mixed", "mock", "modal",
                 "modern", "modest", "modesty", "momentous", "monetary", "monstrous", "moral", "motivating",
                 "muddy", "muggy", "multiple", "mutual", "mystical", "mythical", "naive", "narrow", "nasty",
                 "naughty", "near", "nearby", "neat", "necessary", "neglected", "negligent", "nervous",
                 "net", "new", "nice", "noble", "noisy", "normal", "northern", "nostalgic", "notable",
                 "noted", "noteworthy", "noxious", "numerous", "objective", "obnoxious", "obscure",
                 "observant", "odd", "off", "oily", "okay", "old", "oldfashioned", "operatic", "optimistic",
                 "orderly", "ordinary", "orientated", "oriented", "other", "outdated", "outrageous",
                 "outstanding", "over", "overhanging", "overwhelming", "painful", "parky", "parlous",
                 "passionate", "pathetic", "patronising", "patterned", "peaked", "peculiar", "perforated",
                 "perishable", "pernicious", "perplexed", "perplexing", "persistent", "personal",
                 "persuasive", "perverted", "pessimistic", "petite", "petty", "phenomenal", "picturesque",
                 "pinkish", "plain", "pleasant", "pleased", "pleasing", "pleasurable", "plenty",
                 "poetic", "polite", "poor", "popular", "possessive", "potent", "potential", "powerful",
                 "practical", "pragmatic", "preachy", "precarious", "precious", "precise", "predatory",
                 "predictable", "prepared", "prescriptive", "pressing", "prestigious", "presumptuous",
                 "pretentious", "pretty", "prevalent", "primitive", "privileged", "prodigious", "productive",
                 "professional", "profitable", "profligate", "progressive", "prominent", "promotional",
                 "prone", "proper", "proportionate", "prospective", "prosperous", "protective", "proud",
                 "provocative", "prudential", "psycho", "psychotic", "public", "puerile", "purposeful",
                 "quaint", "qualitative", "queer", "quick", "quiet", "racist", "radical", "rainy",
                 "rampant", "rank", "rapid", "rapt", "rare", "rational", "rattled", "raw", "reactionary",
                 "reactive", "ready", "realistic", "reasonable", "recognisable", "recognised",
                 "recreational", "reddish", "reduced", "refreshing", "regretful", "regular", "relaxed",
                 "relaxing", "relentless", "relevant", "reliable", "reluctant", "remote", "required",
                 "resourceful", "respected", "responsible", "restless", "revealing", "rich", "ridiculous",
                 "risky", "robust", "rocky", "romantic", "rotten", "rough", "rowdy", "rude", "rumbling",
                 "rusty", "sacred", "sad", "safe", "sandy", "sarcastic", "satisfied", "satisfying",
                 "savage", "scarce", "scared", "sceptical", "scientific", "scrappy", "scratchy",
                 "scruffy", "scurrilous", "secret", "secular", "secure", "sedate", "seduced", "seedy",
                 "seismic", "selfconfessed", "selfish", "selfreliant", "senile", "sensational",
                 "sensible", "sensitive", "sentimental", "serious", "severe", "sexist", "sexual",
                 "sexy", "shadowy", "shaky", "shaped", "sharp", "shiny", "shitty", "shocking",
                 "short", "sick", "sickly", "silly", "simple", "sizeable", "skilful", "skilled",
                 "sleepy", "slight", "slim", "slippery", "sloppy", "slow", "small", "smart", "snoopy",
                 "snotty", "sociable", "social", "soft", "soggy", "solid", "sophisticated", "sore",
                 "sorry", "sour", "south", "spare", "sparkling", "spectacular", "spectral", "spiritual",
                 "spiteful", "splendid", "sporting", "starkly", "startling", "staunch", "steady",
                 "steamy", "steep", "stellar", "sticky", "stiff", "stimulating", "stoical", "stormy",
                 "strange", "strategic", "stressful", "stretched", "strict", "striking", "strong",
                 "structured", "stubborn", "stunning", "stupid", "subject", "subtle", "successful",
                 "suffering", "suitable", "sunny", "super", "superficial", "superior", "supernatural",
                 "supportive", "suppressed", "sure", "surplus", "surprised", "surprising", "susceptible",
                 "suspicious", "sustainable", "sweaty", "sweet", "swift", "sympathetic", "tacky",
                 "tactic", "talented", "tall", "tantalising", "tasteful", "tedious", "teensy", "temperate",
                 "tempting", "tended", "tense", "tentative", "terrible", "terrific", "theatrical",
                 "theoretical", "thermal", "thick", "thickened", "thin", "thirsty", "thoughtful",
                 "threatening", "thriving", "tight", "tiny", "tired", "titanic", "tony", "top", "topical",
                 "torrential", "tortious", "tortured", "torturous", "tough", "touring", "tragic",
                 "transcendental", "transferable", "traumatic", "treacherous", "tremendous", "trendy",
                 "tricky", "trim", "triumphal", "trivial", "troubled", "twee", "twisted", "typical",
                 "ugly", "ulterior", "unable", "unattractive", "unaware", "unbeknown", "unbelievable",
                 "uncaring", "uncertain", "unclear", "unctuous", "undecided", "undeniable", "undifferentiated",
                 "undignified", "uneven", "unexpected", "unfair", "unfamiliar", "unfavourable",
                 "unfit", "unflattering", "unforced", "unfortunate", "ungrateful", "unhappy", "unholy",
                 "unified", "unknown", "unlikely", "unlucky", "unorthodox", "unpleasant", "unreal",
                 "unseemly", "unsmiling", "unsocial", "unsound", "unstable", "unusual", "upset",
                 "uptight", "urban", "urbanised", "urgent", "useful", "vague", "vain", "valiant",
                 "valuable", "variable", "varied", "vast", "venerated", "vengeful", "versatile",
                 "vested", "veteran", "viable", "vigorous", "vile", "violent", "virtual", "visionary",
                 "visual", "vital", "vivid", "vocal", "volatile", "vulnerable", "wakeful", "warm",
                 "wayward", "weak", "weakly", "wealthy", "weary", "wee", "weird", "wet", "wicked",
                 "wide", "widespread", "wild", "willing", "wise", "wishful", "witty", "wobbly",
                 "wonderful", "wondrous", "worried", "worthwhile", "worthy", "wounded", "young",
                 "yukky", "yummy")
grd_manual <- c("delusive", "abdominal", "aboriginal", "absent", "absolute", "academic",
                "accented", "acceptable", "accessible", "accomplished", "accountable", "acoustical",
                "acrylic", "actual", "additional", "adequate", "adjacent", "administrative",
                "adolescent", "advantageous", "aerial", "affected", "affirmative", "affordable",
                "african", "aggregate", "agricultural", "albanian", "alive", "allergic", "alternative",
                "ambiguous", "american", "analogous", "analytical", "ancestral", "anecdotal",
                "angled", "anglican", "announced", "annual", "anonymous", "antarctic", "apocryphal",
                "aqueous", "arbitrary", "archaeological", "archaic", "arctic", "armed", "armoured",
                "artificial", "artistic", "asian", "asthmatic", "atmospheric", "atomic", "aussie",
                "australian", "austrian", "authorised", "automatic", "autonomous", "average", "awake",
                "back", "backward", "baked", "balanced", "bald", "bankrupt", "basic", "bearded",
                "beneficial", "best", "biblical", "bibliographic", "binding", "biodegradeable",
                "biographical", "biological", "black", "blank", "blatant", "blind", "blonde", "blue",
                "bodily", "booed", "botanical", "bottom", "british", "broke", "broken", "brown",
                "bucketful", "budgetary", "bureaucratic", "burnt", "businesslike", "busy", "californian",
                "canonical", "capitalistic", "captive", "cardiac", "catholic", "cellular", "central",
                "centralised", "centred", "certain", "characteristic  ", "chartered", "cheated",
                "chemical", "chilean", "chinese", "chivalrous", "christian", "chromatic", "chronological",
                "churchy", "classic", "classical", "clean", "clear", "close", "closed", "coarse",
                "coated", "coherent", "cohesive", "coincidental", "colloquial", "coloured", "coming",
                "commercial", "common", "compact", "comparable", "complete", "compound", "comprehensive",
                "compulsory", "computerised", "conceptual", "concrete", "confessional", "confirmed",
                "conscious", "conservative", "consistent", "constant", "constituent", "contemporary",
                "contestable", "continual", "contraceptive", "contrary", "cooked", "cooking",
                "corporate", "correct", "cracked", "crushed", "cubic", "cultural", "curly", "current",
                "customary", "cut", "daily", "dark", "dead", "deadly", "deaf", "decisive", "definite",
                "definitive", "deliberate", "democratic", "demographic", "determined", "diagnostic",
                "diagonal", "dietetic", "different", "digestive", "digital", "diplomatic", "direct",
                "discursive", "displaced", "disqualified", "distinct", "distinctive", "diverse", "divine",
                "domestic", "down", "downward", "dry", "dual", "dubious", "dummy", "dutch", "east",
                "educational", "effluent", "egalitarian", "electable", "electric", "electrical", "electronic",
                "elemental", "empty", "endemic", "endless", "english", "enough", "enrolled", "entailed",
                "entire", "equal", "equatorial", "equestrian", "equitable", "equivalent", "eritrean",
                "essential", "estonian", "ethic", "ethiopian", "ethnic", "european", "ewen", "exalted",
                "excellent", "executive", "exiguous", "existent", "existing", "exotic", "expected",
                "experimental", "explosive", "exponential", "external", "extinct", "extra", "extreme",
                "fair", "fake", "false", "far", "fatal", "favourite", "federal", "federated",
                "fellow", "female", "feminist", "feudal", "few", "fictional", "final", "financial",
                "first", "fixed", "flagged", "flannelled", "flat", "fleet", "flowing", "fluent",
                "fluid", "focused", "folded", "folding", "following", "foreign", "foremost", "formal",
                "forthcoming", "forward", "fossil", "foster", "founding", "fragile", "free", "french",
                "fresh", "frisian", "front", "frontal", "frosted", "fucking", "full", "fundamental",
                "funded", "further", "future", "gaelic", "gay", "general", "generational", "generic",
                "genuine", "geographical", "geological", "geotechnical", "german", "germanic", "glandular",
                "global", "gold", "golden", "governmental", "granulitic", "graphical", "gray", "greek",
                "green", "grey", "guaranteed", "half", "halved", "halving", "healthy", "hereditary",
                "heterogeneous   ", "heterogenious", "hidden", "historic", "historical", "holistic",
                "homosexual", "hooped", "horizontal", "hourly", "human", "humanitarian", "humiliating",
                "hungary", "hydroplaning", "hypocritical", "hypothetical", "iambic", "ideal", "identical",
                "ideological", "idle", "ill", "illegal", "imaginable", "immune", "imperial", "implicit",
                "implied", "impossible", "improved", "inaccessible", "inaccurate", "inadequate", "inclusive",
                "incoming", "incorrect", "incumbent", "indian", "indifferent", "indigenous", "indispensable",
                "indisputable", "industrial", "inefficient", "inescapable", "inexplicable", "infallible",
                "inflatable", "inflated", "informed", "infrequent", "inherent", "initial", "innate",
                "inner", "innocent", "innumerable", "inorganic", "inside", "insignificant", "instant",
                "instrumental", "insufficient", "intact", "integral", "intentional", "interactive",
                "intercultural", "interested", "interesting", "internal", "international", "interrupted",
                "intervening", "intriguing", "intrinsic", "inverted", "iraq", "irish", "irrelevant",
                "irrespective", "islamic", "italian", "japanese", "jewish", "joint", "journalistic",
                "judicial", "judicious", "junior", "just", "last", "latter", "leading", "learned",
                "learnt", "left", "lefthand", "legal", "legged", "legislative", "lesbian", "liable",
                "lime", "limited", "linear", "linguistic", "liquid", "literary", "liturgical", "live",
                "loaded", "local", "logarithmic", "logical", "logistic", "lost", "macrocyclic",
                "magnetic", "main", "male", "marine", "marked", "married", "masqueraded", "masterly",
                "materialistic   ", "maternal", "mathematical", "mature", "maximum", "mechanistic",
                "medieval", "mega", "melodic", "mental", "messy", "metamorphic", "meterological",
                "metrical", "metropolitan", "mexican", "micro", "microeconomic", "mid", "middle",
                "militaristic", "military", "milky", "minimal", "minimalist", "minimum", "ministerial",
                "missionary", "mobile", "moderate", "molecular", "molten", "monotonous", "mundane",
                "muscovite", "musical", "mutant", "naked", "narrative", "nasal", "natal", "national",
                "nationwide", "native", "natural", "nautical", "naval", "nazi", "needy", "negative",
                "neurotic", "next", "nitric", "north", "noticeable", "now", "nuclear", "obligatory",
                "obvious", "occasional", "octave", "official", "olympic", "ongoing", "only", "onward",
                "open", "operational", "opposed", "opposite", "optical", "optimum", "optional", "oral",
                "orange", "orchestral", "orchestrated", "organic", "original", "outside", "overlapping",
                "pacific", "painless", "pakistani", "parallel", "paramount", "parental", "parliamentary",
                "partial", "particular", "partisan", "passive", "past", "pastoral", "paternal",
                "paternalistic", "patriarchal", "patriotic", "perfect", "peripheral", "permanent",
                "permissive", "pertinent", "peruvian", "philosophical", "phonetic", "physical", "pink",
                "plastic", "pluralistic", "polar", "political", "politicised", "polynesian", "pornographic",
                "portable", "positive", "possible", "practicable", "preconceived", "preferential",
                "preferred", "pregnant", "preliminary", "presbyterian", "present", "presidential",
                "previous", "prewarned", "priceless", "primary", "prime", "principal", "prior", "pristine",
                "private", "privatised", "probable", "procedural", "programmed", "prolonged", "pronged",
                "proportional", "provincial", "psychiatric", "psychic", "pure", "purple", "quantifiable",
                "quantitative", "racial", "radioactive", "random", "readable", "real", "rear", "recent",
                "recycled", "red", "redundant", "reformed", "regional", "registered", "regulated",
                "regulatory", "reissued", "related", "relational", "relative", "remarkable", "remedial",
                "reportable", "reported", "residential", "respective", "resulting", "retrospective",
                "reusable", "reverse", "revolutionary", "ridged", "right", "rightful", "righthand",
                "rigid", "rigorous", "romanian", "rotary", "round", "royal", "ruined", "rural",
                "russian", "same", "samoan", "sane", "saturated", "scandinavian", "scholastic",
                "scottish", "scriptural", "seasonal", "secondary", "securing", "selected", "selective",
                "selfstyled", "senior", "senseless", "separate", "separated", "serial", "sheer",
                "siberian", "significant", "silver", "similar", "simultaneous", "sincere", "singaporean",
                "single", "skinned", "sleeveless", "sliced", "smokefree", "smooth", "sober", "socialist",
                "sociodemographic", "socioeconomic", "sole", "solitary", "soluble", "southern",
                "southwest", "sovereign", "soviet", "spanish", "special", "specialised", "specific",
                "spinal", "spontaneous", "spurious", "square", "stable", "stagnant", "standard",
                "stated", "stationary", "statistical", "statutory", "steely", "stereo", "stolen",
                "straight", "stratospheric", "striped", "structural", "subconscious", "subordinate",
                "subset", "substantial", "substantive", "suburban", "sudden", "sufficient", "suggestive",
                "sundry", "superheated", "supplementary", "supreme", "surgical", "sustained", "swedish",
                "swiss", "swollen", "symbolic", "synthetic", "technical", "technological", "temporary",
                "terminal", "territorial", "textual", "textural", "thematic", "thorough", "thoroughgoing",
                "timely", "total", "totalitarian", "toxic", "traditional", "transmitted", "traversable",
                "true", "twin", "ultimate", "unacceptable", "unaffected", "unallocated", "unannounced",
                "unanswered", "unbeaten", "unbiased", "unblemished", "unchanged", "uncoordinated",
                "under", "undisclosed", "undone", "unemployed", "unequal", "unexpired", "unfilled",
                "unfurnished", "unique", "universal", "unlimited", "unnatural", "unnecessary", "unoccupied",
                "unofficial", "unplayable", "unpopular", "unprecedented", "unprejudiced", "unpretentious",
                "unpromising", "unreceptive", "unregulated", "unrelated", "unresolved", "unrhymed",
                "unseeded", "unseen", "unselective", "unselfish", "unspecified", "unspoilt", "unstressed",
                "unsubsidised", "untold", "untrue", "unvarnished", "unwanted", "unwarranted", "unwilling",
                "unwrinkled", "upward", "usable", "useless", "usual", "utter", "vacant", "valid",
                "various", "veiled", "venetian", "verbal", "verbatim", "verifiable", "vertical",
                "volcanic", "voluntary", "weekly", "west", "western", "white", "whole", "wilful",
                "wooden", "woollen", "written", "wrong", "yellow", "youthful", "religious")
# gradability - data driven classification
# determine which adjectives are gradable
alladj <- names(table(ampaus$Adjective)) 
grd1 <- names(table(ampaus$Adjective[ampaus$Variant == "very"])) # adjs with very are gradable
grd2 <- names(table(ampaus$Adjective[ampaus$Variant == "extremely"])) # adjs with extremely are gradable
ngrd1 <- names(table(ampaus$Adjective[ampaus$Variant == "completely"])) # adjs with completely are gradable
ngrd2 <- names(table(ampaus$Adjective[ampaus$Variant == "total"])) # adjs with total are gradable
ngrd3 <- names(table(ampaus$Adjective[ampaus$Variant == "totally"])) # adjs with totally are gradable
ngrd4 <- names(table(ampaus$Adjective[ampaus$Variant == "utterly"])) # adjs with utterly are gradable
ngrd5 <- names(table(ampaus$Adjective[ampaus$Variant == "absolutely"])) # adjs with absolutely are gradable
# create vector with gradable adjectives
grdadj <- intersect(grd1, grd2)
# create vector with non-gradable adjectives
ngrdadj <- c(ngrd1, ngrd2, ngrd3, ngrd4, ngrd5)
ngrdadj <- names(table(ngrdadj))
# find elements that occur in both groups
bthgrdngrd1 <- grdadj[grdadj %in% ngrdadj]
bthgrdngrd2 <- ngrdadj[ngrdadj %in% grdadj]
bthgrdngrd <- names(table(c(bthgrdngrd1, bthgrdngrd2)))
# extract adjs that are clearly gradable 
grd_datadriven <- grdadj[!grdadj %in% bthgrdngrd]
# extract adjs that are clearly not gradable 
ngrd_datadriven <- ngrdadj[!ngrdadj %in% bthgrdngrd]
# find elements that are neither in gradable nor in nongradable
gradnongrad <- names(table(c(grd_datadriven, ngrd_datadriven)))
nagrad <- alladj[!alladj %in% gradnongrad]
naadjs <- names(table(c(nagrad, bthgrdngrd)))
###############################################################
# combine data driven and manual classification
# adj unclassified by datadriven now assigned value based on manual coding
grd_add <- naadjs[naadjs %in% grd_manual]
ngrd_add <- naadjs[naadjs %in% ngrd_manual]
# combine data driven and manual coding
grdnze <- names(table(c(grd_datadriven, grd_add)))
ngrdnze <- names(table(c(ngrd_datadriven, ngrd_add)))
# check which adjs are still unclassified
nagradnze1 <- alladj[!alladj %in% grdnze]
nagradnze <- nagradnze1[!nagradnze1 %in% ngrdnze]
# inspect unclassified adj
#nagradnze

# inspect length of vectors
length(alladj); length(grdnze); length(ngrdnze); length(nagradnze)

# add gradability coding
ampaus$Gradabilty <- ifelse(ampaus$Adjective %in% grdnze, "Gradable", ampaus$Adjective)
ampaus$Gradabilty <- ifelse(ampaus$Gradabilty %in% ngrdnze, "NotGradable", ampaus$Gradabilty)
ampaus$Gradabilty <- ifelse(ampaus$Gradabilty  == "Gradable" | ampaus$Gradabilty  == "NotGradable", 
                            ampaus$Gradabilty, "GradabilityUndetermined")
# inspect data
head(ampaus); table(ampaus$Gradabilty)

# save table of gradable and non-gradable adj
gradtb <- table(ampaus$Adjective, ampaus$Gradabilty)
gradtb <- data.frame(gradtb)
colnames(gradtb) <- c("adj", "grad", "freq")
head(gradtb)

# dave table to file
write.table(gradtb, "gradtb.txt", sep = "\t", row.names = T)
###############################################################
# add semantic types (tagliamonte 2008, based on dixon 1977)
# dimension = semdim (e.g. big, large, little, small, long, short, wide, narrow, thick)
# difficulty = semdif (e.g. difficult, simple)
# physical property = (e.g. hard, soft, heavy, light, rough, smooth, hot, sweet)
# color = semcol (e.g. black, white, red)
# human propensity: semhup (e.g. jealous, happy, kind, clever, generous, gay, rude)
# age = semage (e.g. new, young, old) 
# value (e.g. good, bad, proper, perfect, excellent, delicious, poor), 
# speed Speed (fast, quick, slow)
# position (e.g. right, left, near, far)
# other

# age
semage <- c("actual", "adolescent", "aged", "ancestral", "ancient", "annual", "archaeological", "archaic", 
            "biographical", "contemporary", "elderly", "foster", "generational", "historic", "historical", 
            "immature", "junior", "late", "mature", "medieval", "modern", "old", "oldfashioned", "outdated", 
            "past", "preliminary", "present", "primary", "prime", "prior", "puerile", "recent", "seasonal", 
            "senile", "senior", "temporary", "topical", "veteran", "young", "youthful")
# color
semcol <- c("colourful", "darkened", "pinkish", "reddish", "black", "blue", "brown",
            "coloured", "dark", "gold", "golden", "gray", "green", "grey", "lime", "marine",
            "orange", "pink", "purple", "red", "silver", "white", "yellow")
semdif <- c("basic", "complicated", "difficult", "easy", "elusive", "facile", "precarious",
            "risky", "simple", "stressful", "tricky", "twisted", "unpromising")
# dimension
semdim <- c("adjacent", "angled", "arctic", "back", "backward", "big", "bottom", "brief", "bright", 
            "broad", "central", "centralised", "centred", "close", "compact", "deep", "diagonal", 
            "direct", "distant", "distorted", "down", "downward", "early", "east", "easterly", "eastern", 
            "endemic", "endless", "equatorial", "european", "ewen", "far", "few", "first", "flat", 
            "foreign", "foremost", "forthcoming", "forward", "free", "front", "frontal", "further", 
            "geographical", "giant", "gigantic", "global", "grand", "half", "halved", "halving", 
            "high", "horizontal", "huge", "inner", "inside", "internal", "international", "large", 
            "last", "latter", "left", "linear", "little", "local", "locating", "long", "low", "massive", 
            "micro", "mid", "middle", "minimal", "minimalist", "minimum", "minor", "misleading", 
            "narrow", "national", "nationwide", "native", "near", "nearby", "next", "north", "northern", 
            "off", "onward", "orientated", "outside", "over", "overhanging", "overlapping", "pacific", 
            "parallel", "paramount", "peripheral", "petite", "polar", "proportional", "provincial", 
            "public", "rear", "regional", "remote", "reverse", "round", "rural", "separate", 
            "separated", "short", "sizeable", "slight", "small", "south", "southern", "southwest", 
            "spinal", "square", "steep", "stratospheric", "suburban", "super", "tall", "teensy", 
            "terminal", "territorial", "thick", "thickened", "thin", "tight", "tiny", "titanic", 
            "top", "torrential", "touring", "tremendous", "under", "universal", "unseeded", "upward", 
            "urban", "urbanised", "vast", "vertical", "warped", "wee", "west", "western", "wide", "widespread")
semhup <- c("able", "abrasive", "abusive", "academic", "accomplished", "advanced", "adverse", "afraid", 
            "aggressive", "aimless", "amused", "amusing", "analytical", "angry", "anxious", "appreciative", 
            "apprehensive", "ashamed", "astute", "aware", "benevolent", "besetting", "bold", "bossy", 
            "brave", "brutal", "busy", "callous", "candid", "capable", "careful", "challenging", 
            "charismatic", "cheated", "clever", "cocky", "compelling", "competent", "competitive", 
            "concerned", "confident", "consultative", "convinced", "creative", "cross", "cruel", "cute", 
            "cynical", "delighted", "depressed", "despairing", "desperate", "despondent", "disappointed", 
            "dodgy", "dotty", "dubious", "dull", "dumb", "eager", "elitist", "embarrassed", "encouraging", 
            "entertaining", "enthusiastic", "erudite", "evil", "excited", "fanatic", "fearful", "ferocious", 
            "fierce", "foolish", "forceful", "fortunate", "fraudulent", "friendly", "frustrated", "fun", 
            "funny", "furious", "generous", "gifted", "glad", "goodhearted", "gracious", "grateful", 
            "grim", "gross", "gutless", "hapless", "happy", "hopeful", "hopeless", "horrible", "hostile", 
            "hysterical", "ignorant", "ill", "imperative", "incompetent", "inexorable", "inexperienced", 
            "infallible", "informed", "insatiable", "insidious", "intellectual", "intelligent", "intriguing", 
            "inventive", "jealous", "joyful", "keen", "lazy", "learned", "learnt", "loath", "lone", "lonely", 
            "lucky", "lunatic", "mad", "mean", "minded", "motivating", "nasty", "nervous", "nice", 
            "optimistic", "passionate", "patronising", "pessimistic", "pleased", "polite", "poor", 
            "preachy", "prepared", "presumptuous", "primitive", "procedural", "professional", "promotional", 
            "proud", "prudential", "psycho", "puzzled", "rapt", "rational", "regretful", "relentless", 
            "resourceful", "respected", "rich", "romantic", "rowdy", "rude", "sad", "sane", "sarcastic", 
            "satisfied", "satisfying", "scared", "sceptical", "selective", "selfish", "sensitive", 
            "sentimental", "sick", "silly", "skilful", "skilled", "smart", "snotty", "sociable", "sophisticated", 
            "sorry", "sovereign", "spiteful", "staunch", "strategic", "strict", "stubborn", "stupid", 
            "suffering", "superior", "supportive", "suspicious", "tactic", "talented", "technical", "treacherous", 
            "troubled", "unable", "unanswered", "unaware", "uncaring", "ungrateful", "unhappy", "unsmiling", 
            "unsocial", "upset", "valiant", "valid", "vengeful", "vile", "wicked", "willing", "wise", "witty", 
            "worried")
# physical property
semphy <- c("cheap", "clear", "cold", "comfortable", "cool", "dark", "different", "dry", "flexible", "hard", 
            "heavy", "hot", "light", "neat", "obvious", "quick", "quiet", "real", "same", "scarce", "similar", 
            "slow", "strong", "sweet", "tidy", "warm")
# Value
semval <- c("amazing", "appropriate", "awful", "bad", "beautiful", "bizarre", "boring", "brilliant", 
            "competitive", "counterproductive", "easy", "effective", "efficient", "essential", "excellent", 
            "exciting", "expensive", "fantastic", "fat", "good", "great", "important", "interesting", 
            "new", "original", "painful", "pathetic", "popular", "relevant", "ridiculous", "right", 
            "scary", "serious", "simple", "special", "strange", "sure", "terrible", "tough", "trendy", "true", 
            "unrealistic", "unusual", "useful", "useless", "weird", "worser", "worthwhile", "wrong", "yummy")
# add semantic type classification
ampaus$SemanticCategory <- ifelse(ampaus$Adjective %in% semage, "Age", ampaus$Adjective)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semcol, "Color", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semdif, "Difficulty", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semdim, "Dimension", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semhup, "HumanPropensity", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semphy, "PhysicalProperty", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory %in% semval, "Value", ampaus$SemanticCategory)
ampaus$SemanticCategory <- ifelse(ampaus$SemanticCategory == "Age" | ampaus$SemanticCategory == "Color" | ampaus$SemanticCategory == "Difficulty" | 
                                    ampaus$SemanticCategory == "Dimension" | ampaus$SemanticCategory == "HumanPropensity" |
                                    ampaus$SemanticCategory == "PhysicalProperty" | ampaus$SemanticCategory == "Value",  ampaus$SemanticCategory, "NoSemType")
# table sem class of tokens
table(ampaus$SemanticCategory)

# check classification
names(table(ampaus$Adjective[which(ampaus$SemanticCategory == "NoSemType")]))

# inspect data
head(ampaus); nrow(ampaus); length(table(ampaus$Adjective))

###############################################################
# load library
library(syuzhet)
# code emotion
class_emo <- get_nrc_sentiment(ampaus$Adjective)
# process sentiment
ampaus$Emotionality <- as.vector(unlist(apply(class_emo, 1, function(x){
  x <- ifelse(x[9] == 1, "NegativeEmotional",
              ifelse(x[10] == 1, "PositiveEmotional", "NonEmotional")) } )))
# revert order of factor Emotionality
ampaus$Emotionality <- factor(ampaus$Emotionality, levels = c("NonEmotional", "NegativeEmotional", "PositiveEmotional"))
# inspect data
head(ampaus); str(ampaus); nrow(ampaus)

###############################################################
# save raw data to disc
write.table(ampaus, "ampaus04_clean.txt", sep = "\t", row.names = F)
###############################################################
#                        END PART 2
###############################################################
