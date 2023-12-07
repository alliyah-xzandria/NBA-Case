# Title:  NBA 2019 & 2020 Tweet Analysis  
# Author: Alliyah Thompson


######## 
# set your working directory
setwd("/Users/alliyahthompson/Documents/Text Analytics/Text-Mining-NLP/Case/Case I/Data")

# Load packages 
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(dplyr)
library(stringi)
library(qdap)
library(wordcloud)
library(wordcloud)
library(RColorBrewer)
library(ggdendro)
library(wordcloud2) 


######## 
# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

######## 

# Functions & Stop Words
options(stringsAsFactors = FALSE)

# TrytoLower - convert all text to the same lowercase  
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Bigram token maker - analyzing the context and correlations around words
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Corpus - cleaning the tweets from the csv
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# words that are likely to occur in the data that we want to remove from the analysis 
fan_words<- c(stopwords("SMART"),
                  "nba", "coach", "game","win","fan","playoffs","rookie", "quarter",
                  "tickets","nike","league","team","lebron","heat", "bam", "playoffs",
                  "points", "games", "finals", "season", "team", "lost", "home", "arena",
                  "championship", "won", "win", "tonight", "play","evidence",  
                  "nbatwitter", "dunk", "announcement", "allstar", "bubble", "disney", "showtime",
                   "trade","rigged", "cheat","players", 
                    "ttyl", "smh", "rly", "bball", "omg")

######## 
# Reading the data for Feb2020 & Oct2020

# Feb2020 - selected this month because of AllStar events and fan selection
# Oct2020 - due to COVID championships were delayed in 2020 and began later

Allstar <- read.csv('E_Feb2020.csv', encoding = "UTF-8")
Allstar_sample<- slice_sample(Allstar, prop = 0.05)

Champ <- read.csv('M_Oct2020.csv', encoding = "UTF-8")
Champ_sample <- slice_sample(Champ, prop = 0.05)


########  
# Tweets about Players 

# Allstar Data

# Lebron James - Team Captain 
LBJ    <- grepl("lebron", Allstar$text, ignore.case=TRUE)

# LBJ shows up 1.6 % of Allstar tweets
sum(LBJ) / nrow(Allstar)

# Giannis Antetokounmpo - Team Captain
Giannis   <- grepl("giannis", Allstar$text, ignore.case=TRUE)

# Giannis shows up 1.2 % of the tweets
sum(Giannis) / nrow(Allstar)


# Anthony Davis - Team Lebron
Anthony   <- grepl("anthony davis", Allstar$text, ignore.case=TRUE)

# Anthony shows up 0.1 % of the tweets
sum(Anthony) / nrow(Allstar)


# Jimmy Butler - Team Giannis 
Jimmy  <- grepl("jimmy butler", Allstar$text, ignore.case=TRUE)

# Jimmy shows up 0.4 % of the tweets
sum(Jimmy) / nrow(Allstar)


# Tyler Herro - not chosen to play
Tyler  <- grepl("tyler", Allstar$text, ignore.case=TRUE)

# Tyler shows up 0.4% of the tweets
sum(Tyler) / nrow(Allstar)



# Championship Data 
##### LA LAKERS   

#  LA LAKERS PLAYERS
latweets <- grepl("lakers", Champ$text, ignore.case=TRUE)

# Lakers shows up 16 % of the tweets
sum(latweets) / nrow(Champ)


# Lebron James - LA Lakers
LBJchamp    <- grepl("lebron", Champ$text, ignore.case=TRUE)

# LBJ shows up 3.29 % of Champ tweets
sum(LBJchamp) / nrow(Champ)

# Anthony Davis also known as the brow - LA Lakers
Anthonychamp <- grepl("anthony davis", Champ$text, ignore.case=TRUE)

# Anthony shows up 0.8 % of the tweets
sum(Anthonychamp) / nrow(Champ)


##### MIAMI HEAT PLAYERS

#  Miami Heat 
miamitweets <- grepl("heat", Champ$text, ignore.case=TRUE)

# Heat shows up 10 % of the tweets
sum(miamitweets) / nrow(Champ)


# Tyler Herro - Miami Heat
Tylerchamp  <- grepl("tyler", Champ$text, ignore.case=TRUE)

# Tyler shows up 0.4% of the tweets
sum(Tylerchamp) / nrow(Champ)


# Jimmy Butler -  Miami Heat
Jimmychamp  <- grepl("jimmy butler", Champ$text, ignore.case=TRUE)

# Jimmy shows up 1.1% of the tweets
sum(Jimmychamp) / nrow(Champ)


######## 
# Frequency table:

# Used to determine which team occurs the most in the data

# Feb2020

sort(table(Allstar_sample$team), decreasing=TRUE)

# The frequency of the full data of Feb, 2020
# Top Teams:
# 1.Miami Heat:        28,039 
# 2.Boston Celtics:    27,081 
# 3.LA Lakers:         25,801
# 4.Toronto Raptors:    25,090
# 5.Houston Rockets :    24,206 


# Oct2020
sort(table(Champ_sample$team), decreasing=TRUE)

# The frequency of the full data of October, 2020
# Top Teams:
# 1.Miami Heat:         28,109 
# 2.LA Lakers:          25,882 
# 3.Houston Rockets :   20,045
# 4.Boston Celtics:     17,447
# 5.Toronto Raptors:    14,949 


######## 
# Subsetting

#Subsetting the tweets of the most reoccurring team in Allstar
Allstar_teams <- subset(Allstar_sample, team %in% c("Miami Heat", "Boston Celtics"))

#Subsetting the tweets of the most reoccurring team in Champs
Champ_teams <- subset(Champ_sample, team %in% c("Miami Heat", "LA Lakers"))


######## 
# Cleaning the sample size data of allstars and champ

#Clean and Organize Allstar_team 
CorpusTxt1 <- VCorpus(VectorSource(Allstar_teams$text))
CorpusTxt1 <- cleanCorpus(CorpusTxt1, fan_words)
fanTDM  <- TermDocumentMatrix(CorpusTxt1)
fanTDMm <- as.matrix(fanTDM)


#Clean and Organize Champ_team
CorpusTxt <- VCorpus(VectorSource(Champ_teams$text))
CorpusTxt <- cleanCorpus(CorpusTxt, fan_words)
champfanTDM  <- TermDocumentMatrix(CorpusTxt)
champfanTDMm <- as.matrix(champfanTDM)


######## 
#### Word Association  

# Inspect word associations - Allstar 
associations_allstar <- findAssocs(fanTDM , 'west', 0.30)
associations_allstar

# Organize the word associations for Allstar
assocDF <- data.frame(terms=names(associations_allstar[[1]]),
                     value=unlist(associations_allstar))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Inspect word associations - Champ
associations_game <- findAssocs(champfanTDM, 'title', 0.30)
associations_game

associations_champ2 <- findAssocs(champfanTDM, 'show', 0.30)
associations_champ2

# Organize the word associations for Champ 
assocGameDF <- data.frame(terms=names(associations_game[[1]]),
                            value=unlist(associations_game))
assocGameDF$terms <- factor(assocGameDF$terms, levels=assocGameDF$terms)
rownames(assocGameDF) <- NULL
assocGameDF

assocChamp2DF <- data.frame(terms=names(associations_champ2[[1]]),
                          value=unlist(associations_champ2))
assocChamp2DF$terms <- factor(assocChamp2DF$terms, levels=assocChamp2DF$terms)
rownames(assocChamp2DF) <- NULL
assocChamp2DF


######## 
# Make a dot plot - all star
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#0B66DE') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="blue",hjust="inward", vjust ="inward" , size=3) 

# Make a dot plot - Champs
# title
ggplot(assocGameDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocGameDF, col='#FFF833') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="purple",hjust="inward", vjust ="inward" , size=3) 

# show 
ggplot(assocChamp2DF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocChamp2DF, col='#FFF833') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="purple",hjust="inward", vjust ="inward" , size=3) 


######## 
# Cleaning the data to utilize bigram tokens in each dataset

# Make a volatile corpus - Allstar
CorpusTxt3 <- VCorpus(DataframeSource(Allstar_teams))

# Preprocess the corpus
CorpusTxt3 <- cleanCorpus(CorpusTxt3, fan_words)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
fan1TDM  <- TermDocumentMatrix(CorpusTxt3, 
                                 control=list(tokenize=bigramTokens))
fan1TDMm <- as.matrix(fan1TDM)


# Make a volatile corpus - Champ
CorpusTxt2 <- VCorpus(DataframeSource(Champ_teams))

# Preprocess the corpus
CorpusTxt2 <- cleanCorpus(CorpusTxt2, fan_words)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
champfan2TDM <- TermDocumentMatrix(CorpusTxt2, 
                              control=list(tokenize=bigramTokens))
champfan2TDMm <- as.matrix(champfan2TDM )

######## 
# See a bi-gram - All star
bigramtweet <- grep('west', rownames(fan1TDMm))
fan1TDMm[(bigramtweet-2):(bigramtweet),870:871]

# See a bi-gram - Champ
bigramtweetchamp <- grep('la', rownames(champfan2TDMm))
champfan2TDMm[(bigramtweetchamp-2):(bigramtweetchamp),870:871]


######## 
# Get Row Sums & organize - Allstar
fanTDMv <- sort(rowSums(fanTDMm), decreasing = TRUE)
fan_DF   <- data.frame(word = names(fanTDMv), freq = fanTDMv)

# Get Row Sums & organize - Champ
champTDMv <- sort(rowSums(champfan2TDMm), decreasing = TRUE)
champ_DF   <- data.frame(word = names(champTDMv), freq = champTDMv)

######## 
# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Greens")
pal <- pal[-(1:2)]

# Make simple word cloud - Allstar
# Reminder to expand device pane
set.seed(1234)
wordcloud(fan_DF$word,
          fan_DF$freq,
          max.words    = 100,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# Choose a color & drop light ones
pal2 <- brewer.pal(8, "Spectral")
pal2 <- pal[-(1:2)]

# Make simple word cloud - Champ
# Reminder to expand device pane
set.seed(1234)
wordcloud(champ_DF$word,
          champ_DF$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal2,
          scale        = c(2,1))

########  

# Reduce TDM - All Star 
reducedTDM <- removeSparseTerms(fanTDM, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))


# Reduce TDM - Champ
reducedTDM1 <- removeSparseTerms(champfanTDM, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM1

# Organize the smaller TDM
reducedTDM1 <- as.data.frame(as.matrix(champfanTDM))


######## 
# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 


# Allstar
assocText <- rm_url(Allstar_teams$text)

# Champs
assocText1 <- rm_url(Champ_teams$text)

# Allstar QDAP
word_associate(assocText, 
               match.string = 'west', 
               stopwords = fan_words,
               network.plot = T,
               nw.label.proportional = T,
               cloud.colors = c('black','darkred'))

# Champs QDAP
word_associate(assocText1, 
               match.string = 'lbj', 
               stopwords = fan_words,
               wordcloud = T,
               cloud.colors = c('purple','black'))


# End 