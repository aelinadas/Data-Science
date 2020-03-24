# Basic installation packages
install.packages("dpylr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
Needed <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("udpipe")
library(udpipe)
install.packages("tm")
library(tm)
install.packages("tibble")
library(tibble)

#UDpipe for Hindi language

model <- udpipe_download_model(language = "hindi")
udmodel_hindi <- udpipe_load_model(file = 'hindi-hdtb-ud-2.4-190531.udpipe')
hinditxt <- readLines(con <- file(description = "hindi.txt", encoding = "utf-8"),n = -1L)
hinditxt
install.packages("stopwords")
library(stopwords)
annotatedData <- udpipe_annotate(udmodel_hindi, hinditxt)
annotatedData <- data.frame(annotatedData)
sorted_freq <- sort(table(annotatedData$token), decreasing=T)
table(annotatedData$upos)
annotatedData$upos
library(utf8)
library(lattice)
stats <- txt_freq(annotatedData$upos)

#Universal Parts of Speech Chart

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "blue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#Nouns Frequency chart

stats <- subset(annotatedData, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "blue", 
         main = "Most occurring nouns", xlab = "Frequency")

#Adjective Frequency chart

stats <- subset(annotatedData, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "blue", 
         main = "Most occurring adjectives", xlab = "Frequency")

#Verbs Frequency chart

stats <- subset(annotatedData, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "blue", 
         main = "Most occurring Verbs", xlab = "Frequency")

#Rake Chart

stats <- keywords_rake(x = annotatedData, term = "lemma", group = "doc_id",
                       relevant = annotatedData$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 0), 20), col = "blue",
         main = "Keywords Identified by RAKE", xlab = "Rake")

#Simple noun-phrases chart

annotatedData$phrase_tag <- as_phrasemachine(annotatedData$upos, type = "upos")
stats <- keywords_phrases(x = annotatedData$phrase_tag, term = tolower(annotatedData$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "blue",
         main = "Keywords - simple noun phrases", xlab = "Frequency")

library(wordcloud)

#Word Cloud
par(bg="black")
wordcloud(words = stats$key, freq = stats$freq, min.freq = 3, max.words = 1000,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"), scale= c(0.8,0.9))
