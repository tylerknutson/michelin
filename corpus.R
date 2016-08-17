library(tm)
library(ngram)
library(quanteda)


# title_corpus = create_corpus(master_df$event_title)
# title_freq = word_freq(title_corpus)
# pal = brewer.pal(9,"GnBu")[-(1:4)]
# wordcloud(title_corpus, max.words = 100, random.order = FALSE, colors=pal)
# 
# 
# a <- Corpus(text_source)
# summary(a)  
# a <- tm_map(a, removeNumbers)
# a <- tm_map(a, removePunctuation)
# a <- tm_map(a , stripWhitespace)
# a <- tm_map(a, tolower)
# a <- tm_map(a, removeWords, stopwords("english")) 
# # a <- tm_map(a, stemDocument, language = "english") 
# # I also got it to work with stemming, but it takes so long...
# adtm <- as.matrix(DocumentTermMatrix(a))
# adtm <- removeSparseTerms(adtm, 0.75)
# 
# inspect(adtm) 
# 
# 
# 
# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tdm <- TermDocumentMatrix(a, control = list(tokenize = TrigramTokenizer))
# 
# 
# 
# 
# # declare words to be removed from frequency count
# 
# 
# # function to create corpus for counting word frequency
# create_corpus = function(value) {
#   # remove quotes from the charaters
#   text = noquote(value)
#   # create a master string containg all the word
#   text = paste(text, collapse=" ")
#   text_source = VectorSource(text)
#   corpus = Corpus(text_source)
#   # remove punctuation
#   corpus = tm_map(corpus, removePunctuation) 
#   # convert all words to lower case
#   corpus = tm_map(corpus, content_transformer(tolower)) 
#   # remove basic stop words, check out stopwords("english") for the full list
#   corpus = tm_map(corpus, removeWords, stopwords("english"))
#   # remove words indicating location, meetup, weekday, month, year
#   corpus = tm_map(corpus, removeWords, word_to_remove) 
#   return (corpus)
# }
# 
# 
# 
# #do ngram tokenizer before cleanup to preserve order
# 
# # function to get the word frequency
# word_freq = function(corpus){
#   dtm = as.matrix(DocumentTermMatrix(corpus)) 
#   frequency = sort(colSums(dtm), decreasing = TRUE)
#   return (frequency)
# }

lost_star_pos = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.michelin_stars = 0 and m.review_rating in (4,5)
      ;')


one_star_pos = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.michelin_stars = 1  and m.review_rating in (4,5)
      ;')

two_star_pos = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.michelin_stars = 2  and m.review_rating in (4,5)
      ;')

three_star_pos = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.michelin_stars = 3  and m.review_rating in (4,5)
;')

all_michelin = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.michelin_stars in (1,2,3)
;')

all_lost_event = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.event_flag2 = "Lost Star"
;')

all_gained_event = sqldf('
select
m.review_content
from m inner join s on m.restaurant_name = s.restaurant_name and m.michelin_year = s.michelin_year
where s.event_flag2 = "Gained Star"
;')

nm_trestle = sqldf('
select
nm.review_content
from nm where nm.restaurant_name = "Trestle"
;')




word_to_remove = c("french", "laundry", "thomas", "keller", "gary", "danko", "san", "francisco",
                   "state", "bird", "chez", "panisse", "atelier", "crenn", "michael", "mina")


pos_1 <- tokenize(toLower(one_star_pos$review_content), removePunct = TRUE, ngrams = 2)

pos_1_dfm = dfm(pos_1, ignoredFeatures = c(word_to_remove, stopwords("english")))

pos1_top20 <- topfeatures(pos_1_dfm, 20)


pos_2 <- tokenize(toLower(two_star_pos$review_content), removePunct = TRUE, ngrams = 2)

pos_2_dfm = dfm(pos_2, ignoredFeatures = c(word_to_remove, stopwords("english")))

pos2_top20 <- topfeatures(pos_2_dfm, 20)


pos_3 <- tokenize(toLower(three_star_pos$review_content), removePunct = TRUE, ngrams = 2)

pos_3_dfm = dfm(pos_3, ignoredFeatures = c(word_to_remove, stopwords("english")))

pos3_top20 <- topfeatures(pos_3_dfm, 20)


pos_lost <- tokenize(toLower(lost_star_pos$review_content), removePunct = TRUE, ngrams = 2)

pos_lost_dfm = dfm(pos_lost, ignoredFeatures = c(word_to_remove, stopwords("english")))

poslost_top20 <- topfeatures(pos_lost_dfm, 20)

pos3_top20
pos2_top20
pos1_top20
poslost_top20

all_m <- tokenize(toLower(all_michelin$review_content), removePunct = TRUE, ngrams = 2)

all_m_dfm = dfm(all_m, ignoredFeatures = c(word_to_remove, stopwords("english")))

all_m_top20 <- topfeatures(all_m_dfm, 20)


all_lost <- tokenize(toLower(all_lost_event$review_content), removePunct = TRUE, ngrams = 2)

all_lost_dfm = dfm(all_lost, ignoredFeatures = c(word_to_remove, stopwords("english")))

all_lost_top20 <- topfeatures(all_lost_dfm, 20)


all_gained <- tokenize(toLower(all_gained_event$review_content), removePunct = TRUE, ngrams = 2)

all_gained_dfm = dfm(all_gained, ignoredFeatures = c(word_to_remove, stopwords("english")))

all_gained_top20 <- topfeatures(all_gained_dfm, 20)


trestle <- tokenize(toLower(nm_trestle$review_content), removePunct = TRUE, ngrams = 2)

trestle_dfm = dfm(trestle, ignoredFeatures = c(word_to_remove, stopwords("english")))

trestle_top20 <- topfeatures(trestle_dfm, 20)


all_m_top20
all_lost_top20
all_gained_top20
trestle_top20

require(quanteda)
plot(trestle_dfm, min.freq = 15, random.order = FALSE)  
plot(all_gained_dfm, min.freq = 6, random.order = FALSE)


# rm(list=ls(all=TRUE))






