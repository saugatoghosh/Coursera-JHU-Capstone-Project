
library(slam)
library(stringi)
library(readr)
library(data.table)
library(quanteda)


# import the blogs and twitter datasets in text mode
blogs <- read_lines("final/en_US/en_US.blogs.txt")
twitter <- read_lines("final/en_US/en_US.twitter.txt")
# import the news dataset in binary mode
con <- file("final/en_US/en_US.news.txt", open="rb")
news <- read_lines(con)
close(con)
rm(con)


###Basic summary of files imported

rawstats <- data.frame(
  File = c("blogs","news","twitter"), 
  t(rbind(sapply(list(blogs,news,twitter),stri_stats_general),
          TotalWords = sapply(list(blogs,news,twitter),stri_stats_latex)[4,]))
)
print(rawstats[c(1,2,4,6)])


###Sample 10 % of the data

set.seed(1234)

blogsTrainIndices <- sample(seq_along(blogs),
                            size = round(0.1 * length(blogs)),
                            replace = F)
blogs_train <- blogs[blogsTrainIndices]



TwitterTrainIndices <- sample(seq_along(twitter),
                            size = round(0.1 * length(twitter)),
                            replace = F)
twitter_train <- twitter[TwitterTrainIndices]



NewsTrainIndices <- sample(seq_along(news),
                              size = round(0.1 * length(news)),
                              replace = F)
news_train <- news[NewsTrainIndices]


rawstats <- data.frame(
  File = c("blogs_train","news_train","twitter_train"), 
  t(rbind(sapply(list(blogs_train,news_train,twitter_train),stri_stats_general),
          TotalWords = sapply(list(blogs_train,news_train,twitter_train),stri_stats_latex)[4,]))
)
print(rawstats[c(1,2,4,6)])

save(blogs_train, file = 'BlogsTrain10.Rdata')
save(twitter_train, file = 'TwitterTrain10.Rdata')
save(news_train, file = 'NewsTrain10.Rdata')

#Clean up the global environment
rm(blogs, news, twitter)

load('BlogsTrain10.Rdata')
load('TwitterTrain10.Rdata')
load('NewsTrain10.Rdata')

#Preprocess

profanewords <- read_lines('data/bad-words.txt')
class(profanewords)

#Tokenize

blogs_token <- tokens(char_tolower(blogs_train), remove_numbers = T, remove_punct = T, remove_symbols = T,
                   remove_twitter = T, remove_url = T, remove_hyphens = T, verbose = F)
ntype(blogs_token)

blogs_token <- removeFeatures(blogs_token, profanewords)

blogs_uni_token <- blogs_token
blogs_bi_token <- tokens_ngrams(blogs_token, n =2, concatenator = " ")
blogs_tri_token <- tokens_ngrams(blogs_token, n = 3, concatenator = " ")
blogs_quad_token <- tokens_ngrams(blogs_token, n = 4, concatenator = " ")
blogs_penta_token <- tokens_ngrams(blogs_token, n = 5, concatenator = " ")
blogs_hexa_token <- tokens_ngrams(blogs_token, n = 6, concatenator = " ")

twitter_token <- tokens(char_tolower(twitter_train), remove_numbers = T, remove_punct = T, remove_symbols = T,
                      remove_hyphens = T, remove_twitter = T, remove_url = T, verbose = F)

ntype(twitter_token)
twitter_token <- removeFeatures(twitter_token, profanewords)


twitter_uni_token <- twitter_token
twitter_bi_token <- tokens_ngrams(twitter_token, n =2, concatenator = " ")
twitter_tri_token <- tokens_ngrams(twitter_token, n = 3, concatenator = " ")
twitter_quad_token <- tokens_ngrams(twitter_token, n = 4, concatenator = " ")
twitter_penta_token <- tokens_ngrams(twitter_token, n = 5, concatenator = " ")
twitter_hexa_token <- tokens_ngrams(twitter_token, n = 6, concatenator = " ")

news_token <- tokens(char_tolower(news_train), remove_numbers = T, remove_punct = T, remove_symbols = T,
                        remove_hyphens = T,remove_twitter = T, remove_url = T, verbose = F)

ntype(news_token)
news_token <- removeFeatures(news_token, profanewords)


news_uni_token <- news_token
news_bi_token <- tokens_ngrams(news_token, n =2, concatenator = " ")
news_tri_token <- tokens_ngrams(news_token, n = 3, concatenator = " ")
news_quad_token <- tokens_ngrams(news_token, n = 4, concatenator = " ")
news_penta_token <- tokens_ngrams(news_token, n = 5, concatenator = " ")
news_hexa_token <- tokens_ngrams(news_token, n = 6, concatenator = " ")

#Create dfm

blogs_uni_dfm <- dfm( blogs_uni_token, tolower =F)
#topfeatures(blogs_uni_dfm, 10)

blogs_bi_dfm <- dfm(blogs_bi_token, tolower = F)
#topfeatures(blogs_bi_dfm, 10)

blogs_tri_dfm <- dfm(blogs_tri_token, tolower = F)
#topfeatures(blogs_tri_dfm, 10)

blogs_quad_dfm <- dfm(blogs_quad_token, tolower = F)
#topfeatures(blogs_quad_dfm, 10)

blogs_penta_dfm <- dfm(blogs_penta_token, tolower = F)
#topfeatures(blogs_penta_dfm, 10)

blogs_hexa_dfm <- dfm(blogs_hexa_token, tolower = F)
#topfeatures(blogs_hexa_dfm, 10)


twitter_uni_dfm <- dfm( twitter_uni_token, tolower =F)
#topfeatures(twitter_uni_dfm, 10)

twitter_bi_dfm <- dfm(twitter_bi_token, tolower = F)
#topfeatures(twitter_bi_dfm, 10)

twitter_tri_dfm <- dfm(twitter_tri_token, tolower = F)
#topfeatures(twitter_tri_dfm, 10)

twitter_quad_dfm <- dfm(twitter_quad_token, tolower = F)
#topfeatures(twitter_quad_dfm, 10)

twitter_penta_dfm <- dfm(twitter_penta_token, tolower = F)

twitter_hexa_dfm <- dfm(twitter_hexa_token, tolower = F)
#topfeatures(twitter_hexa_dfm)

news_uni_dfm <- dfm( news_uni_token, tolower =F)
#topfeatures(news_uni_dfm, 10)

#news_uni_dfm

news_bi_dfm <- dfm(news_bi_token, tolower = F)
#topfeatures(news_bi_dfm, 10)

news_tri_dfm <- dfm(news_tri_token, tolower = F)
#topfeatures(news_tri_dfm, 10)

news_quad_dfm <- dfm(news_quad_token, tolower = F)
#topfeatures(news_quad_dfm, 10)

news_penta_dfm <- dfm(news_penta_token, tolower = F)

news_hexa_dfm <- dfm(news_hexa_token, tolower = F)
#topfeatures(news_hexa_dfm)

all_uni_dfm <- rbind(blogs_uni_dfm, twitter_uni_dfm, news_uni_dfm)

topfeatures(all_uni_dfm)

all_bi_dfm <- rbind(blogs_bi_dfm, twitter_bi_dfm, news_bi_dfm)

topfeatures(all_bi_dfm)

all_tri_dfm <- rbind(blogs_tri_dfm, twitter_tri_dfm, news_tri_dfm)

topfeatures(all_tri_dfm)

all_quad_dfm <- rbind(blogs_quad_dfm, twitter_quad_dfm, news_quad_dfm)
topfeatures(all_quad_dfm)

all_penta_dfm <- rbind(blogs_penta_dfm, twitter_penta_dfm, news_penta_dfm)
topfeatures(all_penta_dfm)

all_hexa_dfm <- rbind(blogs_hexa_dfm, twitter_hexa_dfm, news_hexa_dfm)
topfeatures(all_hexa_dfm)

# Calculate  term frequencies and convert to data table

tfuni <- slam::col_sums(all_uni_dfm, na.rm = T)
tfuni <- sort(tfuni, decreasing = TRUE)
tfuni[1:10]


unigramdata <- data.table("count" = as.numeric(tfuni), "ngram" = names(tfuni))

tfbi <- slam::col_sums(all_bi_dfm, na.rm = T)
tfbi <- sort(tfbi, decreasing = TRUE)
tfbi[1:10]

bigramdata <- data.table("count" = as.numeric(tfbi), "ngram" = names(tfbi))


tftri <- slam::col_sums(all_tri_dfm, na.rm = T)
tftri <- sort(tftri, decreasing = TRUE)
tftri[1:10]

trigramdata <- data.table("count" = as.numeric(tftri), "ngram" = names(tftri))

tfquad <- slam::col_sums(all_quad_dfm, na.rm = T)
tfquad <- sort(tfquad, decreasing = TRUE)
tfquad[1:10]

quadgramdata <- data.table("count" = as.numeric(tfquad), "ngram" = names(tfquad))

tfpenta <- slam::col_sums(all_penta_dfm, na.rm = T)
tfpenta <- sort(tfpenta, decreasing = TRUE)
tfpenta[1:10]

pentagramdata <- data.table("count" = as.numeric(tfpenta), "ngram" = names(tfpenta))

tfhexa <- slam::col_sums(all_hexa_dfm, na.rm = T)
tfhexa <- sort(tfhexa, decreasing = TRUE)
tfhexa[1:10]

hexagramdata <- data.table("count" = as.numeric(tfhexa), "ngram" = names(tfhexa))

# Simple function to predict the next word based on unsmoothed n-gram probabilities
save(trigramdata,file = "allTrigrams10.RData")
save(bigramdata, file ="allBigrams10.RData")
save(unigramdata, file = "allUnigrams10.RData")
save(quadgramdata, file = "allQuadgrams10.Rdata")
save(pentagramdata, file = "allPentagrams10.Rdata")
save(hexagramdata, file = "allHexagrams10.Rdata")

load("allTrigrams10.RData")
load("allBigrams10.RData")
load("allUnigrams10.RData")
load("allQuadgrams10.Rdata")
load("allPentagrams10.Rdata")
load("allHexagrams10.Rdata")

head(trigramdata)
head(unigramdata)
head(bigramdata)
head(quadgramdata)
head(pentagramdata)
head(hexagramdata)

tail(bigramdata)
tail(unigramdata)
tail(trigramdata)
tail(quadgramdata)
tail(pentagramdata)
tail(hexagramdata)


# Split ngram into first word(s) and the next word

hexagrams <- do.call(rbind, strsplit(hexagramdata$ngram, split = " "))
hexagramdata$ngram1 <- hexagrams[,1]
hexagramdata$ngram2 <- hexagrams[, 2]
hexagramdata$ngram3 <- hexagrams[, 3]
hexagramdata$ngram4 <- hexagrams[, 4]
hexagramdata$ngram5 <- hexagrams[, 5]
hexagramdata$nextword <- hexagrams[,6]
hexagramdata$ngram <- NULL

hexagramdata <- hexagramdata[, .(nextword,count, prob = count/sum(count)), by = .(ngram1, ngram2, ngram3, ngram4, ngram5)]
head(hexagramdata)

saveRDS(hexagramdata, file = "allHexagrams10_2.rds")
rm(hexagrams, hexagramdata)

pentagrams <- do.call(rbind, strsplit(pentagramdata$ngram, split = " "))
pentagramdata$ngram1 <- pentagrams[,1]
pentagramdata$ngram2 <- pentagrams[, 2]
pentagramdata$ngram3 <- pentagrams[, 3]
pentagramdata$ngram4 <- pentagrams[, 4]
pentagramdata$nextword <- pentagrams[,5]
pentagramdata$ngram <- NULL

pentagramdata <- pentagramdata[, .(nextword,count, prob = count/sum(count)), by = .(ngram1, ngram2, ngram3, ngram4)]
head(pentagramdata)

saveRDS(pentagramdata, file = "allPentagrams10_2.rds")
rm(pentagramdata)

quadgrams <- do.call(rbind, strsplit(quadgramdata$ngram, split = " "))
quadgramdata$ngram1 <- quadgrams[,1]
quadgramdata$ngram2 <- quadgrams[, 2]
quadgramdata$ngram3 <- quadgrams[, 3]
quadgramdata$nextword <- quadgrams[,4]
quadgramdata$ngram <- NULL
#setkey(quadgramdata2, ngram1, ngram2, ngram3)

quadgramdata <- quadgramdata[, .(nextword,count, prob = count/sum(count)), by = .(ngram1, ngram2, ngram3)]

#setkey(quadgramdata2, 'prob', 'ngram')

saveRDS(quadgramdata, file = "allQuadgrams10_2.rds")


trigrams <- do.call(rbind, strsplit(trigramdata$ngram, split = " "))

trigramdata$ngram1 <- trigrams[, 1]
trigramdata$ngram2 <- trigrams[, 2]
trigramdata$nextword <- trigrams[, 3]
trigramdata$ngram <- NULL
#setkey(trigramdata2, ngram1,ngram2)
trigramdata <- trigramdata[, .(ngram1, ngram2, nextword,count, prob = count/sum(count)), by = .(ngram1,ngram2)]
trigramdata[,c(1,2)] <- NULL                                                                                                     

#setkey(trigramdata2, 'prob', 'ngram')
head(trigramdata)

saveRDS(trigramdata, file = "allTrigrams10_2.rds")

bigrams <- do.call(rbind, strsplit(bigramdata$ngram, split = " "))
bigramdata$ngram <- bigrams[, 1]
bigramdata$nextword <- bigrams[, 2]
#setkey(bigramdata2,ngram)
bigramdata <- bigramdata[, .(ngram, nextword,count, prob = count/sum(count)), by = ngram]

#setkey(bigramdata2, 'prob', 'ngram')
bigramdata[,1] <- NULL
head(bigramdata)
saveRDS(bigramdata, file = "allBigrams10_2.rds")

#setkey(unigramdata2, 'count', 'ngram')

unigramdata <- unigramdata[, .(ngram, count, prob = count/sum(count)), by = ngram]
unigramdata[,1] <- NULL
head(unigramdata)
saveRDS(unigramdata, file = "allUnigrams10_2.rds")











