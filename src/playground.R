# This is the playground where I try to get familiar with the datasets and some of the required techniques

# Working on the English data

# First we create a toy data set which is much smaller to help us write
# our functions more quickly

# Also we used a short script to remove a lot of special characters to make it easier to work with.

# library(tm)
library(tidyverse)

set.seed(123)

## Twitter data
# twitter_con <- file("data/en_US/radical_twitter.txt", "r")
# twitter <- readLines(twitter_con, encoding = "UTF-8")
# length(twitter)
# close(twitter_con)
# 
# twitter_sample <- sample(twitter, 30000)
# writeLines(twitter_sample, "data/toydata/twitter.txt")
# 
# ## News data
# news_con <- file("data/en_US/radical_news.txt", "r")
# news <- readLines(news_con, encoding = "UTF-8")
# length(news)
# close(news_con)
# 
# news_sample <- sample(news, 30000)
# writeLines(news_sample, "data/toydata/news.txt")
# 
# ## Blog data
# blog_con <- file("data/en_US/radical_blogs.txt", "r")
# blog <- readLines(blog_con, encoding = "UTF-8")
# length(blog)
# close(blog_con)
# 
# blog_sample <- sample(blog, 30000)
# writeLines(blog_sample, "data/toydata/blog.txt")
# 
# all_sample <- c(twitter_sample,news_sample,blog_sample)
# writeLines(all_sample,"data/toydata/all/all_sample.txt")
## From now on we will work with the samples only

## First we clean some more. We do the following
### Everything to lower case
### Remove all numbers
### Remove all punctuation

# corp <- VCorpus(VectorSource(all_sample))
# corp <- VCorpus(DirSource("data/toydata/all"))


# clean_corpus <- function (corp) {
#     corp %>% tm_map(stripWhitespace) %>% tm_map(removePunctuation) %>% tm_map(removeNumbers) %>% tm_map(content_transformer(tolower))
# }

# I have preprepared the sources by radically removing anything but unicode character \u0000 to \u0079
# hence the name

## TODO: Do the preprocessing in here again. Just replace all whitespace with " "
## And then make it lower and kick everything but [a-z].


con <- file("data/en_US/radical_news.txt", "r")
text <- readLines(con, encoding = "UTF-8")

corpus <- c()
con <- file("data/en_US/radical_twitter.txt", "r")
corpus <- c(corpus,readLines(con, encoding = "UTF-8"))
close(con)
con <- file("data/en_US/radical_news.txt", "r")
corpus <- c(corpus,readLines(con, encoding = "UTF-8"))
close(con)
con <- file("data/en_US/radical_blogs.txt", "r")
corpus <- c(corpus,readLines(con, encoding = "UTF-8"))
close(con)
length(corpus)


# do the functions first via sapply, I think this is faster
clean_corpus <- function(corp) {
    corp <- corp %>%
        strsplit(split = "\\s+")  %>% # split at any whitespace
        unlist %>% # give me one huge vector instead of a list 
        tolower %>% 
        gsub(pattern = "[^a-z]+", replacement = "") # this should get rid of puncutation and number
        # old pattern "[0-9\\.\\!\\?\\+#\"\':\`~\\*-_&\\(\\)\\$%]"
    corp[corp != ""] # remove empty strings
}
# Test string
corp <- " he sa.id: \"hello -_m-y%  * ~`~  dear R$$_code!?\""

# It's not great but ok.

unique_words <- unique(unlist(t))

word_count  <-  as.data.frame(table(unlist(corpus)),stringsAsFactors = FALSE) %>%
    arrange(desc(Freq))
word_count <- word_count %>% mutate(len = nchar(Var1))


bigram_builder <- function(values){
    n <- length(values)
    if (n==0){
        return(c())
    }
    result <- c()
    for (k in 1:(n-1)){
        result = c(result,paste(values[k],values[k+1],sep=" "))
    }
    return(result)
}

bigram_count  <-  as.data.frame(table(bigrams)) %>%
    arrange(desc(Freq))

# we can add frequency tables by doing a full_join,
# then replacing NA with 0
# then mutating to create a new column value = value.x + value.y
# then only keep "name" and "value" columns

nth_word <- function(n) {
    function(x) {
        strsplit(x, " ")[[1]][n]
    }
}


last_n <- function(sentence, n=1){
    words <- strsplit(sentence, " ")[[1]]    
    N <- length(words)
    take <- min(n,N) #  make sure not to take more words than there are
    words[(N-take+1):N]
}
# Even with the full bi tri and quadgrams loaded this is performant enough
# The lowest hanging fruit is probably to store the filtered things for the last words
# somewhere in a global variable and then update that the filter for the last word
# becomes the second to last, once a new word is entered. But if the shiny server
# gives enough power, that won't even be necessary.
predict_word <- function(sentence,maxn=3){
    words <- last_n(sentence,n=maxn)
    N <- length(words)
    print(N)
    if (N == 3){ # Look if we find a quadgram
        canidates <- qg %>% filter(First == words[1],Second == words[2],Third == words[3])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            print("quad prediction")
            canidates$Fourth[1]
        }
    }
    else if (N == 2){ # looking for a trigram now
        canidates <- tg %>% filter(First == words[1],Second == words[2])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            print("tri prediction")
            canidates$Third[1]
        }
    }
    else if (N == 1){ # looking for bigram
        canidates <- bg %>% filter(First == words[1])
        if (nrow(canidates) == 0){
            predict_word(sentence, maxn - 1)
        }
        else {
            print("bi prediction")
            canidates$Second[1]
        }
    }
    else { # return most frequent word
        print("default prediction")
        "the"
    }
}



