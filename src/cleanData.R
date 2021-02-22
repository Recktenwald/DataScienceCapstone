library(tidyverse)
set.seed(123)

all_lines <- c()

for (f in list.files("data/en_US",pattern=".txt",full.names = TRUE)){
    all_lines <- c(all_lines,read_lines(f))
}

# We only want to loop through all the lines once, so we want to do a lot of things at once
# We want to
# There may be different types of whitespace, so let's make all of them into regular " "
# Then we want to make everything lowercase
# Finally we want to kill anything that is not a regular letter, i.e. a-z.
## I will not bother trying to make é into e or so
# Once this preprocessing is done it is time to split everything in words
# This sometimes produces 'empty' words, we want to get rid of those.

clean_line <- function(line){
    line <- line %>% 
        gsub(pattern="\\s+", replacement=" ") %>%
        tolower %>%
        gsub(pattern="[^a-z ]+",replacement="") %>%
        strsplit(split=" ") %>%
        unlist
    line[line != ""]
}

# testline <- " he sa.id: \"hello -_m-y%  * ~`~  dear R$$_code!?\""
# clean_line(testline)
# system.time(corpus <- lapply(all_lines,clean_line)) #roughly 30minutes
# rm("all_lines")
# saveRDS(corpus,file="data/corpus.Rds")


build_nGram <- function(values,n=2){
    m <- length(values)
    if (m<n){
        return(c())
    }
    result <- c()
    for (k in 1:(m-(n-1))){
        result <-  c(result,paste0(values[k:(k+(n-1))],collapse=" "))
    }
    return(result)
}

# I used build_nGram to build bigrams, trigrams and quadgrams. 
# Next I counted the occurances of each.
# I decided to remove all the nGrams that only appear once or twice.
# From looking at the data, it seemed to me that many of them are gibberish or typos
# Also they are less useful for the application, as users are unlikely to use a trigram,
# that only appeared once or twice in this corpus.
# The benefit is performance: It is quicker to do the next steps and run the model
# Finally I sorted them by their frequency. 

# All this took multiple hours on my machine, so I comment it out of the assingment,
# otherwise R will run it again when building the html output, and I don't want that.

# Here is the code I used for bigrams


# bigrams <- lapply(corpus,build_nGram) %>% unlist
# bigram_freqs <- as.data.frame(table(bigrams)) %>%  filter(Freq>2) %>% arrange(desc(Freq))
# names(bigram_freqs) <- c("bigrams","Freq")
# bigrams_freqs$bigrams <- as.character(bigrams_freqs$bigrams)
# saveRds(bigram_freqs,"data/bigram_freqs.Rds")
quarter <-  1067420
first_quarter <- corpus[1:quarter]
quadgrams_first_quarter_unlisted <- unlist(lapply(first_quarter,function(x){build_nGram(x,n=4)}))
saveRDS(quadgrams_first_quarter_unlisted,"data/quadgrams_first_quarter_unlisted.Rds")

second_quarter <- corpus[(quarter+1):(2*quarter)]
system.time(quadgrams_second_quarter_unlisted <- unlist(lapply(second_quarter,function(x){build_nGram(x,n=4)})))
saveRDS(quadgrams_second_quarter_unlisted,"data/quadgrams_second_quarter_unlisted.Rds")


third_and_fourth_quarter <- corpus[(2*quarter+1):(4*quarter)]
system.time(quadgrams_third_and_fourth_quarter_unlisted <- unlist(lapply(third_and_fourth_quarter,function(x){build_nGram(x,n=4)})))
saveRDS(quadgrams_third_and_fourth_quarter_unlisted,"data/quadgrams_third_and_fourth_quarter_unlisted.Rds")

q2freqs <- as.data.frame(table(q2freqs)) %>% filter(Freq>2) %>% arrange(desc(Freq))
saveRDS(q2freqs,"data/q2freqs.Rds")


# It turns out, that shiny restricts memory, so I cannot load all the data in. 
# My solution is to keep only the most frequent nGram starting with a given word.
bigrams <- bg %>% arrange(First,desc(Freq))
bigrams_shortened <- bigrams[!duplicated(bigrams$First),]
bg_short <- bigrams_shortened %>% arrange(desc(Freq))
saveRDS(bg_short,"data/model_components/bg_short.Rds")

trigrams <- tg %>% arrange(First,desc(Freq))
trigrams_shortened <- trigrams[!duplicated(trigrams$First),]
tg_short <- trigrams_shortened %>% arrange(desc(Freq))
saveRDS(tg_short,"data/model_components/tg_short.Rds")

quadgrams <- qg %>% arrange(First, desc(Freq))
quadgrams_shortened <- quadgrams[!duplicated(quadgrams$First),]
qg_short <- quadgrams_shortened %>% arrange(desc(Freq))
saveRDS(qg_short,"data/model_components/qg_short.Rds")
