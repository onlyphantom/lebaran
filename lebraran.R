library("openssl")
library("httpuv")
library("twitteR")
library("tm")
library("katadasaR")
library("wordcloud")

cons_key <- 
cons_sec <- 
acc_tok  <- "337982123-e0p3ptGsLDdTu0r3lobargjIxSEbP66BAmsUInyi"
acc_sec  <- 
  
# expect to see "using direct authentication"
setup_twitter_oauth(cons_key, cons_sec, acc_tok, acc_sec)

ramadhan <- searchTwitter("ramadhan", n=1000, since="2018-05-20", until="2018-05-31")
lebaran <- searchTwitter("lebaran", n=1000, since="2018-05-20", until="2018-05-31")
puasa <- searchTwitter("puasa", n=1000, since="2018-05-20", until="2018-05-31")

rama <- twListToDF(ramadhan)
leba <- twListToDF(lebaran)
puasa <- twListToDF(puasa)

twt <- rbind(leba, rama, puasa)

## text processing
twt$text <- gsub("@\\w+", "", twt$text)
twt$text <- gsub(":", "", twt$text, fixed=T)
twt$text <- gsub("RT", "", twt$text, fixed=T)

## corpus
twtcorp <- VCorpus(VectorSource(twt$text))

## cleaning up test
twtclean <- tm_map(twtcorp, content_transformer(tolower))
twtclean <- tm_map(twtclean, removePunctuation)
twtclean <- tm_map(twtclean, removeNumbers)

transformer <- content_transformer(function(x, pattern)
  gsub(pattern, " ", x)
)

#handle paragraph divider
twtclean <- tm_map(twtclean, transformer, "\\.")

#handle repeated word
twtclean <- tm_map(twtclean, transformer, "-")

## remove stop words
stopwords <- suppressWarnings(readLines("stopwords-id.txt"))
print("removing stop words...")
system.time({
  twtclean <- tm_map(twtclean, removeWords, stopwords)  
})

stemming_bahasa <- content_transformer(function(x){
  paste(sapply(words(x),katadasar),collapse = " ")
})

twtclean <- tm_map(twtclean, stemming_bahasa)

## removing additional whitespace
twtclean <- tm_map(twtclean, stripWhitespace)

## Produce wordcloud
wordcloud(twtclean, min.freq = 10, max.words=250, random.order = F, colors=brewer.pal(8, "Set2"))

