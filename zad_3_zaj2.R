
text_1<- readLines(file.choose())
text
text_2 <- readLines(file.choose())
text


install.packages("wordcloud")
library(wordcloud)


install.packages("qdap")
library(qdap)


freq_terms(text)



frequent_terms <- freq_terms(text_1)
frequent_terms
frequent_terms <- freq_terms(text_1, stopwords = Top200Words)
plot(frequent_terms)


frequent_terms_2 <- freq_terms(text_2)
frequent_terms_2
frequent_terms_2 <- freq_terms(text_2, stopwords = Top200Words)
plot(frequent_terms_2)


wordcloud(frequent_terms$WORD, frequent_terms$FREQ)
wordcloud(frequent_terms_2$WORD, frequent_terms_2$FREQ)


wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)
wordcloud(frequent_terms_2$WORD, frequent_terms_2$FREQ, min.freq = 4)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))


wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)
wordcloud(frequent_terms_2$WORD, frequent_terms_2$FREQ, max.words = 5)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))
