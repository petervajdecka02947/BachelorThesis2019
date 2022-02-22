library(tidyverse)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library(xlsx)
library(data.table)
library(h2o)
library(pracma)

start_processing <- Sys.time()

## set paths to relevant data folders
#project_root <- getwd() 
project_root <- "C:/Users/PeterVajdecka/Desktop/bc progress/"
vectorized_data_path <- c(paste0(project_root, '/Data/intermediate/vectorized/'))
Top_Max<-20

## set up info about the data
topics <- c('hotel', 
            'restaurant', 
            'attraction',
            'train')
mode<-'treshold'

relevancy <- tribble(
  ~value, ~label, ~short_label,
  0, 'outliers', 'irrel',
  1, 'inliers', 'rel'
)

## define functions for getting collocations

get_collocations <- function(df) {
  collocations<-data.frame(words = df$lemma,
                           bigram = txt_nextgram(df$lemma, n = 2,sep = " "),
                           trigram = txt_nextgram(df$lemma, n = 3, sep = " "),
                           quatrogram = txt_nextgram(df$lemma, n = 4, sep = " "),
                           stringsAsFactors = FALSE)
  collocations<-cbind(df,collocations)
  return(collocations)
}

# here I get similar phrase for each significant word
colMax_value <- function(data) sapply(data, max, na.rm = TRUE)
colMax_position <- function(data) sapply(data, which.max)

h2o.init()
df_results<-data.frame()

topic<-"attraction"

for(topic in topics) {
  
  Outliers_Significant<-read.csv2(file=paste0(project_root, '/Data/output/words_significant/Outliers_Significant_Keywords_LR.csv'))
  Outliers_Significant<-as.matrix(Outliers_Significant[Outliers_Significant[,topic]!="",topic])
  
  
  topic_lang_label_data_path <- paste0(project_root, '/Data/intermediate/vectorized/', 
                                       topic, '_irrel_vectorized_',mode,'.RData')
  load(topic_lang_label_data_path)
  
  df_out<-topic_lang_label_vectorized
  
  
  # sort matrix by ids 
  df_out <-df_out %>% 
    arrange(doc_id,paragraph_id,sentence_id,token_id)
  
  #create n grams matrix
  df_collocations_out <- get_collocations(df_out)
  df_collocations_out$gram_id<-c(1:nrow(df_collocations_out))
  
 
  
  # bi GRAM   
  
  df_collocations_out<-df_collocations_out[is.na(df_collocations_out$bigram)==FALSE,]
  
  s <- strsplit(df_collocations_out$bigram, split = " ")
  
  # prepare bigrams for do2vec 
  bi_gram_vectorized<-data.frame(
    doc_id= rep(df_collocations_out[,names(df_collocations_out)=="doc_id"],
                sapply(s, length)),
    lemma = rep(df_collocations_out[,names(df_collocations_out)=="lemma"],
                sapply(s, length)), 
    gram_id = rep(df_collocations_out[,names(df_collocations_out)=="gram_id"],
                  sapply(s, length)), 
    bigram = rep(df_collocations_out[,names(df_collocations_out)=="bigram"],
                     sapply(s, length)),
    bi_gram_sep = unlist(s))
  
  # add NA after each bigram = preparation for word2vec
  
  df_vectorized <- rbind(setDT(bi_gram_vectorized), bi_gram_vectorized[, tail(.SD, 1) , 
                                                                               by = gram_id][, c("bi_gram_sep") := .(NA)])[order(gram_id)]
  df_vectorized<-df_vectorized[-nrow(df_vectorized),]
  
  # Set only words column with NA after each bigram
  
  df_words<-as.h2o(df_vectorized$bi_gram_sep)
  df_words<-as.character(df_words)
  
  #word2vec model for bigram
  
  
  w2v.model <- h2o.word2vec(df_words,min_word_freq=1,word_model = "CBOW", #c("SkipGram", "CBOW")
                            vec_size = 128,window_size = 2, sent_sample_rate = 0,init_learning_rate=0, epochs = 10)
  df_w2v.model<-h2o.toFrame(w2v.model)
  
  
  #doc2vec -> rows?sapply=df_collocations_out$bigram
  
  Doc2v.model <-h2o.transform(w2v.model, df_words, aggregate_method = "AVERAGE")
  
  #print("Sanity check - find synonyms for the word 'teacher'")
  #h2o.findSynonyms(w2v.model, "free", count = 30)
  
  
  colnames(Doc2v.model)<-colnames(df_w2v.model[,2:ncol(df_w2v.model)])
  Doc2v.model<-as.matrix(Doc2v.model)
  
  df_w2v.model<-as.data.frame(df_w2v.model)
  df_w2v.model <- df_w2v.model[df_w2v.model$Word %in% Outliers_Significant, ]
  
  unique_words<-df_w2v.model[,"Word"]
  df_w2v.model<-df_w2v.model[,2:ncol(df_w2v.model)]
  df_w2v.model<-as.matrix(df_w2v.model)
  
  # euclidean distance 
  
  distance_matrix <- distmat(Doc2v.model, df_w2v.model)
  
  #distance based similarity
  
  distance_matrix<-1/(1+distance_matrix)
  
  distance_matrix<-as.data.frame(distance_matrix) # rows=bigrams columns=significant keywords
  
  
  #name columns and add row of gram names of matrix
  colnames(distance_matrix)<-unique_words
  distance_matrix$gram_name<-df_collocations_out[,"bigram"] 
  
  
  for (i in 1:ncol(distance_matrix)) {
    
    columnName<-colnames(distance_matrix)[i] # significant keyword
    
    to_zero<-which(grepl(columnName,distance_matrix[,"gram_name"]))  # get rows of all grams with keyword
    
    if (length(to_zero)!=0)
    {
      distance_matrix[to_zero,columnName]<-0   # if keyword in 
    }
    
  }
  
  distance_matrix<-distance_matrix[,names(distance_matrix)!= c("gram_name")]
  df_results_bi<-data.frame()
  #df_results<-data.frame()
  for (j in 1:Top_Max){
    
    maxpositions<-colMax_position(distance_matrix)
    maxvalues<-colMax_value(distance_matrix)
    names(maxvalues)<-unique_words
    names(maxpositions)<-unique_words
    
    
    df_results_bi<-as.data.frame(cbind(keyword=unique_words,phrase=df_collocations_out[maxpositions,"bigram"],
                                           similarity=maxvalues,
                                           conversation=df_collocations_out[maxpositions,"Content"]))
    df_results_bi$topic_name<-topic
    df_results_bi$gram_type<-"bigram"
    df_results_bi$level<-j
    df_results<-rbind(df_results,df_results_bi)  
    
    for (g in 1:length(maxpositions))
    {
      distance_matrix[maxpositions[g],g]<-0 
    }
    
  }
  
  
  # TRI GRAM   
  
  df_collocations_out<-df_collocations_out[is.na(df_collocations_out$trigram)==FALSE,]
  
  s <- strsplit(df_collocations_out$trigram, split = " ")
  
  # prepare trigrams for do2vec 
  tri_gram_vectorized<-data.frame(
    doc_id= rep(df_collocations_out[,names(df_collocations_out)=="doc_id"],
                sapply(s, length)),
    lemma = rep(df_collocations_out[,names(df_collocations_out)=="lemma"],
                sapply(s, length)), 
    gram_id = rep(df_collocations_out[,names(df_collocations_out)=="gram_id"],
                  sapply(s, length)), 
    trigram = rep(df_collocations_out[,names(df_collocations_out)=="trigram"],
                     sapply(s, length)),
    tri_gram_sep = unlist(s))
  
  # add NA after each trigram = preparation for word2vec
  
  df_vectorized <- rbind(setDT(tri_gram_vectorized), tri_gram_vectorized[, tail(.SD, 1) , 
                                                                               by = gram_id][, c("tri_gram_sep") := .(NA)])[order(gram_id)]
  df_vectorized<-df_vectorized[-nrow(df_vectorized),]
  
  # Set only words column with NA after each trigram
  
  df_words<-as.h2o(df_vectorized$tri_gram_sep)
  df_words<-as.character(df_words)
  
  #word2vec model for trigram
  
  
  w2v.model <- h2o.word2vec(df_words,min_word_freq=1,word_model = "CBOW", #c("SkipGram", "CBOW")
                            vec_size = 128,window_size = 2, sent_sample_rate = 0,init_learning_rate=0, epochs = 10)
  df_w2v.model<-h2o.toFrame(w2v.model)
  
  
  #doc2vec -> rows?sapply=df_collocations_out$trigram
  
  Doc2v.model <-h2o.transform(w2v.model, df_words, aggregate_method = "AVERAGE")
  
  #print("Sanity check - find synonyms for the word 'teacher'")
  #h2o.findSynonyms(w2v.model, "free", count = 30)
  
  
  colnames(Doc2v.model)<-colnames(df_w2v.model[,2:ncol(df_w2v.model)])
  Doc2v.model<-as.matrix(Doc2v.model)
  
  df_w2v.model<-as.data.frame(df_w2v.model)
  df_w2v.model <- df_w2v.model[df_w2v.model$Word %in% Outliers_Significant, ]
  
  unique_words<-df_w2v.model[,"Word"]
  df_w2v.model<-df_w2v.model[,2:ncol(df_w2v.model)]
  df_w2v.model<-as.matrix(df_w2v.model)
  
  # euclidean distance 
  
  distance_matrix <- distmat(Doc2v.model, df_w2v.model)
  
  #distance based similarity
  
  distance_matrix<-1/(1+distance_matrix)
  
  distance_matrix<-as.data.frame(distance_matrix) # rows=trigrams columns=significant keywords
  
  
  #name columns and add row of gram names of matrix
  colnames(distance_matrix)<-unique_words
  distance_matrix$gram_name<-df_collocations_out[,"trigram"] 
  
  
  for (i in 1:ncol(distance_matrix)) {
    
    columnName<-colnames(distance_matrix)[i] # significant keyword
    
    to_zero<-which(grepl(columnName,distance_matrix[,"gram_name"]))  # get rows of all grams with keyword
    
    if (length(to_zero)!=0)
    {
      distance_matrix[to_zero,columnName]<-0   # if keyword in 
    }
    
  }
  
  distance_matrix<-distance_matrix[,names(distance_matrix)!= c("gram_name")]
  df_results_tri<-data.frame()
  #df_results<-data.frame()
  for (j in 1:Top_Max){
    
    maxpositions<-colMax_position(distance_matrix)
    maxvalues<-colMax_value(distance_matrix)
    names(maxvalues)<-unique_words
    names(maxpositions)<-unique_words
    
    
    df_results_tri<-as.data.frame(cbind(keyword=unique_words,phrase=df_collocations_out[maxpositions,"trigram"],
                                           similarity=maxvalues,
                                           conversation=df_collocations_out[maxpositions,"Content"]))
    df_results_tri$topic_name<-topic
    df_results_tri$gram_type<-"trigram"
    df_results_tri$level<-j
    df_results<-rbind(df_results,df_results_tri)  
    
    for (g in 1:length(maxpositions))
    {
      distance_matrix[maxpositions[g],g]<-0 
    }
    
  }
  
  
  # Quatro GRAM   
  
  df_collocations_out<-df_collocations_out[is.na(df_collocations_out$quatrogram)==FALSE,]
  
  s <- strsplit(df_collocations_out$quatrogram, split = " ")
  
  # prepare quatrograms for do2vec 
  quatro_gram_vectorized<-data.frame(
    doc_id= rep(df_collocations_out[,names(df_collocations_out)=="doc_id"],
                sapply(s, length)),
    lemma = rep(df_collocations_out[,names(df_collocations_out)=="lemma"],
                sapply(s, length)), 
    gram_id = rep(df_collocations_out[,names(df_collocations_out)=="gram_id"],
                  sapply(s, length)), 
    quatrogram = rep(df_collocations_out[,names(df_collocations_out)=="quatrogram"],
                     sapply(s, length)),
    quatro_gram_sep = unlist(s))
  
  # add NA after each quatrogram = preparation for word2vec
  
  df_vectorized <- rbind(setDT(quatro_gram_vectorized), quatro_gram_vectorized[, tail(.SD, 1) , 
                                                                               by = gram_id][, c("quatro_gram_sep") := .(NA)])[order(gram_id)]
  df_vectorized<-df_vectorized[-nrow(df_vectorized),]
  
  # Set only words column with NA after each quatrogram
  
  df_words<-as.h2o(df_vectorized$quatro_gram_sep)
  df_words<-as.character(df_words)
  
  #word2vec model for quatrogram
  
  
  w2v.model <- h2o.word2vec(df_words,min_word_freq=1,word_model = "CBOW", #c("SkipGram", "CBOW")
                            vec_size = 128,window_size = 2, sent_sample_rate = 0,init_learning_rate=0, epochs = 10)
  df_w2v.model<-h2o.toFrame(w2v.model)
  
  
  #doc2vec -> rows?sapply=df_collocations_out$quatrogram
  
  Doc2v.model <-h2o.transform(w2v.model, df_words, aggregate_method = "AVERAGE")
  
  #print("Sanity check - find synonyms for the word 'teacher'")
  #h2o.findSynonyms(w2v.model, "free", count = 30)
  
  
  colnames(Doc2v.model)<-colnames(df_w2v.model[,2:ncol(df_w2v.model)])
  Doc2v.model<-as.matrix(Doc2v.model)
  
  df_w2v.model<-as.data.frame(df_w2v.model)
  df_w2v.model <- df_w2v.model[df_w2v.model$Word %in% Outliers_Significant, ]
  
  unique_words<-df_w2v.model[,"Word"]
  df_w2v.model<-df_w2v.model[,2:ncol(df_w2v.model)]
  df_w2v.model<-as.matrix(df_w2v.model)
  
  # euclidean distance 
  
  distance_matrix <- distmat(Doc2v.model, df_w2v.model)
  
  #distance based similarity
  
  distance_matrix<-1/(1+distance_matrix)
  
  distance_matrix<-as.data.frame(distance_matrix) # rows=quatrograms columns=significant keywords
  
  
  #name columns and add row of gram names of matrix
  colnames(distance_matrix)<-unique_words
  distance_matrix$gram_name<-df_collocations_out[,"quatrogram"] 
  
  
  for (i in 1:ncol(distance_matrix)) {
    
    columnName<-colnames(distance_matrix)[i] # significant keyword
    
    to_zero<-which(grepl(columnName,distance_matrix[,"gram_name"]))  # get rows of all grams with keyword
    
    if (length(to_zero)!=0)
    {
      distance_matrix[to_zero,columnName]<-0   # if keyword in 
    }
    
  }
  
  distance_matrix<-distance_matrix[,names(distance_matrix)!= c("gram_name")]
  df_results_quatro<-data.frame()
  #df_results<-data.frame()
  for (j in 1:Top_Max){
    
    maxpositions<-colMax_position(distance_matrix)
    maxvalues<-colMax_value(distance_matrix)
    names(maxvalues)<-unique_words
    names(maxpositions)<-unique_words
    
    
    df_results_quatro<-as.data.frame(cbind(keyword=unique_words,phrase=df_collocations_out[maxpositions,"quatrogram"],
                             similarity=maxvalues,
                             conversation=df_collocations_out[maxpositions,"Content"]))
    df_results_quatro$topic_name<-topic
    df_results_quatro$gram_type<-"quatrogram"
    df_results_quatro$level<-j
    df_results<-rbind(df_results,df_results_quatro)  
    
    for (g in 1:length(maxpositions))
    {
      distance_matrix[maxpositions[g],g]<-0 
    }
    
  }
  
  df_results<-rbind(df_results,df_results_quatro)
  df_results<-unique(df_results)
  df_results<-df_results[,c("keyword","topic_name","gram_type","level","phrase","similarity","conversation")]
  
}

h2o.removeAll()
h2o.shutdown(prompt = FALSE)
View(df_results)


write.csv2(df_results, file=paste0(project_root, '/Data/output/phrases/phrases_results.csv')
            ,row.names = FALSE)

end_processing <- Sys.time()

cat('LR modeling done in :', round(difftime(end_processing, start_processing, units = 'mins'), 2), 'minutes')

