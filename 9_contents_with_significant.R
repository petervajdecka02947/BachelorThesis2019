library(tidyverse)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library(xlsx)

start_processing <- Sys.time()

## set paths to relevant data folders
#project_root <- getwd() 
project_root <- "C:/Users/PeterVajdecka/Desktop/bc progress/"
vectorized_data_path <- c(paste0(project_root, '/Data/intermediate/vectorized/'))

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

df_results<-data.frame()
topic<-"attraction"

for(topic in topics) {
  
  Outliers_Significant<-read.csv2(file=paste0(project_root, '/Data/output/words_significant/Outliers_Significant_Keywords_LR.csv'))
  
  Outliers_Significant<-as.matrix(Outliers_Significant[Outliers_Significant[,topic]!="",topic])
  
  Inliers_Significant<-read.csv2(file=paste0(project_root, '/Data/output/words_significant/Inliers_Significant_Keywords_LR.csv'))
  
  Inliers_Significant<-as.matrix(Inliers_Significant[Inliers_Significant[,topic]!="",topic])

   
  for(j in 1:nrow(relevancy)) {
    topic_lang_label_data_path <- paste0(project_root, '/Data/intermediate/vectorized/', 
                                         topic, '_', relevancy[j, 'short_label', drop = TRUE], '_vectorized_',mode,'.RData')
    load(topic_lang_label_data_path)
    
    if (relevancy[j, 'short_label', drop = TRUE]=='irrel'){
      topic_lang_label_vectorized<-topic_lang_label_vectorized[topic_lang_label_vectorized$lemma %in% Outliers_Significant,c("doc_id","lemma","sentence","Content","outlier.scores")]
      
    }else{
      topic_lang_label_vectorized<-topic_lang_label_vectorized[topic_lang_label_vectorized$lemma %in% Inliers_Significant,c("doc_id","lemma","sentence","Content","outlier.scores")]
      
    }
    topic_lang_label_vectorized<-unique(topic_lang_label_vectorized)
    topic_lang_label_vectorized$mode<- relevancy[j, 'label', drop = TRUE]
    topic_lang_label_vectorized$topic<- topic
    topic_lang_label_vectorized<-topic_lang_label_vectorized[,c("topic","mode","lemma","sentence","Content","outlier.scores")]
    df_results<-rbind(df_results,topic_lang_label_vectorized)

  } 
  
  
}

write.csv2(df_results, paste0(project_root, '/Data/output/contents_significant/Significant_conversations_All.csv'),row.names = FALSE)


end_processing <- Sys.time()

cat('LR modeling done in :', round(difftime(end_processing, start_processing, units = 'mins'), 2), 'minutes')