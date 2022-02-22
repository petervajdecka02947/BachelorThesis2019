library(h2o)
library(tidyverse)
library(udpipe)
library(text2vec)

setwd("C:/Users/PeterVajdecka/Desktop/bc progress/")
project_root <- getwd()
vectorized_data_path <- paste0(project_root, '/Data/intermediate/vectorized/')

## set up info about the data
topics <- c('hotel', 
            'restaurant',  
             'attraction',
            'train')

relevancy <- tribble(
  ~value, ~label, ~short_label,
  0, 'irrelevant', 'irrel',
  1, 'relevant', 'rel'
)

modes <- tribble(
  ~value, ~label, 
  0.4, 'percentage', 
  1.1, 'treshold'
)

## get top n frequent lemmas
get_top_words <- function(df,counts=4) {
  topwords <- df %>% 
    dplyr::count(lemma) %>% 
   # top_n(top_n, wt = n) %>% 
    arrange(desc(n))%>%
    filter(n>counts)
  
  return(topwords)
}

load(paste0(project_root,"/Data/_vectorized.RData"))
df_vectorized$doc_id<-as.numeric(df_vectorized$doc_id)

for (i in 1:nrow(modes)){
  
  mode <- modes[i, 'label', drop = TRUE]

for (topic in topics){
  
  
    
  load(paste0(project_root, '/Data/intermediate/outlier_seperated/',
              topic, '_outlier_seperated_',mode,'.RData'))
  total$ID<-as.numeric(total$ID)
  
  df_preparation <- df_vectorized %>% 
    right_join(total, by = c('doc_id' = 'ID'))
  
  
  for(j in 1:nrow(relevancy)) {
    
    topic_lang_label_vectorized <- df_preparation[
      df_preparation$relevant == relevancy[j, 'value', drop = TRUE], ]
    
    
    save(topic_lang_label_vectorized, 
         file = paste0(project_root, '/Data/intermediate/vectorized/', 
                       topic, '_', relevancy[j, 'short_label', drop = TRUE], '_vectorized_',mode,'.RData'))
    print(paste("Number of attributes for topic",topic," with label ",relevancy[j, 'label', drop = TRUE],"and mode ",mode,"  : ",nrow(topic_lang_label_vectorized)))
    # Set top n words
    
    if (j==1)
    {
      
      topic_lang_irrelevant <- topic_lang_label_vectorized
      topwords_irrelevant <- get_top_words(topic_lang_irrelevant)
      save(topwords_irrelevant, file = paste0(project_root, '/Data/intermediate/word_counts/', topic, '_irrel_word_counts_',mode,'.RData'))
      
    }else{
      
      topic_lang_relevant <- topic_lang_label_vectorized
      topwords_relevant <- get_top_words(topic_lang_relevant)
      save(topwords_relevant, file = paste0(project_root, '/Data/intermediate/word_counts/', topic,  '_rel_word_counts_',mode,'.RData'))
    }
    
    
  }
  
  }
}
