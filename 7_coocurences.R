library(tidyverse)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library("xlsx")

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

NoTopWords<-30

## define functions for getting cooccurrences

get_cooccurrences <- function(df, categories = c('NOUN', 'ADJ', 'ADV', 'VERB', 'PROPN'), skipgram = 4) {
  cooccurrences <-cooccurrence(x = df$lemma,
                                relevant = df$upos %in% categories,
                                group = c('doc_id'),
                                skipgram = skipgram)
  return(cooccurrences)
}

## define function for visualizing cooccurrence (word graph)
create_word_graph <- function(df, topic, relevancy,
                              top_n =NoTopWords, distance = 5, categories_label = c('nouns, verbs, adverbs, adjectives, proper nouns')) {
  word_network <- head(df, top_n)
  word_network <- graph_from_data_frame(word_network)
  word_graph <- ggraph(word_network, 
                       layout = 'fr') +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), 
                   edge_colour = 'pink') +
    geom_node_text(aes(label = name), 
                   col = 'black', size = 4) + # darkgreen
    theme_graph() +
    theme(legend.position = 'none') +
    labs(title = paste('Dvojice slov s maximalnou dialkou ', distance, 'slov\n na temu ', topic, ', uvedenych top', top_n), 
         subtitle = categories_label)
  return(word_graph) 
}
#topic<-"attraction"
df_summarized_all<-data.frame()

for(topic in topics) {
  
      Outliers_Significant<-read.csv2(file=paste0(project_root, '/Data/output/words_significant/Outliers_Significant_Keywords_LR.csv'))
      Outliers_Significant<-as.matrix(Outliers_Significant[Outliers_Significant[,topic]!="",topic])
  
    for(j in 1:nrow(relevancy)) {
      topic_lang_label_data_path <- paste0(project_root, '/Data/intermediate/vectorized/', 
                                           topic, '_', relevancy[j, 'short_label', drop = TRUE], '_vectorized_',mode,'.RData')
      load(topic_lang_label_data_path)
      
        if (relevancy[j, 'short_label', drop = TRUE]=='irrel'){
          topic_lang_label_vectorized_out<-topic_lang_label_vectorized
          
        }else{
          topic_lang_label_vectorized_in<-topic_lang_label_vectorized
          
        }
    } 
      
      topic_lang_label_cooccurrences_out <- get_cooccurrences(topic_lang_label_vectorized_out)
      
      
      rows_vector <- which(topic_lang_label_cooccurrences_out[,1] %in% Outliers_Significant
                           | topic_lang_label_cooccurrences_out[,2] %in% Outliers_Significant ) #|
      
      topic_lang_label_cooccurrences_out <-topic_lang_label_cooccurrences_out[rows_vector,]
      topic_lang_label_cooccurrences_out <-topic_lang_label_cooccurrences_out[topic_lang_label_cooccurrences_out$term1 != topic_lang_label_cooccurrences_out$term2 , ]
      
      
      topic_lang_label_cooccurrences_out<-topic_lang_label_cooccurrences_out[1:NoTopWords,]
    
      save(topic_lang_label_cooccurrences_out, 
           file = paste0(project_root, '/Data/intermediate/cooccurrences/',topic, '_outliers_cooccurrences_',mode,'.RData'))
      word_graph <- create_word_graph(topic_lang_label_cooccurrences_out, topic = topic, relevancy = 'irrel')
      ggsave(paste0(project_root, '/Images/cooccurrences_graphs/', topic, '_irrel_cooc_graph_',mode,'.png'))
      
      df_summarized<-data.frame()
      df_prep<-data.frame()
      
      # create queries for conversations
      
      for (i in 1:nrow(topic_lang_label_cooccurrences_out)){
        
      df_prep<-topic_lang_label_vectorized_out[grepl(topic_lang_label_cooccurrences_out[i,1],topic_lang_label_vectorized_out$doc_lemmas)&
                                                 grepl(topic_lang_label_cooccurrences_out[i,2],topic_lang_label_vectorized_out$doc_lemmas),c("doc_id","Content")] 
      df_prep$term1<-topic_lang_label_cooccurrences_out[i,1]
      df_prep$term2<-topic_lang_label_cooccurrences_out[i,2]
      df_prep<-unique(df_prep)
      
      df_prep<-df_prep %>% mutate(keywords=ifelse(term1 %in% Outliers_Significant & !term2 %in% Outliers_Significant ,term1,
                                                  ifelse((term1 %in% Outliers_Significant & term2 %in% Outliers_Significant),paste0(term1," , ",term2),term2)))
      
      
      df_summarized<-rbind(df_summarized,df_prep)
        
      }
      
      df_summarized$topic<-topic
      df_summarized<-df_summarized[,c("keywords","topic","term1","term2","Content","doc_id" )]
      df_summarized_all<-rbind(df_summarized_all,df_summarized)
      write.csv2(df_summarized_all, paste0(project_root, '/Data/output/coocurences/Coocurences_results_',topic,'.csv'),row.names=FALSE)
}
write.csv2(df_summarized_all, paste0(project_root, '/Data/output/coocurences/Coocurences_results_All.csv'),row.names=FALSE)
end_processing <- Sys.time()

cat('LR modeling done in :', round(difftime(end_processing, start_processing, units = 'mins'), 2), 'minutes')
