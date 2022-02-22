library(tidyverse)
library(qdapRegex)
library(udpipe)
library(tidytext)
library(lubridate)
library(textTinyR)
library(fastTextR)
library(devtools)
library(roxygen2)
library(testthat)
library(fastTextR)
library(dplyr)
library(rword2vec)
library(readxl)


## set paths to relevant data folders
setwd("C:/Users/PeterVajdecka/Desktop/bc progress/")
project_root <- getwd() 
compiled_data_path <- c(paste0(project_root, '/Data/intermediate/compiled/'))



## define POS that should be part of the following analysis
categories<-c('NOUN', 'ADJ', 'VERB', 'ADV', 'PROPN')

## define unwated characters that should be replaced
unwanted_array = list('a' = 'a', 'â' = 'a', 'á' = 'a', 'ä' = 'a', 'ç' = 'c', 'ç' = 'c', 
                      'é' = 'e', 'e' = 'e', 'e' = 'e', 'í' = 'i', 'î' = 'i', 'n' = 'n', 'ô' = 'o', 'ö' = 'o',
                      'ó' = 'o', 'o' = 'oe', 'ß' = 'ss', 'ú' = 'u', 'ü' = 'u', 'u' = 'u', 'u' = 'u')


## define function to extract urls from the main content
parse_urls <- function(data, pattern = '@rm_url2') {
  Encoding(data$content) <- 'UTF-8'
  data <- data %>% 
    mutate(content = iconv(content, 'UTF-8', 'UTF-8', sub = '')) %>% 
    mutate(url = ex_url(content, pattern = pattern)) %>% 
    mutate(content2 = rm_url(content, pattern = pattern))
  return(data)
}

start_processing <- Sys.time()

load(paste0(compiled_data_path,'AllTopics_processed.RData'))


model_name<-"english-gum-ud-2.5-191206.udpipe"

## define POS that should be part of the following analysis
categories<-c('NOUN', 'ADJ', 'VERB', 'ADV', 'PROPN') 


# get czech stop words

stop_words<- get_stopwords(language = "en")
custom_stopwords <- read_excel(paste0(project_root, '/Resources/stopwords.xlsx'))
stop_words <- bind_rows(stop_words, custom_stopwords)


udmodel_object <- udpipe_load_model(file = paste0(project_root,"/Resources/", model_name))

df_vectorized <- udpipe_annotate(object = udmodel_object, 
                                 x = df$Content,
                                 doc_id = df$ID)


df_vectorized <- as.data.frame(df_vectorized) %>% 
  ## filter out unwanted categories
  filter(upos %in% categories) %>%
  ## make the lemmas lowercase and get rid of special characters that have not been removed
  mutate(lemma = tolower(lemma)) %>%
  mutate(lemma =ifelse(grepl("star",lemma)==FALSE,trimws(gsub("\\w*[0-9]+\\w*\\s*", "", lemma)),lemma))%>%
  mutate(lemma = gsub(c(';|&|[.]|-'), '', lemma)) %>% 
  mutate(lemma = gsub('A©', 'e', lemma)) %>% 
  mutate(sentence = gsub(". .",".",sentence,fixed = TRUE))%>%
  mutate(sentence = gsub(". ?","?",sentence,fixed = TRUE))%>%
  mutate(sentence = gsub("? .","?",sentence,fixed = TRUE))%>%
  mutate(sentence = gsub("! .","!",sentence,fixed = TRUE))%>%
  mutate(sentence = gsub(". !","!",sentence,fixed = TRUE))%>%
  mutate(sentence = gsub('A©', 'e', sentence)) %>% 
  ## remove stopword and adjust synonyms, if needed
  anti_join(stop_words, by = c('lemma' = 'word')) %>% 
  #custom_stopwords <- read_csv(paste0(project_root, 'Resources/custom_stopwords_', language, '_2.csv'))
  #stop_words <- bind_rows(stop_words, custom_stopwords)
  left_join(df, by = c('doc_id' = 'ID')) %>% 
  #mutate(Content = gsub(c(';|&|[.]|-'), '', Content)) %>% 
  mutate(Content = gsub('A©', 'e', Content)) %>%
  mutate(Content = gsub(". .",".",Content,fixed = TRUE))%>%
  mutate(Content = gsub(". ?","?",Content,fixed = TRUE))%>%
  mutate(Content = gsub("? .","?",Content,fixed = TRUE))%>%
  mutate(Content = gsub("! .","!",Content,fixed = TRUE))%>%
  mutate(Content = gsub(". !","!",Content,fixed = TRUE))%>%
  ## filter out lemmas with nchar of 1
  filter(nchar(lemma) > 2)
#mutate(lemma = ifelse(lemma %in% synonyms$synonym, synonyms[synonyms$synonym == lemma, 'lemma', drop = TRUE], lemma))

## add column with all lemmas for each mention
df_vectorized <- df_vectorized[df_vectorized$lemma!="",] %>% 
  group_by(doc_id) %>% 
  mutate(doc_lemmas = paste(lemma, collapse = ' ')) %>% 
  ungroup()%>%
  ## remove lemmas with very small occurrence  
  # group_by(lemma) %>%
  # filter(n() > 3) %>%
  #ungroup()%>% 
  ## replace unwanted characters with their simple character equivalents
  mutate(lemma = iconv(lemma, 'UTF-8', 'UTF-8', sub = '')) %>%
  mutate(lemma = chartr(paste(names(unwanted_array), collapse = ''),
                        paste(unwanted_array, collapse = ''),
                        lemma))


## remove mentions with duplicated set of lemmas
unique_mentions <- df_vectorized %>% 
  select(doc_id, doc_lemmas) %>% 
  distinct()
unique_mentions <- unique_mentions[!duplicated(unique_mentions$doc_lemmas), ]
df_vectorized <- df_vectorized %>% 
  filter(doc_id %in% unique_mentions$doc_id)

save(df_vectorized, 
     file = paste0(project_root, '/Data/_vectorized.RData'))

end_processing <- Sys.time()

cat('Lasting time :', round(difftime(end_processing, start_processing, units = 'mins'), 2), 'minutes')

write.csv2(df_vectorized,paste0(project_root, '/Data/_vectorized_csv.csv'))
