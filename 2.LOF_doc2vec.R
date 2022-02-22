library(tidyverse)
library(tidytext)
library(h2o)
library(data.table)
library(xlsx)

# The following two commands remove any previously installed H2O packages for R.
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
#pkgs <- c("RCurl", "jsonlite", "statmod", "devtools", "roxygen2", "testthat")
#for (pkg in pkgs) {
#  if (! (pkg %in% rownames(installed.packages()))) install.packages(pkg)
#}


#install.packages("h2o")
#install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/master/4987/R")


## set paths to relevant data folders
setwd("C:/Users/PeterVajdecka/Desktop/bc progress/")
project_root <- getwd() 
compiled_data_path <- c(paste0(project_root, '/Data/intermediate/compiled/'))

## set up info about the data
topics <- c('hotel', 
            'restaurant', 
             'attraction',
            'train')

modes <- tribble(
  ~value, ~label, 
  0.4, 'percentage', 
  1.1, 'treshold'
)


load(paste0(project_root,"/Data/_vectorized.RData"))


h2o.init()

# Add empty rows after different doc_id

df_vectorized<-cbind(df_vectorized[,c("doc_id","lemma","Content")],df_vectorized[,topics])

df_vectorized$doc_id<-as.numeric(df_vectorized$doc_id)

print(paste("Unique lemmas :",length(unique(df_vectorized$lemma))))
print(paste("Unique IDs :",length(unique(df_vectorized$doc_id))))
# Add empty row after different IDS

df_vectorized <- rbind(setDT(df_vectorized), df_vectorized[, tail(.SD, 1) , by = doc_id][, c("lemma") := .(NA)])[order(doc_id)]
df_vectorized<-df_vectorized[-nrow(df_vectorized),]

# Set only words by Contents

df_words<-as.h2o(df_vectorized$lemma)
df_words<-as.character(df_words)

print("Build word2vec model")
start_processing <- Sys.time()
w2v.model <- h2o.word2vec(df_words,min_word_freq=0,word_model = "CBOW", #c("SkipGram", "CBOW")
                          vec_size = 128,window_size = 5, sent_sample_rate = 0,init_learning_rate=0, epochs = 100)
end_processing <- Sys.time()

nrow(h2o.toFrame(w2v.model))


print("Calculate a vector for each dialogue")
Doc2v.model <-as.data.frame(h2o.transform(w2v.model, df_words, aggregate_method = "AVERAGE"))

#print("Sanity check - find synonyms for the word 'teacher'")
#print(h2o.findSynonyms(w2v.model, "train", count = 30))

df_vectorized<-df_vectorized[,-c("lemma","Content")]

Allmodels.data<-as.data.frame(unique(df_vectorized)) %>% 
  cbind(as.data.frame(Doc2v.model))

colnames(Doc2v.model)

#h2o.removeAll()
#h2o.shutdown(prompt = FALSE)


models_results <- data.frame()
df_results<- data.frame()
for (topic in topics)
{



  #topic<-"attraction"
#Set dependent col


df_vectorized_notopics<-Allmodels.data[ , !(names(Allmodels.data) %in% topics)] 
df_vectorized_onetopic<-Allmodels.data[ , names(Allmodels.data) %in% topic] 

topic_lang_label_vectorized<-cbind(df_vectorized_notopics,df_vectorized_onetopic)
colnames(topic_lang_label_vectorized)[ncol(topic_lang_label_vectorized)]<-"relevancy"
# Test F1 score 

index_first_col <- which(colnames(topic_lang_label_vectorized)=="relevancy")


#Test Accuracy of the model

ID<-topic_lang_label_vectorized[,1]
topic_lang_label_vectorized<-topic_lang_label_vectorized[,2:ncol(topic_lang_label_vectorized)]
topic_lang_label_vectorized$relevancy<-as.factor(topic_lang_label_vectorized$relevancy)

train_size <- floor(0.8 * nrow(topic_lang_label_vectorized))
set.seed(123)
train_vector <- sample(seq_len(nrow(topic_lang_label_vectorized)), size = train_size)
train_df <- topic_lang_label_vectorized[train_vector, ]
test_df <- topic_lang_label_vectorized[-train_vector, ]

## start h2o cluster and load training and testing all_df sets
#h2o.init() #max_mem_size = "20g"
train.h2o <- as.h2o(train_df)
test.h2o <- as.h2o(test_df)

start_processing_LR <- Sys.time()
glm_dtm <- h2o.glm(x = setdiff(colnames(train.h2o), 'relevancy'),
                   y = 'relevancy',
                   training_frame = train.h2o, 
                   seed = 123, 
                   family = 'binomial',
                   #lambda_search = TRUE, 
                   lambda = 0,
                   alpha = 0.5, 
                   compute_p_values = FALSE,
                   remove_collinear_columns = TRUE,
                   nfolds = 5)
end_processing_LR <- Sys.time()

perf_glm_dtm <- h2o.performance(glm_dtm, test.h2o)
optimal_threshold_glm <-perf_glm_dtm@metrics$max_criteria_and_metric_scores[1, 'threshold']


# create matrix with only dialogs with certain topic

topic_lang_label_vectorized<-cbind(ID,topic_lang_label_vectorized)

index_first_col <- which(colnames(topic_lang_label_vectorized)=="relevancy")


# Set performance for each topic 

#models_results <- data.frame()
colname<-paste("LR_doc2vec_",topic)
models_results[ c('accuracy'),c(colname)] <- unlist(h2o.accuracy(perf_glm_dtm, optimal_threshold_glm))
models_results[ c('precision'),c(colname)] <- unlist(h2o.precision(perf_glm_dtm, optimal_threshold_glm))
models_results[ c('recall'),c(colname)] <- unlist(h2o.sensitivity(perf_glm_dtm, optimal_threshold_glm))
models_results[c('F1 score'),c(colname)] <- 2*((models_results[ c('precision'),c(colname)]*models_results[ c('recall'),c(colname)])
                                               /(models_results[ c('precision'),c(colname)]+models_results[ c('recall'),c(colname)]))
models_results[c('AUC'),c(colname)] <- perf_glm_dtm@metrics[["AUC"]]
models_results[ c('no_observations_train'),c(colname)] <- nrow(train_df)
models_results[c('no_observations_test'),c(colname)] <- nrow(test_df)

df_results[ c('relevantne'),c(topic)] <- nrow(topic_lang_label_vectorized[topic_lang_label_vectorized[,index_first_col]==1,])
df_results[ c('irelevantne'),c(topic)] <- nrow(topic_lang_label_vectorized[topic_lang_label_vectorized[,index_first_col]==0,])

# create matrix with only dialogs with certain topic


topic_lang_label_vectorized<-topic_lang_label_vectorized[
  topic_lang_label_vectorized[,index_first_col]==1,]


# LOF score and separation 


library(dbscan)

outlier.scores <-lof(topic_lang_label_vectorized[,-c(1,index_first_col)], k=9)

total<-as.data.frame(cbind(ID=topic_lang_label_vectorized$ID,outlier.scores))

total<-total[order(desc(total[,2])),]

for (i in 1:nrow(modes)){
  
  mode <- modes[i, 'label', drop = TRUE]
  mode_value<-modes[i, 'value', drop = TRUE]
  
if (mode=="percentage")
{
total$relevant<-1
# Take top 10 %
total$relevant[1:round(mode_value*nrow(total))]<-0
}else{
  # LOF higher than 1.1
total<-total  %>%
  mutate(relevant=ifelse(outlier.scores>mode_value,0,1))
}
  
print(topic)
print(summary(outlier.scores))

#models_results[ c('No Outliers'),c(colname)]<- nrow(total[total$relevant==0,])
#models_results[ c('No Inliers'),c(colname)]<- nrow(total[total$relevant==1,])


df_results[ c('odlahle'),c(topic)] <- nrow(total[total$relevant==0,])
df_results[ c('prilahle'),c(topic)] <- nrow(total[total$relevant==1,])

save(total, 
     file = paste0(project_root, '/Data/intermediate/outlier_seperated/',
                   topic, '_outlier_seperated_',mode,'.RData'))

}
}

h2o.removeAll()
h2o.shutdown(prompt = FALSE)
View(models_results)
write.xlsx2(models_results, file=paste0(project_root, '/Data/intermediate/outlier_seperated/doc2vec_results.xlsx')
            , sheetName = "LR",col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx2(df_results, file=paste0(project_root, '/Data/output/descriptives/descriptive_results.xlsx')
            , sheetName = "LR",col.names = TRUE, row.names = TRUE, append = FALSE)

