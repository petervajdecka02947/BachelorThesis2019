library(h2o)
library(tidyverse)
library(udpipe)
library(text2vec)
library("xlsx")
library(quanteda)
library(dplyr)
library(janeaustenr)
library(tidytext)

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

modes <- c(
  'percentage',  'treshold'
)

#topic <- topics[2]
#mode<-modes[2, 'label', drop = TRUE]

h2o.init()
models_results <- data.frame()
whole_coef<-data.frame()  

for (topic in topics)
{
  for (mode in modes[2]){
  
    
  load(paste0(project_root, '/Data/intermediate/word_counts/', topic,  '_rel_word_counts_',mode,'.RData'))
  load(paste0(project_root, '/Data/intermediate/word_counts/', topic,  '_irrel_word_counts_',mode,'.RData'))
  top_n_words <- union(topwords_relevant$lemma, topwords_irrelevant$lemma)
 
   
  load(paste0(project_root, '/Data/intermediate/vectorized/', 
              topic, '_','rel_vectorized_',mode,'.RData'))
  df_rel <- topic_lang_label_vectorized
  load(paste0(project_root, '/Data/intermediate/vectorized/', 
              topic, '_','irrel_vectorized_',mode,'.RData'))
  df_irrel <- topic_lang_label_vectorized
  df <- bind_rows(df_rel, df_irrel)
 
  book_words <- df %>%
    unnest_tokens(lemma, Content) %>%
    count(doc_id, lemma, sort = TRUE)
  
  total_words <- book_words %>% 
    group_by(doc_id) %>% 
    summarize(total = sum(n))
  
  book_words <- left_join(book_words, total_words)
  
  
  #TF IDF
  
  book_words <- book_words %>%
    bind_tf_idf(lemma, doc_id, n)
  
  book_words
  
  #high TF IDF
  book_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  # get Frequency matrix ready
  dtm_matrix<-book_words %>%
    cast_dfm(doc_id, lemma, n)
  #set matrix as all_df frame and sort it by doc_id
  dtm_matrix<-as.data.frame(dtm_matrix)%>%
    mutate(document=as.numeric(document))%>%
    arrange(document)
  
  #TF-IDF
  tfidf_matrix<-book_words %>%
    cast_dfm(doc_id, lemma, tf_idf)
  
  #set matrix as all_df frame and sort it by doc_id
  tfidf_matrix<-as.data.frame(tfidf_matrix)%>%
    mutate(document=as.numeric(document))%>%
    arrange(document)
  
  # Remove low frequencies columns 
  columns_vector <- which(colnames(dtm_matrix) %in% top_n_words)
  dtm_matrix <- dtm_matrix[,columns_vector]
  tfidf_matrix <- tfidf_matrix[, columns_vector]
  
  
  #Prepare relevant column as depended variable 
  df<-df %>%
    distinct(doc_id,relevant)%>%
    mutate(doc_id=as.numeric(doc_id))%>%
    arrange(doc_id)
  
  #Set all_df for regression model 
  
  ## dtm matrix
  all_dtm<-dtm_matrix[,2:ncol(dtm_matrix)] # remove id column 
  
  all_dtm <- cbind("relevant"=df[,2],all_dtm)
  
  all_dtm <- all_dtm[ , !duplicated(colnames(all_dtm))]
  
  
  all_df<-tfidf_matrix[,2:ncol(tfidf_matrix)] # remove id column 
  
  all_df <- cbind("relevant"=df[,2],all_df)
  
  all_df <- all_df[ , !duplicated(colnames(all_df))]
  
  
  #4.Classification
  #4a.Clasification term matrix 
  ## make train-test split
  
  all_dtm$relevant<-as.factor(all_dtm$relevant)
  train_size <- floor(0.8 * nrow(all_dtm))
  set.seed(123)
  train_vector <- sample(seq_len(nrow(all_dtm)), size = train_size)
  train_df <- all_dtm[train_vector, ]
  test_df <- all_dtm[-train_vector, ]
  
  ## start h2o cluster and load training and testing all_df sets
  #h2o.init() #max_mem_size = "20g"
  train.h2o <- as.h2o(train_df)
  test.h2o <- as.h2o(test_df)
  
  start_processing_LR <- Sys.time()
  glm_dtm <- h2o.glm(x = setdiff(colnames(train.h2o), 'relevant'),
                     y = 'relevant',
                     training_frame = train.h2o, 
                     seed = 123, 
                     family = 'binomial',
                     #lambda_search = TRUE, 
                     lambda = 0,
                     alpha = 0.5, 
                     compute_p_values = TRUE,
                     remove_collinear_columns = TRUE,
                     nfolds = 5)
  end_processing_LR <- Sys.time()
  
  cat('LR modeling done in :', round(difftime(end_processing_LR, start_processing_LR, units = 'mins'), 2), 'minutes')
  
  perf_glm_dtm <- h2o.performance(glm_dtm, test.h2o)
  optimal_threshold_glm <-perf_glm_dtm@metrics$max_criteria_and_metric_scores[1, 'threshold']
  
  cat('\nTopic:', topic, 
      '\nModel: glm', 
      '\nNumber of observations:', nrow(all_dtm),
      '\nNumber of observations test set:', nrow(test_df), '\n',
      '\nPERFORMANCE FOR OPTIMAL F1 VALUE',
      '\nAccuracy:', unlist(h2o.accuracy(perf_glm_dtm, optimal_threshold_glm)),
      '\nSensitivity:', unlist(h2o.sensitivity(perf_glm_dtm, optimal_threshold_glm)),
      '\nPrecision:', unlist(h2o.precision(perf_glm_dtm, optimal_threshold_glm)),
      '\nSpecificity:', unlist(h2o.specificity(perf_glm_dtm, optimal_threshold_glm)),
      '\nR2:', h2o.r2(glm_dtm),
      '\nAUC:', glm_dtm@model$cross_validation_metrics@metrics$AUC, '\n')
  
  h2o.confusionMatrix(perf_glm_dtm)
  
  ## create data frame for saving performance of all models
  glm_dtm_coefficients_outliers <- glm_dtm@model$coefficients_table %>% 
    filter(p_value <= 0.05 & coefficients < 0 )
  glm_dtm_coefficients_inliers <- glm_dtm@model$coefficients_table %>% 
    filter(p_value <= 0.05 & coefficients > 0 )
  
  #models_results <- data.frame()
  colname<-paste(topic,"LR_Freq_",mode)
  models_results[ c('accuracy'),c(colname)] <- unlist(h2o.accuracy(perf_glm_dtm, optimal_threshold_glm))
  models_results[ c('precision'),c(colname)] <- unlist(h2o.precision(perf_glm_dtm, optimal_threshold_glm))
  models_results[ c('recall'),c(colname)] <- unlist(h2o.sensitivity(perf_glm_dtm, optimal_threshold_glm))
  models_results[c('F1 score'),c(colname)] <- 2*((models_results[ c('precision'),c(colname)]*models_results[ c('recall'),c(colname)])
                                                 /(models_results[ c('precision'),c(colname)]+models_results[ c('recall'),c(colname)]))
  models_results[c('AUC'),c(colname)] <- perf_glm_dtm@metrics[["AUC"]]
  models_results[ c('no_observations_train'),c(colname)] <- nrow(train_df)
  models_results[c('no_observations_test'),c(colname)] <- nrow(test_df)
  models_results[ c('no_predictors'),c(colname)] <- ncol(all_df) - 1
  models_results[ c('significant outliers'),c(colname)] <- nrow(glm_dtm_coefficients_outliers)
  models_results[ c('significant inliers'),c(colname)] <- nrow(glm_dtm_coefficients_inliers)
  
  write.xlsx2(glm_dtm_coefficients_outliers, file=paste0(project_root, '/Data/output/',topic,'_coefLR_out_',mode,'.xlsx')
              , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)
  write.xlsx2(glm_dtm_coefficients_inliers, file=paste0(project_root, '/Data/output/',topic,'_coefLR_in_',mode,'.xlsx')
              , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)
  
  
  View(models_results)
  
  #4b.Clasification tf idf matrix 
  
  ## make train-test split
  
  all_df$relevant<-as.factor(all_df$relevant)
  train_size <- floor(0.8 * nrow(all_df))
  set.seed(123)
  train_vector <- sample(seq_len(nrow(all_df)), size = train_size)
  train_df <- all_df[train_vector, ]
  test_df <- all_df[-train_vector, ]
  
  ## start h2o cluster and load training and testing all_df sets
  #h2o.init()
  trainTf.h2o <- as.h2o(train_df)
  testTf.h2o <- as.h2o(test_df)
  
  start_processing_LR <- Sys.time()
  glm <- h2o.glm(x = setdiff(colnames(trainTf.h2o), 'relevant'),
                 y = 'relevant',
                 training_frame = trainTf.h2o, 
                 seed = 123, 
                 family = 'binomial',
                 #lambda_search = TRUE, 
                 lambda = 0,
                 alpha = 0.5, 
                 compute_p_values = TRUE,
                 remove_collinear_columns = TRUE,
                 nfolds = 5)
  end_processing_LR <- Sys.time()
  
  
  cat('LR modeling done in :', round(difftime(end_processing_LR, start_processing_LR, units = 'mins'), 2), 'minutes')
  
  perf_glm <- h2o.performance(glm, testTf.h2o)
  optimal_threshold_glm <-perf_glm@metrics$max_criteria_and_metric_scores[1, 'threshold']
  
  cat('\nTopic:', topic, 
      '\nModel: glm', 
      '\nNumber of observations:', nrow(all_df),
      '\nNumber of observations test set:', nrow(test_df), '\n',
      '\nPERFORMANCE FOR OPTIMAL F1 VALUE',
      '\nAccuracy:', unlist(h2o.accuracy(perf_glm, optimal_threshold_glm)),
      '\nSensitivity:', unlist(h2o.sensitivity(perf_glm, optimal_threshold_glm)),
      '\nPrecision:', unlist(h2o.precision(perf_glm, optimal_threshold_glm)),
      '\nSpecificity:', unlist(h2o.specificity(perf_glm, optimal_threshold_glm)),
      '\nR2:', h2o.r2(glm),
      '\nAUC:', glm@model$cross_validation_metrics@metrics$AUC, '\n')
  
  h2o.confusionMatrix(perf_glm)
  
  ## create data frame for saving performance of all models
  
  glm_coefficients_outliers <- glm@model$coefficients_table %>% 
    filter(p_value <= 0.05 & coefficients < 0 )
  glm_coefficients_inliers <- glm@model$coefficients_table %>% 
    filter(p_value <= 0.05 & coefficients > 0 )
  
  glm_coefficients_outliers$type<-"outlier"
  glm_coefficients_inliers$type<-"inlier"
  glm_coefficients_outliers$topic<-topic
  glm_coefficients_inliers$topic<-topic
  glm_coefficients_outliers$mode<-mode
  glm_coefficients_inliers$mode<-mode
  
  colname<-paste(topic,"LR_Tf-Idf",mode)
  models_results[ c('accuracy'),c(colname)] <- unlist(h2o.accuracy(perf_glm, optimal_threshold_glm))
  models_results[ c('precision'),c(colname)] <- unlist(h2o.precision(perf_glm, optimal_threshold_glm))
  models_results[ c('recall'),c(colname)] <- unlist(h2o.sensitivity(perf_glm, optimal_threshold_glm))
  models_results[c('F1 score'),c(colname)] <- 2*((models_results[ c('precision'),c(colname)]*models_results[ c('recall'),c(colname)])
                                                 /(models_results[ c('precision'),c(colname)]+models_results[ c('recall'),c(colname)]))
  models_results[c('AUC'),c(colname)] <- perf_glm@metrics[["AUC"]]
  models_results[ c('no_observations_train'),c(colname)] <- nrow(train_df)
  models_results[c('no_observations_test'),c(colname)] <- nrow(test_df)
  models_results[ c('no_predictors'),c(colname)] <- ncol(all_df) - 2
  models_results[ c('significant outliers'),c(colname)] <- nrow(glm_coefficients_outliers)
  models_results[ c('significant inliers'),c(colname)] <- nrow(glm_coefficients_inliers)
  
  whole_coef<-rbind(whole_coef,glm_coefficients_outliers,glm_coefficients_inliers)
  
  write.xlsx2(glm_coefficients_outliers, file=paste0(project_root, '/Data/output/',topic,'_coefLR_TfIdf_out_',mode,'.xlsx')
              , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)
  write.xlsx2(glm_coefficients_inliers, file=paste0(project_root, '/Data/output/',topic,'_coefLR_TfIdf_in_',mode,'.xlsx')
              , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)

  
  rm(list=setdiff(ls(), c("project_root","vectorized_data_path","topics","topic","languages","relevancy","lang","models_results","mode","modes","whole_coef")))
  
  
  
 }
}

h2o.removeAll()
h2o.shutdown(prompt = FALSE)
View(models_results)

write.csv2(whole_coef,paste0(project_root, '/Data/output/Coeficients_all.csv'),row.names=FALSE)
write.xlsx2(models_results,paste0(project_root, '/Data/output/LR_results.xlsx'), row.names = TRUE)
