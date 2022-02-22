library(h2o)
library(tidyverse)
library(udpipe)
library(text2vec)
library("xlsx")
library(dplyr)
library(janeaustenr)
library(tidytext)
library(quanteda)

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

modes <- c('percentage',  'treshold')

alternatives <-c("significant","default")

no_default_words<-100

IsLR<-FALSE
IsRF<-TRUE

#Only_significant<-TRUE  

models_results <- data.frame()

start_processing <- Sys.time()

h2o.init()

for (topic in topics)
{
  
  
  for (mode in modes[2]){
    
    for (alternative in alternatives){
  
      #topic<-"hotel"
      
      if (alternative=="significant")
      {
        Outliers_Significant<-read.csv2(file=paste0(project_root, '/Data/output/words_significant/Outliers_Significant_Keywords_LR.csv'))
        Outliers_Significant<-as.matrix(Outliers_Significant[Outliers_Significant[,topic]!="",topic])
        
        
        Inliers_Significant<-read.csv2( file=paste0(project_root, '/Data/output/words_significant/Inliers_Significant_Keywords_LR.csv')
                                        , sheetName = "LR",col.names = TRUE, row.names = TRUE, append = FALSE)
        Inliers_Significant<-as.matrix(Inliers_Significant[Inliers_Significant[,topic]!="",topic])
        
        df_significant<-rbind(Outliers_Significant,Inliers_Significant)
        
      }
      
      
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
      
      # Transform Tidy text format to matrix 
      
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
      
      
      # Significant
      
      if (alternative=="significant"){
          
          columns_vector <- which(colnames(dtm_matrix) %in% df_significant)
          tfidf_matrix <- tfidf_matrix[, columns_vector]
          
          
       }else{
          
        #2.max tf-idf by words  this in article 
       
          outlier_df<-book_words %>%
            left_join(df,by="doc_id")%>%
            filter(relevant==0)
          
          outlier_df<-aggregate(x=outlier_df$tf_idf, by=list(outlier_df$lemma), FUN=max)%>%
            arrange(desc(x))
          
          outlier_df<-outlier_df[1:no_default_words,]
          
          
          inlier_df<-book_words %>%
            left_join(df,by="doc_id")%>%
            filter(relevant==1)
          
          inlier_df<-aggregate(x=inlier_df$tf_idf, by=list(inlier_df$lemma), FUN=max)%>%
            arrange(desc(x))
          
          inlier_df<-inlier_df[1:no_default_words,]
          
          words_default<-rbind(outlier_df,inlier_df)
          words_default<-unique(words_default[,1])
          
          
          columns_vector <- which(colnames(dtm_matrix) %in% words_default)
          tfidf_matrix <- tfidf_matrix[, columns_vector]
          
        } 
      
      #Set all_df for regression model 
    
      all_df<-tfidf_matrix[,2:ncol(tfidf_matrix)] # remove id column 
      
      all_df <- cbind("relevant"=df[,2],all_df)
      
      all_df <- all_df[ , !duplicated(colnames(all_df))]
      # Correlation matrix
      
      #cormat_dtm<-cor(all_df, method = "pearson")
      #cormat<-cor(all_df, method = "pearson")
      
      # library(caret)
      
      #correlated_dtm<-findCorrelation(
      # cormat_dtm,
      # cutoff = 0.7,
      # names = TRUE
      
      # )
      
      # correlated<-findCorrelation(
      #  cormat,
      # cutoff = 0.7,
      #names = TRUE
      
      # )
      
      #columns_vector_dtm <- which(colnames(all_df) %in% correlated_dtm)
      #all_df <- all_df[,-columns_vector_dtm]
      
      #columns_vector <- which(colnames(all_df) %in% correlated)
      #all_df <- all_df[,-columns_vector]
      
      # Keywords by article , to do top 100 words  TO DO 
      
      
      #5b.Clasification tf idf matrix 
      
      ## make train-test split
      
      all_df$relevant<-as.factor(all_df$relevant)
      train_size <- floor(0.8 * nrow(all_df))
      set.seed(123)
      train_vector <- sample(seq_len(nrow(all_df)), size = train_size)
      train_df <- all_df[train_vector, ]
      test_df <- all_df[-train_vector, ]
      
      ## start h2o cluster and load training and testing all_df sets
      #h2o.init()
      
      train.h2o <- as.h2o(train_df)
      test.h2o <- as.h2o(test_df)
   if (IsRF==TRUE)
   {
      
      
      RF <- h2o.randomForest(
        x = setdiff(colnames(train.h2o), 'relevant'),
        y = 'relevant',
        training_frame = train.h2o,
        ntrees = 200,
        seed = 123)
      
      perf_RF <- h2o.performance(RF, test.h2o)
      optimal_threshold_RF <-perf_RF@metrics$max_criteria_and_metric_scores[1, 'threshold']
      
      cat('\nTopic:', topic, 
          '\nModel: RF', 
          '\nNumber of observations:', nrow(all_df),
          '\nNumber of observations test set:', nrow(test_df), '\n',
          '\nPERFORMANCE FOR OPTIMAL F1 VALUE',
          '\nAccuracy:', unlist(h2o.accuracy(perf_RF, optimal_threshold_RF)),
          '\nSensitivity:', unlist(h2o.sensitivity(perf_RF, optimal_threshold_RF)),
          '\nPrecision:', unlist(h2o.precision(perf_RF, optimal_threshold_RF)),
          '\nSpecificity:', unlist(h2o.specificity(perf_RF, optimal_threshold_RF)),
          '\nR2:', h2o.r2(RF),
          '\nAUC:', RF@model$cross_validation_metrics@metrics$AUC, '\n')
      
      h2o.confusionMatrix(perf_RF)
      
      ## create data frame for saving performance of all models
      
      
      colname<-paste(topic,"_RF_",mode,"_",alternative)
      models_results[ c('accuracy'),c(colname)] <- unlist(h2o.accuracy(perf_RF, optimal_threshold_RF))
      models_results[ c('precision'),c(colname)] <- unlist(h2o.precision(perf_RF, optimal_threshold_RF))
      models_results[ c('recall'),c(colname)] <- unlist(h2o.sensitivity(perf_RF, optimal_threshold_RF))
      models_results[c('F1 score'),c(colname)] <- 2*((models_results[ c('precision'),c(colname)]*models_results[ c('recall'),c(colname)])
                                                     /(models_results[ c('precision'),c(colname)]+models_results[ c('recall'),c(colname)]))
      models_results[c('AUC'),c(colname)] <- perf_RF@metrics[["AUC"]]
      models_results[ c('no_observations_train'),c(colname)] <- nrow(train_df)
      models_results[c('no_observations_test'),c(colname)] <- nrow(test_df)
      models_results[ c('no_predictors'),c(colname)] <- ncol(all_df) - 2
   }
      
  if (IsLR==TRUE)
    { 
     
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
      
      perf_glm_dtm <- h2o.performance(glm_dtm, test.h2o)
      optimal_threshold_glm <-perf_glm_dtm@metrics$max_criteria_and_metric_scores[1, 'threshold']
      
      cat('\nTopic:', topic, 
          '\nModel: glm', 
          '\nNumber of observations:', nrow(all_df),
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
      glm_outliers <- glm_dtm@model$coefficients_table %>% 
        filter(p_value <= 0.05 & coefficients < 0 )
      glm_inliers <- glm_dtm@model$coefficients_table %>% 
        filter(p_value <= 0.05 & coefficients > 0 )
      
      #models_results <- data.frame()
      colname<-paste(topic,"LR_",mode,"_",alternative)
      models_results[ c('accuracy'),c(colname)] <- unlist(h2o.accuracy(perf_glm_dtm, optimal_threshold_glm))
      models_results[ c('precision'),c(colname)] <- unlist(h2o.precision(perf_glm_dtm, optimal_threshold_glm))
      models_results[ c('recall'),c(colname)] <- unlist(h2o.sensitivity(perf_glm_dtm, optimal_threshold_glm))
      models_results[c('F1 score'),c(colname)] <- 2*((models_results[ c('precision'),c(colname)]*models_results[ c('recall'),c(colname)])
                                                     /(models_results[ c('precision'),c(colname)]+models_results[ c('recall'),c(colname)]))
      models_results[c('AUC'),c(colname)] <- perf_glm_dtm@metrics[["AUC"]]
      models_results[ c('no_observations_train'),c(colname)] <- nrow(train_df)
      models_results[c('no_observations_test'),c(colname)] <- nrow(test_df)
      models_results[ c('no_predictors'),c(colname)] <- ncol(all_df) - 1
      #models_results[ c('significant outliers'),c(colname)] <- nrow(glm_outliers)
      #models_results[ c('significant inliers'),c(colname)] <- nrow(glm_inliers)
      
      }  

      rm(list=setdiff(ls(), c("project_root","vectorized_data_path","topics","IsLR","IsRF",
                              "topic","relevancy","models_results","modes","mode","alternative","alternatives","start_processing","no_default_words")))
      
      View(models_results) 
          
  } 
  }
}

h2o.removeAll()
h2o.shutdown(prompt = FALSE)
View(models_results)  
 
end_processing <- Sys.time()

cat('LR modeling done in :', round(difftime(end_processing, start_processing, units = 'mins'), 2), 'minutes')

write.csv2(models_results,paste0(project_root, '/Data/output/Keywords_comparison_results.csv'), row.names = TRUE)
