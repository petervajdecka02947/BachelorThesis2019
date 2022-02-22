library("xlsx")

## set up info about the data
topics <- c('hotel',
            'restaurant',  
            'attraction',
            'train')

setwd("C:/Users/PeterVajdecka/Desktop/bc progress/")
project_root <- getwd()

top_keywords<-100

words_outliers<-data.frame()
words_inliers<-data.frame()

pvalues_outliers<-data.frame()
pvalues_inliers<-data.frame()

#topic<-"hotel"

for (topic in topics)
{
  print(topic)
  colname<-colname<-paste(topic,"LR_Tf-Idf treshold")
  
  Outliers<- read.xlsx2(file=paste0(project_root, '/Data/output/',topic,'_coefLR_TfIdf_out_treshold.xlsx')
                        , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)
  Outliers<-Outliers[order(Outliers$p_value),] 
  #if (nrow(Outliers)>top_keywords){Outliers<-Outliers[1:top_keywords,]}
  
  Inliers<- read.xlsx2(file=paste0(project_root, '/Data/output/',topic,'_coefLR_TfIdf_in_treshold.xlsx')
                       , sheetName = colname,col.names = TRUE, row.names = TRUE, append = FALSE)
  Inliers<-Inliers[order(Inliers$p_value),]
  #if (nrow(Outliers)>top_keywords){Outliers<-Outliers[1:top_keywords,]}
  
  words_outliers[1:nrow(Outliers),c(topic)]<-Outliers[,1]
  words_inliers[1:nrow(Inliers),c(topic)]<-Inliers[,1]
  
  pvalues_outliers[1:nrow(Outliers),c(topic)]<-Outliers[,5]
  pvalues_inliers[1:nrow(Inliers),c(topic)]<-Inliers[,5]
  
}

# check whether there are some coeficient similar whithin a topic 



words_outliers<-as.matrix(words_outliers)
words_inliers<-as.matrix(words_inliers)

pvalues_outliers<-as.matrix(pvalues_outliers)
pvalues_inliers<-as.matrix(pvalues_inliers)



# set unique key words for each topic by max p value 

for (topic in topics)
{
  print(topic)
  
  for(i in 1:nrow(words_outliers)){
    
    
    if((words_outliers[i,topic] %in% (words_outliers[,!colnames(words_outliers) %in% topic])) & is.na(words_outliers[i,topic])==FALSE) {
      
      
      
      for (g in colnames((words_outliers[,!colnames(words_outliers) %in% topic])))
      {
        
        for (j in 1:nrow(words_outliers) ){
        
          if(words_outliers[j,g]==words_outliers[i,topic] & pvalues_outliers[j,g] < pvalues_outliers[i,topic]
             & is.na(words_outliers[j,g])==FALSE & is.na(words_outliers[i,topic])==FALSE )
          {
            words_outliers[i,topic]=NA
          }
          
        }
      }
      
    }
  
    
  }
  
  
  
  for(i in 1:nrow(words_inliers)){
    

    
    if((words_inliers[i,topic] %in% (words_inliers[,!colnames(words_inliers) %in% topic])) & is.na(words_inliers[i,topic])==FALSE) 
    {
      
          for (g in colnames((words_inliers[,!colnames(words_inliers) %in% topic])))
          {
            
            for (j in 1:nrow(words_inliers) ){
              
              
              if(words_inliers[j,g]==words_inliers[i,topic] & pvalues_inliers[j,g] < pvalues_inliers[i,topic]
                 & is.na(words_inliers[j,g])==FALSE & is.na(words_inliers[i,topic])==FALSE )
              {
                words_inliers[i,topic]=NA
              }
              
            }
          }
      
     
      
    }
    
  }
  
}

#sort to remove useless rows in matrix

for (f in 1:length(topics)){
  # Inliers
  no_notEmpty_inliers <-nrow(words_inliers[is.na(words_inliers[,f])==FALSE,])
  NotEmpty_inliers<-words_inliers[is.na(words_inliers[,f])==FALSE,f]
  
  if (no_notEmpty_inliers>top_keywords){
    NotEmpty_inliers<-NotEmpty_inliers[1:top_keywords]
    no_notEmpty_inliers <-top_keywords
  }
  
  no_Empty_inliers<-nrow(words_inliers)-no_notEmpty_inliers
  
  EmptyWords_inliers<-c(1:no_Empty_inliers)
  EmptyWords_inliers[1:no_Empty_inliers]<-NA
  words_inliers[,f]<-c(NotEmpty_inliers,EmptyWords_inliers)
 #Outliers
  no_notEmpty_outliers <-nrow(words_outliers[is.na(words_outliers[,f])==FALSE,])
  NotEmpty_outliers<-words_outliers[is.na(words_outliers[,f])==FALSE,f]
  
  if (no_notEmpty_outliers>top_keywords){
    NotEmpty_outliers<-NotEmpty_outliers[1:top_keywords]
    no_notEmpty_outliers <-top_keywords
  }
  
  no_Empty_outliers<-nrow(words_outliers)-no_notEmpty_outliers
  
  EmptyWords_outliers<-c(1:no_Empty_outliers)
  EmptyWords_outliers[1:no_Empty_outliers]<-NA
  words_outliers[,f]<-c(NotEmpty_outliers,EmptyWords_outliers)
  
}

# remove all last empty rows

words_outliers<-words_outliers[rowSums(is.na(words_outliers)) != ncol(words_outliers),]
words_inliers<-words_inliers[rowSums(is.na(words_inliers)) != ncol(words_inliers),]


write.csv2(words_outliers, file=paste0(project_root, '/Data/output/words_significant/Outliers_Significant_Keywords_LR.csv')
           ,row.names = FALSE)
write.csv2(words_inliers, file=paste0(project_root, '/Data/output/words_significant/Inliers_Significant_Keywords_LR.csv')
           ,row.names = FALSE)

