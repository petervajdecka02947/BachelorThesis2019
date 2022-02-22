
###################################################################################################################################################
library(jsonlite)
library(tidyverse)


## set up folder structure
project_root <- 'C:/Users/PeterVajdecka/Desktop/bc progress'
raw_data_path <- c(paste0(project_root, '/Data/raw/'))

json_data <- fromJSON(paste0(raw_data_path,'data.json'))

Topic<-c()
df<-data.frame(Topic)

for(i in 1:length(json_data))
{
  
  
  df[i,1]<-paste(json_data[[i]][["log"]][["text"]], collapse = " . ")
  
  df$ID[i]<-paste(i)
  
  if (length(json_data[[i]][["goal"]][["hotel"]])!=0)
  {
    df$hotel[i]<-1
  }else{
    
    df$hotel[i]<-0
  }
  

  if (length(json_data[[i]][["goal"]][["restaurant"]])!=0)
  {
    df$restaurant[i]<-1
  }else{
    
    df$restaurant[i]<-0
  }
  
  if (length(json_data[[i]][["goal"]][["attraction"]])!=0)
  {
    df$attraction[i]<-1
  }else{
    
    df$attraction[i]<-0
  }
  
  if (length(json_data[[i]][["goal"]][["train"]])!=0)
  {
    df$train[i]<-1
  }else{
    
    df$train[i]<-0
  }
  
  
  
}

#df <- tibble::rowid_to_column(df, "ID")

df<-df %>%
  rename("Content"=V1)



## remove duplicated rows
df <- df[!duplicated(df), ]
## remove rows where text is NA
df <- df[!is.na(df$Content), ]

save(df, 
     file = paste0(project_root, '/Data/intermediate/compiled/AllTopics_processed.RData'))

################################## Test
#df$suma=rowSums(df[,2:ncol(df)])
#min(df$suma)

#hotel       taxi restaurant     police   hospital    general attraction      train    booking       suma 
#4197       2057       4692        245        287          0       3515       4096          0      19089 








###################################################################################################################################################
library(jsonlite)
library(tidyverse)

# make ready for overal destriptives 

topics <- c('hotel',  
            'restaurant', 
             'attraction',
            'train')

## set up folder structure
project_root <- getwd()
raw_data_path <- c(paste0(project_root, '/Data/raw/'))

json_data <- fromJSON(paste0(raw_data_path,'data.json'))

Topic<-c()

for (j in 1:length(topics))
{
  print(j)
  topic<-topics[j]
  topic_data<-data.frame(Topic)
  
  for(i in 1:length(json_data))
  {
    
      
      topic_data[i,1]<-paste(json_data[[i]][["log"]][["text"]], collapse = " . ")
      
      topic_data$ID[i]<-paste(i)
      
      
      if (length(json_data[[i]][["goal"]][[topic]])!=0)
      {
        topic_data$relevant[i]<-1
      }else{
        
        topic_data$relevant[i]<-0 
      }
    
    
   }
  
  topic_data<-topic_data %>%
    rename("Content"=V1)
  
  ## remove duplicated rows
  topic_data <- topic_data[!duplicated(topic_data), ]
  ## remove rows where text is NA
  topic_data <- topic_data[!is.na(topic_data$Content), ]
  
  save(topic_data, 
       file = paste0(project_root, '/Data/intermediate/compiled/',topic,'_processed.RData'))
  
}

#topic_data <- tibble::rowid_to_column(topic_data, "ID")



################################## Test
#topic_data$suma=rowSums(topic_data[,2:ncol(topic_data)])
#min(topic_data$suma)

#hotel       taxi restaurant     police   hospital    general attraction      train    booking       suma 
#4197       2057       4692        245        287          0       3515       4096          0      19089 
