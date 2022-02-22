library(tidyverse)
library(tidytext)
library(h2o)
library(data.table)
library(spatstat)
library(xlsx)
require(sp)

## set up info about the data
topics <- c('hotel', 
            'restaurant', 
            'attraction',
            'train')

modes <- tribble(
  ~value, ~label, 
  0.4, 'percentage',   # 0.41
  1.1, 'treshold'      # 1.09
)

NoToValidate<-100

## set paths to relevant data folders
setwd("C:/Users/PeterVajdecka/Desktop/bc progress/")
project_root <- getwd() 

load(paste0(project_root,"/Data/_vectorized.RData"))

AverageNNI <- function(x, win = "hull",k) {
  # if(class(x) == "sf") { x <- as(x, "Spatial") }
  if (!class(x) == "SpatialPointsDataFrame" & !class(x) == "SpatialPoints") 
    stop(deparse(substitute(x)), " MUST BE A sp POINTS OBJECT")
  if (win == "hull") {
    w <- spatstat::convexhull.xy(sp::coordinates(x))
  }
  if (win == "extent") {
    e <- as.vector(sp::bbox(x))
    w <- spatstat::as.owin(c(e[1], e[3], e[2], e[4]))
  }
  x <- spatstat::as.ppp(sp::coordinates(x), w)
  A <- spatstat::area.owin(w)
  obsMeanDist <- sum(spatstat::nndist(x,k=k))/x$n
  expMeanDist <- 0.5 * sqrt(A / x$n)
  se <- 0.26136 / ((x$n**2.0 / A)**0.5)
  nni <- obsMeanDist / expMeanDist
  z <- (obsMeanDist - expMeanDist) / se
  return(list(NNI = nni, z.score = z, p = round(2*stats::pnorm(-abs(z)),-4),SE=se,  
              expected.mean.distance = expMeanDist,
              observed.mean.distance = obsMeanDist))
}

for (topic in topics){
 
if (topic=="hotel")
  {
    hotel<-sample((df_vectorized %>% 
    distinct(doc_id,hotel,restaurant,attraction,train)%>%
    filter(hotel==1,restaurant==0,attraction==0,train==0))$doc_id,NoToValidate)
    
 }else if(topic=="restaurant"){
   restaurant<-sample((df_vectorized %>% 
                    distinct(doc_id,hotel,restaurant,attraction,train)%>%
                    filter(hotel==0,restaurant==1,attraction==0,train==0))$doc_id,NoToValidate)
    
 }else if(topic=="attraction"){
   attraction<-sample((df_vectorized %>% 
                         distinct(doc_id,hotel,restaurant,attraction,train)%>%
                         filter(hotel==0,restaurant==0,attraction==1,train==0))$doc_id,NoToValidate)
    
 }else{# train
   train<-sample((df_vectorized %>% 
                         distinct(doc_id,hotel,restaurant,attraction,train)%>%
                         filter(hotel==0,restaurant==0,attraction==0,train==1))$doc_id,NoToValidate)
    
 }
  
}

all<-c(hotel,restaurant,train,attraction)

#set 1 to all topics columns in selected row_ids

df_vectorized[df_vectorized$doc_id %in% all,topics]<-1

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
                          vec_size = 128,window_size = 5, sent_sample_rate = 0,init_learning_rate=0, epochs = 10)
end_processing <- Sys.time()

nrow(h2o.toFrame(w2v.model))


print("Calculate a vector for each dialogue")
Doc2v.model <-as.data.frame(h2o.transform(w2v.model, df_words, aggregate_method = "AVERAGE"))

#print("Sanity check - find synonyms for the word 'teacher'")
#print(h2o.findSynonyms(w2v.model, "train", count = 30))

df_vectorized<-df_vectorized[,-c("lemma","Content")]

#df_vectorized<-df_vectorized[!duplicated(df_vectorized$doc_id),]

Allmodels.data<-as.data.frame(unique(df_vectorized)) %>% 
  cbind(as.data.frame(Doc2v.model))

colnames(Doc2v.model)

#h2o.removeAll()
#h2o.shutdown(prompt = FALSE)


models_results <- data.frame()
df_spatial_whole<-data.frame()
#topic<-"hotel"

for (topic in topics)
{
  
  
  
  #topic<-"attraction"
  #Set dependent col
  
  
  df_vectorized_notopics<-Allmodels.data[ , !(names(Allmodels.data) %in% topics)] 
  df_vectorized_onetopic<-Allmodels.data[ , names(Allmodels.data) %in% topic] 
  
  topic_lang_label_vectorized<-cbind(df_vectorized_notopics,df_vectorized_onetopic)
  colnames(topic_lang_label_vectorized)[ncol(topic_lang_label_vectorized)]<-"relevancy"
  # Test F1 score 
  
  library(data.table)
  
  index_first_col <- which(colnames(topic_lang_label_vectorized)=="relevancy")
  
  
  #Test Accuracy of the model
  
  ID<-topic_lang_label_vectorized[,1]
  topic_lang_label_vectorized<-topic_lang_label_vectorized[,2:ncol(topic_lang_label_vectorized)]
  topic_lang_label_vectorized$relevancy<-as.factor(topic_lang_label_vectorized$relevancy)
  
  # create matrix with only dialogs with certain topic
  
  topic_lang_label_vectorized<-cbind(ID,topic_lang_label_vectorized)
  
  index_first_col <- which(colnames(topic_lang_label_vectorized)=="relevancy")
  
  
  # create matrix with only dialogs with certain topic
  
  topic_lang_label_vectorized<-topic_lang_label_vectorized[
    topic_lang_label_vectorized[,index_first_col]==1,]
  
  
  # LOF score and separation 
  
  
  library(dbscan)
  
  outlier.scores <-lof(topic_lang_label_vectorized[,-c(1,index_first_col)], k=9)
  
  total<-as.data.frame(cbind(ID=topic_lang_label_vectorized$ID,outlier.scores))
  
  total<-total[order(desc(total[,2])),]
  
  total<-as.data.frame(total)
  
  df_results<-data.frame()
  df_spatial<-data.frame()
  total_both<-data.frame()
  #i<-1
  for (i in 1:nrow(modes)){
    total_ad<-total
    df_spatial<-data.frame()
    mode <- modes[i, 'label', drop = TRUE]
    mode_value<-modes[i, 'value', drop = TRUE]
    
    if (mode=="percentage")
    {
      total_ad$relevant<-1
      # Take top 10 %
      total_ad$relevant[1:round(mode_value*nrow(total_ad))]<-0
      
    }else{
      # LOF higher than 1.1
      total_ad<-total_ad  %>%
        mutate(relevant=ifelse(outlier.scores>mode_value,0,1))
    }
    
    
    #1.Statistical test for average distance
    usual_df<-total_ad[,names(total_ad)!="relevancy"] %>% left_join(topic_lang_label_vectorized,by="ID")%>%filter(relevant==1)
    unusual_df<-total_ad[,names(total_ad)!="relevancy"] %>% left_join(topic_lang_label_vectorized,by="ID")%>%filter(relevant==0)
    usual_df<-as.data.frame(usual_df[,!names(usual_df) %in% c("ID","outlier.scores","relevant","relevancy")])
    unusual_df<-as.data.frame(unusual_df[,!names(unusual_df) %in% c("ID","outlier.scores","relevant","relevancy")])
    coordinates(usual_df)<-~.
    coordinates(unusual_df)<-~.
    Usual_model<-AverageNNI(usual_df,k=9)
    Unusual_model<-AverageNNI(unusual_df,k=9)
    
    rowname<-paste(topic,"_",mode,"_usual")
    df_spatial[ c(rowname),c("NNI")]<-Usual_model$NNI
    df_spatial[ c(rowname),c("p_value")]<-Usual_model$p
    df_spatial[ c(rowname),c("str.dev.")]<-Usual_model$SE
    rowname<-paste(topic,"_",mode,"_unusual")
    df_spatial[ c(rowname),c("NNI")]<-Unusual_model$NNI
    df_spatial[ c(rowname),c("p_value")]<-Unusual_model$p
    df_spatial[ c(rowname),c("str.dev.")]<-Unusual_model$SE
    
    df_spatial_whole<-rbind(df_spatial_whole,df_spatial)
    
    #2.fabricated response insertion
    rowname<-paste(topic,"_",mode)
    total_ad$mode<-mode
    total_ad<-total_ad[,c("ID","relevant","mode")]
    total_ad$topic<-topic
    total_ad<-total_ad[total_ad$ID %in% all,]
    total_both<-rbind(total_both,total_ad)
    
    
    for (topic_row in topics)
    {  # count number of set outliers
      df_results[ c(rowname),c(topic_row)] <- ifelse(topic_row==topic,"X",
                                                 nrow(total_both[total_both$mode==mode & total_both$ID %in% get(topic_row)
                                                            & total_both$relevant==0,]))
    
    }
    
      df_results[ c(rowname),"Total_%"]<-paste(round(sum(df_results[ c(rowname),names(df_results)!=topic & 
                                                                     names(df_results) %in% topics])
                                                   /((length(topics)-1)*NoToValidate)*100,1),"%")
    
  }
  
  
  models_results<-rbind(models_results,df_results)
}

write.xlsx2(df_spatial_whole, paste0(project_root, '/Data/output/validation/Spatial_Statistics.xlsx'), sheetName = "results",row.names = TRUE)
write.xlsx2(models_results, paste0(project_root, '/Data/output/validation/Fabricated_Response.xlsx'), sheetName = "results",row.names = TRUE)

