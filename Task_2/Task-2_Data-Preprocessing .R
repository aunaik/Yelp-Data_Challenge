# Authors: Akshay and Saniya

require(data.table)
require(dplyr)
library("NLP")
library("openNLP")
library(stringr)
require(quanteda)
require(qdap)

require(caret)
require(e1071)

text_train_data<-read.csv("TestingReviewData.csv",stringsAsFactors = F,row.names = NULL)
View(text_train_data)
str(text_train_data)

#Building Vocabulary with only nouns
txt<-text_train_data$text
txt

vocab_noun<-c()

insert_vocab<-function(vocab_noun,nouns)
{
  for(i in 1:length(nouns))
  {
    
      vocab_noun<-c(vocab_noun,nouns[i])
    
  }
  
  return(vocab_noun)
}

for(i in 1:length(txt))
{
  s<-txt[i]
  tag.s<-tagPOS(s)
  print(tag.s)
  tagged.s<-as.vector(tag.s$POStagged)
  tag.s$POStags
  vec.tagged.split <- str_split(tag.s$POStagged, " ")
  all.noun <- grep("\\NN", vec.tagged.split[[1]], value = TRUE)
  nouns<-gsub( "\\.|/|\\-|\"|\\s||," , "" , str_replace(all.noun, "/NN.|/NN", ""))
  vocab_noun<-insert_vocab(vocab_noun,nouns)
  print(vocab_noun)
}



# POS tagging function
# tagPOS <-  function(x, ...) 
#   {
#   s <- as.String(x)
#   word_token_annotator <- Maxent_Word_Token_Annotator()
#   a2 <- Annotation(1L, "sentence", 1L, nchar(s))
#   a2 <- annotate(s, word_token_annotator, a2)
#   a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
#   a3w <- a3[a3$type == "word"]
#   POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
#   POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
#   list(POStagged = POStagged, POStags = POStags)
# }


tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

table(vocab_noun)

#n.freq<-as.data.frame(table(vocab_noun))
x<-tolower(vocab_noun)
View(as.data.frame(table(x)))
n.freq<-as.data.frame(table(x))
n.freq1<-n.freq[n.freq$Freq>50,]
View(n.freq1)

n.freq1<-n.freq1[-c(1,2),]
View(n.freq1)

vocab<-n.freq1$x
vocab

v_c<-as.character(vocab)
v_c

write.csv(v_c,"vector.csv",row.names = F)

train_matrix<-matrix(c(0),nrow=nrow(text_train_data),ncol=(length(v_c)+1))
colnames(train_matrix)<-c(v_c,"star_rate")

# insert_nn_rev<-function(rev_nn,n)
# {
#   rev_nn<-c()
#   rev_nn<-c(rev_nn,n)
#   return(rev_nn)
# }

rev_nn<-c()

#Counting Word frequencies for each review

for(i in 1:nrow(train_matrix))
{
  rev_txt_df<-text_train_data[i,]
  star<-text_train_data[i,]$stars
  rev_txt<-rev_txt_df$text
  print(rev_txt)
  #Getting nouns only
  rev_txt.pos<-tagPOS(rev_txt)
  #print(tag.s)
  
  rev_txt.tagged<-as.vector(rev_txt.pos$POStagged)
  #tag.s$POStags
  rev_txt.tagged
  rev_txt.tagged.split <- str_split(rev_txt.tagged, " ")
  all.noun <- grep("\\NN", rev_txt.tagged.split[[1]], value = TRUE)
  nouns<-gsub( "\\.|/|\\-|\"|\\s||," , "" , str_replace(all.noun, "/NN.|/NN", ""))
  print(nouns)
  rev_nn<-nouns
  #rev_nn<-insert_vocab(rev_nn,nouns)
  #print(rev_nn)
  #Frequency of imp words from rev_nn
  row<-i
  train_matrix<-dofreq(rev_nn,v_c,star,row)
  ##break;
  
}

#print(f_m)
print(train_matrix)

write.csv(train_matrix,"train_matrix.csv",row.names = FALSE)

dofreq<-function(rev_nn,v_c,star,row)
{
rownum<-which(v_c %in% rev_nn )
#print(rownum)

for(z in 1:length(rownum))
{
  #print(train_matrix[row,rownum[i]])
  train_matrix[row,rownum[z]]<-sum(rev_nn==v_c[rownum[z]])
  print(sum(rev_nn==v_c[rownum[z]]))
  #print(rownum)
  #print(train_matrix[row,rownum[i]])
  #break
}
train_matrix[row,ncol(train_matrix)]<-star
  
return(train_matrix)
}






