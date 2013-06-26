# Loading up the 10k train and 5k test sets (was unable to get my tm functions to process in a
# decent timeframe on the 50k+ training)
setwd("/Users/davidlogsdon/data_science/Homework_2/kaggle_salary/")
train <- read.csv("train.csv", as.is=F, stringsAsFactors= F, header=T)                          
test <- read.csv("test.csv", as.is=F, stringsAsFactors= F, header=T)                         

## commenting out as this was for earlier test/train model development.
#set.seed(756)
#full.data$fold <- sample(1:10, nrow(full.data), replace=TRUE) #add fold to dataset, will contain a random number from 1:10. 
# nfold - split set in to n pieces. train/test. repeat
#train <- subset(full.data, fold != 3) #90% train
#test  <- subset(full.data, fold == 3) #10% test
#train$fold = NULL
#test$fold = NULL


# this is a custom function. The idea was I wanted to be able to take any dataframe column
# and keep only the num most frequent terms, changing the others to '(Other)' to focus the model
# on the more frequent data
ranker <-function(x,num)
{
  col.counts <- summary(x)
  top.col <- names(col.counts[order(col.counts,decreasing=T)][1:num])
  factor(x,levels=top.col)
}

#Category field adjustments using ranker()

###   TRAIN and TEST
train$Category <- ranker(train$Category,10)
train$Category[is.na(train$Category)] <- "(Other)"
test$Category <- ranker(test$Category,10)
test$Category[is.na(test$Category)] <- "(Other)"


#title field adjustments using ranker()
library('tm')
###   TRAIN and TEST
train$Title <- ranker(train$Title,50)
train$Title[is.na(train$Title)] <- "(Other)"
test$Title <- ranker(test$Title,50)
test$Title[is.na(test$Title)] <- "(Other)"

### FULL DESCRIPTION modifications using text tools
###   TRAIN
doc <- data.frame(Text=train$FullDescription)
row.names(doc) <- 1:nrow(doc)
corpus <- Corpus(DataframeSource(doc))
corpus <- tm_map(corpus,removeWords, stopwords("english"))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
dtm <- DocumentTermMatrix(corpus)
freq_train <- as.array(findFreqTerms(dtm, lowfreq = 1250, highfreq = Inf))
train <- cbind (train, as.matrix(dtm[,freq_train]))
train$FullDescription <- NULL

###TEST
doc_test <- data.frame(Text=test$FullDescription)
row.names(doc_test) <- 1:nrow(doc_test)
corpus_test <- Corpus(DataframeSource(doc_test))
corpus_test <- tm_map(corpus_test,removeWords, stopwords("english"))
corpus_test <- tm_map(corpus_test, tolower)
corpus_test <- tm_map(corpus_test, removePunctuation)
dtm_test <- DocumentTermMatrix(corpus_test)
test <- cbind (test, as.matrix(dtm_test[,freq_train]))
test$FullDescription <- NULL

###LOCATION mods with ranker
train$LocationRaw <- NULL
test$LocationRaw <- NULL
train$ranked.location = ranker(train$LocationNormalized,100)
train$ranked.location[is.na(train$ranked.location)] <- "(Other)"
test$ranked.location = ranker(test$LocationNormalized,100)
test$ranked.location[is.na(test$ranked.location)] <- "(Other)"
train$LocationNormalized <- NULL
test$LocationNormalized <- NULL


###COMPANY
train$ranked.Company = ranker(train$Company,100)
train$ranked.Company[is.na(train$ranked.Company)] <- "(Other)"
test$ranked.Company = ranker(test$Company,100)
test$ranked.Company[is.na(test$ranked.Company)] <- "(Other)"
train$Company <- NULL
test$Company <- NULL

#just in case, fixers. With more optimization these wouldn't be necessary. Sort of a hack 
#to get an incomplete model working
test$ranked.location[!(test$ranked.location %in% train$ranked.location)] <- "(Other)" 
test$ranked.Company[!(test$ranked.Company %in% train$ranked.Company)] <- "(Other)"
test$Category[!(test$Category %in% train$Category)] <- "(Other)"
test$Title[!(test$Title %in% train$Title)] <- "(Other)"
test$SourceName <- NULL
train$SourceName <- NULL
#more last minute hacking...
test$Title[test$Title=="Marketing Manager"] <- "(Other)"
test$Title[test$Title=="Store Manager"] <- "(Other)"

# create the linear model
model <- lm(as.formula(paste(colnames(train)[7], "~",
                             paste(colnames(train)[c(2,3,4,5,8:60,80:211)], collapse = "+"),
                             sep = ""
)), data=train)

mae <- function(one, other) {
  return(mean(abs(one - other)))
} 

# use model to create predictions
test.predict <- predict(model, test)

submission <- data.frame(Id=test$Id,Salary=test.predict)
m <- mean(submission$Salary, na.rm=T)
submission$Salary[is.na(submission$Salary)] <- m
write.csv(submission, "my_submission.csv", row.names=FALSE)