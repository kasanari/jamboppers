rm(list = ls()) #clears workspace

source("files.R")
source("log_reg.R")
source("discriminant.R")
source("makeWebsiteString.R")
source("knn_cross_val.R")
install.packages("randomForest") # Vet inte om det ska finnas n?gra package installs h?r/var? Vet inte om install.packages("rpart") beh?vs ox?? /2Lunch
source("random_forest.R")

songs.train <- read.csv('training_data.csv', header = T)
songs.classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = list("label", "key", "mode", "time_signature")
songs.train = setQualitative(songs.train, qualitative_vars)
songs.classify = setQualitative(songs.classify, qualitative_vars)

filename = "results.txt"

txt = "Start of file"
write(txt, file = filename) #Empty results file

##Logistic Regression
f = label ~ key + danceability + energy + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + time_signature
preds = log_reg(songs.train, songs.classify, f)

labels = preds[[1]]
answers = preds[[2]]

txt = makeWebsiteString(labels)
txt = paste("Logistic regression:", txt)
write(txt, file = filename, append = TRUE)

## Quadratic Discriminant
f = label ~  danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence 
preds = quadda(songs.train, songs.classify, f)

txt <- makeWebsiteString(preds)
txt = paste("Quadratic Discriminant:", txt)
write(txt, file=filename, append = TRUE)


## KNN
library(class)
kNN_vars = c(
  "key",
  "danceability",
  "energy",
  "loudness",
  "mode",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "liveness",
  "valence",
  "time_signature"
)

x = as.matrix(songs.train[kNN_vars])
y = as.matrix(songs.train["label"])
z = as.matrix(songs.classify[kNN_vars])
#knn.pred = knn_cross_val(data =songs.train, vars = kNN_vars, label = "label")

n.k = knn_cross_val(songs.train, kNN_vars, "label") #From cross validation

knn.pred <- knn(train = x, test = z, cl = y, k = n.k) #Do kNN prediction

write_to_file(filename,"knn:", knn.pred)


## Random Forest

## For simple testing
# set.seed(1)
# training_indices <- sample(nrow(songs.train), size = 400, replace = FALSE)
# songs.training <- songs.train[training_indices,]
# songs.testing <- songs.train[-training_indices,]
##

## This because of the/a bug in randomForest; other methods trying to assimilate levels didn't work.
# levels(songs.classify$label) <- levels(songs.train$label) # Tried fixing randomForest bug, didn't work. Looked into another method, didn't work either.
songs.classify <- rbind(songs.train[1, ] , songs.classify)
songs.classify <- songs.classify[-1,]
# Maybe nicer to put them together permanently instead. But believe still nicer but more knowledgable solution exist.

f = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + time_signature # Duration removed - because problematic (?). But tempo stays?? Probably best removed: Definitely time_signature. Probably key and mode as well.
rf.pred <- serendipityGrove(songs.train, songs.classify, f)

## For simple testing
# rf.pred <- serendipityGrove(songs.training, songs.testing, f)
# rf.testing.error <- mean(songs.testing$label != rf.pred$aggregate) # Test error rate.
# rf.testing.error
##

write_to_file(filename, "Random Forest:", rf.pred)







txt = "End of file"
write(txt, file = filename, append = TRUE)