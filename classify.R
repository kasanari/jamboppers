rm(list = ls()) #clears workspace

source("files.R")
source("log_reg.R")
source("discriminant.R")
source("makeWebsiteString.R")
source("knn_cross_val.R")

write_to_file = function(name, title, predicts){
  txt <- makeWebsiteString(predicts)
  txt = paste(title, txt)
  write(txt, file=name, append = TRUE)
}

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

txt = "End of file"
write(txt, file = filename, append = TRUE) 


