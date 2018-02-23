rm(list = ls()) #clears workspace

source("files.R")

filename = "Boosting_results.txt"
 
txt = "Start of file"
write(txt, file = filename) #Empty results file

#BOOSTING
#installera adabag innan ni kör.
library(adabag)

load("train_scaled.data")
load("classify_scaled.data")

songs.train  = train.scaled
songs.classify = classify.scaled

f = label ~ danceability + energy + key+ mode + loudness +speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration +  time_signature 

boost.fit <- boosting(formula=f,data=songs.train)
boost.pred <- predict(boost.fit, newdata=songs.classify)
 
write_to_file(filename,"B-b-b-boosting:", boost.pred$class)

txt = "End of file"
write(txt, file = filename, append = TRUE)

