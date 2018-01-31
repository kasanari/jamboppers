importFiles = function(path = ".") {
  songs.train <- read.csv(file.path(path, "training_data.csv"), header=T)
  songs.test <- read.csv(file.path(path, "songs_to_classify.csv"), header=T)
}