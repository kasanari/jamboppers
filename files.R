# importFiles = function(path = ".") { 
#   songs.train <- read.csv(file.path(path, "training_data.csv"), header=T)
#   songs.test <- read.csv(file.path(path, "songs_to_classify.csv"), header=T)
# }

scaleData = function(data) {
  names = names(data)
  data.scaled = data
  for (name in names) {
    tryCatch({
      #Catch the error that pops up when it tries to scale a non-numeric column
      data.scaled[name] = scale(data[[name]])
    },
    error = function(e) {
      cat(name, "is not a numeric column and can not be scaled\n")
    })
  }
  return(data.scaled)
}

setQualitative = function(data, names){
  for (name in names) {
    data[name] = as.factor(data[[name]]) 
  }
  return(data)
}
