source("log_reg.R")
source("discriminant.R")
source("random_forest.R")

rf_error = function(train, test, f) {
  answer = serendipityGrove(train, test, f)
  error <- mean(answer != test$label)
}

qda_error = function(train, test, f) {
  answer = quadda(train,test,f)
  error <- mean(answer != test$label)
}

lda_error = function(train, test, f) {
  answer = linearda(train,test,f)
  error <- mean(answer != test$label)
}

log_reg_error = function(train, test, f) {
  answer = log_reg(train, test, f)
  error <- mean(answer != test$label)
}

k_fold = function(data, f, method) {
  N.CV = 5
  
  set.seed(666)
  
  randomize.indices <- sample(nrow(data), size = nrow(data), replace = FALSE)
  
  data.randomized <- data[randomize.indices,]
  
  errors = c()
  weights_vector = c()
  
  for (i in 1:N.CV) {
    
    start.index = (i - 1) * ceiling(nrow(data) / N.CV) + 1
    end.index = min(i * ceiling(nrow(data) / N.CV), nrow(data))
    validation.indices <- seq(from = start.index, to = end.index, by = 1)
    
    train <- data.randomized[-validation.indices, ]
    validate <- data.randomized[validation.indices,] 
    
    errors[i] <- method(train, validate, f)
    
  }
  return(mean(errors))
}

load("train_scaled.data")

train = train.scaled

f = label~.

log_err = k_fold(train, f, method = log_reg_error)
lda_err = k_fold(train, f, method = lda_error)
rf_err = k_fold(train, f, method = rf_error)
f = label ~ danceability + energy + key+ mode + loudness +speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration
qda_err = k_fold(train, f, method = qda_error)
