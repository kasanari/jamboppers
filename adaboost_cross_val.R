#Cross validation algorithm modified from lab code

songs <- read.csv('training_data.csv', header = T)

load('train_scaled.data')

#songs <- train.scaled

N.CV = 5

set.seed(666)

randomize.indices <- sample(nrow(songs), size = nrow(songs), replace = FALSE)

songs.randomized <- songs[randomize.indices,]

error = c()

#old_weights = nn_old


#f = label ~ danceability + energy + key+ mode + loudness +speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration +  time_signature 

# n <- names(train)
# learning_vars = n[!n %in% ("like")]
# string_learning = paste(learning_vars, collapse = " + ")
# formel = paste("like ~", string_learning)
# f <- as.formula(formel)

#K-fold cross-validation of AdaBoost:
for (i in 1:N.CV) {
    #Assigning indices to train and validate for current iteration 
    start.index = (i - 1) * ceiling(nrow(songs) / N.CV) + 1
    end.index = min(i * ceiling(nrow(songs) / N.CV), nrow(songs))

    validation.indices <- seq(from = start.index, to = end.index, by = 1)
    train <- songs.randomized[-validation.indices, ]
    validate <- songs.randomized[validation.indices, ]
    
    #Trains an AdaBoost model
    boosting.fit <- boosting(formula=label~., data=train)
    #Predicts like/dislike using AdaBoost.
    boosting.pred <- predict(boosting.fit, newdata=validate)
    
    ##  test error rates
    error[i] <- mean(validate$label != boosting.pred$class)
}


mean_error = mean(error)
mean_correct = 1-mean_error

#lowest = which.min(rowMeans(error.crossvalidation))

#pred.final <- knn(train = X, test = X, cl = y, k = lowest)

