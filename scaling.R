##Methods for scaling the data

classify = function(data) {
  # Encode as a one hot vector multilabel data
  songs.class <- cbind(data[1:10], 
                       class.ind(as.factor(data$key)), 
                       class.ind(as.factor(data$time_signature)), 
                       class.ind(as.factor(data$mode)),
                       class.ind(as.factor(data$label)))
  keys = c("C", "C_major", "D_minor", "D", 
           "D_major", "E_minor", "E", "E_major", 
           "F_minor", "F", "F_major", "G_minor")
  # Set labels name
  songs.class = cbind(songs.class[,1:28], songs.class[,30])
  names(songs.class) <- c(names(data)[1:10], keys, "onefourths", "threefourths", "fourfourths", 
                          "fivefourths", "minor", "major", "like")
  return(songs.class)
}

train <- read.csv('training_data.csv', header = T)
classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = c("key", "time_signature", "mode","label")
train = setQualitative(train, qualitative_vars)
classify = setQualitative(classify, qualitative_vars)


total = rbind(train, classify)

data_to_scale = total[, !(names(total) %in% qualitative_vars)]
maxs <- apply(data_to_scale, 2, max)
mins <- apply(data_to_scale, 2, min)

scaled <- as.data.frame(scale(data_to_scale, center = mins, scale = maxs - mins))

for (var in qualitative_vars) {
  scaled[[var]] = total[[var]]
}

classified = classify(scaled)

final_train = classified[1:500, ]
final_classify = classified[501:600, ]

save(final_train, file="train_scaled_classed")
save(final_train, file="classify_scaled_classed")
save(scaled[1:500, ], file="train_scaled")
save(scaled[501:600,], file="classify_scaled")




