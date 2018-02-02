log_reg = function(train, test, f) {
  #f = label ~ key + danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + time_signature    
  
  log_reg.fit = glm(formula = f, data = train, family = "binomial")
  log_reg.probs = predict(object = log_reg.fit, newdata = test, type="response")
  
  limit = 0.4
  
  log_reg.pred <- rep("dislike", length(log_reg.probs))
  log_reg.pred[log_reg.probs > limit] <- "like"
  
  
  log_reg.pred.answer <- makeWebsiteString(log_reg.pred)
  
  # log_reg.pred.answer <- rep(0, length(log_reg.probs))
  # log_reg.pred.answer[log_reg.probs > limit] <- 1
  
  return(list(log_reg.pred, log_reg.pred.answer))
}