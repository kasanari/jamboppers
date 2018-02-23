log_reg = function(train, test, f) {
 
  log_reg.fit = glm(formula = f, data = train, family = "binomial")
  log_reg.probs = predict(object = log_reg.fit, newdata = test, type="response")
  
  limit = 0.5
  
  log_reg.pred <- rep("dislike", length(log_reg.probs))
  log_reg.pred[log_reg.probs > limit] <- "like"
  
  return(log_reg.pred)
}