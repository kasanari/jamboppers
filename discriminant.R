linearda  = function(train, test, f) {
  #Uses function lda from library(MASS) to learn a 
  #linear discriminant analysis model using data "train" and "test"
  #which have labels contained in f.
  lda.fit <- lda(formula = f, data = train)
  #Uses the model learned in lda.fit to predict class ("like" or "dislike")
  #of testdata
  lda.testdata <- predict(object = lda.fit, newdata = test)
  #Places the "class" list from the prediction into a return variable
  lda.pred <- lda.testdata$class
  return(lda.pred)
}  
quadda  = function(train, test, f) {  
  qda.fit <- qda(formula = f, data = train)
  #Uses the model learned in lda.fit to predict class ("like" or "dislike")
  #of testdata
  qda.testdata <- predict(object = qda.fit, newdata = test)
  #Places the "class" list from the prediction into a return variable
  qda.pred <- qda.testdata$class
  
  return(qda.pred)
}