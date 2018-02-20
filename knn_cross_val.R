#Cross validation algorithm modified from lab code

knn_cross_val = function (data, vars, label) {

kNN_vars = vars

X = as.matrix(data[kNN_vars])
y = as.matrix(data[label])

N.K = 30
N.CV = 10

randomize.indices <- sample(nrow(data), size = nrow(data), replace = FALSE)
X.randomized <- X[randomize.indices, ]
y.randomized <- y[randomize.indices]
error.crossvalidation <- matrix(0, N.K, N.CV)
for (i in 1:N.CV)
{
  start.index = (i - 1) * ceiling(nrow(data) / N.CV) + 1
  end.index = min(i * ceiling(nrow(data) / N.CV), nrow(data))
  validation.indices <- seq(from = start.index, to = end.index, by = 1)
  X.training <- X.randomized[-validation.indices, ]
  y.training <- y.randomized[-validation.indices]
  X.validation <- X.randomized[validation.indices, ]
  y.validation <- y.randomized[validation.indices]
  
  for (kt in 1:N.K)
  {
    pred <- knn(
      train = X.training,
      test = X.validation,
      cl = y.training,
      k = kt
    )
    error.crossvalidation[kt, i] <- 1 - mean(pred == y.validation)
  }
}

lowest = which.min(rowMeans(error.crossvalidation))

#pred.final <- knn(train = X, test = X, cl = y, k = lowest)

return(lowest)

}