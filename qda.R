qda.fit <- qda(formula = f, data = songs.train)
qda.testdata <- predict(object = qda.fit, newdata = songs.test)
qda.pred <- qda.testdata$label

table(songs.test$label, qda.pred)
print(mean(qda.pred == songs.test$label))