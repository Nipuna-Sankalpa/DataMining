library(class)
knn.model <- knn(train[, 2:785], test, cl = train[,1], k = 50)
submit_knn <- data.frame(ImageId = seq(1,28000), Label = knn.model)
write.csv(submit_knn, file = "/home/yellowflash/semester_7/DM/Digit recognizer/submit_knn.csv", row.names=F)


#library(neuralnet)
#n <- names(seeds)
#f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))
#nn <- neuralnet(f,data=seeds,hidden=c(10),linear.output=FALSE)

library(randomForest)
rf <- randomForest(label~., data = train, mtry = 9, ntree = 500)
submit <- data.frame(ImageId = seq(1,nrow(test)),
                     Label = predict(rf, test, type = "class"))
write.csv(tempSubmit, file = "/home/yellowflash/semester_7/DM/Digit recognizer/submission-r-randomForest.csv", row.names=F)

