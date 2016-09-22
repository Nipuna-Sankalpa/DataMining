library("e1071")

train<-fread(input = '/home/yellowflash/semester_7/DM/Digit recognizer/train.csv',showProgress = TRUE)

train<-as.data.frame(train)
trainLabel<-train$label
trainData<-subset(train,select = c(2:length(train)))
trainLabel<-as.data.frame(trainLabel)

svm_model<-svm(label~.,data = train)

svm_tune <- tune(svm, train.x=trainData, train.y=trainLabel, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

svm_model_after_tune <- svm(label ~ ., data=train, kernel="radial", cost=1, gamma=0.5)

pred <- predict(svm_model_after_tune,test)

