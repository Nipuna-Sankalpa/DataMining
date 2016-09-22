options( java.parameters = "-Xmx12g" )

#import libraries
library(data.table)
library(FSelector)
library(plotly)
library(ggplot2)
library(h2o)


#import data set
train<-fread(input = '/home/yellowflash/semester_7/DM/Digit recognizer/train.csv',showProgress = TRUE)
train<-as.data.frame(train)
trainLabel<-train$label
trainData<-subset(train,select = c(2:length(train)))
trainLabel<-as.data.frame(trainLabel)

test<-fread(input = '/home/yellowflash/semester_7/DM/Digit recognizer/test.csv',showProgress = TRUE)
test<-as.data.frame(test)

#plot images of digits
imageMatrix<-matrix(unlist(test[27999,]),ncol = 28,byrow = TRUE)
rotate <- function(x) t(apply(x, 2, rev))
image((imageMatrix),col=grey.colors(255))


#information gain
infoGain<-information.gain(label~.,train)
print(infoGain)

# info gain plot
p <- plot_ly(data = infoFrame, x = label, y = importance , name = "Information Gain",type = "bar")

firstImage<-matrix(ncol=28,nrow= 28)

#remove columns which has zero values
for(index in 1:length(trainData)){
  trainData[,index]<-as.numeric(trainData[,index])
}

#use NN to predict values
localH2o<-h2o.init(max_mem_size = '6g',nthreads = -1)
train$label<-as.factor(train$label)
train_h2o<-as.h2o(train)
test_h2o <-as.h2o(test)

model =
  h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train_h2o, # data in H2O format
                   activation = "RectifierWithDropout", # algorithm
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(100,100), # two layers of 100 nodes
                   momentum_stable = 0.99,
                   nesterov_accelerated_gradient = T, # use it for speed
                   epochs = 200) # no. of epochs

h2o_y_test <- h2o.predict(model, test_h2o)
df_y_test = as.data.frame(h2o_y_test)

write.csv(df_y_test$predict, file = "/home/yellowflash/semester_7/DM/Digit recognizer/submission-r-h2o.csv", row.names=T)

## shut down virutal H2O cluster
h2o.shutdown(prompt = F)


pca=h2o.prcomp(training_frame = train_h2o,k=709,transform = "STANDARDIZE",seed = 34234)

