# final project 
# Loading all the required packages. 
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

# setting a seed 
set.seed(25314)

# Loading the data
# Download the trainigna dn testing dataset and store them in your current directory 

training = read.csv("pml-training.csv", header=T, na.strings=c("NA","#DIV/0!","")))
testing = read.csv("pml-testing.csv", header=T, na.strings=c("NA","#DIV/0!","")))

# Sub Splitting the Training data into training and testing in 70% and 30% each resply

partTrain = createDataPartition(y=training$classe, p=0.7, list=FALSE)
myTraining = training[partTrain, ]
myTesting = training[-partTrain, ]
dim(myTraining) 
dim(myTesting)


### Cleaning the data 

## Cleaning Step - I 
# first cleaning all the near zero or no value variables in the data sets
# This was done by checking all the variables which have no value and 
# removed using the below code. 

NearZeroVar = names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt", 
"kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
 "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm", "stddev_yaw_forearm", 
"var_yaw_forearm")
myTraining = myTraining[!NearZeroVar]

#To check the new dimensions of observations
dim(myTraining)

## Cleaning Step - II
# removing the first column that contains the person id
myTraining = myTraining[c(-1)]

## Cleaning Step -III
# Cleaning Variables with too many NA values. 
# I have considered a threshold of 50% of NA values to remove that variable.  

trainingTemp = myTraining #creating another subset to iterate in loop
for(i in 1:length(myTraining)) 
  { 
  #for every column in the training dataset
  if( sum( is.na( myTraining[, i] ) )/nrow(myTraining) > .5 ) 
    { 
    #if no. of NA values > 50% of total observations
    for(j in 1:length(trainingTemp)) 
      {
      if( length( grep(names(myTraining[i]), names(trainingTemp)[j]) ) ==1)  
        { 
        #if the columns are the same:
        trainingTemp = trainingTemp[ , -j] 
        #Remove that column
      }   
    } 
  }
}
#To check the new dimension of our observations
dim(trainingTemp)

#Seting back to our set:
myTraining = trainingTemp
rm(trainingTemp)


## Cleaning the Testing data sets. 
# Here we have to perform all the above cleaning steps on both the myTesting and testing dataset. 
# But for testing set we have to also remove the Classe column which is empty(No values assigned)

clean1 = colnames(myTraining)
clean2 = colnames(myTraining[, -58]) # classe column  = 58 is removed
myTesting = myTesting[clean1]
testing = testing[clean2]

#To check the new dimension of observations
dim(myTesting)

#To check the new dimension of observations
dim(testing)

### Machinelearning part 


## Using decision trees for modeling
modelFitTrees = rpart(classe ~ ., data=myTraining, method="class")

# to view the decision tree with fancy run this command
fancyRpartPlot(modelFitTrees)

## Predicting with decision trees on myTesting

predictTrees = predict(modelFitTrees, myTesting, type = "class")

## Generating confusion Matrix to myTesting results:
confusionMatrix(predictTrees, myTesting$classe)


##  Using Random Forests for modeling

modelFitRF = randomForest(classe ~. , data=myTraining)

## Predicting:

predictRF = predict(modelFitRF, myTesting, type = "class")

# Using confusion Matrix to test results:
confusionMatrix(predictRF, myTesting$classe)
 

###Generating Files to submit as answers for the Assignment:

##Finally, using the provided Test Set:
#For Decision Tree would be like this, but not going to use it:
#predictionsA2 = predict(modFitA1, testing, type = "class")

#For Random Forests is:
predictRF = predict(modelFitRF, testing, type = "class")

#Function to generate files with predictions to submit for assignment:

pml_write_files = function(x){ 
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictRF)
