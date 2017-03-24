library(caret)
library(corrplot)
library(plyr)
library(PerformanceAnalytics)
library(mclust)
library(pander)
library(captioner)
library(data.table)
library(xlsx)
library(e1071)
library(ggplot2)
library(kernlab)
library(class)
library(nnet)
library(pROC)
library(MASS)
library(ROCR)
library(neuralnet)
#Load Forest Cover Type Data Set
#forestCoverType <- read.csv("covtype.csv", sep = ",")
forestCoverType <- data.table::fread("covtype.csv",header=TRUE)

#Looking at the Dataset, the last column corresponds to the cover type with labels from 1 to 7.
#There are 4 wilderness areas represented by binary vectors. It is a good idea to combine these vectors and create original labels for each wilderness area.
#Also there exist 40 different soil types which are represented by binary vectors too.
#The first 10 columns consist of numerical data.

#The 4 different wilderness areas can be represented by numbers from 1 to 4, each corresponding to each wilderness area.
#Rawah = 1, Neota = 2, Comanche_Peak = 3, Cache_la_Poudre = 4
w_area_num <- c(1, 2, 3, 4)
w_area_names <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")

for(i in 1:length(w_area_num)) {
  wilderness_area <- w_area_names[i]
  winderness_area_number <- w_area_num[i]
  forestCoverType <- forestCoverType[get(wilderness_area)==1,wildernessArea:=winderness_area_number]
}

#Do the same for the 40 soil types

soil_type_num <- c(1:40)
soil_type_names <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8",
             "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16",
             "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
             "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
             "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

for(j in 1:length(soil_type_num)) {
  soil_type <- soil_type_names[j]
  soil_type_ref <- soil_type_num[j]
  forestCoverType <- forestCoverType[get(soil_type)==1,soilType:=soil_type_ref]
}


#Remove the binary columns of Soil Types and Winderness Areas
forestCoverType <- forestCoverType[ , colnames(forestCoverType[,11:54,with=FALSE]):=NULL]
forestCoverType <- as.data.frame(forestCoverType)

#See the distribution of each cover type
hist(forestCoverType$Cover_Type)
#We observe that the classes are way imbalanced and class 1 and 2 are dominating.

hist(forestCoverType$wildernessArea)
#The most cover types exist at Rawah and Comanche_Peak wilderness areas, whereas much fewer belongs to Neota and Cache_la_Poudre.

#Save the Class Labels to a vector, and then remove the Class columns from the dataset.
forestCoverType_class <- forestCoverType[, grep("Cover_Type", colnames(forestCoverType))]
forestCoverType_class <- as.matrix(forestCoverType_class)
drop <- c("Cover_Type")
forestCoverType <- forestCoverType[, !(names(forestCoverType) %in% drop)]

#Because of the very big dataset, we select 2000 observations of each Cover_Type
observations = 2000
num_of_classes <- 7
coverType_subset <- matrix(nrow = observations*7, ncol = length(forestCoverType))
coverType_subset <- as.data.frame(coverType_subset)
coverType_labels <- matrix(nrow = observations*num_of_classes)
c_names <- colnames(forestCoverType)
colnames(coverType_subset) <- c_names
#colnames(coverType_labels) <- colnames(forestCoverType_class)
count <- 0

for (k in 1:num_of_classes) {
  s <- subset(forestCoverType, forestCoverType_class == k)
  s_labels <- subset(forestCoverType_class, forestCoverType_class == k)
  s <- s[sample(nrow(s), observations), ]
  s_labels <- s_labels[sample(nrow(s_labels), observations), ]
  s_labels <- as.matrix(s_labels)
  count <- count + length(s[, 1])
  coverType_subset <- rbind(coverType_subset, s)
  coverType_labels <- rbind(coverType_labels, s_labels)
  
}
coverType_subset <- coverType_subset[-c(1:count), ]
coverType_labels <- coverType_labels[-c(1:count), ]
coverType_labels <- as.matrix(coverType_labels)
row.names(coverType_subset) <- c(1:count)
row.names(coverType_labels) <- c(1:count)

write.csv(coverType_subset, file = "coverType_subset.csv")
write.csv(coverType_labels, file = "labels.csv")

#Correlation matrix
summary(coverType_subset)
CorrMatrix <- cor(coverType_subset)
corrplot(CorrMatrix, method = "number", main="Correlation between variables", tl.cex = 0.7, type = "upper")
#High correlation between Elevation and Soiltype at 0.83. Also there exists a correlation between Horizontal_Distance_To_Hydrology
#and Vertical_Distance_To_Hydrology at 0.65 and a high negative of -.78 between Hillshade9am and Hillshade 3pm.

hist(coverType_subset$wildernessArea)
#Now in the selected balanced subset we observe that the cover types in Neota area are still very few comparing to the other 3 areas and the cover types
#in Cache_la_Poudre are are increased.

#The most cover types exist at Rawah and Comanche_Peak wilderness areas, whereas much fewer belongs to Neota and Cache_la_Poudre.

#Density of each variable
par(mfrow=c(4,3))
par(mar = rep(2, 4))
colnames <- dimnames(coverType_subset)[[2]]
for (i in 1:length(colnames)) {
  d <- density(coverType_subset[,i])
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}


#Feature Selection

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
coverType_subset <- range01(coverType_subset)
y = as.factor(coverType_labels)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
model <- rfe(coverType_subset, y, rfeControl=control)
important_var <- predictors(model)
important_var
plot(model, type = c("g", "o"))
#By choosing the 8 more important variables the model gets the best accuracy.
coverType_subset <- coverType_subset[, (names(coverType_subset) %in% important_var)]

#Split data in  train and test set. 70% train set, 30% test set.
train_index <- createDataPartition(coverType_labels, p = 2/3, list = FALSE)
train_data <-coverType_subset[train_index, ]
train_data_labels <- coverType_labels[train_index, ]
row.names(train_data) <- c(1:length(train_data[, 1]))
test_data <- coverType_subset[-train_index, ]
test_data_labels <- coverType_labels[-train_index, ]
row.names(test_data) <- c(1:length(test_data[, 1]))

y_train <- as.factor(train_data_labels)
y_test <- as.factor(test_data_labels)
y <- as.factor(coverType_labels)
C <- c(2^(-1:2))
g <- c(2^(-2:1))

#----------------------------------------------------------------
#SVM
#----------------------------------------------------------------

#SVM using Linear kernel
#Tune the parameters using 5-fold cross validation
tc <- tune.control(cross = 5)
tuning_svm_linear <- tune.svm(train_data, y_train, kernel = "linear", cost = C, tunecontrol = tc)
print(tuning_svm_linear)
#Apply the best parameters to train_data
svm_model_linear <- svm(train_data, y_train, kernel = "linear", cost = tuning_svm_linear$best.parameters[1,1])
#Use test_data to predict cover types
svm_linear_pred <- predict(svm_model_linear, test_data)
#confusion matrix
confusionMatrix(svm_linear_pred, y_test)

#SVM using RBF kernel
tuning_svm_rbf <- tune.svm(train_data, y_train, kernel = "radial", gamma = g, cost = C, tunecontrol = tc)
print(tuning_svm_rbf)

svm_model_rbf <- svm(train_data, y_train, kernel = "radial", gamma = tuning_svm_rbf$best.parameters[1,1], cost = tuning_svm_rbf$best.parameters[1,2])
svm_rbf_pred <- predict(svm_model_rbf, test_data)
confusionMatrix(svm_rbf_pred, y_test)




#----------------------------------------------------------------
#KNN
#----------------------------------------------------------------
ctrl <- trainControl(method="repeatedcv", repeats = 3)  
knnFit <- train(train_data, y_train, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"))
bestTune <- knnFit$bestTune
bestTune #k

knn.pred <- knn(train_data,test_data, y_train ,k = bestTune)
confusionMatrix(knn.pred, y_test)

#-----------------------------------------------------------------------------#
#Naive Bayes
#-----------------------------------------------------------------------------#

naive_bayes_model <- train(train_data, y_train, 'nb', trControl = trainControl(method = 'cv', number = 10))
naive_bayes_model
nb_pred <- predict(naive_bayes_model$finalModel, test_data)
confusionMatrix(nb_pred$class, y_test)

