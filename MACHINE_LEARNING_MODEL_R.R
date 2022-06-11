library(caret)
data_voice = read.csv('/home/mrigank/Public/WINTER_SEMESTER_2021_22/CSE3506_ESSENTIAL_OF_DATA_ANALYTICS/CSE3506_PROJECT/LPC_MFCC_FEATURES_EXTRACTED.csv',row.names=1)
str(data_voice)
data_voice$class = as.factor(data_voice$class)
levels(data_voice$class)

train_ind = createDataPartition(data_voice$class,p=0.75,list=FALSE)

train_data = data_voice[train_ind,]

test_data = data_voice[-train_ind,]

control_cv = trainControl(method="cv",number=10)
metric = "Accuracy"

### LDA
set.seed(120)
fit.lda = train(class~ .,data=data_voice,method="lda",metric=metric,trControl=control_cv)
predictions = predict(fit.lda,test_data)abuzeina2018employing
confusionMatrix(predictions,test_data$class)

##CART

set.seed(120)
fit.cart = train(class~ .,data=data_voice,method="rpart",metric=metric,trControl=control_cv)
predictions = predict(fit.cart,test_data)
confusionMatrix(predictions,test_data$class)

##KNN

set.seed(130)
fit.knn = train(class~ .,data=data_voice,method="knn",metric=metric,trControl=control_cv)
predictions = predict(fit.knn,test_data)
confusionMatrix(predictions,test_data$class)

## SVM LINEAR

set.seed(130)
fit.svmlinear = train(class~ .,data=data_voice,method="svmLinear",metric=metric,trControl=control_cv)
predictions = predict(fit.svmlinear,test_data)
confusionMatrix(predictions,test_data$class)

## SVM RADIAL

set.seed(130)
fit.svmradial= train(class~ .,data=data_voice,method="svmRadial",metric=metric,trControl=control_cv)
predictions = predict(fit.svmradial,test_data)
confusionMatrix(predictions,test_data$class)


### RANDOM FOREST 

set.seed(130)
fit.rf = train(class~ .,data=data_voice,method="rf",metric=metric,trControl=control_cv)
predictions = predict(fit.rf,test_data)
confusionMatrix(predictions,test_data$class)


results_model = resamples(list(lda=fit.lda,cart=fit.lda,knn=fit.knn,svm_linear=fit.svmlinear,svm_radial=fit.svmradial,rf=fit.rf))
summary(results_model)

dotplot(results_model)

