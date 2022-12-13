setwd('C:/Users/johan/Desktop/Masters/MSA540/Final Paper/archive')
heart_df = read.csv("heart.csv")

install.packages("aod")
library("aod")
install.packages("ggplot2")
library("ggplot2")
install.packages("MASS")
library("MASS")
install.packages("caret")
library("caret")
install.packages("e1071")
library("e1071")
install.packages("ROSE")
library("ROSE")
install.packages("tidyverse")
library("tidyverse")
install.packages("rpart")
library("rpart")
install.packages("rpart.plot") 
library(rpart.plot)
install.packages("readxl")                                       
library("readxl")
install.packages("randomForest")
library(randomForest)
install.packages("datasets")
library(datasets)
install.packages("cluster")
library(cluster)

sapply(heart_df, sd)

xtabs(~output + age, data = heart_df)

heart_df = na.omit(heart_df)
View(heart_df)

heart_df$output = as.factor(heart_df$output)

scaled_df = scale(heart_df[, -14], center = T, scale = T)

scaled_df = data.frame(scaled_df, "output" = heart_df$output)

View(scaled_df)

index = sort(sample(nrow(scaled_df), nrow(scaled_df) * 0.4))

train = scaled_df[index,]
valid = scaled_df[-index,]
## Method for undersampling

table(train$output)
undersampling<-ovun.sample(output~., data=train, method='under', N = 110)$data

### Method for oversampling

table(train$output)
oversamplingos<-ovun.sample(output~., data=train, method='over', N =134)$data

# Logistic Regression

mylogit <- glm(output~., data = train, family = binomial)
summary(mylogit)

str(scaled_df)

pre = predict(mylogit, newdata = valid, type = 'response')
View(pre)

LogisticResponse1 = ifelse(pre >= 0.5, '1','0')

df = data.frame("Actual" = valid$output,
                 "Predicted" = LogisticResponse1)
View(df)

tb = table("Actual" = df$Actual,
            "Predicted" = df$Predicted)
tb

prob = round(prop.table(tb) * 100, 2)
prob

acc = 35.16 + 48.90
acc

psuedo1 = 1 - (mylogit$deviance/mylogit$null.deviance)
psuedo1

df$Actual = as.factor(df$Actual)
df$Predicted = as.factor(df$Predicted)

confusionMatrix(df$Predicted, df$Actual, mode = "everything")

## Logistic Regression- Undersample
under_log<- glm(output~.,data=undersampling, family='binomial')
summary(under_log)

##Predicting results
predict_under<-predict(under_log, newdata= valid, type='response')
predict_under

##Converting them to alive and dead 
predict_under_response<-ifelse(predict_under>=0.5, '1','0')

##Dataframe
dataframe_under<- data.frame('Actual'=valid$output,
                             'Predicted'= predict_under_response)

View(dataframe_under)

##Creation of confusion matrix
table_under<-table('Actual'=dataframe_under$Actual,
                   'Predicted'= dataframe_under$Predicted)
table_under

##Creation of probability table
prop_table_under<-prop.table(table_under)
round(prop_table_under*100,2)
acc_under<-45.60+34.62
acc_under
## 80.22% Accuracy for Undersampling
##Psuedo r squared
psuedo_under<-1-(under_log$deviance/under_log$null.deviance)
psuedo_under

###Logistic Regression Oversampling

over_logisticos<- glm(output~.,data=oversamplingos, family='binomial')
summary(over_logisticos)

###Predicting results
predict_overos<-predict(over_logisticos, newdata= valid, type='response')
predict_overos

###Converting them to alive and dead
predict_over_responseos<-ifelse(predict_overos>=0.5, '1','0')

###Dataframe
dataframe_overos<- data.frame('Actual'=valid$output,
                            'Predicted'= predict_over_responseos)

View(dataframe_over)

###Creation of confusion matrix
table_overos<-table('Actual'=dataframe_overos$Actual,
                  'Predicted'= dataframe_overos$Predicted)
table_overos

#Creation of probability table
prop_table_overos<-prop.table(table_overos)
round(prop_table_overos*100,2)

acc_overos<-12.64 + 5.49
acc_overos

# Classification Trees
model = rpart(output~., data = train, method = 'class')

rpart.plot(model)

predict_test = predict(model, valid, type = "class")

df2 = data.frame("Actual" = valid$output,
                "Predicted" = predict_test)
View(df2)

tb2 = table("Actual" = df2$Actual,
           "Predicted" = df2$Predicted)
tb2

prob2 = round(prop.table(tb2) * 100, 2)
prob2

acc2 = 29.67 + 47.25
acc2

confusion_matrix = table(valid$output, predict_test)
confusion_matrix

##Classification- Undersample
modunder = rpart(output~., data = undersampling, method = 'class')

rpart.plot(modunder)

predict_classunder = predict(modunder, valid, type = "class")

## Dataframe 
under_class_df = data.frame("Actual" = valid$output,
                            "Predicted" = predict_classunder)
View(predict_classunder)

tb_class_under = table("Actual" = under_class_df$Actual,
                 "Predicted" = under_class_df$Predicted)
tb_class_under

prob_class_under = round(prop.table(tb_class_under) * 100, 2)
prob_class_under

accuracy_under_class = 31.87+39.56
accuracy_under_class

confusion_matrix = table(valid$output, predict_classunder)
confusion_matrix

### Classification Oversample

modelos = rpart(output~., data = oversampling, method = 'class')

rpart.plot(modelos)

predict_testos = predict(modelos, valid, type = "class")

dfos = data.frame("Actual" = valid$output,
                 "Predicted" = predict_testos)
View(dfos)

tbos = table("Actual" = df2$Actual,
            "Predicted" = df2$Predicted)
tbos

probos = round(prop.table(tb2) * 100, 2)
probos

accos = 28.57 + 50.00
accos

confusion_matrixos = table(valid$output, predict_testos)
confusion_matrixos


# Random Forests
rf = randomForest(output~., data = train, proximity=TRUE) 
print(rf)

p1 = predict(rf, train)
confusionMatrix(p1, train$output)

## Random Forest- Undersample
rf_under = randomForest(output~., data = undersampling, proximity=TRUE)
print(rf_under)

predict_rf_under = predict(rf_under, undersampling)
confusionMatrix(predict_rf_under, undersampling$output)

### Random Forest Oversample

rfos = randomForest(output~., data = oversampling, proximity=TRUE) 
print(rfos)

p1os = predict(rfos, oversampling)
confusionMatrix(p1os, oversampling$output)

# Clustering
d_dist = daisy(scaled_df, metric = "gower")
# hierarchical clustering
hc = hclust(d_dist, method = "complete")
# dendrogram 
plot(hc, labels=FALSE)
rect.hclust(hc, k=8, border="red")
# choose k, number of clusters 
cluster = cutree(hc, k=8)


## Clustering- Undersampling 
d_dist_under = daisy(undersampling, metric = "gower")
## hierarchical clustering 
hc_undersampled = hclust(d_dist_under, method = "complete")
## dendrogram Undersampling 
plot(hc_undersampled, labels=FALSE)
rect.hclust(hc_undersampled, k=8, border="red")
## choose k, number of clusters 
cluster = cutree(hc_undersampled, k= 8)

### Clustering Oversample

### hierarchical clustering
d_distos = daisy(oversamplingos, metric = "oversampling")

hcos = hclust(d_distos, method = "complete")

### dendrogram 
plot(hcos, labels=FALSE)
rect.hclust(hc, k=8, border="red")
### choose k, number of clusters 
cluster = cutree(hc, k=8)

# PCA
pc1 = prcomp(scaled_df[,-14])
summary(pc1) 

pca_scaled_df = scaled_df[, -c(12, 13)]

index2 = sort(sample(nrow(pca_scaled_df), nrow(pca_scaled_df) * 0.4))

train2 = scaled_df[index2,]
valid2 = scaled_df[-index2,]

## Method for Undersampling PCA
table(train2$output)
under_pca<-ovun.sample(output~., data=train2, method='under', N = 114)$data

### Method for Oversampling PCA

oversamplingos_pca<-ovun.sample(output~., data=train2, method='over', N =134)$data

# Logistic PCA
mylogit2 <- glm(output~., data = train2, family = binomial)
summary(mylogit2)

pre3 = predict(mylogit2, newdata = valid2, type = 'response')
View(pre3)

LogisticResponse2 = ifelse(pre3 >= 0.5, '1','0')

df3 = data.frame("Actual" = valid2$output,
                "Predicted" = LogisticResponse2)
View(df3)

tb3 = table("Actual" = df3$Actual,
           "Predicted" = df3$Predicted)
tb3

prob3 = round(prop.table(tb3) * 100, 2)
prob3

acc3 = 34.07 + 46.70
acc3

psuedo2 = 1 - (mylogit2$deviance/mylogit2$null.deviance)
psuedo2

## Logisitc PCA- Undersample
under_log_pca<- glm(output~.,data=under_pca, family='binomial')
summary(under_log_pca)

##Predicting results
predict_under_pca<-predict(under_log_pca, newdata= valid2 ,type='response')
predict_under_pca

##Converting them to alive and dead
predict_under_response_pca<-ifelse(predict_under>=0.5, '1','0')

##Dataframe
dataframe_under_pca<- data.frame('Actual'=valid2$output,
                             'Predicted'= predict_under_response_pca)

View(dataframe_under_pca)

##Creation of confusion matrix
table_under_pca<-table('Actual'=dataframe_under_pca$Actual,
                   'Predicted'= dataframe_under_pca$Predicted)
table_under_pca

##Creation of probability table
prop_table_under_pca<-prop.table(table_under_pca)
round(prop_table_under_pca*100,2)
acc_under_pca<-44.51+35.16
acc_under_pca

### Logistic Regression Oversample PCA

mylogit2os_pca <- glm(output~., data = oversamplingos_pca, family = binomial)
summary(mylogit2os)

pre3os_pca = predict(mylogit2, newdata = valid2, type = 'response')
View(pre3os_pca)

LogisticResponse2os_pca = ifelse(pre3os_pca >= 0.5, '1','0')

df3os_pca = data.frame("Actual" = valid2$output,
                 "Predicted" = LogisticResponse2os_pca)
View(df3os_pca)

tb3os_pca= table("Actual" = df3os_pca$Actual,
            "Predicted" = df3os_pca$Predicted)
tb3os_pca

prob3os_pca = round(prop.table(tb3os_pca) * 100, 2)
prob3os_pca

acc3os_pca = 32.97 + 47.25
acc3os_pca

psuedo2os_pca = 1 - (mylogit2os_pca$deviance/mylogit2os_pca$null.deviance)
psuedo2os_pca


# Classification PCA
model2 = rpart(output~., data = train2, method = 'class')

rpart.plot(model2)

predict_test2 = predict(model, valid2, type = "class")

df4 = data.frame("Actual" = valid2$output,
                 "Predicted" = predict_test2)
View(df4)

tb4 = table("Actual" = df4$Actual,
            "Predicted" = df4$Predicted)
tb4

prob4 = round(prop.table(tb4) * 100, 2)
prob4

acc4 = 31.32 + 48.35
acc4

confusion_matrix = table(valid2$output, predict_test)
confusion_matrix

## Classification PCA -  Undersample 
modunder_pca = rpart(output~., data = under_pca, method = 'class')

rpart.plot(modunder_pca)

predict_classunder_pca = predict(modunder_pca, valid2, type = "class")
## Dataframe 
under_class_df_pca = data.frame("Actual" = valid2$output,
                            "Predicted" = predict_classunder_pca)
View(predict_classunder_pca)

tb_class_pca = table("Actual" = under_class_df_pca$Actual,
                 "Predicted" = under_class_df_pca$Predicted)
tb_class_pca

prob_class_under_pca = round(prop.table(tb_class_pca) * 100, 2)
prob_class_under_pca

accuracy_under_class_pca = 42.86+36.81
accuracy_under_class_pca

confusion_matrix = table(valid2$output, predict_classunder_pca)
confusion_matrix

### Classification PCA - Oversample 

odel2os_pca = rpart(output~., data = oversamplingos_pca, method = 'class')

rpart.plot(model2os_pca)

predict_test2os_pca = predict(model, valid2, type = "class")

df4os_pca = data.frame("Actual" = valid2$output,
                 "Predicted" = predict_test2os_pca)
View(df4os_pca)

tb4os_pca = table("Actual" = df4os_pca$Actual,
            "Predicted" = df4os_pca$Predicted)
tb4os_pca

prob4os_pca = round(prop.table(tb4os_pca) * 100, 2)
prob4os_pca

acc4os_pca = 28.02 + 52.20
acc4os_pca

confusion_matrixos_pca = table(valid2$output, predict_test2os_pca)
confusion_matrixos_pca


# Random Forests PCA
rf2 = randomForest(output~., data = train2, proximity=TRUE) 
print(rf2)

p2 = predict(rf, train2)
confusionMatrix(p2, train2$output)

## Random Forest PCA -  Undersample 
rf_under_pca = randomForest(output~., data = under_pca, proximity=TRUE)
print(rf_under_pca)

predict_rf_under_pca = predict(rf_under_pca, under_pca)
confusionMatrix(predict_rf_under_pca, under_pca$output)
## Accuracy of (.9682,1)

### Rancom Forest PCA - Oversample 

rf2os_pca = randomForest(output~., data = oversamplingos_pca, proximity=TRUE) 
print(rf2os_pca)

p2os_pca = predict(rfos, oversamplingos_pca)
confusionMatrix(p2os_pca, oversamplingos_pca$output)

# Clustering PCA
d_dist2 = daisy(pca_scaled_df, metric = "gower")
# hierarchical clustering
hc2 = hclust(d_dist2, method = "complete")
# dendrogram 
plot(hc2, labels=FALSE)
rect.hclust(hc2, k=8, border="red")
# choose k, number of clusters 
cluster2 = cutree(hc2, k=8)

## Clustering PCA - Undersample
d_dist_under_pca = daisy(under_pca, metric = "gower")
## hierarchical clustering
hc_undersampled_pca = hclust(d_dist_under_pca, method = "complete")
## dendrogram 
plot(hc_undersampled_pca, labels=FALSE)
rect.hclust(hc_undersampled_pca, k=8, border="red")
## choose k, number of clusters 
cluster = cutree(hc_undersampled_pca, k= 8)

### Clustering PCA - Oversampling 

d_dist2os_pca = daisy(oversamplingos_pca, metric = "gower")
### hierarchical clustering
hc2os_pca = hclust(d_dist2os_pca, method = "complete")
### dendrogram 
plot(hc2os_pca, labels=FALSE)
rect.hclust(hc2os_pca, k=8, border="red")
### choose k, number of clusters 
cluster2os_pca = cutree(hc2, k=8)

rm(list = ls())
