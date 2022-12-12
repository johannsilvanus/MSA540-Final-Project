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

# Random Forests
rf = randomForest(output~., data = train, proximity=TRUE) 
print(rf)

p1 = predict(rf, train)
confusionMatrix(p1, train$output)

# Clustering
d_dist = daisy(scaled_df, metric = "gower")
# hierarchical clustering
hc = hclust(d_dist, method = "complete")
# dendrogram 
plot(hc, labels=FALSE)
rect.hclust(hc, k=8, border="red")
# choose k, number of clusters 
cluster = cutree(hc, k=8)
# add cluster to original data 

# PCA
pc1 = prcomp(scaled_df[,-14])
summary(pc1) 

pca_scaled_df = scaled_df[, -c(12, 13)]

index2 = sort(sample(nrow(pca_scaled_df), nrow(pca_scaled_df) * 0.4))

train2 = scaled_df[index2,]
valid2 = scaled_df[-index2,]

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

# Random Forests PCA
rf2 = randomForest(output~., data = train2, proximity=TRUE) 
print(rf2)

p2 = predict(rf, train2)
confusionMatrix(p2, train2$output)

# Clustering PCA
d_dist2 = daisy(pca_scaled_df, metric = "gower")
# hierarchical clustering
hc2 = hclust(d_dist2, method = "complete")
# dendrogram 
plot(hc2, labels=FALSE)
rect.hclust(hc2, k=8, border="red")
# choose k, number of clusters 
cluster2 = cutree(hc2, k=8)


rm(list = ls())