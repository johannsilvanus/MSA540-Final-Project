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

acc = 33.52 + 45.60
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

rm(list = ls())
