getwd()
setwd("C:/Users/HP/Documents/R")

dataset <- read.csv("mushrooms.csv", header = TRUE, stringsAsFactors = TRUE)
View(dataset)
dim(dataset)
str(dataset)

summary(dataset)

install.packages("tidyverse")
library(tidyverse)

glimpse(dataset)

#Checking Missing Values
library(naniar)

missing <- is.na(dataset)
missing_sum <- sum(missing == TRUE)
missing_perc <- (missing_sum/8124)*100
print(missing_perc)
vis_miss(dataset)

#Remove Duplicate Values
library(dplyr)
dataset_backup <- dataset
dataset <- dataset %>% distinct()
dim(dataset)

summary(dataset)

#Remove factor variable with only one level
dataset <- dataset %>% select(-veil.type)
dim(dataset)
summary(dataset)
str(dataset)

## We redefine each of the category for each of the variables
levels(dataset$class) <- c("edible", "poisonous")
levels(dataset$cap.shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(dataset$cap.color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(dataset$cap.surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(dataset$bruises) <- c("false", "true")
levels(dataset$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(dataset$gill.attachment) <- c("attached", "free")
levels(dataset$gill.spacing) <- c("close","crowded")
levels(dataset$gill.size) <- c("broad", "narrow")
levels(dataset$gill.color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow")
levels(dataset$stalk.shape) <- c("enlarging", "tapering")
levels(dataset$stalk.root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(dataset$stalk.surface.above.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(dataset$stalk.surface.below.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(dataset$stalk.color.above.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(dataset$stalk.color.below.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(dataset$veil.color) <- c("brown", "orange", "white", "yellow")
levels(dataset$ring.number) <- c("none", "one", "two")
levels(dataset$ring.type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(dataset$spore.print.color) <- c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow")
levels(dataset$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(dataset$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")
view(dataset)


library(ggplot2)
library(gridExtra)

#Visualizing Data by bar plot
p1 <- ggplot(data = dataset)
m1 <- p1 + geom_bar(mapping = aes(x = cap.shape, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$cap.shape, dataset$class)

p2 <- ggplot(data = dataset)
m2 <- p2 + geom_bar(mapping = aes(x = cap.surface, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$cap.surface, dataset$class)

grid.arrange(m1, m2, ncol = 2)

p3 <- ggplot(data = dataset)
m3 <- p3 + geom_bar(mapping = aes(x = cap.color, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$cap.color, dataset$class)
grid.arrange(m3, ncol = 1)

p4 <- ggplot(data = dataset)
m4 <- p4 + geom_bar(mapping = aes(x = odor, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$odor, dataset$class)
grid.arrange(m4, ncol = 1)

p5 <- ggplot(data = dataset)
m5 <- p5 + geom_bar(mapping = aes(x = bruises, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$bruises, dataset$class)

p6 <- ggplot(data = dataset)
m6 <- p6 + geom_bar(mapping = aes(x = gill.attachment, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$gill.attachment, dataset$class)

p7 <- ggplot(data = dataset)
m7 <- p7 + geom_bar(mapping = aes(x = gill.spacing, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$gill.spacing, dataset$class)

p8 <- ggplot(data = dataset)
m8 <- p8 + geom_bar(mapping = aes(x = gill.size, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$gill.size, dataset$class)
grid.arrange(m5, m6, m7, m8, ncol = 2)

p9 <- ggplot(data = dataset)
m9 <- p9 + geom_bar(mapping = aes(x = gill.color, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$gill.color, dataset$class)
grid.arrange(m9, ncol = 1)

p10 <- ggplot(data = dataset)
m10 <- p10 + geom_bar(mapping = aes(x = stalk.shape, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.shape, dataset$class)

p11 <- ggplot(data = dataset)
m11 <- p11 + geom_bar(mapping = aes(x = stalk.root, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.root, dataset$class)

p12 <- ggplot(data = dataset)
m12 <- p11 + geom_bar(mapping = aes(x = stalk.surface.above.ring, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.surface.above.ring, dataset$class)

p13 <- ggplot(data = dataset)
m13 <- p13 + geom_bar(mapping = aes(x = stalk.surface.below.ring, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.surface.below.ring, dataset$class)
grid.arrange(m10, m11, m12, m13, ncol = 2)

p14 <- ggplot(data = dataset)
m14 <- p14 + geom_bar(mapping = aes(x = stalk.color.above.ring, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.color.above.ring, dataset$class)
grid.arrange(m14, ncol = 1)

p15 <- ggplot(data = dataset)
m15 <- p15 + geom_bar(mapping = aes(x = stalk.color.below.ring, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$stalk.color.below.ring, dataset$class)
grid.arrange(m14, m15, ncol = 1)

p16 <- ggplot(data = dataset)
m16 <- p16 + geom_bar(mapping = aes(x = veil.color, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$veil.color, dataset$class)

p17 <- ggplot(data = dataset)
m17 <- p17 + geom_bar(mapping = aes(x = ring.type, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$ring.type, dataset$class)

p18 <- ggplot(data = dataset)
m18 <- p18 + geom_bar(mapping = aes(x = ring.number, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$ring.number, dataset$class)

p19 <- ggplot(data = dataset)
m19 <- p19 + geom_bar(mapping = aes(x = spore.print.color, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$spore.print.color, dataset$class)
grid.arrange(m16, m17, m18, m19, ncol = 2)

p20 <- ggplot(data = dataset)
m20 <- p20 + geom_bar(mapping = aes(x = population, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$population, dataset$class)

p21 <- ggplot(data = dataset)
m21 <- p21 + geom_bar(mapping = aes(x = habitat, fill=class), position = position_dodge())+ theme(legend.position = "top")
table(dataset$habitat, dataset$class)
grid.arrange(m20, m21, ncol = 1)


##Chi-Sqaured Test

library(MASS)
tbl1 <- table(dataset$class, dataset$cap.shape)
chisq.test(tbl1)

tbl2 <- table(dataset$class, dataset$cap.surface)
chisq.test(tbl2)

tbl3 <- table(dataset$class, dataset$cap.color)
chisq.test(tbl3)

tbl4 <- table(dataset$class, dataset$bruises)
chisq.test(tbl4)

tbl5 <- table(dataset$class, dataset$odor)
chisq.test(tbl5)

tbl6 <- table(dataset$class, dataset$gill.attachment)
chisq.test(tbl6)

tbl7 <- table(dataset$class, dataset$gill.spacing)
chisq.test(tbl7)

tbl8 <- table(dataset$class, dataset$gill.size)
chisq.test(tbl8)

tbl9 <- table(dataset$class, dataset$gill.color)
chisq.test(tbl9)

tbl10 <- table(dataset$class, dataset$stalk.shape)
chisq.test(tbl10)

tbl11 <- table(dataset$class, dataset$stalk.root)
chisq.test(tbl11)

tbl12 <- table(dataset$class, dataset$stalk.surface.above.ring)
chisq.test(tbl12)

tbl13 <- table(dataset$class, dataset$stalk.surface.below.ring)
chisq.test(tbl13)

tbl14 <- table(dataset$class, dataset$stalk.color.above.ring)
chisq.test(tbl14)

tbl15 <- table(dataset$class, dataset$stalk.color.below.ring)
chisq.test(tbl15)

tbl16 <- table(dataset$class, dataset$veil.color)
chisq.test(tbl16)

tbl17 <- table(dataset$class, dataset$ring.number)
chisq.test(tbl17)

tbl18 <- table(dataset$class, dataset$ring.type)
chisq.test(tbl18)

tbl19 <- table(dataset$class, dataset$spore.print.color)
chisq.test(tbl19)

tbl20 <- table(dataset$class, dataset$population)
chisq.test(tbl20)

tbl21 <- table(dataset$class, dataset$habitat)
chisq.test(tbl21)

## Strength of Relationship Test
install.packages("GoodmanKruskal")
library(GoodmanKruskal)
varset1<- c("bruises", "odor", "gill.size", "gill.color", "stalk.surface.above.ring", "stalk.surface.below.ring", "stalk.color.above.ring", 
            "stalk.color.below.ring", "ring.type", "spore.print.color", "class")
mushroomFrame1 <- subset(dataset, select = varset1)
GKmatrix1 <- GKtauDataframe(mushroomFrame1)
plot(GKmatrix1, corrColors = "blue")

#split the data into a training and testing set
set.seed(2000)
dataset[,"train"] <- ifelse(runif(nrow(dataset)) < 0.8,1,0)

train_dataset <- dataset[dataset$train == "1", ]
test_dataset <- dataset[dataset$train == "0", ]
view(train_dataset)
view(test_dataset)

train_dataset <- train_dataset[-23]
test_dataset <- test_dataset[-23]
view(train_dataset)
view(test_dataset)

#Decision Tree
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

model_tree <- rpart(class ~ ., data = train_dataset, method = "class")
rpart.plot(model_tree, extra = 106)
trees_pred <- predict(model_tree, newdata = test_dataset, type = "class")
table(predicted = trees_pred, actual = test_dataset$class)
mean(trees_pred == test_dataset$class)


##Random Forest 
install.packages("party")
library(party)

forest_model <- cforest(class ~., data = train_dataset, control = cforest_unbiased(mtry = 15, ntree = 50))
fm_pred <- predict(forest_model, newdata = test_dataset, type = "response")
table(predicted = fm_pred, actual = test_dataset$class)
mean(fm_pred == test_dataset$class)

##Which feature in dataset is responsible for greatest amount of variance
library(randomForest)
yo <- train_dataset %>% na.omit()
model_rf <- randomForest(class ~ ., data = yo ,impotance = TRUE)
varImpPlot(model_rf, sort = TRUE, main = "Important Feature")

##SVM
install.packages("e1071")
library(e1071)
svm_model <- svm(class ~., data = train_dataset)
svm_pred <- predict(svm_model, newdata = test_dataset, type = "response")
table(predicted = svm_pred, actual = test_dataset$class)
mean(svm_pred == test_dataset$class)


#Check the prediction
test_svm <- predict(svm_model, newdata = test_dataset %>% na.omit())
yo <- test_dataset %>% na.omit()
table(test_svm, yo$class)
mean(test_svm == test_dataset$class)

## Model Evaluation

#Decision Tree
TP <- 877
TN <- 732
FP <- 11
FN <- 0
Acc <- (TP+TN) / (TP+TN+FP+FN)
Acc

#Precision & Recall for Decision Tree
precision <- TP / (TP+FP)
recall <- TP / (TP+FN)
precision
recall

#F1 Score Decision Tree
F1 <- 2 * ((precision * recall) / (precision + recall))
F1

# MCC for Decision Tree
mcc <- ((TP * TN) - (FP * FN)) / (((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)) ^ 0.5)
mcc

##Random Forest
TP <- 877
TN <- 739
FP <- 4
FN <- 0
Acc <- (TP+TN) / (TP+TN+FP+FN)
Acc

#Precision & Recall for Random Forest 
precision <- TP / (TP+FP)
recall <- TP / (TP+FN)
precision
recall

#F1 Score Random Forest
F1 <- 2 * ((precision * recall) / (precision + recall))
F1

# MCC for Random Forest
mcc <- ((TP * TN) - (FP * FN)) / (((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)) ^ 0.5)
mcc


