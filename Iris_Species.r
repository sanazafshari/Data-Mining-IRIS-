# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")
names(iris)
data<- iris[1:4] # four attribute('Sepal.Length' 'Sepal.Width' 'Petal.Length' 'Petal.Width')
class<-as.matrix(iris[5]) # the class ('Species')

cor(data)
#Visualisation
pairs(iris[1:4], pch=21, bg= c("red","green3","blue")[unclass(iris$Species)])

#Principal Component Analysis
iris.pca<- prcomp(iris[1:4])
iris.pca

summary(iris.pca)

pairs(iris.pca$x, main = "PCA PLOT", font.main = 4, pch = 21,bg= c("red","green3","blue")[unclass(iris$Species)])
plot(iris.pca$x, main = "PCA PLOT", font.main = 4, pch = 21, bg= c("red","green3","blue")[unclass(iris$Species)])

#PCA with princomp function
iris.pca2 <- princomp(iris[1:4])
iris.pca2
biplot(iris.pca2)



# PART2

# CLUSTERING

irisn <- t(apply(iris[1:4], 1, as.numeric))
normalize<- function(row){
  (row - mean(row))/ sd(row)
}
iris_zs <- apply(irisn,2,normalize)

# K-Means
#For obtaining a k-means clustering with k = 3 clusters on the third and fifth attributes
c1 <- kmeans(iris[1:4], 3)
print(c1)
plot(iris[3:4], col = c1$cluster)



# PART3

# Classification

# Decision Tree

library(rpart)
library("rpart.plot")
"dividing the dataset in two random samples: one for training the model and the other one for testing the
model."
set.seed(2586)
n <- nrow(iris)
train <- sort(sample(1:n, floor(n/2)))
iris.train <- iris[train, ]
iris.test <- iris[-train, ]

library(rpart)
library(rpart.plot)
tree <- rpart(Species ~., data = iris.train, method = "class")
tree
rpart.plot(tree)

pred.rep <- predict(tree, newdata = iris[-train,], type = "class" )

table(class[-train], pred.rep)

#k-Nearest Neighbours (k-NN):

library(class)
data(iris3)
str(iris3)

train <- rbind(iris3[1:25,,1],iris3[1:25,,2],iris3[1:25,,3])
test <-  rbind(iris3[26:50,,1],iris3[26:50,,2],iris3[26:50,,3])
cl <- factor (c(rep("Setosa",25), rep("Versicolor",25), rep("Virginica",25)))
pred <- knn(train, test, cl, k=3)
table(pred, cl)




