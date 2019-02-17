#SVM
#Support vector machine is a supervised classification method separtes data using hyperplane
#hyperplane act as a discussion boundry
#in general svm can be used as a multiple separtlying hyperplanecalled segment and each segment is of separte datatype
#advantage of svm is used in both regression and classification
#anther advatage is svm use kernal trick using kernl trick svm make use of tranformation of data in such a way that data can be separble by hyerplane

#SVM WORKS?
data(iris)
str(iris)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,color=Species)

#Support Vector Machines
library(e1071)
mymodel<-svm(Species~.,data=iris)
summary(mymodel)
plot(mymodel,data = iris)
#C: Penalty parameter C of the error term. It also controls the trade off between smooth decision boundary and classifying the training points correctly.
#gamma: Kernel coefficient for ‘rbf’, ‘poly’ and ‘sigmoid’. Higher the value of gamma, will try to exact fit the as per training data set i.e. generalization error and cause over-fitting problem.
#Parameters:
 # SVM-Type:  C-classification  because our output variabl is factorial
# SVM-Kernel:  radial there are 4 type of kernal by dfault its radial 
#cost:  1  cost arameter is 1
#gamma:  0.25 
#Number of Support Vectors:  51
#( 8 22 21 ) 8 belong to 1st type and 2dna nd 3rd
#Number of Classes:  3 
#Levels: 
 # setosa versicolor virginica
plot(mymodel,data=iris,Petal.Width~Petal.Length,slice = list(Sepal.Width=3,Sepal.Length=4))

#missclassification and crossvaliation
pred<-predict(mymodel,iris)
tab<-table(pred,iris$Species)
tab
1-sum(diag(tab))/sum(tab)


mymodel<-svm(Species~.,data=iris ,kernel="linear")
summary(mymodel)
plot(mymodel,data = iris)
#C: Penalty parameter C of the error term. It also controls the trade off between smooth decision boundary and classifying the training points correctly.
#gamma: Kernel coefficient for ‘rbf’, ‘poly’ and ‘sigmoid’. Higher the value of gamma, will try to exact fit the as per training data set i.e. generalization error and cause over-fitting problem.
#Parameters:
# SVM-Type:  C-classification  because our output variabl is factorial
# SVM-Kernel:  radial there are 4 type of kernal by dfault its radial 
#cost:  1  cost arameter is 1
#gamma:  0.25 
#Number of Support Vectors:  51
#( 8 22 21 ) 8 belong to 1st type and 2dna nd 3rd
#Number of Classes:  3 
#Levels: 
# setosa versicolor virginica
plot(mymodel,data=iris,Petal.Width~Petal.Length,slice = list(Sepal.Width=3,Sepal.Length=4))

#missclassification and crossvaliation
pred<-predict(mymodel,iris)
tab<-table(pred,iris$Species)
tab
1-sum(diag(tab))/sum(tab) #error rate:3.33%

mymodel<-svm(Species~.,data=iris ,kernel="sigmoid")
summary(mymodel)
plot(mymodel,data = iris)
#C: Penalty parameter C of the error term. It also controls the trade off between smooth decision boundary and classifying the training points correctly.
#gamma: Kernel coefficient for ‘rbf’, ‘poly’ and ‘sigmoid’. Higher the value of gamma, will try to exact fit the as per training data set i.e. generalization error and cause over-fitting problem.
#Parameters:
# SVM-Type:  C-classification  because our output variabl is factorial
# SVM-Kernel:  radial there are 4 type of kernal by dfault its radial 
#cost:  1  cost arameter is 1
#gamma:  0.25 
#Number of Support Vectors:  51
#( 8 22 21 ) 8 belong to 1st type and 2dna nd 3rd
#Number of Classes:  3 
#Levels: 
# setosa versicolor virginica
plot(mymodel,data=iris,Petal.Width~Petal.Length,slice = list(Sepal.Width=3,Sepal.Length=4))

#missclassification and crossvaliation
pred<-predict(mymodel,iris)
tab<-table(pred,iris$Species)
tab
1-sum(diag(tab))/sum(tab) #error rate:11.33%


mymodel<-svm(Species~.,data=iris ,kernel="polynomial")
summary(mymodel)
plot(mymodel,data = iris)
#C: Penalty parameter C of the error term. It also controls the trade off between smooth decision boundary and classifying the training points correctly.
#gamma: Kernel coefficient for ‘rbf’, ‘poly’ and ‘sigmoid’. Higher the value of gamma, will try to exact fit the as per training data set i.e. generalization error and cause over-fitting problem.
#Parameters:
# SVM-Type:  C-classification  because our output variabl is factorial
# SVM-Kernel:  radial there are 4 type of kernal by dfault its radial 
#cost:  1  cost arameter is 1
#gamma:  0.25 
#Number of Support Vectors:  51
#( 8 22 21 ) 8 belong to 1st type and 2dna nd 3rd
#Number of Classes:  3 
#Levels: 
# setosa versicolor virginica
plot(mymodel,data=iris,Petal.Width~Petal.Length,slice = list(Sepal.Width=3,Sepal.Length=4))

#missclassification and crossvaliation
pred<-predict(mymodel,iris)
tab<-table(pred,iris$Species)
tab
1-sum(diag(tab))/sum(tab) #error rate:4.7%

#TUNNING
#HIGHPER parameter optimization help us to choose best model
set.seed(123)
tmodel<-tune(svm,Species~.,data = iris,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)

#best model
mymodel1<-tmodel$best.model
summary(mymodel1)
#for clssification radial is best kernel

plot(mymodel1,data=iris,Petal.Width~Petal.Length,slice = list(Sepal.Width=3,Sepal.Length=4))
pred<-predict(mymodel1,iris)
tab<-table(pred,iris$Species)
tab
1-sum(diag(tab))/sum(tab)
#1.1% error