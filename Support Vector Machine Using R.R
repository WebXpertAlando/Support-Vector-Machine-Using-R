# SVM Implementation using R Studio

#install Packages
install.packages(e1071)

#include library
library(e1071)

# Load Dataset
#animal_data<-read.csv("C:\Users\Lux Dev\animal_data.csv", header= TRUE)


# create an ishorse indicator variable
ishorse<-c(rep(-1, 20), rep(+1, 20))

# Create a data frame for performing SVM
my.data<-data.frame(Height=animal_data['Height'],
                    Weight=animal_data['Weight'],
                    animal = as.factor(ishorse))

my.data
plot(my.data[,-3], col=3/2, pch=19);abline(h=0, v=0, lty=3)

#Perform the SVM by calling the SVM methods parameters

svm.model<-svm(animal~ ., 
            data=my.data, type="C-classification",
            kernel='linear',
            scale=FALSE)

summary(svm.model)

# Show the support Vector
points(my.data[svm.model$index, c(1,2)], col="orange", cex=2)

#get the parameters of the hyperplane is the line 
w<- t(svm.model$coefs) %*%  svm.model$SV
b<- -svm.model$rho


# In this 2D case, the hyperplane is the line
abline(a=-b/w[1,2],b=-w[1,1]/w[1,2], col="blue", lty=3)

#new data -mule, horse, mule
observations<- data.frame(Height=c(67,121,100), Weight=c(171, 190, 100))

# Plot the new data
plot(my.data[,-3], col=(ishorse+3)/2, pch=19, xlim=c(0,250), ylim=c(0,250))

abline(h=0, v=0, lty=3)
points(observations[1,], col="green", pch=19)
points(observations[2,], col="blue", pch=19)
points(observations[3,], col="dark orange", pch=19)
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3)

#Evaluate the results
predict(svm.model, observations)
               
