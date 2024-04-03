# Author: Prof. Sandro Camargo <github.com/sandrocamargo>
# This script uses the basic concepts of descriptive statistics
# In this script, we used the iris dataset https://archive.ics.uci.edu/dataset/53/iris

# cleaning workspace
rm(list=ls()) 

# Creating a function to compute de Mode
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Set working directory
#setwd("~/Downloads")

# Library to compute skewness and kurtosis
library(moments)  

# Downloading a public zip file and uncompressing
temp <- tempfile()
download.file("https://archive.ics.uci.edu/static/public/53/iris.zip",temp)
data <- read.table(unz(temp, "iris.data"), sep=",")

# Naming columns
colnames(data) <- c("SepalLength","SepalWidth","PetalLength","PetalWidth","Species")

# Viewing dataset: 5 attributes, 150 samples
View(data)

# Central tendency measurements

# Calculating the mean of the Sepal Length attribute from the Data variable
mean(data$SepalLength)

# Calculating the median of the Sepal Width attribute from the Data variable
median(data$SepalWidth)

# Finding the mode of the Petal Length attribute from the Data variable
getMode(data$PetalLength)

# Dispersion measurements
# Finding the quantiles of the Petal Width attribute from the Data variable
quantile(data$PetalWidth, probs = c(0.25,0.5,0.75)) 

# Finding the interquartile range of the Petal Width attribute from the Data variable
IQR(data$PetalWidth)

# Finding the 5 number summary of the Petal Width attribute from the Data variable
summary(data$PetalWidth)

# Summary of all attributes from the Data variable
summary(data)

# Plotting quantiles
boxplot(data$PetalWidth, horizontal = TRUE, xlab = "Petal Width")

boxplot(data[,1:4], cex.axis=0.8)

# Computing standard deviation
sd(data$SepalLength)

# Computing variance
var(data$SepalLength)

# Computing coefficient of variation
for (i in 1:4){
  print(paste(colnames(data)[i],": CV=",sd(data[,i]) / mean(data[,i]) * 100,"%"))
}

# Measuring the Shape
# Skewness
for (i in 1:4){
  plot(density(data[,i]), main=paste(colnames(data)[i], "- Skewness:",round(skewness(data[,i]),3),", Kurtosis:",round(kurtosis(data[,i]),3)))
  abline(v=mean(data[,i]), col="green")
  abline(v=median(data[,i]), col="blue")
  abline(v=getMode(data[,i]), col="red")
  legend("topright", c("Mean","Median","Mode"), col=c("green","blue","red"), lty=1)
}

# Kurtosis
#pdf("kurtosis.pdf")
plot(density(data$SepalLength), xlim=c(0,10), ylim=c(0,1), col="blue", main="Distributions")
lines(density(data$SepalWidth), col="red")
lines(density(data$PetalLength), col="green")
lines(density(data$PetalWidth), col="yellow")
legend("topright", c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"), col=c("blue","red","green","yellow"), lty=1)
#dev.off()

kurtosis(data$SepalLength)
kurtosis(data$SepalWidth)
kurtosis(data$PetalLength)
kurtosis(data$PetalWidth)

#pdf("boxplot-iris.pdf")
boxplot(data[,1:4], cex.axis=0.8)
#dev.off()

#pdf("hist-sepalwidth.pdf")
hist(data$SepalWidth)
#dev.off()
#pdf("hist-petalwidth.pdf")
hist(data$PetalWidth)
#dev.off()

#pdf("scatterplot-sepal.pdf")
plot(data$SepalLength,data$SepalWidth, main="Scatterplot", xlab="Sepal Length (cm)", ylab="Sepal Width (cm)")
#dev.off()
#pdf("scatterplot-petal.pdf")
plot(data$PetalLength,data$PetalWidth, main="Scatterplot", xlab="Petal Length (cm)", ylab="Petal Width (cm)")
#dev.off()

pairs(data[,1:4], col=as.factor(data$Species))

library(corrplot)
corrplot(cor(data[-5]), addCoef.col = "black",  )

