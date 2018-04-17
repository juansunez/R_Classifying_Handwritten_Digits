###############################################################################################################
####### Below, you will need to change the file location to where the dataset is saved on YOUR computer #######
###############################################################################################################

digits <- read.csv("D:/Documents/Material/R Programming/Classifying Handwritten Digits/digits_train.csv",header=TRUE)

x <- c(rep(seq(1,28,1),28))
y <- c(rep(seq(1,28,1),28))
y <- sort(y,decreasing = TRUE)

#Examples is a list of row numbers from digits_train.csv. Each row numbers has the data to plot a digit.
#rows 206, 7, 100 and 660 were chosen for this example.
examples <- c(206,7,100,660)
#Change index of list to display digit in that row.
test <- examples[3]
digits.answer <- digits[test,1]

digits.test <- cbind(x,y,as.numeric(digits[test,-1]))

plot(x,y,pch=15,col="white",main=digits.answer,xlab="",ylab="")

for(i in 1:784){					
  if(digits.test[i,3] > 0 && digits.test[i,3]<50)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="snow2")
  if(digits.test[i,3] > 50 && digits.test[i,3]<150)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="snow3")
  if(digits.test[i,3] > 150 && digits.test[i,3]<225)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="snow4")
  if(digits.test[i,3] > 225)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="black")
}

legend(0,28,c("0","1-50","51-150","151-225","226-255"),fill=c("white","snow2","snow3","snow4","black"),title="Darkness values",bty="n")

#Combine data for each digit.
sum(as.numeric(digits[,1]==0))
digits.0<-c(rep(0,4132))
k<-1
for(i in 1:42000){
  if(digits[i,1]==0){
    digits.0[k]<-i
    k<-k+1
  }
}
digits.0data<- rbind(digits[digits.0,-1])

sum(as.numeric(digits[,1]==1))
digits.1<-c(rep(0,4684))
k<-1
for(i in 1:42000){
  if(digits[i,1]==1){
    digits.1[k]<-i
    k<-k+1
  }
}
digits.1data<- rbind(digits[digits.1,-1])


sum(as.numeric(digits[,1]==2))
digits.2<-c(rep(0,4177))
k<-1
for(i in 1:42000){
  if(digits[i,1]==2){
    digits.2[k]<-i
    k<-k+1
  }
}
digits.2data<- rbind(digits[digits.2,-1])


sum(as.numeric(digits[,1]==3))
digits.3<-c(rep(0,4351))
k<-1
for(i in 1:42000){
  if(digits[i,1]==3){
    digits.3[k]<-i
    k<-k+1
  }
}
digits.3data<- rbind(digits[digits.3,-1])


sum(as.numeric(digits[,1]==4))
digits.4<-c(rep(0,4072))
k<-1
for(i in 1:42000){
  if(digits[i,1]==4){
    digits.4[k]<-i
    k<-k+1
  }
}
digits.4data<- rbind(digits[digits.4,-1])


sum(as.numeric(digits[,1]==5))
digits.5<-c(rep(0,3795))
k<-1
for(i in 1:42000){
  if(digits[i,1]==5){
    digits.5[k]<-i
    k<-k+1
  }
}
digits.5data<- rbind(digits[digits.5,-1])


sum(as.numeric(digits[,1]==6))
digits.6<-c(rep(0,4137))
k<-1
for(i in 1:42000){
  if(digits[i,1]==6){
    digits.6[k]<-i
    k<-k+1
  }
}
digits.6data<- rbind(digits[digits.6,-1])


sum(as.numeric(digits[,1]==7))
digits.7<-c(rep(0,4401))
k<-1
for(i in 1:42000){
  if(digits[i,1]==7){
    digits.7[k]<-i
    k<-k+1
  }
}
digits.7data<- rbind(digits[digits.7,-1])


sum(as.numeric(digits[,1]==8))
digits.8<-c(rep(0,4063))
k<-1
for(i in 1:42000){
  if(digits[i,1]==8){
    digits.8[k]<-i
    k<-k+1
  }
}
digits.8data<- rbind(digits[digits.8,-1])


sum(as.numeric(digits[,1]==9))
digits.9<-c(rep(0,4188))
k<-1
for(i in 1:42000){
  if(digits[i,1]==9){
    digits.9[k]<-i
    k<-k+1
  }
}
digits.9data <- rbind(digits[digits.9,-1])

#########################Plot Number######################################
#with different colors
test <- digits.6[6]
digits.answer <- digits[test,1]

digits.test <- cbind(x,y,as.numeric(digits.6data[test,-1]))

plot(x,y,pch=15,col="white",main=digits.answer,xlab="",ylab="")

for(i in 1:784){					
  if(digits.test[i,3] > 0 && digits.test[i,3]<50)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="red")
  if(digits.test[i,3] > 50 && digits.test[i,3]<150)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="red")
  if(digits.test[i,3] > 150 && digits.test[i,3]<225)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="black")
  if(digits.test[i,3] > 225)
    points(digits.test[i,1],digits.test[i,2],pch=15,cex=3, col="blue")
}

#########################################################################################
###### To run the remaining code, you first need to install the rgl package #############
#########################################################################################
#calculate average of values for each digit.

library(rgl)

xx<- seq(1,300,length.out=28)#Create sequence from 1 to 300 and divide it in 28 equal pieces.
yy<- seq(1,300,length.out=28)
#xx <- seq(351,650,length.out=28)
#yy <- seq(351,650,length.out=28)

digits.0avg<- apply(digits.0data,2,mean) #Calculate mean for all the columns in digits.0data
digits.0matrix<- t(matrix(digits.0avg,28,28))
surface3d(xx,yy,digits.0matrix,color="white",back="lines")

digits.1avg<- apply(digits.1data,2,mean)
digits.1matrix<- t(matrix(digits.1avg,28,28))
open3d()
surface3d(xx,yy,digits.1matrix,color="white",back="lines")

digits.2avg<- apply(digits.2data,2,mean)
digits.2matrix<- t(matrix(digits.2avg,28,28))
open3d()
surface3d(xx,yy,digits.2matrix,color="white",back="lines")

digits.3avg<- apply(digits.3data,2,mean)
digits.3matrix<- t(matrix(digits.3avg,28,28))
open3d()
surface3d(xx,yy,digits.3matrix,color="white",back="lines")

digits.4avg<- apply(digits.4data,2,mean)
digits.4matrix<- t(matrix(digits.4avg,28,28))
open3d()
surface3d(xx,yy,digits.4matrix,color="white",back="lines")

digits.5avg<- apply(digits.5data,2,mean)
digits.5matrix<- t(matrix(digits.5avg,28,28))
open3d()
surface3d(xx,yy,digits.5matrix,color="white",back="lines")

digits.6avg<- apply(digits.6data,2,mean)
digits.6matrix<- t(matrix(digits.6avg,28,28))
open3d()
surface3d(xx,yy,digits.6matrix,color= c("red", "white", "blue"),back="lines")

digits.7avg<- apply(digits.7data,2,mean)
digits.7matrix<- t(matrix(digits.7avg,28,28))
open3d()
surface3d(xx,yy,digits.7matrix,color="white",back="lines")

digits.8avg<- apply(digits.8data,2,mean)
digits.8matrix<- t(matrix(digits.8avg,28,28))
open3d()
surface3d(xx,yy,digits.8matrix,color="white",back="lines")

digits.9avg<- apply(digits.9data,2,mean)
digits.9matrix<- t(matrix(digits.9avg,28,28))
open3d()
surface3d(xx,yy,digits.9matrix,color="white",back="lines")

#########################Draw Average######################################

avgs.draw6 <- cbind(x,y,as.numeric(digits.6avg))
summary(avgs.draw6)
plot(x,y,pch=15,col="white",main=digits.answer,xlab="",ylab="")

for(i in 1:784){					
  if(avgs.draw6[i,3] > 0 && avgs.draw6[i,3]< 2)
    points(avgs.draw6[i,1],avgs.draw6[i,2],pch=15,cex=3, col="snow3")
  if(avgs.draw6[i,3] > 2 && avgs.draw6[i,3] < 35)
    points(avgs.draw6[i,1],avgs.draw6[i,2],pch=15,cex=3, col="red")
  if(avgs.draw6[i,3] > 35 && avgs.draw6[i,3] < 58)
    points(avgs.draw6[i,1],avgs.draw6[i,2],pch=15,cex=3, col="black")
  if(avgs.draw6[i,3] > 58 && avgs.draw6[i,3] < 130)
    points(avgs.draw6[i,1],avgs.draw6[i,2],pch=15,cex=3, col="blue")
  if(avgs.draw6[i,3] > 139)
    points(avgs.draw6[i,1],avgs.draw6[i,2],pch=15,cex=3, col="black")
}

###############################Plot Averages################################################
digit.plot.data <- c(mean(digits.0avg), mean(digits.1avg), mean(digits.2avg), mean(digits.3avg), mean(digits.4avg), mean(digits.5avg), mean(digits.6avg), mean(digits.7avg), mean(digits.8avg), mean(digits.9avg))
plot(digit.plot.data, pch=20, bty="n", col="red", main="Plot of Averages for Each Digit", ylab= "Means", xlab="Digits", axes = FALSE)
axis(side=1,at=seq(0,10,1),lwd=3)
axis(side=2,at=seq(0,45,5),lwd=3)