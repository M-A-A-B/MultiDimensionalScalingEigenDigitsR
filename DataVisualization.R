X = as.matrix(read.table('ocr.txt'))
XLabel = as.matrix(read.table('ocr.lables'))
r = X[1100,] #first digit image, i.e., image in row 1
im = matrix(r,nrow=16,byrow=TRUE) #convert vector to image
image(im[,ncol(im):1]) #view image

#Multi-dimensional Scaling for data visualization
print("Starting MDS Implementation")
meanX <- colMeans(X)

#Step 1
Xc <- scale(X, scale=FALSE) #subtract each column from its corresponding colum mean

#Step 2
B = Xc %*% t(Xc)

#Step 3
eB <- eigen(B)
eigenValuesB <- eB$values
eigenVectorsB <- eB$vectors

#Step 4
print("Top 2 Eigen Values")
print(eigenValuesB[1:2])
V <- eigenVectorsB[,1:2]

#Step 5
S <- rbind(c(sqrt(eigenValuesB[1]),0),c(0,sqrt(eigenValuesB[2])))
Z <- V %*% S

#Step 6-7
digit2 <- XLabel == 2
digit3 <- XLabel == 3
digit4 <- XLabel == 4
digit5 <- XLabel == 5

plot(Z[digit2,1],Z[digit2,2],col='yellow', xlim = c(-4,4), ylim = c(-4,4), main = "MDS", xlab = "X axis", ylab = "Y axis")  	#this will plot training data points for class one in yellow color
points(Z[digit3,1],Z[digit3,2],col='green') #this will plot training data points for class zero in green color
points(Z[digit4,1],Z[digit4,2],col='red') #this will plot training data points for class zero in red color
legend("bottomleft", legend=c("2", "3", "4"),
       col=c("yellow", "green", "red"), pch = 1)

print("Ending MDS Implementation")

print("Starting Eigen Digits Implementation")
#Display eigen digits

#For digit 2
print("For digit 2")
X2 = X[digit2,]
covX2 = cov(X2)
ecovX2 <- eigen(covX2)
first4EigenVectors = ecovX2$vectors[,1:4]
last4EigenVectors = ecovX2$vectors[,253:256]

first = first4EigenVectors[,1]
second = first4EigenVectors[,2]
third = first4EigenVectors[,3]
fourth = first4EigenVectors[,4]

last = last4EigenVectors[,1]
secondLast = last4EigenVectors[,2]
thirdLast = last4EigenVectors[,3]
fourthLast = last4EigenVectors[,4]

dim(first) = c(16,16)
dim(second) = c(16,16)
dim(third) = c(16,16)
dim(fourth) = c(16,16)

dim(last) = c(16,16)
dim(secondLast) = c(16,16)
dim(thirdLast) = c(16,16)
dim(fourthLast) = c(16,16)

image(first)
image(second)
image(third)
image(fourth)

image(last)
image(secondLast)
image(thirdLast)
image(fourthLast)

#For digit 3
print("For digit 3")
X2 = X[digit3,]
covX2 = cov(X2)
ecovX2 <- eigen(covX2)
first4EigenVectors = ecovX2$vectors[,1:4]
last4EigenVectors = ecovX2$vectors[,253:256]

first = first4EigenVectors[,1]
second = first4EigenVectors[,2]
third = first4EigenVectors[,3]
fourth = first4EigenVectors[,4]

last = last4EigenVectors[,1]
secondLast = last4EigenVectors[,2]
thirdLast = last4EigenVectors[,3]
fourthLast = last4EigenVectors[,4]

dim(first) = c(16,16)
dim(second) = c(16,16)
dim(third) = c(16,16)
dim(fourth) = c(16,16)

dim(last) = c(16,16)
dim(secondLast) = c(16,16)
dim(thirdLast) = c(16,16)
dim(fourthLast) = c(16,16)

image(first)
image(second)
image(third)
image(fourth)

image(last)
image(secondLast)
image(thirdLast)
image(fourthLast)

print("Ending Eigen Digits Implementation")
