require(sp)

cp4 <- makeCircle(r=4,class="p",n=20)
cl5 <- makeCircle(r=5,class="l",n=20)
cq6 <- makeCircle(r=6,class="q",n=20)

#plot(cp4,pch=19)
#plot(cl5,add=TRUE)
#plot(cq6,add=TRUE)

randCoords <- matrix(sample(c(1:150)),nrow=50,ncol=3)
test <- apply(randCoords,1,function(x) makeCircle(origin=c(x[1],x[2]),r=x[3]))

bboxU <- bboxUnion(test)

xmin <- bboxU$xmin   # get bboxUnion
xmax <- bboxU$xmax
ymin <- bboxU$ymin
ymax <- bboxU$ymax

for(i in 1:50){
  if(i == 1){
    plot(test[[1]],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  } else {
    plot(test[[i]],xlim=c(xmin,xmax),ylim=c(ymin,ymax),add=TRUE)
  }
}