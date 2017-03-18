


makeRadialPlot <- function(r=100,k=1){
  
  # r <- 100
  # k <- 1
  
  #   ---- Make a limiting circle. 
  C <- makePolyCircle(r=r)
  
  #   ---- From the circle, get the origin and R.
  O <- C@polygons[[1]]@labpt
  R <- sqrt(C@polygons[[1]]@area / pi)
  
  #   ---- Find points on the circle to which to draw axes.  
  K <- seq(0,2*pi,length.out=(k + 1))
  x <- O[1]+R*cos(ifelse(K == 0,0,K))
  y <- O[2]+R*sin(ifelse(K == 0,0,K))
    
  #   ---- Prepare and make axis SpatialLines.
  P <- cbind(x,y)
  L <- vector("list",k)
  for(i in 1:k){
    L[[i]] <- Lines(list(Line(rbind(cbind(0,0),cbind(P[i,1],P[i,2])))),ID=as.character(i))
  }
  L <- SpatialLines(L)
  
  #   ---- Make inside circles for coordinates.
  lilR <- pretty(1:R)
  lilR <- lilR[lilR != 0]
  lilC <- vector("list",length(lilR))
  for(i in 1:length(lilR)){
    lilC[[i]] <- makePolyCircle(r=lilR[i])
  }
    
  plot(C,col="gray",border=NA)
  for(i in 1:length(lilR)){
    plot(lilC[[i]],col=NA,border="lightgray",lwd=2,add=TRUE)
  }
  plot(L,add=TRUE,col="lightgray",lwd=2)
}
    


plot(x,y,type="l")
lines(predict(obj,x=x),col="blue")


require(splines)

prettyCurve <- function(x,y){
  
  #   ---- Original points.
  X <- y*cos(2*pi/12*(x-1))
  Y <- y*sin(2*pi/12*(x-1))
  
  Z <- SpatialPoints(cbind(X,Y))
  
  #   ---- Transformed points.
  obj <- smooth.spline(x,y,penalty=0)
  xfine <- seq(min(x),max(x),0.01)
  yobj <- predict(obj,x=data.frame(xfine=xfine))
  yfine <- yobj[[2]][,1]

  Xfine <- yfine*cos(2*pi/12*(xfine-1))
  Yfine <- yfine*sin(2*pi/12*(xfine-1))
  
  fine <- SpatialLines(list(Lines(list(Line(cbind(Xfine,Yfine))),ID="1")))

  #   ---- Put together results. 
  out <- list(knots=Z,spline=fine)
}

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
y <- c(10,20,30,30,35,50,80,90,85,80,90,100,90,80,90,90,80,80)
test <- prettyCurve(x,y)
  
makeRadialPlot(r=100,k=2)
plot(test[[2]],add=TRUE)
plot(test[[1]],add=TRUE,pch=19)
