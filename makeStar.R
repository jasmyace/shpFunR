

makeStar <- function(rin=4,rout=5,class="q",id=NULL,origin=c(0,0),n=5,angle1=0,angle2=2*pi){
  
  #   rin <- 2
  #   rout <- 5
  #   class <- "q"
  #   id <- NULL
  #   origin <- c(0,0)
  #   n <- 6
  #   angle1 <- 0
  #   angle2 <- 2*pi
  
  iCircle <- makeCircle(r=rin,class="p",origin=origin,n=n,angle1=angle1,angle2=angle2)
  oCircle <- makeCircle(r=rout,class="p",origin=origin,n=n,angle1=angle1 + 2*pi/(n-1)/2,angle2=angle2 + 2*pi/(n-1)/2)
  
  #   ---- Build up the coordinates by alternating the points in the two circles.  
  #   ---- Get rid of last row since only need one to close the loop. 
  imat <- iCircle@coords
  omat <- oCircle@coords
  mat <- rbind(cbind(imat,seq(1,n),rep(1,n)),cbind(omat,seq(1,n),rep(2,n)))
  mat <- mat[order(mat[,3],mat[,4]),]
  mat <- mat[-(2*n),]
  mat[(2*n) - 1,] <- mat[1,]
  coords <- mat[,1:2]
  
  df <- data.frame(X=1,row.names="1")
  
  if(class=="p"){
    c1 <- SpatialPoints(coords)
    df <- data.frame(X=seq(1,nrow(c1@coords)),row.names=seq(1,nrow(c1@coords)))
    c2 <- SpatialPointsDataFrame(c1,df)
    rt <- c2
  } else if(class=="l"){
    c1 <- Line(coords)
    c2 <- Lines(list(c1),ID="1")
    c3 <- SpatialLines(list(c2))
    c4 <- SpatialLinesDataFrame(c3,df)
    rt <- c4
  } else if(class=="q"){
    c1 <- Polygon(coords)
    c2 <- Polygons(list(c1),ID="1")
    c3 <- SpatialPolygons(list(c2))
    c4 <- SpatialPolygonsDataFrame(c3,df)
    rt <- c4
  } else {
    stop("Invalid class specified;  try again with one of 'p', 'l', or 'q'.")
  }
  
  if(length(id) != 1 & !is.null(id)) stop("When specified, the length of argument id should be 1.")
  if(!is.null(id)) rt <- spChFIDs(rt, as.character(id))
  
  return(rt)
}
