makeCircle <- function(r=4,class="q",id=NULL,origin=c(0,0),n=100,angle1=0,angle2=2*pi){
  
  x <- origin[1] + r*cos(seq(angle1,angle2,length.out=n))
  y <- origin[2] + r*sin(seq(angle1,angle2,length.out=n))
  
  x[abs(x) < 1e-5] <- 0
  y[abs(y) < 1e-5] <- 0
  
  #   ---- Deal with rounding issues. 
  coords <- cbind(x,y)
  coords <- rbind(coords)

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