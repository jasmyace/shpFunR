makePolyCircle <- function(r=4,id=NULL,origin=c(0,0),n=100,angle1=0,angle2=2*pi){
  
  x <- origin[1] + r*cos(seq(0,2*pi,length.out=n))
  y <- origin[2] + r*sin(seq(0,2*pi,length.out=n))
  
  coords <- cbind(x,y)
  coords <- rbind(coords,coords[1,])
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  
  if(length(id) != 1) stop("The length of argument id should be 1.")
  if(!is.null(id)) c4 <- spChFIDs(c4, as.character(id))
  
  return(c4)
}