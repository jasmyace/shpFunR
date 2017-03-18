makePolyCircle <- function(origin=c(0,0),r=4,n=100,angle1=0,angle2=2*pi){
  
  x <- origin[1] + r*cos(seq(0,2*pi,length.out=n))
  y <- origin[2] + r*sin(seq(0,2*pi,length.out=n))
  
  coords <- cbind(x,y)
  coords <- rbind(coords,coords[1,])
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  return(c4)
}