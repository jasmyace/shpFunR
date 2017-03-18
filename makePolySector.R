makePolySector <- function(origin=c(1,0),r=4,n=100,angle1,angle2){
  
  x <- origin[1] + r*cos(seq(angle1,angle2,length.out=n))
  y <- origin[2] + r*sin(seq(angle1,angle2,length.out=n))
  
  coords <- cbind(x,y)
  coords <- rbind(coords,c(0,0),coords[1,])
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  return(c4)
}