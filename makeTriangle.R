makeTriangle <- function(p1,p2,p3){
  
  coords <- rbind(p1,p2,p3,p1)
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  return(c4)
  
}