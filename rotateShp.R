#    ---- This function works only on a single-polygon SpatialPoints(DataFrame).  
rotateShp <- function(shp,theta){
  # shp <- env
  # theta <- pi/2
  
  #   ---- Utilized homoegenous coordinates.  Not sure this is necessary.  It makes the 
  #   ---- the matrix multiplication below possibly over-complicated.  
  T <- matrix(c(1,0,0,0,1,0,-1*gCentroid(shp)@coords,1),nrow=3,ncol=3)
  R0 <- matrix(c(cos(theta),sin(theta),0,-sin(theta),cos(theta),0,0,0,1),nrow=3,ncol=3)
  Tinv <- matrix(c(1,0,0,0,1,0,gCentroid(shp)@coords,1),nrow=3,ncol=3)
  
  R <- Tinv %*% R0 %*% T
  
  k <- nrow(shp@polygons[[1]]@Polygons[[1]]@coords)
  shp2 <- shp
  shp2@polygons[[1]]@Polygons[[1]]@coords <- t(R %*% rbind(t(shp@polygons[[1]]@Polygons[[1]]@coords),rep(1,k)))[,1:2]
  return(shp2)
}