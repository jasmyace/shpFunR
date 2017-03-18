

#   ---- Get the biggest possible bounding box, based on feeding in set of shapefiles.

#   ---- Polygons.

  # shpList <- test

bboxUnion <- function(shpList){
  
  bboxMat <- sapply(shpList,function(x) x@bbox)
  
  xmin <- min(bboxMat[1,])
  xmax <- max(bboxMat[3,])
  ymin <- min(bboxMat[2,])
  ymax <- max(bboxMat[4,])
  
  bboxUnion <- data.frame(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)
 
  return(bboxUnion)
}