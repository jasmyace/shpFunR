gIntersectionDF <- function(shpA,shpB){
  #shpA <- env
  #shpB <- contour
  
  # shpA@data$R_ID <- seq(1,nrow(shpA))
  # shpB@data$R_ID <- seq(1,nrow(shpB))
  
  shpA@data$rowNamesA <- rownames(shpA@data)
  shpB@data$rowNamesB <- rownames(shpB@data) 
  
  if(sum(names(shpA@data) %in% c("R_ID")) > 0){shpA@data$R_ID <- NULL}
  if(sum(names(shpB@data) %in% c("R_ID")) > 0){shpB@data$R_ID <- NULL}
  if(sum(names(shpA@data) %in% c("BASRank")) > 0){shpA@data$BASRank <- NULL}
  
  int <- gIntersection(shpA,shpB,byid=TRUE,drop_lower_td=TRUE)
  
  
#   # testing loss of intersected towns.
#   shpB <- shpB[shpB@data$Grid_ID == 'WY085650',]
#   shpA <- shpA[shpA@data$Grid_ID == 'WY085650',]
#   
#   int <- gIntersection(shpA,shpB,byid=TRUE)#,drop_lower_td=TRUE)
#   
#   plot(shpA)                              # get town bounding box
#   plot(shpB,add=TRUE,col="gray")          # the grid cell
#   plot(shpA,add=TRUE,col="red")           # the whole town
#   plot(int@polyobj,add=TRUE,col="green")  # the polygon part
#   plot(int@lineobj,add=TRUE,col="blue",lwd=4)   # the linear part

  # to deal specifically with the oklahoma case of one int polygon.  weird.
  if(is.null(int)){
    int <- gIntersection(shpA,shpB,byid=TRUE)@polyobj
  }
  
  if( class(int) == "SpatialPolygons" ){
    theIDs <- sapply(int@polygons,function(x) x@ID)
  } else if( class(int) == "SpatialLines" ){
    theIDs <- sapply(int@lines,function(x) x@ID)
  }
  theIDsA <- data.frame(rowNamesA=unlist(strsplit(theIDs," "))[c(TRUE,FALSE)])
  theIDsB <- data.frame(rowNamesB=unlist(strsplit(theIDs," "))[c(FALSE,TRUE)])
  bbone0 <- cbind(data.frame(theIDs=theIDs),theIDsA,theIDsB)
  bbone0$R_ID <- seq(1,nrow(bbone0),1)
  
  bbone1 <- merge(bbone0,shpA@data,by=c('rowNamesA'),all.x=TRUE)
  bbone2 <- merge(bbone1,shpB@data,by=c('rowNamesB'),all.x=TRUE)
  bbone2 <- bbone2[order(as.numeric(bbone2$R_ID)),]
  
  rownames(bbone2) <- theIDs
  
  if( class(int) == "SpatialPolygons" ){
    theInt <- SpatialPolygonsDataFrame(int,bbone2)
  } else if( class(int) == "SpatialLines" ){
    theInt <- SpatialLinesDataFrame(int,bbone2)
  }
  return(theInt)
}