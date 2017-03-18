combineShps <- function(shpList){
  
  require(maptools)
  
  #   ---- code here eventually to make sure that all objects in shpList are of the same class.
  
  #   ---- code here to make sure data frames, if present, are of same size.
  
  #   ---- code here to make sure all objects have the same projection. 
  
  
  if( class(shpList[[1]]) %in% c("SpatialPoints","SpatialPointsDataFrame") ){
    
    classType <- (class(shpList[[1]]) == "SpatialPointsDataFrame")
    
    allShps <- NULL
    nullCheck <- 0
    for(i in 1:length(shpList)){
      if(!is.null(shpList[[i]])){
        if(nullCheck == 0){
          nullCheck <- 1
          newCoords <- slot(shpList[[1]],"coords")
          if(classType == 1){
            newDF <- slot(shpList[[1]],"data")
          }
        } else {
          newCoords <- rbind(newCoords,slot(shpList[[i]],"coords"))
          if(classType == 1){
            newDF <- rbind(newDF,slot(shpList[[i]],"data"))
          }
          nullCheck <- 2
        }
      }
    }
    rownames(newCoords) <- rownames(newDF)
    allShps <- SpatialPointsDataFrame(newCoords,newDF)
  }
  
  
  #   ---- logic for line & polygon type here.
  if( class(shpList[[1]]) %in% c("SpatialPolygons","SpatialPolygonsDataFrame","SpatialLines","SpatialLinesDataFrame") ){
    
    if( length(grep("lines",slotNames(shpList[[1]]))) > 0 ){
      type <- "lines"
    } else if( length(grep("polygons",slotNames(shpList[[1]]))) > 0 ){
      type <- "polygons"
    } else {
      stop("Trouble.\n")
    }

    allShps <- NULL
    nullCheck <- 0
    for(i in 1:length(shpList)){
      if(!is.null(shpList[[i]])){
        if(nullCheck == 0){
          nullCheck <- 1
        }
        nR <- length(slot(shpList[[i]],type))
        if(nullCheck == 1){
          uidR          <- 1
          allShps       <- spChFIDs(shpList[[i]], as.character(uidR:(uidR + nR - 1)))
          uidR          <- uidR + nR
          nullCheck     <- 2
        } else {
          shpList[[i]]  <- spChFIDs(shpList[[i]], as.character(uidR:(uidR + nR - 1)))
          uidR          <- uidR + nR
          allShps       <- spRbind(allShps,shpList[[i]])
        }
      }
    }
  }
  allShps@proj4string <- shpList[[1]]@proj4string
  return(allShps)
}