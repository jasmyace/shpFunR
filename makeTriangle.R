makeTriangle <- function(p1,p2,p3,class="q",id=NULL){
  
  coords <- rbind(p1,p2,p3,p1)
  
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