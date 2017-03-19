makeSector <- function(r=4,class="q",id=NULL,origin=c(0,0),n=100,angle1,angle2){
  
  if((angle2 - angle1) %% (2*pi) == 0 & (angle1 != angle2)){
    stop("Variables angle1 and angle2 are multiples of 2*pi.  Set these unequal and try again, or, use function makeCircle.")
  } else if((angle2 - angle1) %% (2*pi) == 0 & (angle1 == angle2)){
    stop("Variable angle1 and angle2 are equal.  Set these unequal and try again.")
  } else if((angle2 - angle1) > 2*pi){
    stop("The angular difference between angle1 and angle2 is greater than 2*pi.  Ensure these are smaller than 2*pi and try again.")
  } else if(angle1 < angle2){
    x <- origin[1] + r*cos(seq(angle1,angle2,length.out=n))
    y <- origin[2] + r*sin(seq(angle1,angle2,length.out=n))
  } else if(angle2 < angle1){
    x <- origin[1] + r*cos(seq(angle1,2*pi + angle2,length.out=n))
    y <- origin[2] + r*sin(seq(angle1,2*pi + angle2,length.out=n))
  } 
  
  coords <- cbind(x,y)
  coords <- rbind(coords,origin,coords[1,])
  
  df <- data.frame(X=1,row.names="1")
  
  if(class=="p"){
    c1 <- SpatialPoints(coords)
    rt <- c1
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