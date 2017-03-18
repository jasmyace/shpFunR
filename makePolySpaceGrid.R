makePolySpaceGrid <- function(origin=c(0,0),xPoly,yPoly,delx,dely,delta,epsilon,xaxis=TRUE,yaxis=TRUE){
  
  #   origin <- c(0,0)
  #   xPoly <- 2
  #   yPoly <- 5
  #   delx <- 1
  #   dely <- 1
  #   delta <- 0.25
  #   epsilon <- 0.25
  #   xaxis <- TRUE
  #   yaxis <- FALSE
  
  #   ---- Start the plotting by identifying the top-left origin.  
  x <- origin[1]
  y <- origin[2]
  
  #   ---- If we want x-axis labels, bump up the xPoly by 1.
  if(xaxis == TRUE){
    xPoly <- xPoly + 1
  }
  
  #   ---- If we want y-axis labels, bump up the yPoly by 1.
  if(yaxis == TRUE){
    yPoly <- yPoly + 1
  }
  
  #   ---- Define a list to house the set of individual Polygons.  
  P <- vector("list",xPoly*yPoly)
  
  #   ---- Build the list of individual Polygons. 
  for(i in 1:yPoly){
    
    x <- origin[1]
    
    for(j in 1:xPoly){
      
      #   ---- Define the coordinates for Polygon box (i,j).
      xVec <- c(x,x + delx,x + delx,x,x)
      yVec <- c(y,y,y + dely,y + dely,y)
      
      #   ---- Identify which number Polygon this is, actually put the coordinates 
      #   ---- into a Polygon object, and then slot it into the list.  
      z <- (i - 1)*xPoly + j
      p <- Polygon(cbind(xVec,yVec))                   
      P[[z]] <- Polygons(list(p),ID=as.character(z))  
      
      #   ---- Advance the x-coordinate for the next box to the right.
      x <- x + delx + delta
    }
    
    #   ---- Advance the y-coordinate for the next row below the previous.  
    y <- y - dely - epsilon
  } 
  
  SP <- SpatialPolygons(P)                              # Make a SpatialPolygons.
  return(SP)
}