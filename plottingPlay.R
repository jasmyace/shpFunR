


gridPlot(rowVar=info$Decomposition2,
         colVar=info$EstimatedTimeOfDeath2,
         title="Estimated Time of Death (y) vs. Decomposition (x)",
         rowLab="Estimated Time of Death",
         colLab="Decomposition",
         colPal="Greens")


# cal365 <- makePolySpaceGrid(c(0,0),31,12,1,1,0.15,0.15,TRUE,TRUE)
# plot(cal365)

gridPlot <- function(rowVar,colVar,title,colLab,rowLab,colPal="Blues"){
  
  require(RColorBrewer)
  #rowVar <- info$Decomposition2
  #colVar <- info$EstimatedTimeOfDeath2
  #title <- "Decomposition (x) vs. Estimated Time of Death (y)"
  #rowLab <- "Decomposition"
  #colLab <- "Estimated Time of Death"
  #colPal <- "Blues"
  
  #   ---- PART 1:  PREP THE SHAPEFILE.
  #   ---- Find the dimensions of what is to be plotted.  
  c <- length(unique(colVar))
  r <- length(unique(rowVar))
  
  #   ---- Prep the underlying data and blank grid.  Note that "#ffffff" is white.
  coords <- expand.grid(x=seq(2,c + 1),y=seq(1,r))
  df <- as.data.frame.table(table(colVar,rowVar))
  df$color <- rep("#ffffff",nrow(df))
  df[df$Freq > 0,]$color <- brewer.pal(4,colPal)[cut(df[df$Freq > 0,]$Freq,4,labels=FALSE)]
  df <- cbind(coords,df)
  
  #   ---- Put in rows in data frame for the shapefile rowLabels -- (x,y) = (1,1) is the top left.
  rowLabel <- if(is.factor(rowVar)){as.character(droplevels(sort(unique(rowVar))))} else {sort(unique(rowVar))}
  rowLabel <- substr(rowLabel,2,nchar(rowLabel))
  rowLabel <- data.frame(x=1,y=seq(1,r),colVar="rowLabel",rowVar=rowLabel,Freq=0,color="#ffffff")
  
  #   ---- Put in rows in data frame for the shapefile colLabels -- (x,y) = (1,1) is the top left.  
  colLabel <- if(is.factor(colVar)){as.character(droplevels(sort(unique(colVar))))} else {sort(unique(colVar))}
  colLabel <- substr(colLabel,2,nchar(colLabel))
  colLabel <- data.frame(x=seq(2,c + 1),y=(r + 1),colVar=colLabel,rowVar="colLabel",Freq=0,color="#ffffff")
  
  botLeft <- data.frame(x=1,y=(r + 1),colVar="BotLeftBlank",rowVar="BotLeftBlank",Freq=0,color="#ffffff")
  
  df <- rbind(df,rowLabel,colLabel,botLeft)
  df <- df[order(df$y,df$x),]
  rownames(df) <- NULL
  
  #   ---- Give the grid shapefile the data.
  gridShp <- makePolySpaceGrid(c(0,0),c,r,1,1,0.15,0.15,TRUE,TRUE)
  shp <- SpatialPolygonsDataFrame(gridShp,df)

  #   ---- PART 2:  MAKE THE PLOT.  
  #   ---- Prep the layout for plotting.  
  mat <- matrix(c(1,1,1,2,3,4,5,6,7),nrow=3,ncol=3,byrow=TRUE)
  l <- layout(mat,widths=c(0.05,0.9,0.05),heights=c(0.05,0.9,0.05))
  layout.show(l)
  
  #   ---- Cell 01:  Place title. 
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,title,cex=2.0)
  
  #   ---- Cell 02:  Place x-axis label.
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,colLab,cex=1.5,srt=90)
  
  #   ---- Cell 03:  Place and format the shapefile.
  par(mar=c(0,0,0,0))
  plot(shp[shp@data$colVar != "BotLeftBlank",],col=shp@data$color)
  text(t(sapply(shp[shp@data$colVar == "rowLabel",]@polygons,function(x) x@labpt)),label=shp[shp@data$colVar == "rowLabel",]$rowVar)
  text(t(sapply(shp[shp@data$rowVar == "colLabel",]@polygons,function(x) x@labpt)),label=shp[shp@data$rowVar == "colLabel",]$colVar)
  text(t(sapply(shp[shp@data$Freq != 0,]@polygons,function(x) x@Polygons[[1]]@coords[1,])) + 0.15,label=shp[shp@data$Freq != 0,]$Freq,col="black",cex=2.0)
  
  
  #   ---- Cell 04:  Blank right section.  
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,"",cex=2.5,srt=90)
  
  #   ---- Cell 05:  Blank bottom-left corner.
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,"",cex=2.5,srt=90)
  
  #   ---- Cell 06:  Place y-axis label.
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,rowLab,cex=1.5)
  
  #   ---- Cell 07:  Blank bottom-right corner.
  par(mar = c(0,0,0,0)); plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE); u <- par("usr") 
  text(1,u[1] + 0.95*(u[2]-u[1])/2,"",cex=2.5,srt=90)
  
}





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



makePolyCircle <- function(origin=c(1,0),r=4,n=100,angle1=0,angle2=2*pi){
  
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



makeTriangle <- function(p1,p2,p3){
  
  coords <- rbind(p1,p2,p3,p1)
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  return(c4)
  
}

makeSquare <- function(p1,p2,p3,p4){
  
  coords <- rbind(p1,p2,p3,p4,p1)
  
  df <- data.frame(X=1,row.names="1")
  
  c1 <- Polygon(coords)
  c2 <- Polygons(list(c1),ID="1")
  c3 <- SpatialPolygons(list(c2))
  c4 <- SpatialPolygonsDataFrame(c3,df)
  return(c4)
  
}



AddHoleToPolygon <-function(poly,hole){
  
  # poly <- outCircle
  # hole <- inCircle
  
  # invert the coordinates for Polygons to flag it as a hole
  coordsHole <-  hole@polygons[[1]]@Polygons[[1]]@coords
  newHole <- Polygon(coordsHole,hole=TRUE)
  
  # punch the hole in the main poly
  listPol <- poly@polygons[[1]]@Polygons
  listPol[[length(listPol)+1]] <- newHole
  punch <- Polygons(listPol,poly@polygons[[1]]@ID)
  
  # make the polygon a SpatialPolygonsDataFrame as the entry
  new <- SpatialPolygons(list(punch),proj4string=poly@proj4string)
  #new <- SpatialPolygonsDataFrame(new,data=as(poly,"data.frame"))
  
  return(new)
}






