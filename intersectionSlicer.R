
require(raster)
require(maptools)


intersectionSlicer <- function(shp,grd,verbose=TRUE){

  # shp <- s[[j]]
  # grd <- g[[j]]
  
  #   ---- Make rows out of the grid shapefile.  
  grd@data$R_ID <- seq(1,nrow(grd),1)
  grddf <- grd@data
  grddf <- grddf[order(grddf$Grid_ID),]
  
  #   ---- Find where one row ends, and the next begins.
  id.num <- c(0,diff(as.numeric(substr(grddf$Grid_ID,3,8))) > 1)
  
  #   ---- Make an identifier to pluck out different rows. 
  seq <- 1
  ugh <- data.frame(id.num=id.num)
  ugh$seq <- rep(NA,nrow(grd))
  
  for(i in 1:nrow(grd)){
    ugh[i,]$seq <- seq
    if(ugh[i,]$id.num == 1){
      seq <- seq + 1
      ugh[i,]$seq <- seq
    }
  }
  
  #   ---- Add our identifier to the data frame.  
  grddf <- cbind(grddf,ugh)
  grddf <- grddf[order(grddf$R_ID),]
  grd@data <- grddf
  
  #   ---- Print warnings immediately as they occur, i.e., so we can investigate 
  #   ---- the ith intersection.  
  options(warn=1)
  
  #   ---- Slice off one row at a time, and intersect with that.  
  grdBits <- vector("list",seq)
  shpBits <- vector("list",seq)
  for(i in 1:seq){
    if(verbose){cat(paste0("Compiling row ",i,".\n"))}
    grdBits[[i]] <- grd[grd@data$seq == i,]
    shpBits[[i]] <- intersect(grdBits[[i]],shp)
  }
  
  #   ---- Put all the shapefiles back together again.  
  source("//lar-file-srv/Data/Jason/shpHelper/combineShps.R")
  cat(paste0("Reassembling all ",i," bits...\n"))
  shpIntersect <- combineShps(shpBits)
  
  #   ---- Clean up attribute data.
  shpIntersect@data$R_ID.1 <- shpIntersect@data$R_ID.2 <- shpIntersect@data$sampleID.1 <- NULL
  names(shpIntersect@data)[names(shpIntersect@data) == "sampleID.2"] <- "sampleID"
  names(shpIntersect@data)[names(shpIntersect@data) == "Grid_ID.2"] <- "Grid_ID"
  
  #   ---- Put the warnings back to normal.  
  options(warn=0)

  if(verbose){
    cat("Done!\n")
    cat("Output each of your shp, grd, and resulting shpIntersect shapefile to a GIS to ascertain correctness.\n")
  }
  return(shpIntersect)
 
}


#ans <- intersectionSlicer(CO,grid)
#shpIntersect@data$AreaAcBit <- gArea(ans,byid=TRUE) * 0.000247105 

#writeOGR(CO,"//lar-file-srv/Data/BTPD_2016/Analysis/Overnights/Results/Overall/intersectionSlicer","CO",overwrite_layer=TRUE,driver="ESRI Shapefile")
#writeOGR(grid,"//lar-file-srv/Data/BTPD_2016/Analysis/Overnights/Results/Overall/intersectionSlicer","grid",overwrite_layer=TRUE,driver="ESRI Shapefile")
#writeOGR(shpIntersect,"//lar-file-srv/Data/BTPD_2016/Analysis/Overnights/Results/Overall/intersectionSlicer","CO_intersected",overwrite_layer=TRUE,driver="ESRI Shapefile")

