fmpFormulaBTPD2 <- function(alpha,state,recoShps,FMPGrid,fileSave,Bn,samps,tLength){

#   alpha <- 0.10
#   state <- 'CO'
#   recoShps <- CO[CO@data$sampleID <= upToThis,]
#   FMPGrid <- grid[grid@data$doneStatus == 1 & grid@data$sampleID <= upToThis,]
#   fileSave <- fileSave1
#   Bn <- 500
#   samps <- samps
#   tLength <- 2.5
 
  #   ---- Collect total number of cells and sampled number of cells.
  totalN <- samps$Grid_N
  sampled <- nrow(FMPGrid)
  
  #   ---- Define useful projections.  
  projAEAc  <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
  
  #   ---- Identify the type of 'state.'  Could be a real state, or could 
  #   ---- be BLM or the set of larger towns.  
  if(state == 'BLM'){
    gridLines <- as(readOGR(gridPath,'BTPD_BLM_Grid'),"SpatialLinesDataFrame")  
    shpTowns <- recoShps
  } else if(state %in% c('1k','5k')){
    gridLines <- as(readOGR('//lar-file-srv/Data/BTPD/Product/National','5k--BAS'),"SpatialLinesDataFrame")
    shpTowns <- recoShps
  } else { 
    gridLines <- as(FMPGrid,"SpatialLinesDataFrame")        
    shpTowns <- recoShps[substr(recoShps@data$Grid_ID,1,2) == state,]
  }
  
  #   ---- Don't know what this does currently.  5/25/2016
#   if(state %in% c('1k','5k')){
#     theIDs <- sapply(shpTowns@polygons, function(x) x@ID)
#     if(length(unique(theIDs)) != nrow(shpTowns)){print(cat('ugh stop not equal'))} # make sure these are unique.
#     shpTowns@data$R_ID <- theIDs
#     rownames(shpTowns@data) <- theIDs
#   }

  #   ---- Maybe want to adapt this to estimate acreage as a function of 
  #   ---- increasing sample size.  (Increasing BAS number.)
#   # for WY, which has a 100% sample of towns -- restrict to the BAS sample.
#   if(state == "WY"){
#     wyBAS <- readOGR('//lar-file-srv/Data/BTPD/dataBase/shapeFiles/WY_BASSamplingFiles','WY-All BAS Sample')
#     shpTowns@data$Ranker <- c(1:nrow(shpTowns@data))
#     temp <- merge(shpTowns@data,wyBAS@data[,c('Grid_ID','BASRank')],by=c('Grid_ID'),all.x=TRUE)
#     temp <- temp[order(temp$Ranker),]
#     shpTowns@data <- temp
#     shpTowns <- shpTowns[!is.na(shpTowns@data$BASRank) & shpTowns@data$BASRank <= 1760,]
#   }
  
  #   ---- Make lists of Lines objects.  These are composed of transect-length
  #   ---- units, with one list for each cardinal direction.  
  w <- vector("list",sampled)                                                                                                                           
  n <- vector("list",sampled)
  e <- vector("list",sampled)
  s <- vector("list",sampled)
  for(j in 1:sampled){
    w[[j]] <- Lines(list(Line(gridLines@lines[[j]]@Lines[[1]]@coords[1:2,])), as.character(j))
    n[[j]] <- Lines(list(Line(gridLines@lines[[j]]@Lines[[1]]@coords[2:3,])), as.character(j))
    e[[j]] <- Lines(list(Line(gridLines@lines[[j]]@Lines[[1]]@coords[3:4,])), as.character(j))
    s[[j]] <- Lines(list(Line(gridLines@lines[[j]]@Lines[[1]]@coords[4:5,])), as.character(j))
  }
  
  #   ---- Make bootstrapping objects.  
  bsData <- matrix(NA,nrow=nrow(shpTowns),ncol=4)
  bsArray <- array(NA,dim=c(nrow(bsData),ncol=Bn,4))
  NSampleArray <- array(NA,dim=c(nrow(bsData),ncol=Bn,4))
  SSampleArray <- array(NA,dim=c(nrow(bsData),ncol=Bn,4))
  NSamplej <- matrix(NA,nrow=Bn,ncol=4)
  SSamplej <- matrix(NA,nrow=Bn,ncol=4)
  SampleIj <- matrix(NA,nrow=Bn,ncol=4)

  #   ---- Make a helpful vectors and objects.
  N1HatAll <- S1HatAll <- N2HatAll <- S2HatAll <- NULL                                                                                                                                 
  dir <- c('w','n','e','s')                                                                                                                                                                                                                                                                           
  Sigma.N1 <- Sigma.S1 <- Sigma.N2 <- Sigma.S2 <- rep(NA,4)
  allSpines <- NULL
  for(g in 1:4){
    if(g == 1){the.list <- w}
    if(g == 2){the.list <- n}
    if(g == 3){the.list <- e}
    if(g == 4){the.list <- s}
    SL <- as(SpatialLines(the.list,proj4string=CRS(projAEAc)),"SpatialLinesDataFrame")
    SL@data <- data.frame(sapply(slot(SL, "lines"), function(k) slot(k, "ID")))
    colnames(SL@data) <- 'Trans_ID'
    
    #   ---- Count the number of total crossings, per polygon / town.
    #   ---- http://r-sig-geo.2731867.n2.nabble.com/split-divide-SpatialLines-sp-into-n-segments-td7583234.html
    rownames <- rownames(shpTowns@data)
    IntersectCounts <- rep(NA,length(rownames))
    for(h in 1:length(rownames)){
      Sl = as(shpTowns[rownames(shpTowns@data) == rownames[h],],"SpatialLines")#SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))),ID="a"))) 
      cSl <- coordinates(Sl) 
      cSl 
      in_nrows <- lapply(cSl, function(x) sapply(x, nrow)) 
      outn <- sapply(in_nrows, function(y) sum(y-1)) 
      res <- vector(mode="list", length=outn) 
      i <- 1 
      for (j in seq(along=cSl)) { 
        for (k in seq(along=cSl[[j]])) { 
          for (l in 1:(nrow(cSl[[j]][[k]])-1)) { 
            res[[i]] <- cSl[[j]][[k]][l:(l+1),] 
            i <- i + 1 
          } 
        } 
      }   
      res1 <- vector(mode="list", length=outn) 
      for (i in seq(along=res)){ 
        res1[[i]] <- Lines(list(Line(res[[i]])), as.character(i))
      }
      outSL <- SpatialLines(res1) 
      outSL@proj4string <- CRS(projAEAc)
      IntersectCounts[h] <- sum(gIntersects(outSL,SL,byid=TRUE))     
    }
    names(IntersectCounts) <- rownames
    IntersectCounts <- IntersectCounts[IntersectCounts > 0]
    
    #   ---- Find the number of segments totally contained in a town.  These should be very few.
    #   ---- While gIntersects count these as an intersection, they are in fact not a 
    #   ---- crossing;  so find and remove these.  
    contains <- NA
    segsContain <- gContains(shpTowns,SL,byid=TRUE)
    for(h in 1:length(rownames)){
      segsContain.h <- segsContain[,h]
      if(sum(segsContain.h) > 0){
        contains <- names(segsContain.h[segsContain.h==TRUE])
      }
    }

    theInts <- gIntersects(shpTowns,SL,byid=TRUE)   
    theIntsClean <- theInts[!(rownames(theInts) %in% contains),] 
    
    #   ---- Check if there's only one town -- results lead to a vector or matrix, and so must 
    #   ---- be set up to handle each.  
    if(class(theIntsClean) == "logical"){ 
      
      #   ---- Identify and count the intersections.  
      Ind <- sum(theIntsClean) > 0
      Intersects <- sum(theIntsClean)[Ind]
      M <- shpTowns@data$R_ID    # M needs the TOWN id, not the segment id
    } else {
      
      #   ---- Identify and count the intersections. 
      Ind <- colSums(theIntsClean) > 0
      Intersects <- colSums(theIntsClean)[Ind]
      M <- names(Intersects)
    }

    #   ---- Compute an estimate for the probability of crossing.  This is the fundamental
    #   ---- estimate of probability used in the formula.  
    if( length(M) == 0 ){
      phat1 <- phatAdj <- 0
    } else {
      phat1 <- 2*(tLength/(tLength*tLength))*(1/pi)*shpTowns@data[shpTowns@data$R_ID %in% M,]$PerimMi  #*(1/getit2$factor[shpTowns@data$R_ID %in% M])
      phatAdj <- phat1 #/ IntersectCounts
    }
    
    #   ---- Can't have a probability greater than one; do something else?
    phatAdj[phatAdj > 1] <- 1   
    phat2 <- phatAdj
    
    #   ---- Compile statistics for each town (number of crossings, probabilities, etc.) 
    #   ---- for this direction, and add in to the mix.  
    thisSpine <- compilePHats(state,shpTowns,IntersectCounts,theIntsClean,Intersects,M,samps,gridLines,phat1,phat2)
    if( !is.null(thisSpine) ){
      allSpines <- rbind(allSpines,thisSpine)
    }
  
    #   ---- Calculate the estimates for N and S in this particular direction. 
    if( sum(phat1) > 0 ){
      Sigma.N1[g] <- sum(1/phat1)
      Sigma.S1[g] <- sum(shpTowns@data[shpTowns@data$R_ID %in% M,]$AreaMi2/phat1)
    } else {
      Sigma.N1[g] <- 0
      Sigma.S1[g] <- 0
    }
    if( sum(phat2) > 0){
      Sigma.N2[g] <- sum(1/phat2)
      Sigma.S2[g] <- sum(shpTowns@data[shpTowns@data$R_ID %in% M,]$AreaMi2/phat2)
    } else {
      Sigma.N2[g] <- 0
      Sigma.S2[g] <- 0
    }
    
    #   ---- Count up the number of crossings.  
    nCross <- nrow(shpTowns@data[rownames(shpTowns@data) %in% names(Ind[Ind==TRUE]),c('Grid_ID','Town_ID')])

    #   ---- Compile statistic for this direction for each of N and S.  Expand out to the 
    #   ---- total number of cells estimated.  The set labeled "1" are for the original
    #   ---- estimates of probability, while the "2" have those phats > 1 capped at 1.
    N1Hat <- data.frame(State=state,Dir=toupper(dir[g]),cvM=NA,cvS=NA,cv=NA,Metric='N'   ,pHat="Unbndd",NTownsDigi=nrow(shpTowns),SampObs=nrow(shpTowns)              ,nCross=nCross,NCellsDigi=sampled,FMPSampEst=Sigma.N1[g]          ,NCellsGrid=totalN,Multiplier=totalN/sampled,FMPExpanded=Sigma.N1[g]*totalN/sampled)
    S1Hat <- data.frame(State=state,Dir=toupper(dir[g]),cvM=NA,cvS=NA,cv=NA,Metric='Area',pHat="Unbndd",NTownsDigi=nrow(shpTowns),SampObs=sum(shpTowns@data$AreaAcres),nCross=nCross,NCellsDigi=sampled,FMPSampEst=Sigma.S1[g]/0.0015625,NCellsGrid=totalN,Multiplier=totalN/sampled,FMPExpanded=Sigma.S1[g]*totalN/sampled/0.0015625)  
    N1HatAll <- rbind(N1HatAll,N1Hat)
    S1HatAll <- rbind(S1HatAll,S1Hat)
    
    N2Hat <- data.frame(State=state,Dir=toupper(dir[g]),cvM=NA,cvS=NA,cv=NA,Metric='N'   ,pHat="1Bndd",NTownsDigi=nrow(shpTowns),SampObs=nrow(shpTowns)              ,nCross=nCross,NCellsDigi=sampled,FMPSampEst=Sigma.N2[g]          ,NCellsGrid=totalN,Multiplier=totalN/sampled,FMPExpanded=Sigma.N2[g]*totalN/sampled)
    S2Hat <- data.frame(State=state,Dir=toupper(dir[g]),cvM=NA,cvS=NA,cv=NA,Metric='Area',pHat="1Bndd",NTownsDigi=nrow(shpTowns),SampObs=sum(shpTowns@data$AreaAcres),nCross=nCross,NCellsDigi=sampled,FMPSampEst=Sigma.S2[g]/0.0015625,NCellsGrid=totalN,Multiplier=totalN/sampled,FMPExpanded=Sigma.S2[g]*totalN/sampled/0.0015625)  
    N2HatAll <- rbind(N2HatAll,N2Hat)
    S2HatAll <- rbind(S2HatAll,S2Hat)
    
    
    #   ---- Output transect shapefiles utilized.  Helpful for in-depth investigations.
    if(state %in% c('1k','5k')){
      writeOGR(SL,fileSave,paste0('the',state,'-',toupper(dir[g])),driver="ESRI Shapefile",overwrite_layer=TRUE)
    } else {
      writeOGR(SL,fileSave,paste0(state,'-',toupper(dir[g])),driver="ESRI Shapefile",overwrite_layer=TRUE)      
    }
  }
    
  #   ---- The estimates for this round of data.  
  theEstimates <- rbind(N1HatAll,N2HatAll,S1HatAll,S2HatAll)
  theLabel <- max(recoShps@data$sampleID)
 
  #   ---- Format information on those towns that crossed a transect, and output for
  #   ---- in-depth investigations.  
  allSpines <- allSpines[order(allSpines$Grid_ID,allSpines$Town_ID,allSpines$Dir),]
  allSpines[is.na(allSpines)] <- 0
  allSpines$recipFactor <- 1/allSpines$factor
  allSpines <- allSpines[,c('ID','Grid_ID','Town_ID','Dir','Intersects','factor','recipFactor','PerimMi','phat1','IntersectCounts','phat2')]
  allSpines$check <- allSpines$recipFactor/pi*allSpines$PerimMi/allSpines$IntersectCounts
  
  write.csv(allSpines,paste0(fileSave,"/",state,"_pHats.csv"))
  write.csv(theEstimates,paste0(fileSave,"/",state,"_theEsts_",theLabel,".csv"))
  
}
#   ---- Jason turns off for now...deal with later, if at all.  5/26/2016.
#   
#   FMPBootstrapping(g)
#   
#   
#   
#     
#     NAvgD <- 1/16*(sum(theCovN))                                                                                         # get estimates of var(N)
#     SAvgD <- 1/16*(sum(theCovS))                                                                                         # get estimates of var(S)
#   
#     NAvgI <- rowMeans(NSamplej)                                                                                         # get averaged estimates of N
#     SAvgI <- rowMeans(SSamplej)                                                                                         # get averaged estimates of S
#   
#     Bmean.N <- mean(NAvgI)
#     Bmean.S <- mean(SAvgI)
#     Bsdev.N <- sqrt(NAvgD)
#     Bsdev.S <- sqrt(SAvgD)
#     Bcv.N <- round(Bsdev.N / Bmean.N * 100,2)
#     Bcv.S <- round(Bsdev.S / Bmean.S * 100,2)
#     N.cv.stuff <- c(Bmean.N,Bsdev.N,Bcv.N)
#     S.cv.stuff <- c(Bmean.S,Bsdev.S,Bcv.S)
#     cv.stuff <- data.frame(rbind(N.cv.stuff,S.cv.stuff))
#     names(cv.stuff) <- c('cvM','cvS','cv')
#       #toNowBAS <- cbind(toNowBAS,cv.stuff)
#       #rownames(toNowBAS) <- NULL
# 
# #     
# #     # plot histograms of Avg N & S over the four directions.
# #     hist(NAvgI,breaks=100,main=paste0('Bootstrap Estimates of Avg N (Avg N-hat = ',round(mean(NHatAll$Est),1),')'),xlab='Bootstrapped Avg N-hat') 
# #     par(new=T)
# #     abline(v=mean(NHatAll$Est),col='red')    
# # 
# #     hist(SAvgI,breaks=100,main=paste0('Bootstrap Estimates of Avg S (Avg S-hat = ',round(mean(SHatAll$Est),1),')'),xlab='Bootstrapped Avg S-hat') 
# #     par(new=T)
# #     abline(v=mean(SHatAll$Est),col='red')    
# #     
# # #     getSHistall <- hist(SAvgI,breaks=100,plot=FALSE) 
# # #     par(new=T)
# # #     lines(getSHistall$mids,10000*1000*dlnorm(getSHist$mids,mle.m[j],mle.s[j]),type="l",col='blue',lwd=2)
# # 
# #     
# #     # 'plot' the covariance and correlation matrices
# # #     plot(0.6,1,type='n',frame.plot=FALSE,axes=F,xlab = NA,ylab = NA,main=paste0('N-hat Covar Matrix - 1/16*Var(sum N)=',round(sqrt(NAvgD),1)))
# # #     text(0.6,1,paste(print.data.frame(capture.output(theCovN)),collapse='\n'), xaxt='n',yaxt='n',pos=1, family="mono",cex=0.6,xlab = NA, ylab = NA)
# # #     
# # #     plot(0.6,1,type='n',frame.plot=FALSE,axes=F,xlab = NA,ylab = NA,main=paste0('S-hat Covar Matrix - 1/16*Var(sum N)=',round(sqrt(SAvgD),1)))
# # #     text(0.6,1,paste(print.data.frame(capture.output(theCovS)),collapse='\n'), xaxt='n',yaxt='n',pos=1, family="mono",cex=0.6,xlab = NA, ylab = NA)
# #   
# #     plot(0.6,1,type='n',frame.plot=FALSE,axes=F,xlab = NA,ylab = NA,main='N-hat Corr Matrix')
# #     text(0.6,1,paste(print.data.frame(capture.output(theCorN)),collapse='\n'), xaxt='n',yaxt='n',pos=1, family="mono",cex=0.6,xlab = NA, ylab = NA)
# #    
# #     plot(0.6,1,type='n',frame.plot=FALSE,axes=F,xlab = NA,ylab = NA,main='S-hat Corr Matrix')
# #     text(0.6,1,paste(print.data.frame(capture.output(theCorS)),collapse='\n'), xaxt='n',yaxt='n',pos=1, family="mono",cex=0.6,xlab = NA, ylab = NA)
# #   
# #   dev.off()
# 
#     otherN <- data.frame(MID=NA,L=NA,U=NA,Median=NA,P=NA,acc=NA) #biasCorrectedBootstrap(alpha=0.10,Bn=Bn,thetaHat=mean(NHatAll$Est),metric=NAvgI,MID='All')
# #heyS <- parametricBootstrap(alpha=0.10,Bn=Bn,m=mle.mu[j],s=mle.s2[j],thetaHat=mle.mu[j],metric=SSamplej[,j],MID=toupper(dir[j]))
# 
#     otherS <- data.frame(MID=NA,L=NA,U=NA,Median=NA,P=NA,acc=NA)  #biasAcceleratedCorrectedBootstrap(alpha=0.10,Bn=Bn,thetaHat=log(mean(SHatAll$Est)),metric=log(SAvgI),MID='All',acc=acc)
#     otherS$L <- exp(otherS$L); otherS$U <- exp(otherS$U); otherS$Median <- exp(otherS$Median);
#     
#     otherN$Metric <- 'N'
#     otherS$Metric <- 'Area'
#     AllBS <- rbind(otherN,otherS)
#     AllBS$halfWidth <- (AllBS$U - AllBS$L) / 2
#     names(AllBS)[names(AllBS) == 'MID'] <- 'Dir'
#       
#     AllAvgNHat <- data.frame(Metric='N'   ,State=state,Dir='All',cvM=cv.stuff[1,1],cvS=cv.stuff[1,2],cv=cv.stuff[1,3],Est=mean(NHatAll$Est),NSampled=sampled,NCells=uniqueCells,Expand=1,Hat=mean(NHatAll$Est)*factor)
#     AllAvgSHat <- data.frame(Metric='Area',State=state,Dir='All',cvM=cv.stuff[2,1],cvS=cv.stuff[2,2],cv=cv.stuff[2,3],Est=mean(SHatAll$Est),NSampled=sampled,NCells=uniqueCells,Expand=1,Hat=mean(SHatAll$Est)*factor)  
#   
#     BSConfInt <- rbind(AllBS,dirConf)
#     BSConfInt$LHat <- ifelse(BSConfInt$Metric == 'N',BSConfInt$L*uniqueCells/sampled*factor,BSConfInt$L*uniqueCells/sampled*factor)
#     BSConfInt$UHat <- ifelse(BSConfInt$Metric == 'N',BSConfInt$U*uniqueCells/sampled*factor,BSConfInt$U*uniqueCells/sampled*factor)
#       
#     AllEsts <- rbind(NHatAll,AllAvgNHat,SHatAll,AllAvgSHat)
#     AllEsts1 <- merge(AllEsts,BSConfInt,by=c('Dir','Metric'),all.x=TRUE)
#   
#     if(state %in% c('1k','5k')){
#       writeOGR(shpTowns,shpsPath,paste0('the',state,'Towns'),driver="ESRI Shapefile",overwrite_layer=TRUE)
#     } else {
#       writeOGR(shpTowns,shpsPath,paste(state,'Towns'),driver="ESRI Shapefile",overwrite_layer=TRUE)
#     }
#     AllEsts1
#   }