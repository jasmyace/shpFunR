

require(xlsx)                 

hourStem <- "C:/Users/jmitchell/Desktop/Billable Hour Reports"
files <- dir(hourStem)
files <- files[nchar(files) > 10]
files <- files[files != "EmpIDLookUp.csv"]

df <- vector("list",length(files))
alldf <- NULL
for(i in 1:length(files)){
  df[[i]] <- read.xlsx(paste0(hourStem,"/",files[i]),sheetIndex=1,startRow=6)  
  if(names(df[[i]][2]) != "Employee" & names(df[[i]][2]) != "Senior.Manager"){
    df[[i]] <- read.xlsx(paste0(hourStem,"/",files[i]),sheetIndex=1,startRow=5)  
  }
  lastCol <- which(names(df[[i]]) == "Total.Non.Billable.Hours")
  if(!("Location" %in% names(df[[i]]))){
    df[[i]] <- data.frame(df[[i]][,c(1:lastCol)],Location=NA,"Employee Type"=NA)
  }
  if(names(df[[i]][1]) == "NA."){
    names(df[[i]])[names(df[[i]]) == "NA."] <- "Emp.ID"
  }
  if("Emp.ID" %in% names(df[[i]])){
    df[[i]]$Emp.ID <- NULL
  }
  fileDate <- strsplit(substr(files[i],24,nchar(files[i])-5)," ")[[1]]
  df[[i]]$start <- rep(as.POSIXlt(as.Date(paste0(fileDate[[1]]," 1, ",fileDate[[2]]),"%B %d, %Y"),"%d-%m-%Y"),nrow(df[[i]]))

  df[[i]]$end <- df[[i]]$start
  df[[i]]$end$mday <- df[[i]]$end$mday - 1

  alldf <- rbind(alldf,df[[i]])
  fileDate <- NULL
  cat(paste0("The i = ",i," run is complete.\n"))
}

#   ---- Remove these NAs.  Should probably investigate these at some point.  
alldf <- alldf[!is.na(alldf$Employee),]

#   ---- Four prefixes are possible:  *, ^, *^, and ^*.  Make variables to capture this information.
alldf$Employee <- as.character(droplevels(alldf$Employee))
alldf$Senior.Manager <- as.character(droplevels(alldf$Senior.Manager))
alldf$partTime <- alldf$statChange <- 0
for(i in 1:nrow(alldf)){
  if(substr(alldf[i,]$Employee,1,2) %in% c("*^","^*")){
    alldf[i,]$partTime <- 1
    alldf[i,]$statChange <- 1
    alldf[i,]$Employee <- substr(alldf[i,]$Employee,3,nchar(alldf[i,]$Employee))
  } else if(substr(alldf[i,]$Employee,1,1) %in% c("*")){
    alldf[i,]$partTime <- 1
    alldf[i,]$Employee <- substr(alldf[i,]$Employee,2,nchar(alldf[i,]$Employee))
  } else if(substr(alldf[i,]$Employee,1,1) %in% c("^")){
    alldf[i,]$statChange <- 1
    alldf[i,]$Employee <- substr(alldf[i,]$Employee,2,nchar(alldf[i,]$Employee))
  }
}

#   ---- These rows are blank.  
alldf <- alldf[  alldf$Employee != "Former Staff",]

#   ---- Pull out the summary data.  These are creating problems.  Save them for later maybe.    
totdf <- alldf[  alldf$Employee %in% c("Billable Current Staff Totals","Billable Staff Totals","Former Staff Totals","All Staff Totals"),]
alldf <- alldf[!(alldf$Employee %in% c("Billable Current Staff Totals","Billable Staff Totals","Former Staff Totals","All Staff Totals")),]

#   ---- Clean up the class for each variable.  While this is better dealt with in the reading of read.xlsx,
#   ---- the variable structure of the incoming Excel sheets makes that annoying.  So clean up here. 
alldf$EmpFirst <- unlist(strsplit(alldf$Employee,", "))[c(FALSE,TRUE)]
alldf$EmpLast <- unlist(strsplit(alldf$Employee,", "))[c(TRUE,FALSE)]
alldf$SMFirst <- unlist(strsplit(alldf$Senior.Manager,", "))[c(FALSE,TRUE)]
alldf$SMLast <- unlist(strsplit(alldf$Senior.Manager,", "))[c(TRUE,FALSE)]
alldf$Dates <- as.character(droplevels(alldf$Dates))
alldf$TotHrsWorked <- as.numeric(alldf$Total.Hours.Worked)
alldf$TargHrs <- as.numeric(alldf$Target.Hours)
alldf$HrsOverUnder <- as.numeric(alldf$Hours.Over..Under.)
alldf$BillHrsWorked <- as.numeric(alldf$Billable.Hours.Worked)
alldf$TargBillHrs <- as.numeric(alldf$Target.Billable.Hours)
alldf$HrsOverUnderTargBill <- as.numeric(alldf$Hrs.Over..Under..Target.Billable)
alldf$TargRatio <- as.numeric(alldf$Target.Ratio)
alldf$BillHrsTargHrs <- as.numeric(alldf$Billable.Hours...Target.Hours)
alldf$Sick <- as.numeric(alldf$Sick)
alldf$Vacation <- as.numeric(alldf$Vacation)
alldf$Holiday <- as.numeric(alldf$Holiday)
alldf$Mrkting <- as.numeric(alldf$Mrkting)
alldf$Proposals <- as.numeric(alldf$Proposals)
alldf$Research <- as.numeric(alldf$Research)
alldf$OtherOver <- as.numeric(alldf$Other.Overhead)
alldf$X998Hrs <- as.numeric(alldf$X998.Hours)
alldf$TotNonBillHrs <- as.numeric(alldf$Total.Non.Billable.Hours)
names(alldf)[names(alldf) == "Employee.Type"] <- "EmpType"

#   ---- Read in the listing of EmpIDs and get those in.  This ensure that people like Kristen Nasman are captured 
#   ---- throughout their entire temporal history.  
EmpID <- read.csv(paste0(hourStem,"/","EmpIDLookUp.csv"))
EmpID <- EmpID[,c("EmpID","Employee")]

#   ---- Bring in a unique identifier and format it for consistency.  
alldf <- merge(alldf,EmpID,by=c("Employee"),all.x=TRUE)
alldf$EmpID <- toupper(alldf$EmpID)
alldf$Dates <- toupper(alldf$Dates)

#   ---- Remove columns no longer needed.  
alldf <- alldf[,!(names(alldf) %in% c("Employee","Senior.Manager","Total.Hours.Worked","Target.Hours","Hours.Over..Under.","Billable.Hours.Worked","Target.Billable.Hours","Hrs.Over..Under..Target.Billable","Target.Ratio","Billable.Hours...Target.Hours","Other.Overhead","Total.Non.Billable.Hours","X998.Hours"))]

#   ---- Arrange the columns to be in a nice order.
alldf <- alldf[,c("EmpID","Dates","SMFirst","SMLast","EmpFirst","EmpLast","start","end","statChange","partTime","TargRatio","TotHrsWorked","TargHrs","HrsOverUnder","BillHrsWorked","TargBillHrs","HrsOverUnderTargBill","BillHrsTargHrs","Sick","Vacation","Holiday","Mrkting","Proposals","Research","X998Hrs","TotNonBillHrs")]

alldf <- alldf[order(alldf$EmpID,alldf$Dates,alldf$start),]



E <- unique(alldf$EmpID)
N.E <- length(E)

for(i in 1:N.E){
  
  e <- E[i]
  e.df <- alldf[alldf$EmpID == e,]
  D <- unique(e.df$Dates)
  N.D <- length(D)
  
  png(paste0(hourStem,"/Radial Plots/",e,"radialPlot.png"),res=400,width=11,height=8.5,units="in")
  
  par(mfrow=c(3,1))
  for(j in 1:N.D){
    
    d <- D[j]
    d.df <- e.df[e.df$Dates == d,]
    
    if(nrow(d.df) >= 4){
      mindate <- min(d.df$start)
      x <- mindate$mon + 1 + 12 * (d.df$start$year - mindate$year) + (d.df$start$mon - mindate$mon)
      y <- 100*as.numeric(d.df$BillHrsTargHrs)
      
      p <- prettyCurve(x=x,y=y)
      
      makeRadialPlot(r=100,k=1)
      plot(p[[2]],add=TRUE)
      plot(p[[1]],add=TRUE,pch=19)
    }
  }
  par(mfrow=c(1,1))
  dev.off()
}


# totdf <- totdf[order(totdf$Employee),]
# write.csv(totdf,"C:/Users/jmitchell/Desktop/totdf.csv",row.names=FALSE)
#write.csv(alldf,"C:/Users/jmitchell/Desktop/alldf.csv",row.names=FALSE)




jason <- alldf[alldf$Employee == "Mitchell, Jason",]

jason <- jason[order(jason$end),]
jasonM <- jason[jason$Dates == "YTD",]

#x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
#y <- c(10,20,30,30,35,50,80,90,85,80,90,100,90,80,90,90,80,80)

mindate <- min(jasonM$start)
x <- mindate$mon + 1 + 12 * (jasonM$start$year - mindate$year) + (jasonM$start$mon - mindate$mon)
y <- 100*as.numeric(jasonM$Billable.Hours...Target.Hours)


jasonMt <- prettyCurve(x=x,y=y)

makeRadialPlot(r=100,k=1)
plot(jasonMt[[2]],add=TRUE)
plot(jasonMt[[1]],add=TRUE,pch=19)




