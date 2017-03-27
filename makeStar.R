#' @export
#' 
#' @description
#' 
#' @param rin The inner radius size length.  
#' 
#' @param rout The outer radius size length.
#' 
#' @param class The type of \code{Spatial*DataFrame} object desired.  Setting
#'   \code{class='p'} leads to a \code{SpatialPointsDataFrame}, and
#'   \code{class='l'} leads to a \code{SpatialLinesDataFrame}. Finally,
#'   \code{class='q'} initiates a \code{SpatialPolygonDataFrame}.
#'   
#' @param id A set of \code{id} values to be set to the \code{id} value of the
#'   underlying resulting \code{Spatial*} object.  Default is \code{NULL}.
#' 
#' @param origin The origin of all spatial coordinates in a numeric vector if
#'   length 2. Depending on the application, this may not matter.  Default is
#'   \code{c(0,0)}.
#' 
#' @param n The number of outer points in the resulting star.  For example, the 
#' traditional 6-pointed Star-of-David has 6 points, obtained when \code{n=6}.
#' 
#' @param angle1
#' 
#' @param angle2
#' 
#' @details
#' 
#' @author Jason Mitchell jasmyace@gmail.com
#' 


makeStar <- function(p=5,q=2,r=5,class="q",id=NULL,origin=c(0,0),angle1=0,angle2=2*pi){
  
#     p <- 7
#     q <- 2
#     r <- 5
#     class <- "q"
#     id <- NULL
#     origin <- c(0,0)
#     angle1 <- 0
#     angle2 <- 2*pi
  
  #n <- p + 1
  
  #   ---- Define the outer points of the star.  We add one more point so that it closes,
  #   ---- which is the default behavior of the makeCircle function.  
  oCircle <- makeCircle(r=r,class="p",origin=origin,n=p+1,angle1=angle1,angle2=angle2)
 
  #   ---- Make the inner circle of points.  Have to be careful with these, because they 
  #   ---- have to be a certain distance from the origin to line up correctly.  
  theta <- pi / p #pi*(p - 2*q) / p #pi/p
  #R <- r*cos(2*theta) / cos(theta)
  #int <- theta + 2*theta*seq(0,p-1,1)
  #i <- cbind(R*cos(int),R*sin(int))
  #i <- rbind(i,i[1,])
  #iCircle <- SpatialPoints(i)
  
  
  
  theta <- pi / p 
  R <- r*sin(pi/2 * (p-2*q)/p) / sin(pi - pi/p - pi/2*(p-2*q)/p)
  int <- theta + 2*theta*seq(0,p-1,1)
  i <- cbind(R*cos(int),R*sin(int))
  i <- rbind(i,i[1,])
  iCircle <- SpatialPoints(i)
  
  
  #   ---- Build up the coordinates by alternating the points in the two circles.  
  #   ---- Get rid of last row since only need one to close the loop. 
  imat <- iCircle@coords
  omat <- oCircle@coords
  mat <- rbind(cbind(omat,seq(1,p+1),rep(1,p+1)),cbind(imat,seq(1,p+1),rep(2,p+1)))
  mat <- mat[order(mat[,3],mat[,4]),]
  mat <- mat[-(2*(p+1)),]
  mat[(2*(p+1)) - 1,] <- mat[1,]
  coords <- mat[,1:2]

  df <- data.frame(X=1,row.names="1")
  

#   #   ---- Build up the coordinates by alternating the points in the two circles.  
#   #   ---- Get rid of last row since only need one to close the loop. 
#   
#   if( p %% q == 0 ){
#     
#     #   ---- Need to be able to reproduce the Star of David. 
#     
#   } else {
#     
#     #   ---- We have a traditional star not composed of individual triangles, or 
#     #   ---- other such shapes.  We first build a sequence of the points of the 
#     #   ---- circle that we need to hit.  For example, the regular 5-pointed 
#     #   ---- star with Schlafi symbol |5/2| starts at point 1, and then travels
#     #   ---- to 3, 5, 2, 4, 0.  
#     
#     #   ---- Start the sequence at 1.
#     vec <- c(1)
# 
#     #   ---- Build up the sequence we want, being smart to stop 
#     #   ---- when we get back to one; i.e., the points at which we started.  Note
#     #   ---- that because we are basically building with respect to mod (n), we 
#     #   ---- take advantage of the fact that 0 = n mod (n).  
#     repeat{
#       if( vec[length(vec)] %% p == 1 & length(vec) != 1){
#         break
#       } else {      
#         vec <- c(vec,(vec[length(vec)] + q) %% p)
#       }
#     }
#     vec[vec == 0] <- p
#     
#     #   ---- Build a new coordinate matrix, putting the points to draw in the 
#     #   ---- order we need them.  The vec vector tells us this necessary order.
#     coords <- NULL
#     for(i in 1:length(vec)){
#       if(i == 1){
#         coords <- c@coords[vec[1],]
#       } else {
#         coords <- rbind(coords,c@coords[vec[i],])
#       }
#     }
#   } 
  
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
