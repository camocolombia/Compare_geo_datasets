

DIST_FUNCTION <- function(lon1,lat1,lon2,lat2,threshold,matrix){

  ##############################################
  # CALL LIBRARIES
  ############################################## 
  require(stringdist);require(psych);library(geosphere)
  
  ##############################################    
  #CONVERTING TO NUMERIC
  ############################################## 
  lon1  <-  as.numeric(as.character(lon1));  lat1 <- as.numeric(as.character(lat1));
  lon2  <-  as.numeric(as.character(lon2));  lat1 <- as.numeric(as.character(lat2)); 
  
  ##############################################    
  #CHOOSING AN OUTPUT
  ##############################################   

  if(matrix==TRUE |matrix==T) {
  ##############################################
  #CREATING THE FINAL MATRIX
  ############################################## 
  S  <-  as.data.frame(matrix(ncol = 2,nrow = 1))
  colnames(S)  <-  c("DIST","THRESHOLD")
  ##############################################
  #CALCULATING THE MATRIX
  ############################################## 
  DISM  <-  distm(c(lon1,lat1),c(lon2,lat2),distHaversine)/1000
  ##############################################   
  S[1,1]  <-  DISM
  
  ############################################## 
  #THRESHOLD#
  ############################################## 
  if(is.na(lon1) | is.na(lat1) | is.na(lon2) | is.na(lat2)) {
    S[1,2]  <-  NA
    } else if(DISM>threshold){
      S[1,2]  <-  0
    } else if(DISM<=threshold){
      S[1,2]  <-  1
    }  
   
  } else if(matrix==FALSE | matrix==F) {
    
    DISM  <-  distm(cbind(lon1,lat1),cbind(lon2,lat2),distHaversine)/1000
    ##############################################   
    S  <-  DISM
    
    ############################################## 
    #THRESHOLD#
    ############################################## 
    if(is.na(lon1)|is.na(lat1)|is.na(lon2)|is.na(lat2)) {
      S  <-  NA
    } else if(DISM>threshold) {
      S  <-  0
    } else if(DISM<=threshold) {
      S  <-  1
    }  
  
   }
  return(S)
  } 

#    

#csv  <-  read.csv("D:/FIX_2.csv",header=T,sep="|")
lon1  <-  csv$lon
lat1  <-  csv$lat

lon2  <-  csv$long_geor_2
lat2  <-  csv$lat_geor_2
# 
 threshold <- 100 #Km
#   
#      
#     
#   



#  x <- lapply(1:nrow(csv),function(i){
#    cat(i," of ", nrow(csv)," | ",round((i/nrow(csv)*100),2)," % ","\n")
#    x <- DIST_FUNCTION(lon1[[i]],lat1[[1]],lon2[[i]],lat2[[i]],threshold=threshold,matrix=TRUE)
#    return(x)  
#    
#  })
# #   
#  x2 <- do.call(rbind,x)
# 
# x3 <- cbind(csv,x2)
# 
# write.csv(x3,"D:/FIX_2_final.csv",quote = F,row.names = F)
