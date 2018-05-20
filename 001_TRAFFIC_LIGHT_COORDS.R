

source("E:/Dropbox/Dropbox/NPGS georeferencing project/SCRIPTS/THR/000_DIST_FUNCTION.R")

file_to_fix  <-  read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir  <-  "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/TRAFFIC_LIGHT"; if(!file.exists(out_dir)){dir.create(out_dir)}

#file_to_fix2  <-  file_to_fix
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
####CRITERIA 0 SOS####
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
####
is.integer0   <-   function(x)
{
  is.integer(x) && length(x) == 0L
}
#####
###DELETING SOS AND HIJMANS LOCALITIES GEOREFERENCED WITH ADM 1

for(i in 1:nrow(file_to_fix)){
  
 if(file_to_fix$SOS_FLAG[[i]]==1){
   cat(i," row","\n")
   file_to_fix$ACCESS_final_lat[[i]]  <-  NA
   file_to_fix$ACCESS_final_lon[[i]]  <-  NA
   file_to_fix$GRIN_final_lat[[i]]  <-  NA
   file_to_fix$GRIN_final_lon[[i]]  <-  NA
   file_to_fix$GEO_final_lat[[i]]  <-  NA
   file_to_fix$GEO_final_lon[[i]]  <-  NA
   file_to_fix$HIJMANS_final_lat[[i]]  <-  NA
   file_to_fix$HIJMANS_final_lon[[i]]  <-  NA
   
   
   file_to_fix$ACCESS_final_iso2[[i]]  <-  NA
   file_to_fix$GRIN_final_iso2[[i]]  <-  NA
   file_to_fix$GEO_IMPROVED_final_iso2[[i]]  <-  NA   
   file_to_fix$HIJMANS_final_iso2[[i]]  <-  NA   
   

 } else if(file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1){ #HIJ_FLAG
   cat(i," row","\n")
   
   file_to_fix$HIJMANS_final_lat[[i]]  <-  NA
   file_to_fix$HIJMANS_final_lon[[i]]  <-  NA
   file_to_fix$HIJMANS_final_iso2[[i]]  <-  NA   
   
 } else if(file_to_fix$LOCALITY_FLAG[[i]]==1){
   
   cat(i," row","\n")
   file_to_fix$ACCESS_final_lat[[i]]  <-  NA
   file_to_fix$ACCESS_final_lon[[i]]  <-  NA   
   file_to_fix$GRIN_final_lat[[i]]  <-  NA
   file_to_fix$GRIN_final_lon[[i]]  <-  NA
   file_to_fix$GEO_final_lat[[i]]  <-  NA #GEO_IMPROVED_final_lat
   file_to_fix$GEO_final_lon[[i]]  <-  NA  #GEO_IMPROVED_final_lon
   file_to_fix$HIJMANS_final_lat[[i]]  <-  NA   
   file_to_fix$HIJMANS_final_lon[[i]]  <-  NA   
 # } else {
 #   cat("skip: ",i,"\n")
 # }
   #else if(file_to_fix$georefremarks_flag[[i]]==1){
 #   
 #   
 #   file_to_fix$HIJMANS_final_lat[[i]]  <-  NA
 #   file_to_fix$HIJMANS_final_lon[[i]]  <-  NA
 #   file_to_fix$HIJMANS_final_iso2[[i]]  <-  ""    
 #   file_to_fix$HIJMANS_distance_georef[[i]]  <-  NA   
 #   
  }
};rm(i)



file_to_fix2   <-   file_to_fix

################################################################################################################################################################

NDATA  <-  list()

for(i in 1:nrow(file_to_fix)){
  
  
  s3  <-  as.matrix(matrix(nrow=1,ncol=4))
  #####
  if(!is.na(file_to_fix$ACCESS_final_lat[[i]])){
    s3[1,1]  <-  1 
    
  } else {
    
    s3[1,1]  <-  0
  }
  #####
  if(!is.na(file_to_fix$GRIN_final_lat[[i]])){
    s3[1,2]  <-  1 
    
  } else {
    
    s3[1,2]  <-  0
  }
  #####
  if(!is.na(file_to_fix$GEO_final_lat[[i]])){ #GEO_IMPROVED_final_lat
    s3[1,3]  <-  1 
    
  } else {
    
    s3[1,3]  <-  0
  }
  #####
  if(!is.na(file_to_fix$HIJMANS_final_lat[[i]])|
     file_to_fix$IRRI_LOCALITY_FLAG[[i]]==0 #HIJ_FLAG
  ){
    s3[1,4]  <-  1 
    
  } else if(is.na(file_to_fix$HIJMANS_final_lat[[i]])|
           file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1 #HIJ_FLAG
  ){
    
    s3[1,4]  <-  0
  }
  #####
  
  NDATA[[i]]  <-  sum(s3)
  
  ##### 
  if(file_to_fix$SOS_FLAG[[i]]==1){ #SOS_STATUS
    
    NDATA[[i]]  <-  NA 
  } else if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    NDATA[[i]]  <-  NA 
  }
  #####
  cat("ROW: ",i,"|","DATASET NUMBER: ", NDATA[[i]],"\n")      
}





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
########################TRYING DIFFERENT THRESHOLDS
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################


#thresholds  <-  c(10,20,30,40,50,60,70,80,90,100)

#thresholds  <-  c(110,120,130,140,150,160,170,180,190,200,210)
thresholds  <-  seq(from = 0, to = 100, by = 10)

X  <-  lapply(1:length(thresholds),function(j){
#X  <-  lapply(1:1,function(j){
COORDS_MAT  <-  as.data.frame(matrix(ncol=18,nrow=nrow(file_to_fix)))



################################################################################################################################################################
####CRITERIA 1 COORDINATES <10 Km####
################################################################################################################################################################
cat("####################################################","\n")

threshold=thresholds[[j]]

cat("CALCULATING SCORES USING THRESHOLD: ",threshold," Km","\n")

LON_ACC  <-  file_to_fix$ACCESS_final_lon
LAT_ACC  <-  file_to_fix$ACCESS_final_lat
LON_RAW  <-  file_to_fix$GRIN_final_lon
LAT_RAW  <-  file_to_fix$GRIN_final_lat
LON_RDGI  <-  file_to_fix$GEO_final_lon #GEO_IMPROVED_final_lon
LAT_RDGI  <-  file_to_fix$GEO_final_lat #GEO_IMPROVED_final_lat
LON_IR  <-  file_to_fix$HIJMANS_final_lon
LAT_IR  <-  file_to_fix$HIJMANS_final_lat


##.1.1) ACCESS DATA  Vs GRIN GLOBAL
cat("CALCULATING DISTANCES FROM ACCESS DATA AND GRIN GLOBAL",threshold," Km","\n")
THR_ACC_RAW  <-  unlist(lapply(1:nrow(file_to_fix),function(i){

X  <-  DIST_FUNCTION(LON_ACC[[i]],LAT_ACC[[i]],LON_RAW[[i]],LAT_RAW[[i]],threshold,matrix=F)  
#X  <-  ((X/NDATA[[i]])*100)
  return(X)
cat((i/nrow(file_to_fix)*100),"\n")
}));gc()
  
  
##.1.2) ACCESS DATA  Vs GEO IMPROVED DATA
cat("CALCULATING DISTANCES FROM ACCESS DATA AND GEO IMPROVED",threshold," Km","\n")
THR_ACC_RDGI  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  
  X  <-  DIST_FUNCTION(LON_ACC[[i]],LAT_ACC[[i]],LON_RDGI[[i]],LAT_RDGI[[i]],threshold,matrix=F) 
#  X  <-  ((X/NDATA[[i]])*100)
  return(X)
  cat((i/nrow(file_to_fix)*100),"\n")
}));gc()

##.1.3) ACCESS DATA  Vs HIJMANS DATA
cat("CALCULATING DISTANCES FROM ACCESS DATA AND HIJMANS DATA",threshold," Km","\n")
THR_ACC_IR  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  
  X  <-  DIST_FUNCTION(LON_ACC[[i]],LAT_ACC[[i]],LON_IR[[i]],LAT_IR[[i]],threshold,matrix=F)  
  #X  <-  ((X/NDATA[[i]])*100)
  return(X)
  cat((i/nrow(file_to_fix)*100),"\n")
}));gc()


##.1.4)  GRIN GLOBAL Vs GEO IMPROVED
cat("CALCULATING DISTANCES FROM GRIN GLOBAL DATA AND GEO IMPROVED",threshold," Km","\n")
THR_RAW_RDGI  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  
  X  <-  DIST_FUNCTION(LON_RAW[[i]],LAT_RAW[[i]],LON_RDGI[[i]],LAT_RDGI[[i]],threshold,matrix=F) 
 # X  <-  ((X/NDATA[[i]])*100)
  return(X)
  cat((i/nrow(file_to_fix)*100),"\n")
}));gc()


##.1.5 GRIN GLOBAL Vs HIJMANS DATA
cat("CALCULATING DISTANCES FROM GRIN GLOBAL DATA AND HIJMANS DATA",threshold," Km","\n")
THR_RAW_IR  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  
  X  <-  DIST_FUNCTION(LON_RAW[[i]],LAT_RAW[[i]],LON_IR[[i]],LAT_IR[[i]],threshold,matrix=F)  
  #X  <-  ((X/NDATA[[i]])*100)
  return(X)
  cat((i/nrow(file_to_fix)*100),"\n")
}));gc()


##.1.6 GEO IMPROVED Vs HIJMANS DATA
cat("CALCULATING DISTANCES GEO IMPROVED AND HIJMANS DATA",threshold," Km","\n")
THR_RDGI_IR  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  
  X  <-  DIST_FUNCTION(LON_RDGI[[i]],LAT_RDGI[[i]],LON_IR[[i]],LAT_IR[[i]],threshold,matrix=F)  
  #X  <-  ((X/NDATA[[i]])*100)
  return(X)
  cat((i/nrow(file_to_fix)*100),"\n")
}));gc()

################################################################################

#THR_ACC_RAW
#THR_ACC_RDGI
#THR_ACC_IR

#THR_RAW_RDGI
#THR_RAW_IR

#THR_RDGI_IR

################################################################################
###SCORES ACCESS # THR_ACC # 
################################################################################
cat("CALCULATING SUMS OF THRESHOLDS FOR ACCESS ",threshold," Km","\n")
THR_ACC  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  if(is.na((file_to_fix$ACCESS_final_lat[[i]]))){
    S  <-  NA
  } else {
    SSS_N  <-  c(THR_ACC_RAW[[i]],THR_ACC_RDGI[[i]],THR_ACC_IR[[i]])
    SSS_N  <-  SSS_N[!is.na(SSS_N)]
    if(length(SSS_N)==0){
    S  <-  0#*SOS[[i]]
    } else {
      S  <-  sum(THR_ACC_RAW[[i]],THR_ACC_RDGI[[i]],THR_ACC_IR[[i]],na.rm = T)/length(SSS_N)     
         }
      }
  return(S)
}));gc()

################################################################################
###SCORES GRIN GLOBAL # THR_RAW # 
################################################################################
cat("CALCULATING SUMS OF THRESHOLDS FOR GRIN GLOBAL",threshold," Km","\n")
THR_RAW  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  if(is.na((file_to_fix$GRIN_final_lat[[i]]))){
    S  <-  NA
  } else {
    SSS_N  <-  c(THR_ACC_RAW[[i]],THR_RAW_RDGI[[i]],THR_RAW_IR[[i]])
    SSS_N  <-  SSS_N[!is.na(SSS_N)]
    if(length(SSS_N)==0){
      S  <-  0
    } else {
      S  <-  sum(THR_ACC_RAW[[i]],THR_RAW_RDGI[[i]],THR_RAW_IR[[i]],na.rm = T)/length(SSS_N)     
    }
  }
  
  return(S)
}));gc()
################################################################################
###SCORES GEO IMPROVED # THR_RDGI # 
################################################################################
cat("CALCULATING SUMS OF THRESHOLDS FOR GEO IMPROVED ",threshold," Km","\n")

THR_RDGI  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  if(is.na((file_to_fix$GEO_final_lat[[i]]))){#GEO_IMPROVED_final_lat
    S  <-  NA
  } else {
    SSS_N  <-  c(THR_ACC_RDGI[[i]],THR_RAW_RDGI[[i]],THR_RDGI_IR[[i]])
    SSS_N  <-  SSS_N[!is.na(SSS_N)]
    if(length(SSS_N)==0){
      S  <-  0
    } else {
      S  <-  sum(THR_ACC_RDGI[[i]],THR_RAW_RDGI[[i]],THR_RDGI_IR[[i]],na.rm = T)/length(SSS_N)     
    }
  }
  return(S)
}));gc()

################################################################################
###SCORES HIJMANS RESULTS # THR_IR# 
################################################################################
cat("CALCULATING SUMS OF THRESHOLDS FOR HIJMANS RESULTS ",threshold," Km","\n")

THR_IR  <-  unlist(lapply(1:nrow(file_to_fix),function(i){
  if(is.na((file_to_fix$HIJMANS_final_lat[[i]]))){
    S  <-  NA
  } else {
    SSS_N  <-  c(THR_ACC_IR[[i]],THR_RAW_IR[[i]],THR_RDGI_IR[[i]])
    SSS_N  <-  SSS_N[!is.na(SSS_N)]
    if(length(SSS_N)==0){
      S  <-  0
      } else {
      S  <-  sum(THR_ACC_IR[[i]],THR_RAW_IR[[i]],THR_RDGI_IR[[i]],na.rm = T)/length(SSS_N)     
    }
    
  }
  return(S)
}));gc()
###############################################################################
#THR_ACC_RAW
#THR_ACC_RDGI
#THR_ACC_IR

#THR_RAW_RDGI
#THR_RAW_IR

#THR_RDGI_IR



###############################################################################
cat("MAKING RESULTS MATRIX FOR ",threshold," Km","\n")

COORDS_MAT[,1]  <-  file_to_fix$ACID
COORDS_MAT[,2]  <-  file_to_fix$PII
COORDS_MAT[,3]  <-  threshold
COORDS_MAT[,4]  <-  THR_ACC_RAW
COORDS_MAT[,5]  <-  THR_ACC_RDGI
COORDS_MAT[,6]  <-  THR_ACC_IR
COORDS_MAT[,7]  <-  THR_RAW_RDGI
COORDS_MAT[,8]  <-  THR_RAW_IR
COORDS_MAT[,9]  <-  THR_RDGI_IR
COORDS_MAT[,18]  <-  unlist(NDATA)
cat("THR_ACC ",threshold," Km","\n")
#COORDS_MAT[,10]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$ACCESS_final_lat[[i]]))){
  COORDS_MAT[i,10]  <-  NA
} else {
  COORDS_MAT[i,10]  <-  THR_ACC[[i]] 
  }
};rm(i)

cat("THR_GRIN ",threshold," Km","\n")
#COORDS_MAT[,11]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$GRIN_final_lat[[i]]))){
    COORDS_MAT[i,11]  <-  NA
  } else {
    COORDS_MAT[i,11]  <-  THR_RAW[[i]] 
  }
};rm(i)

cat("THR_GEO ",threshold," Km","\n")
#COORDS_MAT[,12]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$GEO_final_lat[[i]]))){ #GEO_IMPROVED_final_lat
    COORDS_MAT[i,12]  <-  NA
  } else {
    COORDS_MAT[i,12]  <-  THR_RDGI[[i]] 
  }
};rm(i)

cat("THR_IR ",threshold," Km","\n")
#COORDS_MAT[,13]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$HIJMANS_final_lat[[i]]))){
    COORDS_MAT[i,13]  <-  NA
  } else {
    COORDS_MAT[i,13]  <-  THR_IR[[i]] 
  }
};rm(i)

cat("COORD_ACCESS ",threshold," Km","\n")
#COORDS_MAT[,14]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$ACCESS_ID[[i]]))){
    COORDS_MAT[i,14]  <-  0
  } else {
    COORDS_MAT[i,14]  <-  1
  }
};rm(i)

cat("COORD_RAW ",threshold," Km","\n")
#COORDS_MAT[,15]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$GRIN_final_lat[[i]]))){
    COORDS_MAT[i,15]  <-  0
  } else {
    COORDS_MAT[i,15]  <-  1
  }
};rm(i)

cat("COORD_RDGI ",threshold," Km","\n")
#COORDS_MAT[,16]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$GEO_final_lat[[i]]))){ #GEO_IMPROVED_final_lat
    COORDS_MAT[i,16]  <-  0
  } else {
    COORDS_MAT[i,16]  <-  1
  }
};rm(i)

cat("COORD_IR ",threshold," Km","\n")
#COORDS_MAT[,17]

for(i in 1:nrow(file_to_fix)){
  if(is.na((file_to_fix$HIJMANS_final_lat[[i]]))){
    COORDS_MAT[i,17]  <-  0
  } else {
    COORDS_MAT[i,17]  <-  1
  }
};rm(i)


###############################################################################

colnames(COORDS_MAT)  <-  c(
  "ACID",
  "PII",
  "THRESHOLD",
  "THR_ACC_RAW",  
  "THR_ACC_RDGI",
  "THR_ACC_IR",
  "THR_RAW_RDGI",
  "THR_RAW_IR",  
  "THR_RDGI_IR",    
  "THR_ACC",
  "THR_RAW",
  "THR_RDGI",
  "THR_IR",
  "COORD_ACCESS",
  "COORD_RAW",  
  "COORD_RDGI",
  "COORD_HIJMANS",
  "N_DATASETS"
);gc()

cat("SAVING CSV FILE FOR THRESHOLD: ",threshold," Km","\n")

write.table(COORDS_MAT,paste0(out_dir,"/","SCORES_",threshold,".csv"),row.names = F,quote = F,sep = "|",na = "");gc()
cat("####################################################","\n")
})

