dir_analysis <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/TRAFFIC_LIGHT"
inDir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"; if(!file.exists(out_dir)){dir.create(out_dir)}
out_dir_TH <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/THRESHOLD"; if(!file.exists(out_dir_TH)){dir.create(out_dir_TH)}
file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)

####
is.integer0  <-  function(x)
{
  is.integer(x) && length(x) == 0L
}
#####
#s <- seq(from = 10, to = 2000, by = 10)
s <- seq(from = 0, to = 100, by = 10)
s2 <- seq(from = 0, to = 100, by = 10)
######################################################################################
######################################################################################
######################################################################################
######################################################################################
SCORES_FILES2 <- lapply(1:length(s),function(i){
  
  cat("reading csv file for ",s[[i]]," Km","\n")
  X <- read.csv(paste0(dir_analysis,"/","SCORES_",s[[i]],".csv"),header=T,sep="|")
  return(X)
  
});gc()
######################################################################################
######################################################################################
######################################################################################
######################################################################################
AC_ID <- SCORES_FILES2[[1]]
AC_ID <- AC_ID[,1:3]

DATASET_AVAILABLE <- as.data.frame(AC_ID)
DATASET_AVAILABLE$DATASET_AVAILABLE <- NA
  for(i in 1:nrow(AC_ID)){
    cat("ROW: ",i,"\n")
    
    
    if(file_to_fix$SOS_FLAG[[i]]==1){ #SOS_FLAG
      DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 10
      
    } else {
      
      if(file_to_fix$LOCALITY_FLAG[[i]]==1 &
         file_to_fix$SOS_FLAG[[i]]==0){ #SOS_FLAG
        
        DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 40 ###NO LOCALITY (CENTROID)
        
      } else  if(file_to_fix$IRRI_BYHAND_FLAG[[i]]==1 & #georefremarks_flag
               file_to_fix$SOS_FLAG[[i]]==0    #SOS_FLAG
      ){
        
        DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 20 ### GEOREF BY HAND
      } else  if(file_to_fix$GG_COORDS_FLAG[[i]]==1 & #GRIN_GLOBAL_FLAG
               file_to_fix$SOS_FLAG[[i]]==0  #SOS_FLAG
      ){
        
        DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 30 ### GRIN GLOBAL COORDS
        
      } else  if(file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1 & #IRRI_LOCALITY_FLAG
               file_to_fix$SOS_FLAG[[i]]==0 ){ #SOS_FLAG
      
        DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 
          sum(SCORES_FILES2[[1]]$COORD_ACCESS[[i]],
              SCORES_FILES2[[1]]$COORD_RAW[[i]],
              SCORES_FILES2[[1]]$COORD_RDGI[[i]],
              SCORES_FILES2[[1]]$COORD_HIJMANS[[i]],
              na.rm=T)
      
      } else  {
        DATASET_AVAILABLE$DATASET_AVAILABLE[[i]] <- 
      sum(SCORES_FILES2[[1]]$COORD_ACCESS[[i]],
          SCORES_FILES2[[1]]$COORD_RAW[[i]],
          SCORES_FILES2[[1]]$COORD_RDGI[[i]],
          SCORES_FILES2[[1]]$COORD_HIJMANS[[i]],
          na.rm=T)
        }
      }
    }


######################################################################################
######################################################################################

NDATA <- list()

for(i in 1:nrow(file_to_fix)){
  
  
  s3 <- as.matrix(matrix(nrow=1,ncol=4))
  #####
  if(!is.na(file_to_fix$ACCESS_final_lat[[i]])){
    s3[1,1] <- 1 
    
  } else {
    
    s3[1,1] <- 0
  }
  #####
  if(!is.na(file_to_fix$GRIN_final_lat[[i]])){
    s3[1,2] <- 1 
    
  } else {
    
    s3[1,2] <- 0
  }
  #####
  if(!is.na(file_to_fix$GEO_final_lat[[i]])){
    s3[1,3] <- 1 
    
  } else {
    
    s3[1,3] <- 0
  }
  #####
  if(!is.na(file_to_fix$HIJMANS_final_lat[[i]])|
     file_to_fix$IRRI_LOCALITY_FLAG[[i]]==0
  ){
    s3[1,4] <- 1 
    
  } else  if(is.na(file_to_fix$HIJMANS_final_lat[[i]])|
           file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1
  ){
    
    s3[1,4] <- 0
  }
  #####
  
  NDATA[[i]] <- sum(s3)
  
  ##### 
  if(file_to_fix$SOS_FLAG[[i]]==1){
    
    NDATA[[i]] <- NA 
  } else  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    NDATA[[i]] <- NA 
  }
  #####
  cat("ROW: ",i,"|","DATASET NUMBER: ", NDATA[[i]],"\n")      
}

######################################################################################
######################################################################################
THR_ACC <- as.data.frame(matrix(nrow=nrow(AC_ID),ncol=14))
colnames(THR_ACC) <- c(
  "ACID",
  "PII",
  "COORD_STATUS",
  "Km_0",
  "Km_10",
  "Km_20",
  "Km_30",
  "Km_40",
  "Km_50",
  "Km_60",
  "Km_70",
  "Km_80",
  "Km_90",
  "Km_100"
)                       

THR_ACC$ACID <- AC_ID[,1]
THR_ACC$PII <- AC_ID[,2]
THR_ACC$COORD_STATUS <- DATASET_AVAILABLE$DATASET_AVAILABLE


  
scol <- 4:ncol(THR_ACC)

for(i in 1:11){
  cat(i," COL","\n")
    THR_ACC[,scol[[i]]] <- SCORES_FILES2[[i]]$THR_ACC
    };rm(i)
######################################################################################
THR_RAW <- as.data.frame(matrix(nrow=nrow(AC_ID),ncol=14))
colnames(THR_RAW) <- c(
  "ACID",
  "PII",
  "COORD_STATUS",
  "Km_0",
  "Km_10",
  "Km_20",
  "Km_30",
  "Km_40",
  "Km_50",
  "Km_60",
  "Km_70",
  "Km_80",
  "Km_90",
  "Km_100"
)                       

THR_RAW$ACID <- AC_ID[,1]
THR_RAW$PII <- AC_ID[,2]
THR_RAW$COORD_STATUS <- DATASET_AVAILABLE$DATASET_AVAILABLE

scol <- 4:ncol(THR_RAW)

for(i in 1:11){
  cat(i," COL","\n")
  THR_RAW[,scol[[i]]] <- SCORES_FILES2[[i]]$THR_RAW
};rm(i)
######################################################################################
THR_RDGI <- as.data.frame(matrix(nrow=nrow(AC_ID),ncol=14))
colnames(THR_RDGI) <- c(
  "ACID",
  "PII",
  "COORD_STATUS",
  "Km_0",
  "Km_10",
  "Km_20",
  "Km_30",
  "Km_40",
  "Km_50",
  "Km_60",
  "Km_70",
  "Km_80",
  "Km_90",
  "Km_100"
)                       


THR_RDGI$ACID <- AC_ID[,1]
THR_RDGI$PII <- AC_ID[,2]
THR_RDGI$COORD_STATUS <- DATASET_AVAILABLE$DATASET_AVAILABLE

scol <- 4:ncol(THR_RDGI)

for(i in 1:11){
  cat(i," COL","\n")
  THR_RDGI[,scol[[i]]] <- SCORES_FILES2[[i]]$THR_RDGI
};rm(i)
######################################################################################
THR_IR <- as.data.frame(matrix(nrow=nrow(AC_ID),ncol=14))
colnames(THR_IR) <- c(
  "ACID",
  "PII",
  "COORD_STATUS",
  "Km_0",
  "Km_10",
  "Km_20",
  "Km_30",
  "Km_40",
  "Km_50",
  "Km_60",
  "Km_70",
  "Km_80",
  "Km_90",
  "Km_100"
)                       


THR_IR$ACID <- AC_ID[,1]
THR_IR$PII <- AC_ID[,2]
THR_IR$COORD_STATUS <- DATASET_AVAILABLE$DATASET_AVAILABLE

scol <- 4:ncol(THR_IR)

for(i in 1:11){
  cat(i," COL","\n")
  THR_IR[,scol[[i]]] <- SCORES_FILES2[[i]]$THR_RDGI
};rm(i)
######################################################################################

write.table(THR_ACC,paste0(out_dir_TH,"/","THR_ACC.csv"),row.names = F,sep="|")
write.table(THR_RAW,paste0(out_dir_TH,"/","THR_RAW.csv"),row.names = F,sep="|")
write.table(THR_RDGI,paste0(out_dir_TH,"/","THR_RDGI.csv"),row.names = F,sep="|")
write.table(THR_IR,paste0(out_dir_TH,"/","THR_IR.csv"),row.names = F,sep="|")


######################################################################################
######################################################################################
######################################################################################
######################################################################################


######################################################################################
THR_METRICS <- THR_ACC

#THR_METRICS_SD <- THR_ACC


for(i in 4:14){
  cat("COL: ",i,"\n")
  for(j in 1:nrow(THR_METRICS)){
    cat("COL: ",i,"|"," ROW:",j,"\n")
    #THR_METRICS[j,i] <- ((sum(THR_ACC[j,i],THR_RAW[j,i],THR_RDGI[j,i],THR_IR[j,i],na.rm=T)/NDATA[[j]])/NDATA[[j]])
    #cat("SCORE THR: ",((sum(THR_ACC[j,i],THR_RAW[j,i],THR_RDGI[j,i],THR_IR[j,i],na.rm=T)/NDATA[[j]])/NDATA[[j]]),"\n")
    xx <- ((sum(THR_ACC[j,i],THR_RAW[j,i],THR_RDGI[j,i],THR_IR[j,i],na.rm=T)/4))
    #Txx2 <- (xx/NDATA[[j]])
    #THR_METRICS[j,i] <- ((sum(THR_ACC[j,i],THR_RAW[j,i],THR_RDGI[j,i],THR_IR[j,i],na.rm=T)/NDATA[[j]]))
    THR_METRICS[j,i] <- xx
    
   # cat("SCORE THR: ",((sum(THR_ACC[j,i],THR_RAW[j,i],THR_RDGI[j,i],THR_IR[j,i],na.rm=T)/NDATA[[j]])),"\n")
    cat("SCORE THR: ",xx,"\n")
    
  }
}

#THR_METRICS2 <- THR_METRICS
#write.table(THR_METRICS,paste0(out_dir_TH,"/","THR_METRICS.csv"),row.names = F,sep="|")

THR_METRICS$NDATASET <- NA
THR_METRICS$TRAFFIC_LIGHT <- NA

THR_METRICS$NDATASET <- unlist(NDATA)



for(i in 1:nrow(THR_METRICS)){
  #SOS
  cat("ROW: ",i,"\n")
  if(DATASET_AVAILABLE$DATASET_AVAILABLE[[i]]==10){
    THR_METRICS$TRAFFIC_LIGHT[[i]] <- "PURPLE" 
  } else  if(DATASET_AVAILABLE$DATASET_AVAILABLE[[i]]==20){
    THR_METRICS$TRAFFIC_LIGHT[[i]] <- "GREEN"
    } else  if(DATASET_AVAILABLE$DATASET_AVAILABLE[[i]]==30){
      THR_METRICS$TRAFFIC_LIGHT[[i]] <- "GREEN"
    } else  if(DATASET_AVAILABLE$DATASET_AVAILABLE[[i]]==40){
      THR_METRICS$TRAFFIC_LIGHT[[i]] <- "RED"
    } else {
      # if(THR_METRICS$Km_10[[i]]>=0.5 &  THR_METRICS$Km_50[[i]]>=0.75 ){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "GREEN" 
      # } else  if(THR_METRICS$Km_10[[i]]<0.5 & THR_METRICS$Km_10[[i]]>=0.3 & THR_METRICS$Km_50[[i]]>=0.6){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW" 
      # } else  if(THR_METRICS$Km_10[[i]]<0.5 & THR_METRICS$Km_50[[i]]>=0.6){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW" 
      # } else  if(THR_METRICS$Km_50[[i]]<0.6 & THR_METRICS$Km_50[[i]]>=0.5){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW" 
      # } else  if(THR_METRICS$Km_50[[i]]<0.5){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "RED" 
      # } else  if(THR_METRICS$Km_10[[i]]==0.5 & THR_METRICS$Km_50[[i]]<=0.5){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW" 
      # } else  if(THR_METRICS$Km_10[[i]]>=0 & THR_METRICS$Km_50[[i]]<=0.5){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "RED" 
      # } else  if(THR_METRICS$Km_10[[i]]>=0.6 & THR_METRICS$Km_50[[i]]>=0.6){
      #   THR_METRICS$TRAFFIC_LIGHT[[i]] <- "GREEN" 
      # }
      if(THR_METRICS$Km_10[[i]]>=0.5){
        THR_METRICS$TRAFFIC_LIGHT[[i]] <- "GREEN" 
      } else  if(THR_METRICS$Km_10[[i]]<0.5 & THR_METRICS$Km_50[[i]]>=.5){
        THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW"
      } else  if(THR_METRICS$Km_10[[i]]<0.5 & THR_METRICS$Km_50[[i]]<0.5){
        THR_METRICS$TRAFFIC_LIGHT[[i]] <- "RED"
      } else  if(THR_METRICS$Km_10[[i]]>=0.5 & THR_METRICS$NDATASET[[i]]==2){
        THR_METRICS$TRAFFIC_LIGHT[[i]] <- "YELLOW"
      }
      
  }
}

THR_METRICS$COORD_STATUS[which(THR_METRICS$COORD_STATUS==10)] <- "SOS"
THR_METRICS$COORD_STATUS[which(THR_METRICS$COORD_STATUS==20)] <- "GEOREF BY HAND"
THR_METRICS$COORD_STATUS[which(THR_METRICS$COORD_STATUS==30)] <- "GRIN GLOBAL"
THR_METRICS$COORD_STATUS[which(THR_METRICS$COORD_STATUS==40)] <- "NO LOCALITY (CENTROID)"

write.table(THR_METRICS,paste0(inDir,"/","THR_METRICS_2.csv"),row.names = F,sep="|")
  