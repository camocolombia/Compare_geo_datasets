#options(java.home="C:\\Program Files\\Java\\jre1.8.0_141")#
#options(java.home="C:/Program Files (x86)/Java/jre1.8.0_171")#

#require(xlsx);#require(rJava)

file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"; if(!file.exists(out_dir)){dir.create(out_dir)}  

####THRESHOLD TL####

THR <- read.csv(paste0(out_dir,"/","THR_METRICS_2.csv"),sep="|",header=TRUE)

  ####ISO_2####

ISO <- read.csv(paste0(out_dir,"/","FILE_ISO2_1.csv"),sep="|",header=TRUE)


####UNCERTAINTIES####
UNC_MIN <- read.csv(paste0(out_dir,"/","UNCERTAINTIES_MIN.csv"),sep="|",header=TRUE)

UNC_MAX <- read.csv(paste0(out_dir,"/","UNCERTAINTIES_MAX.csv"),sep="|",header=TRUE)

####################

AC_ID <- as.data.frame(file_to_fix[,1:3])
AC_ID$THR <- as.character(THR$TRAFFIC_LIGHT)
AC_ID$UNC <- as.character(UNC_MAX$TRAFFIC_LIGHT)
AC_ID$ISO <- as.character(ISO$ISO2_COMPARISON)
AC_ID$SCOORD <- as.character(UNC_MIN$SUGGESTED.COORD)
AC_ID$TRAFFIC_LIGHT <- NA
AC_ID$COMMENTS <- NA

####################




for(i in 1:nrow(file_to_fix)){
  cat("ROW" ,i,"\n")
  
  
  if(file_to_fix$SOS_FLAG[[i]]==1){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "PURPLE"
    AC_ID$COMMENTS[[i]] <- "SOS ACCESION (EXCLUDED)"
  } else {
  #FALTA#

  ###GEOREFERENCED BY HAND AVAILABLE###

  if(file_to_fix$IRRI_BYHAND_FLAG[[i]]==1 &
     file_to_fix$IRRI_LOCALITY_FLAG[[i]]==0 
     ){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "DARK GREEN"
    AC_ID$COMMENTS[[i]] <- "ACCESION GEOREFERENCED BY HAND" 
    
  } else  if(file_to_fix$IRRI_BYHAND_FLAG[[i]]==1 & file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1){
  AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
  AC_ID$COMMENTS[[i]] <- "NOT LOCALITY GEOREFERENCED BY HAND" 
} else  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
  AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
  AC_ID$COMMENTS[[i]] <- "NOT LOCALITY" 
  } else {
  
         
   XX <- cbind(AC_ID$THR[[i]],AC_ID$UNC[[i]]) 
   
  if(XX[1]=="RED" & XX[2]=="RED"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
  AC_ID$COMMENTS[[i]] <- "BOTH TRAFFIC LIGHTS WERE RED"
  } else  if(XX[1]=="RED" & XX[2]=="YELLOW"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
    AC_ID$COMMENTS[[i]] <- "CHECK THRESHOLDS"
  } else  if(XX[1]=="RED" & XX[2]=="GREEN"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
    AC_ID$COMMENTS[[i]] <- "CHECK THRESHOLDS"
  } else if(XX[1]=="YELLOW" & XX[2]=="RED"){
      AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
    AC_ID$COMMENTS[[i]] <- "CHECK UNCERTAINTY"
  } else  if(XX[1]=="YELLOW" & XX[2]=="YELLOW"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "YELLOW"
    AC_ID$COMMENTS[[i]] <- "CHECK BOTH TRAFFIC LIGHTS (YELLOW)"
  } else  if(XX[1]=="YELLOW" & XX[2]=="GREEN"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "YELLOW"
    AC_ID$COMMENTS[[i]] <- "CHECK THRESHOLDS (YELLOW)"
  } else if(XX[1]=="GREEN" & XX[2]=="RED"){
     AC_ID$TRAFFIC_LIGHT[[i]] <- "RED"
    AC_ID$COMMENTS[[i]] <- "CHECK UNCERTAINTY"
  } else  if(XX[1]=="GREEN" & XX[2]=="YELLOW"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "YELLOW"
    AC_ID$COMMENTS[[i]] <- "CHECK THRESHOLDS (YELLOW)"
  } else  if(XX[1]=="GREEN" & XX[2]=="GREEN"){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "GREEN"
    AC_ID$COMMENTS[[i]] <- "PERFECT MATCH"
  } else if(XX[1]=="GREEN" & XX[2]=="BLUE"){
     AC_ID$TRAFFIC_LIGHT[[i]] <- "BLUE"
     AC_ID$COMMENTS[[i]] <- "INFORMATION FOUND IN GRIN GLOBAL (GOOD FIT IN THRESHOLD, CHECK WITH ISO FLAG)"
   } else if(XX[1]=="GREEN" & XX[2]=="DARK GREEN"){
     AC_ID$TRAFFIC_LIGHT[[i]] <- "DARK GREEN"
     AC_ID$COMMENTS[[i]] <- "ACCESION GEOREFERENCED BY HAND (GOOD THRESHOLD PERFORMANCE)"
   } else  if(XX[1]=="RED" & XX[2]=="DARK GREEN"){
     AC_ID$TRAFFIC_LIGHT[[i]] <- "DARK GREEN"
     AC_ID$COMMENTS[[i]] <- "ACCESION GEOREFERENCED BY HAND (BAD THRESHOLD PERFORMANCE)"
   } else  if(XX[1]=="RED" & XX[2]=="BLUE"){
     AC_ID$TRAFFIC_LIGHT[[i]] <- "BLUE"
     AC_ID$COMMENTS[[i]] <- "INFORMATION FOUND IN GRIN GLOBAL (CHECK WITH ISO FLAG AND THRESHOLD)"
   }
   
}
 
     
  
  ###GRIN GLOBAL COORDS AVAILABLE###
  
  if(file_to_fix$GG_COORDS_FLAG[[i]]==1 & file_to_fix$LOCALITY_FLAG[[i]]==0){
    AC_ID$TRAFFIC_LIGHT[[i]] <- "BLUE"
    AC_ID$COMMENTS[[i]] <- "INFORMATION FOUND IN GRIN GLOBAL (CHECK WITH ISO FLAG)"
    }
  }  
  
  if(AC_ID$SCOORD[[i]]=="UPLOADED IN GRIN GLOBAL"){
    AC_ID$COMMENTS[[i]] <- "PLEASE USE ISO TRAFFIC LIGHT TO CONFIRM"
  }
  
  if(AC_ID$SCOORD[[i]]=="NO LOCALITY"){
    AC_ID$COMMENTS[[i]] <- "NO LOCALITIES IN THE DATASETS"
  }
  if(AC_ID$SCOORD[[i]]=="CENTROID"){
    AC_ID$COMMENTS[[i]] <- "A CENTROID WAS GEOREFERENCED, EXCLUIDING"
  }
  if(AC_ID$SCOORD[[i]]=="NO SOS"){
    AC_ID$COMMENTS[[i]] <- "SOS PROJECT ACCESSION"
  }
  if(AC_ID$SCOORD[[i]]=="GEOREF_BY_HAND"){
    AC_ID$COMMENTS[[i]] <- "ACCESSION GEOREFERENCED BY HAND (USING IRRI FINAL COORDS)"
  } 
  if(AC_ID$SCOORD[[i]]=="NO COORD SUGGESTED"){
    AC_ID$COMMENTS[[i]] <- "NO COORD SUGGESTED"
  } 
  
  

  
} 


for(i in 1:nrow(AC_ID)){
  cat("SECOND ROUND | ROW:" ,i,"\n")
  
if(AC_ID$THR[[i]]=="DARK GREEN" & AC_ID$UNC[[i]]=="DARK GREEN"){
  AC_ID$TRAFFIC_LIGHT[[i]] <- "DARK GREEN" 
  AC_ID$SCOORD[[i]]<- "GEOREF BY HAND"
  AC_ID$COMMENTS[[i]] <- "NO COORD SUGGESTED"
} 
  if(AC_ID$THR[[i]]=="BLUE" & AC_ID$UNC[[i]]=="BLUE"){
  AC_ID$TRAFFIC_LIGHT[[i]] <- "BLUE"  
  AC_ID$SCOORD[[i]] <- "UPLOADED IN GRIN GLOBAL"
  AC_ID$COMMENTS[[i]] <- "NO COORD SUGGESTED"
  }
}

for(i in 1:nrow(AC_ID)){
#  cat("THIRD ROUND | ROW:" ,i,"\n")
  if(AC_ID$THR[[i]]=="PURPLE" & AC_ID$UNC[[i]]=="BLUE"){
    cat("Applied to: ",i,"\n")
    AC_ID$TRAFFIC_LIGHT[[i]] <- "PURPLE"
    AC_ID$SCOORD[[i]] <- "SOS RECORD UPLOADED IN GRIN GLOBAL"
    AC_ID$COMMENTS[[i]] <- "SOS UPLOADED IN GRIN GLOBAL (NO COORD SUGGESTED)"
  }else{
    cat("skip: ",i,"\n")
  }
}
# 
#   
#   
#   
# }
final_out <- merge(file_to_fix,AC_ID,by ="ACID")
final_out <- final_out[,-c(59,60)]

#tapply(final_out$TRAFFIC_LIGHT.y,final_out$TRAFFIC_LIGHT.y,length)
#i <- 28768
out_dir_outcome <- paste0(out_dir,"/","OUTCOME"); if(!file.exists(out_dir_outcome)){dir.create(out_dir_outcome)}  
write.table(final_out,paste0(out_dir_outcome,"/","COORDS_FINAL",".csv"),sep="|",row.names=F,na="");gc()


      
#write.xlsx2(final_out,paste0(out_dir,"/","OUTCOME","/","COORDS_FINAL",".xlsx"),row.names=F,showNA = F);gc()

write.table(AC_ID,paste0(out_dir_outcome,"/","COORDS_FINAL_IDs",".csv"),sep="|",row.names=F,na="");gc()

