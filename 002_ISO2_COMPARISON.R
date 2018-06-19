
source("E:/Dropbox/Dropbox/NPGS georeferencing project/SCRIPTS/THR/000_DIST_FUNCTION.R")

file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS2.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"; if(!file.exists(out_dir)){dir.create(out_dir)}

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
####CRITERIA 0 SOS####
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

###FLAG



###DELETING SOS AND HIJMANS LOCALITIES GEOREFERENCED WITH ADM 1

for(i in 1:nrow(file_to_fix)){
  cat(i," ROW/NAs ACCESS","\n")
  
  
  if(is.na(file_to_fix$ACCESS_final_lat[[i]])){
    
    file_to_fix$ACCESS_final_iso2[[i]] <- ""
    
  }
  
  
  cat(i," ROW/NAs GRIN GLOBAL","\n")
  if(is.na(file_to_fix$GRIN_final_lat[[i]])){
    
    file_to_fix$GRIN_final_iso2[[i]] <- ""
    
  }
  
  cat(i," ROW/NAs GEO IMPROVED","\n")
  if(is.na(file_to_fix$GEO_final_lat[[i]])){ #GEO_IMPROVED_final_lat
    
    file_to_fix$GEO_IMPROVED_final_iso2[[i]] <- ""
    
  }
  
  cat(i," ROW/NAs HIJMANS","\n")
  
  if(is.na(file_to_fix$HIJMANS_final_lat[[i]])){
    
    file_to_fix$HIJMANS_final_iso2[[i]] <- ""
  }
  
  
  cat(i," ROW/NAs HIJMANS ISO2","\n")
  
  if(is.na(file_to_fix$HIJMANS_final_iso2[[i]])){
    file_to_fix$HIJMANS_final_iso2[[i]] <- ""
    
  }
  
  cat(i," ROW\ NOT AVAILABLE ISO2 HIJMANS","\n")
  
  if(file_to_fix$HIJMANS_final_iso2[[i]]==0){
    file_to_fix$HIJMANS_final_iso2[[i]] <- ""
    
  }
  
  cat(i," ROW/SOS RECORDS","\n")
  if(file_to_fix$SOS_FLAG[[i]]==1){ #SOS_STATUS
    cat(i," row","\n")
    file_to_fix$ACCESS_final_lat[[i]] <- NA
    file_to_fix$ACCESS_final_lon[[i]] <- NA
    file_to_fix$GRIN_final_lat[[i]] <- NA
    file_to_fix$GRIN_final_lon[[i]] <- NA
    
    file_to_fix$GEO_final_lat[[i]] <- NA #GEO_IMPROVED_final_lat
    file_to_fix$GEO_final_lon[[i]] <- NA #GEO_IMPROVED_final_lon
    file_to_fix$HIJMANS_final_lat[[i]] <- NA
    file_to_fix$HIJMANS_final_lon[[i]] <- NA
    
    file_to_fix$ACCESS_distance_georef[[i]] <- NA    
    file_to_fix$GRIN_distance_georef[[i]] <- NA
    file_to_fix$GEO_IMPROVED_distance_georef[[i]] <- NA
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
    file_to_fix$ACCESS_final_iso2[[i]] <- "" 
    file_to_fix$GRIN_final_iso2[[i]] <- "" 
    file_to_fix$GEO_IMPROVED_final_iso2[[i]] <- "" 
    file_to_fix$HIJMANS_final_iso2[[i]] <- ""   
    
  }
  
  
  
  cat(i," ROW/HIJMANS FLAG","\n")
  
  
  if(file_to_fix$IRRI_LOCALITY.FLAG[[i]]==1){ #HIJ_FLAG
    
    file_to_fix$HIJMANS_final_lat[[i]] <- NA
    file_to_fix$HIJMANS_final_lon[[i]] <- NA
    file_to_fix$HIJMANS_final_iso2[[i]] <- "" 
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
    file_to_fix$GRIN_final_iso2[[i]] <- file_to_fix$GRIN_final_iso2[[i]]
    file_to_fix$ACCESS_final_iso2[[i]] <- file_to_fix$ACCESS_final_iso2[[i]]
    file_to_fix$GEO_final_lon[[i]] <- file_to_fix$GEO_final_lon[[i]] #GEO_IMPROVED_final_lon
    
  }  
  
  
  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    
    cat(i," row/ no localities","\n")
    file_to_fix$ACCESS_final_lat[[i]] <- NA
    file_to_fix$ACCESS_final_lon[[i]] <- NA
    file_to_fix$GRIN_final_lat[[i]] <- NA
    file_to_fix$GRIN_final_lon[[i]] <- NA
    
    file_to_fix$GRIN_final_lat[[i]] <- NA #GEO_IMPROVED_final_lat
    file_to_fix$GEO_final_lon[[i]] <- NA #GEO_IMPROVED_final_lon
    file_to_fix$HIJMANS_final_lat[[i]] <- NA
    file_to_fix$HIJMANS_final_lon[[i]] <- NA
    
    file_to_fix$ACCESS_distance_georef[[i]] <- NA    
    file_to_fix$GRIN_distance_georef[[i]] <- NA
    file_to_fix$GEO_IMPROVED_distance_georef[[i]] <- NA
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
    file_to_fix$ACCESS_final_iso2[[i]] <- "" 
    file_to_fix$GRIN_final_iso2[[i]] <- "" 
    file_to_fix$GEO_IMPROVED_final_iso2[[i]] <- "" 
    file_to_fix$HIJMANS_final_iso2[[i]] <- ""   
  }    
  
};rm(i)  

file_to_fix2  <-  file_to_fix
###################################################################################
####ISO2 COMPARISON###############################################################
###################################################################################

ISO2_COMPARISON <- as.matrix(lapply(1:nrow(file_to_fix),function(i){
  
  cat(" ROW/ISO2 COMPARISON ",i,"\n")
  
  xx <- as.data.frame(table(cbind(as.character(file_to_fix$ACCESS_final_iso2[[i]]),
                                as.character(file_to_fix$GRIN_final_iso2[[i]]),
                                as.character(file_to_fix$GEO_IMPROVED_final_iso2[[i]]),
                                as.character(file_to_fix$HIJMANS_final_iso2[[i]]))))
  
  
  
  xx2 <- subset(xx,xx$Var1==xx$Var1[which(xx$Freq==(max(xx$Freq)))])
  xx3 <- xx3 <- subset(xx,xx$Var1!="")
  
  ## majority frequency
  if(nrow(xx2)==1){
    if(xx2$Var1!=""){
      if(xx2$Freq==1){
        S1 <- "RED"
      } else  if(xx2$Freq==2){
        S1 <- "YELLOW"
      } else  if(xx2$Freq>2){
        S1 <- "GREEN"
      }
    } else {
      if(xx2$Freq[xx2$Var1==""]==4){
        S1 <- "RED"
      } else  if(xx2$Freq[xx2$Var1==""]==3){
        
        S1 <- "RED"
      } else  if(xx2$Freq[xx2$Var1==""]==2){
        S1 <- "YELLOW"
      } else  if(xx2$Freq[xx2$Var1==""]==1){
        S1 <- "GREEN"
      }
      
    }
  } else  if(nrow(xx2)==2){
    ## no iso2 
    if(any(xx2$Var1=="")){
      
      if(nrow(xx3)==1){
        
        if(xx$Freq==1){
          S1 <- "YELLOW"
        } else  if(xx$Freq==2){
          S1 <- "YELLOW"
        } else if(xx$Freq>2){
          S1 <- "YELLOW"
        } else  if(xx$Freq>=3){
          S1 <- "GREEN"
        }
      }
    } else  if(xx2$Freq[1]==xx2$Freq[2]){
      S1 <- "YELLOW" 
    }  else {
      if(xx2$Freq[xx2$Var1==""]==4){
        S1 <- "RED"
      } else  if(xx2$Freq[xx2$Var1==""]==3){
        
        S1 <- "RED"
      } else  if(xx2$Freq[xx2$Var1==""]==2){
        S1 <- "YELLOW"
      }
    }
  } else  if(nrow(xx2)>=3){
    S1 <- "RED"
    
  }
  
  
  return(S1)
})
)


file_2 <- as.data.frame(cbind(file_to_fix$DUMMY,file_to_fix$ACID,
                            as.character(file_to_fix$PII),
                            as.character(file_to_fix$ACCESS_final_iso2),
                            as.character(file_to_fix$GRIN_final_iso2),
                            as.character(file_to_fix$GEO_IMPROVED_final_iso2),
                            as.character(file_to_fix$HIJMANS_final_iso2)
))
colnames(file_2) <- c("DUMMY",
                    "ACID",
                    "PII",
                    "ACCESS_final_iso2",
                    "GRIN_final_iso2",
                    "GEO_IMPROVED_final_iso2",
                    "HIJMANS_final_iso2"
)
file_2$ISO2_COMPARISON <- NA

for(i in 1:nrow(file_2)){
  cat(i, " ROW","\n")
  file_2$ISO2_COMPARISON[[i]] <-  ISO2_COMPARISON[[i]]
  
}

write.table(file_2,paste0(out_dir,"/","FILE_ISO2_1.csv"),sep="|",quote = F,row.names = F, na = "")
