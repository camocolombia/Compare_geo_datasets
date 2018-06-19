require(ggplot2);require(reshape)

source("E:/Dropbox/Dropbox/NPGS georeferencing project/SCRIPTS/THR/000_DIST_FUNCTION.R")

file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS2.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"; if(!file.exists(out_dir)){dir.create(out_dir)}

###


for(i in 1:nrow(file_to_fix)){
  cat(i," row","\n")
  if(file_to_fix$SOS_FLAG[[i]]==1){
    
    file_to_fix$ACCESS_final_lat[[i]] <- NA
    file_to_fix$ACCESS_final_lon[[i]] <- NA
    file_to_fix$GRIN_final_lat[[i]] <- NA
    file_to_fix$GRIN_final_lon[[i]] <- NA
    file_to_fix$GEO_final_lat[[i]] <- NA
    file_to_fix$GEO_final_lon[[i]] <- NA
    file_to_fix$HIJMANS_final_lat[[i]] <- NA
    file_to_fix$HIJMANS_final_lon[[i]] <- NA
    
    file_to_fix$ACCESS_distance_georef[[i]] <- NA
    file_to_fix$GRIN_distance_georef[[i]] <- NA   
    file_to_fix$GEO_IMPROVED_distance_georef[[i]] <- NA
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
  } else if(file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1){
    cat(i," row","\n")
    
    file_to_fix$HIJMANS_final_lat[[i]] <- NA
    file_to_fix$HIJMANS_final_lon[[i]] <- NA
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
    
    
  } else  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    cat(i," row","\n")
    
    file_to_fix$ACCESS_distance_georef[[i]] <- NA
    file_to_fix$GRIN_distance_georef[[i]] <- NA   
    file_to_fix$GEO_IMPROVED_distance_georef[[i]] <- NA
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
    
  }
  ######
  if(is.na(file_to_fix$ACCESS_final_lat[[i]])){
    cat(i," row","\n")
    file_to_fix$ACCESS_distance_georef[[i]] <- NA
  }
  if(is.na(file_to_fix$GRIN_final_lat[[i]])){
    cat(i," row","\n")
    file_to_fix$GRIN_distance_georef[[i]] <- NA
  }
  if(is.na(file_to_fix$GEO_final_lat[[i]])){
    cat(i," row","\n")
    file_to_fix$GEO_IMPROVED_distance_georef[[i]] <- NA
  }
  if(is.na(file_to_fix$HIJMANS_final_lat[[i]])){
    cat(i," row","\n")
    file_to_fix$HIJMANS_distance_georef[[i]] <- NA
  }
  
  
};rm(i)




#############################################################
#############################################################


AC_ID <- file_to_fix
AC_ID <- AC_ID[,1:3]


S1 <- list();SCOORD <- list()

for(i in 1:nrow(file_to_fix)){
  
  cat("TRAFFIC LIGHT FOR ROW: ",i,"\n")
  
  
  if(file_to_fix$SOS_FLAG[[i]]==1){
    S1[[i]] <- "PURPLE"
    SCOORD[[i]] <- "SOS"
  } else  if(file_to_fix$IRRI_BYHAND_FLAG[[i]]==1){
    S1[[i]] <- "GREEN"
    SCOORD[[i]] <- "GEOREF BY HAND"
  } else  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    S1[[i]] <- "RED"
    SCOORD[[i]] <- "NO LOCALITY"
  } else  if(file_to_fix$IRRI_LOCALITY_FLAG[[i]]==1){
    S1[[i]] <- "RED"
    SCOORD[[i]] <- "CENTROID"
  } else  if(file_to_fix$GG_COORDS_FLAG[[i]]==1){
    S1[[i]] <- "GREEN"
    SCOORD[[i]] <- "UPLOADED IN GRIN GLOBAL"
  } else {
    
    Unc <- cbind(file_to_fix$ACCESS_distance_georef[[i]],
                 file_to_fix$GRIN_distance_georef[[i]],
                 file_to_fix$GEO_IMPROVED_distance_georef[[i]],
                 file_to_fix$HIJMANS_distance_georef[[i]]
    )
    
    colnames(Unc) <- c(
      "ACCESS_distance_georef",
      "GRIN_distance_georef",
      "GEO_IMPROVED_distance_georef",
      "HIJMANS_distance_georef"
    )
    
    
    xx <- as.data.frame(t(Unc))
    xx$UNCERTAINTY <- xx$V1
    xx$DATASET <- row.names(xx)
    xx <- as.data.frame(cbind(xx$DATASET,xx$UNCERTAINTY))
    colnames(xx) <- c("DATASET","UNCERTAINTY")
    row.names(xx) <- 1:nrow(xx)
    xx$DATASET <- as.character(xx$DATASET)
    xx$UNCERTAINTY <- as.numeric(as.character(xx$UNCERTAINTY))
    
    xx$UNCERTAINTY <- xx$UNCERTAINTY/1000
    # xx2 <- subset(xx,xx$DATASET==xx$DATASET[which(xx$UNCERTAINTY==(max(xx$UNCERTAINTY,na.rm=T)))])
    #xx2 <- xx[xx$DATASET[which(xx$UNCERTAINTY==(max(xx$UNCERTAINTY,na.rm=T)))],]
    xx2 <- xx[which(xx$UNCERTAINTY == max(xx$UNCERTAINTY,na.rm = T)), ]
    
    
    compM <- xx2$DATASET==c("ACCESS_distance_georef","GRIN_distance_georef","GEO_IMPROVED_distance_georef")
    
    if(nrow(xx2)==0){
      #################################################
      SCOORD[[i]] <- "NO COORD"
      S1[[i]] <- "RED"
    } else if(nrow(xx2)==1){
      
      ####
      if(xx2$DATASET[1]=="ACCESS_distance_georef" &
         xx2$UNCERTAINTY[[1]]<=10){
        S1[[i]] <- "GREEN"
        SCOORD[[i]] <- "GRIN 2007" 
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        S1[[i]] <- "YELLOW"
        SCOORD[[i]] <- "GRIN 2007" 
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>50){
        S1[[i]] <- "RED"
        SCOORD[[i]] <- "GRIN 2007" 
      }
      ####
      if(xx2$DATASET[1]=="GRIN_distance_georef" &
         xx2$UNCERTAINTY[[1]]<=10){
        S1[[i]] <- "GREEN"
        SCOORD[[i]] <- "GRIN 2017" 
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        S1[[i]] <- "YELLOW"
        SCOORD[[i]] <- "GRIN 2017" 
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>50){
        S1[[i]] <- "RED"
        SCOORD[[i]] <- "GRIN 2017" 
      }
      ####
      if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" &
         xx2$UNCERTAINTY[[1]]<=10){
        S1[[i]] <- "GREEN"
        SCOORD[[i]] <- "IRRI IMP Data" 
      } else  if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        S1[[i]] <- "YELLOW"
        SCOORD[[i]] <- "IRRI IMP Data" 
      } else  if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>50){
        S1[[i]] <- "RED"
        SCOORD[[i]] <- "IRRI IMP Data" 
      }
      ####
      if(xx2$DATASET[1]=="HIJMANS_distance_georef" &
         xx2$UNCERTAINTY[[1]]<=10){
        S1[[i]] <- "GREEN"
        SCOORD[[i]] <- "IRRI FINAL COORDS" 
      } else  if(xx2$DATASET[1]=="HIJMANS_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        S1[[i]] <- "YELLOW"
        SCOORD[[i]] <- "IRRI FINAL COORDS" 
      } else  if(xx2$DATASET[1]=="HIJMANS_distance_georef" &
                 xx2$UNCERTAINTY[[1]]>50){
        S1[[i]] <- "RED"
        SCOORD[[i]] <- "IRRI FINAL COORDS" 
      }
      #################################################  
    } else  if(nrow(xx2)==2){
      
      ########10 KM
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef"&  
         xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "GRIN 2007 OR GRIN 2017"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "GRIN 2007 OR IRRI IMP Data"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "GRIN 2007"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "GRIN 2017 OR IRRI IMP Data"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "GRIN 2017"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]<=10){
        SCOORD[[i]] <- "IRRI IMP Data"
        S1[[i]] <- "GREEN"
      }
      ######## >10 Km & 50 Km
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef"&  
         xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "GRIN 2007 OR GRIN 2017"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "GRIN 2007 OR IRRI IMP Data"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "GRIN 2007"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "GRIN 2017 OR IRRI IMP Data"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "GRIN 2017"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
        SCOORD[[i]] <- "IRRI IMP Data"
        S1[[i]] <- "YELLOW"
      }
      ######## >50 Km
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef"&  
         xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "GRIN 2007 OR GRIN 2017"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "GRIN 2007 OR IRRI IMP Data"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "GRIN 2007"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "GRIN 2017 OR IRRI IMP Data"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "GRIN 2017"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="GEO_IMPROVED_distance_georef" & 
                 xx2$DATASET[2]=="HIJMANS_distance_georef"&  
                 xx2$UNCERTAINTY[[1]]>50){
        SCOORD[[i]] <- "IRRI IMP Data"
        S1[[i]] <- "RED"
      }
      
      
      
      #################################################
    } else   if(nrow(xx2)==3){
      ########10 KM     
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef" &
         xx2$DATASET[3]=="GEO_IMPROVED_distance_georef" &  
         xx2$UNCERTAINTY[[1]]<=10
      ){
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GRIN_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]<=10
      ){
        
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017"  
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]<=10
      ){
        
        SCOORD[[i]] <- "GRIN 2007/IRRI IMP Data"
        S1[[i]] <- "GREEN"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]<=10
      ){
        
        SCOORD[[i]] <- "GRIN 2017 /IRRI IMP Data"  
        S1[[i]] <- "GREEN"
      }
      
      ########10 & 50 KM     
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef" &
         xx2$DATASET[3]=="GEO_IMPROVED_distance_georef" &  
         xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50
      ){
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GRIN_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50
      ){
        
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017"  
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50
      ){
        
        SCOORD[[i]] <- "GRIN 2007/IRRI IMP Data"
        S1[[i]] <- "YELLOW"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50
      ){
        
        SCOORD[[i]] <- "GRIN 2017 /IRRI IMP Data"  
        S1[[i]] <- "YELLOW"
      }
      ########>50 KM     
      if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
         xx2$DATASET[2]=="GRIN_distance_georef" &
         xx2$DATASET[3]=="GEO_IMPROVED_distance_georef" &  
         xx2$UNCERTAINTY[[1]]>50
      ){
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GRIN_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>50
      ){
        
        SCOORD[[i]] <- "GRIN 2007/GRIN 2017"  
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="ACCESS_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>50
      ){
        
        SCOORD[[i]] <- "GRIN 2007/IRRI IMP Data"
        S1[[i]] <- "RED"
      } else  if(xx2$DATASET[1]=="GRIN_distance_georef" & 
                 xx2$DATASET[2]=="GEO_IMPROVED_distance_georef" &
                 xx2$DATASET[3]=="HIJMANS_distance_georef" &  
                 xx2$UNCERTAINTY[[1]]>50 
      ){
        
        SCOORD[[i]] <- "GRIN 2017 /IRRI IMP Data"  
        S1[[i]] <- "RED"
      }
      
      
      
      #################################################    
      ######## 10 KM
    } else   if(nrow(xx2)== 4&  
                xx2$UNCERTAINTY[[1]]<=10){
      
      SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
      S1[[i]] <- "GREEN"
      
      ######## 10 & 50  KM
    } else   if(nrow(xx2)== 4 &  
                xx2$UNCERTAINTY[[1]]>10 & xx2$UNCERTAINTY[[1]]<=50){
      
      SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
      S1[[i]] <- "YELLOW"
      ######## 50  KM
    } else   if(nrow(xx2)== 4 &  
                xx2$UNCERTAINTY[[1]]>50){
      
      SCOORD[[i]] <- "GRIN 2007/GRIN 2017/IRRI IMP Data"  
      S1[[i]] <- "RED"
    }
  }
}

# for(i in 1:length(SCOORD)){
#   cat("list n | row: ",i,"\n")
#   SCOORD[[i]][which(file_to_fix$GG_COORDS_FLAG[[i]] ==1)] <- "BLUE"
#   
# }

S1 <- as.data.frame(unlist(S1))
SCOORD <- as.data.frame(unlist(SCOORD))

UNCER <- cbind(AC_ID,S1,SCOORD)
colnames(UNCER) <- c("DUMMY","ACID","PII","TRAFFIC_LIGHT","SUGGESTED COORD")
UNCER$TRAFFIC_LIGHT <- as.character(UNCER$TRAFFIC_LIGHT)
UNCER$TRAFFIC_LIGHT[which(file_to_fix$IRRI_BYHAND_FLAG==1)] <- "DARK GREEN"
UNCER$TRAFFIC_LIGHT[which(file_to_fix$GG_COORDS_FLAG==1)] <- "BLUE"
UNCER$TRAFFIC_LIGHT[which(file_to_fix$SOS_FLAG==1)] <- "PURPLE"
write.table(UNCER,paste0(out_dir,"/","UNCERTAINTIES_MAX.csv"),sep="|",quote = F,row.names = F, na = "")
