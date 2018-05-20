require(ggplot2);require(reshape)

source("E:/Dropbox/Dropbox/NPGS georeferencing project/SCRIPTS/THR/000_DIST_FUNCTION.R")

file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"; if(!file.exists(out_dir)){dir.create(out_dir)}

###

f2 <- cbind(file_to_fix$ACID,
          as.numeric(as.character(file_to_fix$ACCESS_distance_georef)),
          as.numeric(as.character(file_to_fix$GRIN_distance_georef)),
          as.numeric(as.character(file_to_fix$GEO_IMPROVED_distance_georef)),
          as.numeric(as.character(file_to_fix$HIJMANS_distance_georef))
          )
f2[,2:5] <- f2[,2:5]/1000

colnames(f2) <- c("PII","GRIN 2007","GRIN GLOBAL 2017","IRRI IMP DATA"," IRRI FINAL COORDS")


UNC_DATA  <-  melt(f2, id="PII") 
UNC_DATA <- UNC_DATA[which(UNC_DATA$X2!="PII"),]
colnames(UNC_DATA) <- c("ID","DATASET","VALUE")
###

p1 <- ggplot(data=UNC_DATA,aes(x=VALUE,fill=DATASET,colour=DATASET))+
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha = 0.1)+
  xlim(c(0,100))+
  ylab("Density")+ xlab("Uncertainty (km)")+
  ggtitle("Georeferencing uncertainty (NO EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())#+

####  
p1a <- ggplot(data=UNC_DATA,aes(x=DATASET,y=VALUE))+
  geom_boxplot(aes(fill=DATASET),outlier.size=NA) +
  stat_boxplot(geom ='errorbar') +
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  guides(fill=FALSE)+
  ylim(c(0,40))+
  ylab("Uncertainty (km)")+ xlab("")+
  ggtitle("Georeferencing uncertainty Status (NO EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),legend.position="none")#+




#########################################################



f3 <- f2

for(i in 1:nrow(file_to_fix)){
  cat("ROW: ",i,"\n")
  if(is.na(file_to_fix$ACCESS_final_lat[[i]])){
    f3[i,2] <- NA
      } else  if(is.na(file_to_fix$GRIN_final_lat[[i]])){
    f3[i,3] <- NA
  } else  if(is.na(file_to_fix$GEO_final_lat[[i]])){
    f3[i,4] <- NA
  } else  if(is.na(file_to_fix$HIJMANS_final_lat[[i]])){
    f3[i,5] <- NA
  } else  if(file_to_fix$georefremarks[[i]]==1){
    f3[i,2] <- NA
    f3[i,3] <- NA
    f3[i,4] <- NA
    #f3[i,5] <- NA
    
  } else  if(file_to_fix$LOCALITY_FLAG[[i]]==1){
    f3[i,2] <- NA
    f3[i,3] <- NA
    f3[i,4] <- NA
    f3[i,5] <- NA
    
  } else  if(file_to_fix$SOS_FLAG[[i]]==1){
    f3[i,2] <- NA
    f3[i,3] <- NA
    f3[i,4] <- NA
    f3[i,5] <- NA
    
  } else  if(file_to_fix$GG_COORDS_FLAG[[i]]==1){
    f3[i,2] <- NA
   # f3[i,3] <- NA
    f3[i,4] <- NA
    f3[i,5] <- NA
    
  } else  if(file_to_fix$HIJ_FLAG[[i]]==1){
    f3[i,5] <- NA
  }
  
}



UNC_DATA2 <-  melt(f3, id="PII") 
UNC_DATA2 <- UNC_DATA2[which(UNC_DATA2$X2!="PII"),]
colnames(UNC_DATA2) <- c("ID","DATASET","VALUE")
###

p2 <- ggplot(data=UNC_DATA2,aes(x=VALUE,fill=DATASET,colour=DATASET))+
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha = 0.1)+
  xlim(c(0,100))+
  ylab("Density")+ xlab("Uncertainty (km)")+
  ggtitle("Georeferencing uncertainty  (EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank())#+


####  
p2a <- ggplot(data=UNC_DATA2,aes(x=DATASET,y=VALUE))+
  geom_boxplot(aes(fill=DATASET),outlier.size=NA) +
  stat_boxplot(geom ='errorbar') +
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  guides(fill=FALSE)+
  ylim(c(0,40))+
  ylab("Uncertainty (km)")+ xlab("")+
  ggtitle("Georeferencing uncertainty Status (EXCLUSIONS)")+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=40,colour="black"),
        legend.title=element_blank(),legend.position="none")#+

ggsave(paste0(out_dir,"/","UNC_ALL_DENS",".pdf"),p1,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
ggsave(paste0(out_dir,"/","UNC_EXC_DENS",".pdf"),p2,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
ggsave(paste0(out_dir,"/","UNC_ALL_BP",".pdf"),p1a,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
ggsave(paste0(out_dir,"/","UNC_EXC_BP",".pdf"),p2a,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)

gc();


