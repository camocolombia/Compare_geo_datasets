library("reshape2")
library("ggplot2")

#file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"
out_dir_plot <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"
out_dir_outcome <- paste0(out_dir,"/","OUTCOME"); if(!file.exists(out_dir_outcome)){dir.create(out_dir_outcome)}  

file_ready <- read.csv(paste0(out_dir_outcome,"/","COORDS_FINAL",".csv"),sep="|",header=TRUE)


file2 <- read.csv(paste0(out_dir,"/","FILE_ISO2_1.csv"),sep="|")
s2 <- seq(0,100,by=10);s <- s2


#TRAFFIC_LIGHT FOR ISO2
file3 <- file2[,c(2,4:8)]
colnames(file3) <- c("Accession ID"," GRIN 2007","GRIN GLOBAL 2017","IRRI IMP DATA","IRRI FINAL COORDS","TRAFFIC_LIGHT")

file3_DATA <-  melt(file3, id="Accession ID")  # convert to long format
#file3_DATA$value <- factor(file3_DATA$value,unique(file3_DATA$value))
file3_DATA  <-  file3_DATA[which(file3_DATA$variable=="TRAFFIC_LIGHT"),]
file3_DATA$value <- factor(file3_DATA$value, levels = c("GREEN", "YELLOW", "RED"))
file3_DATA <- file3_DATA[order(file3_DATA$value== "GREEN",
                             file3_DATA$value== "YELLOW",
                             file3_DATA$value== "RED",
                             decreasing = T),]

#TRAFFIC_LIGHT FOR ISO2
ISO2_PLOT <-  ggplot(file3_DATA, aes(value))+
  #ggplot(data=file3_DATA,
  #                       aes(x=value,y=value,fill=value)) +
#  geom_bar(stat="identity") +
  geom_bar(aes(fill = value))+
  guides(fill=FALSE)+
  ylab("Number of occurrences")+ xlab("Traffic light")+
  ggtitle("ISO2 Traffic light counts")+
  #scale_fill_manual(name="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
  #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+

  scale_fill_manual("legend", values = c("green","yellow","red"))+
  theme(panel.background = element_rect(fill = "gray95"),
        text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
        axis.text.y  = element_text(size=49,colour="black"),
        legend.title=element_blank())
  #scale_colour_discrete(c("red","yellow","green"))
 
  
#ISO2_PLOT
#scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

 ggsave(paste0(out_dir_outcome,"/","ISO_PLOT",".pdf"),ISO2_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
# gc()

 ###THREHSOLD PLOT
THR <-  as.data.frame(cbind(file_ready$ACID,as.character(file_ready$THR.y)))
 colnames(THR) <- c("Accession id","value")
 THR <- THR[which(THR$value!=""),]
 THR$value <- factor(THR$value, levels = c("BLUE","PURPLE","DARK GREEN","GREEN","YELLOW","RED"))
 THR <- THR[order(THR$value== "BLUE",
                  THR$value== "PURPLE",
                  THR$value== "DARK GREEN",
                  THR$value== "GREEN",
                  THR$value== "YELLOW",
                  THR$value== "RED",
                                decreasing = T),]
 
# THR <- THR[which(!is.na(THR$value)),]

 
  THR_PLOT <-  ggplot(THR, aes(value))+
   #ggplot(data=file3la re_DATA,
   #                       aes(x=value,y=value,fill=value)) +
   #  geom_bar(stat="identity") +
   geom_bar(aes(fill = value))+
   guides(fill=FALSE)+
   ylab("Number of occurrences")+ xlab("Traffic light")+
   ggtitle("Geographical distances traffic light")+
   #scale_fill_manual(nameThre="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
   #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+
   
   scale_fill_manual("legend", values = c("blue","purple","darkgreen","green","yellow","red"))+
   theme(panel.background = element_rect(fill = "gray95"),
         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
         axis.text.y  = element_text(size=49,colour="black"),
         legend.title=element_blank())
 #scale_colour_discrete(c("red","yellow","green"))
 
 ggsave(paste0(out_dir_outcome,"/","THR_PLOT",".pdf"),THR_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
 
 ###################################################
 
 
 ###UNCERTAINTY PLOT
 UNC <-  as.data.frame(cbind(file_ready$ACID,as.character(file_ready$UNC.y)))
 colnames(UNC) <- c("Accession id","value")
 UNC <- UNC[which(UNC$value!=""),]
 
 UNC$value <- factor(UNC$value, levels = c("BLUE","PURPLE","DARK GREEN","GREEN","YELLOW","RED"))
 UNC <- UNC[order(UNC$value== "BLUE",
                  UNC$value== "PURPLE",
                  UNC$value== "DARK GREEN",
                  UNC$value== "GREEN",
                  UNC$value== "YELLOW",
                  UNC$value== "RED",
                  decreasing = T),]

 
 # THR <- THR[which(!is.na(THR$value)),]
 
 
 UNC_PLOT <-  ggplot(UNC, aes(value))+
   #ggplot(data=file3la re_DATA,
   #                       aes(x=value,y=value,fill=value)) +
   #  geom_bar(stat="identity") +
   geom_bar(aes(fill = value))+
   guides(fill=FALSE)+
   ylab("Number of occurrences")+ xlab("Traffic light")+
   ggtitle("Wieczorek uncertainty traffic light counts")+
   #scale_fill_manual(name="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
   #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+
   
   scale_fill_manual("legend", values = c("blue","purple","darkgreen","green","yellow","red"))+
   theme(panel.background = element_rect(fill = "gray95"),
         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
         axis.text.y  = element_text(size=49,colour="black"),
         legend.title=element_blank())
 #scale_colour_discrete(c("red","yellow","green"))
 
 ggsave(paste0(out_dir_outcome,"/","UNC_PLOT",".pdf"),UNC_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)

 
 ###################################################
 ###################################################
 
 
 ###TRAFFIC LIGHT PLOT
 TRAFFIC <-  as.data.frame(cbind(file_ready$ACID,as.character(file_ready$TRAFFIC_LIGHT.y)))
 colnames(TRAFFIC) <- c("Accession id","value")
 TRAFFIC <- TRAFFIC[which(TRAFFIC$value!=""),]
 TRAFFIC <- TRAFFIC[which(!is.na(TRAFFIC$value)),]
 value2 <- as.character(TRAFFIC$value)
 value2 <- gsub("ACCESSION GEOREFERENCED BY HAND (USING IRRI FINAL COORDS)",
              "BY HAND", value2,fixed = T)
 

 
 TRAFFIC$value <- factor(TRAFFIC$value, levels = c("BLUE","PURPLE","DARK GREEN","GREEN","YELLOW","RED"))
 TRAFFIC <- TRAFFIC[order(TRAFFIC$value== "BLUE",
                          TRAFFIC$value== "PURPLE",
                          TRAFFIC$value== "DARK GREEN",
                          TRAFFIC$value== "GREEN",
                          TRAFFIC$value== "YELLOW",
                          TRAFFIC$value== "RED",
                  decreasing = T),]
 

 # THR <- THR[which(!is.na(THR$value)),]
tapply(TRAFFIC$value,TRAFFIC$value,length)
 
 TRAFFIC_PLOT <-  ggplot(TRAFFIC, aes(value))+
   #ggplot(data=file3la re_DATA,
   #                       aes(x=value,y=value,fill=value)) +
   #  geom_bar(stat="identity") +
   geom_bar(aes(fill = value))+
   guides(fill=FALSE)+
   ylab("Number of occurrences")+ xlab("Traffic light")+
   ggtitle("Traffic light counts")+
   #scale_fill_manual(name="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
   #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+
   
   scale_fill_manual("legend", values = c("blue","purple","darkgreen","green","yellow","red"))+
   theme(panel.background = element_rect(fill = "gray95"),
         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
         axis.text.y  = element_text(size=49,colour="black"),
         legend.title=element_blank())
 #scale_colour_discrete(c("red","yellow","green"))
 
 ggsave(paste0(out_dir_outcome,"/","TRAFFIC_PLOT",".pdf"),TRAFFIC_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
 
 
 ##################
 
 #SUG_COORDS
 
 SUG_coords <- as.data.frame(cbind(file_ready$ACID,as.character(file_ready$SCOORD.y)))
 colnames(SUG_coords) <- c("Accession id","value")
 SUG_coords <- SUG_coords[which(SUG_coords$value!=""),]
 SUG_coords$value <- factor(SUG_coords$value, levels = c(
   "NO COORD",
   "NO LOCALITY",
   "SOS",
   "SOS RECORD UPLOADED IN GRIN GLOBAL",
   "CENTROID",
   "UPLOADED IN GRIN GLOBAL",
   "GRIN 2007", 
   "GRIN 2017",
   "IRRI IMP Data",
   "IRRI FINAL COORDS",
   "GRIN 2007 OR GRIN 2017",
   "GRIN 2007 OR IRRI IMP Data",
   "GRIN 2017 OR IRRI IMP Data",
   "GRIN 2007/GRIN 2017/IRRI IMP Data",
   "GEOREF BY HAND"
   ))
 SUG_coords <- SUG_coords[order(
   SUG_coords$value==  "NO COORD",
   SUG_coords$value==    "NO LOCALITY",
   SUG_coords$value==    "SOS",
   SUG_coords$value==    "SOS RECORD UPLOADED IN GRIN GLOBAL",
   SUG_coords$value==    "CENTROID",
   SUG_coords$value==    "UPLOADED IN GRIN GLOBAL",
   SUG_coords$value==    "GRIN 2007", 
   SUG_coords$value==    "GRIN 2017",
   SUG_coords$value==    "IRRI IMP Data",
   SUG_coords$value==    "IRRI FINAL COORDS",
   SUG_coords$value==    "GRIN 2007 OR GRIN 2017",
   SUG_coords$value==    "GRIN 2007 OR IRRI IMP Data",
   SUG_coords$value==    "GRIN 2017 OR IRRI IMP Data",
   SUG_coords$value==    "GRIN 2007/GRIN 2017/IRRI IMP Data",
   SUG_coords$value==    "GEOREF BY HAND",
   
                  decreasing = T),]
 
 
 

 SUG_coords_PLOT <-  ggplot(SUG_coords, aes(value))+
   #ggplot(data=file3la re_DATA,
   #                       aes(x=value,y=value,fill=value)) +
   #  geom_bar(stat="identity") +
   geom_bar(aes(fill = value))+
   guides(fill=FALSE)+
   ylab("Number of occurrences")+ xlab("Coordinate status")+
  # ggtitle("Coordinate status")+
   #scale_fill_manual(name="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
   #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+
   
  # scale_fill_manual("legend", values = c("purple","red","yellow","green","blue"))+
   theme(panel.background = element_rect(fill = "gray95"),
         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
         axis.text.y  = element_text(size=49,colour="black"),
         legend.title=element_blank())
 #scale_colour_discrete(c("red","yellow","green"))
 SUG_coords_PLOT <- SUG_coords_PLOT + scale_fill_grey()

 ggsave(paste0(out_dir_outcome,"/","SUG_PLOT",".pdf"),SUG_coords_PLOT,dpi=600,width =90,height=60,units = "cm",scale=1.2,limitsize = FALSE)
 
 