library("reshape2")
library("ggplot2")

#file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV"
out_dir_plot <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"
out_dir_outcome <- paste0(out_dir,"/","OUTCOME"); if(!file.exists(out_dir_outcome)){dir.create(out_dir_outcome)}  

file_ready <- read.csv(paste0(out_dir_outcome,"/","COORDS_FINAL",".csv"),sep="|",header=TRUE)


file2 <- read.csv(paste0(out_dir_plot,"/","FILE_ISO2_1.csv"),sep="|")
s2 <- seq(0,100,by=10);s <- s2


#TRAFFIC_LIGHT FOR ISO2
file3 <- file2[,c(2,4:8)]
colnames(file3) <- c("Accession ID"," GRIN 2007","GRIN GLOBAL 2017","IRRI IMP DATA","IRRI FINAL COORDS","TRAFFIC_LIGHT")

file3_DATA <-  melt(file3, id="Accession ID")  # convert to long format
#file3_DATA$value <- factor(file3_DATA$value,unique(file3_DATA$value))
file3_DATA  <-  file3_DATA[which(file3_DATA$variable=="TRAFFIC_LIGHT"),]
file3_DATA$value <- factor(file3_DATA$value, levels = c("RED", "YELLOW", "GREEN"))
file3_DATA <- file3_DATA[order(file3_DATA$value== "RED",
                             file3_DATA$value== "YELLOW",
                             file3_DATA$value== "GREEN",
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

  scale_fill_manual("legend", values = c("red","yellow","green"))+
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
 THR$value <- factor(THR$value, levels = c("PURPLE","RED", "YELLOW", "GREEN"))
 THR <- THR[order(THR$value== "PURPLE",
                  THR$value== "RED",
                  THR$value== "YELLOW",
                  THR$value== "GREEN",
                                decreasing = T),]
 
# THR <- THR[which(!is.na(THR$value)),]

 
  THR_PLOT <-  ggplot(THR, aes(value))+
   #ggplot(data=file3la re_DATA,
   #                       aes(x=value,y=value,fill=value)) +
   #  geom_bar(stat="identity") +
   geom_bar(aes(fill = value))+
   guides(fill=FALSE)+
   ylab("Number of occurrences")+ xlab("Traffic light")+
   ggtitle("Threshold traffic light counts")+
   #scale_fill_manual(nameThre="Traffic\n light",labels = c("Red","Yellow","Green"),values = c("red","yellow","green"))+
   #scale_fill_manual("legend", values = c("RED" = "red", "YELLOW" = "yellow", "GREEN" = "green"))+
   
   scale_fill_manual("legend", values = c("purple","red","yellow","green"))+
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
 UNC$value <- factor(UNC$value, levels = c("PURPLE","RED", "YELLOW", "GREEN"))
 UNC <- UNC[order(UNC$value== "PURPLE",
                  UNC$value== "RED",
                  UNC$value== "YELLOW",
                  UNC$value== "GREEN",
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
   
   scale_fill_manual("legend", values = c("purple","red","yellow","green"))+
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
 

 TRAFFIC$value <- factor(value2, levels = c("PURPLE","RED", "YELLOW", "GREEN","BY HAND"))
 TRAFFIC <- TRAFFIC[order(TRAFFIC$value== "PURPLE",
                  TRAFFIC$value== "RED",
                  TRAFFIC$value== "YELLOW",
                  TRAFFIC$value== "GREEN",
                  TRAFFIC$value== "BY HAND",
                  decreasing = T),]
 
 # THR <- THR[which(!is.na(THR$value)),]
 
 
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
   
   scale_fill_manual("legend", values = c("purple","red","yellow","green","blue"))+
   theme(panel.background = element_rect(fill = "gray95"),
         text=element_text(size=40),axis.text.x  = element_text(size=40,colour="black",angle = 90, hjust = 1),
         axis.text.y  = element_text(size=49,colour="black"),
         legend.title=element_blank())
 #scale_colour_discrete(c("red","yellow","green"))
 
 ggsave(paste0(out_dir_outcome,"/","TRAFFIC_PLOT",".pdf"),TRAFFIC_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
 