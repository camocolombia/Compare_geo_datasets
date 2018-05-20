library("reshape2")
library("ggplot2")

#file_to_fix <- read.csv("E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/COORDS.csv",sep="|",header=TRUE)
out_dir <- "E:/Dropbox/Dropbox/NPGS georeferencing project/DATASET_FINAL/WORKSPACE/CSV/PLOT"; if(!file.exists(out_dir)){dir.create(out_dir)}

file2 <- read.csv(paste0(out_dir,"/","FILE_ISO2_1.csv"),sep="|")
s2 <- seq(0,100,by=10);s <- s2

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

#TRAFFIC_LIGHT
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
 
  
ISO2_PLOT
#scale_x_continuous(limits = c(s[[1]], s[[length(s)]]),breaks=s2)

 ggsave(paste0(out_dir,"/","ISO_PLOT",".pdf"),ISO2_PLOT,dpi=600,width =90,height=34.395,units = "cm",scale=1.2,limitsize = FALSE)
# gc()

 
 