#data preparation peer
#Jan Verschoor/Alex van Schaik

setwd("D:/Ausgelagert/PeerAppel/")

options(java.parameters = "-Xmx1024m") #options( java.parameters = "-Xmx4g" )
library(XLConnect)
 
meta.wb.peer <- loadWorkbook("Dataset_orchard_and_quality_pears_correct_version_SKS.xlsx")
meta.peer <- readWorksheet(meta.wb.peer, sheet = "Sheet1", header = TRUE, startRow=2)
 
head(meta.peer)
apply(meta.peer, 2, table)

data.peer <- read.table("Peer_Final_Data_Combined_v1_no_meta.csv", sep=",", skip=3, header=TRUE)

grep("b.avg.", colnames(data.peer))#210
grep("opmerk", colnames(data.peer))#717

data.peer.no.met <- data.peer[grep("Conf",data.peer$prod),c(1:210, 717)]

replications(data.peer.no.met[,1:6])

data.peer.no.met$herkomst[which(data.peer.no.met$herkomst=="l")] <- "L"

data.cl <- droplevels.data.frame(data.peer.no.met)

idint <- interaction(factor(data.cl$vrucht), data.cl$herkomst, factor(data.cl$batch), factor(data.cl$duplo))

#data.cl is the data without metabolomics data but included the LBA(?) data

data.peer.meta <- read.table("Peer_Final_Data_Combined_v1_no_LBAHue.csv", sep=",", skip=3, header=TRUE)

grep("SPME.GCMS.392.1", colnames(data.peer.meta))#481
grep("opmerk", colnames(data.peer.meta))#548

Conf <- grep("Conf",data.peer.meta$prod)
data.peer.meta.new <- data.peer.meta[Conf,c(1:481,548)]

data.peer.meta.new$herkomst[which(data.peer.meta.new$herkomst=="l")] <- "L"

data.cl.meta <- droplevels.data.frame(data.peer.meta.new)

###
#meta data
###

save(data.cl, data.cl.meta, meta.peer, file="Wspace_peer_metaData_CleanedData_CleanedDataMeta.Rdata")




replications(data.cl[,1:6])

hist(data.cl$penetro)
barplot(table(data.cl$kleuri), col="darkgreen")

data.cl.harvest <- data.cl[data.cl$met=="na oogst",]

library(ggplot2)

pp <- ggplot(data=data.cl.harvest, aes(x=penetrometerwaarde)) + ggtitle("Firmness after harvest")
pp <- pp + geom_histogram(aes(y= ..density.., fill=herkomst), binwidth=0.25, xlim=c(4.5,9.5))
pp  <- pp + geom_density() 
pp <- pp + facet_wrap( ~ herkomst, ncol=4)   
pp

print(table(data.cl$met))
# 
# eind stresstest                    eind stresstest zonder koude 
# 720                                              80 
# na 6 maanden CA                   na 6 maanden CA + 1 week 10°C 
# 720                                             720 
# na 6 maanden CA + China simulatie na 6 maanden CA + China simulatie + 1 week 10°C 
# 1440                                            1440 
# na 9 maanden CA                   na 9 maanden CA + 1 week 10°C 
# 720                                             720 
# na oogst                                start stresstest 
# 1440                                             720 

tl <- names(table(data.cl$met))

time.list <- tl[c(9,10,1,3,4,5,6,7,8)]


plot_list = list()

#pdf("Firmness_histograms.pdf", paper="a4r", width=10, height=7, onefile=TRUE)
for (i in c(1, 3:9)) {
  data.plot <- data.cl[data.cl$met==time.list[i],]
  if(sum(is.na(data.plot$pen)==dim(data.plot)[1])) next
  
  pp <- ggplot(data=data.plot, aes(x=penetrometerwaarde)) + ggtitle(paste("Firmness", time.list[i]))
  pp <- pp + geom_histogram(aes(y= ..density.., fill=herkomst), binwidth=0.25) + scale_x_continuous(limits = c(0, 9.5))#xlim=c(4.5,9.5))
  pp  <- pp + geom_density() 
  pp <- pp + facet_wrap( ~ herkomst, ncol=5)   
  pp
  plot_list[[i]] <- pp
}
#dev.off()
  
pdf("Firmness_histograms.pdf", onefile=TRUE, paper="a4r", width=10, height=7)
for (i in c(1, 3:9)) {
  data.plot <- data.cl[data.cl$met==time.list[i],]
  if(sum(is.na(data.plot$pen)==dim(data.plot)[1])) next  
  print(plot_list[[i]])
}
dev.off()

#start stresstest does not have measurements

#ideally a model is wanted that can predict the firmness after 6/9 months plus 1 week from the firmness 
#after harvest plus potentially other measurements

#some more summary statistics about firmness and other parameters at the different time points. 

hist(data.cl$kleurindex[data.cl$met=="na oogst"])

data.plot <- data.cl[data.cl$met=="na oogst",]

pp <- ggplot(data=data.plot, aes(x=kleurindex..1.groen.5.geel.)) + ggtitle(paste("Kleurindex na oogst"))
pp <- pp + geom_bar(aes(fill=herkomst), binwidth=0.5)#, binwidth=0.5) geom_histogram(aes(y= ..density.., fill=herkomst), binwidth=0.5) + scale_x_continuous(limits = c(0, 9.5))#xlim=c(4.5,9.5))
#pp  <- pp + geom_density() 
pp <- pp + facet_wrap( ~ herkomst, ncol=5)   
pp
  



