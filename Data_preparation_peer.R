#data preparation peer
#Jan Verschoor/Alex van Schaik

setwd("D:/Ausgelagert/PeerAppel/")

options(java.parameters = "-Xmx1024m") #options( java.parameters = "-Xmx4g" )
library(XLConnect)

meta.peer <- loadWorkbook("Peer_Final_Data_Combined_v1.xlsx")

data.peer <- read.table("Peer_Final_Data_Combined_v1_no_meta.csv", sep=",", skip=3, header=TRUE)

grep("b.avg.", colnames(data.peer))#210
grep("opmerk", colnames(data.peer))#717

data.peer.no.met <- data.peer[grep("Conf",data.peer$prod),c(1:210, 717)]

replications(data.peer.no.met[,1:6])

data.peer.no.met$herkomst[which(data.peer.no.met$herkomst=="l")] <- "L"

data.cl <- droplevels.data.frame(data.peer.no.met)

replications(data.cl[,1:6])

hist(data.cl$penetro)
barchart(table(data.cl$kleuri), col="darkgreen")

par(mfrow=c(4,5))

hh <- unique(data.cl$herkomst)
bb <- seq(4,9.5, by=0.5)


for(i in 1:18){
  hist(data.cl$penetr[data.cl$herk==hh[i]&data.cl$met=="na oogst"], main=paste(hh[i]), xlab="firmness", breaks=bb)
}

data.cl.harvest <- data.cl[data.cl$met=="na oogst",]

pp <- ggplot(data=data.cl.harvest, aes(x=penetrometerwaarde))
pp <- pp + geom_histogram(aes(fill=herkomst), binwidth=0.5, xlim=c(4.5,9.5))
#pp <- pp + scale_fill_brewer()#palette="rainbow"
pp <- pp + facet_wrap( ~ herkomst, ncol=4) #ncol=1
pp  



  



