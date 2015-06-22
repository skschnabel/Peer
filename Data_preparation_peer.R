#data preparation peer
#Jan Verschoor/Alex van Schaik

setwd("D:/Ausgelagert/PeerAppel/")

library(XLConnect)


# meta.wb <- loadWorkbook("Additional Information_Marta_new.xlsx")
# 
# meta.data <- readWorksheet(meta.wb, sheet = "info", header = TRUE)
# 
# 
# wb.va <- loadWorkbook(filename="data avocados total.xlsx")
data.av <- read.table("data_avocados_total_noNIR.csv", sep=",", skip=2, header=TRUE)

head(data.av)

table(data.av$Firm_acou)