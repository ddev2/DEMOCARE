setwd("~/Google Drive/DemoCare_BS")

#save(depTrans, file="depTrans.Rdata")
load(file="depTrans.Rdata")

cDepTrans <- dcast(depTrans, Edu + Sex + CCAA + Age ~ variable)
cDepTrans$type <- "main"

cDepTrans1_2 <- cDepTrans
cDepTrans1_2$type <- "1_2"
cDepTrans1_2$p2_3 <- 0

cDepTrans1_3 <- cDepTrans1_2
cDepTrans1_3$type <- "1_3"
cDepTrans1_3$p2_3 <- 1

cDepTrans <- rbind(cDepTrans, cDepTrans1_2)
cDepTrans <- rbind(cDepTrans, cDepTrans1_3)

mDepTrans <- melt(cDepTrans, id=c("Edu", "Sex", "CCAA", "Age", "type"))

te(mDepTrans)
