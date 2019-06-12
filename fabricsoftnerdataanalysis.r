setwd("C:\\SDM_FabricSoftner");
dataone = read.table("DATAONEPUR.DAT",header=F, fill=T, col.names=c("HH_id","trip_info"),strip.white=T)
datatwo=read.fwf(file=textConnection(as.character(dataone$trip_info)), widths=rep(3,3), col.names =c("IRIweek","storeno","SKUno"))
dataone$trip_info <- NULL
Data12bind <- cbind(dataone,datatwo)
datathree = read.table("MERCH.DAT",header=F, fill=T, col.names=c("SKUno","storeno","IRIweek",
                  "price_paid","merchandising"),strip.white=T)
attach(datathree)
 
datafour=read.fwf(file=textConnection(as.character(datathree$merchandising)), widths=rep(3,2),
            col.names=c("depromoted_price","temp"))
datafive=read.fwf(file=textConnection(as.character(datafour$temp)), widths=rep(1,3),
            col.names=c("ignore","DISP","FEAT"))
mergeddata <- cbind(datathree,datafour,datafive)
arspdataa= read.table("ARSP.DAT",header=F, fill=T, col.names=c("SKU","storeno","ARSP"),
                strip.white=T)

mergeddata$IRIweek <- as.numeric(mergeddata$IRIweek)
Merge2 <- merge(Data12bind,mergeddata,by=c("IRIweek","storeno","SKUno"))
 

atribdata <- read.csv("MembershipPaneldata.csv")
atribdata  <- atribdata [,-1]
 
 
colnames(Merge2)[3] <- "SKU"
attribandmergeddata <- merge(Merge2,atribdata ,by=c("SKU"))
 
 
finaldataset <- merge(arspdata,attribandmergeddata, by=c("SKU","storeno"))
colnames(finaldataset)[7] <- "Regprice"
colnames(finaldataset)[3] <- "AveragePrice"
finaldataset$pricecut<-finaldataset$Regprice-finaldataset$price_paid
finaldataset$pricecut<-ifelse(finaldataset$pricecut<0, 0, finaldataset$pricecut)
 
finaldata<-finaldataset[order(finaldataset$IRIweek),] 
write.csv(finaldata, "FinalData.csv", row.names = F)

dataw=cor(FinalFSData,method="Kendall")
corrplot(cor(FinalFSData),method=”circle”)
FinalFSData=FinalFSData[IRIweek<=644,]

kitchenSinkModel2=lm(SKU~AveragePrice+IRIweek+HH_id+price_paid+Regprice+Display+Feature+ARM+BNC+CLF+DWN+FNT+GEN+PRL+SNG+STP+B+F+L+LT+RG+ST+LR+MD+SM)
summary(kitchenSinkModel2)
AIC(kitchenSinkModel)
library(forecast)
BoxCox.lambda(AveragePrice)
AveragePrice2<- ((AveragePrice^-0.5772121)-1)/-0.5772121
BoxCox.lambda(SKU)
SKU2 <- ((SKU^-0.9999242)-1)/-0.9999242
BoxCox.lambda(IRIweek)
IRIweek2 <- ((IRIweek^  1.000001)-1)/  1.000001
BoxCox.lambda(HH_id)
HH_id <- ((HH_id^1.999924)-1)/ 1.999924
BoxCox.lambda(price_paid)
price_paid <- ((price_paid^ -0.5752739)-1)/ -0.5752739
boxcoxmodel1=lm(SKU2~AveragePrice2+IRIweek2+price_paid2+Regprice2+Display+Feature+ARM+BNC+CLF+DWN+FNT+GEN+PRL+SNG+STP+B+F+L+LT+RG+ST+LR+MD+SM)
summary(boxcoxmodel1)
AIC(boxcoxmodel1)
library(rpart)

new.tree.fit<-rpart(as.numeric(SKU)~as.numeric(AveragePrice)+as.numeric(IRIweek)+
                      +as.numeric(price_paid)+as.numeric(Regprice)+
                      as.factor(Display)+as.factor(Feature)+as.factor(ARM)+
                      as.factor(BNC)+as.factor(CLF)+as.factor(DWN)+as.factor(FNT)+
                      as.factor(GEN)+as.factor(PRL)+as.factor(SNG)+as.factor(STP)+
                      as.factor(B)+as.factor(F)+as.factor(L)+as.factor(LT)+as.factor(RG)+
                      as.factor(ST)+as.factor(LR)+as.factor(MD)+as.factor(SM), 
                      control = rpart.control(minsplit = 30, maxdepth = 4))

pred.tree <- predict(new.tree.fit, newdata=FinalFSData)
pred.tree
d33 <- finaldata[finaldata$IRIweek<=643,]
d33 <- finaldata[finaldata$IRIweek>=600,]

attach(d33)

library(nnet)

multnm <- multinom(SKU ~ AveragePrice+B+LT+SM+MD+LR+ARM+BNC+CLF+
                  DWN+FNT+GEN+PRL+SNG+STP, data=d33
