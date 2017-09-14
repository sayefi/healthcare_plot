dataUrl<-"https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1505433600&Signature=So17H3FdC5knCIGpY5Hb3AkdaZtppFLA0UL9VkoPVB-VWOMlGSPJh8g4XpOU036sBd-RC4G7OaZLStw6-96~ov3f0-H-AuQcFCYux9mvXk09~G06d0T-VgSe8DA8G2Ql9K-DXLbhZGjRlOeosE~gV7bWctu-EG5K5sZxNVtmJRE_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"

download.file(dataUrl,"input.csv","curl")

pData<-read.csv("input.csv")

library(dplyr)

## Make a plot that answers the question: what is the relationship between mean 
## covered charges (Average.Covered.Charges) and mean total payments 
## (Average.Total.Payments) in New York?

pDataNy<-subset(pData,Provider.State=="NY")

pDataNy<-select(pDataNy,Provider.Id,Average.Covered.Charges,Average.Total.Payments)


colv<-log(pDataNy$Average.Total.Payments)>log(pDataNy$Average.Covered.Charges)

avgDiff<-round(mean(pDataNy$Average.Covered.Charges-pDataNy$Average.Total.Payments),-2)
colv<-colv+1

myColors<-topo.colors(2,alpha=.4)
vColors<-myColors[colv[seq(1,length(colv),1)]]

par(mfrow=c(1,1))
par(mar=c(4,5,5,1),cex.main=2)

plot(log10(pDataNy$Average.Total.Payments),log10(pDataNy$Average.Covered.Charges),
     col=vColors,pch=19,bg="gray",
     xlab="Log10 of Average Total Payments",
     ylab="Log10 of Average covered charges",
     main="Covered charges are higher than Payments in NY State")
fit<-lm(log10(pDataNy$Average.Covered.Charges)~log10(pDataNy$Average.Total.Payments))
abline(fit,col="red",lty=2,lwd=3)

abline(coef=c(0,1),col = "gray", lwd=2,lty = 3)
text(x=3.7,y=4.8,paste("Difference is around ",avgDiff, " Dollars"))

legend("topleft", c("Charges>Payments","Charges<Payments", 
                    "Trend line","Ref. line (Chareges=Payments)"), cex=1, 
       bty="n", fill=rbind(myColors[1],myColors[2],"red","gray"))

dev.copy(pdf,"plot1.pdf",paper="A4r",width=640,height=480)
dev.off()

## Make a plot (possibly multi-panel) that answers the question: how does the 
## relationship between mean covered charges (Average.Covered.Charges) and mean 
## total payments (Average.Total.Payments) vary by 
## medical condition (DRG.Definition) and the state in which care was 
## received (Provider.State)?

dDef<-unique(pData$DRG.Definition)
states<-unique(pData$Provider.State)



# par(mfrow=c(r,c))

par(mfrow=c(2,3))
par(mar=c(4,5,5,1),cex.main=1)


for(n in 1:6){
     plotData<-subset(pData,DRG.Definition==dDef[n])
     
     plotData<-select(plotData,Provider.State,Average.Covered.Charges,
                      Average.Total.Payments)
     
     plotData<-group_by(plotData,Provider.State)
     plotData<-dplyr::summarise_all(plotData,mean)
     
     # rownames(plotData)<-plotData$Provider.State
     
     plotDataMatrix<-as.matrix(t(plotData[2:3]))
     colnames(plotDataMatrix)<-plotData$Provider.State
     
     
     #plotData<-subset(pData,Provider.State==states[n])
     
     barplot(plotDataMatrix,beside=TRUE,col=topo.colors(2,alpha=.80),
             ylim=c(0,85000),main=dDef[n],ylab="Amount in $",xlab="State")
     
     abline(h=mean(plotData$Average.Covered.Charges),
            col = topo.colors(2,alpha=.80)[1], lwd=2,lty = 3)
     
     abline(h=mean(plotData$Average.Total.Payments),
            col ="gray", lwd=2,lty = 3)
     
     # abline(h=mean(plotData$Average.Covered.Charges),col = "gray", lwd=2,lty = 3)
     
     if(n==1){
          legend("topleft", c("Average Covered Charges","Average Payments"), cex=1, 
                 bty="n", fill=topo.colors(2,alpha=.80))
     }
     
}


dev.copy(pdf,"plot2.pdf",paper="A4r",width=640,height=480)
dev.off()
