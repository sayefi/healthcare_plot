dataUrl<-"https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1505433600&Signature=So17H3FdC5knCIGpY5Hb3AkdaZtppFLA0UL9VkoPVB-VWOMlGSPJh8g4XpOU036sBd-RC4G7OaZLStw6-96~ov3f0-H-AuQcFCYux9mvXk09~G06d0T-VgSe8DA8G2Ql9K-DXLbhZGjRlOeosE~gV7bWctu-EG5K5sZxNVtmJRE_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"

download.file(dataUrl,"input.csv","curl")

pData<-read.csv("input.csv")

library(dplyr)

## Make a plot that answers the question: what is the relationship between mean 
## covered charges (Average.Covered.Charges) and mean total payments 
## (Average.Total.Payments) in New York?

pData<-subset(pData,Provider.State=="NY")

pData<-select(pData,Provider.Id,Average.Covered.Charges,Average.Total.Payments)


colv<-log(pData$Average.Total.Payments)>log(pData$Average.Covered.Charges)
pct<-100-round(mean(colv)*100,2)
avgDiff<-round(mean(pData$Average.Covered.Charges-pData$Average.Total.Payments),-2)
colv<-colv+1

myColors<-topo.colors(2,alpha=.4)
vColors<-myColors[colv[seq(1,length(colv),1)]]



plot(log10(pData$Average.Total.Payments),log10(pData$Average.Covered.Charges),
     col=vColors,pch=19,bg="gray",
     xlab="Log10 of Average Total Payments",
     ylab="Log10 of Average covered charges",
     main=paste(pct, "% Covered charges are higher than Payments in NY State"))
fit<-lm(log10(pData$Average.Covered.Charges)~log10(pData$Average.Total.Payments))
abline(fit,col="red",lty=2,lwd=3)

abline(coef=c(0,1),col = "gray", lwd=2,lty = 3)
text(x=3.7,y=4.8,paste("Difference is around ",avgDiff, " Dollars"))

legend("topleft", c("Charges>Payments","Charges<Payments", 
                    "Trend line","Ref. line (Chareges=Payments)"), cex=1, 
       bty="n", fill=rbind(myColors[1],myColors[2],"red","gray"))

dev.copy(pdf,"plot1.pdf",paper="A4r",width=640,height=480)
dev.off()



