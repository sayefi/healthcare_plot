dataUrl<-"https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1505433600&Signature=So17H3FdC5knCIGpY5Hb3AkdaZtppFLA0UL9VkoPVB-VWOMlGSPJh8g4XpOU036sBd-RC4G7OaZLStw6-96~ov3f0-H-AuQcFCYux9mvXk09~G06d0T-VgSe8DA8G2Ql9K-DXLbhZGjRlOeosE~gV7bWctu-EG5K5sZxNVtmJRE_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"

download.file(dataUrl,"input.csv","curl")

pData<-read.csv("input.csv")

library(dplyr)

## Make a plot that answers the question: what is the relationship between mean 
## covered charges (Average.Covered.Charges) and mean total payments 
## (Average.Total.Payments) in New York?

pData<-subset(pData,Provider.State=="NY")
#pData<-transform(pData,Provider.Id=factor(Provider.Id))
pData<-select(pData,Provider.Id,Average.Covered.Charges,Average.Total.Payments)
#pData<-arrange(pData,Provider.Id)
pData<-group_by(pData,Provider.Id)
pData<-summarise_all(pData,mean)
pData<-transform(pData,diff=Average.Covered.Charges-Average.Total.Payments)
pData<-arrange(pData,diff)




length(unique(pData$Provider.Id))
length(unique(pData$Provider.Zip.Code))

par(mfrow=c(1,1))

# Graph cars using a y axis that ranges from 0 to 12 , ylim=c(0,12)
plot(pData$Provider.Id,log10(pData$Average.Covered.Charges), type="p", col="red",pch=19)

# Graph trucks with red dashed line and square points
lines(pData$Provider.Id,log10(pData$Average.Total.Payments), type="p", pch=20, 
      lty=2, col="blue")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)


## ----------------

#pData<-select(pData,Average.Covered.Charges,Average.Total.Payments)

ptData<-as.matrix(t(select(pData,Average.Covered.Charges,Average.Total.Payments)))

colnames(ptData)<-pData$Provider.Id

# ptData$Average.balance<-ptData[2,]-ptData[1,]

dim(ptData)
par(mfrow=c(1,1))

# pdf("plot1.pdf")

h1<-mean(pData$Average.Covered.Charges)
h2<-mean(pData$Average.Total.Payments)

barplot(ptData, ylab= "Amount in $",xlab="Service provider Id"
        beside=TRUE, col=c("blue","red"),
        main=paste("Around $",round(h1-h2,0)," gap between average covered charges and payement in NY State"))



abline(h=h1,cex=3,col="blue",lty=5)
text(100,h1+1000,col="blue",paste("Average Covered Charges","-$",round(h1,0)))



abline(h=h2,cex=3,col="red",lty=5)
text(50,h1-3000,col="red",bg="yellow",paste("Average payment","-$",round(h2,0)))

text

diff<-ptData[2,]-ptData[1,]

# ptData<-rbind(ptData,diff)
# 
# ptData2<-ptData[3,]

# barplot(diff, main="Autos", ylab= "Total",
#         beside=TRUE, col=c("red"))

# boxplot(pData$Average.Covered.Charges~pData$Provider.Id)

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("Average Covered Charges","Average Payment"), cex=1, 
       bty="n", fill=c("blue","red"))

dev.copy(pdf,"plot1.pdf",paper="A4r",width=640,height=480)
dev.off()

