#Load in R data table

load('C:\\Users\\97420\\OneDrive\\Documents\\MSA Fall\\Linear Algebra\\Homework\\LeukError.RData')
View(leuk)
#38 observations 5001 variables

table(leuk$V24)
#Eg: should see value 20 for almost all 38 obs.

leuk$V5001
# 3 type of Leukemia: AML; ALL-B; ALL-T


#View(leuk)


library(RColorBrewer)
display.brewer.all()

#Make a scatterplot that's less cool

randomColumns = sample(1:5000,2)
plot(leuk[,randomColumns],col = leuk$V5001)


#Create the first two principal components

pcaOut = prcomp(leuk[,1:5000],3, scale = F)
plot(pcaOut$x[,1],pcaOut$x[,2],col = leuk$V5001, 
        xlab = "Principal Component 1", 
        ylab = "Principal Component 2", 
        main = 'Genetic Samples Projected into 2-dimensions')
text(pcaOut$x[,1], pcaOut$x[,2], labels=(as.numeric(rownames(leuk))), 
     data=leuk, cex=0.5, font=2)
legend(40000, -13000, legend = c("AML", "ALL-T", "ALL_B"), 
       col=c("green", "red", "black"), lty=1:2, cex=0.7)
# Observation 19 is misclassified a AML(Green). Should be ALL-T (Red).