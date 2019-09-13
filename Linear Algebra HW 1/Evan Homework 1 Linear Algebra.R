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
plot(pcaOut$x[,1],pcaOut$x[,2],col = leuk$V5001, cex = 2, xlab = "Principal Component 1", ylab = "Principal Component 2", main = 'Genetic Samples Projected into 2-dimensions')

############################ Using ggplot ##########################################
library(ggplot2)
plotdata = data.frame(
  PC1 = pcaOut$x[,1],
  PC2 = pcaOut$x[,2],
  Type = leuk$V5001
)

ggplot(plotdata, aes(x = PC1, y=PC2)) + geom_point(aes(colour = Type),size=3) + 
  scale_color_manual(values = c("ALL-B" = "black", "ALL-T" = "purple", "AML" = "green")) +
  labs(title='Genetic Samples Projected into 2-dimensions', x='Principal Component 1', y='Principal Component 2') +
  theme(legend.position = 'right')+
  geom_text(aes(label=as.numeric(rownames(plotdata))),hjust=0, vjust=0, size=4)

