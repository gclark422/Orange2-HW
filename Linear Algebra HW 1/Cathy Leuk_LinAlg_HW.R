# Linear Algebra 
# Case Studies with PCA
# Gene Missed labeled
# September 10, 2019
# Cathy Tran

# Color Scheme Library
library(RColorBrewer)
display.brewer.all()
palette(brewer.pal(n = 8, name = "Dark2"))

# Reads the data set
load('/Users/CathyTran/Documents/Fall I 2019/Linear Algebra/HW Assignment/LeukError.RData')

# Freq ct of leuk type
table(leuk$V5001)

# ALL-B  ALL-T   AML 
# 19     7       12 

# Matrix Dimension
dim(leuk.x)

# Explore the data
# Pick two column numbers at random, then draw the plot, coloring by the label.
randomColumns = sample(1:5000,2)
plot(leuk[,randomColumns],col = leuk$V5001)

# Compute the first two principal components 
pcaOut = prcomp(leuk[,1:5000],3, scale = F)
plot(pcaOut$x[,1],pcaOut$x[,2],col = leuk$V5001, xlab = "Principal Component 1", ylab = "Principal Component 2", main = 'Leukemia Samples Projected into 2-dimensions')

plot(pcaOut$x[,1],pcaOut$x[,2],col = leuk$V5001, cex = 2, xlab = "Principal Component 1", ylab = "Principal Component 2", main = 'Genetic Samples Projected into 2-dimensions')

# There's 1 purple AML dot is with the ALL-T group
# The green colored dots are ALL-B - all 19 of the data pts are there!

# Observation 19 is misclassified a AML(Green). Should be ALL-T (Red).
text(pcaOut$x[,1], pcaOut$x[,2], labels=(as.numeric(rownames(leuk))), data=leuk, cex=1, font=2)

legend(40000, -13000, legend = c("AML", "ALL-T", "ALL_B"), col=c("green", "red", "black"), lty=1:2, cex=0.7, pch = 1)