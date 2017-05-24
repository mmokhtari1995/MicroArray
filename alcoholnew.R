# -------------------- TASK 1 ----------------------------------
#3. read data
dataset <- read.table(file='C:\\Users\\avon\\Desktop\\hy390\\GDS4879.soft', sep = '\t', na.strings = c("NA" , "null" ), header=TRUE, stringsAsFactors = FALSE)
 
#4. subseting null
newdata <- dataset[ complete.cases( dataset ) ,]
newdata <- newdata[ 1:10000,]
#newdata <- newdata[ which(dataset$GSM1085677!="NA.strings")]


#5. take second column
gene.names <- newdata[,2:2] 



#6. take 24 samples

#delete 2 first columns
newdata <- newdata[c(-1,-2)]

#delete unnecessary columns by name
finaldata <- subset(newdata, select = -c(
GSM1085680,
GSM1085680,
GSM1085682,
GSM1085683,
GSM1085684,
GSM1085687,
GSM1085691,
GSM1085697,
GSM1085700,
GSM1085686,
GSM1085688,
GSM1085690,
GSM1085692,
GSM1085693,
GSM1085702,
GSM1085703) )
 
#view data and columns that left
View(finaldata)
colnames(finaldata)


# -------------------- TASK 2 ----------------------------------
#boxplot
#convert finaldata to numeric
dataNum <- matrix(data = NA, nrow = dim(finaldata)[1], ncol = dim(finaldata)[2])
 
for (i in 1:dim(finaldata)[2]) {
    dataNum[,i] <- c(as.numeric(finaldata[[i]]))
}

#boxplot(dataNum, ylim= c(0,15))




# -------------------- TASK 3 ----------------------------------
#heatmap
		#library
if (!require("gplots")) {
   install.packages("gplots", dependencies = TRUE)
   library(gplots)
   }
if (!require("RColorBrewer")) {
   install.packages("RColorBrewer", dependencies = TRUE)
   library(RColorBrewer)
   }
		#create table 
nba <- finaldata
matrix <- data.matrix(nba)
		#heatmap1
#heatmap <- heatmap(matrix)
		#heatmap2
#heatmap.2 <- heatmap.2(matrix, scale="none",  trace = "none")


 #PCA
finaldatanew <- t(matrix)
temp <- prcomp(finaldatanew)
colors <-as.factor(c(rep('yellow',6),rep('green',6),rep('blue',6),rep('red',6)))
plot(temp$x[,1], temp$x[,2], xlab = 'pc1', ylab = 'pc2')
plot(temp$x[,1], temp$x[,2], xlab = 'pc1', ylab = 'pc2',col=colors)

  
  
  
  
##------TASK 5 & 6---------
#building the outcomes
nonalcoholicsValcoholics <-  as.factor(rep(c(rep('alcoholics', 5),rep('non-alcoholics',5)),2))
femalesVmales <- as.factor(c(rep('females', 12),rep('males',12)))


#applying on all row genes
myTtest <- function(x,y){
  levs <- unique(y);
  a <- x[y == levs[1]]
  b <- x[y == levs[2]]
  res <- t.test(a, b, var.equal = TRUE)
  res$p.value
}

#save the results (p-values) of females and males in a vector
pvalues_femaleVmale <- apply(matrix, 1, myTtest, femalesVmales)
#View(pvalues_femaleVmale)
#ascending order of pvalues_femaleVmale
ordered_femaleVmale <- pvalues_femaleVmale[order(pvalues_femaleVmale)]
#take the first 100 minimum pvalues
list1=head(ordered_femaleVmale ,100)





#save the results (p-values) of nonalcoholics and alcoholics in a vector
pvalues_nonalcoholicsValcoholics <- apply(matrix, 1, myTtest, nonalcoholicsValcoholics)
#View(pvalues_nonalcoholicsValcoholics)
#ascending order of pvalues_nonalcoholicsValcoholic
ordered_nonalcoholicsValcoholics <- pvalues_nonalcoholicsValcoholics[order(pvalues_nonalcoholicsValcoholics)]
#take the first 100 minimum pvalues
list2=head(ordered_nonalcoholicsValcoholics ,100)



for (i in 1:length(list1) ) {
	for (y in 1:length(pvalues_femaleVmale) ) {
		if( pvalues_femaleVmale[y] == list1[i])
			write.table(matrix[y],file='C:\\Users\\avon\\Desktop\\hy390\\tmp.txt',append=TRUE,sep = "\t")
	}
}


for (i in 1:length(list2) ) {
	for (y in 1:length(pvalues_nonalcoholicsValcoholics) ) {
		if( pvalues_nonalcoholicsValcoholics[y] == list2[i])
			write.table(matrix[y],file='C:\\Users\\avon\\Desktop\\hy390\\tmp2.txt',append=TRUE,sep = "\t")
	}
}

  
