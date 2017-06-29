############################### fnScoreHist ##############################
# A function to plot a histogram and boxplot of student assessment scores
# If specified, the background is shaded according to the awarded grades
	# Written by: Martin Roberts
	# Last updated: 29/06/2017
	# ARGUMENTS
	# x   A vector of scores (usually percentages but may be negative)
	# lo   Minimum value for plotting the x axis
	# hi   Maximum value for plotting the x axis
	# gradeScheme   A character string giving the first letter of the grades. e.g. "UBSE"
	# gradeBounds   A numeric vector of grade thresholds. e.g. c(40,50,60) NOTE: length(gradeBounds) should equal nchar(gradeScheme) - 1 
	# main   Title for the plot, if required
	# ylab   Label for the y axis
	# addBox   If TRUE (the default) adds a boxplot with mean point above the histogram
	# pcScore  If TRUE changes the x axis label from "Score" to "Score (%)"
	# PACKAGE REQUIREMENTS
	# Needs psychometricsPSMD and ggplot2
################################################
if (!require("ggplot2")) { install.packages("ggplot2", dependencies = TRUE) }
library("ggplot2")
if (!require("psychometricsPSMD")) { install_github("PSMD-Psychometrics/psychometricsPSMD", force=TRUE) }
library("psychometricsPSMD")
#######
fnScoreHist<-function(x,lo=0,hi=100,gradeScheme="",gradeBounds=c(),main="",ylab="Frequency (N students)",addBox=TRUE,pcScore=TRUE){
	xdf<-as.data.frame(x)
	if(min(x,na.rm=TRUE)<lo) {lo<-10*floor(min(x,na.rm=TRUE)/10)}	# If min(x)<lo then lo is adjusted accordingly (in multiples of 10)
	if(max(x,na.rm=TRUE)>hi) {hi<-10*ceiling(max(x,na.rm=TRUE)/10)}	# If max(x)>hi then hi is adjusted accordingly (in multiples of 10)
	maxfreq<-max(hist(x,breaks=seq(lo,hi,1),plot=FALSE)$counts)	
	gradeLabels<-strsplit(gradeScheme,"")[[1]]
	gradeCols=c(U='#D92120',B='#E68B33',S='#86BB6A',E='#3D52A1',F='#D92120',P='#86BB6A')[gradeLabels]
	Ngrades<-nchar(gradeScheme)
	gradeSwitch<-(Ngrades>0 & Ngrades==length(gradeBounds)+1) 	# If TRUE, background is shaded according to grade boundaries
	if(gradeSwitch) {rectsGB<-data.frame(xstart=c(lo,gradeBounds), xend=c(gradeBounds,hi+1), Grade=gradeLabels)}
	rectsBox<-data.frame(xstart=boxplot.stats(x)$stats[-5], xend=boxplot.stats(x)$stats[-1], 
					ystart=c(maxfreq*1.1,maxfreq*1.05,maxfreq*1.05,maxfreq*1.1),yend=c(maxfreq*1.1,maxfreq*1.15,maxfreq*1.15,maxfreq*1.1))
	outliers<-boxplot.stats(x)$out 	#Saves data needed to plot outliers on boxplot
	ggplot() + 
		theme_psmd() + 
		theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),axis.line.x=element_blank(),axis.ticks.length=unit(4,"pt")) +
		scale_x_continuous(name=paste("Score",if(pcScore){" (%)"}),breaks=seq(lo,hi,10),limits=c(lo,hi+1),expand=c(0,0)) +
		annotate(geom="segment",x=seq(lo,hi+1,1),xend=seq(lo,hi+1,1),y=0,yend=-0.1) +  # Adds minor ticks
		scale_y_continuous(name=ylab,breaks=seq(0,maxfreq,ifelse(maxfreq>20,10*ceiling(maxfreq/100),2)),expand=c(0,0)) +	
		{if(gradeSwitch) geom_rect(data=rectsGB,aes(xmin=xstart,xmax=xend,ymin=0,ymax=maxfreq*1.24,fill=Grade),fill=gradeCols[1:Ngrades],col="grey40",alpha=0.5)} +
		{if(gradeSwitch) annotate(geom="text",x=(rectsGB$xstart + rectsGB$xend)/2, y=rep(maxfreq*1.2,length(gradeLabels)),label=gradeLabels,col=gradeCols[1:Ngrades],alpha=0.7,size=6,fontface=2)} +
		geom_histogram(data=xdf,aes(xdf),breaks=seq(lo,hi+1,1),col="black",fill=ifelse(gradeSwitch,"grey40","maroon"),na.rm=TRUE,closed="right") +
		theme(axis.line=element_line(colour="black")) +
		labs(title=main) +
		{if(addBox) geom_rect(data=rectsBox,aes(xmin=xstart,xmax=xend,ymin=ystart,ymax=yend),fill=ifelse(gradeSwitch,"grey80","maroon"),col="grey20",size=1)} +
		{if(addBox) geom_point(data=data.frame(x=outliers,y=rep(maxfreq*1.1,length(outliers))),aes(x=x,y=y),shape=8,col="grey20",size=1.5)} +
		{if(addBox) geom_point(data=data.frame(x=mean(x,na.rm=TRUE),y=maxfreq*1.1),aes(x=x,y=y),shape=9,col="grey10",size=3)} +
		{if(addBox) geom_point(data=data.frame(x=lo,y=maxfreq*1.16),aes(x=x,y=y),shape=1,col="black",size=0)}
}
################################################
# EXAMPLES
# fnScoreHist(x=c(0,2,rnorm(86,30,10),65,93,100,NA),gradeScheme="UBSE",gradeBounds=c(18.25,22.62,40.19))
# fnScoreHist(x=c(-1,2,rnorm(86,30,10),65,93,100,NA),gradeScheme="",gradeBounds=c(18.25,25.62,45.19))
# fnScoreHist(x=c(0,2,rnorm(86,30,10),65,93,100,NA),gradeScheme="FP",gradeBounds=c(18.25))
# fnScoreHist(x=c(0,2,rnorm(86,30,10),65,93,100,NA),gradeScheme="FP",gradeBounds=c(18.25),addBox=FALSE)
# fnScoreHist(x=c(0,2,rnorm(86,30,10),75,NA),hi=80,gradeScheme="FP",gradeBounds=c(),addBox=FALSE,pcScore=FALSE)
# fnScoreHist(x=c(NA,rnorm(150,15,3),30),gradeScheme="",hi=30,gradeBounds=c(),pcScore=FALSE)