# Script containing plot functions for {PSMD.Psychometrics} - MR 240417

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Notes ####
# Required packages: Hmisc

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  INSTALL ALL REQUIRED PACKAGES
#for minor tick marks
#if (!require("Hmisc")) { install.packages("Hmisc ", dependencies = TRUE) }
#library("Hmisc")

#################################################################################
# Function to plot percentage score histogram + rugplot + gradebar + boxplot	#
#################################################################################
Fn.ScoreHist<-function(x, lo=0, hi=100, gradeBounds=c(18.25,25.62,45.19), gradeCols=c("red","orange","green","blue"), main=""){
	# Requires the "Hmisc" package
	# ARGUMENTS
	# x is a vector of percentage scores (max 100 but may be negative)
	# lo and hi are the required extremes of the x axis
	# gradeBounds is a numeric vector of grade thresholds
	# gradeCols is a character vector of colours for the gradebar
	# Note: length(gradeCols) should equal 1 + length(gradeBounds)
	if(min(x,na.rm=TRUE)<lo) {lo<-10*floor(min(x,na.rm=TRUE)/10)}	# If min(x) < lo then lo is adjusted accordingly, usually to account for negative scores	
	layout(matrix(c(1,2,3),3,1),heights=c(2.0,0.2,1))		#sets the layout up so there's one big, one thin, and one average sized plot

	# Add HISTOGRAM to the first area
	par(font.lab=2, cex.axis=1.5, cex.lab=1.3, cex.main=1.5, mar=c(2, 4.1, 4, 2) )
	hist(x,breaks=seq(lo,hi,1),main=main, xlab="", ylab = "Frequency (N students)",col="maroon")
	axis(1, at=seq(lo,hi,10), labels=TRUE)
	minor.tick(nx=20, ny=0, tick.ratio=0.25)
	rug(x)	# Add a rugplot to histogram
	
	# Derive the stacked barchart values for the grade bar in the second area
	bounds<-as.matrix(c(lo,diff(c(lo,gradeBounds,hi))))	
	# Add BARPLOT showing grades in the second area. 
	par(font.lab=2, cex.axis=1.5, cex.lab=1.5,cex.main=1.8,mar=c(1,4.1,1,2))
	barplot(bounds, width=1, horiz=TRUE, beside=FALSE, xlim=c(lo-0.04*(hi-lo),hi+0.04*(hi-lo)),col=c(NA,gradeCols),border=FALSE)
	axis(1, at=seq(lo,hi,10), labels=TRUE)
	minor.tick(nx=20, ny=0, tick.ratio=0.25)
	
	# Add BOXPLOT in the third area	
	par(font.lab=2, cex.axis=1.5, cex.lab=1.5,cex.main=1.8,mar=c(5.1,4.1,3,2))
	boxplot(x,horizontal=TRUE,col="maroon",ylim=c(lo,hi),xlab="Score (%)",boxwex=1.2,medlwd=2,whisklty="solid",whisklwd=2,staplelwd=2,outpch=8,outcex=1.5)
	points(mean(x,na.rm=TRUE),1,pch=10,cex=2,col="white")
	axis(1, at=seq(lo,hi,10), labels=TRUE)
	minor.tick(nx=20, ny=0, tick.ratio=0.25)
}
# Test using random data with an NA value and possible negative scores
#Fn.ScoreHist(c(rnorm(86,30,10),NA))
