\name{fnScoreHist}
\alias{fnScoreHist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram of student assessment scores with optional boxplot and background shading by grade ranges if requested.
}
\description{
fnScoreHist creates a histogram of scores. Optional arguments permit a boxplot to be drawn above the histogram and the plot background to be shaded according to supplied grade boundaries.

The output from fnScoreHist is a ggplot object which can then be saved or customised as needed.
 
See Examples for more details.
}
\usage{
fnScoreHist(x,lo=0,hi=100,gradeScheme="",gradeBounds=c(),main="",ylab="Frequency (N students)",addBox=TRUE,pcScore=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of assessment scores (usually percentages but may exceed 100 or be negative).      
}
  \item{lo}{
Minimum value for plotting the x axis, best given as an appropriate multiple of 10 because the axis labels are spaced in tens. If min(x)<lo then lo is adjusted downwards (in multiples of 10)
}
  \item{hi}{
Maximum value for plotting the x axis, If max(x)>hi then hi is adjusted upwards (in multiples of 10)
}
  \item{gradeScheme}{
A character string giving the first letter of the grades, ordered from lowest to highest. e.g. "UBSE" or "FP". Letters other than U, B, S, E, F or P will be displayed but not shaded.
If gradeScheme is not specified then the background will be unshaded
}

  \item{gradeBounds}{
A numeric vector of grade thresholds. e.g. c(40,50,60).
NOTE: length(gradeBounds) must equal nchar(gradeScheme) - 1, otherwise the background will be unshaded
}

  \item{main}{
Title for the plot, if required.
}

  \item{ylab}{
Label for the y axis.
}

  \item{addBox}{
If TRUE (the default) adds a boxplot with mean point above the histogram.
}

  \item{pcScore}{
If TRUE changes the x axis label from "Score" to "Score (\%)".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
A ggplot object which can be saved or customised as needed.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Martin Roberts, e:psychometrics@plymouth.ac.uk
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# fnScoreHist Help file
	library("ggplot2")
	library("psychometricsPSMD")
	?fnScoreHist

# Basic Usage (examples employ randomly generated data so the resulting plots are variable)
	fnScoreHist(x=rnorm(86,30,10),lo=-10,hi=80,gradeScheme="UBSE",gradeBounds=c(20,30,45))

# Ability to accommodate NA values and to adjust lo and hi (set to 0 and 80 in this example) to fit data
	fnScoreHist(x=c(-1,2,rnorm(86,30,10),65,93,100,NA),lo=0,hi=80,gradeScheme="USE",gradeBounds=c(18.25,25.62))
 
# If length(gradeBounds) not equal to nchar(gradeScheme) - 1 then no background shading is carried out
	fnScoreHist(x=rnorm(86,30,10),gradeScheme="FP",gradeBounds=c(18.25,25.62))
	
# Scores need not be percentages and the boxplot can be removed
	fnScoreHist(x=rnorm(86,150,20),lo=100,hi=200,gradeScheme="FP",gradeBounds=c(135),addBox=FALSE,pcScore=FALSE)

# Grade boundaries falling outside the lo-hi interval will cause the shading and grade labelling to fail
	fnScoreHist(x=rnorm(86,150,20),lo=100,hi=200,gradeScheme="US",gradeBounds=c(55),pcScore=FALSE)
	fnScoreHist(x=rnorm(86,150,20),lo=50,hi=200,gradeScheme="US",gradeBounds=c(55),pcScore=FALSE)

# Outputs a ggplot object, which can them be editted as necessary.
	plotExample<-fnScoreHist(x=rnorm(86,50,15),gradeScheme="FP",gradeBounds=c(44))
	plotExample  # plot output
	plotExample+ggtitle("Example Distribution of Assessment Scores") # edits
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line