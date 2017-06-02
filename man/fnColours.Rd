\name{fnColours}
\alias{fnColours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to provide a hex-code colour palette for a specified scheme or data vector.
}
\description{
fnColours takes either a specified scheme or a vector of data and returns (a) a character string of hex-codes called Colour.Palette to the global environment, and (b) the same string as a return from the function.

This allows fnColours to be used in isolation within a script to create Colour.Palette for use elsewhere, or to be used within plotting functions to provide a string of colours appropriate for the scheme/data being plotted. See 'Examples'.

Use fnColours(Info="Y") to display details of the various colours and schemes handled by the function. Note that although the abbreviations listed in recognised schemes are recommended if specifying a theme, fnColours can handle (some) variations in these, e.g. 'ubse', 'U/B/S/E', and 'u-b-s-e' will all be recognised as 'UBSE'. 
}
\usage{
fnColours(DataOrScheme, DropValue, Info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrScheme}{
Either a specific scheme or a vector of data. Schemes should be entered as "UBSE", "PF", "UBLSHSE" etc. Data should be entered as a single vector. Schemes such as "Assessor" and "Maroon" can also be used where a single colour is required. See 'Notes' for recognised/determined schemes. Where subset-schemes are required, e.g. PF, the scheme determined will be PFE. DropValue (see below) can be used to correct this by excluding unwanted elements.} 
  \item{DropValue}{
The DropValue argument allows specific elements of the scheme to be excluded. They shoul dbe entered as a single vector. For example, DropValue="S" will exclude S from any scheme including/requiring a colour for S, DropValue=c("B","S") will exclude B and S and their associated colours. See 'Notes' for limitations.}
  \item{Info}{
Optional argument. If Info="Y", fnColours returns a list object of package information listing hexcodes associated with each colour, recognised schemes and associated palettes, and a list of themes detectable from data inputs. Info="Y" over-rides all other function outputs.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr Daniel Zahra, e: psychometrics@plymouth.ac.uk
}
\note{
DZ170517: Any values entered in DropValue not being used in the scheme will be ignored.
  Also, no utility to drop specific cohorts from Cohort/Historic schemes.
  Add switch to supress comments.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Warnings for missing critical inputs

fnColours()
  
# Details of recognised schemes, their palettes, and hex codes

fnColours(Info="Y")

# Palettes for specific schemes

fnColours(DataOrScheme = "UBSE")
fnColours(DataOrScheme = "UBLSHSE")
fnColours(DataOrScheme = "Assessor")

# Can reverse the order of the palette

rev(fnColours("PF"))

# Palettes for data, including variation in input (within reason)

fnColours(DataOrScheme = dataExample$Grade.UBSE)
fnColours(DataOrScheme = c("U","E","Excellent","HS","HS","LS","borderline","HS"))

# Schemes and Data with excluded elements

fnColours(DataOrScheme = "UBSE", DropValue = c("B","S"))
fnColours(DataOrScheme = dataExample$Grade.UBSE, DropValue = "B")
fnColours(DataOrScheme = c(1,2,3,5,1,2,2,5,3), DropValue = c(4))

# Recommend using fnColours() to create objects of hexcodes for use later
# The reduces repetition of fnColours() comments.
# (until an option is included to supress them)

StageColours<-fnColours("Stage")

# Palette derived within plot functions (examples requires ggplot2)

ggplot(data=dataExample, aes(x=Stage, y=Score.Perc, fill=Grade.UBSE)) +
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values=fnColours("UBSE"))
  
ggplot(data=dataExample, aes(x=Stage, y=Score.Perc, fill=Grade.PFE)) +
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values=fnColours(dataExample$Grade.PFE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
\keyword{}
