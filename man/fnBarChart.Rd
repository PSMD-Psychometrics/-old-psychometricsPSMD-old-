\name{fnBarChart}
\alias{fnBarChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency or Percentage bar-charts of Grades or Responses.
}
\description{
fnBarChart creates Frequency or Percentage bar-charts of Grades or Responses (and select characteristics such as Gender, Ethnicity, and Disability). It is designed to require only minimal input, but will request additional arguments if it cannot distinguish between two schemes (e.g. UBSE and USE when there are no B grades in Data).
  
fnBarChart has also been designed to be as autonomous as possible, and can handle most variations in Data values, e.g. c("U","b","Satisfactory","E","excellent","S") will still be recognised as using a UBSE scheme, and the plot will relabel the values accordingly.

The output from Fn.BarChart is a ggplot object which can then be saved or customised as needed.  
  
See Examples for more details.
}
\usage{
fnBarChart(Data, Type, Force.Scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
Data should be a vector of values representing grades or responses, e.g. c("U","B","S","S","E") or Data.All$Grades.

Factors will be coerced into suitable formats, data frames and lists will return a prompt to check the format.

fnBarChart can handle combinations of capitalisation, alphanumeric or numeric abbreviations, and full-text labels for UBSE, USE, UBS, CIDK, CNINC, PFE, and PF schemes.

The function can also handle selected themes outside of those related to responses and grades, though scheme detection is more restricted and less reliable. e.g. Gender, Ethnicity (will be reduced to "White"/"Asian"/"Other"), and Disability (will be reduced to "No Known Disability"/"Specific Learning Difficulty"/"Other Disability").      
}
  \item{Type}{
Type can be set to "Frequency" or "Percentage", and determines the y-axis scale. Type="Frequency" plots counts of each value in Data, whereas Type="Percentage" plots the percentage of each value in Data.
  
fnBarChart can handle variations on "Frequency" and "Percentage" (within reason), e.g. "Perc" or "f"
}
  \item{Force.Scheme}{
Force.Scheme is an optional argument that can take the values "USE", "UBS", "PF", "CIDK", and "CNINC". It is requested when the function cannot determine a single scheme from the values in Data. For example, if Data consists of a string of "C" values, the function will ask the user to specify Force.Scheme="CIDK" or Force.Scheme="CNINC", or if Data only contains U, S, and E grades (or variations), it will ask the user specify Force.Scheme="USE" if the scheme should be USE rather than UBSE.

These pieces of additional information allow the correct handling of USE, UBS, and PF schemes, and C-only Data.
  
Force.Scheme has no effect when a scheme can be determined or on totally unknown themes (unknown as opposed to indeterminate).
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
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr Daniel Zahra, e:psychometrics@plymouth.ac.uk
}
\note{
DZ260417: Added a fix for Type being converted to a factor after the reference table being used to handle abbreviated type names. This seems to work, but it needs checking with respect to different classes of Type object and associated error messages.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# fnBarChart Help file; can handle UBSE, USE, UBS, CIDK, CNINC, PFE, PF, and Gender

  ?fnBarChart

# Basic Usage; Data examples from Data.Example in PSMD.Psychometrics

  fnBarChart(Data=dataExample$Grade.UBSE, Type="Percentage")
  
  fnBarChart(Data=dataExample$Response.CIDK, Type="Frequency")

# Ability to handle variation in Data values (within schemes)

  Data<-c("u","e","b","satisfactory","S","s","E")
  fnBarChart(Data,Type="Percentage")
  
  Data<-c("C","I","Incorrect","Correct","correct","DK","dont know", "Don't Know")
  fnBarChart(Data,Type="Percentage")

# Warnings when either or both inputs are missing

  fnBarChart()
  
  fnBarChart(Type="Percentage")
  
# Warnings when either or both inputs are of the wrong type or a type that can't be coerced into the correct type.
  
  head(dataExample)
  fnBarChart(dataExample, Type="Percentage") # e.g. can't input a whole data.frame (this example) 

  fnBarChart(Data=dataExample$Response.CIDK, Type="Chickens") # e.g. if an argument value is incorrect

# If there is a rogue value in Data, a plot is generate but a warning raised.
  
  Data<-c("U","B","S","E","E","DK") # Grade or response value but inconsistent with scheme.
  fnBarChart(Data,Type="Percentage")
  
  Data<-c("U","B","S","E","E","W") # Value that isn't a grade or response, warning specifies unknown value.
  fnBarChart(Data,Type="Percentage")
  
# If there is too little information in Data to determine or differentiate a scheme, instructions are displayed
  
  Data<-c("U","S","S","S","E","E") # e.g. could be UBSE where noone was awarded B, or could be USE scheme.
  fnBarChart(Data,Type="Percentage") # Defaults to UBSE, but asks if USE and explains how to correct.
  fnBarChart(Data,Type="Percentage", Force.Scheme = "USE")

# Outputs a ggplot object, which can them be editted as necessary.
  
  Plot.BarChart.CNINC<-fnBarChart(Data=dataExample$Grade.CNINC, Type="Frequency")
  Plot.BarChart.CNINC # plot output
  Plot.BarChart.CNINC+ggtitle("C-NI-NC Response Frequencies") # edits
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
