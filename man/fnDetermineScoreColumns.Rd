\name{fnDetermineScoreColumns.}
\alias{fnDetermineScoreColumns.}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to determine which column or columns in a data frame contain (numeric) scores.
%% ~~function to do ... ~~
}
\description{
fnDetermineScoreColumns is designed to determine which column or columns in a data frame contain (numeric) scores. It operates based on a series of conditionals which evaluate aspects such as whether the variable is numeric, whether the values have a range typical of scoring schemes, or have ranges narrower or broader than scoring schemes used in PSMD.  

}
\usage{
fnDetermineScoreColumns.(Dataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dataframe}{
Dataframe is a data frame of which you want to determine the possible score column(s).
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
DZ250417: This is currently an experimental function and will be updated to handle real data more accurately.

DZ250417: Build in a probabilistic element to provide the single best-guess if length(Possible.Score.Columns)>1.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fnDetermineScoreColumns.(dataExample) will return a list of column names for the columns the function thinks could be scores.

In the case of dataExamples, this is multiple possibilities. In reality, there is likely to be fewer. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
