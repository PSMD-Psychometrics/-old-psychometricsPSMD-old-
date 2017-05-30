\name{theme_psmd}
\alias{theme_psmd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A custom ggplot theme for PSMD.
}
\description{
A custom ggplot theme for PSMD.
}
\usage{
theme_psmd()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{()}{
theme_psmd() does not require any arguments.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Uses data from Data.Example in PSMD.Psychometrics
## Requires ggplot2 to be loaded

ggplot(data=Data.Example, aes(x=Grade.UBSE)) + 
  geom_bar(stat="count", position=position_dodge()) +
  ggtitle("Basic Bar-Plot") +
  theme_psmd()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
