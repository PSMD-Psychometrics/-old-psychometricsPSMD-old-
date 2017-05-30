\name{dataExample}
\alias{dataExample}
\docType{data}
\title{
dataExample Documentation
}
\description{
A dataset containing example data from 20 students, included assorted variables of scores, grades, stage etc.
}
\usage{data("dataExample")}
\format{
  A data frame with 20 rows and 11 variables:
  \describe{
   \item{Student.ID}{Student identification numbers (Numeric)}
   \item{Gender}{Student Gender ("Male"/"Female", String)}
   \item{Ethnicity}{Student Ethnicity ("White"/"Asian"/"Other", String)}
   \item{Disability}{Student Disability ("No known disability"/"Specific learning difficulty"/"Other disability", String)}
   \item{Stage}{Student stage (Numeric)}
   \item{Score.Perc}{Student percentage scores, two decimal places (Numeric)}
   \item{Score.Raw}{Student raw scores, integers (Numeric)}
   \item{Score.Restricted}{Student raw scores, integers between 0-25 (Numeric)}
   \item{Score.Neg}{Student percentage scores including some negative values (Numeric)}
   \item{Grade.UBSE}{Grades in a UBSE scheme (Abbreviated String)}
   \item{Grade.USE}{Grades in a USE scheme (Abbreviated String)}
   \item{Grade.CNINC}{Grades in a CNINC scheme (String)}
   \item{Grade.PF}{Grades in a PF scheme (Abbreviated String)}
   \item{Grade.PFE}{Grades in a PFE scheme (Abbreviated String)}
   \item{Responses.CIDK}{Responses in a CIDK scheme (Abbreviated String)}
   \item{Responses.CIDK.Numeric}{Responses in a CIDK scheme (Numeric)}
   \item{Assessor.ID}{Assessor identification numbers (Numeric)}
   \item{Element.Score}{Element scores, integers between 0-4 (Numeric)}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Example data created by the PSMD Psychometrics team.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
dataExample
}
\keyword{datasets}
