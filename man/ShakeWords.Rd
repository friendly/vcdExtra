\name{ShakeWords}
\alias{ShakeWords}
\docType{data}
\title{
Shakespeare's Word Type Frequencies
}
\description{
This data set, from Efron and Thisted (1976), 
gives the number of distinct words types (\code{Freq}) of words
that appeared exactly once, twice, etc. up to 100 times (\code{count})
in the complete works of Shakespeare.  In these works, Shakespeare
used 31,534 distinct words (types), comprising 884,647 words in total.

Efron & Thisted used this data to ask the question, "How many
words did Shakespeare know?"  Put another way, suppose another
new corpus of works Shakespeare were discovered, also with
884,647 words. How many new word types would appear?
The answer to the main question involves contemplating
an infinite number of such new corpora.

}
\usage{data(ShakeWords)}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{count}}{the number of times a word type appeared in Shakespeare's written works}
    \item{\code{Freq}}{the number of different words (types) appearing with this count.}
  }
}
\details{
In addition to the words that appear \code{1:100} times, there are 846 words 
that appear more than 100 times, not listed in this data set. 
}
\source{
Bradley Efron and Ronald Thisted (1976).
Estimating the Number of Unsen Species: How Many Words Did Shakespeare Know?
\emph{Biometrika},
Vol. 63, No. 3, pp. 435-447, 
\url{http://www.jstor.org/stable/2335721}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(ShakeWords)
## maybe str(ShakeWords) ; plot(ShakeWords) ...
}
\keyword{datasets}
