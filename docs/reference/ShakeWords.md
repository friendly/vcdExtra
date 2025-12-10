# Shakespeare's Word Type Frequencies

This data set, from Efron and Thisted (1976), gives the number of
distinct words types (`Freq`) of words that appeared exactly once,
twice, etc. up to 100 times (`count`) in the complete works of
Shakespeare. In these works, Shakespeare used 31,534 distinct words
(types), comprising 884,647 words in total.

## Format

A data frame with 100 observations on the following 2 variables.

- `count`:

  the number of times a word type appeared in Shakespeare's written
  works

- `Freq`:

  the number of different words (types) appearing with this count.

## Source

Bradley Efron and Ronald Thisted (1976). Estimating the Number of Unseen
Species: How Many Words Did Shakespeare Know? *Biometrika*, Vol. 63, No.
3, pp. 435-447,

## Details

Efron & Thisted used this data to ask the question, "How many words did
Shakespeare know?" Put another way, suppose another new corpus of works
Shakespeare were discovered, also with 884,647 words. How many new word
types would appear? The answer to the main question involves
contemplating an infinite number of such new corpora.

In addition to the words that appear `1:100` times, there are 846 words
that appear more than 100 times, not listed in this data set.

## Examples

``` r
data(ShakeWords)
str(ShakeWords)
#> 'data.frame':    100 obs. of  2 variables:
#>  $ count: int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Freq : num  14376 4343 2292 1463 1043 ...

plot(sqrt(Freq) ~ count, data=ShakeWords)

```
