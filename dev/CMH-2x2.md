# CMHtest 2 x 2 display

## Idea
A new idea to try to make the results of the CMH test easier to understand: Take the 4 $\chi^2$ values
and put these in a 2 x 2 table, with labels c("General", "Ordered"). 

This would allow a final row / column of the difference in $\chi^2$ for each separately (rmeans, cmeans), and combined (cor)
Taking the df difference into account, could add "*"s to the cells to indicate significance.


```
> CMHtest(Freq ~ ses + mental, data=Mental)
Cochran-Mantel-Haenszel Statistics for ses by mental 

                 AltHypothesis  Chisq Df       Prob
cor        Nonzero correlation 37.156  1 1.0907e-09
rmeans  Row mean scores differ 40.297  5 1.3012e-07
cmeans  Col mean scores differ 40.666  3 7.6971e-09
general    General association 45.958 15 5.4003e-05
```

For the results above this would give something like:

| row\col  |  gen                  | ord                  | diff                |
| gen      | $X^2_{15} = 45.958$   | $X^2_{3} = 40.666$   | $X^2_{12} = 5.29$   |
| ord      | $X^2_{3}  = 40.297$   | $X^2_{1} = 37.156$   | $X^2_{2} = 3.14$    |
| diff     | $X^2_{12} = 5.662$    | $X^2_{2} =  3.510$   |

## Implementation notes

* What to show in each cell:
  - The values of X^2 and their degrees of freedom, as above
  - Perhaps more informative would be X^2 / df -- it would display the relative strength of evidence for alternative, per degree of freedom

* Use of stars: In the example above, all tests are highly signif in the table, so none is distinguished. Try other examples, perhaps
  synthetic ones to see the impact.

