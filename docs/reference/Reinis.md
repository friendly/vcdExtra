# Risk Factors for Coronary Heart Disease

Data from the beginning of a 15-year follow-up study of probable risk
factors for coronary thrombosis. The data are from 1841 men employed in
a Czechoslovakian car factory and represent all possible combinations of
the six risk factors.

## Format

A 6-dimensional array resulting from cross-tabulating 6 binary variables
for 1841 observations. The variable names and their levels are:

|     |           |                                                       |
|-----|-----------|-------------------------------------------------------|
| No  | Name      | Levels (meaning)                                      |
| 1   | `smoke`   | `"y", "n"` (smoking)                                  |
| 2   | `mental`  | `"y", "n"` (strenuous mental work)                    |
| 3   | `phys`    | `"y", "n"` (strenuous physical work)                  |
| 4   | `systol`  | `"y", "n"` (systolic blood pressure \> 140)           |
| 5   | `protein` | `"y", "n"` (ratio of beta to alpha lipoproteins \> 3) |
| 6   | `family`  | `"y", "n"` (family history of CHD)                    |

## Source

Originally from the gRbase package as `data(reinis, package = "gRbase")`

## Details

The study was conducted to examine risk factors for coronary heart
disease (CHD) and collected information on smoking habits, mental and
physical work strain, systolic blood pressure, ratio of lipoproteins,
and family history of CHD.

The six variables form a \\2^6 = 64\\ contingency table. The data have
been used extensively to illustrate model search procedures for
high-dimensional contingency tables.

## References

Edwards, D. and Havranek, T. (1985). A fast procedure for model search
in multidimensional contingency tables. *Biometrika*, 72, 339-351.

Gauraha, N. and Parui, S. K. (2020). Mutual conditional independence and
its applications to model selection in Markov networks. *Annals of
Mathematics and Artificial Intelligence*, 88, 951-972.
[doi:10.1007/s10472-020-09690-7](https://doi.org/10.1007/s10472-020-09690-7)

Reinis, Z., Pokorny, J., Basika, V., Tiserova, J., Gorican, K.,
Horakova, D., Stuchlikova, E., Havranek, T., and Hrabovsky, F. (1981).
Prognostic significance of the risk profile in the prevention of
coronary heart disease. *Bratis. lek. Listy*, 76, 137-150.

## Examples

``` r
data(Reinis)
str(Reinis)
#>  'table' num [1:2, 1:2, 1:2, 1:2, 1:2, 1:2] 44 40 112 67 129 145 12 23 35 12 ...
#>  - attr(*, "dimnames")=List of 6
#>   ..$ smoke  : chr [1:2] "y" "n"
#>   ..$ mental : chr [1:2] "y" "n"
#>   ..$ phys   : chr [1:2] "y" "n"
#>   ..$ systol : chr [1:2] "y" "n"
#>   ..$ protein: chr [1:2] "y" "n"
#>   ..$ family : chr [1:2] "y" "n"
ftable(Reinis, row.vars = 1:3)
#>                   systol    y               n            
#>                   protein   y       n       y       n    
#>                   family    y   n   y   n   y   n   y   n
#> smoke mental phys                                        
#> y     y      y             44   5  23   7  35   4  24   4
#>              n            129   9  50   9 109  14  51   5
#>       n      y            112  21  70  14  80  11  73  13
#>              n             12   1   7   2   7   5   7   4
#> n     y      y             40   7  32   3  12   3  25   0
#>              n            145  17  80  16  67  17  63  14
#>       n      y             67   9  66  14  33   8  57  11
#>              n             23   4  13   3   9   2  16   4

# Fit all 0-way through 6-way models using Kway()
Reinis.gmodels <- Kway(Freq ~ smoke + mental + phys + systol + protein + family,
                       data = Reinis, family = poisson)

# Examine fit statistics
LRstats(Reinis.gmodels)
#> Likelihood summary table:
#>            AIC     BIC LR Chisq Df Pr(>Chisq)    
#> kway.0 2318.39 2320.55  2026.74 63     <2e-16 ***
#> kway.1 1147.61 1162.72   843.96 57     <2e-16 ***
#> kway.2  381.00  428.50    47.35 42     0.2634    
#> kway.3  395.25  485.92    21.60 22     0.4841    
#> kway.4  412.83  535.89     9.18  7     0.2400    
#> kway.5  416.05  552.05     0.39  1     0.5300    
#> kway.6  417.65  555.82     0.00  0     <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Sequential tests for k vs k+1 way effects
anova(Reinis.gmodels, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model 1: Freq ~ 1
#> Model 2: Freq ~ smoke + mental + phys + systol + protein + family
#> Model 3: Freq ~ smoke + mental + phys + systol + protein + family + smoke:mental + 
#>     smoke:phys + smoke:systol + smoke:protein + smoke:family + 
#>     mental:phys + mental:systol + mental:protein + mental:family + 
#>     phys:systol + phys:protein + phys:family + systol:protein + 
#>     systol:family + protein:family
#> Model 4: Freq ~ smoke + mental + phys + systol + protein + family + smoke:mental + 
#>     smoke:phys + smoke:systol + smoke:protein + smoke:family + 
#>     mental:phys + mental:systol + mental:protein + mental:family + 
#>     phys:systol + phys:protein + phys:family + systol:protein + 
#>     systol:family + protein:family + smoke:mental:phys + smoke:mental:systol + 
#>     smoke:mental:protein + smoke:mental:family + smoke:phys:systol + 
#>     smoke:phys:protein + smoke:phys:family + smoke:systol:protein + 
#>     smoke:systol:family + smoke:protein:family + mental:phys:systol + 
#>     mental:phys:protein + mental:phys:family + mental:systol:protein + 
#>     mental:systol:family + mental:protein:family + phys:systol:protein + 
#>     phys:systol:family + phys:protein:family + systol:protein:family
#> Model 5: Freq ~ smoke + mental + phys + systol + protein + family + smoke:mental + 
#>     smoke:phys + smoke:systol + smoke:protein + smoke:family + 
#>     mental:phys + mental:systol + mental:protein + mental:family + 
#>     phys:systol + phys:protein + phys:family + systol:protein + 
#>     systol:family + protein:family + smoke:mental:phys + smoke:mental:systol + 
#>     smoke:mental:protein + smoke:mental:family + smoke:phys:systol + 
#>     smoke:phys:protein + smoke:phys:family + smoke:systol:protein + 
#>     smoke:systol:family + smoke:protein:family + mental:phys:systol + 
#>     mental:phys:protein + mental:phys:family + mental:systol:protein + 
#>     mental:systol:family + mental:protein:family + phys:systol:protein + 
#>     phys:systol:family + phys:protein:family + systol:protein:family + 
#>     smoke:mental:phys:systol + smoke:mental:phys:protein + smoke:mental:phys:family + 
#>     smoke:mental:systol:protein + smoke:mental:systol:family + 
#>     smoke:mental:protein:family + smoke:phys:systol:protein + 
#>     smoke:phys:systol:family + smoke:phys:protein:family + smoke:systol:protein:family + 
#>     mental:phys:systol:protein + mental:phys:systol:family + 
#>     mental:phys:protein:family + mental:systol:protein:family + 
#>     phys:systol:protein:family
#> Model 6: Freq ~ smoke + mental + phys + systol + protein + family + smoke:mental + 
#>     smoke:phys + smoke:systol + smoke:protein + smoke:family + 
#>     mental:phys + mental:systol + mental:protein + mental:family + 
#>     phys:systol + phys:protein + phys:family + systol:protein + 
#>     systol:family + protein:family + smoke:mental:phys + smoke:mental:systol + 
#>     smoke:mental:protein + smoke:mental:family + smoke:phys:systol + 
#>     smoke:phys:protein + smoke:phys:family + smoke:systol:protein + 
#>     smoke:systol:family + smoke:protein:family + mental:phys:systol + 
#>     mental:phys:protein + mental:phys:family + mental:systol:protein + 
#>     mental:systol:family + mental:protein:family + phys:systol:protein + 
#>     phys:systol:family + phys:protein:family + systol:protein:family + 
#>     smoke:mental:phys:systol + smoke:mental:phys:protein + smoke:mental:phys:family + 
#>     smoke:mental:systol:protein + smoke:mental:systol:family + 
#>     smoke:mental:protein:family + smoke:phys:systol:protein + 
#>     smoke:phys:systol:family + smoke:phys:protein:family + smoke:systol:protein:family + 
#>     mental:phys:systol:protein + mental:phys:systol:family + 
#>     mental:phys:protein:family + mental:systol:protein:family + 
#>     phys:systol:protein:family + smoke:mental:phys:systol:protein + 
#>     smoke:mental:phys:systol:family + smoke:mental:phys:protein:family + 
#>     smoke:mental:systol:protein:family + smoke:phys:systol:protein:family + 
#>     mental:phys:systol:protein:family
#> Model 7: Freq ~ smoke + mental + phys + systol + protein + family + smoke:mental + 
#>     smoke:phys + smoke:systol + smoke:protein + smoke:family + 
#>     mental:phys + mental:systol + mental:protein + mental:family + 
#>     phys:systol + phys:protein + phys:family + systol:protein + 
#>     systol:family + protein:family + smoke:mental:phys + smoke:mental:systol + 
#>     smoke:mental:protein + smoke:mental:family + smoke:phys:systol + 
#>     smoke:phys:protein + smoke:phys:family + smoke:systol:protein + 
#>     smoke:systol:family + smoke:protein:family + mental:phys:systol + 
#>     mental:phys:protein + mental:phys:family + mental:systol:protein + 
#>     mental:systol:family + mental:protein:family + phys:systol:protein + 
#>     phys:systol:family + phys:protein:family + systol:protein:family + 
#>     smoke:mental:phys:systol + smoke:mental:phys:protein + smoke:mental:phys:family + 
#>     smoke:mental:systol:protein + smoke:mental:systol:family + 
#>     smoke:mental:protein:family + smoke:phys:systol:protein + 
#>     smoke:phys:systol:family + smoke:phys:protein:family + smoke:systol:protein:family + 
#>     mental:phys:systol:protein + mental:phys:systol:family + 
#>     mental:phys:protein:family + mental:systol:protein:family + 
#>     phys:systol:protein:family + smoke:mental:phys:systol:protein + 
#>     smoke:mental:phys:systol:family + smoke:mental:phys:protein:family + 
#>     smoke:mental:systol:protein:family + smoke:phys:systol:protein:family + 
#>     mental:phys:systol:protein:family + smoke:mental:phys:systol:protein:family
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
#> 1        63    2026.74                         
#> 2        57     843.96  6  1182.78   <2e-16 ***
#> 3        42      47.35 15   796.61   <2e-16 ***
#> 4        22      21.60 20    25.75   0.1741    
#> 5         7       9.18 15    12.42   0.6472    
#> 6         1       0.39  6     8.79   0.1860    
#> 7         0       0.00  1     0.39   0.5300    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Fit sequential models of joint independence
Reinis.seqjoint <- seq_loglm(Reinis, type = "joint", prefix="joint")
LRstats(Reinis.seqjoint)
#> Likelihood summary table:
#>            AIC    BIC LR Chisq Df Pr(>Chisq)    
#> joint.1  22.89  21.58     3.56  1   0.059011 .  
#> joint.2  47.48  45.64     9.66  1   0.001879 ** 
#> joint.3 775.91 776.31   709.78  3  < 2.2e-16 ***
#> joint.4 133.78 140.73    14.83  7   0.038183 *  
#> joint.5 280.68 305.60    67.76 15  1.116e-08 ***
#> joint.6 397.57 468.82    41.92 31   0.091126 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Fit sequential models of conditional independence
Reinis.seqcond <- seq_loglm(Reinis, type = "conditional", prefix = "cond")
LRstats(Reinis.seqcond)
#> Likelihood summary table:
#>            AIC     BIC LR Chisq Df Pr(>Chisq)    
#> cond.1   22.89   21.58     3.56  1   0.059011 .  
#> cond.2   47.48   45.64     9.66  1   0.001879 ** 
#> cond.3   74.12   74.60     5.99  2   0.050096 .  
#> cond.4  839.59  845.77   722.65  8  < 2.2e-16 ***
#> cond.5  936.14  950.80   737.22 22  < 2.2e-16 ***
#> cond.6 1147.51 1173.42   833.86 52  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
