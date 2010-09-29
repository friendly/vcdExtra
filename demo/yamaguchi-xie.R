## Models for Yamaguchi (1987) data on social mobility in US, UK and Japan, following Xie (1992)
## These models reproduce the results in Table 1, appplied to the off-diagonal
## cells

library(gnm)
library(vcdExtra)

data(Yamaguchi87)
# create table form
Yama.tab <- xtabs(Freq ~ Son + Father + Country, data=Yamaguchi87)

# define labeling_args for convenient reuse in 3-way displays
largs <- list(rot_labels=c(right=0), offset_varnames = c(right = 0.6), offset_labels = c(right = 0.2),
              set_varnames = c(Son="Son's status", Father="Father's status") 
             )

# no association between S and F given country ('perfect mobility')
# asserts same associations for all countries

yamaNull <- gnm(Freq ~ (Son + Father) * Country, data=Yamaguchi87, family=poisson)
summarize(yamaNull)
mosaic(yamaNull, ~Country + Son + Father, condvars="Country", 
	labeling_args=largs,
	main="[SC][FC] Null SF association (perfect mobility)")

# same, with data in xtabs form
yamaNull <- gnm(Freq ~ (Son + Father) * Country, data=Yama.tab, family=poisson)
summarize(yamaNull)
mosaic(yamaNull, ~Country + Son + Father, condvars="Country", 
	labeling_args=largs,
	main="[SC][FC] Null SF association (perfect mobility)")

# ignore diagonal cells, overall; NB: switch to gnm()
yamaDiag0 <- gnm(Freq ~ (Son + Father) * Country + Diag(Son, Father), data=Yama.tab, family=poisson)
summarize(yamaDiag0)
# same, using update()
yamaDiag0 <- update(yamaNull, ~ . + Diag(Son, Father))
summarize(yamaDiag0)

# ignore diagonal cells in each Country [Model NA in Xie(1992), Table 1]
yamaDiag <- update(yamaNull, ~ . + Diag(Son, Father):Country)
summarize(yamaDiag)
#mosaic(yamaDiag, ~Country + Son + Father, condvars="Country", main="[SC][FC] Quasi Perfect mobility")

mosaic(yamaDiag, ~Country + Son + Father, condvars="Country", 
	main="[SC][FC] Quasi Perfect mobility",
	labeling_args=largs,
  gp=shading_Friendly)

# fit models using integer scores for rows/cols 
Rscore <- as.numeric(Yamaguchi87$Son)
Cscore <- as.numeric(Yamaguchi87$Father)

# cross-nationally homogeneous row effect associations (Xie, model R_o)
yamaRo <- update(yamaDiag, ~ . + Rscore:Father)
summarize(yamaRo)

# cross-nationally log multiplicative row effect associations (Xie, model R_x)
yamaRx <- update(yamaDiag, ~ . + Mult(Rscore:Father, Exp(Country)))
summarize(yamaRx)

# cross-nationally homogeneous col effect associations (Xie, model C_o)
yamaCo <- update(yamaDiag, ~ . + Son:Cscore)
summarize(yamaCo)

# cross-nationally log multiplicative col effect associations (Xie, model C_x)
yamaCx <- update(yamaDiag, ~ . + Mult(Son:Cscore, Exp(Country)))
summarize(yamaCx)

# cross-nationally homogeneous row and col effect associations I (Xie, model (R+C)_o)
yamaRpCo <- update(yamaDiag, ~ . + Rscore:Father + Son:Cscore)
summarize(yamaRpCo)
mosaic(yamaRpCx, ~Country + Son + Father, condvars="Country", 
		labeling_args=largs,
		main="Model RpCx: log multiplicative iS + jF : Country")

# cross-nationally log multiplicative row and col effect associations I (Xie, model (R+C)_x)
yamaRpCx <- update(yamaDiag, ~ . + Mult(Rscore:Father + Son:Cscore, Exp(Country)))
summarize(yamaRpCx)

# cross-nationally homogeneous row and col effect associations II (Xie, model RC_o)
yamaRCo <- update(yamaDiag, ~ . + Mult(Son,Father))
summarize(yamaRCo)

# cross-nationally log multiplicative row and col effect associations II (Xie, model RC_x)
yamaRCx <- update(yamaDiag, ~ . + Mult(Son,Father, Exp(Country)))
summarize(yamaRCx)
mosaic(yamaRCx, ~Country + Son + Father, condvars="Country", 
		labeling_args=largs,
		main="Model RCx: log multiplicative RC : Country")

yamaFIo <- update(yamaDiag, ~ . + Son:Father)
summarize(yamaFIo)

yamaFIx <- update(yamaDiag, ~ . + Mult(Son:Father, Exp(Country)))
summarize(yamaFIx)

models <- glmlist(yamaNull, yamaDiag, yamaRo, yamaRx, yamaCo, yamaCx, yamaRpCo, yamaRpCx, yamaRCo, yamaRCx, yamaFIo, yamaFIx)
summarize(models)
