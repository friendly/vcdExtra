# example:

library(car)


logits(work=dichotomy(c("fulltime", "parttime"), "not.work"),
       full=dichotomy("fulltime", "parttime"))


m <- nestedLogit(partic ~ hincome + children, 
                 logits(work=dichotomy(c("fulltime", "parttime"), "not.work"),
                        full=dichotomy("fulltime", "parttime")),
                 data=Womenlf)
m
summary(m)
Anova(m)

m2 <- nestedLogit(partic ~ log(hincome)*children, 
                  logits(work=dichotomy(c("fulltime", "parttime"), "not.work"),
                         full=dichotomy("fulltime", "parttime")),
                  data=Womenlf)
m2
summary(m2)
Anova(m2)

(new <- expand.grid(hincome=seq(1, 45, length=10), children=c("absent", "present")))

(p <- predict(m2, newdata=new))
rowSums(p)
p2 <- predict(m2)
all(abs(rowSums(p2) - 1) < .Machine$double.eps)

plotdata <- cbind(new, p)

op <- par(mfrow=c(1, 2))
plot(c(1, 45), 0:1, type="n", xlab="Husband's Income", ylab="Probability",
     main="Children Absent")
with(plotdata, lines(spline(hincome[1:10], fulltime[1:10]), col="blue", lwd=2))
with(plotdata, lines(spline(hincome[1:10], parttime[1:10]), col="magenta", lwd=2))
with(plotdata, lines(spline(hincome[1:10], not.work[1:10]), col="darkcyan", lwd=2))
legend("topright", inset=.02, lwd=2, col=c("blue", "magenta", "darkcyan"),
       legend=c("Full Time", "Part Time", "Not Working"))

plot(c(1, 45), 0:1, type="n", xlab="Husband's Income", ylab="Probability",
     main="Children Present")
with(plotdata, lines(spline(hincome[11:20], fulltime[11:20]), col="blue", lwd=2))
with(plotdata, lines(spline(hincome[11:20], parttime[11:20]), col="magenta", lwd=2))
with(plotdata, lines(spline(hincome[11:20], not.work[11:20]), col="darkcyan"), lwd=2)
par(op)

