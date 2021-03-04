oregon <- readr::read_csv("oregon.csv")

plot(Y.2016 ~ Y.2012, data=oregon, type="n",
     main="Democratic vote shares in Oregon: 2016 vs 2012",
     xlim = range(Y.2016, Y.2012),
     ylim = range(Y.2016, Y.2012))
abline(a=0, b=1, col="green", lty=2)
text(Y.2016 ~ Y.2012, data=oregon, 
     labels=County, cex = 0.5,
     xlim = range(Y.2016, Y.2012),
     ylim = range(Y.2016, Y.2012))
legend("topleft", bty="n", cex = 0.7,
       legend = "y = x",
       lty=2, col="green")

mod <- lm(Y.2016 ~ Y.2012, data = oregon)
par(mfrow=c(1,2))
plot(mod, which=c(1,2),
     labels.id = oregon$County,
     pch=19)
confint(mod)
confint(mod, level = .9)
t.test(mod$coefficients)


new.xvalue <- data.frame(Y.2012=seq(0, 100, by=0.5))

conf_interval <- predict(mod, newdata = new.xvalue,
                         interval = "confidence", level = 0.95)
pred_interval <- predict(mod, newdata = new.xvalue,
                         interval = "prediction", level = 0.95)


plot(Y.2016 ~ Y.2012, data=oregon, type="n",
     main="Democratic vote shares: 2016 vs 2012",
     xlim = range(Y.2012, Y.2016), ylim = range(Y.2012, Y.2016))

DescTools::DrawBand(y = pred_interval[, 2:3],
                    x = new.xvalue[,1], col = "grey90")
DescTools::DrawBand(y = conf_interval[, 2:3],
                    x = new.xvalue[,1], col = "grey80")

abline(mod, col='red') # regr line

abline(0,1, col="green", lty=2)                       # y=x

text(Y.2016 ~ Y.2012, data=oregon, 
     labels=County,
     cex=0.5)

legend("topleft", bty = "n",
       legend=c("y=x", "Regression Line"),
       lty=c(2,1), col=c("green","red"))
predict(mod, newdata=oregon, interval="prediction", level=0.95)

florida = readr::read_csv("florida.csv")
head(florida)
palm = subset(florida, County=="Palm") # extract Palm County

lin.mod = lm(Buch.Votes ~ Reg.Reform, data=florida)

par(mfrow=c(1, 1))
plot(Buch.Votes ~ Reg.Reform, data=florida, 
     main = "Registered Voters vs Actual Votes", 
     xlab = "Registered Reform Voters", ylab = "Actual Votes", 
     col = "black",
     pch=19)

abline(lin.mod, col = "blue")

# Identify Palm County on the plot
text(Buch.Votes ~ Reg.Reform, data=palm, 
     label='Palm', pos=2, col="red")

florida.NoPalm = florida[-which(florida$County=="Palm"),]
lin.mod.NoPalm = lm(Buch.Votes ~ Reg.Reform, data=florida.NoPalm) 

plot(Buch.Votes ~ Reg.Reform, data=florida, 
     main = "Registered Voters vs Actual Votes", 
     xlab = "Registered Reform Voters", ylab = "Actual Votes",
     col = "black",
     pch = 19)
abline(lin.mod.NoPalm, col = "green")  # new regression line

abline(lin.mod, col = "blue")  # old regression line


text(Buch.Votes ~ Reg.Reform, data=palm, 
     label='Palm', pos=2, col="red")

legend("topleft", 
       legend=c("With Palm county", 
                "Without Palm county"),
       lwd=1, col=c('blue','green'))
logmod <- lm(log(Buch.Votes) ~ log(Reg.Reform), data=florida.NoPalm)
