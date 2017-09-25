setwd("C:/Users/Tom Kelly/Documents/Downloads/20170927_Stats_Workshop_Osumi_Group")

data <- rnorm(1000000, 1, 1.25)
data1 <- rnorm(100, 1, 1.25)
data2 <- rnorm(50, 1.25, 1)

pdf("1_distribution.pdf", width=800/75, 600/75)
plot(density(data), main="distribution", ylab="frequency", xlab="value of data")
abline(v=mean(data))
abline(v=mean(data)-sd(data), col="grey")
abline(v=mean(data)-2*sd(data), col="grey")
abline(v=mean(data)+sd(data), col="grey")
abline(v=mean(data)+2*sd(data), col="grey")
dev.off()

pdf("2_boxplot.pdf", width=800/75, 600/75)
boxplot(data1, data2, col=c("red", "blue"), names=c("group A", "group B"), xlab="categorical group", ylab="continuous outcome", main="difference between groups?")
dev.off()

pdf("3_distribution_compare.pdf", width=800/75, 600/75)
plot(density(data2), main="distribution", ylab="frequency", xlab="value of data", col="blue")
lines(density(data1), main="distribution", ylab="frequency", xlab="value of data", col="red")
legend("topright", fill=c("red", "blue"), legend=c("group A", "group B"))
dev.off()

pdf("4_t_distribution.pdf", width=800/75, 600/75)
plot(density(data2), main="distribution", ylab="frequency", xlab="value of data (t)", col="blue")
lines(density(data1), main="distribution", ylab="frequency", xlab="value of data", col="red")
lines(density(data), main="distribution", ylab="frequency", xlab="value of data")
legend("topright", fill=c("red", "blue", "black"), legend=c("group A", "group B", "normal (z)"))
dev.off()

t.test(data1, data2) #p.val = 0.029, t=-2.2051

dataset <- cbind(data1, data2)
colnames(dataset) <- c("placebo", "treatment")
write.csv(dataset, file="1_exercise_t_test.csv")

data3 <- rnorm(100, 0.95, 2.25)
data4 <- rnorm(50, 1.05, 2)

pdf("5_boxplot.pdf", width=800/75, 600/75)
boxplot(data3, data4, col=c("red", "blue"), names=c("group A", "group B"), xlab="categorical group", ylab="continuous outcome", main="difference between groups?")
dev.off()

pdf("6_distribution_compare.pdf", width=800/75, 600/75)
plot(density(data4), main="distribution", ylab="frequency", xlab="value of data", col="red")
lines(density(data3), main="distribution", ylab="frequency", xlab="value of data", col="blue")
legend("topright", fill=c("red", "blue"), legend=c("group A", "group B"))
dev.off()

t.test(data3, data4) #p.val = 0.3624, t=--0.91469

dataset <- cbind(data3, data4)
colnames(dataset) <- c("placebo", "treatment")
write.csv(dataset, file="1_example_t_test.csv")

example <- rbinom(100, 4, 0.5)
anime <- factor(c("never", "monthly", "weekly", "daily")[example+1], levels=c("never", "monthly", "weekly", "daily"))
table(anime)
sex <- factor(c("Male", "Female")[c(rep(1, 50), rep(2, 50))])
table(anime, sex)

fisher.test(table(anime, sex))
chisq.test(table(anime, sex)) #chi.sq = 0.80299, p.val = 0.8488

write.csv(table(anime, sex), file="2_example_chi_sq_test.csv")

example <- rbinom(100, 2, 0.5)
smoking <- factor(c("never", "ex-smoker", "current smoker")[example+1], levels=c("never", "ex-smoker", "current smoker"))
table(smoking)
cancer_diagnosed <- factor(c("Positive", "Negative")[c(rep(1, 50), rep(2, 50))])
table(smoking, cancer_diagnosed)

fisher.test(table(smoking, cancer_diagnosed))
chisq.test(table(smoking, cancer_diagnosed)) #chi.sq = 0.80299, p.val = 0.8488

write.csv(table(smoking, cancer_diagnosed), file="2_exercise_chi_sq_test.csv")

example <- rbinom(100, 1, 0.5)
risk_allele <- factor(c("A", "T")[example+1], levels=c("A", "T"))
table(risk_allele)
batten_disease <- factor(c("Case", "Control")[c(rep(1, 50), rep(2, 50))])
table(risk_allele, batten_disease)

fisher.test(table(risk_allele, batten_disease))
chisq.test(table(risk_allele, batten_disease)) #chi.sq = 0.80299, p.val = 0.8488

write.csv(table(risk_allele, batten_disease), file="3_exercise_fishers_test.csv")


fit <- lm(data1~sex+risk_allele)
summary(fit)
anova(fit)

cholesterol <- c(rnorm(50, 110, 30), rnorm(50, 90, 35))

fit <- lm(cholesterol~sex+risk_allele)
summary(fit)
anova(fit)

fit <- lm(cholesterol~sex*risk_allele)
summary(fit)
anova(fit)

boxplot(cholesterol~sex)
t.test(cholesterol~sex)

boxplot(cholesterol~risk_allele)
t.test(cholesterol~risk_allele)

dataset <- cbind(cholesterol, sex, risk_allele)
write.csv(dataset, file="4_exercise_anova.csv")
