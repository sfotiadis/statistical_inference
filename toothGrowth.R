attach(ToothGrowth)

# counts
library(knitr)
kable(table(supp,dose))

# basic stats
means <- aggregate(len, by=list(supp,dose), FUN=mean) 
stdev <- aggregate(len, by=list(supp,dose), FUN=sd)
stats <- cbind(means, stdev$x)
colnames(stats) <- c("Supplement type", "Dose", "Mean", "Standard Deviation")
kable(stats)

# basis stats2 
library(psych)

stats2 <- describeBy(len, interaction(supp, dose, sep=" "), mat = TRUE)
stats2$item <- NULL
stats2$vars <- NULL
stats2$trimmed <- NULL
stats2$mad <- NULL
stats2$range <- NULL

stats2
colnames(stats2) <- c("Group", "Samples", "Mean", "Std. dev.", "Median", "Min", "Max", "Skew", "Kurtosis", "SE")
rownames(stats2) <- stats2$group
kable(stats2)

# box plot

par(mfrow=c(1, 1))
boxplot(len ~ interaction(supp, dose, sep="/"),data=ToothGrowth,
        xlab="Group (supp/dose)",
        ylab="Tooth length (microns)",
        main='Guinea pig tooth growth')


#par(mfrow=c(1, 2))
#boxplot(len ~ dose,data=ToothGrowth,
#        xlab="Vitamin C dose (mg)",
#        ylab="Tooth length (microns)")
#boxplot(len ~ supp,data=ToothGrowth,
#        xlab="Supplement type",
#        ylab="Tooth length (microns)")
#mtext("Guinea Pig Tooth Growth", side=3, outer=TRUE, line=-2.5)

# t-tests
library(stats)

t <- pairwise.t.test(len, interaction(supp, dose, sep=" "), paired=FALSE, p.adjust="bonf" )
str(t)
kable(t$p.value)


library(knitr)
library(rmarkdown)
knit("toothGrowth.Rmd")
render("toothGrowth.Rmd", "pdf_document")

