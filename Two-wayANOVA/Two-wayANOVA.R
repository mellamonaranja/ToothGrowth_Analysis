#Two-way ANOVA(Analysis of Variance)#

#Import the data
data("ToothGrowth")
str(ToothGrowth)

#Change factor to vector
ToothGrowth$dose=factor(ToothGrowth$dose, levels = c(0.5,1,2),labels = c('D0.5','D1','D2'))
str(ToothGrowth)
View(ToothGrowth)

#Create the graph(boxplot, interaction plot)
boxplot(len~supp * dose,
        data=ToothGrowth,
        frame=FALSE,
        col=c('#00AFBB','#E7B800'),ylab = 'Tooth Length')

interaction.plot(x.factor = ToothGrowth$dose, trace.factor = ToothGrowth$supp, response = ToothGrowth$len, fun = mean, type = 'b', legend = TRUE, xlab = 'Dose', ylab = 'Tooth Length', pch = c(1,19), col = c('#00AFBB','#E7B800'))

#Statistic Analysis-Homogeneity of variance
#I : Levene Test
library(car)
leveneTest(len~supp*dose, data=ToothGrowth)

#Verify ANOVA(Analysis of Variance)
#Hypothesis I : Two variables are independent
tooth.aov1=aov(len~supp+dose, data=ToothGrowth)

#Get the ANOVA result easily
summary(tooth.aov1)

#Hypothesis II : Two variables aren't independent
tooth.aov2=aov(len~supp*dose, data=ToothGrowth)
summary(tooth.aov2)

#Appendix, Multicomparision Test : TukeyHSD
TukeyHSD(tooth.aov2,which = 'dose')

pairwise.t.test(ToothGrowth$len, ToothGrowth$dose, p.adjust.method = 'BH')
