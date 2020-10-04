library(dplyr)
library(ggplot2)
data(ToothGrowth)
summary(ToothGrowth)

suppDose <- ToothGrowth %>% 
    group_by(supp, dose) %>%
    summarize(avgLen = mean(len))

g <- ggplot(suppDose, aes(x=dose, y=avgLen)) +
    geom_line() +
    facet_wrap(  ~ supp) +
    ggtitle("Average Tooth Length by Supplement and Dosage")
g

g2 <- ggplot(ToothGrowth, aes(len)) +
    geom_histogram() +
    facet_wrap(supp ~ dose)
g2

t.test(ToothGrowth[ToothGrowth$dose == 0.5,]$len, ToothGrowth[ToothGrowth$dose == 1,]$len)$p.value
t.test(ToothGrowth[ToothGrowth$dose == 1,]$len, ToothGrowth[ToothGrowth$dose == 2,]$len)$p.value

t.test(ToothGrowth[ToothGrowth$supp == "OJ",]$len, ToothGrowth[ToothGrowth$supp == "VC",]$len)$p.value

OJSupp <- ToothGrowth[ToothGrowth$supp == "OJ",]
VCSupp <- ToothGrowth[ToothGrowth$supp == "VC",]

ggplot(OJSupp[OJSupp$dose == 0.5,], aes(len)) +
    geom_histogram(bins = 30)


t.test(OJSupp$len, VCSupp$len)

t.test(OJSupp[OJSupp$dose == 0.5,]$len, VCSupp[VCSupp$dose == 0.5,]$len)$p.value
t.test(OJSupp[OJSupp$dose == 1,]$len, VCSupp[VCSupp$dose == 1,]$len)$p.value
t.test(OJSupp[OJSupp$dose == 2,]$len, VCSupp[VCSupp$dose == 2,]$len)$p.value

p <- c(
    t.test(ToothGrowth[ToothGrowth$dose == 0.5,]$len, ToothGrowth[ToothGrowth$dose == 1,]$len)$p.value,
    t.test(ToothGrowth[ToothGrowth$dose == 0.5,]$len, ToothGrowth[ToothGrowth$dose == 2,]$len)$p.value,
    t.test(ToothGrowth[ToothGrowth$dose == 1,]$len, ToothGrowth[ToothGrowth$dose == 2,]$len)$p.value,
    t.test(ToothGrowth[ToothGrowth$supp == "OJ",]$len, ToothGrowth[ToothGrowth$supp == "VC",]$len)$p.value
)

p <- c(
    p,
    t.test(OJSupp[OJSupp$dose == 0.5,]$len, VCSupp[VCSupp$dose == 0.5,]$len)$p.value,
    t.test(OJSupp[OJSupp$dose == 1,]$len, VCSupp[VCSupp$dose == 1,]$len)$p.value,
    t.test(OJSupp[OJSupp$dose == 2,]$len, VCSupp[VCSupp$dose == 2,]$len)$p.value
)

p.adjust(p, method = "BH") < 0.05
