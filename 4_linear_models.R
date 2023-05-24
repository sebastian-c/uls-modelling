
library(knitr)
library(data.table)
library(ellipse)
library(RColorBrewer)
library(knitr)

opts_chunk$set(echo = FALSE)
options(knitr.kable.NA = '')




raw_airquality <- fread("data/annecy_airquality.csv")
airquality_vars <- raw_airquality[,c("PM10", "dioxyde_azote", "monoxyde_azote")]



#cor_colours <- brewer.pal(5, "Spectral")
#cor_colours <- colorRampPalette(cor_colours)(100)

quality_cor <- cor(airquality_vars)
quality_cor[upper.tri(quality_cor)] <- NA
kable(round(quality_cor, 2), format="html")

plotcorr(quality_cor, col="cornflowerblue")



plot(airquality_vars)



airquality_vars[,log_NO := log10(monoxyde_azote)]
#airquality_vars[,sq_NO := monoxyde_azote^2]
plot(airquality_vars[, c("PM10", "dioxyde_azote", "log_NO")])




basic_model <- lm(PM10 ~ dioxyde_azote + monoxyde_azote + city, data = raw_airquality)
par(mfrow = c(2,2))
plot(basic_model)
par(mfrow = c(1,1))




#purl("regression_report.qmd", "4_linear_models.R")

