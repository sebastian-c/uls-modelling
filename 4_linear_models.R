#' ---
#' title: "Data cleaning and linear regression"
#' format: 
#'   html:
#'     self-contained: true
#' ---
#' 

library(knitr)
library(data.table)
library(ellipse)
library(RColorBrewer)
library(ggplot2)
library(knitr)
library(MASS)

opts_chunk$set(echo = FALSE)
options(knitr.kable.NA = '')


#' 
#' # Data processing
#' 
#' The data was obtained from the ATMO Auvergne Rhône-Alpes website (https://www.atmo-auvergnerhonealpes.fr/). We chose two towns: Lyon and Annecy. Lyon is the capital of the region and we thought it would have a distinct profile from Annecy, which is much smaller and nestled in the mountains. We took all the data that was available for the whole period:
#' 
#' - PM10: Fine particles < 10 µm (µg/m^2)
#' - monoxyde_azote: Nitrogen monoxide (NO, µg/m^2)
#' - dioxyde_azote: Nitrogen dioxide (NO~2~, µg/m^2)
#' 
#' While the ATMO website does have an API, it's not designed to be used directly – it only serves to feed an interactive data visualization on their webpage. This doesn't pose too much of a problem as it's possible to use Google Chrome's Developer tools to isolate the REST calls and backwards-engineer the API. 
#' 
#' The API has another quirk that makes it a bit tricky to use. The granularity of the data is dependent on the length of the timeseries requested. For example, hourly data is given for a timeseries length of < 7 days but annual data is given if the timer series is > 3 years. To get around this, we pulled data 1 year at a time allowing us to get monthly data. We then aggregated this to be monthly. 
#' 
#' 
#' # Linear regression
#' 
#' Beyond autocorrelative trends, there exist also more "classical" relationships between variables. As a second part to this report, we will examine the relationships between air quality variables. 
#' 
#' ## Necessity of simple linear regression
#' 
#' While there's clearly an autocorrelation component, there are still uses in this dataset for classical regression. It's useful for sensors which may not have access to historical data and for situations where historical values aren't relevant such as measuring contaminants after an environmental accident. It's also very easy to interpret with a clear link between the predictors and the response.
#' 
#' We would like to predict particulates (PM10) based on nitrogen monoxide (`monoxyde_azote`, NO) and nitrogen dioxide (`dioxyde_azote`, NO~2~) levels
#' 

raw_airquality <- fread("data/annecy_airquality.csv")
airquality_vars <- raw_airquality[,c("PM10", "dioxyde_azote", "monoxyde_azote")]

#' 
#' 
#' 
#' 
#' ## Correlation
#' 
#' As is standard, we'll start with correlations between our variables:
#' 
#' 

#cor_colours <- brewer.pal(5, "Spectral")
#cor_colours <- colorRampPalette(cor_colours)(100)

quality_cor <- cor(airquality_vars)
quality_cor[upper.tri(quality_cor)] <- NA
kable(round(quality_cor, 2), format="html")

plotcorr(quality_cor, col="cornflowerblue")

#' 
#' The above graph shows ellipses which correspond to the strength of correlation (Pearson). We can see that nitrogen dioxide (NO~2~) and nitrogen monoxide(NO) are highly correlated (0.83), which shouldn't be surprising as they are both grouped under the NO~x~ category. Particulates also increase with NO (0.61) and NO~2~ (0.73) which makes sense as they're all connected to pollution, but the correlation is much weaker.
#' 
#' However, as Anscombe's quartet shows, correlation is not sufficient to characterise relationships between variables. One needs to use plots.
#' 

plot(airquality_vars)

#' <!--
#' One thing we can see is that the NO seems to have a disproportionate number of low values - this can indicate a non-linear relationship. A log transform will bring the higher and lower values closer together.
#' 

airquality_vars[,log_NO := log10(monoxyde_azote)]
#airquality_vars[,sq_NO := monoxyde_azote^2]
plot(airquality_vars[, c("PM10", "dioxyde_azote", "log_NO")])


#' The relationships now
#' -->
#' 
#' ### Linear model
#' 
#' We can be satisfied that the relationships are linear, but there is a high degree of collinearity between our predictor variables NO and NO~2~ (r = 0.83). We'll fit the entire model and fix this later. We're also interested in seeing if there's a difference between the different cities, Lyon and Annecy.
#' 


basic_model <- lm(PM10 ~ dioxyde_azote + monoxyde_azote + city, data = raw_airquality)
par(mfrow = c(2,2))
plot(basic_model)
par(mfrow = c(1,1))


#' 
#' We don't see an increase in spread with increasing magnitude nor do we see any vast deviations from normality. There are a number of values with high leverage however. Let's try to isolate those. We can use `hatvalues` to isolate high leverage values.
#' 

high_leverage <- which(hatvalues(basic_model) > 0.05)
raw_airquality[high_leverage, is_highlev := "high leverage"]
raw_airquality[is.na(is_highlev), is_highlev := "normal leverage"]

ggplot(raw_airquality, aes(x = dioxyde_azote, y = PM10, size = monoxyde_azote, colour = is_highlev)) +
  geom_point(alpha = 0.8) +
  scale_colour_manual(values = c("high leverage" = "red", "normal leverage" = "grey30")) +
  facet_wrap(~city) +
  labs(colour = "Leverage")


#' We have NO~2~ on the x axis, NO on the size axis, PM10 on the y axis and high leverage values in red. We can definitely see the high influence these values have. It's tempting to get rid of them, but they seem largely in line with the existing data. We also see something very interesting when we split these two cities apart. While they have similar values for NO~2~ and PM10, there's a very big difference. We theorize that this difference is due to burning of fuels for heating. This would also explain why the pollution spikes in winter. According to the US Environmental Protection Agency (EPA, https://www3.epa.gov/ttncatc1/dir1/fnoxdoc.pdf), NO emissions come principally from combustion and is largely anthropogenic. This is of no major concern however; unlike carbon monoxide, nitrogen monoxide isn't very soluble in water and so only poses a threat to infants and other sensitive individuals.
#' 


summary(basic_model)


#' We see that City and NO~2~ are good predictors, but what about the collinearity?
#' 
#' ### Stepwise regression
#' 
#' We'll use AIC as a criterion and progressively drop predictors from our model
#' 


stepAIC(basic_model, direction = "backward")


#' Automatic methods have failed us, performing the manual approach:
#' 

trimmed_model <- lm(PM10 ~ dioxyde_azote + city, data = raw_airquality)
summary(trimmed_model)

#' The trimmed model has similar predictive power. The AIC of this model is `r round(AIC(trimmed_model), 2)` whereas the original model had an AIC of `r round(AIC(basic_model), 2)`. We see that an increase in  1 µ/m^3 NO~2~ results in a 0.47 µ/m^3 increase in PM10.
#' 

raw_airquality[, trimmed_pred := predict(trimmed_model)]

ggplot(raw_airquality, aes(x = dioxyde_azote, y = PM10)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~city)

#' Blue line is model, gray area corresponds to Standard Error.
#' 
#' # Conclusions
#' 
#' - Annecy and Lyon differ in the NO pollutant levels, likely due to burning of fuel for heating
#' - We can predict PM10 levels by using NO~2~ levels
#' 
#' 

#purl("regression_report.qmd", "4_linear_models.R", documentation = 2)

