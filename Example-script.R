### ---- R Bootcamp script ---- ### 
# load packages
library(WDI)
library(ggplot2)


####################
### Get WDI data ###
####################

# Define countries, indicators form above, and time period
wd.df <- WDI(country = "all", 
             indicator = c('population' = "SP.POP.TOTL", 
                           'gdp_pc' = "NY.GDP.PCAP.KD", 
                           'co2_pc' = "EN.ATM.CO2E.PC"),
             extra = TRUE, start = 2019, end = 2019)

# Drop all country aggregrates
wd.df <- wd.df[which(wd.df$region != "Aggregates"), ]

# Save data
save(wd.df, file = "WDI_short.RData")


#################
### Plot data ###
#################

### Ggplot of GDP and CO2
pl <-
  ggplot(wd.df, aes(
    x = gdp_pc,
    y = co2_pc,
    size = population,
    color = region
  )) +
  geom_point(alpha = 0.5) +
  theme_minimal() + scale_y_log10() +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(y = "CO2 emissions per capita", x = "GDP per capita")

# Didplay plot
pl


########################
### Summary measures ###
########################

# use log transformed variables
wd.df$ln_gdp_pc <- log(wd.df$gdp_pc)
wd.df$ln_co2_pc <- log(wd.df$co2_pc)

# calculate covariance
cov <- cov(wd.df$ln_gdp_pc, wd.df$ln_co2_pc, use = "complete.obs")

# determine complete observations
cobs <- complete.cases(wd.df)

# sd for complete observations only
sd_gdp <- sd(wd.df$ln_gdp_pc[cobs], na.rm = TRUE)
sd_co2 <- sd(wd.df$ln_co2_pc[cobs], na.rm = TRUE)

# correlation
cor <- cor(wd.df$ln_gdp_pc, wd.df$ln_co2_pc, use = "complete.obs")
cor

# Manually calculated
cov / (sd_gdp*sd_co2)
