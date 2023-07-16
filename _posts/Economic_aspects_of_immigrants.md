---
layout: post
title: "Economic_aspects_of_immigrants (Ukraine Migration 2022-2023)"
---

{% highlight ruby %}
#####################################



###
install.packages("WDI")
install.packages("ExPanDaR")
install.packages("knitr")
install.packages("tidyverse")



```{r}
#   load the necessary packages
library(WDI)
library(ExPanDaR)
library(knitr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plm)
library("Formula")
library("stargazer")
library(lmtest)

```
#Downloading Data from directly DATABANK by using WDI package 

Variables
Y = GDP per capita growth (annual %): "NY.GDP.PCAP.KD.ZG"
K = Gross fixed capital formation (% of GDP): "NE.GDI.FTOT.ZS"
L = Labor force participation rate, total (% of total population ages 15-64) (modeled ILO estimate): "SL.TLF.ACTI.ZS"
TECH =Research and development expenditure (% of GDP): "GB.XPD.RSDV.GD.ZS"
M = Net migration: "SM.POP.NETM" (need to calculate )
POP = Population, total: SP.POP.TOTL
CPI = Inflation, consumer prices (annual %)	FP.CPI.TOTL.ZG
EDUC = Adjusted savings: education expenditure (% of GNI)	NY.ADJ.AEDU.GN.ZS
AID = Net official aid received (current US$)	DT.ODA.OATL.CD
UN = Net official flows from UN agencies, UNHCR (current US$)	DT.NFL.UNCR.CD
SAFETY = Coverage of social safety net programs (% of population)	per_sa_allsa.cov_pop_tot
NEX = Exports of goods and services (% of GDP) 'NE.EXP.GNFS.ZS
NIMP =Imports of goods and services (% of GDP)'NE.IMP.GNFS.ZS'
INV =  "Foreign direct investment, net inflows (% of GDP) "BX.KLT.DINV.WD.GD.ZS"

```{r}

# Specify the desired indicators 
variables <- c("NY.GDP.PCAP.KD.ZG", "NE.GDI.FTOT.ZS", "SL.TLF.ACTI.ZS", "GB.XPD.RSDV.GD.ZS", "SM.POP.NETM", "SP.POP.TOTL","FP.CPI.TOTL.ZG", "NY.ADJ.AEDU.GN.ZS", "DT.ODA.OATL.CD", "DT.NFL.UNCR.CD",
               "per_sa_allsa.cov_pop_tot", "NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS", "BX.KLT.DINV.WD.GD.ZS")

# Specify the desired countries (iso2c or iso3c codes have to be use)

iso2c_codes <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LV",
                      "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE")

# Retrieve the data using the WDI() function
eu_data <- WDI(country = iso2c_codes, indicator = variables, start = 2000, end = 2020)
# View the downloaded data
head(eu_data)

#checking  class of data
class(eu_data)
dim(eu_data)

```




```{r}

# Rename the variables
Y <- eu_data$NY.GDP.PCAP.KD.ZG
K <- eu_data$NE.GDI.FTOT.ZS
L <- eu_data$SL.TLF.ACTI.ZS
TECH <- eu_data$GB.XPD.RSDV.GD.ZS
M <- eu_data$SM.POP.NETM
POP<- eu_data$SP.POP.TOTL
CPI <- eu_data$FP.CPI.TOTL.ZG
EDUC <- eu_data$NY.ADJ.AEDU.GN.ZS
AID <- eu_data$DT.ODA.OATL.CD
UN <- eu_data$DT.NFL.UNCR.CD
SAFETY <- eu_data$per_sa_allsa.cov_pop_tot
NEX <- eu_data$NE.EXP.GNFS.ZS
NIMP <- eu_data$NE.IMP.GNFS.ZS
INV <- eu_data$BX.KLT.DINV.WD.GD.ZS


# Create a new data frame with the renamed variables and additional columns
eu_data_new <- data.frame(country = eu_data$country, iso2c = eu_data$iso2c, iso3c = eu_data$iso3c, year = eu_data$year,
                          Y, K, L, TECH, M, POP , CPI, EDUC, AID, UN, SAFETY, NEX, NIMP, INV)


# View the new data frame
head(eu_data_new)


# View the new data frame
head(eu_data_new)

str(eu_data_new)

summary(eu_data_new)


```


```{r}

##checking missing values for further
# Calculate the number of missing values for each variable
missing_counts <- colSums(is.na(eu_data_new))

# Display the results
print(missing_counts)

```

I really wanted to add one of AID, UN, SAFETY variables which I guess effects the decision of migrants. But unfortunately almost all missing for observations. There is no way to handle such missing amount. SO I will just remove the features before starting any modeling.

```{r}
##removing variables which have largest number of missing value 
eu_data_new <- subset(eu_data_new, select = -c(AID, UN, SAFETY))

```



```{r}
# Save the data frame as a CSV file
write.csv(eu_data_new, file = "eu_data_new.csv", row.names = FALSE)

```



#Reading data and starting for process

```{r}
eu_data_new <- read.csv("eu_data_new.csv")

```




```{r}
missing_counts <- eu_data_new %>%
  group_by(country) %>%
  summarize(across(everything(), ~sum(is.na(.))))

print(missing_counts)

```




```{r} 
#replacing the missing values in variables with the median of non-missing values for each country
eu_data_filled <- eu_data_new %>%
  group_by(country) %>%
  mutate(across(Y:INV, ~if_else(is.na(.), median(., na.rm = TRUE), .)))

```




```{r}

##checking again the missing values

missing_counts <- colSums(is.na(eu_data_filled))

# Display the results
print(missing_counts)

str(eu_data_filled)
```
```{r}
names(eu_data_filled)


# Scale the variables
eu_data_scaled <- eu_data_filled
eu_data_scaled$K <- scale(eu_data_scaled$K)
eu_data_scaled$L <- scale((eu_data_scaled$L))
eu_data_scaled$TECH <- scale(eu_data_scaled$TECH)
eu_data_scaled$M <- scale(eu_data_scaled$M)
eu_data_scaled$POP <- scale(eu_data_scaled$POP)
eu_data_scaled$CPI <- scale(eu_data_scaled$CPI)
eu_data_scaled$EDUC <- scale(eu_data_scaled$EDUC)
eu_data_scaled$NEX <- scale(eu_data_scaled$NEX)
eu_data_scaled$NIMP <- scale(eu_data_scaled$NIMP)



```



#EDA

```{r}
library(corrplot)

# Calculate the correlation matrix
cor_matrix <- cor(eu_data_scaled[, c("Y", "K","L",  "TECH", "M", "POP","CPI", "EDUC", "NEX","NIMP", "INV")])


# Round the correlation coefficients to 2 decimal places
cor_matrix <- round(cor_matrix, 2)

# Print the correlation matrix
print(cor_matrix)

# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "number")

```





# Modeling

```{r}
# Save eu_data_scaled as a CSV file
write.csv(eu_data_scaled, "eu_data_scaled.csv", row.names = FALSE)

names(eu_data_scaled)

```

```{r}

# Calculate migration growth
MG <- ave(eu_data_scaled$M, eu_data_filled$country, FUN = function(x) c(NA, diff(x)))



# NET EXPORT
NET_EXPORT= NEX-NIMP
```



```{r}
random1 <-plm(Y~ K+ L +  TECH+ MG + CPI+ EDUC+ I(NEX-NIMP) + INV  , data=eu_data_scaled,
             index=c("country", "year"), model="random")
summary(random1)
```




```{r}
fixed1 <-plm(Y~ K+ L +  TECH+ MG + CPI+ EDUC+ I(NEX-NIMP) + INV  , data=eu_data_scaled,  
             index=c("country", "year"), model="within")
summary(fixed1)
```

```{r}
# Perform the Hausman test
hausman_test1 <- phtest(random1, fixed1)
print(hausman_test1)

```
##Because INV variables has highest pvalue for both model i decided to remove INV variable from model and try again with both Fixed Effect and Random Effect model and compare results again.


##removing insignificant feature from model and building second model

```{r}

# Build the model
random2 <- plm(Y~ K+ L +  TECH+ MG + CPI+ EDUC+ I(NEX-NIMP)   , data=eu_data_scaled, 
               index = c("country", "year"), 
               model = "random")

# Print the model summary
summary(random2)

```




```{r}

fixed2 <-plm(Y~ K+ L +  TECH+ MG + CPI+ EDUC+ I(NEX-NIMP)  , data=eu_data_scaled,  
             index=c("country", "year"), model="within")
summary(fixed2)
```


```{r}
# Perform the Hausman test
hausman_test <- phtest(random2, fixed2)
print(hausman_test)


```
Again a p-value  is less than the conventional significance level of 0.05, we reject the null hypothesis and conclude that the random effects model is inconsistent. This suggests that there are time-invariant unobserved factors (individual-specific effects) that significantly influence the dependent variable and are better captured by the fixed effects model.


```{r}
fixed3 <-plm(Y~ K+ L +  TECH+ MG + CPI+ EDUC , data=eu_data_scaled,
             index=c("country", "year"), model="within")
summary(fixed3)
```

```{r}
fixed4 <-plm(Y~ K+ L +  TECH+ MG + EDUC
             , data=eu_data_scaled,
             index=c("country", "year"), model="within")
summary(fixed4)
```







```{r}


# Fit the updated model
fixed5 <- plm(Y~ K+ L +  TECH+ MG + EDUC + K:MG + L:MG  , data = eu_data_scaled, model = "within", index = c("country", "year"))

# View the summary of the updated model
summary(fixed5)

```



```{r}
# Extract the estimated fixed effects
fixed_effects <- fixef(fixed5)

# View the estimated fixed effects
print(fixed_effects)
```




#Testing serial correlation and heteroscedasticity

```{r}

# Testing for serial correlation
pbgtest(fixed5)

##

# Testing for heteroskedasticity
bptest(fixed5, studentize = T)

#p-value is less than the conventional significance level of 0.05, we would reject the null hypothesis of homoscedasticity and conclude that there is evidence of heteroscedasticity in the model.

#Therefore, based on the studentized Breusch-Pagan test, there is indication of heteroscedasticity in the fixed3 model.
```



The findings the Breusch-Godfrey/Wooldridge test point The p-value is extremely small, indicating strong evidence of serial correlation in the idiosyncratic errors.

The test statistic in this specific case is 216.13, has 20 degrees of freedom, and has a p-value that is less than significance level. Strong evidence is presented against the null hypothesis of no serial correlation by the low p-value. As a result, we can say that the model's unique errors are serially correlated.

Because Breusch-Godfrey/Wooldridge test and found evidence of serial correlation in the idiosyncratic errors, we  consider studentizing the residuals (studentize = TRUE) to account for potential heteroscedasticity or outliers. 

Based on Breusch-Pagan test results, pvalaue is greather then 0.05 significance level and so there is no strong evidence of heteroscedasticity.


```{r}
# Generate the quality table

stargazer(fixed1, fixed3,fixed5, title="Results", align=TRUE,column.labels = c("General Model", "Intermediate Model", "Final Model"), type="text")
```


{% endhighlight %}
