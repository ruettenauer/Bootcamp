######################################
# * Pre-sessional Quant bootcamp *   #
# Tobias Ruttenauer and Rocio Mendez #
######################################

#  RM created 26/09/2023 *

# Remove all objects from the environment
rm(list = ls())

# Check if the environment is empty (optional)
ls()

#Packages we will use today
#install.packages("dplyr")
#install.packages("summarytools")
#install.packages("haven")
#install.packages("expss")


  
# This R script accompanies the pre-sessional statistics 
# bootcamp for the MSc SRM. It introduces students to 
# the R environment and several basic commands. 
  
# Changing the working directory:
  setwd ("C:/Users/stnvrdm/OneDrive - University College London/Teaching/Quant Bootcamp")
    # You have to select here the folder on your  Windows computer 
    #where you have saved the "WDI_Data" dataset. 
    #setwd("~/Desktop/QuantBootcamp") if MAC

  
# Loading the data:
  # Load the data
  WDI_Data <- read.csv("WDI_Data.csv") 
    # We chose to name the object or dataset WDI_Data, the name should appear in
    # the environment (upper-right corner)
  
  
# 1. Summary statistics 
  
    # 1a. Using View, head and dim: */ 
  
    View(WDI_Data) # Opens a new tab with contents of dataset
    
    head(WDI_Data) # Shows the first six rows 
    # NA is how R represents missing variables
    
    head(WDI_Data, n=3) # Shows the first three rows 
    
    #1b. Accessing values of one variable
    
    WDI_Data$gdpcap # accesses the variable named gdpcap inside the WDI_Data dataframe
    
    mean(WDI_Data$gdpcap) # The result is NA, as there are some missing 
    # observations, thus you must tell R to consider this with "na.rm=TRUE"
      
    mean(WDI_Data$gdpcap, na.rm = TRUE)
    
    median(WDI_Data$gdpcap, na.rm = TRUE)
    
    # Load the summarytools package if not already loaded
    library(summarytools)
    
    # Assuming 'WDI_Data' is your data frame
    descr(WDI_Data)
    descr(WDI_Data$gdpcap)
    
    # What if you wanted to see the descriptive statistics of gdpcap
    # grouped by continent
    #Load the dplyr package if not already loaded
    library(dplyr)
    
       # Assuming 'WDI_Data' is your data frame
        WDI_Data %>%
          group_by(continent) %>%
          summarise(
            Mean = mean(gdpcap, na.rm = TRUE),  # Exclude missing values
            SD = sd(gdpcap, na.rm = TRUE),      # Exclude missing values
            Median = median(gdpcap, na.rm = TRUE),  # Exclude missing values
            Min = min(gdpcap, na.rm = TRUE),      # Exclude missing values
            Max = max(gdpcap, na.rm = TRUE),      # Exclude missing values
            N = sum(!is.na(gdpcap))  # Count non-missing values
          )


 
# 2. First task            
  
  
    # 2a. How many variables and countries? */ 
  
    dim(WDI_Data) # Shows the dimensions of your dataset, or in R speak dataframe
    # number of rows (observations) and columns (variables)
    # We have 61 variables and 217 units (observations). 
  

    # 2b. Number of units per continent? 
  
    table(WDI_Data$continent)
   
    # Load the dplyr package if not already loaded
    #library(dplyr)
    
    # Assuming 'WDI_Data' is your data frame and 'continent' is your categorical variable
    WDI_Data %>%
      group_by(continent) %>%
      summarise(
        Count = n(),
        Percentage = n() / nrow(WDI_Data) * 100
      )
    

    # 2c. Variable labels? */ 
    #There are many ways to apply labels to each one of your variables
    # We have provided one simple way to do so 
    library(expss)
    data(WDI_Data)
    WDI_Data = apply_labels(WDI_Data,
                            countryname	=	"	Country Name	"	,
                            countrycode	=	"	Country Code	"	,
                            continent	=	"	Continent	"	,
                            gdpcap	=	"	GDP per capita (constant 2015 US$)	"	,
                            bsc_fem	=	"	Educational attainment, at least Bachelor's, population 25+, female (%)	"	,
                            bsc_mal	=	"	Educational attainment, at least Bachelor's, population 25+, male (%)	"	,
                            bsc_tot	=	"	Educational attainment, at least Bachelor's, population 25+, total (%)	"	,
                            popdens	=	"	Population density (people per sq. km of land area)	"	,
                            urbpop	=	"	Urban population (% of total population)	"	,
                            co2intens	=	"	CO2 emissions (kg per 2015 US$ of GDP)	"	,
                            forest	=	"	Forest area (% of land area)	"	,
                            natresource	=	"	Total natural resources rents (% of GDP)	"	,
                            femparl	=	"	Proportion of seats held by women in national parliaments (%)	"	,
                            drinkwater	=	"	People using safely managed drinking water services (% of population)	"	,
                            sanitation	=	"	People using safely managed sanitation services (% of population)	"	,
                            healthexp	=	"	Current health expenditure (% of GDP)	"	,
                            healthexpcap	=	"	Current health expenditure per capita, PPP (current international $)	"	,
                            doctors	=	"	Physicians (per 1,000 people)	"	,
                            nurses	=	"	Nurses and midwives (per 1,000 people)	"	,
                            lifexp_fem	=	"	Life expectancy at birth, female (years)	"	,
                            lifexp_tot	=	"	Life expectancy at birth, total (years)	"	,
                            lifexp_mal	=	"	Life expectancy at birth, male (years)	"	,
                            infmort_tot	=	"	Mortality rate, infant (per 1,000 live births)	"	,
                            infmort_mal	=	"	Mortality rate, infant, male (per 1,000 live births)	"	,
                            infmort_fem	=	"	Mortality rate, infant, female (per 1,000 live births)	"	,
                            suicide_mal	=	"	Suicide mortality rate, male (per 100,000 male population)	"	,
                            suicide_fem	=	"	Suicide mortality rate, female (per 100,000 female population)	"	,
                            popgrowth	=	"	Population growth (annual %)	"	,
                            birthrate	=	"	Birth rate, crude (per 1,000 people)	"	,
                            deathrate	=	"	Death rate, crude (per 1,000 people)	"	,
                            oldagedep	=	"	Age dependency ratio, old (% of working-age population)	"	,
                            sexratio	=	"	Sex ratio at birth (male births per female births)	"	,
                            totpop	=	"	Population, total	"	,
                            tfr	=	"	Fertility rate, total (births per woman)	"	,
                            contracep	=	"	Contraceptive use, any modern method (% married women age 15-49)	"	,
                            hiv	=	"	Prevalence of HIV, total (% of population ages 15-49)	"	,
                            internet	=	"	Fixed broadband subscriptions (per 100 people)	"	,
                            mobiles	=	"	Mobile cellular subscriptions (per 100 people)	"	,
                            gini	=	"	Gini index	"	,
                            top10p	=	"	Income share held by highest 10%	"	,
                            povrat	=	"	Multidimensional poverty headcount ratio (% of total population)	"	,
                            povgap215	=	"	Poverty gap at $2.15 a day (2017 PPP) (%)	"	,
                            povgap685	=	"	Poverty gap at $6.85 a day (2017 PPP) (%)	"	,
                            povgap365	=	"	Poverty gap at $3.65 a day (2017 PPP) (%)	"	,
                            hom_tot	=	"	Intentional homicides (per 100,000 people)	"	,
                            hom_fem	=	"	Intentional homicides, female (per 100,000 female)	"	,
                            hom_mal	=	"	Intentional homicides, male (per 100,000 male)	"	,
                            armyexp	=	"	Military expenditure (% of GDP)	"	,
                            ptemp_fem	=	"	Part time employment, female (% of total female employment)	"	,
                            ptemp_mal	=	"	Part time employment, male (% of total male employment)	"	,
                            ptemp_tot	=	"	Part time employment, total (% of total employment)	"	,
                            femmanage	=	"	Female share of employment in senior and middle management (%)	"	,
                            emprat_fem	=	"	Employment to population ratio, 15+, female (%)	"	,
                            emprat_mal	=	"	Employment to population ratio, 15+, male (%)	"	,
                            emprat_tot	=	"	Employment to population ratio, 15+, total (%)	"	,
                            emp_serv	=	"	Employment in services (% of total employment)	"	,
                            emp_indu	=	"	Employment in industry (% of total employment)	"	,
                            emp_agri	=	"	Employment in agriculture (% of total employment)	"	,
                            lfp_fem	=	"	Labor force participation rate (% of female population ages 15-64)	"	,
                            lfp_mal	=	"	Labor force participation rate (% of male population ages 15-64)	"	,
                            unemploy	=	"	Unemployment, total (% of total labor force)	"	
                            
    )
    
    

    #2d. Other information for variables femmanage and armyexp
  
      # femmanage - Assuming 'WDI_Data' is your data frame
      WDI_Data %>%
          summarise(
          Mean = mean(femmanage, na.rm = TRUE),  # Exclude missing values
          SD = sd(femmanage, na.rm = TRUE),      # Exclude missing values
          Median = median(femmanage, na.rm = TRUE),  # Exclude missing values
          Min = min(femmanage, na.rm = TRUE),      # Exclude missing values
          Max = max(femmanage, na.rm = TRUE),      # Exclude missing values
          N = sum(!is.na(femmanage))  # Count non-missing values
        ) 
      
      descr(WDI_Data$femmanage)
      typeof(WDI_Data$femmanage) # Kind of storage
      class(WDI_Data$femmanage) # Type of variable
      
      # armyexp - Assuming 'WDI_Data' is your data frame
      WDI_Data %>%
        summarise(
          Mean = mean(armyexp, na.rm = TRUE),  # Exclude missing values
          SD = sd(armyexp, na.rm = TRUE),      # Exclude missing values
          Median = median(armyexp, na.rm = TRUE),  # Exclude missing values
          Min = min(armyexp, na.rm = TRUE),      # Exclude missing values
          Max = max(armyexp, na.rm = TRUE),      # Exclude missing values
          N = sum(!is.na(armyexp))  # Count non-missing values
        ) 
      
      descr(WDI_Data$armyexp)
      typeof(WDI_Data$armyexp) # Kind of storage
      class(WDI_Data$armyexp) # Type of variable

  # 2e. Countries with highest scores for femmanage and armyexp 
  
  # Based on the maximum values for each variable, lets produce the countryname
      #for the highest scores, and lets save them into a new 'object'

      
      # Tabulate 'countryname' based on 'femmanage' condition
      country_maxfemmanage <- table(subset(WDI_Data, femmanage > 77.3 & !is.na(femmanage))$countryname)
      print(country_maxfemmanage) # Print shows the object
    
      # Tabulate 'countryname' based on 'armyexp' condition
      country_maxarmyexp <- table(subset(WDI_Data, armyexp > 8.6 & !is.na(armyexp))$countryname)
      print(country_maxfemmanage) # Print shows the object
  

      
# 3. Accessing help files 
  
  
  # Let's look at the variable continent: 
		
			table(continent) 
		
		# What is going wrong in the command above? 
		
			?table
		
		# Aha, we forgot to add the dataframe first!
		   
			table(WDI_Data$continent) 
		

# 4. Illustrating a few things *

	
		# 4a. Different parts of your coding
		
			  # Filter the data based on the condition totpop > 10000000
			  filtered_data <- subset(WDI_Data, totpop > 10000000)
			
			  # Summarize gdpcap and gini for the filtered data
			  summary(filtered_data[c("gdpcap", "gini")])
			
			  # - subset(WDI_Data, totpop > 10000000) filters the data frame WDI_Data 
			  # based on the condition totpop > 10000000, creating a new data frame 
			  # called filtered_data.
			  
			  # summary(filtered_data[c("gdpcap", "gini")]) summarizes the gdpcap and 
			  # gini variables for the filtered data using the summary() function. 
			  #It will provide summary statistics such as mean, median, quartiles, 
			  # and more for these numeric variables.
				
		# 4b. How to view your entire dataset: 
		
			View(filtered_data) 
		
		# 4c. How to list data in your Console window: 
		
			# Load the dplyr package if not already loaded
			library(dplyr)
			
			# Assuming your data frame is named WDI_Data
			result <- WDI_Data %>%
			  filter(!is.na(gdpcap) & !is.na(hom_mal) & gdpcap > 30000) %>%
			  select(countryname, hom_mal, hom_fem) %>%
			  slice(1:100)
			
			# Print the result
			print(result)
			
			  #In this code	filter is used to filter the data based on the specified 
			  # conditions for gdpcap and ensuring that gdpcap and hom_mal are not 
			  # missing.
			  #	select is used to select the columns you want in the final result: 
			  # countryname, hom_mal, and hom_fem.
			  #	slice is used to select the first 100 rows of the filtered data.
			
			
	
# 5. Second task            
	
	
		# 5a. Numerical descriptions: 
			
			descr(WDI_Data$infmort_mal)
			descr(WDI_Data$infmort_fem)
			descr(WDI_Data$sexratio)
			descr(WDI_Data$femparl)
			# You could possibly create a loop for this repetitive action, 
			# it could be good practice to research loops in R 

		# 5b. Visual descriptions:
			
			# Load the ggplot2 package for data visualization
			library(ggplot2)
			
			# Create a scatterplot using ggplot2
			scatter1 <- ggplot(WDI_Data, aes(x = gdpcap, y = povrat, size = totpop, color = continent)) +
			  
			  # Add points to the scatterplot with transparency
			  geom_point(alpha = 0.5) +
			  
			  # Set the plot theme to minimal for a clean appearance
			  theme_minimal() +
			  
			  # Use a logarithmic scale for the x-axis and format labels as dollar values
			  scale_x_log10(labels = scales::dollar_format()) +
			  
			  # Set custom labels for the axes
			  labs(y = "Multidimensional poverty rate", x = "GDP per capita")
			
			# Display the scatterplot
			scatter1
			
			
		#5c. How about the relationship between GDP per capita and C02 emissions
			scatter2 <- ggplot(WDI_Data, aes(x = gdpcap, y = co2intens, size = totpop, color = continent)) +
			  geom_smooth(aes(group = 1), show.legend = "none") + geom_point(alpha = 0.5) + 
			  theme_minimal() + scale_y_log10() +  scale_x_log10(labels = scales::dollar_format()) +
			  labs(y = "C02 emissions", x = "GDP per capita")
			
			scatter2
				
	#5d. Let's look at some histograms
			# Assuming 'WDI_Data' is your data frame
			# Replace 'WDI_Data' with the actual name of your data frame if different
			hist(WDI_Data$sexratio, 
			     main = "Histogram of Sex Ratio",
			     xlab = "Sex Ratio (Male Births per Female Births)",
			     ylab = "Frequency",
			     col = "lightblue")
			
			
			#Or using ggplot
			# Load the ggplot2 package if not already loaded
			library(ggplot2)
			
			# Assuming 'WDI_Data' is your data frame
			# Replace 'WDI_Data' with the actual name of your data frame if different
			
			# Create a histogram using ggplot2
			ggplot(WDI_Data, aes(x = sexratio)) +
			  geom_histogram(binwidth = , fill = "lightblue", color = "black", na.rm = TRUE) +
			  labs(title = "Histogram of Sex Ratio",
			       x = "Sex Ratio (Male Births per Female Births)",
			       y = "Frequency") +
			  theme_minimal()
				 
      #And then maybe add a density curve:
			
			# Load the ggplot2 package if not already loaded
			library(ggplot2)
			
			# Assuming 'WDI_Data' is your data frame
			# Replace 'WDI_Data' with the actual name of your data frame if different
			
			# Create a histogram using ggplot2 and handle missing values, and add a density curve
			ggplot(WDI_Data, aes(x = sexratio)) +
			  geom_histogram(binwidth = , fill = "lightblue", color = "black", na.rm = TRUE) +
			  
			  # Add a density curve to the plot
			  geom_density(alpha = 0.2, fill = "orange") +  # Adjust alpha and fill as needed
			  
			  labs(title = "Histogram with Density Curve of Sex Ratio",
			       x = "Sex Ratio (Male Births per Female Births)",
			       y = "Frequency") +
			  theme_minimal()
			
		# 5c. Low vs. high income countries: 
		
			descr(WDI_Data$gdpcap)
				# Median GDP per capita = 6,590. 
				
			# Load the dplyr package if not already loaded
			library(dplyr)
			
			# Assuming 'WDI_Data' is your data frame
			# Replace 'WDI_Data' with the actual name of your data frame if different
			
			# Summarize specified numeric variables based on the condition
			result <- WDI_Data %>%
			  filter(gdpcap <= 6590) %>%
			  summarise(
			    Mean_infmort_mal = mean(infmort_mal, na.rm = TRUE),
			    Mean_infmort_fem = mean(infmort_fem, na.rm = TRUE),
			    Mean_sexratio = mean(sexratio, na.rm = TRUE),
			    Mean_femparl = mean(femparl, na.rm = TRUE)
			  )
			
			# Print the summary statistics
			print(result)
					
			# LNow if we want standard deviations, min or max, use the following code:
			result2 <- WDI_Data %>%
			  filter(gdpcap <= 6590) %>%
			  summarise(
			    N = n(),  # Number of observations
			    Mean_infmort_mal = mean(infmort_mal, na.rm = TRUE),
			    StdDev_infmort_mal = sd(infmort_mal, na.rm = TRUE),  # Standard deviation
			    Min_infmort_mal = min(infmort_mal, na.rm = TRUE),  # Minimum value
			    Max_infmort_mal = max(infmort_mal, na.rm = TRUE),  # Maximum value
			    Mean_infmort_fem = mean(infmort_fem, na.rm = TRUE),
			    StdDev_infmort_fem = sd(infmort_fem, na.rm = TRUE),
			    Min_infmort_fem = min(infmort_fem, na.rm = TRUE),
			    Max_infmort_fem = max(infmort_fem, na.rm = TRUE),
			    Mean_sexratio = mean(sexratio, na.rm = TRUE),
			    StdDev_sexratio = sd(sexratio, na.rm = TRUE),
			    Min_sexratio = min(sexratio, na.rm = TRUE),
			    Max_sexratio = max(sexratio, na.rm = TRUE),
			    Mean_femparl = mean(femparl, na.rm = TRUE),
			    StdDev_femparl = sd(femparl, na.rm = TRUE),
			    Min_femparl = min(femparl, na.rm = TRUE),
			    Max_femparl = max(femparl, na.rm = TRUE)
			  )
			
			# Print the summary statistics
			print(result2)
			
				   
		# 5d. Europe and Africa: */ 
		
			# Create histograms for 'sexratio' based on 'continent'
			# Filter data for 'Africa' (continent == "Africa")
			ggplot(WDI_Data[WDI_Data$continent == "Africa", ], aes(x = sexratio)) +
			  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
			  labs(title = "Histogram of Sex Ratio in Africa",
			       x = "Sex Ratio",
			       y = "Frequency") +
			  theme_minimal()
			
			# Filter data for 'Europe' (continent == "Europe")
			ggplot(WDI_Data[WDI_Data$continent == "Europe", ], aes(x = sexratio)) +
			  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
			  labs(title = "Histogram of Sex Ratio in Europe",
			       x = "Sex Ratio",
			       y = "Frequency") +
			  theme_minimal()
			
			# Create histograms for 'sexratio' based on 'continent' (combined)
			ggplot(WDI_Data[WDI_Data$continent %in% c("Africa", "Europe"), ], aes(x = sexratio)) +
			  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
			  facet_wrap(~continent, ncol = 1) +  # Separate histograms by continent
			  labs(title = "Histogram of Sex Ratio by Continent",
			       x = "Sex Ratio",
			       y = "Frequency") +
			  theme_minimal()
			
			# Create histograms for 'femparl' based on 'continent' (combined)
			ggplot(WDI_Data[WDI_Data$continent %in% c("Africa", "Europe"), ], aes(x = femparl)) +
			  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
			  facet_wrap(~continent, ncol = 1) +  # Separate histograms by continent
			  labs(title = "Histogram of Female Parliament Representation by Continent",
			       x = "Female Parliament Representation",
			       y = "Frequency") +
			  theme_minimal()
			  #Tip: Try different values for binwidth and see what happens

	   

	# 6. Associations

	
		#Basic scatter plot:
		
			  # Load the ggplot2 package if not already loaded
			  library(ggplot2)
			
			# Create a scatter plot using ggplot2
			ggplot(WDI_Data, aes(x = contracep, y = tfr)) +
			  geom_point(size = 3, color = "blue", alpha = 0.7) +  # Customize point appearance
			  labs(title = "Scatter Plot of Fertility Rate vs. Contraceptive Use",
			       x = "Contraceptive Use (Modern Method, % of Married Women Age 15-49)",
			       y = "Fertility Rate (Total Births per Woman)") +
			  theme_minimal()
			
			
		    # Adding a best-fitting line: */ 
			
			  ggplot(WDI_Data, aes(x = contracep, y = tfr)) +
			  geom_point(size = 3, color = "blue", alpha = 0.7) +  # Customize point appearance
			  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # Add regression line
			  labs(title = "Scatter Plot of Fertility Rate vs. Contraceptive Use",
			       x = "Contraceptive Use (Modern Method, % of Married Women Age 15-49)",
			       y = "Fertility Rate (Total Births per Woman)") +
			  theme_minimal()
				
		      # Calculating Pearson's correlation: 
  
			     # Calculate Pearson's correlation coefficient
			     correlation <- cor(WDI_Data$tfr, WDI_Data$contracep, use = "complete.obs")
			  
			      # Print the correlation coefficient
			       print(correlation) 


				   

		
				