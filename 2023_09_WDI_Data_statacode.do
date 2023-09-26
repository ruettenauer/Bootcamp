**************************
* Pre-sessional bootcamp *
**************************

*---------------------------------------------------------------------*
* DW created 22/09/2022, DW updated 27/09/2022, RM updated 26/09/2023 *
*---------------------------------------------------------------------* 

/* This do-file accompanies the pre-sessional statistics 
   bootcamp for the MSc SRM. It introduces students to 
   the Stata environment and several basic commands. */ 

/* Changing the working directory: */ 
cd "C:\Users\stnvrdm\OneDrive - University College London\Teaching\Quant Bootcamp"
	/* You have to select here the folder on your computer 
	   where you have saved the "WDI_Data" dataset. */ 

/* Loading the data: */ 
use "WDI_Data", clear

	/* Alternatively, you could open data files by
	   double-clicking on them in your file explorer. 
	   Yet, explicitly specifying a working directory 
	   that you will be using is generally safer and 
	   allows for easier reproducibility. */
	   
	*************************
	* 1. Summary statistics *
	*************************
			
		/* 1a. Using the summarize command: */ 
		
			summarize totpop popdens urbpop ///
					gdpcap gini top10p ///
					povrat povgap215 povgap685 ///
					co2intens natresource 	
					
		/* 1b. Using the tabstat command: */ 
		
			tabstat totpop popdens urbpop ///
					gdpcap gini top10p ///
					povrat povgap215 povgap685 ///
					co2intens natresource, ///
					statistics(mean median p10 p90)
					
	****************************
	* 2. First task            *
	****************************
	
		/* 2a. How many variables and countries? */ 
					
			/* Properties window tells us we have 62 
			   62 variables and 217 units (countries). 
			   The latter could also be seen from the 
			   number of unique values for the country
			   identifier variables: */ 
			   
			codebook countryname
			
		/* 2b. Number of units per continent? */ 
		
			codebook continent
			
		/* 2c. Variable labels? */ 
		
			describe femmanage armyexp 
			
		/* 2d. Other information for these variables? */ 
		
			codebook femmanage armyexp 
			
		/* 2e. Countries with highest scores? */ 
		
			/* Search in data browser for maximum values
			   for these variables (as identified using 
			   codebook command) and which countries 
			   they belong to. Or, anticipating what we 
			   learn later in this class, you can use 
			   "if" conditions: */
			
			tabulate countryname ///
				if femmanage>77.3 & femmanage!=. 
			tabulate countryname ///
				if armyexp>8.6 & armyexp!=. 
				
			/* Note: Stata stores missing values as 
			   extremely large positive numbers. Hence, 
			   we need to add the second condition: 
			   "variable is not missing". */ 
			   
	***************************
	* 3. Accessing help files *
	***************************
			   
		/* Let's calculate summary statistics: */ 
		
			summarize gdpcap, gini 
		
		/* What is going wrong in the command above? */ 
		
			help summarize
		
		/* Aha, everything behind commas is treated as an 
		   option; we should specify variable list without 
		   commas between the variables: */ 
		   
			summarize gdpcap gini 
		
	********************************
	* 4. Illustrating a few things *
	********************************
	
		/* 4a. Different parts of command syntax: */ 
		
			summarize gdpcap gini ///
				if totpop>10000000, ///
					detail 
				
		/* 4b. How to view your entire dataset: */ 
		
			browse 
		
		/* 4c. How to list data in your Results window: */ 
		
			list countryname hom_mal hom_fem ///
				if gdpcap>30000 & gdpcap!=. & hom_mal!=. ///
				in 1/100
			
	*****************************
	* 5. Second task            *
	*****************************
	
		/* 5a. Numerical descriptions: */ 
		
			summarize infmort_mal infmort_fem ///
				sexratio femparl, ///
					detail 
				/* Median = 50th percentile. */ 
					
		/* 5b. Visual descriptions: */ 
				
			scatter gdpcap povrat ///
				[w=totpop], ///
					msymbol(circle_hollow)
				/* Descriptive: A scatter plot to see the relationship between 
				 gdp per capita and poverty rates, weighted by population*/
				 
			graph twoway (scatter gdpcap povrat ///
				[w=totpop], ///
					msymbol(circle_hollow)) ///
						(lfitci gdpcap povrat [w=totpop])
				/* Causal: A scatter plot to see the relationship between 
				 gdp per capita and poverty rates, weighted by population*/
				 
			histogram sexratio 
			histogram femparl
			
			help histogram
			
			histogram sexratio, bin(30)
			histogram sexratio, width(0.02)
				/* "bin" specifies the number of bars to be 
				   used; "width" the width of the bars. */ 
				   
			histogram sexratio, bin(30) normal 
				/* "normal" adds a scaled density curve to 
				   the plot, with the same mean and standard 
				   deviation as the underlying data. */
				   
		/* 5c. Low vs. high income countries: */ 
		
			summarize gdpcap, detail
				/* Median GDP per capita = 6,590. */ 
				
			summarize infmort_mal infmort_fem ///
				sexratio femparl ///
					if gdpcap<=6590
					
			summarize infmort_mal infmort_fem ///
				sexratio femparl ///
					if gdpcap>6590 & gdpcap!=. 
				/* See note above about how missing 
				   values are stored in Stata. */ 
				   
		/* 5d. Europe vs. Africa: */ 
		
			codebook continent
				/* Africa = 1, Europe = 3. */ 
					
			histogram sexratio ///
				if continent==1, bin(10)
			histogram sexratio ///
				if continent==3, bin(10)
				
			/* We could also do both graphs in one 
			   go, using the "by" option: */ 
			histogram sexratio ///
				if continent==1 | continent==3, ///
					by(continent) bin(10)
			histogram femparl ///
				if continent==1 | continent==3, ///
					by(continent) bin(10)

	   
	*******************
	* 6. Associations *
	*******************
	
		/* Basic scatter plot: */ 
		
			scatter tfr contracep
			
		/* Adding a best-fitting line: */ 
			
			scatter tfr contracep || ///
				lfit tfr contracep 
				
		/* Calculating Pearson's correlation: */ 
		
			pwcorr tfr contracep, ///
				sig obs 
				
	
				   
	**************************
	* 7. Saving our dataset *
	**************************
	
		/* If we want to save a copy of our dataset, with the 
		   newly created variables included, we use "save": */ 
		   
			save "WDI_Data_v2", replace 
				/* See "help save" for more details. */ 
		
				