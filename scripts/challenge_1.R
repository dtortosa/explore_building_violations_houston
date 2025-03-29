#!/usr/bin/env Rscript

#This is done to have the possibility to run this script as an executable: 'chmod +x myscript.R' and then ' ./myscript.R'. If you run the script as 'R CMD BATCH myscript.R', i THINK this is not used, because it is annotated. 
	#https://www.jonzelner.net/statistics/make/docker/reproducibility/2016/05/31/script-is-a-program/

#In case you run this script as an executable, you can save the output without warnings "./myscript.R > myscript.Rout" or with errors "./myscript.R &> myscript.Rout"
	#https://askubuntu.com/questions/420981/how-do-i-save-terminal-output-to-a-file




#############################################################################
####################### FIRST CHALLENGE #####################################
#############################################################################



#################################################################
####################### REQUIRED PACKAGES #######################
#################################################################

require(readxl) #for reading .xlsx
require(plyr) #for apply functions across lists and data.frames. This is better than apply, because split rows of a data.frame without converting into matrix or array. In that way you can use "$" to call columns. In addition, you can save the output as a data frame or a list.
require(dplyr) #for using the function "bind_rows", which binds the data.frames of a list by rows



###############################################################
####################### DATA PREPARATION ######################
###############################################################

## load the data from excel and then convert to data.frame
bd_viol = data.frame(read_excel("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_1/data/BUILDING_VIOLATIONS_After_2014.xlsx", col_names=TRUE))
	#https://stackoverflow.com/questions/7049272/importing-excel-files-into-r-xlsx-or-xls
head(bd_viol)
str(bd_viol)

##load the data as csv (previously converted in excel) to check that the reading from xlsx was fine
#load the db as csv
bd_viol_check = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_1/data/BUILDING_VIOLATIONS_After_2014.csv", sep=",", header=TRUE)
str(bd_viol_check)

#compare the variables I will use
variables_to_check = c("NPPRJID", "RecordCreateDate", "Zip", "CouncilDistrict", "Violation_Category", "ShortDescription", "DeadLineDate", "CheckBackDate")

#open an empty dataframe
results_check_db = data.frame(selected_variable=NA, check=NA)

#open a loop
for(i in 1:length(variables_to_check)){

	#select the [i] variable
	selected_variable = variables_to_check[i]

	#check all entries of the excel database are all equal for the [i] variable
	check = length(which(bd_viol[, selected_variable] == bd_viol_check[, selected_variable])) == length(which(!is.na(bd_viol[, selected_variable])))

	#save
	results_check_db = rbind.data.frame(results_check_db, cbind.data.frame(selected_variable, check))
}

#remove first row with NA
results_check_db = results_check_db[which(rowSums(is.na(results_check_db)) != ncol(results_check_db)),]

#see results
results_check_db #All identical except short description, but this can be caused by the fact that this variable has long lines of strings.



##### FIRST QUESTION #####

#the total number of rows is 376092
nrow(bd_viol)

#some violations have the same ID (NPPRJID)
bd_viol[which(bd_viol$NPPRJID == "340418"),] #For example, the ID=340418 is present in two rows. This is a "Junked Motor Vehicle" violation, with similar date, district, request number, but the deadline is different.

#indeed, the repetition of IDs is widespread, as the number of unique cases of NPPRJID is just 157936
length(unique(bd_viol$NPPRJID))

#I am assuming that each line is a violation as the instructions of the challenge indicate that "Each row in the data represents a single violation", thus the number of rows (without the header) is the number of violations. However, more information about the "NPPRJID" variable would be needed to confirm the independence of the observations, i.e., rows.



##### SECOND QUESTION #####

#select only those rows for which "CouncilDistrict" is equal to "D"
bd_viol_cd = bd_viol[which(bd_viol$CouncilDistrict == "D"),]
#check that no violation included in the dataset does not belong to the D district
length(which(bd_viol_cd$CouncilDistrict != "D")) == 0

#then calculate the number of rows to get the number of violations in that district
nrow(bd_viol_cd) #12899



##### THIRD QUESTION #####

#create a vector with the target short descriptions
t_sd = c("kitchen", "bathroom", "toilet")

#select those rows for which ShortDescription includes "kitchen" (t_sd[1]) OR "bathroom" (t_sd[2]) OR "toilet" (t_sd[3])
t_sd_rows = which(grepl(t_sd[1], bd_viol$ShortDescription, fixed = TRUE) |
	grepl(t_sd[2], bd_viol$ShortDescription, fixed = TRUE) |
	grepl(t_sd[3], bd_viol$ShortDescription, fixed = TRUE))
		#fixed: If ‘TRUE’, ‘pattern’ is a string to be matched as is.
		#https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
bd_viol_t_sd = bd_viol[t_sd_rows,]

#check that in the rest of the dataset left, no short description includes any of the selected words 
opposite_subset = bd_viol[-t_sd_rows,]
length(which(grepl(t_sd[1], opposite_subset$ShortDescription, fixed = TRUE) |
	grepl(t_sd[2], opposite_subset$ShortDescription, fixed = TRUE) |
	grepl(t_sd[3], opposite_subset$ShortDescription, fixed = TRUE))) == 0

#the total number of violations including "kitchen", "bathroom" or "toilet" in the short description is 368
nrow(bd_viol_t_sd)



##### FOURTH QUESTION #####

#vector with violation categories
vio_cat = unique(bd_viol$Violation_Category)

#open an empty data.frame to save the results
results_fourth_question = data.frame(selected_vio_cat=NA, violations_selected_cat=NA, open_cases_absolute=NA, open_cases_proportion=NA, check_1=NA, check_2=NA)

#for each violation category
for(i in 1:length(vio_cat)){

	#select the [i] violation category
	selected_vio_cat = vio_cat[i]

	#select those rows for the [i] violation category and then those with a open status
	bd_viol_subset = bd_viol[which(bd_viol$Violation_Category == selected_vio_cat),]
	bd_viol_subset_open = bd_viol_subset[which(bd_viol_subset$Project_Status == "OPEN"),]
	#checks
	check_1 = length(which(bd_viol_subset$Violation_Category != selected_vio_cat)) == 0
	check_2 = length(which(bd_viol_subset_open$Project_Status != "OPEN")) == 0

	#calculate the total number of violations for the [i] category
	violations_selected_cat = nrow(bd_viol_subset)

	#calculate the number of open cases for that category
	open_cases_absolute = nrow(bd_viol_subset_open)

	#calculate the proportion of open cases respect to the total
	open_cases_proportion = open_cases_absolute/violations_selected_cat

	#save the results as a new row in the results data.frame
	results_fourth_question = rbind.data.frame(results_fourth_question, cbind.data.frame(selected_vio_cat, violations_selected_cat, open_cases_absolute, open_cases_proportion, check_1, check_2))
}

#remove first row with NAs
results_fourth_question = results_fourth_question[which(rowSums(is.na(results_fourth_question)) != ncol(results_fourth_question)),]

#select the violation category with the highest fraction of open cases
results_fourth_question[which(results_fourth_question$open_cases_proportion == max(results_fourth_question$open_cases_proportion)),] #this is DON - 2 - Dangerous Building, with a proportion of 0.26 open cases. 



##### FIFTH QUESTION #####

## obtain population of the target zips

#load the population size by zip
pop_zip = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_1/data/houston_population_by_ZIP_code.csv", sep=",", header=TRUE)
str(pop_zip)
head(pop_zip)

#extract those Zips of populations equal or higher than 20,000
large_zips = pop_zip[which(pop_zip$Population >= 20000),]
#check
length(which(large_zips$Population < 20000)) == 0


## clean the zip variable in the working dataset

#select only the first 5 digits of each zip
bd_viol$zip_5 = substr(bd_viol$Zip, start = 1, stop = 5)
	#https://www.tutorialspoint.com/how-to-extract-first-two-characters-from-a-string-in-r
#example to check 
bd_viol[which(bd_viol$Zip == "77091-2114"),]$Zip
substr(bd_viol[which(bd_viol$Zip == "77091-2114"),]$Zip, start = 1, stop = 5)

#select only those zips between 75 and 79
#first create a variable with the 2 first digits
bd_viol$zip_2 = substr(bd_viol$zip_5, start = 1, stop = 2)
	#https://www.tutorialspoint.com/how-to-extract-first-two-characters-from-a-string-in-r
#check that all Zips different from NA have two characters
length(which(sapply(bd_viol$zip_2, nchar) == 2)) == length(which(!is.na(bd_viol$zip_2)))
	#count the number of characters for each zip in the new zip variable, all should have 2. For that, I apply nchar across all zips using sapply to get a vector with the results.
#filter by the two conditions
bd_viol_subset_tx_zips = bd_viol[which(bd_viol$zip_2 >= 75 & bd_viol$zip_2 <= 79),]
#check
length(which(bd_viol_subset_tx_zips$zip_2 < 75 | bd_viol_subset_tx_zips$zip_2 > 79)) == 0

#select only violations form zips with a population size bigger than 20,000
bd_viol_subset_tx_zips = bd_viol_subset_tx_zips[which(bd_viol_subset_tx_zips$zip_5 %in% large_zips$Zip.Code),]


## calculate the number of violations per capita by zip

#calculate the number violations for each zip code and convert the result to df
count_viol_zip = data.frame(table(bd_viol_subset_tx_zips$zip_5))
	#table builds a contingency table of the counts at each combination of factor levels.
	#NAs are not considered
		#table(c("A", "A", "B", NA)) gives 2 counts for A and 1 for B.

#change column names to match the name of Zip column in the population size df
colnames(count_viol_zip)[which(colnames(count_viol_zip)=="Var1")] <- "Zip.Code"
colnames(count_viol_zip)[which(colnames(count_viol_zip)=="Freq")] <- "n_violations"

#merge the number of violations by zip with the population size by zip
violations_pop_size = merge(large_zips, count_viol_zip, by="Zip.Code")

#calculate violations per capita and save as a new column
violations_pop_size$violation_per_capita = violations_pop_size$n_violations / violations_pop_size$Population

#calculate the average of violations per capita across all TX zips
mean(violations_pop_size$violation_per_capita)



##### SIXTH QUESTION #####

##create a vector with the combinations of district and violation category

#code a new variable for the interaction between district and violation category
bd_viol$disrict_vio_cat = interaction(bd_viol$CouncilDistrict, bd_viol$Violation_Category, sep="_")
#check
length(which(bd_viol$disrict_vio_cat == paste(bd_viol$CouncilDistrict, bd_viol$Violation_Category, sep="_"))) == nrow(bd_viol[which(!is.na(bd_viol$disrict_vio_cat)),])

#calculate the frequency of each combination, i.e., the number of violations
combination_counts = data.frame(table(bd_viol$disrict_vio_cat))

#select those combinations with 50 or more counts
combination_counts = combination_counts[which(combination_counts$Freq >= 50),]
#check
length(which(combination_counts$Freq < 50)) == 0

#save the names of the combinations
disrict_vio_cat_unique = combination_counts$Var1


##run a loop for each district / violation category combination

#empty data.frame
results_fifth_question = data.frame(selected_combination=NA, n_viol_district=NA, n_viol_district_cat=NA, cond_prob=NA, check_1=NA, check_2=NA)

#open the loop
for(i in 1:length(disrict_vio_cat_unique)){

	#select the [i] combination
	selected_combination = disrict_vio_cat_unique[i]

	#split the two conditions
	district_condition = strsplit(as.vector(selected_combination), split="_")[[1]][1]
	viol_cat_condition = strsplit(as.vector(selected_combination), split="_")[[1]][2]

	#select those rows for the selected district
	subset_district = bd_viol[which(bd_viol$CouncilDistrict == district_condition),]
	#check
	check_1 = length(which(subset_district$CouncilDistrict != district_condition)) == 0 

	#select those rows for the selected district AND the selected violation category
	subset_district_viol_cat = bd_viol[which(bd_viol$CouncilDistrict == district_condition & bd_viol$Violation_Category == viol_cat_condition),]
	#check
	check_2 = length(which(subset_district_viol_cat$CouncilDistrict != district_condition | subset_district_viol_cat$Violation_Category != viol_cat_condition)) == 0 

	#total number of violations in the selected district
	n_viol_district = nrow(subset_district)

	#total number of violations in the selected district for the selected category
	n_viol_district_cat = nrow(subset_district_viol_cat)

	#calculate the conditional probability of a violation of the selected category given the selected district
	cond_prob = n_viol_district_cat / n_viol_district
		#we divide the violations of the selected category AND the selected district by the total number of violations in the selected district. 
		#This equals to the probability of a violation of the selected category occurring in the selected district divided by the probability of a violation occurring in the selected district.
			#https://www.statology.org/conditional-probability-in-r/

	#save the results
	results_fifth_question = rbind.data.frame(results_fifth_question, cbind.data.frame(selected_combination, n_viol_district, n_viol_district_cat, cond_prob, check_1, check_2))
}

#remove first row with NAs
results_fifth_question = results_fifth_question[which(rowSums(is.na(results_fifth_question)) != ncol(results_fifth_question)),]

#check
length(which(results_fifth_question$n_viol_district_cat / results_fifth_question$n_viol_district == results_fifth_question$cond_prob)) == nrow(results_fifth_question)


##calculate the median of the conditional probabilities
median(results_fifth_question$cond_prob)



##### SEVENTH QUESTION #####

#load coordinates of zips
pop_zip = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_1/data/houston_population_by_ZIP_code.csv", sep=",", header=TRUE)
str(pop_zip)
head(pop_zip)

#check that each zip is unique
length(which(duplicated(pop_zip$Zip.Code) == TRUE)) == 0


##create a function to calculate the distance of each zip to the rest of zips
#for debugging
#row_selected_zip = pop_zip[1,] #this function will be applied with ddply by "Zip.Code", which has one unique value per row, so each time the function will be applied to one row.
distance_to_each_zips = function(row_selected_zip){

	#selected zip
	selected_zip = row_selected_zip$Zip.Code

	#separate the target Zip from the rest
	subset_target_zip = pop_zip[which(pop_zip$Zip.Code == selected_zip),]
	subset_rest_zips = pop_zip[which(pop_zip$Zip.Code != selected_zip),]
	#check
	check_1 = length(which(subset_target_zip$Zip.Code != selected_zip)) == 0
	check_2 = length(which(subset_rest_zips$Zip.Code == selected_zip)) == 0

	#save the coordinate of the selected zip
	selected_coordinate = subset_target_zip$Location

	#open data.frame
	distance_results = data.frame(selected_zip=NA, second_zip=NA, selected_lat=NA, selected_lon=NA, second_lat=NA, second_lon=NA, diff_lat=NA, diff_lon=NA, avg_lat=NA, distance=NA, check_1=NA, check_2=NA)

	#open a loop for each of the rest of zips
	for(i in 1:nrow(subset_rest_zips)){

		#select the row of the [i] zip
		selected_row = subset_rest_zips[i,]

		#second zip
		second_zip = selected_row$Zip.Code

		#extract the coordinate
		second_coordinate = selected_row$Location

		#create a function to convert degrees to radians
		degree_to_rad = function(degrees){

			#multiply degrees to pi and divide by 180
			radians = degrees * (pi/180)
				#if 1 degree is pi/180 radians, then 10 degrees would be x; x=(10 degrees * (pi/180 radians)) / 1 degree = 0.17

			#return the radians
			return(radians)
		}

		#separate latitude and longitude for the selected zip
		selected_lat = degree_to_rad(as.numeric(strsplit(as.vector(selected_coordinate), split=", ")[[1]][1]))
		selected_lon = degree_to_rad(as.numeric(strsplit(as.vector(selected_coordinate), split=", ")[[1]][2]))

		#separate latitude and longitude for the second zip
		second_lat = degree_to_rad(as.numeric(strsplit(as.vector(second_coordinate), split=", ")[[1]][1]))
		second_lon = degree_to_rad(as.numeric(strsplit(as.vector(second_coordinate), split=", ")[[1]][2]))

		#calculate differences between coordinates
		diff_lat = selected_lat - second_lat
		diff_lon = selected_lon - second_lon
			#For the formulae we are going to use, it is not relevant the sign of the result.

		#calculate the average latitude
		avg_lat = (selected_lat+second_lat)/2

		#earth radius
		earth_r = 6371

		#calculate distance using the spherical Earth projected to a plane equation.
		distance = earth_r * sqrt((diff_lat^2) + (cos(avg_lat)*diff_lon)^2)
			#https://en.wikipedia.org/wiki/Geographical_distance#Spherical_Earth_projected_to_a_plane

		#save the results
		distance_results = rbind.data.frame(distance_results, cbind.data.frame(selected_zip, second_zip, selected_lat, selected_lon, second_lat, second_lon, diff_lat, diff_lon, avg_lat, distance, check_1, check_2))
	}

	#remove first row with NAs
	distance_results = distance_results[which(rowSums(is.na(distance_results)) != ncol(distance_results)),]

	#return
	return(distance_results)
}


##apply the function across the zips
final_distances = ddply(.data=pop_zip, .variables="Zip.Code", .fun=distance_to_each_zips, .inform=TRUE, .parallel=FALSE, .paropts=NULL)
	#".inform=TRUE" generates and shows the errors. This increases the computation time, BUT is very useful to detect problems in your analyses.
	#".parallel" to paralelize with foreach. 
	#".paropts" is used to indicate additional arguments in for each, specially interesting for using the .export and .packages arguments to supply them so that all cluster nodes have the correct environment set up for computing. 

#check we have the correct number of rows
nrow(final_distances) == nrow(pop_zip) * (nrow(pop_zip)-1)
	#for each zip of the 96 zips (nrow(pop_zip)), I calculated the distance to the rest of 95 zips (nrow(pop_zip)-1). Thus the total number of distances should be the product of the two numbers.

#checks have all TRUE
summary(final_distances)

#check the calculation of the distance
identical(6371*sqrt((final_distances$diff_lat)^2 + (cos(final_distances$avg_lat)*final_distances$diff_lon)^2), final_distances$distance)
	#https://en.wikipedia.org/wiki/Geographical_distance#Spherical_Earth_projected_to_a_plane

#if the Zip.code variable used by ddply and the selected zip saved by me are the same
if(identical(final_distances$Zip.Code, final_distances$selected_zip)){

	#remove the second column with id
	final_distances$selected_zip <- NULL
} else {

	#if not we have an error
	stop("ERROR!! We have a problem with the Zip code in final_distances!!!!")
}

#calculate the average distance
avg_distance = mean(final_distances$distance)


##calculate the radius of the circle and the area
#radius
radious_circle = ((45*pi) / 128) * avg_distance

#area
area_circle = pi * (radious_circle^2)
	#area equals to pi * radius squared

#convert to squared miles
area_circle_miles = area_circle * (1/1.60934) * (1/1.60934)
	#(1 km * 1 km) * (1 mile / 1.60934 km) * (1 mile / 1.60934 km) gives mile*mile

#see the result
area_circle_miles


##### EIGHTH QUESTION #####

##select the Date data and convert to Date format to calculate the difference between checkback and deadline dates in days considering only 2014 to 2016

#subset the data to select the columns of the dates
date_df = bd_viol[, which(colnames(bd_viol) %in% c("NPPRJID", "RecordCreateDate", "CheckBackDate", "DeadLineDate"))]
#check
str(date_df)

#convert the date columns to date format (yyyy-mm-dd)
date_df$RecordCreateDate = as.Date(date_df$RecordCreateDate, format="%Y-%m-%d")
date_df$DeadLineDate = as.Date(date_df$DeadLineDate, format="%Y-%m-%d")
date_df$CheckBackDate = as.Date(date_df$CheckBackDate, format="%Y-%m-%d")
	#https://stackoverflow.com/questions/11666172/calculating-number-of-days-between-2-columns-of-dates-in-data-frame/11666533
#check 
str(date_df)

#calculate the difference in days between the two dates of each violation (CheckBackDate and DeadLineDate)
date_df$diff_in_days = as.numeric(difftime(time1=date_df$CheckBackDate, time2=date_df$DeadLineDate, units="days"))
	#https://stackoverflow.com/questions/11666172/calculating-number-of-days-between-2-columns-of-dates-in-data-frame/11666533

#select only those violations between 2014 and 2016
date_df_subset = date_df[which(date_df$RecordCreateDate >= "2014-01-01" & date_df$RecordCreateDate <= "2016-12-31"),]
	#https://stats.stackexchange.com/questions/220865/how-to-select-a-range-of-dates-in-r/220870
#check
length(which(date_df_subset$RecordCreateDate < "2014-01-01" | date_df_subset$RecordCreateDate > "2016-12-31")) == 0


##calculate the average of the difference between checkback and deadline for each month

#get months
date_df_subset$month = format(date_df_subset$RecordCreateDate, format="%m")
#check
length(which(date_df_subset$month != sapply(strsplit(as.character(date_df_subset$RecordCreateDate), split="-"), "[", 2))) == 0
	#https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r

#get years
date_df_subset$year = format(date_df_subset$RecordCreateDate, format="%y")
#check
length(which(paste("20", date_df_subset$year, sep="") != sapply(strsplit(as.character(date_df_subset$RecordCreateDate), split="-"), "[", 1))) == 0
	#https://stackoverflow.com/questions/31235165/sapply-with-strsplit-in-r

#aggregate 'diff_in_days' on months and year and get the mean
average_month_year = aggregate(diff_in_days ~ month + year, date_df_subset, mean)
	#as both year and month are numeric, the resulting table with averages is ordered from Jan 2014 to Dec 2016.
	#https://stackoverflow.com/questions/16652199/compute-monthly-averages-from-daily-data

#create a variable with a value for each month between 2014 and 2016
average_month_year$date = 1:nrow(average_month_year)
	#we only have one value per month and year (average), so we cannot use the month-year as a factor. Instead, we consider each month-year as continuous and increasing variable. This is what we want, from 2014, as the number of months pass, what happens with difference between checkback and deadline dates?
	#remember that the table with averages is ordered by date.

#fit a linear model with the difference of days between checkback and deadline as a function of the date
linear_model <- lm(diff_in_days ~ date, data=average_month_year)

#see the coefficients
summary(linear_model)
coefficients(linear_model)[2] #1.667 days less each month between the checkback and the deadline.

#plot the association between time difference and date
plot(x=average_month_year$date, y=average_month_year$diff_in_days, xlab="Months from 2014 to 2016", ylab="Days between checkback and deadline")
#add linear prediction
lines(predict(linear_model))