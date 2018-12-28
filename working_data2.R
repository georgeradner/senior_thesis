setwd("~/R Stuff/Thesis_Stuff/Thesis_Data")

# GOALS OF THIS CODE:
# assemble all my key data
# merge datasets 
# make data more usable 

# I'll organize the following code
# in terms of which variable I am trying to construct


# PART 1: Constructing Establishment (Plant) Openings/Closures Variable

#each of the following tables have the number of establishment/plant openings and closures by county
#there's one file for each year from 1999-2014

#the files changed formats in 2007 so we'll have to deal with pre- and post- 07 separately
county_sector_0 <- read.table("county-sector-1999-2000.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_1 <- read.table("county-sector-2000-2001.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_2 <- read.table("county-sector-2001-2002.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_3 <- read.table("county-sector-2002-2003.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_4 <- read.table("county-sector-2003-2004.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_5 <- read.table("county-sector-2004-2005.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_6 <- read.table("county-sector-2005-2006.csv", header = TRUE, sep="," , stringsAsFactors = FALSE)
county_sector_7 <- read.table("county-sector-2006-2007.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)

# the raw files don't indicate which year they refer to
# below we add the relevant year
county_sector_0[,10] <- rep(1999, length(row.names(county_sector_0)))
county_sector_1[,10] <- rep(2000, length(row.names(county_sector_1)))
county_sector_2[,10] <- rep(2002, length(row.names(county_sector_2)))
county_sector_3[,10] <- rep(2002, length(row.names(county_sector_3)))
county_sector_4[,10] <- rep(2003, length(row.names(county_sector_4)))
county_sector_5[,10] <- rep(2004, length(row.names(county_sector_5)))
county_sector_6[,10] <- rep(2005, length(row.names(county_sector_6)))
county_sector_7[,10] <- rep(2006, length(row.names(county_sector_7)))

#to combine the files above by row, we need them to have the same column names

#assigning each file the ideal column names
sector.names <- c("state","county", "naics", "naics.description", "births", "deaths", "expansions", "contractions", "constants", "initial.year")
varlist <- c("county_sector_0","county_sector_1","county_sector_2","county_sector_3","county_sector_4","county_sector_5","county_sector_6","county_sector_7")

for (i in varlist) {
  v <- get(i)
  colnames(v) <- sector.names
  assign(i, v)
}

#combining the files above by row
varlist1 <- list(county_sector_0,county_sector_1,county_sector_2,county_sector_3,county_sector_4,county_sector_5,county_sector_6,county_sector_7)
county_sector1 <- do.call(rbind, varlist1) 

#importing second set of establishment closure files (post 2007)
county_sector_8 <- read.table("county-sector-2007-2008.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_9 <- read.table("county-sector-2008-2009.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_10 <- read.table("county-sector-2009-2010.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_11 <- read.table("county-sector-2010-2011.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_12 <- read.table("county-sector-2011-2012.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_13 <- read.table("county-sector-2012-2013.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
county_sector_14 <- read.table("county-sector-2013-2014.csv", header = TRUE, sep="," , stringsAsFactors = FALSE)
county_sector_15 <- read.table("county-sector-2014-2015.csv", header = TRUE, sep="," , stringsAsFactors = FALSE)

#adding years
county_sector_8[,26] <- rep(2007, length(row.names(county_sector_8)))
county_sector_9[,26] <- rep(2008, length(row.names(county_sector_9)))
county_sector_10[,26] <- rep(2009, length(row.names(county_sector_10)))
county_sector_11[,26] <- rep(2010, length(row.names(county_sector_11)))
county_sector_12[,26] <- rep(2011, length(row.names(county_sector_12)))
county_sector_13[,26] <- rep(2012, length(row.names(county_sector_13)))
county_sector_14[,26] <- rep(2013, length(row.names(county_sector_14)))
county_sector_15[,26] <- rep(2014, length(row.names(county_sector_15)))

#combining second set of plant files (these already have the correct column names)
varlist2 <- list(county_sector_8,county_sector_9,county_sector_10,county_sector_11,county_sector_12,county_sector_13,county_sector_14,county_sector_15)
county_sector2 <- do.call(rbind, varlist2)


colnames(county_sector2)[26] <- "initial.year"

#the second set of files doesn't a list of establishment that didn't expand or contract
#as such the value of the "constants" column is set to NA
county_sector2[27] <- rep(NA, length(row.names(county_sector2)))
colnames(county_sector2)[27] <- "constants"

#combining pre and post 2007 files based on all columns they have in common
county_sector1 <- rbind(county_sector1, county_sector2[,c(2,4,5,6,14,17,20,23,27,26)])

#in the county_sector raw files, numbers in the thousands include a comma (eg: 1,235)
#removing comma so numbers can be coerced into numeric class
county_sector1$deaths <- as.numeric(gsub(",","",county_sector1$deaths,fixed = TRUE))
county_sector1$births <- as.numeric(gsub(",","",county_sector1$births,fixed = TRUE))
county_sector1$contractions <- as.numeric(gsub(",","",county_sector1$contractions,fixed = TRUE))
county_sector1$expansions <- as.numeric(gsub(",","",county_sector1$expansions,fixed = TRUE))
county_sector1$constants <- as.numeric(gsub(",","",county_sector1$constants,fixed = TRUE))


# next we'll add FIP codes to county_sector1
#fip codes identify state and county 

#importing file to merge state/county names with fip codes
fip.codes <- read.table("georef12fip.txt", header=TRUE, sep = ",")


#function to remove the word "county" from our fips code list
#county_sector 1 county names do not have the word "county"
removeWords <- function(str, removals) {
  y <- unlist(strsplit(str, " "))
  paste(y[!y %in% removals], collapse=" ")
}

#create new column with upper case county names without the word "county"
fip.codes[,4] <- sapply(levels(fip.codes$ctyname)[as.numeric(fip.codes$ctyname)],removeWords,removals =c("County,"))
fip.codes[,4] <- toupper(fip.codes[,4])

#removing commas and apostrophes from county names
fip.codes[,5] <- gsub(",","",fip.codes[,4],fixed=TRUE)
fip.codes[,6] <- gsub("'","", fip.codes[,5],fixed=TRUE)

colnames(fip.codes)[6] <- "county.state"

#function to extract state name abbreviations when the string has format like "Bibb County, AL".
#later it will be useful to match state abbreviations to their fip code.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}  
fip.codes[,7] <- substrRight(fip.codes[,6],2)

colnames(fip.codes)[7] <- "state_abbrev"

#preparing county_sector 1 so we can merge it and the fip.codes file
county_sector1[,11] <- rep(NA,length(rownames(county_sector1)))

county_sector1$state <- toupper(county_sector1$state)
county_sector1$county <- toupper(county_sector1$county)

states <- cbind(as.data.frame(toupper(state.name)), as.data.frame(state.abb))
colnames(states) <- c("state","state.abb")
county_sector1 <- merge(county_sector1, states,all.x = TRUE)

county_sector1[,11] <- paste(county_sector1$county,county_sector1$state.abb,sep = " ")
colnames(county_sector1)[11] <- "county.state"

# adding column for difference between estalbishment births and deaths
county_sector1[,(1+length(colnames(county_sector1)))] <- county_sector1$births - county_sector1$deaths
colnames(county_sector1)[length(colnames(county_sector1))] <- "net"  
  
#merge fip codes and county names 
county_sector3 <- merge(county_sector1,fip.codes[,c(1,2,6)], all.x = TRUE)

#unfortunately, ONLY 90% OF COUNTY NAMES IN COUNTY_SECTOR1 MATCHED THE FIP CODES FILE

#the code in comments below shows how I determined 10% was lost

#missing <- c()
#for (n in 1:length(county_sector3$fipscty)){
#  if (is.na(county_sector3$fipscty[n])){missing[length(missing)+1] <- n}
#}

#missing.na <- county_sector3[missing,1]

#for (n in length(county_sector3$fipscty)){
#  if (is.na(county_sector3$fipscty[n])){missing[length(missing)+1] <- n}
#}

#missing.table <- as.data.frame(table(missing.na))
#missing.table5 <- missing.table


# we've lost 10% of our data but pushing forward for now. will recover missing 10% later
county_sector4 <- merge(county_sector1,fip.codes[,c(1,2,6)])


# elections occur the congressional district level, so next we'll merge counties and congressional districts

#district boundaries change every ten years
#the files below indicate the "old" boundaries from 2002-2010
#and the "new" boundaries from 2012-
#we also need the boundaries from 1992-2000, which I haven't found yet
cdc.old <- read.table("cou_cd109_natl.txt",header=TRUE,sep = ",")
cdc.new <- read.table("natl_cocd_delimrecent.txt",header=TRUE,sep = ",")
cdc.old[,4] <- rep("2002-2010",length(rownames(cdc.old))) 
cdc.new[,4] <- rep("2012-", length(rownames(cdc.new)))

colnames(cdc.old) <- c("fipstate","fipscty","district","period")
colnames(cdc.new) <- c("fipstate","fipscty","district","period")

#adding columns to create a code which uniquely identifies the folliwng:
# -> state-county-districts
# -> state-counties
# -> state-districts

cdc.old[,5] <- paste(cdc.old$fipstate,cdc.old$fipscty,sep=",")
cdc.old[,6] <- paste(cdc.old$fipstate,cdc.old$district,sep=",")

cdc.old.table5 <- as.data.frame(table(cdc.old$V5))
colnames(cdc.old.table5) <- c("V5","dis.per.cty")
cdc.old <- merge(cdc.old,cdc.old.table5)

cdc.old.table6 <- as.data.frame(table(cdc.old$V6))
colnames(cdc.old.table6) <- c("V6","cty.per.dis")
cdc.old <- merge(cdc.old,cdc.old.table6)



cdc.new[,5] <- paste(cdc.new$fipstate,cdc.new$fipscty,sep=",")
cdc.new[,6] <- paste(cdc.new$fipstate,cdc.new$district,sep=",")

cdc.new.table5 <- as.data.frame(table(cdc.new$V5))
colnames(cdc.new.table5) <- c("V5","dis.per.cty")
cdc.new <- merge(cdc.new,cdc.new.table5)

cdc.new.table6 <- as.data.frame(table(cdc.new$V6))
colnames(cdc.new.table6) <- c("V6","cty.per.dis")
cdc.new <- merge(cdc.new,cdc.new.table6)

cdc <- rbind(cdc.old,cdc.new)
cdc[,9] <- paste(cdc$fipstate,cdc$fipscty,cdc$district,sep=",")
colnames(cdc)[c(1,2,9)] <- c("state.district","state.county","state.county.district")

#the code below matches a year to its corresponding congressional boundary period

year.period <- as.data.frame(1995:2014)
year.period[,2] <- rep(NA,20)
colnames(year.period) <- c("initial.year","period")
year.period[1:6,2] <- rep("1992-2000",2)
year.period[7:16,2] <- rep("2002-2010",10)
year.period[17:20,2] <- rep("2012-",4)

#matching years to their period
county_sector5 <- merge(county_sector4,year.period,all.x = TRUE)

#matching counties to congressional districts (given period)
county_sector6 <- merge(county_sector5, cdc,
                        by=c("fipstate","fipscty","period"))


#DRAFT MODEL 1 DATASET: period "2002-2010" + "2012-"

#goal: 
# CD    YEAR   NET
# 4,23  2003   -5

# LOCAL AREA LABOR FORCE STATISTICS

#importing files with unemployment data by county-year
lau99 <- read.table("laucnty99.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau00 <- read.table("laucnty00.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau01 <- read.table("laucnty01.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau02 <- read.table("laucnty02.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau03 <- read.table("laucnty03.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau04 <- read.table("laucnty04.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau05 <- read.table("laucnty05.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau06 <- read.table("laucnty06.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau07 <- read.table("laucnty07.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau08 <- read.table("laucnty08.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau09 <- read.table("laucnty09.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau10 <- read.table("laucnty10.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau11 <- read.table("laucnty11.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau12 <- read.table("laucnty12.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau13 <- read.table("laucnty13.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau14 <- read.table("laucnty14.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau15 <- read.table("laucnty15.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau16 <- read.table("laucnty16.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)

lau <- do.call(rbind,
               list(lau99,lau00,lau01,lau02,lau03,lau04,lau05,lau06,lau07,lau08,lau09,lau10,lau11,lau12,lau13,lau14,lau15,lau16))

#removing an empty row and an empty column
lau <- lau[-1,]
lau <- lau[,-5]

colnames(lau) <- c("cn.code","fipstate","fipscty","initial.year","labor.force","employed","unemployed","urate")

# importing nominate score data

#dw-nominate score data
library(readxl)
dw_nominate <- read_excel("HL01113D21_BSSE.XLSX",col_names = FALSE)

#from https://legacy.voteview.com/dwnomin.htm
colnames(dw_nominate) <- c("congress.number","icpsr.id.","icpsr.state","district", "state","party.code","leg.name","dim1","dim2","dim1.bootse","dim2.bootse","dims.se.corr","log.likelihood","votes","errors","geo.mean.prob")

#ICPSR State Codes and U.S. Mail Codes dataset from https://legacy.voteview.com/state_codes_icpsr.htm
icpsr.codes <- read.table("icpsr_state_codes.csv",header=TRUE,sep = ",",stringsAsFactors = FALSE)

#this dataset matches icpsr state codes to fip state codes.
fip.icpsr <- merge(icpsr.codes,unique(fip.codes[,c(1,7)]))

#adding fip state codes to nominate scores
dw_nominate <- merge(dw_nominate,fip.icpsr)

#creating column for state-district to match the other datasets
k.dw <- length(colnames(dw_nominate))
dw_nominate[,(1+k.dw)] <- paste(dw_nominate$fipstate,dw_nominate$district,sep=",") 
colnames(dw_nominate)[k.dw+1] <- "state.district"

congressToElectionYear <- function(con){
  sum(1786,con*2)
}

k.dw <- length(colnames(dw_nominate))
dw_nominate[,(1+k.dw)] <- sapply(dw_nominate$congress.number,congressToElectionYear)
colnames(dw_nominate)[k.dw+1] <- "election.year"



#mass layoff dataset
mass_layoffs <- read.table("extended_mass_layoffs.csv", header = TRUE, sep=",",stringsAsFactors = FALSE)

#creating column for state-county to match the other datasets
k.ml <- length(colnames(mass_layoffs))
mass_layoffs[,(1+k.ml)] <- paste(mass_layoffs$State.FIPS,mass_layoffs$County.FIPS,sep=",")
colnames(mass_layoffs)[c(1,1+k.ml)] <- c("initial.year","state.county")

#adding congressional district boundary period to mass layoff dataset
mass_layoffs <- merge(mass_layoffs,year.period) 


#for fun, just checking if this works...it does! 
#closures_layoffs <- merge(mass_layoffs[,c(1,4,19)],county_sector6,by=c("state.county","initial.year"))


#county_population <- read.csv("~/R Stuff/Thesis_data/county_population.csv",stringsAsFactors = FALSE)
#read.csv("~/R Stuff/Thesis_data/ACS_10_3YR_S0201_with_ann.csv")
#install.packages("dplyr")



#VERY ROUGH:
#PART : SURVEY DATA

library("haven", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("lfe", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

#polarization_survey <- read_sav("~/R Stuff/Thesis_data/Polarization-2014/Polarization 2014 public.sav")
anes_timeseries_cdf <- read_sav("~/R Stuff/Thesis_data/anes_timeseries_cdf.sav")
#anes1 <- as_factor(anes_timeseries_cdf)
length.anes <- length(colnames(anes_timeseries_cdf))

ideo <- as.factor(anes_timeseries_cdf$VCF0803)
ideo.scaled <- as.numeric(ideo) - 5
ideo.abs <- abs(ideo.scaled)

anes_timeseries_cdf[,(1+length.anes)] <- ideo.abs

#model1 <- felm(V949 ~ V950 + VCF0114 + VCF0102 + VCF0104 + VCF0105b + VCF0110  | VCF0004 + VCF0901a, data=anes_timeseries_cdf )
#summary(model1)

laid.off <- as.factor(anes_timeseries_cdf$VCF0156)
laid.off1 <- replace(laid.off,laid.off %in% c(0,8,9),NA)
laid.off2 <- replace(laid.off1,laid.off1 == 5, 0)
laid.off3 <- as.numeric(levels(laid.off2))[laid.off2]

anes_timeseries_cdf[,(2+length.anes)] <- laid.off1


#data.range <- cbind(cbind(anes_timeseries_cdf[,c(2,949,950)], laid.off2), laid.off3)
#data.range <- data.range[!(rowSums(is.na(data.range))),]

