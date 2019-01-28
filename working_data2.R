setwd("~/R Stuff/Thesis_Stuff/Thesis_Data")
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
install.packages("stargazer")
library(stargazer)
install.packages("AER")
library(AER)

library("lfe", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(lfe)

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
fip.codes[,8] <- gsub(" ","",fip.codes[,6],fixed=TRUE)
fip.codes[,8] <- gsub("-","",fip.codes[,8],fixed=TRUE)
fip.codes[,8] <- gsub("BOROUGH","",fip.codes[,8],fixed=TRUE)
fip.codes[,8] <- gsub("PARISH","",fip.codes[,8],fixed=TRUE)
fip.codes[,8] <- gsub("CENSUSAREA","",fip.codes[,8],fixed=TRUE)

colnames(fip.codes)[6] <- "county.state"
colnames(fip.codes)[8] <- "county.state1"

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

county_sector1[,14] <- gsub(" ","",county_sector1$county.state,fixed=TRUE)
county_sector1[,14] <- gsub("'","",county_sector1[,14],fixed=TRUE)
county_sector1[,14] <- gsub("-","",county_sector1[,14],fixed=TRUE)
county_sector1[,14] <- gsub("BOROUGH","",county_sector1[,14],fixed=TRUE)
county_sector1[,14] <- gsub("PARISH","",county_sector1[,14],fixed=TRUE)
county_sector1[,14] <- gsub("CENSUSAREA","",county_sector1[,14],fixed=TRUE)
county_sector1[,14] <- gsub("STATETOTAL","STATEWIDE",county_sector1[,14],fixed=TRUE)

colnames(county_sector1)[14] <- "county.state1"



# adding column for difference between estalbishment births and deaths
county_sector1[,(1+length(colnames(county_sector1)))] <- county_sector1$births - county_sector1$deaths
colnames(county_sector1)[length(colnames(county_sector1))] <- "net"  
  
#merge fip codes and county names 
county_sector3 <- merge(county_sector1[,-11],fip.codes[,c(1,2,8)], all.x = TRUE)

#county_sector3 <- merge(county_sector1,fip.codes[,c(1,2,6)], all.x = TRUE)

#unfortunately, ONLY 90% OF COUNTY NAMES IN COUNTY_SECTOR1 MATCHED THE FIP CODES FILE

#the code in comments below shows how I determined 10% was lost

missing <- c()
for (n in 1:length(county_sector3$fipscty)){
  if (is.na(county_sector3$fipscty[n])){missing[length(missing)+1] <- n}
}

missing.na <- county_sector3[missing,1]

for (n in length(county_sector3$fipscty)){
  if (is.na(county_sector3$fipscty[n])){missing[length(missing)+1] <- n}
}

missing.table <- as.data.frame(table(missing.na))
missing.table5 <- missing.table #previous one


missing.table8 <- county_sector3[which(is.na(county_sector3$fipscty)),c(1,2,3,14,15)]


# we've lost 10% of our data but pushing forward for now. will recover missing 10% later


county_sector4 <- merge(county_sector1[,-11],fip.codes[,c(1,2,8)])
colnames(county_sector4)[1] <- "county.state"

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

county_sector6[,23] <- county_sector6[,11]/county_sector6[,20]
county_sector6[,24] <- county_sector6[,16]/county_sector6[,20]

colnames(county_sector6)[c(23,24)] <- c("deaths.per.cd","net.per.cd")

# 
# cd.closures2 <- unique(county_sector6[,c(17,8,4)])
# goalz <- mapply(deathsDistrict2,
#                            cd.closures2$state.district,
#                            cd.closures2$naics,
#                            cd.closures2$initial.year)

cd.closures2 <- aggregate.data.frame(county_sector6[,c(23,24)],
                               list(state.district = county_sector6$state.district,
                                    year = county_sector6$initial.year,
                                    naics = county_sector6$naics),sum)

# deathsDistrict2 <- function(statedistrict,naics,year){
#   sum(county_sector6[statedistrict==county_sector6$state.district &
#                           naics==county_sector6$naics &
#                           year==county_sector6$initial.year,23])
# }
# 
# netDistrict <- function(statedistrict,naics,year){
#   cs6rows <- which(statedistrict==county_sector6$state.district &
#                     naics==county_sector6$naics &
#                     year==county_sector6$initial.year)
#   sum(county_sector6[cs6rows,16]/county_sector6[cs6rows,20])
#   }
# 
# netDistrict1 <- function(statedistrict,naics,year){
#   cs6 <- county_sector6[statedistrict==county_sector6$state.district &
#                           naics==county_sector6$naics &
#                           year==county_sector6$initial.year,]
#   sum(cs6[,16] # col. 16 is the net plant closures
#       /cs6[,20]) #col 20 is # districts per county
# }
# 
# deathsDistrict <- function(statedistrict,naics,year){
#   cs6rows <- which(statedistrict==county_sector6$state.district &
#                      naics==county_sector6$naics &
#                      year==county_sector6$initial.year)
#   sum(county_sector6[cs6rows,11]/county_sector6[cs6rows,20])
# }
# 
# deathsDistrict1 <- function(statedistrict,naics,year){
#   cs6 <- county_sector6[statedistrict==county_sector6$state.district &
#                           naics==county_sector6$naics &
#                           year==county_sector6$initial.year,c(11,20)]
#   sum(cs6[,1] # col. 11 is the plant closures
#       /cs6[,2]) #col 20 is # districts per county
# }
# 
# cd.closures <- unique(county_sector6[,c(17,8,4)])
# cd.closures <- cbind(cd.closures,data.frame(net = mapply(netDistrict,cd.closures$state.district,cd.closures$naics,cd.closures$initial.year)))
# 
# 
# cd.closures1 <- unique(county_sector6[,c(17,8,4)])
# cd.closures1[,5] <- mapply(deathsDistrict1,cd.closures1$state.district,cd.closures1$naics,cd.closures1$initial.year)

colnames(cd.closures)[3] <- "year"

electionYear <- function(year){
  election.year <- year
  while (!election.year%%4==0) {
    election.year <- election.year + 1
    }
  return(election.year)
}

cd.closures <- cbind(cd.closures, data.frame(election.year = sapply(cd.closures$year,electionYear)))

#sum(county_sector6[which(statedistrict==county_sector6$state.district, naics==county_sector6$naics,year==county_sector6$initial.year),16]*county_sector6[which(statedistrict==county_sector6$state.district, naics==county_sector6$naics, year==county_sector6$initial.year),20])

#cdX = cty1, cyt2, ... ctyN
#netX = cty1*1/k1 + ... ctyN/kN
#DRAFT MODEL 1 DATASET: period "2002-2010" + "2012-"

#goal: 
# CD    YEAR  INDUSTRY NET
# 4,23  2003  23       -5




# LOCAL AREA LABOR FORCE STATISTICS

#importing files with unemployment data by county-year
lau94 <- read.table("laucnty94.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau95 <- read.table("laucnty95.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau96 <- read.table("laucnty96.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau97 <- read.table("laucnty97.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
lau98 <- read.table("laucnty98.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,skip=4)
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
               list(lau94,lau95,lau96,lau97,lau98,lau99,lau00,lau01,lau02,lau03,lau04,lau05,lau06,lau07,lau08,lau09,lau10,lau11,lau12,lau13,lau14,lau15,lau16))

#removing an empty row and an empty column
lau <- lau[-1,]
lau <- lau[,-5]

lau$labor.force <- gsub(",","",lau$labor.force,fixed=TRUE)
lau$labor.force <- gsub("N.A.",NA,lau$labor.force,fixed=TRUE)

lau$employed <- gsub(",","",lau$employed,fixed=TRUE)
lau$employed <- gsub("N.A.",NA,lau$employed,fixed=TRUE)

lau$unemployed <- gsub(",","",lau$unemployed,fixed=TRUE)
lau$unemployed <- gsub("N.A.",NA,lau$unemployed,fixed=TRUE)

lau$urate <- gsub("N.A.",NA,lau$urate, fixed=TRUE)

colnames(lau) <- c("cn.code","fipstate","fipscty","initial.year","labor.force","employed","unemployed","urate")

k.l <- length(colnames(lau))
lau[,(1+k.l)] <- paste(lau$fipstate,lau$fipscty,sep=",")
colnames(lau)[1+k.l] <- "state.county"

lau[,(2+k.l)] <- as.numeric(lau$urate)

#previousUrate <- function(statecounty,year){
#  x<-lau[which(statecounty==lau$state.county & lau$initial.year==(year-1)),10]
#  if(is.na(x[1])){result<-NA
#  } else {result<-x}
#  return(x)
#}

previousUrate <- function(statecounty,year){
  if(0==length(which(statecounty==lau$state.county
           & lau$initial.year==(year-1)))) {NA
  } else {lau[which(statecounty==lau$state.county & lau$initial.year==(year-1)),5]}
}

lau[,(3+k.l)] <- rep(NA,length(rownames(lau)))

lau[,(3+k.l)] <- mapply(previousUrate,lau$state.county,lau$initial.year)

for(n in 1:length(rownames(lau))){
  lau[n,(3+k.l)] <- previousUrate(lau$state.county[n],lau$initial.year[n])
}

lau[,(4+k.l)] <- lau$V10 - lau$V11


colnames(lau)[10:12] <- c("urate.num","prev.urate","delta_urate")
lau[,(5+k.l)] <- lau$delta_urate/lau$prev.urate

colnames(lau)[13] <- "perc_delta_urate"


county_sector7 <- merge(county_sector6[,-c(1,2,5,6,7,19)], lau[,4:12], 
                         by=c("state.county","initial.year"))

county_sector7 <- cbind(county_sector7, 
                        data.frame(death.per.force = county_sector7$deaths.per.cd/county_sector7$labor.force,
                                   net.per.force = county_sector7$net.per.cd/county_sector7$labor.force))

lf <- is.na(as.numeric(county_sector7$labor.force))

county_sector2 <- cbind(county_sector2, 
                        data.frame(state.county = 
                                     paste(county_sector2$state.code,
                                           county_sector2$county.code,
                                           sep=",")))

county_sector2$deaths.employment.change <- gsub(",","",county_sector2$deaths.employment.change,fixed=TRUE)

county_sector2.1 <- merge(county_sector2,year.period)
county_sector2.1 <- merge(county_sector2.1, 
                          cdc[,c(1,2,6,7,8,9)],
                          by.x = c("state.county","period"),
                          by.y = c("state.county","period"))

county_sector2.1$deaths.employment.change <- as.numeric(county_sector2.1$deaths.employment.change)
county_sector2.1 <- cbind(county_sector2.1, 
                          data.frame(deaths.emp.by.cd = 
                                       county_sector2.1$deaths.employment.change/
                                       county_sector2.1$dis.per.cty))


cd.07 <- aggregate.data.frame(county_sector2.1$deaths.emp.by.cd,
                              list(state.district = county_sector2.1$state.district,
                                   naic = county_sector2.1$naics,
                                   year = county_sector2.1$initial.year),sum)  

colnames(cd.07)[4] <- "scaled.job.deaths" 

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

#create delta.abs.dim1, if.incumbent, and if.flip columns

dw_nominate <- cbind(dw_nominate,data.frame(prev.dim1 = rep(NA,length(rownames(dw_nominate))),
                                            prev.leg = rep(NA,length(rownames(dw_nominate))),
                                            prev.party = rep(NA,length(rownames(dw_nominate)))))

previousCongressVal <- function(statedistrict,electionyear,column){
  if(0==length(which(statedistrict==dw_nominate$state.district
                     & dw_nominate$election.year==(electionyear-2)))) {NA
  } else {dw_nominate[(which(statedistrict==dw_nominate$state.district
                     & dw_nominate$election.year==(electionyear-2))),column]}
}

dw_nominate$prev.dim1 <- mapply(previousCongressVal,
                                dw_nominate$state.district,
                                dw_nominate$election.year,
                                8) #column 8: dim1

dw_nominate$prev.leg <- mapply(previousCongressVal,
                               dw_nominate$state.district,
                               dw_nominate$election.year,
                               7) #column 7: leg.name

dw_nominate$prev.party <- mapply(previousCongressVal,
                                 dw_nominate$state.district,
                                 dw_nominate$election.year,
                                 6) #column 7: party.code

commonVal <- function(list1,list2){
  any(list1==list2)
}

dw_nominate <- cbind(dw_nominate,data.frame(if.incumbent = mapply(commonVal,dw_nominate$leg.name,dw_nominate$prev.leg),
                                            if.hold = mapply(commonVal,dw_nominate$party.code,dw_nominate$prev.party),
                                            abs.dim1 = abs(dw_nominate$dim1),
                                            abs.prev.dim1 = abs(sapply(dw_nominate$prev.dim1,mean))))

dw_nominate <- cbind(dw_nominate, data.frame(dlt.abs.dim1 = dw_nominate$abs.dim1 - dw_nominate$abs.prev.dim1))

dw_nominate2 <- merge(dw_nominate, year.period, by.x = "election.year",by.y="initial.year")

dw_nominate3 <- dw_nominate2[which(dw_nominate2$prev.party %in% c(100,200)),]

quantile.20 <- quantile(dw_nominate3$dim1,prob = c(0.2,0.8))[1]
quantile.80 <- quantile(dw_nominate3$dim1,prob = c(0.2,0.8))[2]

dw_nominate3 <- cbind(dw_nominate3, data.frame(moderate = dw_nominate3$dim1 >= quantile.20 & dw_nominate3$dim1 <= quantile.80,
                                               liberal = dw_nominate3$dim1 < quantile.20,
                                               conservative = dw_nominate3$dim1 > quantile.80))



dw_nominate3$moderate <- match(dw_nominate3$moderate,TRUE,nomatch=0)
dw_nominate3$liberal <- match(dw_nominate3$liberal,TRUE,nomatch=0)
dw_nominate3$conservative <- match(dw_nominate3$conservative,TRUE,nomatch=0)


dw_nominate3 <- cbind(dw_nominate3, data.frame(moderate_dem = dw_nominate3$moderate == 1 & dw_nominate3$party.code == 100,
                                               moderate_gop = dw_nominate3$moderate == 1 & dw_nominate3$party.code == 200))

dw_nominate3$moderate_dem <- match(dw_nominate3$moderate_dem,TRUE,nomatch=0)
dw_nominate3$moderate_gop <- match(dw_nominate3$moderate_gop,TRUE,nomatch=0)


#for (n in 1:length(rownames(dw_nominate))){
#  dw_nominate$prev.dim1[n] <- }

dw_closures1 <- merge(dw_nominate3,cd.closures,by.x=c("state.district","election.year"),by.y=c("state.district","initial.year"))


summary(lm(abs.dim1 ~ net,data=dw_closures1,subset=which(dw_closures1$naics=="31-33")))  #+, 0.388
summary(lm(abs.dim1 ~ net,data=dw_closures1,subset=which(dw_closures1$naics=="--"))) #+, 0.069

dw_closures2 <- merge(dw_nominate3,cd.closures2,by.x=c("state.district","election.year"),by.y=c("state.district","year"))

summary(lm(abs.dim1 ~ deaths.per.cd,data=dw_closures2,subset=which(dw_closures2$naics=="31-33")))  
summary(lm(abs.dim1 ~ deaths.per.cd,data=dw_closures2,subset=which(dw_closures2$naics=="--"))) # + 0.025



dw_jobs1 <- merge(dw_nominate3, cd.07,by.x=c("state.district","election.year"),by.y=c("state.district","year"))

summary(lm(abs.dim1 ~ scaled.job.deaths,data=dw_jobs1,subset=which(dw_jobs1$naics=="31-33")))  
summary(lm(abs.dim1 ~ scaled.job.deaths,data=dw_jobs1,subset=which(dw_jobs1$naics=="--"))) 
summary(lm(abs.dim1 ~ scaled.job.deaths,data=dw_jobs1[which(dw_jobs1$naic=="--"),])) #-, 0.521


dw_layoffs1 <- merge(dw_nominate3,cd.layoffs,by.x=c("state.district","election.year"),by.y=c("state.district","year"))
  
summary(lm(abs.dim1 ~ total.layoffs,data=dw_layoffs1)) +#, 0.863


  
#mass layoff dataset
mass_layoffs <- read.table("extended_mass_layoffs.csv", header = TRUE, sep=",",stringsAsFactors = FALSE)

#creating column for state-county to match the other datasets
k.ml <- length(colnames(mass_layoffs))
mass_layoffs[,(1+k.ml)] <- paste(mass_layoffs$State.FIPS,mass_layoffs$County.FIPS,sep=",")
colnames(mass_layoffs)[c(1,1+k.ml)] <- c("initial.year","state.county")

#adding congressional district boundary period to mass layoff dataset
mass_layoffs <- merge(mass_layoffs,year.period) 



mass_layoffs2 <- merge(mass_layoffs,cdc[,c(1,2,6,7,8)],by=c("state.county","period"))

mass_layoffs2 <- cbind(mass_layoffs2,data.frame(total.per.cd = mass_layoffs2$total/mass_layoffs2$dis.per.cty))

cd.layoffs <- aggregate.data.frame(mass_layoffs2$total.per.cd,
                                   list(state.district = mass_layoffs2$state.district,
                                        year = mass_layoffs2$initial.year),sum)

colnames(cd.layoffs)[3] <- "total.layoffs"

read.table(DEC_00_SF1_DP1_with_ann.csv)

# 
# totalDistrict <- function(statedistrict,year){
#   ml2rows <- which(statedistrict==mass_layoffs2$state.district &
#                      year==mass_layoffs2$initial.year)
#   sum(mass_layoffs2[ml2rows,6]/mass_layoffs2[ml2rows,22])
# }
# 
# cd.layoffs <- unique(mass_layoffs2[,c(21,2,3)])
# cd.layoffs <- merge(cd.layoffs, data.frame(total = mapply(totalDistrict,mass_layoffs2$state.district,mass_layoffs2$initial.year)))

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
anes_timeseries_cdf <- read_sav("anes_timeseries_cdf.sav")
#anes1 <- as_factor(anes_timeseries_cdf)
length.anes <- length(colnames(anes_timeseries_cdf))

ideo <- as.factor(anes_timeseries_cdf$VCF0803)
ideo.scaled <- as.numeric(ideo) - 5
ideo.abs <- abs(ideo.scaled)



#model1 <- felm(V949 ~ V950 + VCF0114 + VCF0102 + VCF0104 + VCF0105b + VCF0110  | VCF0004 + VCF0901a, data=anes_timeseries_cdf )
#summary(model1)

laid.off <- as.factor(anes_timeseries_cdf$VCF0156)
laid.off1 <- replace(laid.off,laid.off %in% c(0,8,9),NA)
laid.off2 <- replace(laid.off1,laid.off1 == 5, 0)
laid.off3 <- as.numeric(levels(laid.off2))[laid.off2]

anes_timeseries_cdf[,(1+length.anes)] <- laid.off1

anes_timeseries_cdf <- cbind(anes_timeseries_cdf,
                              data.frame(state = anes_timeseries_cdf[,643],
                                         state.district =
                                           mapply(paste,
                                                  anes_timeseries_cdf[,643],
                                                  anes_timeseries_cdf[,640],
                                                  MoreArgs = list(sep=","))))



anes.sample <- anes_timeseries_cdf[c(40000,50000,59940),]

  
# anes.state.dis <- anes_timeseries_cdf[,c(643,640)]
# anes.state.dis[,3] <- mapply(paste,
#                              anes.state.dis[,643],
#                              anes.state.dis[,640],
#                              MoreArgs = list(sep=","))


#cleaned up version of the ANES survey with all the key variables we need:
anes.clean.withNAs <- cbind(anes_timeseries_cdf[,which(colnames(anes_timeseries_cdf) %in% 
                                                         list("VCF0006a","VCF0004", "VCF0009z", "VCF0010z", "VCF0011z","VCF0114", "VCF0102", "VCF0104",
                                                              "VCF0105a", "VCF0110","VCF0803","state.district"))],
                                         data.frame(state = as.factor(anes_timeseries_cdf[,643]),
                                                    state.district =
                                                      mapply(paste,
                                                             as.factor(anes_timeseries_cdf[,643]),
                                                             as.factor(anes_timeseries_cdf[,640]),
                                                             MoreArgs = list(sep=",")),
                                                    abs.ideo = ideo.abs,
                                                    if.laidoff = laid.off3))
                                                 
colnames(anes.clean.withNAs)[1:11] <- c("election.year","r_id", "weights9","weights10","weights11","age.group","gender",
                                       "race_ethnic.group","education.level","income.group","ideology")


#anes <- anes.clean.withNAs[anes.clean.withNAs$election.year > 1982,]

#removing all "don't knows, no responses, NAs"
anes.clean0 <- anes.clean.withNAs
anes.clean0$age.group <- gsub(0,NA,anes.clean0$age.group,fixed=TRUE)
anes.clean0$gender <- gsub(0,NA,anes.clean0$gender,fixed=TRUE)
anes.clean0$race_ethnic.group <- gsub(9,NA,anes.clean0$race_ethnic.group, fixed=TRUE)
anes.clean0$education.level <- gsub(0,NA,anes.clean0$education.level,fixed=TRUE)
anes.clean0$income.group <- gsub(0,NA,anes.clean0$income.group,fixed=TRUE)
anes.clean0$ideology <- gsub(0,NA,anes.clean0$ideology,fixed=TRUE)
anes.clean0$ideology <- gsub(9,NA,anes.clean0$ideology,fixed=TRUE)
anes.clean0$abs.ideo <- gsub(4,NA,anes.clean0$abs.ideo,fixed=TRUE)



#converting from character to numeric

anes.clean0$education.level <- as.numeric(anes.clean0$education.level)
anes.clean0$age.group <- as.numeric(anes.clean0$age.group)
anes.clean0$gender <- as.numeric(anes.clean0$gender)
anes.clean0$income.group <- as.numeric(anes.clean0$income.group)
anes.clean0$race_ethnic.group<- as.numeric(anes.clean0$race_ethnic.group)
anes.clean0$ideology <- as.numeric(anes.clean0$ideology)
anes.clean0$abs.ideo <- as.numeric(anes.clean0$abs.ideo)

#anes.clean0 <- cbind(anes.clean0, data.frame(if.extreme = lapply(anes.clean0$abs.ideo,isVal,3)))

anes.clean0[,16] <- match(anes.clean0$abs.ideo,3, nomatch = 0)


anes.clean0[,16] <- gsub(1,0,anes.clean0$abs.ideo,fixed=TRUE)
anes.clean0[,16] <- gsub(2,0,anes.clean0[,15],fixed=TRUE)
anes.clean0[,16] <- gsub(3,1,anes.clean0[,15],fixed=TRUE)
colnames(anes.clean0)[16] = "if.extreme"

anes.clean0$state.district <- as.character(anes.clean0$state.district)
anes.clean0$if.extreme <- as.numeric(anes.clean0$if.extreme)



anes.raw <- anes.clean0[anes.clean0$election.year > 1984,]
anes.clean0 <- anes.clean0[!(rowSums(is.na(anes.clean0))),]


anes.clean0 <- cbind(anes.clean0, 
                     data.frame(age17=match(anes.clean0$age.group,1,nomatch = 0),
                                age25=match(anes.clean0$age.group,2,nomatch = 0),
                                age35=match(anes.clean0$age.group,3,nomatch = 0),
                                age45=match(anes.clean0$age.group,4,nomatch = 0),
                                age55=match(anes.clean0$age.group,5,nomatch = 0),
                                age65=match(anes.clean0$age.group,6,nomatch = 0),
                                age75=match(anes.clean0$age.group,7,nomatch = 0), 
                                male=match(anes.clean0$gender,1,nomatch = 0),
                                female=match(anes.clean0$gender,2,nomatch = 0),
                                other.gender = match(anes.clean0$gender,3,nomatch = 0),
                                white.NH = match(anes.clean0$race_ethnic.group,1,nomatch = 0),
                                black.NH = match(anes.clean0$race_ethnic.group,2,nomatch = 0),
                                asian.NH = match(anes.clean0$race_ethnic.group,3,nomatch = 0),
                                indig.NH = match(anes.clean0$race_ethnic.group,4,nomatch = 0),
                                hispanic = match(anes.clean0$race_ethnic.group,5,nomatch = 0),
                                other.mult.race = match(anes.clean0$race_ethnic.group,6,nomatch = 0),
                                GS=match(anes.clean0$education.level,1,nomatch = 0),
                                HS=match(anes.clean0$education.level,2,nomatch = 0),
                                some.college=match(anes.clean0$education.level,3,nomatch = 0),
                                college=match(anes.clean0$education.level,4,nomatch = 0),
                                income.16 =match(anes.clean0$income.group,1,nomatch = 0), 
                                income.33 =match(anes.clean0$income.group,2,nomatch = 0),
                                income.67 =match(anes.clean0$income.group,3,nomatch = 0),
                                income.95 =match(anes.clean0$income.group,4,nomatch = 0),
                                income.100 =match(anes.clean0$income.group,5,nomatch = 0)))
                     
anes.clean0 <- cbind(anes.clean0, data.frame(age.young = anes.clean0$age17+anes.clean0$age25,
                                             age.middle = anes.clean0$age35 + anes.clean0$age45,
                                             age.old= anes.clean0$age55 + anes.clean0$age65+anes.clean0$age75,
                                             other_race = anes.clean0$asian.NH+anes.clean0$indig.NH+anes.clean0$other.mult.race,
                                             GS_HS = anes.clean0$HS+anes.clean0$GS,
                                             income.low = anes.clean0$income.16+anes.clean0$income.33,
                                             income.med =anes.clean0$income.67,
                                             income.high = anes.clean0$income.95+anes.clean0$income.100))

anes.clean.table <- as.data.frame(table(anes.clean0$r_id))
length(anes.clean.table[anes.clean.table$Freq > 1,1]) #1086

anes.clean3 <- anes.clean[which(anes.clean$abs.ideo<4),]


anes.clean <- anes.clean.withNAs[!(rowSums(is.na(anes.clean.withNAs))),]

anes.clean4 <- anes.clean[which((anes.clean$abs.ideo<4)&(anes.clean$election.year>2000)),]



anes.clean1 <- merge(anes.clean,cd.closures,by=c("election.year","state.district"))

anes.clean2 <- anes.clean1[which(anes.clean1$abs.ideo<4),]
  
  
anes.m1 <- lm(abs.ideo ~ if.laidoff, data=anes.clean4)
anes.m2 <- lm(abs.ideo ~ if.laidoff + age.group + income.group +
                          education.level + race_ethnic.group + gender, 
                          data= anes.clean4)
anes.m3 <- lm(abs.ideo ~ if.laidoff + age.group + income.group +
                education.level + race_ethnic.group + gender + state + election.year, 
              data= anes.clean4)

anes.m4 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                  education.level + race_ethnic.group + gender 
                | state + election.year| data= anes.clean3)
anes.m5 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                  education.level + race_ethnic.group + gender 
                | election.year| data= anes.clean3)
anes.m6 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                  education.level + race_ethnic.group + gender 
                | state| data= anes.clean3)

anes0.m1 <- felm(abs.ideo ~ if.laidoff | 0 | 0 | state.district, data=anes.clean0)
anes0.m2 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                education.level + race_ethnic.group + gender | 0 | 0 | state.district,
              data= anes.clean0)

anes0.m3 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state.district | 0 | state.district, data= anes.clean0)

anes0.m4 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                  education.level + race_ethnic.group + gender 
                | election.year| 0 | state.district, data= anes.clean0)

anes0.m5 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state.district + election.year| 0 | state.district, data= anes.clean0)

anes0.m6 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state + election.year| 0 | state, data= anes.clean0)

anes0.m7 <- felm(abs.ideo ~ if.laidoff + age.group + income.group +
                  education.level + race_ethnic.group + gender 
                | state| 0 | state, data= anes.clean0)

anes0.m8 <- felm(if.extreme~ if.laidoff | 0 | 0 | state.district, data=anes.clean0)
anes0.m9 <- felm(if.extreme~ if.laidoff + age.group + income.group +
                 education.level + race_ethnic.group + gender| 0 | 0 | state.district, 
               data= anes.clean0)

anes0.m10 <- felm(if.extreme ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state.district| 0 | state.district,  data= anes.clean0)

anes0.m11 <- felm(if.extreme ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | election.year| 0 | state.district, data= anes.clean0)

anes0.m12 <- felm(if.extreme ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state.district + election.year | 0 | state.district,
                 data= anes.clean0)

anes0.m13 <- felm(if.extreme ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state + election.year| 0 | state, data= anes.clean0)

anes0.m14 <- felm(if.extreme ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender 
                 | state| 0 | state, data= anes.clean0)

######

anes0.m15 <- felm(abs.ideo ~ if.laidoff | 0 | 0 | state.district, data=anes.clean0)
anes0.m16 <- felm(abs.ideo ~ if.laidoff + age.old + age.middle + income.low 
                 + income.med + GS_HS + college + male + other.gender +
                   white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                 | 0 | 0 | state.district,
                 data= anes.clean0)

anes0.m17 <- felm(abs.ideo ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                 | state.district | 0 | state.district, data= anes.clean0)

anes0.m18 <- felm(abs.ideo ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                 | election.year| 0 | state.district, data= anes.clean0)

anes0.m19 <- felm(abs.ideo ~ if.laidoff ++ age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                 | state.district + election.year| 0 | state.district, data= anes.clean0)

anes0.m20 <- felm(abs.ideo ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                 | state + election.year| 0 | state, data= anes.clean0)

anes0.m21 <- felm(abs.ideo ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race  
                 | state| 0 | state, data= anes.clean0)

anes0.m22 <- felm(if.extreme~ if.laidoff | 0 | 0 | state.district, data=anes.clean0)
anes0.m23 <- felm(if.extreme~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                  | 0 | 0 | state.district, 
                 data= anes.clean0)

anes0.m24 <- felm(if.extreme ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                  | state.district| 0 | state.district,  data= anes.clean0)

anes0.m25 <- felm(if.extreme ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race  
                  | election.year| 0 | state.district, data= anes.clean0)

anes0.m26 <- felm(if.extreme ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                  | state.district + election.year | 0 | state.district,
                  data= anes.clean0)

anes0.m27 <- felm(if.extreme ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                  | state + election.year| 0 | state, data= anes.clean0)

anes0.m28 <- felm(if.extreme ~ if.laidoff + age.old + age.middle + income.low 
                  + income.med + GS_HS + college + male + other.gender +
                    white.NH + black.NH + asian.NH + indig.NH + other.mult.race 
                  | state| 0 | state, data= anes.clean0)

######
stargazer(anes0.m1,anes0.m2,anes0.m3,anes0.m4,anes0.m5,
          title = "ANES /#3",align = TRUE,
          add.lines = list(c("District Fixed Effects?","","","X","","X"),
                           c("Year Fixed Effects?"    ,"","","","X","X")),
          notes = "Robust standard errors clustered at the congressional district level.")
stargazer(anes0.m8,anes0.m9,anes0.m10,anes0.m11,anes0.m12,
          title = "ANES /#4",align = TRUE,add.lines = list(c("District Fixed Effects?","","","X","","X"),
                                                          c("Year Fixed Effects?"    ,"","","","X","X")),
          notes = "Robust standard errors clustered at the congressional district level.",notes.align = "l")
stargazer(anes0.m15,anes0.m16,anes0.m17,anes0.m18,anes0.m19,
          title = "ANES /#5",align = TRUE,add.lines = list(c("District Fixed Effects?","","","X","","X"),
                                                           c("Year Fixed Effects?"    ,"","","","X","X")),
          notes = "Robust standard errors clustered at the congressional district level.",notes.align = "l")

stargazer(anes0.m22,anes0.m23,anes0.m24,anes0.m25,anes0.m26,
          title = "ANES /#6",align = TRUE,add.lines = list(c("District Fixed Effects?","","","X","","X"),
                                                           c("Year Fixed Effects?"    ,"","","","X","X")),
          notes = "Robust standard errors clustered at the congressional district level.",notes.align = "l")


anes.clean5 <- anes.clean2[which(anes.clean2$naics == "31-33"),]

anes.jobs <- merge(anes.clean0, cd.07,by.x=c("election.year","state.district"),
                    by.y=c("year","state.district"))

anes.jobsT <- anes.jobs[anes.jobs$naic == "--",]

summary(lm(if.laidoff ~ scaled.job.deaths, data=anes.jobsT)) # -, 0.52
summary(lm(abs.ideo ~ scaled.job.deaths, data=anes.jobsT)) # -, 0.0462



# anes.clean0 <-  cbind(anes.clean0, data.frame(GS = sapply(anes.clean0$education.level, isVal, 1),
#                                               HS = sapply(anes.clean0$education.level, isVal, 2),
#                                               some.college = sapply(anes.clean0$education.level, isVal, 3),
#                                               college = sapply(anes.clean0$education.level, isVal, 4)))
# anes.clean0 <- cbind(anes.clean0, data.frame(GS_laidoff = anes.clean0$GS*anes.clean0$if.laidoff,
#                                              HS_laidoff = anes.clean0$HS*anes.clean0$if.laidoff,
#                                              SC_laidoff = anes.clean0$some.college*anes.clean0$if.laidoff,
#                                              C_laidoff = anes.clean0$college*anes.clean0$if.laidoff))
# 
# anes.clean0 <- cbind(anes.clean0, data.frame(GSHS_laidoff = anes.clean0$GS_laidoff+anes.clean0$HS_laidoff))

summary(lm(abs.ideo ~ GS_laidoff + HS_laidoff + SC_laidoff + C_laidoff, data=anes.clean0))
summary(lm(abs.ideo ~ GSHS_laidoff + SC_laidoff + C_laidoff, data=anes.clean0))
summary(lm(abs.ideo ~ GSHS_laidoff + SC_laidoff + C_laidoff + education.level, data=anes.clean0))

summary(lm(if.laidoff ~ scaled.job.deaths + race_ethnic.group + age.group+income.group+education.level+gender, data=anes.jobsT)) # -, 0.52

cd.anes.jobs <- merge(cd.anes, cd.07,
                       by.x=c("election.year","state.district"),
                       by.y=c("year","state.district"))

cd.anes.jobsT <- cd.anes.jobs[cd.anes.jobs$naic == "--",]



summary(lm(if.laidoff ~ scaled.job.deaths, data=cd.anes.jobsT)) 
summary(lm(abs.ideo ~ scaled.job.deaths, data=cd.anes.jobsT)) 


isVal <- function(x,val){
  result <- 0
  if (x == val){result <- 1}
  return(result)
}

anes.jobsT <- cbind(anes.jobsT, data.frame(GS = sapply(anes.jobsT$education.level, isVal, 1),
                                           HS = sapply(anes.jobsT$education.level, isVal, 2),
                                           some.college = sapply(anes.jobsT$education.level, isVal, 3),
                                           college = sapply(anes.jobsT$education.level, isVal, 4)))

anes.jobsT <- cbind(anes.jobsT, data.frame(GS_job.deaths = anes.jobsT$GS*anes.jobsT$scaled.job.deaths,
                                           HS_job.deaths = anes.jobsT$HS*anes.jobsT$scaled.job.deaths,
                                           SC_job.deaths = anes.jobsT$some.college*anes.jobsT$scaled.job.deaths,
                                           C_job.deaths = anes.jobsT$college*anes.jobsT$scaled.job.deaths))


summary(lm(abs.ideo ~ C_job.deaths + GS_job.deaths + HS_job.deaths + SC_job.deaths, data=anes.jobsT))

anes.layoffs1 <- merge(anes.clean3,cd.layoffs,by.x=c("election.year","state.district"),
                      by.y=c("year","state.district"))

anes.l.m1 <- lm(abs.ideo ~ total.layoffs, data=anes.layoffs1)
anes.l.m2 <- lm(if.laidoff ~ total.layoffs,data=anes.layoffs1)

summary(anes.l.m1)
summary(anes.l.m2)

cd.anes <- aggregate(anes.clean3[,c(3,4,5,6,7,10,11)],
                     list(state.district = anes.clean3$state.district,
                          election.year = anes.clean3$election.year),mean)


cd.anes1 <- aggregate(anes.clean0[,c(3,4,5,6,7,10,11)],
                      list(state.district = anes.clean0$state.district,
                           election.year = anes.clean0$election.year),mean)


cd.anes.m4 <- lm(abs.ideo ~ if.laidoff, data=cd.anes1)
summary(cd.anes.m4)

cd.anes.m5 <- lm(abs.ideo ~ if.laidoff + age.group + gender + race_ethnic.group + education.level + income.group, data=cd.anes1)
summary(cd.anes.m5)



cd.anes.m1 <- lm(abs.ideo ~ if.laidoff, data=cd.anes)
summary(cd.anes.m1)

cd.anes.m2 <- lm(abs.ideo ~ if.laidoff + age.group + gender + race_ethnic.group + education.level + income.group, data=cd.anes)
summary(cd.anes.m2)

cd.anes.m3 <- lm(abs.ideo ~ if.laidoff, data=cd.anes[which((cd.anes$education.level <3) & (cd.anes$education.level > 0)),])
summary(cd.anes.m3)

cd.anes.layoffs <- merge(cd.anes,cd.layoffs,by.x=c("election.year","state.district"),
  
                                                                  by.y=c("year","state.district"))

cd.anes.layoffs1 <- merge(cd.anes1,cd.layoffs,by.x=c("election.year","state.district"),
                         by.y=c("year","state.district"))


cd.anes.l.m1 <- lm(abs.ideo ~ total.layoffs, data=cd.anes.layoffs)
summary(cd.anes.l.m1)
cd.anes.l.m2 <- lm(if.laidoff ~ total.layoffs, data=cd.anes.layoffs)
summary(cd.anes.l.m2)

cd.anes.l.m3 <- lm(abs.ideo ~ total.layoffs, data=cd.anes.layoffs[which((cd.anes.layoffs$education.level <2) & (cd.anes.layoffs$education.level > 0)),])
summary(cd.anes.l.m3)
cd.anes.l.m4 <- lm(if.laidoff ~ total.layoffs, data=cd.anes.layoffs[which((cd.anes.layoffs$education.level <2) & (cd.anes.layoffs$education.level > 0)),])
summary(cd.anes.l.m4)

anes.m7 <- ivreg(abs.ideo ~ if.laidoff + age.group + income.group +
                   education.level + race_ethnic.group + gender | . - if.laidoff + net,
                 data= anes.clean5)
summary(anes.m7)

anes.m8 <- lm(abs.ideo ~ net, data=anes.clean5)
anes.m9 <- lm(if.laidoff ~ net, data=anes.clean5)

summary(anes.m9)

stargazer(anes.m1,anes.m2,anes.m3, title="Survey Scan 2",align =  TRUE)
stargazer(lm(abs.ideo ~ if.laidoff + age.group + income.group + education.level + race_ethnic.group + gender ,data= anes.clean2),title="Merged")
stargazer(anes.m4,anes.m5,anes.m6,title="Survey Scan 3",align = TRUE)


list("VCF0114", "VCF0102", "VCF0104", "VCF0105a", "VCF0110")


anes.data.range <- cbind(cbind(anes_timeseries_cdf[,c(2,949,950)], laid.off2), laid.off3)
anes.data.range <- anes.data.range[!(rowSums(is.na(anes.data.range))),]
anes.data.range <- anes.data.range[,c(1,2,5)]

colnames(anes.data.range) <- c("year","ideo","if.laidoff")

anes.data.range94 <- anes.data.range[which(anes.data.range$year >= 1994),]

###################################







###################################

county_income_1 <- read.table("county_income2001.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_2 <- read.table("county_income2002.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_3 <- read.table("county_income2003.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_4 <- read.table("county_income2004.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_5 <- read.table("county_income2005.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_6 <- read.table("county_income2006.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_7 <- read.table("county_income2007.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_8 <- read.table("county_income2008.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_9 <- read.table("county_income2009.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_10 <- read.table("county_income2010.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_11 <- read.table("county_income2011.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_12 <- read.table("county_income2012.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_13 <- read.table("county_income2013.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_14 <- read.table("county_income2014.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_15 <- read.table("county_income2015.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_16 <- read.table("county_income2016.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)
county_income_17 <- read.table("county_income2017.csv",header=TRUE,sep=",",skip = 4,nrows =9414,stringsAsFactors = FALSE)

income.names <- c("fip.combined","county","income.code","income.description","value")
incomelist <- list("county_income_1",
                "county_income_2",
                "county_income_3",
                "county_income_4",
                "county_income_5",
                "county_income_6",
                "county_income_7",
                "county_income_8",
                "county_income_9",
                "county_income_10",
                "county_income_11",
                "county_income_12",
                "county_income_13",
                "county_income_14",
                "county_income_15",
                "county_income_16",
                "county_income_17")

for (i in incomelist) {
  v <- get(i)
  colnames(v) <- income.names
  assign(i, v)
}

county_income_1[,6] <- rep(2001,length(row.names(county_income_1)))
county_income_2[,6] <- rep(2002,length(row.names(county_income_2)))
county_income_3[,6] <- rep(2003,length(row.names(county_income_3)))
county_income_4[,6] <- rep(2004,length(row.names(county_income_4)))
county_income_5[,6] <- rep(2005,length(row.names(county_income_5)))
county_income_6[,6] <- rep(2006,length(row.names(county_income_6)))
county_income_7[,6] <- rep(2007,length(row.names(county_income_7)))
county_income_8[,6] <- rep(2008,length(row.names(county_income_8)))
county_income_9[,6] <- rep(2009,length(row.names(county_income_9)))
county_income_10[,6] <- rep(2010,length(row.names(county_income_10)))
county_income_11[,6] <- rep(2011,length(row.names(county_income_11)))
county_income_12[,6] <- rep(2012,length(row.names(county_income_12)))
county_income_13[,6] <- rep(2013,length(row.names(county_income_13)))
county_income_14[,6] <- rep(2014,length(row.names(county_income_14)))
county_income_15[,6] <- rep(2015,length(row.names(county_income_15)))
county_income_16[,6] <- rep(2016,length(row.names(county_income_16)))
county_income_17[,6] <- rep(2017, length(row.names(county_income_17)))


county_income <- do.call(rbind,list(county_income_1,county_income_2,county_income_3,county_income_4,county_income_5,county_income_6,county_income_7,county_income_8,county_income_9,county_income_10,county_income_11,county_income_12,county_income_13,county_income_14,county_income_15,county_income_16,county_income_17))

colnames(county_income)[6] <- "initial.year"


county_dem_2010_2017 <- read.csv("cc-est2017-alldata.csv",header=TRUE)
county_dem_2000_2009 <- read.csv("co-est00int-sexracehisp.csv",header=TRUE)
  
county_acs05 <- read.csv("ACS_05_EST_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs06 <- read.csv("ACS_06_EST_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs07 <- read.csv("ACS_07_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs08 <- read.csv("ACS_08_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs09 <- read.csv("ACS_09_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]

county_acs10 <- read.csv("ACS_10_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs11 <- read.csv("ACS_11_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]

county_acs12 <- read.csv("ACS_12_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]

county_acs13 <- read.csv("ACS_13_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]
county_acs14 <- read.csv("ACS_14_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]

county_acs15 <- read.csv("ACS_15_1YR_S2502_with_ann.csv",header=TRUE,skip=1)[,1:129]

acs.list <- list(county_acs05,county_acs06,county_acs07,
                 county_acs08,county_acs09,county_acs10,
                 county_acs11,county_acs12,county_acs13,
                 county_acs14,county_acs15)

acs.list2 <- list("county_acs05","county_acs06","county_acs07",
                 "county_acs08","county_acs09","county_acs10",
                 "county_acs11","county_acs12","county_acs13",
                 "county_acs14","county_acs15")


for (i in acs.list2) {
  v <- get(i)
  colnames(v) <- colnames(county_acs05)
  assign(i, v)
}

county_acs05[,130] <- rep(2005,length(county_acs05[,1])) 
county_acs06[,130] <- rep(2006,length(county_acs06[,1])) 
county_acs07[,130] <- rep(2007,length(county_acs07[,1])) 
county_acs08[,130] <- rep(2008,length(county_acs08[,1])) 
county_acs09[,130] <- rep(2009,length(county_acs09[,1])) 
county_acs10[,130] <- rep(2010,length(county_acs10[,1])) 
county_acs11[,130] <- rep(2011,length(county_acs11[,1])) 
county_acs12[,130] <- rep(2012,length(county_acs12[,1])) 
county_acs13[,130] <- rep(2013,length(county_acs13[,1])) 
county_acs14[,130] <- rep(2014,length(county_acs14[,1])) 
county_acs15[,130] <- rep(2015,length(county_acs15[,1])) 


acs.colnames <- sapply(acs.list,colnames)

View(data.frame(sapply(acs.colnames,'[', seq(max(sapply(acs.colnames,length))))))
View(data.frame(acs.colnames))

#09=/=10
#11=/=12
#12/=/13
#14=/=15


county_acs2 <- do.call(rbind,list(county_acs05,county_acs06,county_acs07,
                                 county_acs08,county_acs09,county_acs10,
                                 county_acs11,county_acs12,county_acs13,
                                 county_acs14,county_acs15))
colnames(county_acs2)[130] <- "year"

county_acs <- county_acs2[,c(2,3,4,10,16,22,28,34,40,46,52,58,64,70,76,82,88,94,100,106,112,118,124,130)]
colnames(county_acs) <- c("fips.comb","country.name","total.units","white","black","indig","asian",
                          "pacific","other.race","multi.race","hispanic","white,NH","age.u35",
                          "age.u44","age.u45","age.u64","age.u74","age,u84","age85",
                          "no.HS","HS","some.college","college","year")


county_dec00 <- read.csv("DEC_00_SF1_DP1_with_ann.csv",header=TRUE,skip=1)
county_dec00[,196] <- rep(2000, length(county_dec00[,1]))

county_dec10 <- read.csv("DEC_10_SF1_SF1DP1_with_ann.csv",header=TRUE, skip=1)

county_mod00 <- read.csv("DEC_00_mod.csv",header=TRUE,skip=1)
county_mod10 <- read.csv("DEC_10_mod.csv",header=TRUE,skip=1)

county_mod00[,1] <- rep(2000,length(county_mod00[,1]))
county_mod10[,1] <- rep(2010,length(county_mod10[,1]))

county_d00 <- cbind(county_mod00[,c(1,2,3,4)],county_mod00[,grepl("Percent",colnames(county_mod00),fixed=TRUE)])
county_d10 <- cbind(county_mod10[,c(1,2,3,4)],county_mod10[,grepl("Percent",colnames(county_mod10),fixed=TRUE)])



county_d00 <- county_d00[,-(34:39)]
county_d00 <- county_d00[,-c(34,36)]
county_d00 <- county_d00[,-c(26,27)]

county_d10 <- county_d10[,-c(32,34)]

county_d10 <- cbind(county_d10,data.frame(Percent..Total.population...SEX.AND.AGE...25.to.34.years = county_d10[,11]+county_d10[,12],                               
Percent..Total.population...SEX.AND.AGE...35.to.44.years = county_d10[,13]+county_d10[,14],                           
Percent..Total.population...SEX.AND.AGE...45.to.54.years=county_d10[,15]+county_d10[,16]))

county_d10 <- county_d10[,-(11:16)]

county_d10 <- cbind(county_d10,data.frame(Percent..Total.population...SEX.AND.AGE...65.to.74.years=
                                            county_d10[,13]+county_d10[,14],
                                          Percent..Total.population...SEX.AND.AGE...75.to.84.years=
                                            county_d10[,15]+county_d10[,16]))

county_d10 <- county_d10[,-(13:16)]

county_d10 <- county_d10[,-15]
county_d10 <- county_d10[,c(1,2,3,4,5,19,20,6,7,8,
                            9,10,29,30,31,32,33,11,12,13,
                            14,15,16,17,18,23,24,25,26,27,
                            28,21,22)]
county_d10[,16:19] <-county_d10[,c(18,19,16,17)]

###########

anes.list <- list(anes.raw$ideology,anes.raw$abs.ideo,anes.raw$if.extreme,
                  anes.raw$if.laidoff,
                  anes.raw$age.group,anes.raw$gender,anes.raw$race_ethnic.group,
                  anes.raw$education.level,anes.raw$income.group,
                  #mass_layoffs$total, 
                  mass_layoffs2$total.per.cd,
                  #county_sector6[which(county_sector6$naics=="--"),11],
                  cd.closures2[cd.closures2$naic == "--",4])

anes.table <- data.frame(sapply(anes.list, '[', seq(max(sapply(anes.list,length)))))
rownames(anes.table) <- c("ideology","absolute ideology","1(extreme ideology)",
                          "1(recently laidoff)","age group","gender","race","education level")

stargazer(anes.clean0[,c(11,14,16,15,17,18,19,20,21,
                         22,23,24,25,26,27,28,29,30,
                         31,32,33,34,35,36,37,38,39,
                         40,41)],
          title="Model A Summary Statistics V1",summary.stat=c("n","mean","sd","min","max"))


stargazer(anes.clean0[,c(11,14,16,15,24,25,42,43,44,27,28,31,45,46,35,36,47,48,49)],
          title="Model A Summary Statistics V2",summary.stat=c("n","mean","sd","min","max"))


anes.iv <- list(cd.layoffs$total.layoffs,
                        cd.closures2[cd.closures2$naic == "--",4])

instruments.df <- data.frame(sapply(anes.iv, '[', seq(max(sapply(anes.iv,length)))))

colnames(instruments.df) = c("mass layoffs (# individuals)","# plant closures")

stargazer(instruments.df,title="Instruments",summary.stat=c("n","mean","sd","min","max"))
#################################### MODELS B & C

stargazer(dw_nominate3[,c(9,26,28,30,31,32,33,34)],tite="DW",summary.stat=c("n","mean","sd","min","max"))

stargazer(lau[,c(10,12)],title = "LAU",summary.stat=c("n","mean","sd","min","max"))
#################################### 

rownames(sum.table1) <- c("ideology","recent layoff","absolute nominate score","# mass layoffs","unemployment rate")

stargazer()

stargazer(county_acs[,4:23],title="ACS Controls",summary.stat=c("n","mean","sd","min","max"))

sum.ideo <- anes.data.range94$ideo
sum.laidoff <- anes.data.range94$if.laidoff
sum.abs.dim1 <- dw_nominate$abs.dim1
sum.masslayoffs <- mass_layoffs$total
sum.urate <- lau$V10[!is.na(lau$V10)]
sum.total.closures <- county_sector6[which(county_sector6$naics=="--"),11]
sum.total.net <- county_sector6[which(county_sector6$naics=="--"),16]
sum.manu.closures <- county_sector6[which(county_sector6$naics=="31-33"),11]
sum.manu.net <- county_sector6[which(county_sector6$naics=="31-33"),16]
sum.mine.closures <- county_sector6[which(county_sector6$naics=="21"),11]
sum.mine.net <- county_sector6[which(county_sector6$naics=="21"),16]

sum.list <- list(sum.ideo,sum.laidoff,sum.abs.dim1,sum.masslayoffs,sum.urate,
                 sum.total.closures,sum.total.net,sum.manu.closures,sum.manu.net,
                 sum.mine.closures,sum.mine.net)

survey.abs.ideo <- 
  
  
sum.table2 <- data.frame(sapply(sum.list, '[', seq(max(sapply(sum.list,length)))))

colnames(sum.table2) <- c("Abs. Ideology Score","Recent Layoff","Abs. Nominate Score","# Mass Layoffs","Unemployment Rate",
                          "Deaths: All Sectors","Net: All Sectors","Deaths: Manufacturing","Net: Manufacturing",
                          "Deaths: Mining","Net: Mining")

stargazer(sum.table2,title="Summary Statistics",summary.stat=c("n","mean","sd","min","max"))


sum.list2 <- data(anes.clean0$abs.ideo,anes.clean0$if.extreme,anes.clean0$if.laidoff)

stargazer(anes.clean0[,c(13,15,14)],title="ANES Sumamry Statistics",
          summary.stat = c("n","mean","sd","min","max"))


save(county_sector1,county_sector2,county_sector6,fip.codes,
     cdc,year.period,cd.closures2,cd.closures,lau,
     dw_nominate,dw_nominate2,dw_nominate3,
     quantile.20,quantile.80,mass_layoffs,mass_layoffs2,
     cd.layoffs,anes.clean0,anes.raw,county_income,county_dem_2000_2009,
     county_dem_2010_2017,county_population,file="thesis_base.Rdata")

# key.vars1 <- c("anes.data.range94$ideo",
#                "anes.data.range94$if.laidoff",
#                "dw_nominate$abs.dim1",
#                "mass_layoffs$total",
#                "lau$V10")
# 
# 
#   
# sum.table1 <- data.frame(mean = rep(NA,length(key.vars1)),
#                          SD = rep(NA,length(key.vars1)),
#                          SE = rep(NA,length(key.vars1)),
#                          min = rep(NA,length(key.vars1)),
#                          max = rep(NA,length(key.vars1)))

# sum.table1[1,] <- c(mean(anes.data.range94$ideo),
#                     sd(anes.data.range94$ideo),
#                     sqrt(var(anes.data.range94$ideo)/length(anes.data.range94$ideo)),
#                     min(anes.data.range94$ideo),
#                     max(anes.data.range94$ideo))
# 
# sum.table1[2,] <- c(mean(anes.data.range94$if.laidoff),
#                     sd(anes.data.range94$if.laidoff),
#                     sqrt(var(anes.data.range94$if.laidoff)/length(anes.data.range94$if.laidoff)),
#                     min(anes.data.range94$if.laidoff),
#                     max(anes.data.range94$if.laidoff))
# 
# sum.table1[3,] <- c(mean(dw_nominate$abs.dim1),
#                     sd(dw_nominate$abs.dim1),
#                     sqrt(var(dw_nominate$abs.dim1)/length(dw_nominate$abs.dim1)),
#                     min(dw_nominate$abs.dim1),
#                     max(dw_nominate$abs.dim1))
# 
# sum.table1[4,] <- c(mean(mass_layoffs$total),
#                     sd(mass_layoffs$total),
#                     sqrt(var(mass_layoffs$total)/length(mass_layoffs$total)),
#                     min(mass_layoffs$total),
#                     max(mass_layoffs$total))
# 
# sum.table1[5,] <- c(mean(lau$V10,na.rm = TRUE),
#                     sd(lau$V10,na.rm = TRUE),
#                     sqrt(var(lau$V10,na.rm=TRUE)/length(lau$V10[!is.na(lau$V10)])),
#                     min(lau$V10,na.rm = TRUE),
#                     max(lau$V10,na.rm=TRUE))



#for (n in 1:length(key.vars1)){
   #df_n = get(strsplit(key.vars1[n],"$",fixed=TRUE)[1])
  #var_n = df_n$strsplit(key.vars1[n],"$",fixed=TRUE)[2]
  #sum.table1[n,] <- c(mean(var_n), sqrt(var(var_n)/length(var_n)),
                    #min(var_n),
                    #  max(var_n))
#}
  
#sum.table
#             mean se min max
# ideo
# if.laidoff
# net
# urate

