dmv <- read.csv("C:/Users/Vasudev/Desktop/R/dmv/dmv.csv")
str(dmv)
dmv$X <- NULL
dmv$`Case Vehicle ID` <- NULL
dmv$`Registration Class` <- NULL
dmv$`Direction of Travel` <- NULL
dmv$`Action Prior to Accident` <- NULL
dmv$`Case Individual ID` <- NULL
dmv$`Transported By` <- NULL
dmv$`Type Axles of Truck or Bus` <- NULL
dmv$`Victim Status` <- NULL
dmv$`Event Type` <- NULL
dmv$`Injury Location` <- NULL
dmv$`Injury Descriptor` <- NULL
dmv$`License State Code` <- NULL

table(dmv$`Year`)

##Renaming Columns
colnames(dmv)[1:31] <- c( "Year",
                             "Case Vehicle ID",
                             "Vehicle Body Type",
                             "Registration Class",
                             "Action Prior to Accident",
                             "Type Axles of Truck or Bus", 
                             "Direction of Travel",
                             "Fuel Type",
                             "Vehicle Year",
                             "State of Registration",
                             "Number of Occupants",
                             "Engine Cylinders",
                             "Vehicle Make",
                             "Contributing Factor 1",
                             "Contributing Factor 1 Description",
                             "Contributing Factor 2",
                             "Contributing Factor 2 Description",
                             "Event Type",
                             "Case Individual ID",
                             "Victim Status",
                             "Role Type",
                             "Seating Position",
                             "Ejection",
                             "License State Code",
                             "Gender",
                             "Transported By",
                             "Safety Equipment",
                             "Injury Descriptor",
                             "Injury Location",
                             "Injury Severity",
                             "Age" )

##Cleaning State of Registration Variable.
dmv$`State of Registration` <- as.character(dmv$`State of Registration`)
dmv$`State of Registration`[dmv$`State of Registration` == c("ny")] <- "NY"
dmv$`State of Registration`[dmv$`State of Registration` == c("YT")] <- "VT"
dmv$`State of Registration`[dmv$`State of Registration` == c("X")] <- "TX"
dmv$`State of Registration`[dmv$`State of Registration` == c("-3")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("1")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("2")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("3")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("32")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("6")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("90")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("AB")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("BC")] <- "NC"
dmv$`State of Registration`[dmv$`State of Registration` == c("BY")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("GL")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("IO")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("KT")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("M")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("MB")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("MC")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NB")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NC1")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NK")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NS")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NT")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("ZS")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("US")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("UNK")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("UN")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("SN")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("SK")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("NU")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("OT")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("ON")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("PE")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("PQ")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("QB")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("QC")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("I1")] <- "IL"
dmv$`State of Registration`[dmv$`State of Registration` == c("HI")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("VI")] <- "N/A"
dmv$`State of Registration`[dmv$`State of Registration` == c("PR")] <- "N/A"

##Remove NA and blanks from Sate of Registration
dmv <- dmv[!(is.na(dmv$`State of Registration`) | dmv$`State of Registration`=="N/A"), ] 

dmv$`State of Registration` <- as.factor(dmv$`State of Registration`)
unique(dmv$`State of Registration`)
table(dmv$`State of Registration`)

##Cleaning Vehicle Year variable.
dput(unique(dmv$`Vehicle Year`)) #used to generate code for c("var1-var10...")
dmv$`Vehicle Year` <- as.character(dmv$`Vehicle Year`)

dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("0")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("-3")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1900")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1901")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1902")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1904")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1905")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1906")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1907")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1908")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1909")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1911")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1912")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1919")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1920")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1921")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1923")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1929")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1930")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1931")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1932")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1933")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1934")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1935")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1937")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1939")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1940")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1941")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1942")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1946")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1947")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1948")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1949")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1950")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1951")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1952")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1953")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1954")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1955")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1956")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1957")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1958")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1959")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1960")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1961")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1962")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1963")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1964")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1965")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1966")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1967")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1968")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1969")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1970")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1971")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1972")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1973")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1974")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1975")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1976")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1977")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1978")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("1979")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("200")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("2014")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("3004")] <- "N/A"
dmv$`Vehicle Year`[dmv$`Vehicle Year` == c("2013")] <- "N/A"

##Remove NA and blanks from Sate of Registration
dmv <- dmv[!(is.na(dmv$`Vehicle Year`) | dmv$`Vehicle Year`=="N/A"), ] 
dmv$`Vehicle Year` <- as.integer(dmv$`Vehicle Year`)

unique(dmv$`Vehicle Year`)
table(dmv$`Vehicle Year`)


##Cleaning Fuel Type variable.
unique(dmv$`Fuel Type`)
table(dmv$`Fuel Type`)

dmv$`Fuel Type` <- as.character(dmv$`Fuel Type`)
dmv <- dmv[!(is.na(dmv$`Fuel Type`) | dmv$`Fuel Type`== "Unknown"), ] 
dmv <- dmv[!(is.na(dmv$`Fuel Type`) | dmv$`Fuel Type`== "None"), ] 
dmv <- dmv[!(is.na(dmv$`Fuel Type`) | dmv$`Fuel Type`== "Not Entered"), ] 
dmv$`Fuel Type` <- as.factor(dmv$`Fuel Type`)


##Cleaning Number of Occupants.
unique(dmv$`Number of Occupants`)
table(dmv$`Number of Occupants`)

dmv$`Number of Occupants` <- as.character(dmv$`Number of Occupants`)
dmv$`Number of Occupants`[dmv$`Number of Occupants` == "100"] <- "N/A"
dmv$`Number of Occupants`[dmv$`Number of Occupants` == "123304"] <- "N/A"

dmv <- dmv[!(is.na(dmv$`Number of Occupants`) | dmv$`Number of Occupants`== "N/A"), ] 
dmv$`Number of Occupants` <- as.integer(dmv$`Number of Occupants`)


##Cleaning Gender variable.
unique(dmv$Gender)
table(dmv$Gender)

dmv$Gender <- as.character(dmv$Gender)
dmv$Gender[dmv$Gender == "C"] <- "N/A"
dmv$Gender[dmv$Gender == "f"] <- "F"
dmv$Gender[dmv$Gender == "m"] <- "M"
dmv$Gender[dmv$Gender == "x"] <- "N/A"
dmv$Gender[dmv$Gender == "O"] <- "N/A"
dmv$Gender[dmv$Gender == "u"] <- "U"
dmv$Gender[dmv$Gender == "U"] <- "N/A"

dmv <- dmv[!(is.na(dmv$Gender) | dmv$Gender== ""), ] 
dmv$Gender <- as.factor(dmv$Gender)


##Cleaning Engine Cylinder variable.
unique(dmv$`Engine Cylinders`)
table(dmv$`Engine Cylinders`)

dmv$`Engine Cylinders` <- as.character(dmv$`Engine Cylinders`)
dmv$`Engine Cylinders`[dmv$`Engine Cylinders` == "11"] <- "N/A"
dmv$`Engine Cylinders`[dmv$`Engine Cylinders` == "7"] <- "N/A"
dmv$`Engine Cylinders`[dmv$`Engine Cylinders` == "9"] <- "N/A"
dmv$`Engine Cylinders`[dmv$`Engine Cylinders` == "0"] <- "N/A"

dmv <- dmv[!(is.na(dmv$`Engine Cylinders`) | dmv$`Engine Cylinders`== "N/A"), ] 
dmv$`Engine Cylinders` <- as.factor(dmv$`Engine Cylinders`)


##Cleaning Contributing Factor 1 variable.
unique(dmv$`Contributing Factor 1`)
table(dmv$`Contributing Factor 1`)

dmv$`Contributing Factor 1` <- as.character(dmv$`Contributing Factor 1`)
dmv <- dmv[!(is.na(dmv$`Contributing Factor 1`) | dmv$`Contributing Factor 1` == "N/A"), ]
dmv$`Contributing Factor 1` <- as.factor(dmv$`Contributing Factor 1`)


##Cleaning Contributing Factor 2 variable.
unique(dmv$`Contributing Factor 2`)
table(dmv$`Contributing Factor 2`)

dmv$`Contributing Factor 2` <- as.character(dmv$`Contributing Factor 2`)
dmv <- dmv[!(is.na(dmv$`Contributing Factor 2`) | dmv$`Contributing Factor 2` == "N/A"), ]
dmv$`Contributing Factor 2` <- as.factor(dmv$`Contributing Factor 2`)


##Cleaning Safety Equipment variable.
unique(dmv$`Safety Equipment`)
table(dmv$`Safety Equipment`)

dmv$`Safety Equipment` <- as.character(dmv$`Safety Equipment`)
dmv$`Safety Equipment`[dmv$`Safety Equipment` == "Unknown"] <- "N/A"
dmv$`Safety Equipment`[dmv$`Safety Equipment` == "Not Entered"] <- "N/A"
dmv$`Safety Equipment`[dmv$`Safety Equipment` == "Not Applicable"] <- "N/A"
dmv$`Safety Equipment`[dmv$`Safety Equipment` == "Other*"] <- "N/A"
dmv$`Safety Equipment`[dmv$`Safety Equipment` == ""] <- "N/A"

dmv <- dmv[!(is.na(dmv$`Safety Equipment`) | dmv$`Safety Equipment` == "N/A"), ]
dmv$`Safety Equipment` <- as.factor(dmv$`Safety Equipment`)


## Cleaning Ejection variable.
unique(dmv$Ejection)
table(dmv$Ejection)

dmv$Ejection <- as.character(dmv$Ejection)
dmv$Ejection[dmv$Ejection == "Not Entered"] <- "N/A"
dmv$Ejection[dmv$Ejection == "Unknown"] <- "N/A"
dmv$Ejection[dmv$Ejection == "Not Applicable"] <- "N/A"

dmv <- dmv[!(is.na(dmv$Ejection) | dmv$Ejection == "N/A"), ]
dmv$Ejection <- as.factor(dmv$Ejection)


## Cleaning Vehicle Body Type variable.
unique(dmv$`Vehicle Body Type`)
table(dmv$`Vehicle Body Type`)

dmv$`Vehicle Body Type` <- as.character(dmv$`Vehicle Body Type`)
dmv$`Vehicle Body Type`[dmv$`Vehicle Body Type` == "UNKNOWN TRUCK"] <- "N/A"
dmv$`Vehicle Body Type`[dmv$`Vehicle Body Type` == "SEDAN"] <- "4 DOOR SEDAN"

dmv <- dmv[!(is.na(dmv$`Vehicle Body Type`) | dmv$`Vehicle Body Type` == "N/A"), ]
dmv$`Vehicle Body Type` <- as.factor(dmv$`Vehicle Body Type`)


## Cleaning Role Type variable.
unique(dmv$`Role Type`)
table(dmv$`Role Type`)
dmv$`Role Type` <- as.character(dmv$`Role Type`)
dmv <- dmv[!(is.na(dmv$`Role Type`) | dmv$`Role Type` == "Registrant"), ]
dmv$`Role Type` <- as.factor(dmv$`Role Type`)


## Cleaning Seating Position Variable.
unique(dmv$`Seating Position`)
table(dmv$`Seating Position`)

dmv$`Seating Position` <- as.character(dmv$`Seating Position`)
dmv$`Seating Position`[dmv$`Seating Position` == "Unknown Position In Vehicle"] <- "N/A"
dmv$`Seating Position`[dmv$`Seating Position` == "Unknown Seating Passenger"] <- "Passenger - 3 Front Right"
dmv$`Seating Position`[dmv$`Seating Position` == "Not Entered"] <- "N/A"
dmv$`Seating Position`[dmv$`Seating Position` == "Not Applicable"] <- "N/A"

dmv <- dmv[!(is.na(dmv$`Seating Position`) | dmv$`Seating Position` == "N/A"), ]
dmv$`Seating Position` <- as.factor(dmv$`Seating Position`)

dput(colnames(dmv))


## Final cleaned file.
write.csv(dmv, file = "dmv_final.csv")
str(dmv)
summary(dmv)
