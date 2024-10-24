MyData<- MyData[, -c(3,4, 5, 6)]

summary(MyData)
############## Data cleaning ###############
# Convert to datetime format
MyData$Date <- as.POSIXct(MyData$Date)  
 
# Extract just the date without time
MyData$Day <- as.Date(MyData$Date)     
     

names(MyData)[10] <- "Dateymd"


# Extract year-month for monthly trends
MyData$Month <- format(MyData$Date, "%m")  
MyData$Year <- format(MyData$Date, "%Y")


# Missingness of the data (Must put in a table)
sum(is.na(MyData))
#colSums(is.na(MyData))
#sapply(SixMonths, function(x) sum(is.na(x)) / length(x) * 100)

#Removal of empty points
MyData <- MyData[!is.na(MyData$`Self-defined ethnicity`) & !is.na(MyData$`Officer-defined ethnicity`), ]
newdata <- newdata[!is.na(newdata$`Self-defined ethnicity`) & !is.na(newdata$`Officer-defined ethnicity`), ]
MyData <- subset(MyData, subset = Type!= "Vehicle search")
MyData <- subset(MyData, subset = Sex != "Missing")
MyData <- subset(MyData, subset = `Age range` != "under 10")
MyData <- subset(MyData, subset = Sex != "Other")


#Restructuring the data 
LegEdit <- c(
  "Misuse of Drugs Act 1971 (section 23)" = "DrugMisuse",
  "Police and Criminal Evidence Act 1984 (section 1)" = "Evidence",
  "Firearms Act 1968 (section 47)" = "Firearms",
  "Criminal Justice and Public Order Act 1994 (section 60)" = "PublicOrder",
  "NA" =  NA
)

MyData$Legisl <- LegEdit[MyData$Legislation]


Purpose <- c(
  "Controlled drugs" = "Drugs",
  "Articles for use in criminal damage" = "Damage",
  "Firearms" = "Weapons",
  "Anything to threaten or harm anyone" = "Threat",
  "Offensive weapons" = "Weapons",
  "Stolen goods" = "StolenGoods",
  "Evidence of offences under the Act" = "Evidence",
  "Fireworks" = "Threat",
  "NA" = NA 
)
MyData$Purpose <- Purpose[MyData$`Object of search`]



Outcomeedit <- c(
  "Nothing found - no further action" = "NFA",
  "Suspect arrested" = "Arrest",
  "Offender given penalty notice" = "Notice",
  "Suspect summonsed to court" = "Summons",
  "Offender given drugs possession warning" = "Caution",
  "Local resolution" = "NFA",
  "Offender cautioned" = "Caution",
  "A no further action disposal" = "NFA",
  "Arrest" = "Arrest",
  "Penalty Notice for Disorder" = "Notice",
  "Khat or Cannabis warning" = "Caution",
  "Summons / charged by post" = "Summons",
  "Community resolution" = "NFA",
  "Caution (simple or conditional)" = "Caution",
  "NA" = NA 
)

# Create a new column with the renamed values
MyData$Outcomes2 <- Outcomeedit[MyData$Outcome]


Resultsedit <- c(
  "Nothing found - no further action" = "NFA",
  "Suspect arrested" = "Arrest",
  "Offender given penalty notice" = "Notice",
  "Suspect summonsed to court" = "Summons",
  "Offender given drugs possession warning" = "Warning",
  "Local resolution" = "Resolved",
  "Offender cautioned" = "Caution",
  "A no further action disposal" = "NFA",
  "Arrest" = "Arrest",
  "Penalty Notice for Disorder" = "Notice",
  "Khat or Cannabis warning" = "Caution",
  "Summons / charged by post" = "Summons",
  "Community resolution" = "Resolved",
  "Caution (simple or conditional)" = "Caution",
  "NA" = NA 
)

# Create a new column with the renamed values
MyData$Result <- Resultsedit[MyData$Outcome]


names(MyData)[3] <- "Sex"


save(MyData, file = "EightYears.Rda")


Self_def_Eth <- c(
  "Black/African/Caribbean/Black British - African" = "Black",
  "Asian/Asian British - Bangladeshi" = "Asian",
  "Black/African/Caribbean/Black British - Caribbean" = "Black",
  "Other ethnic group - Not stated" = "Other",
  "White - English/Welsh/Scottish/Northern Irish/British" = "White",
  "Asian/Asian British - Pakistani" = "Asian",
  "Asian/Asian British - Any other Asian background" = "Asian",
  "White - Any other White background" = "White",
  "White - Irish" = "White",
  "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background" = "Black",
  "Mixed/Multiple ethnic groups - White and Black Caribbean" = "Other",
  "Mixed/Multiple ethnic groups - White and Black African" = "Other",
  "Other ethnic group - Any other ethnic group" = "Other",
  "Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background" = "Other",
  "Asian/Asian British - Indian" = "Asian",
  "Mixed/Multiple ethnic groups - White and Asian" = "Other",
  "Asian/Asian British - Chinese" = "Asian",
  "Other ethnic group - Arab" = "Other",
  "White - Gypsy or Irish Traveller" = "White"
)

# Create a new column with the renamed values
MyData$UpdatedSelfDefEth <- Self_def_Eth[MyData$`Self-defined ethnicity`]


SexChanger <- c("NA"= "Other")
MyData$Sex <- SexChanger[MyData$Sex]


#Logistic Regression 

ResultsvsOther <- c("Arrest" = 1, "NFA" = 0, "Notice" = 0, "Summons" = 0, "Caution" = 0)
LogisticTester$ArrestvsOthers <- ResultsvsOther[MyData$Outcomes2]


NFAvsOther <- c("Arrest" = 0, "NFA" = 1, "Notice" = 0, "Summons" = 0, "Caution" = 0)
LogisticTester$NFAvsOther <- NFAvsOther[MyData$Outcomes2]


combineddata <- rbind(MyData, dummy)
combineddata <- combineddata[order(combineddata$Dateymd), ]

#Bayesian Network and Logistic Regression Prep (Working with 3 Year)
MyData$Year <- as.numeric(MyData$Year)
MyData$Month <- as.numeric(MyData$Month)

oneYearData <- MyData[(MyData$Year > 2023 | (MyData$Year == 2023 & MyData$Month >= 04)) &         
                         +                         (MyData$Year < 2024 | (MyData$Year == 2024 & MyData$Month <= 5)), ]
ThreeYearDataBayesian <- MyData[(MyData$Year > 2021 | (MyData$Year == 2021 & MyData$Month >= 04)) &         
                         +                         (MyData$Year < 2024 | (MyData$Year == 2024 & MyData$Month <= 5)), ]

### Bayesian Network
ThreeYearDataBayesian <- MyData[, -c(2,5,7,8,9,10,14,18,19)]

names(ThreeYearDataBayesian)[3] <- "Age"
names(ThreeYearDataBayesian)[4] <- "Officer_Defined_Eth"
names(ThreeYearDataBayesian)[13] <- "Outcome"


save(MyData, file = "EightYears.Rda")
write.csv(MyData, "8_Year_Data.csv", row.names = FALSE)
write.csv(ThreeYearDataBayesian, "3_Year_Data_Bayesian.csv", row.names = FALSE)

