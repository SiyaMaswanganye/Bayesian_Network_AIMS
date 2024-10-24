library(ggplot2)
library(table1)
library(scales)


###### EDA
head(MyData)
summary(MyData)
str(MyData)

###Table1
table1(~ Year + Sex + `Age range` + `Officer-defined ethnicity` + `Object of search` + Outcome + Outcomes2 + Result+ Legisl + Month + Type | UpdatedSelfDefEth, data = MyData)
table1(~ Type  + Sex + `Officer-defined ethnicity` + Purpose + Outcomes2 + Result+ Legisl  | UpdatedSelfDefEth, data = MyData, caption = "Self-defined ethnicity")
table1(~ Type + `Age range` +`Officer-defined ethnicity`+ UpdatedSelfDefEth + Purpose + Outcomes2 + Legisl  | Sex, data = MyData, caption = "Sex")
table1(~ `Self-defined ethnicity` | UpdatedSelfDefEth, data = MyData, caption = "Self-defined ethnicity")
table1(~ `Self-defined ethnicity` | , data = MyData, caption = "Self-defined ethnicity")
###Missingness of clean data 
sum(is.na(MyData))
colSums(is.na(MyData))
sapply(MyData, function(x) 
  sum(is.na(x)) / length(x) * 100)

ggplot(MyData, aes(x = Month)) + 
  geom_bar( fill = "purple", color = "black") + 
  labs(title = "Incidents Over Time (Monthly)", x = "Date(Months)", y = "Number of Incidents(In thousands)")+
  scale_y_continuous(labels = scales::label_number(scale = 0.001))


#### GenderDist
table(MyData$Sex)
ggplot(MyData, aes(x = Sex)) + 
  geom_bar(fill = "magenta", color = "black") + 
  labs(title = "Gender Distribution")

#### Ethnicity Dist --- This is a problem because the x-lab is to small and the ethnicities have long names. 
table(MyData$UpdatedSelfDefEth, MyData$`Officer-defined ethnicity`)
ggplot(MyData, aes(x = UpdatedSelfDefEth, fill = `Officer-defined ethnicity`) ) + 
  geom_bar(position = "dodge") + 
  labs(title = "Self-Defined vs. Officer-Defined Ethnicity", x = "Cleaned Self-defined Ethnicity", y = "Count per 1000") + 
  scale_y_continuous(labels = scales::label_number(scale = 0.001))
table(MyData$Sex, MyData$`Age range`) 


##### Outcome
table(MyData$Outcomes2)
ggplot(MyData, aes(x = Outcomes2)) +
  geom_bar(fill = "lightgreen") + 
  labs(title = "Outcome Distribution", x= "Outcome of searcher", y = "Counts per 1000")+
  scale_y_continuous(labels = scales::label_number(scale = 0.001))


#Outcome relative to gender
ggplot(MyData, aes(x = Outcomes2, fill = Sex)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Search Outcomes by Gender", x = "Outcome of the search", y = "Count per 1000")+
  scale_y_continuous(labels = scales::label_number(scale = 0.001))


#Outcome relative to Self-defined ethnicity
ggplot(MyData, aes(x = Outcomes2, fill = UpdatedSelfDefEth)) + 
  geom_bar(position = "fill") + 
  labs(title = "Search Outcomes by Self-Defined Ethnicity", x = "Outcome of search")+
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Self-defined Ethnicity")+
  scale_y_continuous(labels = scales::percent)


#Time -Series
ggplot(MyData, aes(x = Year, fill = Outcomes2)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Search Outcomes Over Time", x = "Years", y = "Number of Searches per 1000")+
  scale_fill_discrete(name = "Outcome") +
  scale_y_continuous(labels = scales::label_number(scale = 0.001))





