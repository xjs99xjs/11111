
# Read in data
Rate <- read.csv("~/health-insurance-marketplace/Harvard Project/Rate.csv")


# (3) Count number of beneficiaries of each state
table(Rate$StateCode)

# Remove outliers in Individual Rates
mydata <- Rate[-which(Rate$IndividualRate > 9000),]

# (4) Median cost/person for each state
aggregate(IndividualRate~StateCode,data=mydata,median)


# (5) Mean rate for each state

Meanrate <- aggregate(IndividualRate~StateCode,data=mydata,mean)
Meanrate


# (6) Plot on map by state

library(usmap)
library(ggplot2)

# Changing column name
colnames(Meanrate)[1] <- "state"

# Plot on map by state
plot_usmap(
  data = Meanrate, values = "IndividualRate", lines = "red"
) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Individual Rate", label = scales::comma
  ) + 
  labs(title = "Individual Rate for each State") +
  theme(legend.position = "right")


# (8) Compare with Cancer

USCS_OverviewMap <- read.csv("C:/Users/apple/Downloads/USCS_OverviewMap.csv")

Cancer <- aggregate(AgeAdjustedRate~Area,data=USCS_OverviewMap,mean)

# Delete the states that we do not have data on 
Cancer1 <- Cancer[c(-5:-7,-9,-17,-19,-20,-23,-34,-39,-46:-47),]

# Calculate spearman correlation
cor(Meanrate$IndividualRate,Cancer1$AgeAdjustedRate,method="spearman")

qqplot(Meanrate$IndividualRate,Cancer1$AgeAdjustedRate,
       xlab="Individual Rate",ylab="Cancer Rate",main="Plot of Individual Rate vs. Cancer Rate")



# (9) Compare with Stroke

stroke_mortality <- read.csv("C:/Users/apple/Downloads/stroke_mortality.csv")

# Calculate mean stroke moetality rate
Stroke <- aggregate(RATE~STATE,data=stroke_mortality,mean)


Stroke1 <- Stroke[-which(Stroke$STATE == CA & CO & CT & KY & MA & MD &
                         MN & NY & RI & VT & WA),]

# Delete the states that we do not have data on 

Stroke1 <- Stroke[c(-5:-7,-17,-19,-20,-23,-34,-39,-46:-47),]

# Calculate Spearman correlation
cor(Meanrate$IndividualRate,Stroke1$RATE,method="spearman")

qqplot(Meanrate$IndividualRate,Stroke1$RATE,
       xlab="Mean Rate",ylab="Stroke Mortality Rate",main="Plot of Individual Rate vs. Stroke Motality Rate")
