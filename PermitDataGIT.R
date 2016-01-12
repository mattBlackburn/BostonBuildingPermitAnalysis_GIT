BPData <- read.csv("Building_Permits_2015_Data.csv", header = TRUE)
options(scipen = 999)
require(dplyr)
require(stringr)
require(lubridate)
require(QuantPsyc)
require(ggplot2)
require(ggmap)
require(GGally)
require(Hmisc)



##Cleaned Permit Type Description Variable##

BPData$CleanPermitTypeDescr <- str_replace_all(BPData$PermitTypeDescr,"\xe4\xf3\xf1", "")
BPData$CleanPermitTypeDescr <- str_replace_all(BPData$CleanPermitTypeDescr,"  ", "")
BPData$CleanPermitTypeDescr <- factor(BPData$CleanPermitTypeDescr)





## Converting ISSUED_DATE and from factor to POSIXct date and calculating date range ##

BPData$ISSUED_DATE <- mdy_hm(BPData$ISSUED_DATE)
class(BPData$ISSUED_DATE)

BPData$EXPIRATION_DATE <- mdy_hm(BPData$EXPIRATION_DATE)
class(BPData$EXPIRATION_DATE)



## Creating a Permit Duration Variable ##

BPData$PermitDuration <- as.Date(BPData$EXPIRATION_DATE) - as.Date(BPData$ISSUED_DATE)
BPData$PermitDuration <- ifelse((as.Date(BPData$EXPIRATION_DATE) - as.Date(BPData$ISSUED_DATE) >= 0) 
                                , BPData$PermitDuration, NA)  



##Converting STATE to capital letters in order to eliminate redundant state categories##


BPData$STATE <- toupper(BPData$STATE)



##Converting APPLICANT_STATE to capital letters##


BPData$APPLICANT_STATE <- toupper(BPData$APPLICANT_STATE)



##Inserting a "0" to the front of the zip codes in ZIP##
##The CSV omitted the "0" and the included zip codes are only four digits##

BPData$ZIP <- paste("0", BPData$ZIP, sep='')


##Inserting a "0" to the front of the zip codes in APPLICANT_ZIP##

BPData$APPLICANT_ZIP <- paste("0", BPData$APPLICANT_ZIP, sep='')


##Converting OCCUPANCY to capital letters to eliminate redundant categories##

BPData$OCCUPANCY <- toupper(BPData$OCCUPANCY)



## Replace the negative values with “NA” in the TOTAL_FEES column##

BPData$TOTAL_FEES <- ifelse (BPData$TOTAL_FEES < 0, NA, BPData$TOTAL_FEES)





## Creating a Year Variable ##
BPData$Year_Issued <- year(BPData$ISSUED_DATE)


##Creating a new binary variable to indicate if occupancy is residential##

BPData$IF_RESIDENTIAL <- ifelse(BPData$OCCUPANCY=="1-2FAM", 1,
                                ifelse(BPData$OCCUPANCY=="1-3FAM", 1,
                                       ifelse(BPData$OCCUPANCY=="1-4FAM", 1,
                                              ifelse(BPData$OCCUPANCY=="1-7FAM", 1,
                                                     ifelse(BPData$OCCUPANCY=="1UNIT", 1,
                                                            ifelse(BPData$OCCUPANCY=="2UNIT", 1,
                                                                   ifelse(BPData$OCCUPANCY=="3UNIT", 1,
                                                                          ifelse(BPData$OCCUPANCY=="4UNIT", 1,
                                                                                 ifelse(BPData$OCCUPANCY=="5UNIT", 1,
                                                                                        ifelse(BPData$OCCUPANCY=="6UNIT", 1,
                                                                                               ifelse(BPData$OCCUPANCY=="7MORE", 1,
                                                                                                      ifelse(BPData$OCCUPANCY=="7UNIT", 1,
                                                                                                             ifelse(BPData$OCCUPANCY=="MIXED", 1,
                                                                                                                    ifelse(BPData$OCCUPANCY=="MULTI", 1, 0))))))))))))))                                                                    


##Creating a new binary variable to indicate if occupancy is commercial##

BPData$IF_COMM <- ifelse(BPData$OCCUPANCY=="COMM", 1, 0)



##Creating a new column variable for cleaned STATUS##

require(stringr)
BPData$STATUS_NEW <- BPData$STATUS
BPData$STATUS_NEW <- ifelse(str_detect(BPData$STATUS_NEW, "EXP"), "EXPIRED", BPData$STATUS_NEW)
BPData$STATUS_NEW <- ifelse(str_detect(BPData$STATUS_NEW, "6"), "OPEN", BPData$STATUS_NEW)
BPData$STATUS_NEW <- ifelse(str_detect(BPData$STATUS_NEW, "1"), "NA", BPData$STATUS_NEW)
BPData$STATUS_NEW[is.na(BPData$STATUS_NEW)] <- "NA"



## Creating Major and Minor Changes Permit Variables ##
## http://www.cityofboston.gov/isd/building/permits.asp ##


summary(BPData$CleanPermitTypeDescr)
BPData$MinorChange <- ifelse(str_detect(BPData$CleanPermitTypeDescr, "Short Form Bldg Permit")
                             | str_detect(BPData$CleanPermitTypeDescr, "Electrical")
                             | str_detect(BPData$CleanPermitTypeDescr, "Plumbing")
                             | str_detect(BPData$CleanPermitTypeDescr, "Gas")
                             , 1, 0)




BPData$MajorChange <- ifelse(str_detect(BPData$CleanPermitTypeDescr, "Long Form/Alteration Permit")
                             | str_detect(BPData$CleanPermitTypeDescr, "Certificate of Occupancy")
                             | str_detect(BPData$CleanPermitTypeDescr, "Erect/New Construction")
                             | str_detect(BPData$CleanPermitTypeDescr, "Use of Premises")
                             | str_detect(BPData$CleanPermitTypeDescr, "Foundation Permit")
                             & !str_detect(BPData$CleanPermitTypeDescr, "Street")
                             , 1, 0)





## New Housing Stock Variable ##

BPData$NewHouStock <- ifelse(BPData$CleanPermitTypeDescr == "Erect/New Construction" & BPData$IF_RESIDENTIAL == 1, 1, 0)


## New Commercial Permit Variable Tract Level ## 
BPData$NewConCom <- ifelse(BPData$CleanPermitTypeDescr == "Erect/New Construction" & BPData$IF_COMM == 1, 1, 0)


##Certificate of Occupancy Variable ##
BPData$CertOcc <- ifelse(BPData$CleanPermitTypeDescr == "Certificate of Occupancy", 1, 0)




## New Measurement Variables ##

## Bringing in 2013 ACS 5-Year Estimates MHI Data ##

ACSMHI13 <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/CensusHousingData/ACS_5Year_2013_MHI/ACS_13_5YR_B19013_with_ann.csv", header = TRUE, stringsAsFactors=FALSE)
ACSMHI13 <- ACSMHI13[,c(2,4)]
colnames(ACSMHI13)[1] <- "CT_ID_10"
colnames(ACSMHI13)[2] <- "ACS13_MHI"
ACSMHI13 <- ACSMHI13[-c(1),]
ACSMHI13$CT_ID_10 <- as.numeric(ACSMHI13$CT_ID_10)
ACSMHI13$ACS13_MHI <- as.numeric(ACSMHI13$ACS13_MHI)


# Bringing in 2013 ACS 5-Year Estimates Housing Data ##

ACSHousing13 <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/CensusHousingData/ACS_5Year_2013/ACS_13_5YR_DP04_with_ann.csv", header = TRUE, stringsAsFactors=FALSE)
ACSHousing13 <- ACSHousing13[ ,c(2, 4, 8, 10, 352, 528, 490, 562)]

colnames(ACSHousing13)[1] <- "CT_ID_10"
colnames(ACSHousing13)[2] <- "ACS13UnitsPerTract"
colnames(ACSHousing13)[3] <- "ACS13OccUnitsPerTract"
colnames(ACSHousing13)[4] <- "ACS13PctOccPerTract"
colnames(ACSHousing13)[5] <- "ACS13_MedValOwnerOccupied"
colnames(ACSHousing13)[6] <- "ACS13_MedGrossRent"
colnames(ACSHousing13)[7] <- "ACS13_Owner35PercIncome"
colnames(ACSHousing13)[8] <- "ACS13_Rent35PercIncome"
ACSHousing13 <- ACSHousing13[-c(1),]


ACSHousing13$CT_ID_10 <- as.numeric(ACSHousing13$CT_ID_10)
ACSHousing13$ACS13UnitsPerTract <- as.numeric(ACSHousing13$ACS13UnitsPerTract)
ACSHousing13$ACS13OccUnitsPerTract <- as.numeric(ACSHousing13$ACS13OccUnitsPerTract)
ACSHousing13$ACS13PctOccPerTract <- as.numeric(ACSHousing13$ACS13PctOccPerTract)
ACSHousing13$ACS13_MedValOwnerOccupied <- as.numeric(ACSHousing13$ACS13_MedValOwnerOccupied)
ACSHousing13$ACS13_MedGrossRent <- as.numeric(ACSHousing13$ACS13_MedGrossRent)
ACSHousing13$ACS13_Owner35PercIncome <- as.numeric(ACSHousing13$ACS13_Owner35PercIncome)
ACSHousing13$ACS13_Rent35PercIncome <- as.numeric(ACSHousing13$ACS13_Rent35PercIncome)

ACS2013BPTract <- merge(ACSHousing13,ACSMHI13, by = "CT_ID_10")
write.csv(ACS2013BPTract, "ACS2013_Tract.csv")




## Bring in Commercial Parcel Data ##
CommPar <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/Parcel_Data/Parcels 2015 BARI CSV Comm.csv")
CommPar$land_usage <- str_replace_all(CommPar$land_usage,"CC", 1)
CommPar$land_usage <- str_replace_all(CommPar$land_usage,"C", 1)
CommPar$land_usage <- as.numeric(CommPar$land_usage)
CommTotTract <- aggregate(land_usage ~ CT_ID_10, data = CommPar, sum, na.rm = F)
colnames(CommTotTract)[2] <- "TotalCommTract"



## Bring in Longitudinal Tax Assessor Data ##
LongTaxDB <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/LongTaxDB/TaxLongDB_Midterm_20151117_Tracts.csv", header = TRUE)
LongTaxDBCorr <- LongTaxDB[, c("CT_ID_10", "PercChangeAV2013", "PercChangeAV2014", "PercChangeAV2015")]


## Bring in BARI Tract Data ##
tracts <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/Tracts_Boston_2015_BARI/Tract Census Data.csv")
tractsMerge <- tracts[,c("CT_ID_10","Type", "Res", "GINI", "DT", "Ind", "Park", "homeownership", "popdens", "medyrblt", "EthHet", "medincome")]


## Bring in BARI 2014 Permit Ecometrics ##
BARIPermEco <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/BARIPermitEcometrics/Permits_Ecometrics_CT_2014.csv")
BARIPermEcoPacrel <- data.frame(BARIPermEco[,c("numParcels", "CT_ID_10", "LOCALINV_PC_2014")])




## Aggregate To Tract Level ##

BPDataParcel <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, NewConCom, addition, reno, CertOcc,uppereducation, healthcare, religious, government)~ parcel_num+CT_ID_10, BPData, max)
BPDataTract <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, NewConCom, addition, reno, CertOcc, uppereducation, healthcare, religious, government) ~ CT_ID_10, BPDataParcel, sum)
BPDataTract <- merge(BPDataTract, CommTotTract, by = "CT_ID_10", all.x = TRUE)
BPDataTract <- merge(BPDataTract, ACS2013BPTract, by = "CT_ID_10", all.x = TRUE)   
BPDataTract <- merge(BPDataTract,BARIPermEcoPacrel, by = "CT_ID_10", all.x = TRUE)
BPDataTract$NormMaj <- BPDataTract$MajorChange/BPDataTract$numParcels
BPDataTract$NormMin <- BPDataTract$MinorChange/BPDataTract$numParcels
BPDataTract$NewHouNormPerTract <- BPDataTract$NewHouStock/BPDataTract$numParcels
BPDataTract$NewConComNorm <- BPDataTract$NewConCom/BPDataTract$TotalCommTract
BPDataTract$CertOccNorm <- BPDataTract$CertOcc/BPDataTract$numParcels
BPDataTract$AddNorm <- BPDataTract$addition/BPDataTract$numParcels
BPDataTract$RenNorm <- BPDataTract$reno/BPDataTract$numParcels
BPDataTract <- merge(BPDataTract, LongTaxDBCorr, by = "CT_ID_10", all.x = TRUE)
BPDataTract <- merge(BPDataTract, tractsMerge, by = "CT_ID_10", all.x = TRUE)

write.csv(BPDataTract, "BPDataTract.csv")




## Basic Descriptive Statistics ##

## New Residential Constuction ##
summary(BPDataTract$NewHouNormPerTract)
sd(BPDataTract$NewHouNormPerTract)

summary(BPDataTract$NewHouStock)
sd(BPDataTract$NewHouStock)

BPDataTractNBDNewHouNormPerTract <- aggregate(NewHouNormPerTract) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDNewHouNormPerTract <- BPDataTractNBDNewHouNormPerTract[-c(1),]
BPDataTractNBDNewHouNormPerTract <- BPDataTractNBDNewHouNormPerTract[order(BPDataTractNBDNewHouNormPerTract$NewHouNormPerTract, decreasing = TRUE),]


NewResPerNBD <- ggplot(BPDataTractNBDNewHouNormPerTract, aes(x = reorder(BRA_PD, - NewHouNormPerTract), y = NewHouNormPerTract))

NewResPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='New Residential Construction Permits') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("New Residential Construction Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)




## New Commercial Constuction ##

summary(BPDataTract$NewConComNorm)
sd(BPDataTract$NewConComNorm)

summary(BPDataTract$NewConCom)
sd(BPDataTract$NewConCom)

BPDataTractNBDNewConNormPerTract <- aggregate(NewConComNorm ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDNewConNormPerTract <- BPDataTractNBDNewConNormPerTract[-c(1),]
BPDataTractNBDNewConNormPerTract <- BPDataTractNBDNewConNormPerTract[order(BPDataTractNBDNewConNormPerTract$NewConComNorm, decreasing = TRUE),]

NewConPerNBD <- ggplot(BPDataTractNBDNewConNormPerTract, aes(x = reorder(BRA_PD, - NewConComNorm), y = NewConComNorm))

NewConPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='New Commercial Construction Permits') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("New Commercial Construction Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)




## Major Permits ##

summary(BPDataTract$NormMaj)
sd(BPDataTract$NormMaj)


BPDataTractNBDMaj <- aggregate(cbind(MajorChange, NormMaj) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDMaj <- BPDataTractNBDMaj[-c(1),]
BPDataTractNBDMaj <- BPDataTractNBDMaj[order(BPDataTractNBDMaj$NormMaj, decreasing = TRUE),]


MajPerNBD <- ggplot(BPDataTractNBDMaj, aes(x = reorder(BRA_PD, - NormMaj), y = NormMaj))

MajPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='Permits for Major Projects') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Permits for Major Projects Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)



## Minor Permits ##

BPDataTractNBDMin <- aggregate(cbind(MinorChange, NormMin) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDMin <- BPDataTractNBDMin[-c(1),]
BPDataTractNBDMin <- BPDataTractNBDMin[order(BPDataTractNBDMin$NormMin, decreasing = TRUE),]


MinPerNBD <- ggplot(BPDataTractNBDMin, aes(x = reorder(BRA_PD, - NormMin), y = NormMin))

MinPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='Permits for Minor Projects') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Permits for Minor Projects Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)





## Certificate of Occupation ##


summary(BPDataTract$CertOcc)
sd(BPDataTract$CertOcc)

BPDataTractNBDOcc <- aggregate(cbind(CertOcc, CertOccNorm) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDOcc <- BPDataTractNBDOcc[-c(1),]
BPDataTractNBDOcc <- BPDataTractNBDOcc[order(BPDataTractNBDOcc$CertOccNorm, decreasing = TRUE),]


OccPerNBD <- ggplot(BPDataTractNBDOcc, aes(x = reorder(BRA_PD, - CertOccNorm), y = CertOccNorm))

OccPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='Permits for Certificate of Occupancy') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Permits for Certificates of Occupancy Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)




## Addition Permits ##

summary(BPDataTract$addition)
sd(BPDataTract$addition)

BPDataTractNBDAdd <- aggregate(cbind(addition, AddNorm) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDAdd <- BPDataTractNBDAdd[-c(1),]
BPDataTractNBDAdd <- BPDataTractNBDAdd[order(BPDataTractNBDAdd$AddNorm, decreasing = TRUE),]


AddPerNBD <- ggplot(BPDataTractNBDAdd, aes(x = reorder(BRA_PD, - AddNorm), y = AddNorm))

AddPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='Permits for Addition') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Permits for Addition Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)




## Renovation Permits ##

summary(BPDataTract$reno)
sd(BPDataTract$reno)

BPDataTractNBDReno <- aggregate(cbind(reno, RenNorm) ~ BRA_PD, BPDataTractNBD, mean)
BPDataTractNBDReno <- BPDataTractNBDReno[-c(1),]
BPDataTractNBDReno <- BPDataTractNBDReno[order(BPDataTractNBDReno$RenNorm, decreasing = TRUE),]


RenPerNBD <- ggplot(BPDataTractNBDReno, aes(x = reorder(BRA_PD, - RenNorm), y = RenNorm))

RenPerNBD + geom_point(colour = 'blue', shape = 1, size = 5) + labs(x='Neighborhood', y='Permits for Renovation') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Permits for Renovation Per Neighborhood") + guides(fill = FALSE) +
  geom_segment(aes(xend = BRA_PD), yend=0)



## Mapping Permits ##

require(rgdal)
require(sp)
require(rgeos)
require(ggplot2)
require(ggmap)
require(maptools)
tracts_geo <- readOGR(dsn="/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/Tracts_Boston_2015_BARI", layer="Tracts_Boston BARI")
plot(tracts_geo)
proj4string(tracts_geo)
tracts_geo <- fortify(tracts_geo, region = "CT_ID_10")
tracts_demog <- read.csv("/Users/mattblackburn/Desktop/Building_Permits_Boston/Boston_Building_Permits/Tracts_Boston_2015_BARI/Tract Census Data.csv")
tracts_demog <- merge(BPDataTract, tracts_demog, by = "CT_ID_10", all.x= TRUE)
tracts_geo<-merge(tracts_geo,tracts_demog,by.x='id',by.y='CT_ID_10',all.x=TRUE)
tracts_geo<-tracts_geo[order(tracts_geo$order),]

Boston <- get_map(location=c(left = -71.193799, bottom = 42.15, right = -70.985746, top = 42.5))
Bostonmap <- ggmap(Boston)
Bostonmap

BPBase <- Bostonmap + geom_polygon(aes(x=long, y=lat, group=group), data=tracts_geo, fill = 'grey') + geom_path(aes(x=long, y=lat, group = group), color = 'black', data = tracts_geo)
BPBase


## Map of Total New Housing Permits ##
MapTotalNewHouPermitsBos <- BPBase + geom_polygon(aes(x=long, y=lat, group=group, fill=NewHouNormPerTract), data=tracts_geo) + geom_path(aes(x=long, y=lat, group=group), color = 'black', data=tracts_geo) + scale_fill_gradient(low = "green", high = "blue") + ggtitle("New Residential Construction \n Permits Across Boston") + labs(fill= "New Residential \n Construction \n Permits")
MapTotalNewHouPermitsBos



## Map of Minor Change Permits ##
MapMinorPermitsBos <- BPBase + geom_polygon(aes(x=long, y=lat, group=group, fill=NormMaj), data=tracts_geo) + geom_path(aes(x=long, y=lat, group=group), color = 'black', data=tracts_geo) + scale_fill_gradient(low = "green", high = "blue") + ggtitle("Minor Change Permits \n Across Boston") + labs(fill= "Minor Change \n Permits")
MapMinorPermitsBos


## Map of New Commercial Construction Permits ##
MapComPermitsBos <- BPBase + geom_polygon(aes(x=long, y=lat, group=group, fill=NewConComNorm), data=tracts_geo) + geom_path(aes(x=long, y=lat, group=group), color = 'black', data=tracts_geo) + scale_fill_gradient(low = "green", high = "blue") + ggtitle("New Commercial Construction \n Permits Across Boston") + labs(fill= "New Commercial \n Construction Permits")
MapComPermitsBos



## Map of Certificate of Occupancy Permits ##
CertPermitsBos <- BPBase + geom_polygon(aes(x=long, y=lat, group=group, fill=CertOccNorm), data=tracts_geo) + geom_path(aes(x=long, y=lat, group=group), color = 'black', data=tracts_geo) + scale_fill_gradient(low = "green", high = "blue") + ggtitle("Certificate of Occupancy \n Permits Across Boston") + labs(fill= "Certificate of \n Occpancy Permits")
CertPermitsBos


## Final Correlations ##

BPDataTract[is.na(BPDataTract)] <- 0


BPDataTractCorr<-rcorr(as.matrix(BPDataTract[c("ACS13_MedValOwnerOccupied", 
                                               "ACS13_Owner35PercIncome", 
                                               "ACS13_Rent35PercIncome", 
                                               "NewHouNormPerTract",
                                               "NewConComNorm", 
                                               "AddNorm", "RenNorm", "CertOccNorm", "GINI", 
                                               "medyrblt","popdens", "homeownership", 
                                               "EthHet")]))


BPDataTractCorr[1]
BPDataTractCorr[2]
BPDataTractCorr[3]



BPDataTractCorrGraph <- (BPDataTractCorr[c(1,3)])
write.csv(BPDataTractCorrGraph, "BPDataTractCorrGraph.csv")





# GGally Plot Matrix ##

AllTractCorePlot <- BPDataTract[c("ACS13_MedValOwnerOccupied", 
                                  "ACS13_Rent35PercIncome", 
                                  "NewHouNormPerTract",
                                  "AddNorm", "RenNorm", "CertOccNorm", 
                                  "GINI", "medyrblt","popdens", "homeownership")]






GGMatrixBP <- ggpairs(data = AllTractCorePlot, columns = c(1:10),
                      upper = list(continuous = "density"),
                      lower = list(combo = "facetdensity"),
                      title = "Building Permit Correlations")



GGMatrixBP 




## Final Regressions ##


NewResConLM <- lm(NewHouNormPerTract ~ ACS13_MedValOwnerOccupied 
                  + ACS13_Rent35PercIncome + GINI + medyrblt + popdens + homeownership,
                  data = BPDataTract)

summary(NewResConLM)
lm.beta(NewResConLM)




CommConLM <- lm(NewConComNorm ~ ACS13_MedValOwnerOccupied 
                + ACS13_Rent35PercIncome + GINI + medyrblt + popdens + homeownership,
                data = BPDataTract)

summary(CommConLM)
lm.beta(CommConLM)




AddPermLM <- lm(AddNorm ~ ACS13_MedValOwnerOccupied 
                + ACS13_Rent35PercIncome + GINI + medyrblt + popdens + homeownership,
                data = BPDataTract)

summary(AddPermLM)
lm.beta(AddPermLM)



RenPermLM <- lm(RenNorm ~ ACS13_MedValOwnerOccupied 
                + ACS13_Rent35PercIncome + GINI + medyrblt + popdens + homeownership,
                data = BPDataTract)

summary(RenPermLM)
lm.beta(RenPermLM)




CertOccNormLM <- lm(CertOccNorm ~ ACS13_MedValOwnerOccupied 
                    + ACS13_Rent35PercIncome + GINI + medyrblt + popdens + homeownership,
                    data = BPDataTract)

summary(CertOccNormLM)
lm.beta(CertOccNormLM)



## Regression to predict 2015% Average Assessed Value Change ## 



BPData2013 <- BPData[c(BPData$Year_Issued == "2013"),]

BPData2013Parcel <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, CertOcc, NewConCom, addition, reno)~ parcel_num+CT_ID_10, BPData2013, max)
BPData2013Tract <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, CertOcc, NewConCom, addition, reno) ~ CT_ID_10, BPData2013Parcel, sum)

BPData2013Tract <- merge(BPData2013Tract, BARIPermEcoPacrel, by = "CT_ID_10", all.x = TRUE)                         
BPData2013Tract$NormMaj <- BPData2013Tract$MajorChange/BPData2013Tract$numParcels
BPData2013Tract$NormMin <- BPData2013Tract$MinorChange/BPData2013Tract$numParcels
BPData2013Tract$NewHouNormPerTract <- BPData2013Tract$NewHouStock/BPData2013Tract$numParcels
BPData2013Tract$NewConCom <- BPData2013Tract$NewConCom/BPData2013Tract$numParcels
BPData2013Tract$CertOccNorm <- BPData2013Tract$CertOcc/BPData2013Tract$numParcels
BPData2013Tract$additionNorm <- BPData2013Tract$addition/BPData2013Tract$numParcels
BPData2013Tract$renoNorm <- BPData2013Tract$reno/BPData2013Tract$numParcels
BPData2013Tract <- merge(BPData2013Tract, LongTaxDBCorr, by = "CT_ID_10", all.x = TRUE)
BPData2013Tract <- merge(BPData2013Tract, tractsMerge, by = "CT_ID_10", all.x = TRUE)

BPData2013Tract[is.na(BPData2013Tract)] <- 0



LM2013 <- lm(PercChangeAV2015 ~ NewHouNormPerTract + NewConCom + CertOccNorm + additionNorm + renoNorm, data = BPData2013Tract)
summary(LM2013)
lm.beta(LM2013)



LM2013Ecomet <- lm(PercChangeAV2015 ~ homeownership + medyrblt +  EthHet + GINI, data = BPData2013Tract)
summary(LM2013Ecomet)
lm.beta(LM2013Ecomet)




## Regression with 2 Year Lag ##

BPData2012 <- BPData[c(BPData$Year_Issued == 2012),]
BPData2012Parcel <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, CertOcc, NewConCom, addition, reno)~ parcel_num+CT_ID_10, BPData2012, max)
BPData2012Tract <- aggregate(cbind(MinorChange,MajorChange,NewHouStock, CertOcc, NewConCom, addition, reno) ~ CT_ID_10, data = BPData2012Parcel, sum)
BPData2012Tract <- merge(BPData2012Tract, BARIPermEcoPacrel)                         
BPData2012Tract$NormMaj <- BPData2012Tract$MajorChange/BPData2012Tract$numParcels
BPData2012Tract$NormMin <- BPData2012Tract$MinorChange/BPData2012Tract$numParcels
BPData2012Tract$NormNewHouStock <- BPData2012Tract$NewHouStock/BPData2012Tract$numParcels
BPData2012Tract$NewConCom <- BPData2012Tract$NewConCom/BPData2012Tract$numParcels
BPData2012Tract$CertOccNorm <- BPData2012Tract$CertOcc/BPData2012Tract$numParcels
BPData2012Tract$additionNorm <- BPData2012Tract$addition/BPData2012Tract$numParcels
BPData2012Tract$renoNorm <- BPData2012Tract$reno/BPData2012Tract$numParcels
BPData2012Tract <- merge(BPData2012Tract, LongTaxDBCorr)


BPData2012Tract[is.na(BPData2012Tract)] <- 0



PC_Pred_PermLag <- lm(PercChangeAV2014 ~ NormNewHouStock 
                      + NewConCom + CertOccNorm + additionNorm + renoNorm, data = BPData2012Tract)


summary(PC_Pred_PermLag)
lm.beta(PC_Pred_PermLag)



## Fit Line With Regression #


## Reno Predicted by Homeownership ##
LMBaseRenPerm <- ggplot(data=BPDataTract, aes(x = homeownership, y = RenNorm)) + geom_point(colour = "#009E73") + ylab("Permits for Renovation") + xlab("Homeownership") + ggtitle("Renovation Permits Predicted by Homeownership")
LMRegPlotRen <- LMBaseRenPerm + geom_smooth(method=lm)
LMRegPlotRen


## Major Project Permits Predicted by Homeownership ##
LMBaseMaj <- ggplot(data=BPDataTract, aes(x = homeownership, y = NormMaj)) + geom_point(colour = "#009E73") + ylab("Permits for Major Projects") + xlab("Homeownership") + ggtitle("Permits for Major Projects Predicted by Homeownership")
LMRegPlotMaj <- LMBaseMaj + geom_smooth(method=lm)
LMRegPlotMaj


## Minor Project Permits Predicted by Homeownership ##
LMBaseMin <- ggplot(data=BPDataTract, aes(x = homeownership, y = NormMin)) + geom_point(colour = "#009E73") + ylab("Permits for Minor Projects") + xlab("Homeownership") + ggtitle("Permits for Minor Projects Predicted by Homeownership")
LMRegPlotMin <- LMBaseMin + geom_smooth(method=lm)
LMRegPlotMin


## 2015 % Change Predicted by Renovation Permits ##
LMBase15Change <- ggplot(data=BPData2013Tract, aes(x = RenNorm, y = PercChangeAV2015)) + geom_point(colour = "#009E73") + xlab("Renovation Permits") + ylab("% Change in Appraised \n Value 2014-2015") + ggtitle("Percent Change in Appraised Value 2014-2015 \n Predicted by Renovation Permits")
LMRegPlot15Change <- LMBase15Change + geom_smooth(method=lm)
LMRegPlot15Change


## 2014 % Change Predicted by Renovation Permits 2 Year Lag ##
LMBase14Change2Year <- ggplot(data= BPData2012Tract, aes(x = renoNorm, y = PercChangeAV2015)) + geom_point(colour = "#009E73") + xlab("Renovation Permits") + ylab("% Change in Appraised \n Value 2014") + ggtitle("Percent Change in Appraised Value 2014 \n Predicted by Renovation Permits \ 2 Year Lag")
LMRegLMBase14Change2Year <- LMBase14Change2Year + geom_smooth(method=lm)
LMRegLMBase14Change2Year


## The Quest for the Number of Units ##


library(stringr)
library(dplyr)

NewHousing <- BPData[c(BPData$NewHouStock == 1),]

NewHousing <- aggregate(NewHouStock ~ parcel_num + CT_ID_10 + NOTES, max, data = NewHousing)

NewHousing$NOTES <- as.character(NewHousing$NOTES)

unit <- c("unit", "Unit", "units", "Units", "family", "Family", "apartment", "residential", "Residential")

NewHousingClean <- NewHousing[(grep(paste(unit, collapse="|"), NewHousing$NOTES)),]


for (i in unit){ 
  assign(paste0('New_', i,'_ptrnMatch'), 
         data.frame(unlist( 
           str_extract_all(NewHousingClean$NOTES, paste0('\\d+\\s', i)) 
         ))
  )
}


for(i in unit){
  dat = NewHousingClean$NOTES %>%
    str_detect(pattern = paste0('\\d+\\s', i))
  
  assign(paste0(i, 'filtered'), cbind(dat, NewHousingClean))
  
  assign(paste0(i, 'filtered'), get(paste0(i, 'filtered')) %>%
           filter(dat == T))
  
  units = unlist( 
    str_extract_all(NewHousingClean$NOTES, paste0('\\d+\\s', i))  
  )
  cbind(assign(paste0(i, 'filtered'), cbind(units, get(paste0(i, 'filtered')))))
}

