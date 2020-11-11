library(tidyverse)
library(forcats)
library(corrplot)
path ="C:/R - Workspace/IML"
data = paste0(path, "/train.csv")

housing = read_csv(data)


# col_character() 
# col_date()
# col_time() 
# col_datetime() 
# col_double() 
# col_factor() # to enforce, will never be guessed
# col_integer() 
# col_logical() 
# col_number() 
# col_skip()



housing = read_csv(data, col_types = cols(.default = col_factor(), Id = col_character(), LotFrontage = col_double(), LotArea = col_double(),
                                          YearBuilt = col_integer(), YearRemodAdd = col_integer(), MasVnrArea = col_double(),
                                          BsmtFinSF1 = col_double(), BsmtFinSF2= col_double(), BsmtUnfSF =  col_double(),
                                          TotalBsmtSF = col_double(), "1stFlrSF"= col_double(), "2ndFlrSF"= col_double(),
                                          LowQualFinSF =  col_double(), GrLivArea = col_double(), BsmtFullBath = col_integer(),
                                          BsmtHalfBath = col_integer(), FullBath = col_integer(), HalfBath = col_integer(),
                                          BedroomAbvGr = col_integer(), KitchenAbvGr = col_integer(), TotRmsAbvGrd =  col_integer(),
                                          Fireplaces = col_integer(), GarageYrBlt = col_integer(), GarageCars = col_integer(),
                                          GarageArea = col_double(), WoodDeckSF = col_double(), OpenPorchSF= col_double(),
                                          EnclosedPorch= col_double(), "3SsnPorch"= col_double(), ScreenPorch= col_double(),
                                          PoolArea= col_double(), YrSold  = col_integer()
                                          
                                          
                                          ))

missing_perc = as_vector(housing %>% summarise_all(list(name = ~sum(is.na(.))/length(.))))
missing_perc[missing_perc>0.1]
missing_perc[missing_perc>0]

#############################
#Analysis of missing variables first!

# 1) Alley: indicates the type of alley
summary(housing$Alley)
#seems super useless, only have 91 observations that indicate if gravel or paved alley, and how does that even matter?
#DROP
housing = select(housing, -Alley)

# 2) PoolQC: indicates the quality of pool
summary(housing$PoolQC) 
#Only 7 non NA observations, just drop them
#DROP
housing = select(housing, -PoolQC)

# 3) Fence: indicates Fence quality
summary(housing$Fence) 
#Also 80% missings, and variable shouldnt be super important overall, who decides on fence quality to buy a house?
#DROP
housing = select(housing, -Fence)

# 4) MiscFeature: dont even know, what this should mean
summary(housing$MiscFeature)
#But with 96% missing we cant use it anyway
#DROP
housing = select(housing, -MiscFeature)

# 5) FireplaceQu : indicates quality of fireplaces
summary(housing$FireplaceQu)
summary(housing$Fireplaces)
#NAs here mean, that no fireplace is available, this might be something, we could use over the number of fireplaces, if we want to?
#Could also try to reduce amount of categories if we want to
#First set all NAs to no fireplace:
housing$FireplaceQu = fct_explicit_na(housing$FireplaceQu, "No")
summary(housing$FireplaceQu)
#NOT SURE IF DROP
housing = select(housing, -Fireplaces)

# 6) Garage variables
summary(housing$GarageArea)
housing %>% filter(GarageArea==0) %>% count()  #81 with garagearea = 0 --> no garage
summary(housing$GarageCars) 
summary(housing$GarageCond) #No garage means NA heres
summary(housing$GarageQual) #Garage quality and condition are almost exactly equal, and have almost no variation at all (1311 are average and only 3 poor)
housing$GarageQual = fct_explicit_na(housing$GarageQual, "No") #NAs are only there when no garage, so replace NA with real value
summary(housing$GarageFinish) #What does that even mean, that some have an unfinished garage?
summary(housing$GarageYrBlt) #I dont think the year matters at all, if the condition is good/bad :)
summary(housing$GarageType) #Type somehow describes, how garage relates to house, probably also not super important
#Takeaways: 
#   - year & type seem not important
#   - We either keep area or cars .... I would say cars, because thats what matters
#   - if we really want to have some additional indication about garage quality, we could try to make the GarageQual variable smaller
#DROP: GarageArea, GarageCond, GarageFinish, GarageYrBlt, GarageType
#MAYBE DROP: GarageQual
#KEEP: GarageCars
housing = select(housing, -GarageArea, -GarageCond, -GarageFinish, -GarageYrBlt, -GarageType)
 

# 7) Basement variables: very similar structure to garage
summary(housing$TotalBsmtSF) #thats probably the one to keep for sure, since it gives us the size of the basement
housing %>% filter(TotalBsmtSF == 0) %>% count() #37 with no basement.. all NAs below are ther, when no basement (also exactly 37)
summary(housing$BsmtCond) #Condition of basement, not a lot variation, if we really want to keep it, need to somehow group categories together
summary(housing$BsmtExposure) #refers to walkout, probably useless
summary(housing$BsmtFinType1) #refers to what kind of basement it is (this could be one to keep, since quite a lot of variaton)
housing$BsmtFinType1 = fct_explicit_na(housing$BsmtFinType1, "No")
summary(housing$BsmtFinType1)
summary(housing$BsmtFinSF1) #and that to area of this type, thats for sure a drop, because overall area is more than enough
summary(housing$BsmtFinType2) #if there are multiple types of basements in house, this refers to 2nd type --> almost no variation, just drop
summary(housing$BsmtFinSF2) #see above
summary(housing$BsmtUnfSF) #refers to amount of unfinished area.. I would drop it, since area itself, and maybe type are much more important for sure
summary(housing$BsmtFullBath) #seriously, just drop it, the type and area is enough
summary(housing$BsmtHalfBath) #Wtf?
summary(housing$BsmtQual) #Refers to height of basement, probably not needed
housing$BsmtQual = fct_explicit_na(housing$BsmtQual, "No") #Since NA means no basement
#DROP: BsmtCond, BsmtExposure, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, BsmtFullBath, BsmtHalfBath
#MAYBE DROP: BsmtFinType1, BsmtQual
#KEEP: TotalBsmtSF
housing = select(housing, -BsmtCond, -BsmtExposure, -BsmtFinSF1, -BsmtFinType2, -BsmtFinSF2, -BsmtUnfSF, -BsmtFullBath, -BsmtHalfBath)


# 8) LotFrontage: Linear feet of street connected to property
summary(housing$LotFrontage) #259 NAs in there (maybe because no direct road connected to it, but thats just a guess?)
#In general dont really see, how this should vastly effect house price (besides having maybe slightly more cars driving by?)
#Probably positively correleated by size of property:
cor(housing$LotFrontage, housing$LotArea, use="complete.obs", method="pearson")
cor(housing$LotFrontage, housing$LotArea, use="complete.obs", method="spearman")
#Yep: larger area means obviously more street connected to property
#Since there is also LotConfig, we can probably remove that one (might later on try it with NA = 0 if we really want to)
#DROP
housing = select(housing, - LotFrontage)


# 9) Masonry variables: these should be some type of exterior (almost decoration) on outside wall with not too much impact ("primarily used for its appearance")
summary(housing$MasVnrType)
summary(housing$MasVnrArea) #the area probably doesnt matter as much, as the type (if even that matters)
#We have some missings in there, so dropping it would be nice, but let's see how they relate to other variables:

ggplot(housing, aes(ExterQual, ..count..)) + geom_bar(aes(fill = MasVnrType), position = "dodge") #exterior quality definitely captures a lot of variance in MasVnrType
ggplot(housing, aes(OverallQual, ..count..)) + geom_bar(aes(fill = MasVnrType), position = "dodge") #overall quality also seems to capute some of it
#We are probably fine deleting both variables & not having to deal with the NAs in there
#DROP: MasVnrType, MasVnrArea
housing = select(housing, -MasVnrArea, -MasVnrType)

#10) Eletrical
#Last variable where observations missing :)
housing %>% filter(is.na(Electrical)) #only one single observation with a missing value here
summary(housing$Electrical) 
#Probably fine just deleting that one single observation 
housing = housing %>% filter(!is.na(Electrical))

###########################
#Analyse variables further:

#Take numerics for now and analyse correlation:
numerics = select_if(housing, is.numeric)
corrs = cor(numerics)

corrplot(corrs, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#1) GrLivArea (above ground living area)strongly correlated with: TotRmsAbvGrd (total roombs above ground) 
cor(housing$BedroomAbvGr, housing$GrLivArea)
cor(housing$TotRmsAbvGrd, housing$GrLivArea)
cor(housing$FullBath, housing$GrLivArea)
cor(housing$`1stFlrSF`, housing$TotalBsmtSF)
cor(housing$`1stFlrSF`, housing$GrLivArea)
cor(housing$`2ndFlrSF`, housing$GrLivArea)
cor(housing$TotalBsmtSF, housing$GrLivArea)
#--> probably good enough to keep the amount of living area instead of number of rooms
housing = housing %>% select(-TotRmsAbvGrd )
# --> correlation between 1stFlrSF, 2ndFlrSF and GrLivArea also high... lets drop them for now, dont seem to interesting also for hypothesis later on
#BUT: we can use the number of floors instead :)
housing = housing %>% mutate(numb_add_flr = ifelse(`1stFlrSF`>0, 1, 0))
housing = housing %>% mutate(numb_add_flr = ifelse(`2ndFlrSF`>0, 2, numb_add_flr))
housing = housing %>% select(-"1stFlrSF", -"2ndFlrSF" )


#2) LandContour & Landslope
ggplot(housing, aes(LandContour, ..count..)) + geom_bar(aes(fill = LandSlope), position = "dodge") #both capture almost the same thing
#lets only keep one of them then:
housing = housing %>% select(-LandContour)

#3) Style of dwelling & MSSubClass & numb_add_flr
ggplot(housing, aes( HouseStyle, ..count..)) + geom_bar(aes(fill = MSSubClass), position = "dodge") 
ggplot(housing, aes( HouseStyle, ..count..)) + geom_bar(aes(fill = numb_add_flr), position = "dodge") 
#Dont think we need a combination of HouseStyle together with numb_add_flr & OverallQual --> numb_add_flr & OverallQual should measure exactly that together
#Since: OverallQual: Rates the overall material and finish of the house
#And: HouseStyle: Style of dwelling
# 1Story	One story
# 1.5Fin	One and one-half story: 2nd level finished
# 1.5Unf	One and one-half story: 2nd level unfinished
# 2Story	Two story
# 2.5Fin	Two and one-half story: 2nd level finished
# 2.5Unf	Two and one-half story: 2nd level unfinished
# SFoyer	Split Foyer
# SLvl	Split Level
housing = housing %>% select(-HouseStyle)

#4) What about MSSubClass, numb_add_flr & YearBuilt?

# MSSubClass: Identifies the type of dwelling involved in the sale.	
# 
# 20	1-STORY 1946 & NEWER ALL STYLES
# 30	1-STORY 1945 & OLDER
# 40	1-STORY W/FINISHED ATTIC ALL AGES
# 45	1-1/2 STORY - UNFINISHED ALL AGES
# 50	1-1/2 STORY FINISHED ALL AGES
# 60	2-STORY 1946 & NEWER
# 70	2-STORY 1945 & OLDER
# 75	2-1/2 STORY ALL AGES
# 80	SPLIT OR MULTI-LEVEL
# 85	SPLIT FOYER
# 90	DUPLEX - ALL STYLES AND AGES
# 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# 150	1-1/2 STORY PUD - ALL AGES
# 160	2-STORY PUD - 1946 & NEWER
# 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# 190	2 FAMILY CONVERSION - ALL STYLES AND AGES

#Probably fine to kick out MSSubClass, since we cover the number of floors and the year anyway + the interpretation for IML is questionable if we use this subclass variable
housing = housing %>% select(-MSSubClass)


#5) Condition 1 & Condition 2 seem not very interesting for our IML task:

# Condition1: Proximity to various conditions
# 
# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
#        RRAn	Adjacent to North-South Railroad
#        PosN	Near positive off-site feature--park, greenbelt, etc.
#        PosA	Adjacent to postive off-site feature
#        RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad
# 
# Condition2: Proximity to various conditions (if more than one is present)
# 
# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
#        RRAn	Adjacent to North-South Railroad
#        PosN	Near positive off-site feature--park, greenbelt, etc.
#        PosA	Adjacent to postive off-site feature
#        RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad

housing = housing %>% select(-Condition1, -Condition2)

#6) LotConfig

# LotConfig: Lot configuration
# 
# Inside	Inside lot
# Corner	Corner lot
# CulDSac	Cul-de-sac
# FR2	Frontage on 2 sides of property
# FR3	Frontage on 3 sides of property

summary(housing$LotConfig) #most of them inside or corner
#Probably also not very interesting for our IML task, lets drop it for now
housing = housing %>% select(-LotConfig)

#7) RoofStyle: describes type of roof & RoofMatl: Roof material

summary(housing$RoofMatl) #Roof material has no variation at all, makes no sense to keep, would only lead to overfitting
summary(housing$RoofStyle) #NOT SURE YET about dropping that
housing = housing %>% select(-RoofMatl)

#8)




#corrplot::corrplot(DescTools::PairApply(df, DescTools::CramerV))




