housing = read_csv(path_train, col_types = cols(.default = col_factor(), Id = col_character(), LotFrontage = col_double(), LotArea = col_double(),
                                          YearBuilt = col_integer(), YearRemodAdd = col_integer(), MasVnrArea = col_double(),
                                          BsmtFinSF1 = col_double(), BsmtFinSF2= col_double(), BsmtUnfSF =  col_double(),
                                          TotalBsmtSF = col_double(), "1stFlrSF"= col_double(), "2ndFlrSF"= col_double(),
                                          LowQualFinSF =  col_double(), GrLivArea = col_double(), BsmtFullBath = col_integer(),
                                          BsmtHalfBath = col_integer(), FullBath = col_integer(), HalfBath = col_integer(),
                                          BedroomAbvGr = col_integer(), KitchenAbvGr = col_integer(), TotRmsAbvGrd =  col_integer(),
                                          Fireplaces = col_integer(), GarageYrBlt = col_integer(), GarageCars = col_integer(),
                                          GarageArea = col_double(), WoodDeckSF = col_double(), OpenPorchSF= col_double(),
                                          EnclosedPorch= col_double(), "3SsnPorch"= col_double(), ScreenPorch= col_double(),
                                          PoolArea= col_double(), YrSold  = col_integer(), SalePrice = col_double(), MiscVal =  col_double()
                                          
                                          
))
#------------
#Need to to some cleaning, because if we use one-hot encoding, far too many variables otherwise
#Keep another dataset, where we don't drop variables as well for comparison (only do some collapsing)
housing_full = housing

#------------------------------------
#Analysis of missing variables first!
#Check which are missing  missingness
missing_perc = as_vector(housing %>% summarise_all(list(name = ~sum(is.na(.))/length(.))))
missing_perc[missing_perc>0.1]
missing_perc[missing_perc>0]

# 1) Alley: indicates the type of alley
summary(housing$Alley)
#Seems not usable, only have 91 observations that indicate if gravel or paved alley
#DROP
housing = select(housing, -Alley)
housing_full$Alley = fct_explicit_na(housing_full$Alley, "Missing")

# 2) PoolQC: indicates the quality of pool
summary(housing$PoolQC) 
#Only 7 non NA observations (apparently only 7 houses have a pool), drop them, since 
# we have a pool size as well and here simply not enough observations per category
#DROP
housing = select(housing, -PoolQC)
housing_full$PoolQC = fct_explicit_na(housing_full$PoolQC, "Missing")

# 3) Fence: indicates Fence quality
summary(housing$Fence) 
#Also 80% missings and quite a lot of different categories, can probably drop it
#DROP
housing = select(housing, -Fence)
housing_full$Fence = fct_explicit_na(housing_full$Fence, "Missing")

# 4) MiscFeature: stuff like elevator, shed, tennis court, etc.
summary(housing$MiscFeature)
#96% missigness & quite a lot of categories, so let's drop it for now
#DROP
housing = select(housing, -MiscFeature)
housing_full$MiscFeature = fct_explicit_na(housing_full$MiscFeature, "Missing")

# 5) FireplaceQu : indicates quality of fireplaces
summary(housing$FireplaceQu)
summary(housing$Fireplaces)
#NAs here mean, that no fireplace is available, this might be something, we could use over the number of fireplaces, if we want to?
#Could also try to reduce amount of categories if we want to
#First set all NAs to no fireplace:
housing$FireplaceQu = fct_explicit_na(housing$FireplaceQu, "No")
housing_full$FireplaceQu = fct_explicit_na(housing_full$FireplaceQu, "No")
summary(housing$FireplaceQu)
cor(housing$Fireplaces, housing$GrLivArea)
#Lets keep the quality of them, because number (obviously) correlated with size of houses & quality itself should be more important
#DROP
housing = select(housing, -Fireplaces)

# 6) Garage variables
summary(housing$GarageArea)
housing %>% filter(GarageArea==0) %>% count()  #81 with garagearea = 0 --> no garage
summary(housing$GarageCars) 
cor(housing$GarageCars, housing$GarageArea, use="complete.obs") #very strong correlation
#As expected garage cars and area basically measure the same thing --> keeping one of them should be good enough
summary(housing$GarageCond) #No garage means NA heres
summary(housing$GarageQual) #Garage quality and condition are almost exactly equal, and have almost no variation at all (1311 are average and only 3 poor)
#See:
ggplot(housing, aes(GarageQual, ..count..)) + geom_bar(aes(fill = GarageCond), position = "dodge")
housing$GarageQual = fct_explicit_na(housing$GarageQual, "No") 
housing_full$GarageQual = fct_explicit_na(housing_full$GarageQual, "No")

summary(housing$GarageYrBlt) 
cor(housing$GarageYrBlt, housing$YearBuilt, use="complete.obs") #as expected are yearbuilt & garage year very strongly correlated
#Probably good enough to keep the year in which a house was built, if we also have quality

summary(housing$GarageType) #Type somehow describes, how garage relates to house
#Have a lot of different categories here, that we can't really collapse
#Let's drop it for now, because otherwise a ton of variables in one-hot encoding

summary(housing$GarageFinish) #GarageFinish is the condition of the interior of the garage
#Based on impact in model later also dropped

#Takeaways: 
#DROP: GarageArea, GarageCond, GarageFinish, GarageYrBlt, GarageType
#KEEP: GarageCars, GarageQual
housing = select(housing, -GarageArea, -GarageCond, -GarageFinish, -GarageYrBlt, -GarageType)
housing_full$GarageType = fct_explicit_na(housing_full$GarageType, "Missing")
housing_full$GarageFinish = fct_explicit_na(housing_full$GarageFinish, "Missing")
housing_full$GarageCond = fct_explicit_na(housing_full$GarageCond, "Missing")
housing_full$GarageCond <- factor(housing_full$GarageCond, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex", "Missing" ),
                           labels=c( "3",   "4",   "2",   "1",   "5", "0"  ) )
housing_full$GarageCond  = as.numeric(levels(housing_full$GarageCond))[housing_full$GarageCond]
housing_full = housing_full %>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),0, GarageYrBlt))


# 7) Basement variables: very similar structure to garage
summary(housing$TotalBsmtSF) #thats probably the one to keep for sure, since it gives us the size of the basement
housing %>% filter(TotalBsmtSF == 0) %>% count() #37 with no basement.. all NAs below are there, when no basement (also exactly 37)
summary(housing$BsmtCond) #Condition of basement, not a lot variation, if we really want to keep it, need to somehow group categories together
summary(housing$BsmtExposure) #refers to walkout, a lot of categories with a lot of them having no walkout --> drop for now
summary(housing$BsmtFinType1) #refers to what kind of basement it is (this could be one to keep, since quite a lot of variaton with a lot of observations in each)
housing$BsmtFinType1 = fct_explicit_na(housing$BsmtFinType1, "No")
housing_full$BsmtFinType1 = fct_explicit_na(housing_full$BsmtFinType1, "No")
summary(housing$BsmtFinType1)


summary(housing$BsmtFinSF1) #area of type 1, that should be definitely not needed, because area of 1 + 2 = total area
summary(housing$BsmtFinType2) #if there are multiple types of basements in house, this refers to 2nd type 
# --> almost no variation, because most of houses don't have 2nd type of basement
# for remaining ones there are only a few observations for each category, so just drop the 2nd type of basement

summary(housing$BsmtFinSF2) #same as for type 1
summary(housing$BsmtUnfSF) #refers to amount of unfinished area
# Drop it for now, since we have total area anyway + using quality and type in addition should be enough

summary(housing$LowQualFinSF) #same as for other area variables

summary(housing$BsmtQual) #Refers to height of basement, quite a lot of variation, seems useful 
housing$BsmtQual = fct_explicit_na(housing$BsmtQual, "No") #Since NA means no basement
housing_full$BsmtQual = fct_explicit_na(housing_full$BsmtQual, "No") 

summary(housing$BsmtFullBath) #seriously, just drop it, the type and area is enough
summary(housing$BsmtHalfBath) #Almost only 0s here
#Drop it for now, because probably  type and area are enough, but can later on still include it (or try it out)

#Takeaway:
#DROP: BsmtCond, BsmtExposure, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, BsmtFullBath, BsmtHalfBath
#KEEP: TotalBsmtSF, BsmtFinType1, BsmtQual
housing = select(housing, -BsmtCond, -BsmtExposure, -BsmtFinSF1, -BsmtFinType2, -BsmtFinSF2, -BsmtUnfSF, 
                 -BsmtFullBath, -BsmtHalfBath, -LowQualFinSF)

#For housing_full need to keep all other variables as well and format them:
housing_full$BsmtCond = fct_explicit_na(housing_full$BsmtCond, "No")
housing_full$BsmtCond <- factor(housing_full$BsmtCond, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex", "No" ),
                                  labels=c( "3",   "4",   "2",   "1",   "5", "0"  ) )
housing_full$BsmtCond  = as.numeric(levels(housing_full$BsmtCond))[housing_full$BsmtCond]

housing_full$BsmtExposure = fct_explicit_na(housing_full$BsmtExposure, "No")
housing_full$BsmtFinType2 = fct_explicit_na(housing_full$BsmtFinType2, "No")

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
housing_full = housing_full %>% mutate(LotFrontage = ifelse(is.na(LotFrontage),0, LotFrontage))

# 9) Masonry variables: these should be some type of exterior (almost decoration) on outside wall with not too much impact ("primarily used for its appearance")
summary(housing$MasVnrType) #Type doesnt really have a lot of variation + 8 NAs in there
summary(housing$MasVnrArea) #Area given a type
#We have some missings in there, so dropping it would be nice, but let's see how they relate to other variables:

ggplot(housing, aes(ExterQual, ..count..)) + geom_bar(aes(fill = MasVnrType), position = "dodge") #exterior quality definitely captures a lot of variance in MasVnrType
ggplot(housing, aes(OverallQual, ..count..)) + geom_bar(aes(fill = MasVnrType), position = "dodge") #overall quality also seems to capute some of it
#We are probably fine deleting both variables & not having to deal with the NAs in there
#DROP: MasVnrType, MasVnrArea
housing = select(housing, -MasVnrArea, -MasVnrType)
housing_full$MasVnrType = fct_explicit_na(housing_full$MasVnrType, "Missing")
housing_full = housing_full %>% mutate(MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea))

#10) Electrical
#Last variable where observations missing :)
housing %>% filter(is.na(Electrical)) #only one single observation with a missing value here
summary(housing$Electrical) 
#Probably fine just deleting that one single observation 
housing = housing %>% filter(!is.na(Electrical))
housing_full = housing_full %>% filter(!is.na(Electrical))

###########################
#Analyse variables further:

#Take numerics for now and analyse correlation between features:
numerics = select_if(housing, is.numeric)
corrs = cor(numerics)
corrplot(corrs, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#11) GrLivArea (above ground living area) strongly correlated with: TotRmsAbvGrd (total roombs above ground) 
cor(housing$BedroomAbvGr, housing$GrLivArea)
cor(housing$TotRmsAbvGrd, housing$GrLivArea)
cor(housing$FullBath, housing$GrLivArea)
cor(housing$`1stFlrSF`, housing$TotalBsmtSF)
cor(housing$`1stFlrSF`, housing$GrLivArea)
cor(housing$`2ndFlrSF`, housing$GrLivArea)
cor(housing$TotalBsmtSF, housing$GrLivArea)
#--> probably good enough to keep the amount of living area instead of number of rooms
#Additionally keep all other information relating to specific rooms (e.g. bedrooms or bathrooms)
housing = housing %>% select(-TotRmsAbvGrd )
# --> correlation between 1stFlrSF, 2ndFlrSF and GrLivArea also high... lets drop them for now, dont seem to interesting also for hypothesis later on
#BUT: we can use the number of floors instead :)
housing = housing %>% mutate(numb_add_flr = ifelse(`1stFlrSF`>0, 1, 0))
housing = housing %>% mutate(numb_add_flr = ifelse(`2ndFlrSF`>0, 2, numb_add_flr))
housing = housing %>% select(-"1stFlrSF", -"2ndFlrSF" )


#12) LandContour & Landslope
summary(housing$LandContour) #Almost no variation at all
ggplot(housing, aes(LandContour, ..count..)) + geom_bar(aes(fill = LandSlope), position = "dodge") # + both capture almost the same thing
#lets only keep one of them:
housing = housing %>% select(-LandContour)

#13) Style of dwelling & MSSubClass & numb_add_flr
summary(housing$HouseStyle)
ggplot(housing, aes( HouseStyle, ..count..)) + geom_bar(aes(fill = MSSubClass), position = "dodge") 
ggplot(housing, aes( HouseStyle, ..count..)) + geom_bar(aes(fill = numb_add_flr), position = "dodge") 
#Dont think we need a combination of HouseStyle together with numb_add_flr & OverallQual 
# --> should measure exactly that together
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

#14) What about MSSubClass, numb_add_flr & YearBuilt?

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
#Probably fine to kick out MSSubClass, since we cover the number of floors and the year anyway
housing = housing %>% select(-MSSubClass)


#15) Condition 1 & Condition 2 


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

summary(housing$Condition1)
summary(housing$Condition2)
#Far too many categories and not enough variation
# + collapsing not really possible here, so let's drop it
housing = housing %>% select(-Condition1, -Condition2)

#16) LotConfig
# LotConfig: Lot configuration
# 
# Inside	Inside lot
# Corner	Corner lot
# CulDSac	Cul-de-sac
# FR2	Frontage on 2 sides of property
# FR3	Frontage on 3 sides of property

summary(housing$LotConfig) #most of them inside or corner --> not a lot of variation
#In later analysis also not really an effect, just a lot of variables for one-hot encoding, so drop it:
housing = housing %>% select(-LotConfig)

#17) RoofStyle: describes type of roof & RoofMatl: Roof material
summary(housing$RoofMatl) #Roof material has no variation at all, makes no sense to keep, would only lead to overfitting
summary(housing$RoofStyle) #Here quite a lot of variation actually, and probably want to keep one variable describing roof
housing = housing %>% select(-RoofMatl)


#18) Poolarea
summary(housing$PoolArea)
# As talked about earlier, only 7 observations with pool.. so let's make a dummy, if a house has a pool or not and keep that
# (to avoid overfitting)
housing = housing %>%  mutate(pool=ifelse(PoolArea>0, 1, 0))

#19) YrSold
summary(housing$YrSold)
#All houses sold between 2006 and 2010
#In analysis this had no effect at all, so drop it:
housing = select(housing, -YrSold)



library(purrr)

#20) ID
#ID definitely doesnt matter :)
housing <- select(housing, -Id)
housing_full <- select(housing_full, -Id)

#21) Street
summary(housing$Street)
#No variation at all! Only 6 houses with gravel road, so drop it
housing <- select(housing, -Street)

#22) Utilities
#same with Utilities, only 1 value differs
housing <- select(housing, -Utilities)

#---------------
#Categorical variables
#if there are <= 5 different values with considerable(>=10) amount of observations in each,
#then transform them to categorical
categorical <- c("MSZoning", "LotShape", "LandSlope","BldgType", "ExterQual", 
                 "BsmtQual",  "CentralAir", "KitchenQual", "FireplaceQu", "PavedDrive")
for (name in categorical) {
  housing[name] <- as_factor(housing[name])
  housing_full[name] <- as_factor(housing_full[name])
}
#For all of the remaining categories now collapse if not enough variaton:

#23) Central air
#Simply to make it easier transform to 1/0 instead of Y/N
housing$CentralAir <- factor(housing$CentralAir, levels=c("Y", "N"), labels=c("1","0"))
housing_full$CentralAir <- factor(housing_full$CentralAir, levels=c("Y", "N"), labels=c("1","0"))

#24) Roofstyle: has several types with few observations -> collapse
housing$RoofStyle <- factor(housing$RoofStyle, levels=c("Gable", "Hip", "Gambrel", "Mansard", "Flat", "Shed"), 
                            labels=c("Gable", "Hip", "Other", "Other", "Other", "Other"))
housing_full$RoofStyle <- factor(housing_full$RoofStyle, levels=c("Gable", "Hip", "Gambrel", "Mansard", "Flat", "Shed"), 
                            labels=c("Gable", "Hip", "Other", "Other", "Other", "Other"))

#25) SaleType: same!
#might also collapse COD and Other if we want to
housing$SaleType <- factor(housing$SaleType, levels=c("WD", "New", "COD", "ConLD", "ConLI", "CWD", "ConLw", "Con", "Oth"), 
                           labels=c("WD", "New", "COD", "Other", "Other", "Other", "Other", "Other", "Other"))
housing_full$SaleType <- factor(housing_full$SaleType, levels=c("WD", "New", "COD", "ConLD", "ConLI", "CWD", "ConLw", "Con", "Oth"), 
                           labels=c("WD", "New", "COD", "Other", "Other", "Other", "Other", "Other", "Other"))
#26) Electrical 
housing$Electrical <- factor(housing$Electrical, levels=c("SBrkr", "FuseF", "FuseA", "FuseP", "Mix"),
                             labels=c("SBrkr", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix"))
housing_full$Electrical <- factor(housing_full$Electrical, levels=c("SBrkr", "FuseF", "FuseA", "FuseP", "Mix"),
                             labels=c("SBrkr", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix"))

#27) FireplaceQu: Change categories to numerical, since ratings
housing$FireplaceQu <- factor(housing$FireplaceQu, levels=c("TA", "Gd", "Fa", "Ex", "Po", "No"),
                              labels=c("3", "4", "2", "5", "1", "0"))
housing$FireplaceQu  = as.numeric(levels(housing$FireplaceQu))[housing$FireplaceQu]
housing_full$FireplaceQu <- factor(housing_full$FireplaceQu, levels=c("TA", "Gd", "Fa", "Ex", "Po", "No"),
                              labels=c("3", "4", "2", "5", "1", "0"))
housing_full$FireplaceQu  = as.numeric(levels(housing_full$FireplaceQu))[housing_full$FireplaceQu]

#28) Functional: can collapse by degree of deduction (moderate went to minimal, severe went to major)
housing$Functional <- factor(housing$Functional, levels=c("Typ", "Min1", "Maj1", "Min2", "Mod", "Maj2", "Sev"),
                             labels=c("Typ", "Min", "Maj", "Min", "Min", "Maj", "Maj"))
housing_full$Functional <- factor(housing_full$Functional, levels=c("Typ", "Min1", "Maj1", "Min2", "Mod", "Maj2", "Sev"),
                             labels=c("Typ", "Min", "Maj", "Min", "Min", "Maj", "Maj"))

#29) MoSold & SeasonSold
#from month sold get season sold (maybe that has an impact?) and maybe a dummy if it's a "warm" season
summary(housing$MoSold)
housing$SeasonSold <- factor(housing$MoSold, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"),
                             labels=c("w", "w", "sp", "sp", "sp", "su", "su", "su", "a",  "a", "a", "w"))
housing$SeasonWarm <- factor(housing$SeasonSold, levels=c("w", "w", "sp", "sp", "sp", "su", "su", "su", "a",  "a", "a", "w"),
                             labels=c("0", "0", "0", "0", "1", "1", "1", "1", "1",  "0", "0", "0"))
summary(housing$SeasonSold)

#MoSold & SeasonWarm had no impact in analysis, so drop them:
housing <- select(housing, -SeasonWarm, -MoSold)

housing_full$MoSold  = as.numeric(levels(housing_full$MoSold))[housing_full$MoSold]

#30) Miscval
describe(housing$MiscVal)
# --> 1407/1459 values 0, so lets drop it for now
housing = housing %>% select(-MiscVal)

#31)Landslope
describe(housing$LandSlope)
#not a lot of variation, also not important in later analysis --> drop
housing <- select(housing, -LandSlope)

#What is left?
print(str(housing))



#32) do something with porch variables & wooddeck (almost the same as a porch)
#are there houses with multiple porches?
housing %>% filter(OpenPorchSF>0 & EnclosedPorch>0 & ScreenPorch>0)
housing %>% filter(EnclosedPorch>0 & ScreenPorch>0)
housing %>% filter(OpenPorchSF>0 & ScreenPorch>0)
housing %>% filter(ScreenPorch>0 & WoodDeckSF>0)
summary(housing$WoodDeckSF)
# ---> to some extent there are, although not that many
#Lets try to just sum up the area of wooddeck & porches and then use a categorical to indicate which type :)
housing = housing %>% mutate(porch_area = WoodDeckSF + OpenPorchSF + EnclosedPorch + `3SsnPorch`  + ScreenPorch)
housing = housing %>% mutate(porch_type = ifelse(WoodDeckSF>0, "wood_deck", NA))
housing = housing %>% mutate(porch_type = ifelse(OpenPorchSF>0, ifelse(is.na(porch_type), "open_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(EnclosedPorch>0, ifelse(is.na(porch_type), "enclosed_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(ScreenPorch>0, ifelse(is.na(porch_type), "screen_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(`3SsnPorch`>0, ifelse(is.na(porch_type), "three_s_porch", "multiple"), porch_type))
housing$porch_type = housing$porch_type %>% replace_na("no")
housing$porch_type = as.factor(housing$porch_type)

housing_full = housing_full %>% mutate(porch_area = WoodDeckSF + OpenPorchSF + EnclosedPorch + `3SsnPorch`  + ScreenPorch)
housing_full = housing_full %>% mutate(porch_type = ifelse(WoodDeckSF>0, "wood_deck", NA))
housing_full = housing_full %>% mutate(porch_type = ifelse(OpenPorchSF>0, ifelse(is.na(porch_type), "open_porch", "multiple"), porch_type))
housing_full = housing_full %>% mutate(porch_type = ifelse(EnclosedPorch>0, ifelse(is.na(porch_type), "enclosed_porch", "multiple"), porch_type))
housing_full = housing_full %>% mutate(porch_type = ifelse(ScreenPorch>0, ifelse(is.na(porch_type), "screen_porch", "multiple"), porch_type))
housing_full = housing_full %>% mutate(porch_type = ifelse(`3SsnPorch`>0, ifelse(is.na(porch_type), "three_s_porch", "multiple"), porch_type))
housing_full$porch_type = housing_full$porch_type %>% replace_na("no")
housing_full$porch_type = as.factor(housing_full$porch_type)
summary(housing_full$porch_type)
summary(housing_full$porch_area)

housing = select(housing, -WoodDeckSF, -OpenPorchSF, -EnclosedPorch, -`3SsnPorch`, -ScreenPorch)
housing_full = select(housing_full, -WoodDeckSF, -OpenPorchSF, -EnclosedPorch, -`3SsnPorch`, -ScreenPorch)

#33) do something with exteriors
# Exterior1st and Exterior2nd are factors with far too many categories
ggplot(housing, aes( ExterQual, ..count..)) + geom_bar(aes(fill = Exterior1st), position = "dodge") 
ggplot(housing, aes( ExterQual, ..count..)) + geom_bar(aes(fill = Exterior2nd), position = "dodge") 
#We can revert this later on, but just drop the type of exterior for now... we still evaluate how "good" the exterior is with ExterQual and ExterCond
housing = select(housing, -Exterior1st, -Exterior2nd)

#34) Compressing the factors here, actually performed worse, so not done:
# housing$OverallQual <- factor(housing$OverallQual, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
#                               labels=c("1", "1", "2", "2", "3", "3", "4", "4", "5",  "5"))
# housing$OverallCond <- factor(housing$OverallCond, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
#                               labels=c("1", "1", "2", "2", "3", "3", "4", "4", "5",  "5"))



#35) Since OverallQual & OverallCond are both "ratings", transform them to numerical (performed better in model!)
housing$OverallQual  = as.numeric(levels(housing$OverallQual))[housing$OverallQual]
housing$OverallCond  = as.numeric(levels(housing$OverallCond))[housing$OverallCond]
housing_full$OverallQual  = as.numeric(levels(housing_full$OverallQual))[housing_full$OverallQual]
housing_full$OverallCond  = as.numeric(levels(housing_full$OverallCond))[housing_full$OverallCond]

#36) Get a dummy if remodeling was done or not and drop remodeling year
#Because interpretation a lot easier, and YearRemodAdd can't be used alone anyway, since = to YearBuilt if no remodeling done
housing = housing %>% mutate(Remod = ifelse(YearBuilt == YearRemodAdd, 0, 1))
housing = select(housing, -YearRemodAdd)
housing_full = housing_full %>% mutate(Remod = ifelse(YearBuilt == YearRemodAdd, 0, 1))
housing_full = select(housing_full, -YearRemodAdd)

#-----------
#Still have quite a lot of variables, especially if we do one-hot encoding
#So maybe try to collapse at least some of the categories

#37) Collapse foundation
summary(housing$Foundation)
housing$Foundation <- factor(housing$Foundation, levels=c( "PConc", "CBlock", "BrkTil",   "Wood" ,  "Slab" , "Stone" ),
                             labels=c("PConc", "CBlock", "BrkTil",   "Other" ,  "Other" , "Other" ))
housing_full$Foundation <- factor(housing_full$Foundation, levels=c( "PConc", "CBlock", "BrkTil",   "Wood" ,  "Slab" , "Stone" ),
                             labels=c("PConc", "CBlock", "BrkTil",   "Other" ,  "Other" , "Other" ))

#38) Heating
summary(housing$Heating) #Absolutely no variation here, not seen earlier...
housing = select(housing, -Heating)

#39) GarageQual 
summary(housing$GarageQual) #Almost no variation here as well to be honest, probably better to drop to avoid overfitting
housing = select(housing, -GarageQual)
housing_full$GarageQual <- factor(housing_full$GarageQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex", "No" ),
                                  labels=c( "3",   "4",   "2",   "1",   "5", "0"  ) )
housing_full$GarageQual  = as.numeric(levels(housing_full$GarageQual))[housing_full$GarageQual]

#40) ExterCond: collapse that, because not enough variation & make numerical out of it
summary(housing$ExterCond)
housing$ExterCond <- factor(housing$ExterCond, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "2",   "3",   "1",   "1",   "3"  ) )
housing$ExterCond  = as.numeric(levels(housing$ExterCond))[housing$ExterCond]
summary(housing$ExterCond)
housing_full$ExterCond <- factor(housing_full$ExterCond, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "2",   "3",   "1",   "1",   "3"  ) )
housing_full$ExterCond  = as.numeric(levels(housing_full$ExterCond))[housing_full$ExterCond]

#41) ExterQual: also convert to numerical
housing$ExterQual <- factor(housing$ExterQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing$ExterQual  = as.numeric(levels(housing$ExterQual))[housing$ExterQual]

housing_full$ExterQual <- factor(housing_full$ExterQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing_full$ExterQual  = as.numeric(levels(housing_full$ExterQual))[housing_full$ExterQual]

#42) SaleCondition: collapse, because not enough variation
#For now not by too much
summary(housing$SaleCondition)
housing$SaleCondition <- factor(housing$SaleCondition, levels=c(  "Normal", "Abnorml", "Partial", "AdjLand",  "Alloca",  "Family"  ),
                                labels=c( "Normal", "Abnorml", "Partial", "Other",  "Other",  "Family"  ) )
housing_full$SaleCondition <- factor(housing_full$SaleCondition, levels=c(  "Normal", "Abnorml", "Partial", "AdjLand",  "Alloca",  "Family"  ),
                                labels=c( "Normal", "Abnorml", "Partial", "Other",  "Other",  "Family"  ) )

#43) LotShape: collapse that, because not enough variation
summary(housing$LotShape)
housing$LotShape <- factor(housing$LotShape, levels=c(  "Reg", "IR1", "IR2", "IR3"   ),
                           labels=c( "Reg", "IR1", "IR2.5", "IR2.5"   ) )
summary(housing$LotShape)
housing_full$LotShape <- factor(housing_full$LotShape, levels=c(  "Reg", "IR1", "IR2", "IR3"   ),
                           labels=c( "Reg", "IR1", "IR2.5", "IR2.5"   ) )
#44) PavedDrive: collapse that as well
summary(housing$PavedDrive)
housing$PavedDrive <- factor(housing$PavedDrive, levels=c(  "Y",  "N",  "P"  ),
                             labels=c( "Y", "N or P", "N or P"  ) )
housing$PavedDrive <- factor(housing$PavedDrive, levels=c("Y", "N or P"), labels=c("1","0"))

housing_full$PavedDrive <- factor(housing_full$PavedDrive, levels=c(  "Y",  "N",  "P"  ),
                             labels=c( "Y", "N or P", "N or P"  ) )
housing_full$PavedDrive <- factor(housing_full$PavedDrive, levels=c("Y", "N or P"), labels=c("1","0"))


#45)
#Changing some factors to dummies & numericals:
housing$CentralAir = as.numeric(housing$CentralAir)
housing = housing %>% mutate(CentralAir = ifelse(CentralAir == 2,1,0))
housing$PavedDrive = as.numeric(housing$PavedDrive)
housing = housing %>% mutate(PavedDrive = ifelse(PavedDrive == 2, 1,0 ))

housing_full$CentralAir = as.numeric(housing_full$CentralAir)
housing_full = housing_full %>% mutate(CentralAir = ifelse(CentralAir == 2,1,0))
housing_full$PavedDrive = as.numeric(housing_full$PavedDrive)
housing_full = housing_full %>% mutate(PavedDrive = ifelse(PavedDrive == 2, 1,0 ))

housing$KitchenQual <- factor(housing$KitchenQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing$KitchenQual  = as.numeric(levels(housing$KitchenQual))[housing$KitchenQual]
housing$HeatingQC <- factor(housing$HeatingQC, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                              labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing$HeatingQC  = as.numeric(levels(housing$HeatingQC))[housing$HeatingQC]

housing$BsmtQual <- factor(housing$BsmtQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex", "No" ),
                            labels=c( "3",   "4",   "2",   "1",   "5", "0"  ) )
housing$BsmtQual  = as.numeric(levels(housing$BsmtQual))[housing$BsmtQual]

housing$BsmtFinType1 <- factor(housing$BsmtFinType1, levels=c(  "GLQ",   "ALQ",   "BLQ",   "Rec",   "LwQ", "Unf", "No" ),
                           labels=c( "6","5",   "4",   "3",   "2",   "1", "0"  ) )
housing$BsmtFinType1  = as.numeric(levels(housing$BsmtFinType1))[housing$BsmtFinType1]

housing_full$KitchenQual <- factor(housing_full$KitchenQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                              labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing_full$KitchenQual  = as.numeric(levels(housing_full$KitchenQual))[housing_full$KitchenQual]
housing_full$HeatingQC <- factor(housing_full$HeatingQC, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "3",   "4",   "2",   "1",   "5"  ) )
housing_full$HeatingQC  = as.numeric(levels(housing_full$HeatingQC))[housing_full$HeatingQC]

housing_full$BsmtQual <- factor(housing_full$BsmtQual, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex", "No" ),
                           labels=c( "3",   "4",   "2",   "1",   "5", "0"  ) )
housing_full$BsmtQual  = as.numeric(levels(housing_full$BsmtQual))[housing_full$BsmtQual]

housing_full$BsmtFinType1 <- factor(housing_full$BsmtFinType1, levels=c(  "GLQ",   "ALQ",   "BLQ",   "Rec",   "LwQ", "Unf", "No" ),
                               labels=c( "6","5",   "4",   "3",   "2",   "1", "0"  ) )
housing_full$BsmtFinType1  = as.numeric(levels(housing_full$BsmtFinType1))[housing_full$BsmtFinType1]

print(str(housing))