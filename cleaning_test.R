housing = read_csv(path_test, col_types = cols(.default = col_factor(), Id = col_character(), LotFrontage = col_double(), LotArea = col_double(),
                                              YearBuilt = col_integer(), YearRemodAdd = col_integer(), MasVnrArea = col_double(),
                                              BsmtFinSF1 = col_double(), BsmtFinSF2= col_double(), BsmtUnfSF =  col_double(),
                                              TotalBsmtSF = col_double(), "1stFlrSF"= col_double(), "2ndFlrSF"= col_double(),
                                              LowQualFinSF =  col_double(), GrLivArea = col_double(), BsmtFullBath = col_integer(),
                                              BsmtHalfBath = col_integer(), FullBath = col_integer(), HalfBath = col_integer(),
                                              BedroomAbvGr = col_integer(), KitchenAbvGr = col_integer(), TotRmsAbvGrd =  col_integer(),
                                              Fireplaces = col_integer(), GarageYrBlt = col_integer(), GarageCars = col_integer(),
                                              GarageArea = col_double(), WoodDeckSF = col_double(), OpenPorchSF= col_double(),
                                              EnclosedPorch= col_double(), "3SsnPorch"= col_double(), ScreenPorch= col_double(),
                                              PoolArea= col_double(), YrSold  = col_integer()))

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
#it's random stuff like elevator, shed, tennis court, etc.
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
#GarageFinish is the condition of the interior of the garage
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
#i would rather keep them, e.g. family with kids would want several separate rooms instead of two big ones
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

#corrplot::corrplot(DescTools::PairApply(df, DescTools::CramerV))

#8)
#__________________________________________________________________________
#my party started here
housing$KitchenQual <- as_factor(housing$KitchenQual)

#take a look at correlation between numerical features and target variable
numerics = select_if(housing, is.numeric)

correlations <- c()
for (name in names(numerics)) {
  correlations[name] <- cor(numerics[name], housing$SalePrice)[1]
}
correlations

summary(housing$PoolArea)
housing = housing %>%  mutate(pool=ifelse(PoolArea>0, 1, 0))
housing$pool = as.factor(housing$pool)
#in my humble opinion, we can just drop the columns that have correlation less than 10% with the target variable
housing = select(housing, -c("LowQualFinSF", "PoolArea", "YrSold"))

# now let's see what's up with other features
library(purrr)

#can safely throw out id, like who cares
housing <- select(housing, -Id)

#i would also argue that street is useless as well, only 6 values that differ
housing <- select(housing, -Street)

#same with Utilities, only 1 value differs
housing <- select(housing, -Utilities)

#if there are <= 5 different values with considerable(>=10) amount of observations in each,
#then transform them to categorical
categorical <- c("MSZoning", "LotShape", "LandSlope","BldgType", "ExterQual", 
                 "BsmtQual",  "CentralAir", "KitchenQual", "FireplaceQu", "PavedDrive")

for (name in categorical) {
  housing[name] <- as_factor(housing[name])
}

#too much shit in neighborhood, exterior1st, exterior2nd don't even know what to do with it
non_numerics <- select_if(housing, negate(is.numeric))

#roofstyle has several types with few observations -> collapse
housing$RoofStyle <- factor(housing$RoofStyle, levels=c("Gable", "Hip", "Gambrel", "Mansard", "Flat", "Shed"), 
                            labels=c("Gable", "Hip", "Other", "Other", "Other", "Other"))
#can do the same for SaleType
#might also collapse COD and Other, not sure
housing$SaleType <- factor(housing$SaleType, levels=c("WD", "New", "COD", "ConLD", "ConLI", "CWD", "ConLw", "Con", "Oth"), 
                           labels=c("WD", "New", "COD", "Other", "Other", "Other", "Other", "Other", "Other"))
#Electrical 
housing$Electrical <- factor(housing$Electrical, levels=c("SBrkr", "FuseF", "FuseA", "FuseP", "Mix"),
                             labels=c("SBrkr", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix", "Fuse or Mix"))
#FireplaceQu: leave categories TA - average, AA - above average, BA - below average, No - no fireplace
housing$FireplaceQu <- factor(housing$FireplaceQu, levels=c("TA", "Gd", "Fa", "Ex", "Po", "No"),
                              labels=c("TA", "AA", "BA", "AA", "BA", "No"))
#Functional: can collapse by degree of deduction (moderate went to minimal, severe went to major
housing$Functional <- factor(housing$Functional, levels=c("Typ", "Min1", "Maj1", "Min2", "Mod", "Maj2", "Sev"),
                             labels=c("Typ", "Min", "Maj", "Min", "Min", "Maj", "Maj"))

#

for (name in names(non_numerics)) {
  print(summary(housing[name]))
}

#not sure how to collapse GarageQual and SaleCondition

#from month sold get season sold (maybe that has an impact?)
summary(housing$MoSold)
housing$SeasonSold <- factor(housing$MoSold, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"),
                             labels=c("w", "w", "sp", "sp", "sp", "su", "su", "su", "a",  "a", "a", "w"))
summary(housing$SeasonSold)
#testing categorical variables for importance with ANOVA
non_numerics <- select_if(housing, negate(is.numeric))

probabilities <- c()
for (name in names(non_numerics)) {
  formula <- paste("SalePrice",name, sep = "~")
  model <- aov(as.formula(formula), data = housing)
  summary(model)
  sum_model = unlist(summary(model))
  probabilities[name] = sum_model["Pr(>F)1"]
}

print(probabilities<0.01)
#for 99% confidence LandSlope, MiscVal and MoSold are not important -> drop
housing <- select(housing, -c("LandSlope", "MiscVal", "MoSold"))

print(str(housing))



###my part again
#1) do something with porch variables & wooddeck (almost the same as a porch)
#are there houses with multiple porches?
housing %>% filter(OpenPorchSF>0 & EnclosedPorch>0 & ScreenPorch>0)
housing %>% filter(EnclosedPorch>0 & ScreenPorch>0)
housing %>% filter(OpenPorchSF>0 & ScreenPorch>0)
housing %>% filter(ScreenPorch>0 & WoodDeckSF>0)
summary(housing$WoodDeckSF)
# ---> to some extent there are, although not that many
#Lets try to just some up the area of wooddeck & porches and then use a categorical to indicate which type :)
housing = housing %>% mutate(porch_area = WoodDeckSF + OpenPorchSF + EnclosedPorch + `3SsnPorch`  + ScreenPorch)
housing = housing %>% mutate(porch_type = ifelse(WoodDeckSF>0, "wood_deck", NA))
housing = housing %>% mutate(porch_type = ifelse(OpenPorchSF>0, ifelse(is.na(porch_type), "open_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(EnclosedPorch>0, ifelse(is.na(porch_type), "enclosed_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(ScreenPorch>0, ifelse(is.na(porch_type), "screen_porch", "multiple"), porch_type))
housing = housing %>% mutate(porch_type = ifelse(`3SsnPorch`>0, ifelse(is.na(porch_type), "three_s_porch", "multiple"), porch_type))
housing$porch_type = housing$porch_type %>% replace_na("no")
housing$porch_type = as.factor(housing$porch_type)
summary(housing$porch_type)
summary(housing$porch_area)

housing = select(housing, -WoodDeckSF, -OpenPorchSF, -EnclosedPorch, -`3SsnPorch`, -ScreenPorch)
#2) do something with exteriors
# Exterior1st and Exterior2nd are factors with far too many categories
ggplot(housing, aes( ExterQual, ..count..)) + geom_bar(aes(fill = Exterior1st), position = "dodge") 
ggplot(housing, aes( ExterQual, ..count..)) + geom_bar(aes(fill = Exterior2nd), position = "dodge") 
#We can revert this later on, but just drop the type of exterior for now... we still evaluate how "good" the exterior is with ExterQual and ExterCond and it doesnt help us much for IML
housing = select(housing, -Exterior1st, -Exterior2nd)
#3) Since we still have too many variables, let's try to compress at least some of them, e.g. OverallQual & OverallCond
housing$OverallQual <- factor(housing$OverallQual, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                              labels=c("1", "1", "2", "2", "3", "3", "4", "4", "5",  "5"))
housing$OverallCond <- factor(housing$OverallCond, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                              labels=c("1", "1", "2", "2", "3", "3", "4", "4", "5",  "5"))


#4) Get a factor if remodeling was done or not and drop remodeling year
housing = housing %>% mutate(Remod = ifelse(YearBuilt == YearRemodAdd, 1, 0))
housing$Remod = as.factor(housing$Remod)
housing = select(housing, -YearRemodAdd)

#5) Collapse foundation
summary(housing$Foundation)
housing$Foundation <- factor(housing$Foundation, levels=c( "PConc", "CBlock", "BrkTil",   "Wood" ,  "Slab" , "Stone" ),
                             labels=c("PConc", "CBlock", "BrkTil",   "Other" ,  "Other" , "Other" ))


#6) Heating
summary(housing$Heating) #Absolutely no variation here
housing = select(housing, -Heating)

#7) GarageQual 
summary(housing$GarageQual) #almost no variation in here as well
housing = select(housing, -GarageQual)

#8) ExterCond: collapse that, because not enough variation
summary(housing$ExterCond)
housing$ExterCond <- factor(housing$ExterCond, levels=c(  "TA",   "Gd",   "Fa",   "Po",   "Ex"  ),
                            labels=c( "2",   "3",   "1",   "1",   "3"  ) )
summary(housing$ExterCond)

#9) SaleCondition: collapse that, because not enough variation
#For now not by too much
summary(housing$SaleCondition)
housing$SaleCondition <- factor(housing$SaleCondition, levels=c(  "Normal", "Abnorml", "Partial", "AdjLand",  "Alloca",  "Family"  ),
                                labels=c( "Normal", "Abnorml", "Partial", "Other",  "Other",  "Family"  ) )


#10) LotShape: collapse that, because not enough variation
summary(housing$LotShape)
housing$LotShape <- factor(housing$LotShape, levels=c(  "Reg", "IR1", "IR2", "IR3"   ),
                           labels=c( "Reg", "IR1", "IR2.5", "IR2.5"   ) )
summary(housing$LotShape)

#11) PavedDrive: collapse that as well
summary(housing$PavedDrive)
housing$PavedDrive <- factor(housing$PavedDrive, levels=c(  "Y",    "N",    "P"   ),
                             labels=c( "Y",    "N or P",    "N or P"    ) )
summary(housing$PavedDrive)




library(Hmisc)

for(i in colnames(housing)){
  print(i)
  print(describe(housing[[i]]))
}

#OverallQual variation questionable
#Same for OverallCond
#Same for HeatingQC
describe(housing$OverallQual)
describe(housing$OverallCond)
describe(housing$HeatingQC)

# housing$OverallCond  <- factor(housing$OverallCond , levels=c("1" ,    "2" ,    "3 "  , "4"  ,   "5"),
#                               labels=c("1" ,    "1" ,    "2 "  , "3"  ,   "3"))
# housing$OverallQual  <- factor(housing$OverallQual , levels=c("1" ,    "2" ,    "3 "  , "4"  ,   "5"),
#                                labels=c("1" ,    "1" ,    "2 "  , "3"  ,   "4"))
# 
# housing$OverallQual  <- factor(housing$OverallQual , levels=c( "Ex",    "Gd" ,   "TA",    "Fa",    "Po"),
#                                labels=c("Ex",    "Gd" ,   "TA",    "Fa & Po",    "Fa & Po"))

#For initial analysis can probably get rid of some garage variables, basement variables, exterior stuff
print(str(housing))