library("tidyverse")
library("lubridate")
library("dlnm"); library("gnm")

# 1 Create data frame -------------------------------------------------------

# Mortality data
path1 = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Wellcome_Trust/Data Methods Core/Data/Mortality Data/temp/P20/"

df_mort <- vroom::vroom(paste0(path1,"Processed_data/L2_mort_20240320.csv")) |> 
  mutate(# Put together "Alcalde Diaz","Ernesto Cordoba Campos" and "Las Cumbres"
         # "Alcalde Diaz","Ernesto Cordoba Campos"  originated from "Las Cumbres:
         # assign name and local id of Las Cumbres and aggregate in next step
         L2NAMEOG = L2NAME2,
         L2NAME2 = ifelse(L2NAMEOG %in% c("Alcalde Diaz","Ernesto Cordoba Campos"),"Las Cumbres", L2NAME2),
         SALID2 = as.factor(ifelse(L2NAMEOG %in% c("Alcalde Diaz","Ernesto Cordoba Campos"),"20610140", SALID2)),
         SALID1 = as.factor(SALID1),
         #SALID = SALID2,
         #TYPE  = "L2"
  ) |>  
  summarize(DEATHS = sum(as.numeric(DEATHS)), 
            .by = c(ISO2, SALID1, SALID2, L1NAME, L2NAME2, YEAR, MONTH, DAY))

# Temperature data
df_temp <- vroom::vroom(paste0(path1,"Processed_data/TMPDAILY_L1AD_20240710.csv")) |> 
  mutate(salid1 = as.factor(SALID1),
         year = year(date), 
         month = month(date),
         day = day(date),
         tmp_pw = L1ADtemp_pw, .keep = "none") |> 
  rename_with(toupper)

# Neighborhood level modifier data
df_nbhd <- vroom::vroom("data/dat_all_2023-08-01.csv") |> 
  filter(SALID1 %in% unique(df_mort$SALID1)) |> 
  mutate(SALID1 = as.factor(SALID1),
         SALID2 = as.factor(SALID2),
         SALID2_5 = as.factor(SALID2_5))

# Age data
age_categories <- str_subset(names(df_nbhd), "CNSAGE")
under_65 <- age_categories[1:13]
over_65  <- age_categories[14:21]

# Specify percentage of population over 65 for each L2 neiborhood
df_age <- df_nbhd |> 
  mutate(n_under_65 = rowSums(pick(all_of(under_65)), na.rm = TRUE),
         n_over_65  = rowSums(pick(all_of(over_65)),  na.rm = TRUE),
         n_total = n_under_65 + n_over_65) |> 
  #filter(n_total != 0) |> 
  summarize(n_over_65 = sum(n_over_65),
            n_total = sum(n_total), .by = SALID2_5) |> 
  mutate(percent_over_65 = n_over_65/n_total, .keep = "unused")

# Green space data:             BECGSPCTL3
# Educational attainment data: CNSMINPR_L3
# Note that cities 206101 (Panama City) and 203102 (Guatemala City)
# don't have neighborhood level modifier data.
df_nbhd_L2_5 <- df_nbhd |> 
  select(SALID1, SALID2_5, BECGSPCTL3, CNSMINPR_L3)


# Aggregate final data frame
df <- df_mort |> 
  left_join(df_temp, by = c("SALID1", "YEAR", "MONTH", "DAY")) |> 
  left_join(df_age, by = join_by(SALID2 == SALID2_5)) |> 
  left_join(df_nbhd_L2_5, by = join_by(SALID2 == SALID2_5, SALID1 == SALID1)) |> 
  filter(!if_any(c(percent_over_65, BECGSPCTL3, CNSMINPR_L3), is.na))

# write_csv(df, "data/mort_temp_with_nbhd_modifiers.csv")


# 2 Estimating the exposure response curves ---------------------------------

# Read created data
df <- vroom::vroom("data/mort_temp_with_nbhd_modifiers.csv") |> 
  mutate(across(c(ISO2, SALID1, SALID2), as.factor),
         date = ymd(paste(YEAR, MONTH, DAY, sep = "-")),
         strata = paste(SALID1, YEAR, MONTH, wday(date, label = TRUE), sep = ":") |> as.factor())

## 2.1 City and age ------------------------------------------------------------

# Basic model
var_knots <- quantile(df$TMP_PW, c(.1, .75, .9), na.rm = TRUE)
lag_knots <- logknots(21, nk = 3)

# Cross-basis
cbt <- crossbasis(df$TMP_PW, lag = 21,
                  argvar = list(fun = "ns", knots = var_knots),
                  arglag = list(fun = "ns", knots = lag_knots),
                  group = df$SALID1)

# Fitted model
model1 <- gnm(DEATHS ~ cbt,
              family = quasipoisson(),
              eliminate = strata,
              data = df)

pred1 <- crosspred(cbt, model1)

plot(pred1, "overall")

# By city and age

# By city and greenspace

# By city and education




























# I think this implementation is inefficient [FIX LATER]
dat5 = left_join(dat1, dat4, by = c("SALID1", "YEAR", "MONTH", "DAY")) %>% 
  filter(
    !is.na(TMP_PW)
  ) %>% 
  mutate(
    # drop years were georeferencing is bad
    DATE = as.Date(paste(YEAR, MONTH, DAY, sep="-"), "%Y-%m-%d"),
    drop = ifelse(L1NAME =="Belo Horizonte" & YEAR <2007,1,0 ),
    drop = ifelse(L1NAME =="Sao Paulo" & YEAR <2005,1,drop )
    
  ) %>% 
  filter(
    drop != 1
  ) %>% 
  select(-drop) 

dat5$TMP_PW = as.numeric(dat5$TMP_PW)

#table(dat5$L1NAME)
dat5$L1NAME <- factor(dat5$L1NAME, levels = c("Guatemala City", "Panama City", "Belo Horizonte","Campinas","Rio de Janeiro","Sao Paulo"))

dat5 <- dat5[order(dat5$L1NAME, dat5$SALID), ]


#table(dat5$L1NAME, dat5$YEAR, useNA = "always")
rm(dat1);rm(dat2);rm(dat4);gc()

#c = 6
#for (c in 1:6){
#print(city[c])
#dat6 = filter(dat5, L1NAME == city[c])
dat6 = dat5 %>% 
  ungroup() %>% 
  group_by(L1NAME) %>% 
  mutate(
    units = length(unique(SALID))
  )
#dat6 = filter(dat5, L1NAME %in% c("Guatemala City", "Panama City"))
neighborhood = unique(dat6$SALID)






# City Specific Analysis --------------------------------------------------

library(dlnm)
library(gnm)
library(mvmeta)
library(splines)
library(data.table)
library(tidyverse)

path1 = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Wellcome_Trust/Data Methods Core/Data/Mortality Data/temp/P20/"

#---------------------------------------------------
# Read L1AD mortality data
#---------------------------------------------------
dat1 = fread(paste0(path1,"Processed_data/L1AD_mort_20240320.csv")) %>% 
  mutate(
    SALID = SALID1,
    TYPE  = "L1AD"
  ) %>% 
  select(-time)

#---------------------------------------------------
# Read L1AD population data
#---------------------------------------------------
#directory where is the data
dir<- paste0(path1,"Processed_data/")
#list of files
files<- list.files(dir)
files<-files[grepl("PRJ", files)]
files
dat2 <- data.frame()

#reads all the data that match the pattern 
for (i in 1:length(files)){
  #read in files 
  temp_data <- fread(paste0(dir, files[i])) %>% 
    group_by(SALID1, YEAR) %>% 
    summarise(
      POP = sum(as.numeric(PRJL1ADPOP))
    )
  #for each iteration, bind the new data to the building dataset
  dat2 <- rbindlist(list(dat2, temp_data), use.names = T) 
  rm(temp_data)
}

dat3 = left_join(dat1, dat2, by = c("SALID1", "YEAR"))

#---------------------------------------------------
# Read L1AD temperature data
#---------------------------------------------------
dat4 = fread(paste0(path1,"Processed_data/TMPDAILY_L1AD_20240710.csv")) %>% 
  mutate(salid1 = SALID1,
         year = year(date), 
         month = month(date),
         day = day(date),
         tmp_pw = L1ADtemp_pw, .keep = "none")
names(dat4) = toupper(names(dat4))

dat5 = left_join(dat3, dat4, by = c("SALID1", "YEAR", "MONTH", "DAY")) %>% 
  filter(
    !is.na(TMP_PW)
  ) %>% 
  mutate(
    DATE = as.Date(paste(YEAR, MONTH, DAY, sep="-"), "%Y-%m-%d"),
    drop = ifelse(L1NAME =="Belo Horizonte" & YEAR <2007,1,0 ),
    drop = ifelse(L1NAME =="Sao Paulo" & YEAR <2005,1,drop )
  ) %>% 
  filter(
    drop != 1
  ) %>% 
  select(-drop)

# Read base data set
df_all <- read_csv("data/dat_all_2023-08-01.csv")
#df2 <- read_csv("data/SALURBAL_cities_Brazil.csv")

# Extract all potential age categories (separate to under/over 65)
age_categories <- str_subset(names(df_age), "CNSAGE")
under_65 <- age_categories[1:13]
over_65  <- age_categories[14:21]

# Specify percentage of population over 65 for each L2 neiborhood
df_age <- df_all |> 
  mutate(n_under_65 = rowSums(pick(all_of(under_65)), na.rm = TRUE),
         n_over_65  = rowSums(pick(all_of(over_65)),  na.rm = TRUE),
         n_total = n_under_65 + n_over_65) |> 
  filter(n_total != 0) |> 
  summarize(n_over_65 = sum(n_over_65),
            n_total = sum(n_total), .by = SALID2) |> 
  mutate(percent_over_65 = n_over_65/n_total)

# Add age category




################################################################################

#---------------------------------------------------
# STEP 1
#---------------------------------------------------

# save coefficients
coef1 <- matrix(NA,length(unique(dat5$SALID)), 4, dimnames=list(unique(dat5$SALID)))
vcov1 <- vector("list", length(unique(dat5$SALID))) 
names(vcov1) <- unique(dat5$L1NAME)
red.1 <- vector("list", length(unique(dat5$SALID)))
fitmat<- matrix(NA,length(unique(dat5$SALID)), 1)

for ( i in c(1:length(unique(dat5$SALID)))){
  
  print(unique(dat5$SALID)[i])
  temp1 = filter(dat5, SALID == unique(dat5$SALID)[i])
  
  these.knots = as.vector(quantile(temp1$TMP_PW, probs = c(0.10, 0.75, 0.90)))
  this.bound = c(min(temp1$TMP_PW), max(temp1$TMP_PW))
  
  temp1$MONTH   <- as.factor(temp1$MONTH)
  temp1$YEAR    <- as.factor(temp1$YEAR)
  temp1$DOW     <- as.factor(weekdays(temp1$DATE))
  temp1$STRATUM <- as.factor(temp1$YEAR:temp1$MONTH:temp1$DOW)
  
  argvar <- list(fun="ns" ,knots=these.knots, Boundary.knots=this.bound)
  
  cb <- crossbasis(temp1$TMP_PW, lag=21, argvar=argvar,
                   arglag=list(knots=logknots(21,5))
                   #group=temp1$group.series
  )
  
  model <- gnm(DEATHS~cb , data=temp1, offset=log(POP), family=quasipoisson, eliminate=factor(STRATUM))
  
  cen <- mean(temp1$TMP_PW,na.rm=T)
  mod.fit=model$deviance
  # REDUCTION TO OVERALL CUMULATIVE
  red <- crossreduce(cb,model,cen=cen)
  
  model.all=model
  m1 = list(red=red,fit=mod.fit); 
  
  coef1[i, 1:length(coef(m1$red))] <- coef(m1$red) 
  vcov1[[i]] <- vcov(m1$red)
  red.1[[i]] <- m1$red
  fitmat[i,1] <- m1$fit  
  
}

RESULTS.ALLAGES = list(COEF=coef1, VCOV=vcov1, RED=red.1)

#---------------------------------------------------
# STEP 2
#---------------------------------------------------

#ALL AGES
COEF=RESULTS.ALLAGES$COEF
VCOV=RESULTS.ALLAGES$VCOV
RED = RESULTS.ALLAGES$RED

cities <- data.frame(SALID=unique(dat5$SALID), L1NAME=unique(dat5$L1NAME))
# cities = group_by(dat5, SALID1, L1NAME) %>% 
#   summarise(
#     p50 = median(TMP_PW),
#     rangehigh = max(TMP_PW),
#     rangelow = min(TMP_PW),
#     
#   )

mv1  <- mvmeta(COEF  ,
               #as.factor(metadata$KP_level1_Interpretation ), 
               VCOV, data=cities, control=list(showiter=F))

# OBTAIN BLUPS
blup1 <- blup(mv1,vcov=T) 


#---------------------------------------------------
# PLOTS
#---------------------------------------------------

xlab <- expression(paste("Temperature (",degree,"C)"))

#pdf("C:/Users/jr3553/Documents/GitHub/P20-Research-Center/eda/LA05.Results/City_models4.pdf",width=8,height=9)
layout(matrix(c(0,1,1,2,2,0,rep(3:8,each=2),0,9,9,10,10,0),ncol=6,byrow=T))
par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)

PredictedMV1 = list()
PredictedRaw = list()

i = 1
for(i in c(1:length(unique(dat5$SALID)))) {
  
  data = filter(dat5, SALID == unique(dat5$SALID)[i])
  
  these.knots = as.vector(quantile(data$TMP_PW, probs = c(0.10, 0.75, 0.90)))
  this.bound = c(min(data$TMP_PW), max(data$TMP_PW))
  
  argvar <- list(x=data$TMP_PW,fun="ns",#degree=3,
                 knots=these.knots, Boundary.knots=this.bound )
  bvar <- do.call(onebasis,argvar)
  
  
  #add prediction using meta-analysis prediction using undjusted overall MV-meta, MV1
  pred <- crosspred(bvar,coef=blup1[[i]]$blup,vcov=blup1[[i]]$vcov,
                    model.link="log",by=0.1,cen=mean(data$TMP_PW,na.rm=T))
  
  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
       main= paste0(unique(data$L1NAME)," ",min(data$YEAR),"-",max(data$YEAR))
  )
  
  lines(pred$predvar, pred$allRRfit, col="red", lwd=1.5)  # First prediction in blue
  #lines(pred$predvar,pred$allRRlow,col= "red3",lwd=1 , lty="dashed")
  #lines(pred$predvar,pred$allRRhigh,col= "red3",lwd=1, lty="dashed")
  
  
  
  #plot prediction using city-specific knots (no smoothing)
  pred <- crosspred(bvar,coef=RED[[i]]$coefficients,vcov=RED[[i]]$vcov,
                    model.link="log",by=0.1,cen=mean(data$TMP_PW,na.rm=T))
  PredictedRaw[[i]]=pred
  
  # city specific lines
  # lines(pred$predvar,pred$allRRfit,col= "blue",lwd=1.5)
  # lines(pred$predvar,pred$allRRlow,col= "blue",lwd=1 , lty="dashed")
  # lines(pred$predvar,pred$allRRhigh,col= "blue",lwd=1, lty="dashed")
  # 
  PredictedMV1[[i]]=pred
  
  # ind1 <- pred$predvar<=min(data$TMP_PW)
  # ind2 <- pred$predvar>=min(data$TMP_PW)
  # lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  # lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
  mtext(paste0("deaths :",sum(data$DEATHS)),cex=0.7,line=0)
  
  #axis(1,at=-8:8*5)
  axis(2,at=1:5*0.5)
  breaks <- c(min(data$TMP_PW,na.rm=T)-1,seq(pred$predvar[1],
                                             pred$predvar[length(pred$predvar)],length=30),max(data$TMP_PW,na.rm=T)+1)
  hist <- hist(data$TMP_PW,breaks=breaks,plot=F)
  hist$density <- hist$density/max(hist$density)*0.7
  prop <- max(hist$density)/max(hist$counts)
  counts <- pretty(hist$count,3)
  plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
       breaks=breaks,freq=F,add=T)
  
  
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
  #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
  abline(v=c(mean(data$TMP_PW,na.rm=T)),lty=3.5)
  
}

#dev.off()



# L2 Level Analysis -------------------------------------------------------

library(dlnm)
library(gnm)
library(mvmeta)
library(splines)
library(data.table)
library(tidyverse)

path1 = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Wellcome_Trust/Data Methods Core/Data/Mortality Data/temp/P20/"

#---------------------------------------------------
# Read L1AD mortality data
#---------------------------------------------------
dat1 = fread(paste0(path1,"Processed_data/L2_mort_20240320.csv"), colClasses = "character")  %>% 
  select(-time) |> 
  mutate(across(c(YEAR, MONTH, DAY), as.numeric))

unique(dat1$SALID2[dat1$L2NAME2 == "Alcalde Diaz"])
unique(dat1$SALID2[dat1$L2NAME2 == "Ernesto Cordoba Campos"])
unique(dat1$SALID2[dat1$L2NAME2 == "Las Cumbres"])

unique(dat1$L2NAME2[dat1$L2NAMEOG == "Alcalde Diaz"])
unique(dat1$L2NAME2[dat1$L2NAMEOG == "Ernesto Cordoba Campos"])
unique(dat1$L2NAME2[dat1$L2NAMEOG == "Las Cumbres"])

#dim(dat1)[1] 8043982      
#names(dat1)
dat1 = mutate(dat1, 
              
              # put together "Alcalde Diaz","Ernesto Cordoba Campos" and "Las Cumbres"
              # "Alcalde Diaz","Ernesto Cordoba Campos"  originated from "Las Cumbres:
              # assign name and local id of Las Cumbres and aggregate in next step
              L2NAMEOG = L2NAME2,
              L2NAME2 = ifelse(L2NAMEOG %in% c("Alcalde Diaz","Ernesto Cordoba Campos"),"Las Cumbres", L2NAME2),
              SALID2 = ifelse(L2NAMEOG %in% c("Alcalde Diaz","Ernesto Cordoba Campos"),"20610140", SALID2),
              
              SALID = SALID2,
              TYPE  = "L2"
) %>% 
  group_by(ISO2, SALID1, SALID2, L1NAME, L2NAME2, YEAR, MONTH, DAY, SALID, TYPE) %>% 
  summarise(
    DEATHS = sum(as.numeric(DEATHS))
  )


#---------------------------------------------------
# Read L1AD population data
#---------------------------------------------------
# dat2 = fread(paste0(path1,"Processed_data/PRJBR_L1AD_20220624.csv")) %>% 
#   group_by(SALID1, YEAR) %>% 
#   summarise(
#     POP = sum(as.numeric(PRJL1ADPOP))
#   )
#   
# dat3 = left_join(dat1, dat2, by = c("SALID1", "YEAR"))

#---------------------------------------------------
# Read L1AD temperature data
#---------------------------------------------------
dat4 = fread(paste0(path1,"Processed_data/TMPDAILY_L1AD_20240710.csv"), colClasses = "character") %>%
  mutate(salid1 = SALID1,
         year = year(date), 
         month = month(date),
         day = day(date),
         tmp_pw = L1ADtemp_pw, .keep = "none")
names(dat4) = toupper(names(dat4))

# I think this implementation is inefficient [FIX LATER]
dat5 = left_join(dat1, dat4, by = c("SALID1", "YEAR", "MONTH", "DAY")) %>% 
  filter(
    !is.na(TMP_PW)
  ) %>% 
  mutate(
    # drop years were georeferencing is bad
    DATE = as.Date(paste(YEAR, MONTH, DAY, sep="-"), "%Y-%m-%d"),
    drop = ifelse(L1NAME =="Belo Horizonte" & YEAR <2007,1,0 ),
    drop = ifelse(L1NAME =="Sao Paulo" & YEAR <2005,1,drop )
    
  ) %>% 
  filter(
    drop != 1
  ) %>% 
  select(-drop) 

dat5$TMP_PW = as.numeric(dat5$TMP_PW)

#table(dat5$L1NAME)
dat5$L1NAME <- factor(dat5$L1NAME, levels = c("Guatemala City", "Panama City", "Belo Horizonte","Campinas","Rio de Janeiro","Sao Paulo"))

dat5 <- dat5[order(dat5$L1NAME, dat5$SALID), ]


#table(dat5$L1NAME, dat5$YEAR, useNA = "always")
rm(dat1);rm(dat2);rm(dat4);gc()

#c = 6
#for (c in 1:6){
#print(city[c])
#dat6 = filter(dat5, L1NAME == city[c])
dat6 = dat5 %>% 
  ungroup() %>% 
  group_by(L1NAME) %>% 
  mutate(
    units = length(unique(SALID))
  )
#dat6 = filter(dat5, L1NAME %in% c("Guatemala City", "Panama City"))
neighborhood = unique(dat6$SALID)
#---------------------------------------------------
# STEP 1: Run L2/L2.5 specific model
#---------------------------------------------------
#print(unique(dat5$L1NAME)[c])
# save coefficients
coef1 <- matrix(NA,length(unique(dat6$SALID)), 4, dimnames=list(unique(dat6$SALID)))
vcov1 <- vector("list", length(unique(dat6$SALID))) 
names(vcov1) <- c(rep("Guatemala City",14),rep("Panama City",51),rep("Belo Horizonte",174),rep("Campinas",115),
                  rep("Rio de Janeiro",332),rep("Sao Paolo",621))

red.1 <- vector("list", length(unique(dat6$SALID)))
fitmat<- matrix(NA,length(unique(dat6$SALID)), 1)
#coef1


for ( i in c(1:length(unique(dat6$SALID)))){
  #print( neighborhood[i])
  # print(neighborhood[i])
  temp1 = filter(dat6, SALID == neighborhood[i])
  #print(unique(temp1$L1NAME))
  these.knots = as.vector(quantile(temp1$TMP_PW, probs = c(0.10, 0.75, 0.90)))
  this.bound = c(min(temp1$TMP_PW), max(temp1$TMP_PW))
  
  temp1$MONTH   <- as.factor(temp1$MONTH)
  temp1$YEAR    <- as.factor(temp1$YEAR)
  temp1$DOW     <- as.factor(weekdays(temp1$DATE))
  temp1$STRATUM <- as.factor(temp1$YEAR:temp1$MONTH:temp1$DOW)
  
  argvar <- list(fun="ns" ,knots=these.knots, Boundary.knots=this.bound)
  
  cb <- crossbasis(temp1$TMP_PW, lag=21, argvar=argvar,
                   arglag=list(knots=logknots(21,5))
                   #,group=temp1$group.series
  )
  
  #model <- gnm(DEATHS~cb , data=temp1, offset=log(POP), family=quasipoisson, eliminate=factor(STRATUM))
  model <- gnm(DEATHS~cb , data=temp1, family=quasipoisson, eliminate=factor(STRATUM))
  
  #cen <- mean(temp1$TMP_PW,na.rm=T)
  cen <- quantile(temp1$TMP_PW, .75)
  mod.fit=model$deviance
  # REDUCTION TO OVERALL CUMULATIVE
  red <- crossreduce(cb,model,cen=cen)
  
  model.all=model
  m1 = list(red=red,fit=mod.fit); 
  
  coef1[i, 1:length(coef(m1$red))] <- coef(m1$red) 
  vcov1[[i]] <- vcov(m1$red)
  red.1[[i]] <- m1$red
  fitmat[i,1] <- m1$fit  
  
}

RESULTS.ALLAGES = list(COEF=coef1, VCOV=vcov1, RED=red.1)

#---------------------------------------------------
# STEP 2: Run metanalysis pooling all subcities
#---------------------------------------------------

#ALL AGES
COEF=RESULTS.ALLAGES$COEF
VCOV=RESULTS.ALLAGES$VCOV
RED = RESULTS.ALLAGES$RED

cities <- ungroup(dat6) %>% select(SALID, L1NAME) %>% unique()
COEF2 = as.data.frame(COEF) %>% 
  mutate(SALID = rownames(COEF)) %>% 
  left_join(cities, by ="SALID")

# meta-regression
mv1  <- mvmeta( as.matrix(COEF2[, c("V1", "V2", "V3", "V4")]) ~ as.factor(COEF2$L1NAME) ,
                #as.factor(metadata$KP_level1_Interpretation ), 
                VCOV, data=COEF2, control=list(showiter=F))
#summary(mv1)

# OBTAIN BLUPS for smoothed curves
blup1 <- blup(mv1,vcov=TRUE) 
#blup1


#---------------------------------------------------
# PLOTS
#---------------------------------------------------
p = 1
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9),ncol=6,byrow=T))

for(p in c(1:length(neighborhood))) {
  #for(p in c(1:length(neighborhood))) {
  
  
  
  #print(neighborhood[p])
  
  data = filter(dat6, SALID == neighborhood[p])
  #print(p)
  these.knots = as.vector(quantile(data$TMP_PW, probs = c(0.10, 0.75, 0.90)))
  this.bound = c(min(data$TMP_PW), max(data$TMP_PW))
  
  argvar <- list(x=data$TMP_PW,fun="ns",#degree=3,
                 knots=these.knots, Boundary.knots=this.bound )
  bvar <- do.call(onebasis,argvar)
  
  
  #add prediction using meta-analysis prediction using undjusted overall MV-meta, MV1
  pred <- crosspred(bvar,coef=blup1[[p]]$blup,vcov=blup1[[p]]$vcov,
                    model.link="log",by=0.1,cen= quantile(data$TMP_PW, .75)) #mean(data$TMP_PW,na.rm=T))
  
  # use the first neighborhood in each country as an indicator to change plot:
  if( neighborhood[p] %in% c("20310210","20610110","10213610100","10224810100","10222610100","10219010100")) {
    xlab <- expression(paste("Temperature (",degree,"C)"))
    
    # layout(matrix(c(1,1,2,2,3,3,rep(4:9,each=2),10,10,11,11,12,12),ncol=6,byrow=T))
    
    plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
         main= paste0(unique(data$L1NAME)," ",min(data$YEAR),"-",max(data$YEAR)," (n=", unique(data$units),")")
    )
    rect(xleft=min(data$TMP_PW)-.48, ybottom=0.001, xright=33, ytop=3, col="white", border=NA)
    
    axis(2,at=1:5*0.5)
    breaks <- c(min(data$TMP_PW,na.rm=T)-1,seq(pred$predvar[1],
                                               pred$predvar[length(pred$predvar)],length=30),max(data$TMP_PW,na.rm=T)+1)
    hist <- hist(data$TMP_PW,breaks=breaks,plot=F)
    hist$density <- hist$density/max(hist$density)*0.7
    prop <- max(hist$density)/max(hist$counts)
    counts <- pretty(hist$count,3)
    plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
         breaks=breaks,freq=F,add=T)
    
    
    axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
    #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
    abline(v=c(mean(data$TMP_PW,na.rm=T)),lty=3.5)
    
  }  
  
  
  lines(pred$predvar, pred$allRRfit, col=rgb(1, 0, 0, 0.3), lwd=.6)  # First prediction in blue
  #lines(pred$predvar,pred$allRRlow,col= "red3",lwd=1 , lty="dashed")
  #lines(pred$predvar,pred$allRRhigh,col= "red3",lwd=1, lty="dashed")
  abline(h=1, col="black", lwd=1)
  
  
}

#dev.off()


print("last stage")

#---------------------------------------------------
# PLOTS
#---------------------------------------------------

PredictedMV1 = list()
PredictedRaw = list()

for(p in c(1:length(neighborhood))) {
  #for(p in c(1:10)) {
  
  
  #print(neighborhood[p])
  
  data = filter(dat6, SALID == neighborhood[p])
  #print(p)
  these.knots = as.vector(quantile(data$TMP_PW, probs = c(0.10, 0.75, 0.90)))
  this.bound = c(min(data$TMP_PW), max(data$TMP_PW))
  
  argvar <- list(x=data$TMP_PW,fun="ns",#degree=3,
                 knots=these.knots, Boundary.knots=this.bound )
  bvar <- do.call(onebasis,argvar)
  
  # use the first neighborhood in each country as an indicator to change new page:
  if( neighborhood[p] %in% c("20310210","20610110","10213610100","10224810100","10222610100","10219010100")) {
    
    
    
    layout(matrix(c(1,1,2,2,3,3,rep(4:9,each=2),10,10,11,11,12,12),ncol=6,byrow=T))
    
    plot(1, type="n", axes=FALSE, ann=FALSE) # Create an empty plot
    title(main= paste0(unique(data$L1NAME)," ",min(data$YEAR),"-",max(data$YEAR)," (n=", unique(data$units),")"), cex.main=1.2) # Set a main title
    
    pred <- crosspred(bvar,coef=blup1[[p]]$blup,vcov=blup1[[p]]$vcov,
                      model.link="log",by=0.1,cen=quantile(data$TMP_PW, .75)) #mean(data$TMP_PW,na.rm=T))
    
    plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
         main= paste0(unique(data$L2NAME2)," ",min(data$YEAR),"-",max(data$YEAR)," (n=", unique(data$units),")")
    )
    
    lines(pred$predvar, pred$allRRfit, col="red", lwd=1.5)  # First prediction in blue
    #lines(pred$predvar,pred$allRRlow,col= "red3",lwd=1 , lty="dashed")
    #lines(pred$predvar,pred$allRRhigh,col= "red3",lwd=1, lty="dashed")
    
    #plot prediction using city-specific knots (no smoothing)
    pred <- crosspred(bvar,coef=RED[[p]]$coefficients,vcov=RED[[p]]$vcov,
                      model.link="log",by=0.1,cen=quantile(data$TMP_PW, .75))#mean(data$TMP_PW,na.rm=T))
    
    #PredictedRaw[[i]]=pred
    
    # This is for city specific model
    lines(pred$predvar,pred$allRRfit,col= "blue",lwd=1.5)
    lines(pred$predvar,pred$allRRlow,col= "blue",lwd=.5 , lty="dashed")
    lines(pred$predvar,pred$allRRhigh,col= "blue",lwd=.5, lty="dashed")
    
    xlab <- expression(paste("Temperature (",degree,"C)"))
    
    
    axis(2,at=1:5*0.5)
    breaks <- c(min(data$TMP_PW,na.rm=T)-1,seq(pred$predvar[1],
                                               pred$predvar[length(pred$predvar)],length=30),max(data$TMP_PW,na.rm=T)+1)
    # hist <- hist(data$TMP_PW,breaks=breaks,plot=F)
    # hist$density <- hist$density/max(hist$density)*0.7
    # prop <- max(hist$density)/max(hist$counts)
    # counts <- pretty(hist$count,3)
    # plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
    #      breaks=breaks,freq=F,add=T)
    # axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
    
    #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
    abline(v=c(mean(data$TMP_PW,na.rm=T)),lty=3.5)
    
  }  
  
  pred <- crosspred(bvar,coef=blup1[[p]]$blup,vcov=blup1[[p]]$vcov,
                    model.link="log",by=0.1,cen=quantile(data$TMP_PW, .75)) #mean(data$TMP_PW,na.rm=T))
  
  plot(pred,type="n",ylim=c(0,2.5),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
       main= paste0(unique(data$L2NAME2)," ",min(data$YEAR),"-",max(data$YEAR)," (n=", unique(data$units),")")
  )
  
  lines(pred$predvar, pred$allRRfit, col="red", lwd=1.5)  # First prediction in blue
  #lines(pred$predvar,pred$allRRlow,col= "red3",lwd=.5 , lty="dashed")
  #lines(pred$predvar,pred$allRRhigh,col= "red3",lwd=.5, lty="dashed")
  
  #plot prediction using city-specific knots (no smoothing)
  pred <- crosspred(bvar,coef=RED[[p]]$coefficients,vcov=RED[[p]]$vcov,
                    model.link="log",by=0.1,cen=quantile(data$TMP_PW, .75))#mean(data$TMP_PW,na.rm=T))
  
  #PredictedRaw[[i]]=pred
  
  # This is for city specific model
  lines(pred$predvar,pred$allRRfit,col= "blue",lwd=1.5)
  lines(pred$predvar,pred$allRRlow,col= "blue",lwd=.5 , lty="dashed")
  lines(pred$predvar,pred$allRRhigh,col= "blue",lwd=.5, lty="dashed")
  
  xlab <- expression(paste("Temperature (",degree,"C)"))
  
  
  axis(2,at=1:5*0.5)
  breaks <- c(min(data$TMP_PW,na.rm=T)-1,seq(pred$predvar[1],
                                             pred$predvar[length(pred$predvar)],length=30),max(data$TMP_PW,na.rm=T)+1)
  # hist <- hist(data$TMP_PW,breaks=breaks,plot=F)
  # hist$density <- hist$density/max(hist$density)*0.7
  # prop <- max(hist$density)/max(hist$counts)
  # counts <- pretty(hist$count,3)
  # plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col=grey(0.95),
  #      breaks=breaks,freq=F,add=T)
  # axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
  
  #mtext("N",4,line=-0.5,at=mean(counts*prop),cex=0.5)
  abline(v=c(mean(data$TMP_PW,na.rm=T)),lty=3.5)
  
  
}

dat6 |>
  ungroup() |>
  summarize(L1NAME = first(L1NAME),
            tmp_p75 = quantile(TMP_PW, .75),
            tmp_p99 = quantile(TMP_PW, .99), .by = SALID1) ##|>
  write_csv("p75_p99_temp_by_city.csv")

# Retrieve the MMT from a previous prediction
get_cen <- function(crosspred) {
  if(!is.null(crosspred$fit)) {
    crosspred$predvar[which.min(crosspred$fit)]
  } else if(!is.null(crosspred$allfit)) {
    crosspred$predvar[which.min(crosspred$allfit)]
  } else {
    print("Unable to locate minimum.")
  }
}

get_RR <- function(pred, percentile) {
  pred$allRRfit[which.min(abs(pred$predvar - percentile))]
}

# Get RR at p99 for each nbhd, centering at the mean, p75, and MMT
analyze_nbhd <- function(i) {

  # Filter data to specific neighborhood
  dat_x <- filter(dat6, SALID == neighborhood[[i]])

  # Get the temperature at the 99th percentile
  p99 <- round(quantile(dat_x$TMP_PW, .99), 2)

  # Specify the basis functions
  bvar <- onebasis(x = dat_x$TMP_PW,
                   fun = "ns",
                   knots = quantile(dat_x$TMP_PW, c(.10, .75, .90)),
                   Boundary.knots = range(dat_x$TMP_PW))

  # Smoothed, centered at overall mean
  pred_S_Mean <- crosspred(bvar, coef=blup1[[i]]$blup, vcov=blup1[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=mean(dat_x$TMP_PW, na.rm=TRUE))

  pred_S_P75  <- crosspred(bvar, coef=blup1[[i]]$blup, vcov=blup1[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=quantile(dat_x$TMP_PW, .75))

  pred_S_MMT  <- crosspred(bvar, coef=blup1[[i]]$blup, vcov=blup1[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=get_cen(pred_S_Mean))

  pred_I_Mean <- crosspred(bvar, coef=RED[[i]]$coefficients, vcov=RED[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=mean(dat_x$TMP_PW,na.rm=T))

  pred_I_P75  <- crosspred(bvar, coef=RED[[i]]$coefficients, vcov=RED[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=quantile(dat_x$TMP_PW, .75))

  pred_I_MMT  <- crosspred(bvar, coef=RED[[i]]$coefficients, vcov=RED[[i]]$vcov,
                           model.link="log", by=0.01,
                           cen=get_cen(pred_I_Mean))

  # Extract the RR at the 99 percentile of temperature
  tibble(SALID = neighborhood[[i]],
         RR_S_Mean = get_RR(pred_S_Mean, p99),
         RR_S_P75  = get_RR(pred_S_P75,  p99),
         RR_S_MMT  = get_RR(pred_S_MMT,  p99),
         RR_I_Mean = get_RR(pred_I_Mean, p99),
         RR_I_P75  = get_RR(pred_I_P75,  p99),
         RR_I_MMT  = get_RR(pred_I_MMT,  p99))
}

# Run analysis on each neighborhood
df_RR <- map(1:length(neighborhood), analyze_nbhd) |> list_rbind()

# Save results (centering at mean, p75, and MMT for smoothed and individually estimated data)
write_csv(df_RR, "RR_by_neighborhood.csv")



