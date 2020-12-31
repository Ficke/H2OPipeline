#Adam Ficke 
#This is a pipeline to read in data used for predictive modeling, examine it, munge, train, and evaluate performance. 
#We'll be using dplyr to start - then will port in data.table (either through dplyr package or directly)

#step 0: load packages 
require("dplyr")
require("h2o")
require("ggplot2")
require("tidyverse")
require("dlookr")
require(Hmisc)

#Step 1: Read in data 
getwd()
setwd("~/Coding/R/PipelineGLM")

freq_file <- "freMTPL2freq.csv"
sev_file <- "freMTPL2sev.csv"


if (!file.exists(freq_file)){
        link <- "https://www.openml.org/data/get_csv/20649148/freMTPL2freq.arff"
        download.file(link, freq_file, method="curl")
}  

if (!file.exists(sev_file)){
        link <- "https://www.openml.org/data/get_csv/20649149/freMTPL2sev.arff"
        download.file(link, sev_file, method="curl")
}  


df.freq <- read_csv(freq_file)
df.sev <- read_csv(sev_file)
#group severity by policy ID
df.sev.g <- df.sev %>% 
        group_by(IDpol) %>%
        summarise(sum = sum(ClaimAmount))

#Join data 
df.full <- left_join(df.freq,df.sev.g, by = "IDpol")
rm(df.freq,df.sev.g,df.sev)

#read features, rename & document their data types
names(df.full)
df.full <- df.full %>% 
        rename(ClaimAmt = sum) %>%
        replace_na(list(ClaimAmt = 0))

str(df.full)

#Select Features to use 
predictors <- c("Area","VehPower","VehAge","DrivAge","VehBrand")
target <- c("ClaimNb","ClaimAmt")
id <- c("IDpol","Exposure")
fields <- c(id,target,predictors)

df.model <- df.full %>%
        select(all_of(fields))
#Remove other data from memory

#check for missing rows
df.model %>%
        summarise_all(list(sum=~sum(is.na(.))))

#Which features should be categorical
str(df.model)

#code categorical variables 

df.model <- df.model %>% 
        mutate(across(c(VehPower,Area,VehBrand),factor))

#code target variables 
df.model <- df.model %>% 
        mutate(across(c(ClaimAmt),integer))


str(df.model$ClaimAmt)

#one-way plots

#faster

plot.VehAge <- df.model %>% 
        group_by(VehAge) %>%
        summarise(AverageClaimAmt = mean(ClaimAmt),n()) 

ggplot(plot.VehAge,aes(x=VehAge,y=AverageClaimAmt)) + 
        geom_bar(stat='identity')

plot_Area <- df.model %>% 
        group_by(Area) %>%
        summarise(AverageClaimAmt = mean(ClaimAmt),n()) 

ggplot(plot_Area,aes(x=Area,y=AverageClaimAmt)) + 
        geom_bar(stat='identity')

#less code - this is actually showing the sum 
ggplot(df.model,aes(x=VehPower,y=ClaimAmt)) + 
        geom_col()


#bin characteristics - factors 
df.model <- df.model %>% 
        mutate(DrivAgeBin = cut(DrivAge,breaks=c(0,20,30,60,100)))

#bin chars based on distributions - look into quantile type = 2 vs. others for different methods. use labels=c() for labels cuts explicitly
ints <- quantile(df.model$DrivAge,probs = c(0,.25,.5,.75,1))
table(cut(df.model$DrivAge,breaks=ints,labels = 1:4))

df.model <- df.model %>% 
        mutate(DrivAgeBin1 = cut(DrivAge,breaks=ints,labels = c("one","Two","Three","Four")))

#equal spaced cuts 
df.model <- df.model %>% 
        mutate(DrivAgeBin2 = cut(DrivAge,breaks=c(0,20,30,60,100)))



# df.model <- df.model %>% 
#         mutate(DrivAge, Driver_ntile = ntile(DrivAge,4))


#variable reduction


# GLM Model Fit

#Start H2O 

h2o.init(nthreads=16, max_mem_size = "6g")



df <- as.h2o(df.model)
df.split <- h2o.splitFrame(df,ratios=0.7)
df.train <- df.split[[1]]
df.test <- df.split[[2]]

Insurance_glm <- h2o.glm(family = "poisson",
                         x = predictors,
                         y = "ClaimAmt",
                         training_frame = df.train,
                         validation_frame = df.test,
                         nfolds = 5, 
                         seed = 1,
                         lambda_search = TRUE)

#still need to do: 
## Plot lorenz curve 
## calculate gini coef. on test data 
## impl;ement ridge/lammbda/elastic net 
## cross validation for grid search
## grid search for lambda and alpha
## Save predicitons 
## save model object 
## compare models 

## run random forest
## run GBM 

h2o.coef(Insurance_glm)
Insurance_glm@model$coefficients_table
h2o.std_coef_plot(Insurance_glm)       

#performance summary
perf <- h2o.performance(Insurance_glm, df.test)
perf
h2o.giniCoef(perf)
df.test$pred <- h2o.predict(Insurance_glm, df.test)

h2o.make_metrics(
        predicted,
        actuals,
        domain = NULL,
        distribution = NULL,
        weights = NULL,
        auc_type = "NONE"
)

print(h2o.mse(Insurance_glm, valid = TRUE))
print(h2o.mse(Insurance_glm2, valid = TRUE))
                         


h2o.shutdown()
















