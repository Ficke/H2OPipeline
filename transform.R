#Adam Ficke 
#This is a pipeline to read in data used for predictive modeling, examine it, munge, train, and evaluate performance. 
#We'll be using dplyr to start - then will port in data.table (either through dplyr package or directly)

#step 0: load packages 
require("dplyr")
require("h2o")
require("ggplot2")
require("tidyverse")
require("gglorenz")

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

#create target
df.full <- df.full %>% 
        mutate(ClaimPE = ClaimAmt/Exposure)
str(df.full)
head(df.full)
#Select Features to use 
predictors <- c("Area","VehPower","VehAge","DrivAge","VehBrand","BonusMalus","VehGas","Region","Density")
target <- c("ClaimPE","ClaimAmt")
id <- c("IDpol","Exposure")
fields <- c(id,target,predictors)
fields
names(df.full)
df.model <- df.full %>%
        dplyr::select(all_of(fields))

#Remove other data from memory

#check for missing rows
df.model %>%
        summarise_all(list(sum=~sum(is.na(.))))

#Which features should be categorical
str(df.model)

#code categorical variables 

df.model <- df.model %>% 
        mutate(across(c(VehPower,Area,VehBrand,BonusMalus,Region,VehGas),factor))

#code target variables 
# df.model <- df.model %>% 
#         mutate(across(c(ClaimAmt),integer))


#one-way plots

#faster

plot.VehAge <- df.model %>% 
        group_by(VehAge) %>%
        summarise(AverageClaimAmt = mean(ClaimPE),n()) 

ggplot(plot.VehAge,aes(x=VehAge,y=AverageClaimAmt)) + 
        geom_bar(stat='identity')

plot.Area <- df.model %>% 
        group_by(Area) %>%
        summarise(AverageClaimAmt = mean(ClaimAmt),n()) 

ggplot(plot.Area,aes(x=Area,y=AverageClaimAmt)) + 
        geom_bar(stat='identity')

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

h2o.init(nthreads=16, max_mem_size = "12g")



df <- as.h2o(df.model)
df.split <- h2o.splitFrame(df,ratios=0.7)
df.train <- df.split[[1]]
df.test <- df.split[[2]]

insurance_glm <- h2o.glm(family = "poisson",
                         x = predictors,
                         y = "ClaimPE",
                         weights_column = "Exposure",
                         training_frame = df.train,
                         validation_frame = df.test,
                         nfolds = 5, 
                         seed = 1,
                         alpha = 0.1,
                         lambda_search = TRUE)
#var importance 
h2o.varimp(insurance_glm)

#grid search 
seq(from=0,to=1,0.01)
glm_params1 <- list(alpha = seq(from=0,to=1,0.01))


insurance_glm_grid <- h2o.grid("glm",x = predictors, y = "ClaimPE",
                               weights_column = "Exposure",
                      grid_id = "glm_grid1",
                      training_frame = df.train,
                      validation_frame = df.test,
                      nfolds = 5,
                      seed = 1,
                      lambda_search = TRUE,
                      hyper_params = glm_params1)

#RANDOM FOREST
insurance_rf <- h2o.randomForest(training_frame = df.train,
                                 validation_frame = df.test,
                                 x = predictors, y = "ClaimPE",
                                 ntrees = 200,
                                 weights_column = "Exposure",
                                 stopping_rounds = 2,
                                 seed = 1) 

insurance_gbm <- h2o.gbm(training_frame = df.train,
                         validation_frame = df.test,
                         x = predictors, y = "ClaimPE",
                         weights_column = "Exposure",
                         model_id = "gbm_covType1")
summary(insurance_gbm)

#autoML comparison

contest <- h2o.automl(training_frame = df.train,
                      validation_frame = df.test,
                      x = predictors, y = "ClaimPE",
                      weights_column = "Exposure")

h2o.varimp_plot(insurance_gbm,num_of_features = 20)
h2o.varimp_heatmap(insurance_gbm)

# Get the grid results, sorted by validation AUC
glm_gridperf1 <- h2o.getGrid(grid_id = "glm_grid1",
                             sort_by = "mse",
                             decreasing = FALSE)
print(glm_gridperf1)
best_glm1 <- h2o.getModel(glm_gridperf1@model_ids[[1]])






# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_glm_perf1 <- h2o.performance(model = best_glm1,
                                  newdata = df.test)

print(best_glm1@model[["model_summary"]])


#Variable Importance 


#score test set 
df.test$prediciton_glm <- h2o.predict(insurance_glm,newdata = df.test)
df.test$prediciton_rf <- h2o.predict(insurance_rf,newdata = df.test)
df.test$prediciton_gbm <- h2o.predict(insurance_gbm,newdata = df.test)
validation <- as.data.frame(df.test)
validation_nowt <- as_tibble(validation)

df.test$prediciton_nowt <- h2o.predict(insurance_gbm,newdata = df.test)
validation_nowt <- as.data.frame(df.test)
validation_nowt <- as_tibble(validation_nowt)

validation
validation %>%
        ggplot(aes(x = prediciton)) +
        annotate_ineq(validation$prediciton) +
        stat_lorenz(desc = FALSE,geom = "area", alpha = 0.65) +
        coord_fixed() +
        geom_abline(linetype = "dashed") +
        theme_minimal() +
        hrbrthemes::scale_x_percent() +
        hrbrthemes::scale_y_percent() +
        labs(x = "Cumulative Percentage of Exposures",
             y = "Cumulative Percentage of Claims")

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

h2o.coef(insurance_glm)
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
















