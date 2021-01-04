head(validation)
install.packages("acid")

#sort by prediction
validation_glm <- validation %>% 
        arrange(prediciton_glm) %>%
        mutate(cum_loss = cumsum(ClaimAmt)) %>%
        mutate(cum_exposure_pct = cumsum(Exposure)/sum(Exposure)) %>%
        mutate(rank = row_number()/length(validation$ClaimPE)) %>%
        mutate(equality = max(cum_loss)*cum_exposure_pct) %>%
        mutate(above = equality - cum_loss) 

Area_Above_glm <- sum(validation_glm$above)
Area_Below_glm <- sum(validation_glm$cum_loss)
Gini_glm <- Area_Above_glm/(Area_Above_glm+Area_Below_glm)
Gini_glm

#no weight 
validation_gbm <- validation %>% 
        arrange(prediciton_gbm) %>%
        mutate(cum_loss = cumsum(ClaimAmt)) %>%
        mutate(cum_exposure_pct = cumsum(Exposure)/sum(Exposure)) %>%
        mutate(rank = row_number()/length(validation$ClaimPE)) %>%
        mutate(equality = max(cum_loss)*cum_exposure_pct) %>%
        mutate(above = equality - cum_loss) 

Area_Above_gbm <- sum(validation_gbm$above)
Area_Below_gbm <- sum(validation_gbm$cum_loss)
Gini_gbm <- Area_Above_gbm/(Area_Above_gbm+Area_Below_gbm)
Gini_gbm

validation_rf <- validation %>% 
        arrange(prediciton_rf) %>%
        mutate(cum_loss = cumsum(ClaimAmt)) %>%
        mutate(cum_exposure_pct = cumsum(Exposure)/sum(Exposure)) %>%
        mutate(rank = row_number()/length(validation$ClaimPE)) %>%
        mutate(equality = max(cum_loss)*cum_exposure_pct) %>%
        mutate(above = equality - cum_loss) 

Area_Above_rf <- sum(validation_rf$above)
Area_Below_rf <- sum(validation_rf$cum_loss)
Gini_rf <- Area_Above_rf/(Area_Above_rf+Area_Below_rf)


Gini_glm
Gini_gbm
Gini_rf

#plot cumulative losses - note, we don't need to step by exposures on x axis because losses are already per exposure 

ggplot(validation_gbm,aes(x=cum_exposure_pct,y=cum_loss/max(cum_loss))) + 
        geom_line() +
        geom_abline(slope=1,intercept=0)

ggplot(validation_nowt,aes(x=cum_exposure_pct,y=cum_loss/max(cum_loss))) + 
        geom_line() +
        geom_abline(slope=1,intercept=0)

#gini: 

#area under the curve

Area_Above <- sum(validation$above)
Area_Below <- sum(validation$cum_loss)
Gini <- Area_Above/(Area_Above+Area_Below)
Gini

Area_Above
Area_Below

head(validation)




