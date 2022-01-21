library(piecewiseSEM)
library(dplyr)
library(nlme)
library(lme4)
library(blavaan)
library(lavaan)
library(nnet)
library(tidyverse)
library(mma)
library(mediation)
library(brms)
library(sjstats)
library(semTools)
library(Amelia)
library(arm)
library(OpenMx)
library(glmnet)
library(blme)
library(openxlsx)
library(psych)
library(corrplot)
library(ggstatsplot)


setwd("/Users/denisebradford/Documents/STAT 831 /nfl-playing-surface-analytics")

#injury_record <- read.csv("InjuryRecord.csv", header=TRUE)
#Data with the Spatial coordinates may not need in this analysis
#player_track_data <- read.csv("PlayerTrackData.csv", header=TRUE) 
#play_list <- read.csv("PlayList.csv", header=TRUE) #List of the plays 

#Save Data to .RData
#save(injury_record, play_list, player_track_data, file = "SEMdata.RData")

#Reload Data
load("SEMdata.RData")

#Data cleaning/merging
player_track_data <- player_track_data %>% 
  mutate(isInjured = PlayKey %in% injury_record$PlayKey) 

# solution for the below function taken from here: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
repeat.before <- function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}  

player_track_data$event <- ifelse(player_track_data$event == "", NA, player_track_data$event)
player_track_data <- player_track_data %>% group_by(PlayKey) %>% mutate(event_clean = repeat.before(event)) %>% ungroup()


# create vector of values for in_play
in_play <- c("tackle", "ball_snap", "pass_outcome_incomplete", "out_of_bounds", "first_contact", 
             "handoff", "pass_forward", "pass_outcome_caught", "touchdown", "qb_sack", "touchback", 
             "kickoff", "punt", "pass_outcome_touchdown", "pass_arrived", "extra_point", "field_goal", 
             "play_action", "kick_received", "fair_catch", "punt_downed", "run", "punt_received", "qb_kneel", 
             "pass_outcome_interception", "field_goal_missed", "fumble", "fumble_defense_recovered", "qb_spike", 
             "extra_point_missed", "fumble_offense_recovered", "pass_tipped", "lateral", "qb_strip_sack", "safety", 
             "kickoff_land", "snap_direct", "kick_recovered", "field_goal_blocked", "punt_muffed", "pass_shovel", "extra_point_blocked", 
             "pass_lateral", "punt_blocked", "run_pass_option", "free_kick", "punt_fake", "end_path", "drop_kick", 
             "field_goal_fake", "extra_point_fake", "xp_fake")

# create variable to indicate whenther the even is in_play or dead_ball
player_track_data <- player_track_data %>% 
  mutate(play_stage = ifelse(event_clean %in% in_play, "in_play", "dead_ball"))


# create a summary dataframe of player tracking data for analysis
player_track_data_summary <- player_track_data %>% 
  filter(!is.na(event %in% in_play)) %>%
  group_by(PlayKey) %>% 
  summarise(min_time = min(time, na.rm = T),
            max_time = max(time, na.rm = T),
            avg_speed = mean(s, na.rm = T),
            sd_speed = sd(s, na.rm = T)) %>% 
  mutate(play_time = max_time - min_time) 

# join play list data to get additional metadata
player_track_data_summary <- player_track_data_summary %>% 
  left_join(play_list, by = "PlayKey") 

player_track_data_summary <- player_track_data_summary %>% 
  left_join(comb_data1, by = c("PlayKey", "GameID", "PlayerKey"))

player_track_data_summary <- player_track_data_summary %>% 
  mutate(isInjured = PlayKey %in% injury_record$PlayKey) 

comb_data <- injury_record %>% left_join(play_list ,by = c("PlayKey", "GameID", "PlayerKey"))

#Read in comb_data with new categories for analysis
comb_data1 <- read.xlsx("comb_data.xlsx", colNames =TRUE)

#Data with the categorical Weather and Stadium Type
clean_data <- read.xlsx("player_track.xlsx", colNames =TRUE, na.strings = "-999")

#clean_data1<-[clean_data[order(max(nchar(clean_data$PlayKey))), ], by=PlayerKey]$V1]

clean_data1<- clean_data[order(-clean_data$PlayKey), ]
clean_data1<-clean_data[ duplicated(clean_data$PlayerKey), ]
  
comb_data2 <- player_track_data %>% left_join(comb_data1, by = "PlayKey")

clean_data$isInjured = as.integer(as.logical(clean_data$isInjured))
clean_data$Severity = factor(clean_data$Severity, levels = c("Most Severe", "Severe","Mild/Moderate"))
clean_data$Severity = ordered(clean_data$Severity, levels = c("Mild/Moderate", "Severe","Most Severe"))

clean_data$Severity = as.numeric(as.factor(clean_data$Severity))

clean_data$BodyPart = factor(clean_data$BodyPart, levels = c("None","Knee","Foot","Ankle"))
clean_data$FieldType.x <- as.factor(clean_data$FieldType.x)
clean_data$StadimuType_factor <- as.factor(clean_data$StadimuType_factor)
clean_data$Weather_factor <- as.factor(clean_data$Weather_factor)
clean_data$PositionGroup.x <- as.factor(clean_data$PositionGroup.x)

as.numeric(as.character(clean_data$Severity))

levels <- levels(clean_data$Severity)
levels[length(levels) + 1] <- "None"
clean_data$Severity <- factor(clean_data$Severity, levels = levels)

clean_data$Severity[is.na(clean_data$Severity)] <- "None"
clean_data$BodyPart[is.na(clean_data$BodyPart)] <- "None"

clean_data$Severity = as.character(clean_data$Severity)

#Collapsing the moderate and mild cases into one level
clean_data$Severity <- factor(clean_data$Severity)
levels(clean_data$Severity) <- list("Mild/Moderate"=c("Mild", "Moderate"), "Severe"=c("Severe", "Most Severe"), "None" = "None")
#Collapsing the Most Severe and Severe cases into one level
clean_data$Severity <- factor(clean_data$Severity)
levels(clean_data$Severity) <- list("Severe"=c("Severe", "Most Severe"), "Mild/Moderate"="Mild/Moderate")

clean_data$Severity = as.numeric(as.character(clean_data$Severity))


colnames(clean_data) = c( "PlayKey", "min_time", "max_time","avg_speed", "sd_speed","play_time", "PlayerKey", "GameID",
                          "RosterPosition", "PlayerDay","PlayerGame", "StadiumType","FieldType", "Temperature", "Weather",
                          "PlayType", "PlayerGamePlay",  "Position","PositionGroup","isInjured", "BodyPart", "Surface", 
                          "Weather_factor","StadimuType_factor","Severity")

save(cleandata, file = "SEMcleandata.RData")
load("SEMcleandata.RData")
#################################################   ANALYSIS STARTS HERE  ######################################################
#Ridge Regression

x = cleandata[,c("play_time", "PlayType", "PlayerGamePlay","PositionGroup", "FieldType", "Temperature", "Weather_factor","StadimuType_factor")]
y = cleandata[,c("isInjured")]

fit = glmnet(x, y, family = "binomial",  relax=TRUE, path=TRUE)

#Latent Growth Curve Modeling
cleandata <- clean_data %>% filter(PlayerKey %in% injury_record$PlayerKey)

#1st attempt
mixedmodel = bglmer(isInjured ~ PlayerGamePlay + FieldType + (PlayerGamePlay|PlayerKey) , #+ PlayType + PositionGroup
                   data = clean_data, family = "binomial")
summary(mixedmodel)


cleandata$PlayerGame_factor = as.factor(cleandata$PlayerGame)
scaled.cleandata <- scale(cleandata)

#2nd attempt
mixedmodel.2 = bglmer(isInjured ~ FieldType + PlayerGame  + (1 + PlayerGame|PlayerKey), 
                     data = cleandata, family = binomial(link = "logit"))
summary(mixedmodel.2)
factor.residuals(mixedmodel.2)

cleandata$Weather_factor_na = na.omit(cleandata$Weather_factor)
cleandata$StadimuType_factor_na = na.omit(cleandata$StadimuType_factor)

#3rd attempt
mixedmodel.3 = bglmer(isInjured ~ FieldType + Weather_factor + StadimuType_factor + 
                        PlayerGame + (1 + PlayerGame|PlayerKey), 
                      data = cleandata, family = binomial(link = "logit"))
summary(mixedmodel.3)
std_beta(mixedmodel.3, type = "std", ci.lvl = 0.95)

anova(mixedmodel.2, mixedmodel.3)

#Correlation Matrices for each of the models
cov2cor(vcov(mixedmodel.2))
cov2cor(vcov(mixedmodel.3))

cleandata$isInjured = as.integer(as.logical(cleandata$isInjured))
model5 <- brm(isInjured ~ FieldType + Weather_factor + StadimuType_factor + 
                PlayerGame + (1 + PlayerGame|PlayerKey), family = bernoulli(link = "logit"),
              data  = cleandata, warmup = 100,
              iter  = 1500, chains = 3, 
              seed  = 123, control = list(adapt_delta = 0.97),
              cores = 2) #parallel::detectCores()

summary(model5,waic = TRUE)
posterior_summary(model5)["b_Intercept", ] %>% 
  round(digits = 2)


f <-
  fitted(model5, summary = F,
         scale = "linear") %>% 
  set_names("p")

glimpse(f)

f %>% 
  ggplot(aes(x = p)) +
  geom_density(fill = "grey50", color = "grey50") +
  annotate(geom = "text", 
           x = .08, y = 2.5,
           label = "Posterior probability") +
  scale_x_continuous("probability of water",
                     breaks = c(0, .5, 1),
                     limits = 0:1) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank())


######## WORK DONE NOT PRESENTED ##########

library(gridExtra)
pdf("data_output.pdf", height=8.5, width=11)
grid.table(round(cov2cor(vcov(mixedmodel.2)), 2))
#grid.table(round(cov2cor(vcov(mixedmodel.3)), 2))
dev.off()

##Plot
library(gplots)
# Plot the mean of teeth length by dose groups
plotmeans(isInjured ~ interaction(FieldType, PlayerGame), data = cleandata, na.action = na.omit)

gd <- cleandata %>% 
  group_by(FieldType, PlayerGame) %>% 
  summarise(injury = mean(isInjured))

ggplot(gd, aes(x = PlayerGame, y = injury, color = FieldType)) +
  geom_point()

gd.1 <- cleandata %>% 
  group_by(FieldType, PlayerGame, Weather_factor, StadimuType_factor) %>% 
  summarise(injury = mean(isInjured))

ggplot(gd.1, aes(x = PlayerGame, y = injury, color = FieldType)) + facet_wrap(vars(Weather_factor, StadimuType_factor)) + 
  geom_point()


# plot
ggstatsplot::ggcoefstats(
  x = mixedmodel.2
)

###LATENT GROWTH MODELING###
a = unique(cleandata[,c("isInjured", "FieldType","PlayType", "Weather_factor", "StadimuType_factor", "PositionGroup", "PlayerGame", "PlayerKey")])
a <- a[order(a$PlayerKey, -abs(a$isInjured) ), ]
b=a[!duplicated(a[,2:8]),]

cleandataWide = b %>% 
  dplyr::select(isInjured, FieldType, PlayType, Weather_factor,StadimuType_factor,PositionGroup, PlayerGame, PlayerKey) %>%
  spread(key = PlayerGame, value = isInjured) %>%
  rename_at(vars('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15',
                 '16','17','18','19','20','21','22','23','24','25','26','27','28',
                 '29','30','31','32'), function(x) glue::glue('game_{x}')) 

model.matrix
injury.cov = cov(cleandataWide[,c(7:38)], use = "pairwise.complete.obs")
injury.mean = colMeans(cleandataWide[,c(7:38)], na.rm = T)

cleandataWide$game_3 =  cleandataWide$game_3 %>% replace_na(0)


growth.model <- ' 
  # intercept and slope with fixed coefficients
i =~ 1*game_1 + 1*game_2 + 1*game_3 + 1*game_4 + 1*game_5 + 1*game_6 + 1*game_7 + 1*game_8 + 1*game_9 + 1*game_10 + 1*game_11 + 1*game_12 + 1*game_13 + 1*game_14+ 1*game_15 + 1*game_16 
s =~ 0*game_1 + 1*game_2 + 2*game_3 + 3*game_4 + 4*game_5 + 5*game_6 + 6*game_7 + 7*game_8 + 8*game_9 + 9*game_10 + 12*game_11 + 13*game_12 + 14*game_13 + 15*game_14+ 16*game_15 + 17*game_16 

#regressions
#i + s ~ FieldType + PlayType + Weather_factor + StadimuType_factor 
i + s ~ FieldType
'
#modificationIndices(model = growth.model, data = cleandataWide)

fit <- bgrowth(model = growth.model, data=cleandataWide, missing = "ml", mimic = "Mplus")
summary(fit)

#3rd attempt with all levels of variables that we think play a role in an injury
mixedmodel.3 = bglmer(isInjured ~ play_time + FieldType + PositionGroup +  Weather_factor + 
                        Temperature + StadimuType_factor + (1|PlayerGamePlay), 
                      data = cleandata, family = binomial(link = "logit"))
summary(mixedmodel.3)

#3rd attempt with all levels of variables that we think play a role in an injury
mixedmodel.3 = bglmer(isInjured ~ play_time + FieldType + PositionGroup +  Weather_factor + 
                        Temperature + StadimuType_factor + (1|PlayerGamePlay), 
                      data = cleandata, family = binomial(link = "logit"))
summary(mixedmodel.3)


# Construct Bayesian SEM - My first attempt at a SEM model
#models for mediation analysis 
b1 <- bayesglm(isInjured ~ FieldType.x + Weather_factor + Temperature.x + StadimuType_factor + PositionGroup.x + PlayerGame.x + BodyPart,
          family = "binomial", data = clean_data, maxit = 100)
b2 <- multinom(Severity ~ FieldType.x + Weather_factor + Temperature.x + StadimuType_factor + BodyPart , 
                data = clean_data, maxit = 100) #mcmc.method="IndMH", B0=0,verbose=500, mcmc=100000, thin=10, tune=0.5
b3 <- multinom(BodyPart ~ FieldType.x + PositionGroup.x + PlayerGame.x  , 
               data = clean_data)

m1 <- mediate(b2,b1, sims = 1000, treat = "FieldType", 
              mediator = "FieldType.x", control.value = "Natural", boot = T,
              treat.value = "Synthetic") 

b3 <- multinom(Severity ~ BodyPart + FieldType.x +  play_time + Temperature.x + isInjured , 
               data = clean_data)
m1 <- mediate(b1,b3, sims = 500, treat = "FieldType.x", group.out="Severity", 
              mediator = "isInjured", control.value = "Natural", dropobs = TRUE,
              treat.value = "Synthetic", boot = T) 
#Multilevel SEM 
level1 <- clean_data[, c('PlayKey', 'FieldType')]
level2 <- clean_data[!duplicated(clean_data$PlayerKey),c('Severity','BodyPart','PositionGroup','PlayerGame','PlayKey') ] #'Weather_factor','StadimuType_factor','Temperature.x',

# The manifest variables loading on each proposed latent variable
Severity <- c("Weather_factor","StadimuType_factor","FieldType", "Temperature")
BodyPart <- c("PositionGroup", "PlayerGame","FieldType")

latents   <- c("severity", "bodypart")
manifests <-  c(Severity, BodyPart)

injury.model = mxModel(model = "Injury_NFL", type="RAM", 
                   manifestVars = manifests,
                   latentVars   = latents, 
                   
                   # factor loadings from latents to  manifests
                   mxPath(from="severity", to=Severity),
                   mxPath(from="bodypart", to=BodyPart),
                   
                   # Allow latent variables to covary 
                   mxPath(from="severity", to="bodypart", arrows = 2, free = TRUE),
                   # Allow latent variables to have variance
                   mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
                   # Manifest have residual variance
                   mxPath(from=manifests, arrows=2),   
                   # the data to be analysed
                   mxData(cov(clean_data[,manifests], use = "complete" , method = "pearson"), type = "cov", numObs = 266961))

fitmodel = mxRun(injury.model)
summary(fitmodel)

#Bayesian Model with basic idea
bayes.model <- '
#Latent variables
  BodyPart ~  PositionGroup + PlayerGame + FieldType
  Severity ~  Weather_factor + StadimuType_factor + FieldType + Temperature + BodyPart
#regressions
#Severity ~ BodyPart
#Residual Correlations
 # StadimuType_factor ~~ FieldType
#  Weather_factor ~~ Temperature
#  PositionGroup ~~ PlayerGame
  
'

clean_data$Severity = as.numeric(as.factor(clean_data$Severity))
clean_data$FieldType = as.numeric(as.factor(clean_data$FieldType))
clean_data$StadimuType_factor = as.numeric(as.factor(clean_data$StadimuType_factor))
clean_data$PositionGroup = as.numeric(as.factor(clean_data$PositionGroup))
clean_data$Weather_factor = as.numeric(as.factor(clean_data$Weather_factor))


fit.bayes = bsem(bayes.model, data=comb_data1, link = "logit")
multigroup1 <- sem(bayes.model, comb_data1, ordered = c("Moderate","Mild","Severe","Most Severe") ) 
