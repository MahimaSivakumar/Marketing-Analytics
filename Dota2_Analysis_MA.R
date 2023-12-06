install.packages("mice")

library(greybox)
library(smooth)
library(readxl)
library(ggplot2)
library(tseries)
library(corrplot)
library(forecast)
library(diffusion)
library(stats)
library(vars)
library(ARDL)
library(mice)
Dota = read_xlsx("Dota2_players.xlsx")
Dota_promo = read_xlsx("Dota2_players.xlsx")
Dota_final = read_xlsx("Dota2_Final.xlsx")
#Question 1 
#for twitch users influence on players count flag value is not needed and NA values are removed 
Dota = na.omit(Dota)
#par(mfrow = c(1, 2))
str(Dota)
#Exploratory Data Analysis 

#Correlation 
Cor_mat = Dota[,2:4]
corr =cor(Cor_mat)
corrplot(corr)

#Players count
plot(Dota$Date ,Dota$Players,type="l",xaxt='n',xlab = "Year", ylab = "Players Count",lwd=3.0)
# Create a custom x-axis with a two-week frequency
date_seq <- seq(min(Dota$Date), max(Dota$Date), by = "quarter")
axis(1, date_seq, format(date_seq, "%Y"), cex.axis = 0.6)

#Twitch Viewers
plot(Dota$Date,Dota$Twitchviewers,type="l",xaxt='n',xlab = "Year", ylab = "Twitch Viewers",lwd=3.0)
# Create a custom x-axis with a two-week frequency
date_seq <- seq(min(Dota$Date), max(Dota$Date), by = "quarter")
axis(1, date_seq, format(date_seq, "%Y"), cex.axis = 0.6)


#both players and twitch in one graph
ggplot(Dota, aes(x = as.Date(Date))) +
  geom_line(aes(y = Players, col = "Players")) +
  geom_line(aes(y = Twitchviewers, col = "Twitch Viewers")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Years") +
  ylab("Values") +
  scale_color_manual(name = "Legend", 
                     values = c("Players" = "#F77919", "Twitch Viewers" = "#852E8B"))
#Players vs Twitch viewers 
ggplot(Dota, aes(x = Players, y = Twitchviewers)) +
  geom_point() +
  xlab("Players") +
  ylab("Twitch Viewers")+
  ggtitle("Players vs Twitch Viewers")

spread(Dota)

#Dota$log_twitch <- 1/Dota$Twitchviewers

# Plot the relationship between players and log-transformed twitch
#ggplot(data = Dota, aes(x = log_twitch, y = Players)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  xlab("sqrt(twitch)") +
#  ylab("Players")
#Dota <- subset(Dota, select = -log_twitch)

#ARDL Model 
players = Dota$Players
twitch = Dota$Twitchviewers
#stationary test for players count on 5% significance level 
adf.test(Dota$Players) #Stationary 
kpss.test(Dota$Players) # Non - stationary 

#ACF , PACF , CCF Plots 
layout(matrix(c(1,2),1,2))
forecast::Acf(as.vector(Dota$Players),main="ACF of Players")
pacf(as.vector(Dota$Players),main="PACF of Players")
#ccf(as.vector(twitch),as.vector(players),main="CCF of Twitch vs Players")

#differencing 
adf.test(diff(Dota$Players,differences = 1))
kpss.test(diff(Dota$Players,differences = 1))
forecast::tsdisplay(diff(Dota$Players,differences=1))

#stationary test for twitch viewers on 5% significance level 
adf.test(Dota$Twitchviewers) #Stationary 
kpss.test(Dota$Twitchviewers) # Non - stationary 
#ACF , PACF , CCF Plots 
layout(matrix(c(1,2),1,2))
forecast::Acf(as.vector(Dota$Twitchviewers),main="ACF of Twitch")
pacf(as.vector(Dota$Twitchviewers),main="PACF of Twitch")
#differencing 
adf.test(diff(Dota$Twitchviewers,differences = 1))
kpss.test(diff(Dota$Twitchviewers,differences = 1))
forecast::tsdisplay(diff(Dota$Twitchviewers,differences=1))

#Cointegration test 
GameModel = lm(Dota$Players~Dota$Twitchviewers)
Gameresid = resid(GameModel)
adf.test(Gameresid)

ccfvalue = ccf(diff(twitch),diff(players))
ccfvalue

GamesExpanded <- cbind(player=Dota[,2],
                              x=xregExpander(Dota[,3],lags=c(-2:2),gaps="zero"))
colnames(GamesExpanded)[2:6] <- c("twitch","twitchLag1","twitchLag2","twitchLead1","twitchLead2")


#Different Model construction 
Autogamemodel = auto_ardl(Players~twitch,data = GamesExpanded,max_order = 3)
Autogamemodel$top_orders
d = Autogamemodel$best_model
summary(d)

GamesModels <- vector("list",8)
GamesModels[[1]] <- alm(players~twitch,GamesExpanded,orders=c(1,1,0))
                     
GamesModels[[2]] <- alm(players~twitch+twitchLag1,GamesExpanded,orders=c(1,1,1))
                           
GamesModels[[3]] <- alm(players~twitch+twitchLag1+twitchLag2,GamesExpanded,orders=c(1,1,2))
                            
GamesModels[[4]] <- alm(players~twitch+twitchLag1,GamesExpanded,orders=c(2,1,1))
                          
GamesModels[[5]] <- stepwise(GamesExpanded,orders=c(1,1,0))
GamesModels[[6]] <- stepwise(GamesExpanded,orders=c(2,1,0))
GamesModels[[7]] <- stepwise(GamesExpanded,orders=c(1,0,0))
GamesModels[[8]] <- alm(players~twitch+twitchLag1+twitchLag2,GamesExpanded,orders=c(2,1,2))
                       
names(GamesModels) <- c("ARIDL(1,1,0)","ARIDL(1,1,1)","ARIDL(1,1,2)","ARIDL(2,1,1)")
names(GamesModels)[5:8] <- c("ARIDL(1,1,q)","ARIDL(1,2,q)","ARIDL(1,0,0)","ARIDL(2,1,3)")
sapply(GamesModels,AICc)

summary(GamesModels[[5]])
i <- which.min(sapply(GamesModels,AICc))
summary(GamesModels[[i]])
par(mfcol=c(3,3))
plot(GamesModels[[i]],c(1,2,4,6,7,8,10,11,12))

#multipliers 
GameModel <- alm(players~twitch+twitchLag1,GamesExpanded,orders=c(1,1,2))
a0 <- coef(GamesModels[[i]])[2]
a1 <- coef(GamesModels[[i]])[3]
phi1 <- coef(GamesModels[[i]])[4]
cValues <- vector("numeric",13) 
cValues[1] <- a0
cValues[2] <- a1 + phi1 * cValues[1] 
for(i in 3:13){
  cValues[i] <- phi1 * cValues[i-1]
}
par(mfcol=c(1,1))
plot(c(0:12), cValues, type="l",
     ylab="Multipliers", xlab="Lags", main="Leading indicator effect", col="violet",lwd=4.0)
grid()
sum(cValues)

#VAR Model
Dota$twitch_log <- log(Dota$Twitchviewers)
Dota$twitch_log[is.infinite(Dota$twitch_log)] = NA
data <- Dota[, c("Players", "twitch_log")]
# Create a formula for the regression model
my_formula <- list(twitch_log~Players)
# Set the number of imputations to create
m <- 5
# Perform regression imputation
imp <- mice(data, method = "norm.predict", formula = my_formula, m = m)
imputed_data <- complete(imp)
Dota$twitch_log[is.na(Dota$twitch_log)] = imputed_data$twitch_log[is.na(Dota$twitch_log)]
VARselect(Dota[, c("Players", "twitch_log")], type = "both")
gameModelVAR2 <- VAR(Dota[, c("Players", "twitch_log")], p=2, type="both")
coef(gameModelVAR2)
lapply(gameModelVAR2$varresult, confint)
summary(gameModelVAR2)
layout(matrix(c(1,3,2,3),2,2))
plot(gameModelVAR2)
gameModelVAR2IRF <- irf(gameModelVAR2)
plot(gameModelVAR2IRF)

# var 2 nd model 
gameModelVAR2 <- VAR(Dota[, c("Players", "Twitchviewers")], p=2, type="both")
coef(gameModelVAR2)
lapply(gameModelVAR2$varresult, confint)
summary(gameModelVAR2)
layout(matrix(c(1,3,2,3),2,2))
plot(gameModelVAR2)
gameModelVAR2IRF <- irf(gameModelVAR2)
plot(gameModelVAR2IRF)

#question 2

#Players count
playerscount = ts(as.numeric(Dota_final$Players),start = c(2013,07),
                  end = c(2023,2),frequency = 26)

playerscumulative = ts(cumsum(Dota_final$Players),start = c(2013,07),
                       end = c(2023,2),frequency = 26)
playerscumulative <- head(playerscumulative, -4)

playersData = data.frame(Player=playerscount,PlayerCumulative=playerscumulative)
PlayerModel = alm(Player ~ PlayerCumulative + I(PlayerCumulative^2), playersData)
PlayerModelCoef = coef(PlayerModel)
marketsize = max(Re(polyroot(PlayerModelCoef)))
p = PlayerModelCoef[1]/marketsize
q = PlayerModelCoef[2]+p
PlayerFitted <- ts(fitted(PlayerModel),start = c(2013,07,04),
                   end = c(2023,2,16),frequency = 26)

                                           
PlayerInnovators <- p*(marketsize-playerscumulative)
PlayerImitators <- q*playerscumulative/marketsize*(marketsize-playerscumulative)
print(as.integer(PlayerImitators))
total_innovators <- sum(as.integer(PlayerInnovators))
total_imitators <- sum(as.integer(PlayerImitators))


plot(playerscount , type='l',xlab = "Year", ylab = "Players Count",main = "Innovators vs Immitators Trend (Bass Model)",ylim=c(1800,1200000),lwd=2.0)
lines(PlayerFitted,col="brown", lwd=2.0)
lines(PlayerInnovators,col="darkorchid", lwd=2.0)
lines(PlayerImitators,col="chartreuse3", lwd=2.0)
legend("topright", legend = c("Player Count", "Fitted","Innovators", "Imitators"),
       col = c("black","brown", "darkorchid", "chartreuse3"), lwd = 2, bty = "n")

#Question 4 

#Regression Imputation for missed player count
Dota_promo$lagged_players = lag(Dota_promo$Players,1)
# Create a new data frame with only the rows where Players is NA
na_df <- Dota_promo %>% filter(is.na(Players))
# Perform linear regression on the original data frame
model <- lm(Players ~ lagged_players , data = Dota_promo)
# Predict the NA values using the model
na_df$Players <- predict(model, newdata = na_df)
# Replace the NA values in Dota_promo with the imputed values
Dota_promo$Players[is.na(Dota_promo$Players)] <- na_df$Players
# Remove the lagged players column
Dota_promo$lagged_players <- NULL


data1 <- Dota_promo[, c("Players", "Twitchviewers")]
# Create a formula for the regression model
my_formula <- list(Twitchviewers~Players)
# Set the number of imputations to create
m <- 5
# Perform regression imputation
imp <- mice(data1, method = "norm.predict", formula = my_formula, m = m)
imputed_data <- complete(imp)
Dota_promo$Twitchviewers[is.na(Dota_promo$Twitchviewers)] = imputed_data$Twitchviewers[is.na(Dota_promo$Twitchviewers)]

#DataExploration of Promo 
adf.test(Dota_promo$Flags)
kpss.test(Dota_promo$Flags)
ccf(diff(Dota_promo$Players),Dota_promo$Flags)


layout(matrix(c(1,1),1,1))
plot(Dota_promo$Players, type = "l", xlab = "Year", ylab = "Players Count", main="Promotion on Dota2", lwd = 3.0, xaxt = "n")
axis(side = 1, at = seq(1, nrow(Dota_promo), by = 26), labels = format(as.Date(Dota_promo$Date[seq(1, nrow(Dota_promo), by = 26)], "%Y-%m-%d"), "%Y"))
abline(v = which(Dota_promo$Flags == 1), col = "orange", lwd = 2)

spread(Dota_promo)
#Players vs Twitch viewers 
#plot(Dota_promo$Players,Dota_promo$Flags)
modelPromo = alm(Players~.,data=Dota_promo,distribution = "dlnorm")
par(mfcol=c(3,2))
plot(modelPromo,c(1,2,4,6,7,12))
summary(modelPromo)
#Get the fitted values
fitted_values <- fitted(modelPromo)
# Print the fitted values
print(fitted_values)


# Now merge the variables and correct the name of the response variable:

PromoDataExpanded <- data.frame(player=Dota_promo$Players,as.data.frame(xregExpander(Dota_promo[,4], lags=c(-2:1), gaps="zero")))

modelPromoExpandedForward = vector("list",6)
modelPromoExpandedForward[[1]] <- stepwise(PromoDataExpanded,order=c(1,0,0))
modelPromoExpandedForward[[2]] <- stepwise(PromoDataExpanded,order=c(2,0,0))
modelPromoExpandedForward[[3]] <- stepwise(PromoDataExpanded,order=c(1,1,0))
modelPromoExpandedForward[[4]] <- stepwise(PromoDataExpanded,order=c(1,2,0))
modelPromoExpandedForward[[5]] <- stepwise(PromoDataExpanded,order=c(2,2,0))
modelPromoExpandedForward[[6]] <- stepwise(PromoDataExpanded,order=c(2,1,0))
names(modelPromoExpandedForward) <- c("Model(1,0,0)","Model(2,0,0)","Model(1,1,0)","Model(1,2,0)","Model(2,2,0)","Model(2,1,0)")
sapply(modelPromoExpandedForward,AICc)

summary(modelPromoExpandedForward[[4]])
i <- which.min(sapply(modelPromoExpandedForward,AICc))
summary(modelPromoExpandedForward[[i]])

modelFormula <- formula(modelPromoExpandedForward[[i]])
modelVars <- all.vars(modelPromoExpandedForward[[i]]) # remove the response variable name from the list
modelVars
#fitted_values <- fitted(modelPromoExpandedForward[[i]])
#print(fitted_values)
par(mfcol=c(2,3))
plot(modelPromoExpandedForward[[i]],c(1,2,4,6,7,12))

summary(modelPromoExpandedForward[[i]])
#LmCombine <- lmCombine(PromoDataExpanded, bruteforce=TRUE)
#summary(LmCombine)
a0 <- coef(modelPromoExpandedForward[[i]])[2]
a1 <- coef(modelPromoExpandedForward[[i]])[3]
phi1 <- coef(modelPromoExpandedForward[[i]])[4]
cValues <- vector("numeric",14) 
cValues[1] <- a0
cValues[2] <- a1 + phi1 * cValues[1] 
for(i in 3:14){
  cValues[i] <- phi1 * cValues[i-1]
}
cValues
par(mfcol=c(1,1))
plot(c(0:13), cValues, type="l",
     ylab="Multipliers", xlab="Lags", main="Leading indicator effect", col="violet",lwd=4.0)
grid()
sum(cValues)



#Question 3 
Dota_pricing = read_xlsx("Pricing.xlsx")

#Regression Imputation 

data = Dota_pricing[, c("Playerscount", "Microtransactions")]
# Create a formula for the regression model
my_formula <- list(Microtransactions~Playerscount)
# Set the number of imputations to create
m <- 5
# Perform regression imputation
imp <- mice(data, method = "norm.predict", formula = my_formula, m = m)
imputed_data <- round(complete(imp))
Dota_pricing$Microtransactions[is.na(Dota_pricing$Microtransactions)] = imputed_data$Microtransactions[is.na(Dota_pricing$Microtransactions)]
ccf(Dota_pricing$Microtransactions,Dota_pricing$Playerscount)
ccf(Dota_pricing$BattlePass,Dota_pricing$Playerscount)
#both players and twitch in one graph
ggplot(Dota_pricing, aes(x = Year)) +
  geom_line(aes(y = Playerscount, col = "Players")) +
  geom_line(aes(y = Microtransactions, col = "Microtransactions")) +
  geom_line(aes(y = BattlePass, col = "BattlePass"))+
  xlab("Years") +
  ylab("PlayersCount in Millions") +
  scale_color_manual(name = "Legend", 
                     values = c("Players" = "#F77919", "Microtransactions" = "#852E8B","BattlePass" = "#40EBB6"))+
  scale_x_continuous(breaks = Dota_pricing$Year, labels = Dota_pricing$Year)+
  ggtitle("Pricing Strategy Vs PlayerCount")

pricingmodel = alm(Playerscount~Microtransactions+BattlePass,Dota_pricing,order=c(1,1,0))
summary(pricingmodel)
par(mfcol=c(3,2))
plot(pricingmodel,c(1,2,4,6,7,12))
