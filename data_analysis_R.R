
library(readr)
library(ggplot2)
library(mosaicData)
games.data <- read_csv("C:/Users/ad256/OneDrive - University of Missouri/Classes_Mizzou/SPRING 2022/statistatical_software/datafest/data/DF challange rugby/games.csv")

head(games.data)

ggplot(data = games.data, mapping = aes(x = Opponent , y = TeamPoints,
                                        color = Opponent))+
 geom_point()+
 facet_wrap(~TournamentGame

)


library(dplyr)
outcome_counts = table(games.data$Outcome
) %>% print()

barplot(outcome_counts,
        main="Winning and Loosing Distribution", 
        xlab="Number of games")


# Stacked Bar Plot with Colors and Legend
outcome_opponent_counts <- table(games.data$Outcome, games.data$Opponent
) %>% print()


barplot(outcome_opponent_counts, 
        main="Game Distribution by opponent and outcome",
        xlab="opponents", 
        col=c("blue","red"),
        legend = rownames(outcome_opponent_counts))


# Stacked Bar Plot with Colors and Legend
outcome_Tournament_counts <- table(games.data$Outcome, games.data$Tournament

) %>% print()


barplot(outcome_opponent_counts, 
        main="Game Distribution by Outcome and Tournament",
        xlab="Tournament", 
        col=c("blue","red"),
        legend = rownames(outcome_opponent_counts))

# Stacked Bar Plot with Colors and Legend
outcome_TeamPoints_counts <- table(games.data$Outcome, games.data$TeamPoints
                                   
) %>% print()


barplot(outcome_TeamPoints_counts, 
        main="Game Distribution by Teampoints and outcome",
        xlab="Teampoints", 
        col=c("red","green"),
        legend = rownames(outcome_TeamPoints_counts))


##################
### Boxplot

boxplot(games.data$TeamPoints ~games.data$Outcome,
        main="Box plot visulization of outcome vs Teampoints", 
        xlab="Outcome", 
        ylab="Team points")



hist(games.data$TeamPoints, main = "Histogram of Teampoints count", xlab="Team points")

hist(games.data$TeamPointsAllowed

)


library(readr)
library(ggplot2)
library(mosaicData)
gps.data <- read_csv("C:/Users/ad256/OneDrive - University of Missouri/Classes_Mizzou/SPRING 2022/statistatical_software/datafest/data/DF challange rugby/gps.csv")

head(wellness.data)

# Game count 
game.count <- gps.data[,.(count = .N), by = GameID]

ggplot(data = game.count, aes(x = GameID, y = count)) + 
 geom_bar(stat = 'identity', fill = 'forestgreen') + 
 ggtitle('Number of Data points per game') + ylab('Count of games') + xlab('Game number') + 
 theme_classic() 
 
 
 
 

library(readr)
library(ggplot2)
library(mosaicData)
rpe.data <- read_csv("C:/Users/ad256/OneDrive - University of Missouri/Classes_Mizzou/SPRING 2022/statistatical_software/datafest/data/DF challange rugby/rpe.csv")

head(rpe.data)

ggplot(data = rpe.data, mapping = aes(x = FocusRating

 , y = AcuteChronicRatio,
                                      ))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Daily Load and Acute Chronic Ratio if they are giving their best",
) 

ggplot(data = rpe.data, mapping = aes(x = Duration
                                      , y = AcuteChronicRatio,
                                      color = PlayerID))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Duration and Acute Chronic Ratio",
 )



ggplot(data = rpe.data, mapping = aes(x = DailyLoad , y = AcuteChronicRatio ))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Daily Load and Acute Chronic Ratio",
 )



ggplot(data = rpe.data, mapping = aes(x = DailyLoad , y = AcuteChronicRatio,
))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Daily Load and Acute Chronic Ratio in different Session Types",
 ) +
 facet_wrap(~SessionType)


ggplot(data = rpe.data, mapping = aes(x = DailyLoad , y = AcuteChronicRatio,
))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Daily Load and Acute Chronic Ratio in different Session Types",
 ) +
 facet_wrap(~SessionType)
            
            

            


library(dplyr)
outcome_counts = table(rpe.data$PlayerID
) %>% print()

library(ggplot2)
library(ggpie)
library(vcd)
library(dplyr)
library(treemapify)
library(scales)


##################
# Simple Tree Map
library(ggplot2)
library(dplyr)
library(treemapify)


AcuteChronicRatio_more_than_1.2 <- rpe.data[rpe.data$AcuteChronicRatio  > 1.2, ]
AcuteChronicRatio_more_than_1.2
ggplot(AcuteChronicRatio_more_than_1.2, aes(x=RPE
)) + 
 geom_histogram() +
 labs(title="Default histogram")

##################
### Boxplot

boxplot(rpe.data$AcuteChronicRatio  ~rpe.data$SessionType,
        main="Box plot visulization of SessionType vs Acute Chronic Ratio",
        xlab="SessionType", 
        ylab="Acute Chronic Ratio"
)


##################
# Violin plots

AcuteChronicRatio_more_than_1.2 <- rpe.data[rpe.data$AcuteChronicRatio  > 1.2, ]

rpe.data$SessionLoad <- factor(rpe.data$SessionLoad)



ggplot(AcuteChronicRatio_more_than_1.2, aes(x=SessionLoad, y=AcuteChronicRatio)) + 
 geom_boxplot(width=0.2, 
              fill="green") +
 geom_violin(fill="gold", 
             alpha=0.3) +
 labs(x="SessionLoad", 
      y="AcuteChronicRatio", 
      title="Violin Plots of AcuteChronicRatio and SessionLoad ")





library(readr)
library(ggplot2)
library(mosaicData)
wellness.data <- read_csv("C:/Users/ad256/OneDrive - University of Missouri/Classes_Mizzou/SPRING 2022/statistatical_software/datafest/data/DF challange rugby/wellness.csv")

head(wellness.data)


ggplot(data = wellness.data, mapping = aes(x = SleepHours
                                           
                                           
                                           , y = Fatigue
                                           ,
))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between SleepHours and Fatigue") 




ggplot(data = wellness.data, mapping = aes(x = Soreness
                                      , y = Fatigue
,
))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between Soreness and Fatigue") 


library(dplyr)
outcome_counts = table(wellness.data$SleepHours) %>% print()


ggplot(data = wellness.data, mapping = aes(x = SleepHours


                                           , y = SleepQuality
 ,
))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between SleepHours
 and SleepQuality") 


library(corrplot)
library(RColorBrewer)
M <-cor(wellness.data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


pairs(~Fatigue
+Soreness
+Desire
+Irritability
+USG
,data=wellness.data, main="Scatter Plot Matrix")

library(GGally)

install.packages("GGally")


ggscatmat(wellness.data)
wellness.data_corr = wellness.data[c("Fatigue", "Soreness", "Desire", "Irritability", "SleepHours", "SleepQuality", "MonitoringScore", "USG") ]
wellness.data_corr
cor(wellness.data_corr)


ggplot(data = wellness.data, mapping = aes(x = SleepHours
                                           , y = Fatigue,))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between SleepHours and Fatigue in different Menstruation periods") +  facet_wrap(~Menstruation
 )




ggplot(data = wellness.data, mapping = aes(x = SleepHours
                                           , y = Fatigue,))+
 geom_point()+
 geom_smooth(method = "lm", se = FALSE) +
 labs(title = "Relationship between SleepHours and Fatigue in different Nutrition Adjustment") +  facet_wrap(~NutritionAdjustment)


ggplot(wellness.data, aes(x=SleepHours, y=SleepHours, group =Menstruation )) + 
# geom_boxplot(width=0.2, 
 #             fill="green") +
 geom_violin(fill="gold", 
             alpha=0.3) +
 labs(      title="Violin Plots of AcuteChronicRatio and SessionLoad ")
      
      
library(corrplot)
library(RColorBrewer)
M <-cor(wellness.data_corr)
corrplot(M, type="upper", 
         col=brewer.pal(n=8, name="RdYlBu"))

library(tidyverse)
install.packages("babynames")
library(hrbrthemes)
library(babynames)
library(viridis)


 ggplot(data = wellness.data, aes(x=SleepHours, y=SleepHours, group=Menstruation, fill=Menstruation)) +
 geom_area() +
 scale_fill_viridis(discrete = TRUE) +
 theme(legend.position="none") +
 ggtitle("...") +
 theme_ipsum() +

 facet_wrap(~Menstruation)

 
 
 
 #finding linear model
 wellness.data_corr
 
 wellness.data_lm = wellness.data[c("Fatigue", "PlayerID", "Soreness", "Desire", "Irritability", "SleepHours", "SleepQuality", "MonitoringScore", "USG") ]
 
 
 
 full_model=lm(Fatigue~PlayerID+Soreness+Desire+Irritability+SleepHours+SleepQuality+MonitoringScore+USG, data=wellness.data_lm)
 summary(full_model)
 anova(full_model)
 
 
 install.packages("olsrr")
 library(olsrr)
 ols_vif_tol(full_model)
 
 
 ols_step_forward_p(full_model)
 ols_step_backward_p(full_model)
 ols_step_both_p(full_model)
 
 plot(full_model)
 resid(full_model)
 res <- resid(full_model)
 plot(fitted(full_model), res)
 
 
 