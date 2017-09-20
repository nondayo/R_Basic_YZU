install.packages("plotly")
install.packages("tidyverse")
library(plotly)
library(tidyverse)
#loading file
nba <- read.csv(file = "NBA.csv")

#Heatmap
group_by(nba, Year) %>% 
  select(VORP) %>% 
  View()

heatmap <- sapply(c(1989:2008), function(yr){
  subset(nba[,c("VORP", "Year")], Year == yr)
  }
)
heatmap <- Reduce(x = heatmap, f = cbind)
heatmap <- heatmap[,seq(1,40, by = 2)] %>% as.data.frame()
for(i in ncol(heatmap)){
  heatmap[,i][is.na(heatmap[,i]) == TRUE] <- 0
}
names(heatmap) <- c(1989:2008)
plotly::plot_ly(x = c(1989:2008),
                z = as.matrix(heatmap),  
                type = "heatmap", 
                colors = "YlOrRd") %>% 
  layout(title = "Is there order in the draft?",
         xaxis = list(title = "Year",
                      tick0 = 1989,
                      dtick = 2),
         yaxis = list(title = "Draft Rank",
                      autorange = "reversed")) 
  
#Redraft vs. Rank
redraft_ls <- lapply(c(1989:2008), function(yr){
  df <- subset(nba, Year == yr) 
  df <- df[order(-df$VORP), ]
  df_redraft <- data.frame(VORP = df$VORP, 
                           Redraft = c(1:length(df$VORP)), 
                           Player = df$Player)
  df_redraft <- df_redraft[c("Player", "Redraft")]
  df_full <- left_join(df, df_redraft, by = "Player") %>% 
             arrange(Redraft) %>%
             as.data.frame(stringsAsFactors=FALSE)
  df_full
})
redraft <- Reduce(x = redraft_ls, f = rbind)

ggplot(data = redraft, aes(Redraft, Rk)) +
  scale_y_continuous(name = "Rk", limits = c(0,60)) +
  scale_x_continuous(name = "Redraft", limits = c(0,60)) +
  geom_abline(data = redraft, mapping = aes(slope = 1, intercept = 0)) +
  geom_point(position = "jitter", color = "orange")
  

#Player's Bust/Steal Value
BS = lapply(c(1989:2008), function(yr){
  df_yr <- subset(redraft, Year == yr)
  
  df_1 <- select(df_yr, c("Rk","VORP"))
  colnames(df_1) <- c("Rk", "VORP_actual")
  df_1 <- df_1[order(df_1$Rk), ]
  
  df_2 <- select(df_yr, c("Redraft","VORP"))
  colnames(df_2) <- c("Redraft", "VORP_expect")
  
  df <- cbind(df_1, df_2)
  df$bustSteal <- df$VORP_actual - df$VORP_expect
  df <- select(df, c("Rk", "bustSteal"))
  
  df_yr <- left_join(df_yr, df)
  
  df_yr
})
BustSteal <- Reduce(x = BS, f = rbind)
BustSteal <- filter(BustSteal, MP_T >3000) %>% 
             na.omit()

bestPlayer <- arrange(BustSteal, desc(bustSteal)) %>% 
              slice(1:20)

worstPlayer <- arrange(BustSteal, bustSteal) %>% 
               slice(1:20)

ggplot(data = bestPlayer, aes(reorder(Player, bustSteal), bustSteal)) +
  geom_col(color = "orange", fill = "orange") +
  ylab("Steal Value") +
  xlab("Player") +
  coord_flip() 

ggplot(data = worstPlayer, aes(reorder(Player, bustSteal), bustSteal)) +
  geom_col(color = "orange", fill = "orange") +
  ylab("Bust Value") +
  xlab("Player") +
  coord_flip() 

#What was the best draft class ever?
bestYear_all <- group_by(na.omit(redraft), Year) %>% 
                summarise(maenVORP = mean(VORP)) %>% 
                arrange(desc(maenVORP))
ggplot(data = bestYear_all, aes(reorder(Year, maenVORP), maenVORP)) +
  geom_col(color = "orange", fill = "orange") +
  ylab("maenVORP") +
  xlab("Year") +
  ggtitle("What was the best draft class ever?") +
  coord_flip()
  

top10 <- group_by(redraft, Year) %>% 
         arrange(desc(VORP)) %>% 
         slice(1:10)
bestYear_top10 <- group_by(na.omit(top10), Year) %>% 
  summarise(maenVORP = mean(VORP)) %>% 
  arrange(desc(maenVORP))
ggplot(data = bestYear_top10, aes(reorder(Year, maenVORP), maenVORP)) +
  geom_col(color = "orange", fill = "orange") +
  ylab("maenVORP") +
  xlab("Year") +
  ggtitle("What was the best draft class ever? Focus on top 10 players") +
  coord_flip()

top5 <- group_by(redraft, Year) %>% 
        arrange(desc(VORP)) %>% 
        slice(1:5)
bestYear_top5 <- group_by(na.omit(top5), Year) %>% 
  summarise(maenVORP = mean(VORP)) %>% 
  arrange(desc(maenVORP))
ggplot(data = bestYear_top5, aes(reorder(Year, maenVORP), maenVORP)) +
  geom_col(color = "orange", fill = "orange") +
  ylab("maenVORP") +
  xlab("Year") +
  ggtitle("What was the best draft class ever? Focus on top 5 players") +
  coord_flip()
