################
## limitation ##
################
# limitations column clean up and sentiment analysis
# Lower the case of all letter
ai_open_responses$limitations_cleaned <- tolower(ai_open_responses$limitations)
# Remove stop words using the tm package
# First, create a corpus from the text column
corpus <- Corpus(VectorSource(ai_open_responses$limitations_cleaned))
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# Then, remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Convert the corpus back to a data frame
ai_open_responses$limitations_cleaned <- sapply(corpus, as.character)
# Analyze the sentiment of limitations
sentimentScores <- analyzeSentiment(ai_open_responses$limitations_cleaned)
ai_open_responses$limitations_sentiment_score <- sentimentScores$SentimentGI
ai_open_responses$limitations_sentiment_score[is.nan(ai_open_responses$limitations_sentiment_score)] <- 0

##########
## uses ##
##########
# uses column clean up and sentiment analysis
# Lower the case of all letter
ai_open_responses$uses_cleaned <- tolower(ai_open_responses$uses)
# Remove stop words using the tm package
# First, create a corpus from the text column
corpus <- Corpus(VectorSource(ai_open_responses$uses_cleaned))
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# Then, remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Convert the corpus back to a data frame
ai_open_responses$uses_cleaned <- sapply(corpus, as.character)
# Analyze the sentiment of uses
sentimentScores <- analyzeSentiment(ai_open_responses$uses_cleaned)
ai_open_responses$uses_sentiment_score <- sentimentScores$SentimentGI
ai_open_responses$uses_sentiment_score[is.nan(ai_open_responses$uses_sentiment_score)] <- 0

#########
# stats #
#########
# Linear model
limitations_uses <- lm(uses_sentiment_score ~ limitations_sentiment_score, data=ai_open_responses); summary(limitations_uses)
before_limitations <- lm(before_answer_number ~ limitations_sentiment_score, data=ai_open_responses); summary(before_limitations)
after_limitations <- lm(after_answer_number ~ limitations_sentiment_score, data=ai_open_responses); summary(after_limitations)
before_uses <- lm(before_answer_number ~ uses_sentiment_score, data=ai_open_responses); summary(before_uses)
after_uses <- lm(after_answer_number ~ uses_sentiment_score, data=ai_open_responses); summary(after_uses)
# ANOVA
anova_limitations_data <- na.omit(subset(ai_open_responses_limitations_long, select=c(before_answer_number, after_answer_number, themes_limitations)))
anova_limitations_long_data <- pivot_longer(anova_limitations_data, before_answer_number:after_answer_number, names_to = "before_after", values_to = "answer_number")
anova_limitations_long_data$themes_limitations <- as.factor(anova_limitations_long_data$themes_limitations)
anova_limitations_long_data$before_after <- as.factor(anova_limitations_long_data$before_after)
anova_limitations_long_data$answer_number <- as.numeric(anova_limitations_long_data$answer_number)
anova_uses_data <- na.omit(subset(ai_open_responses_uses_long, select=c(before_answer_number, after_answer_number, themes_uses)))
anova_uses_long_data <- pivot_longer(anova_uses_data, before_answer_number:after_answer_number, names_to = "before_after", values_to = "answer_number")
anova_uses_long_data$themes_uses <- as.factor(anova_uses_long_data$themes_uses)
anova_uses_long_data$before_after <- as.factor(anova_uses_long_data$before_after)
anova_uses_long_data$answer_number <- as.numeric(anova_uses_long_data$answer_number)
# ANOVA with before after differences
limitations_aov <- aov(answer_number ~ themes_limitations*before_after, data=anova_limitations_long_data); summary(limitations_aov); limitations_thsd <- TukeyHSD(limitations_aov)
uses_aov <- aov(answer_number ~ themes_uses*before_after, data=anova_uses_long_data); summary(uses_aov); uses_thsd <- TukeyHSD(uses_aov)

#####################
## Clean sentiment ##
#####################
# Clean plot data
# Sentiment data 
ai_open_responses_limitations_long <- ai_open_responses %>% pivot_longer(themes_limitations_1:themes_limitations_3, names_to = "themes_limitations_num", values_to = "themes_limitations")
ai_open_responses_limitations_sentiment_stats <- na.omit(subset(ai_open_responses_limitations_long, select = c(themes_limitations, limitations_sentiment_score))) %>% group_by(themes_limitations) %>% summarize(sentiment_mean = mean(limitations_sentiment_score), sentiment_sd = sd(limitations_sentiment_score), n=n(), sentiment_se = sd(limitations_sentiment_score)/sqrt(n()))
ai_open_responses_uses_long <- ai_open_responses %>% pivot_longer(themes_uses_1:themes_uses_3, names_to = "themes_uses_num", values_to = "themes_uses")
ai_open_responses_uses_sentiment_stats <- na.omit(subset(ai_open_responses_uses_long, select = c(themes_uses, uses_sentiment_score))) %>% group_by(themes_uses) %>% summarize(sentiment_mean = mean(uses_sentiment_score), sentiment_sd = sd(uses_sentiment_score), n=n(), sentiment_se = sd(uses_sentiment_score)/sqrt(n()))
# Answer number vs category
answer_limitations_long <- na.omit(subset(ai_open_responses_limitations_long, select = c(before_answer_number, after_answer_number, themes_limitations)))
answer_limitations_stats <- answer_limitations_long %>% group_by(themes_limitations) %>% summarize(before_answer_mean = mean(before_answer_number), before_answer_sd = sd(before_answer_number), n=n(), before_answer_se = sd(before_answer_number)/sqrt(n()),
                                                                                                                               after_answer_mean = mean(after_answer_number), after_answer_sd = sd(after_answer_number), n=n(), after_answer_se = sd(after_answer_number)/sqrt(n()))
answer_uses_long <- na.omit(subset(ai_open_responses_uses_long, select = c(before_answer_number, after_answer_number, themes_uses)))
answer_uses_stats <- answer_uses_long %>% group_by(themes_uses) %>% summarize(before_answer_mean = mean(before_answer_number), before_answer_sd = sd(before_answer_number), n=n(), before_answer_se = sd(before_answer_number)/sqrt(n()),
                                                                                                       after_answer_mean = mean(after_answer_number), after_answer_sd = sd(after_answer_number), n=n(), after_answer_se = sd(after_answer_number)/sqrt(n()))
anova_limitations_long_stats <- anova_limitations_long_data %>% group_by(themes_limitations) %>% summarize(answer_mean = mean(answer_number), answer_sd = sd(answer_number), n=n(), answer_se = sd(answer_number)/sqrt(n()))

######################
## Clean proportion ##
######################
n_total <- nrow(na.omit(subset(ai_open_responses_limitations_long, select = c(themes_limitations_num, themes_limitations))))
proportion_limitations <- na.omit(subset(ai_open_responses_limitations_long, select = c(themes_limitations_num, themes_limitations))) %>% group_by(themes_limitations) %>% summarize(count_lim = n(), proportion = n()/n_total)
proportion_limitations$themes <- "Themes"
n_total2 <- nrow(na.omit(subset(ai_open_responses_uses_long, select = c(themes_uses_num, themes_uses))))
proportion_uses <- na.omit(subset(ai_open_responses_uses_long, select = c(themes_uses_num, themes_uses))) %>% group_by(themes_uses) %>% summarize(count_lim = n(), proportion = n()/n_total2)
proportion_uses$themes <- "Themes"

# Clean data for proportion plots
ai_open_responses_limitations_sentiment_stats <- na.omit(subset(ai_open_responses_limitations_long, select = c(themes_limitations, limitations_sentiment_score))) %>% group_by(themes_limitations) %>% summarize(sentiment_mean = mean(limitations_sentiment_score), sentiment_sd = sd(limitations_sentiment_score), n=n(), sentiment_se = sd(limitations_sentiment_score)/sqrt(n()))

# Sentiment plots
plot_theme_limitations <- theme(legend.position = "none",
                    plot.title = element_text(face="bold",size=24,hjust = 0.5),
                    plot.tag = element_text(face="bold",size=24,hjust = 0.5),
                    legend.key.size = unit(0.5, "cm"),
                    legend.title = element_text(face="bold",size=16),
                    legend.text=element_text(size=20),
                    axis.ticks = element_line(size = 1, colour = "black", linetype=1),
                    axis.text.x = element_text(face="bold", color="black",size=20, angle=0),
                    axis.title.x = element_text(face="bold",color="black",size=18),
                    axis.line.y = element_line(linewidth = 1, colour = "black", linetype=1),
                    axis.text.y = element_text(color="black",size=16, angle=0),
                    axis.title.y = element_text(face="bold",color="black",size=18, angle=90))

limitations_sentiment_stats_plot <- ggplot(subset(ai_open_responses_limitations_sentiment_stats, themes_limitations!="other tools address ChatGPT's limitations"), 
                                           aes(x=sentiment_mean, xmin=sentiment_mean-sentiment_se, xmax=sentiment_mean+sentiment_se,
                                               y=themes_limitations, fill=themes_limitations)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "Sentiment score", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4)) + coord_cartesian(xlim=c(0, 0.4)) +
  theme_minimal() + plot_theme_limitations 
ggsave("plots/limitations_sentiment_stats_plot.jpg", plot = limitations_sentiment_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
limitations_answer_stats_plot <- ggplot(subset(anova_limitations_long_stats, themes_limitations!="other tools address ChatGPT's limitations"), 
                                               aes(x=answer_mean, xmin=answer_mean-answer_se, xmax=answer_mean+answer_se,
                                                   y=themes_limitations, fill=themes_limitations)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "Answer mean", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + coord_cartesian(xlim=c(0, 5)) +
  theme_minimal() + plot_theme_limitations 
ggsave("plots/limitations_answer_stats_plot.jpg", plot = limitations_answer_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
limitations_before_answer_stats_plot <- ggplot(subset(answer_limitations_stats, themes_limitations!="other tools address ChatGPT's limitations"), 
                                           aes(x=before_answer_mean, xmin=before_answer_mean-before_answer_se, xmax=before_answer_mean+before_answer_se,
                                               y=themes_limitations, fill=themes_limitations)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "Before answer mean", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + coord_cartesian(xlim=c(0, 5)) +
  theme_minimal() + plot_theme_limitations 
ggsave("plots/limitations_before_answer_stats_plot.jpg", plot = limitations_before_answer_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
limitations_after_answer_stats_plot <- ggplot(subset(answer_limitations_stats, themes_limitations!="other tools address ChatGPT's limitations"), 
                                               aes(x=after_answer_mean, xmin=after_answer_mean-after_answer_se, xmax=after_answer_mean+after_answer_se,
                                                   y=themes_limitations, fill=themes_limitations)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "After answer mean", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + coord_cartesian(xlim=c(0, 5)) +
  theme_minimal() + plot_theme_limitations 
ggsave("plots/limitations_after_answer_stats_plot.jpg", plot = limitations_after_answer_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)

plot_theme_uses <- theme(legend.position = "none", 
                                plot.title = element_text(face="bold",size=24,hjust = 0.5),
                                plot.tag = element_text(face="bold",size=24,hjust = 0.5),
                                legend.key.size = unit(0.5, "cm"),
                                legend.title = element_text(face="bold",size=16),
                                legend.text=element_text(size=20),
                                axis.ticks = element_line(size = 1, colour = "black", linetype=1),
                                axis.text.x = element_text(face="bold", color="black",size=20, angle=0),
                                axis.title.x = element_text(face="bold",color="black",size=18),
                                axis.line.y = element_line(linewidth = 1, colour = "black", linetype=1),
                                axis.text.y = element_text(color="black",size=16, angle=0),
                                axis.title.y = element_text(face="bold",color="black",size=18, angle=90))

uses_sentiment_stats_plot <- ggplot(subset(ai_open_responses_uses_sentiment_stats, themes_uses!="other tools address ChatGPT's uses"), 
                                           aes(x=sentiment_mean, xmin=sentiment_mean-sentiment_se, xmax=sentiment_mean+sentiment_se,
                                               y=themes_uses, fill=themes_uses)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "Sentiment score", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4)) + coord_cartesian(xlim=c(0, 0.4)) + 
  theme_minimal() + plot_theme_uses 
ggsave("plots/uses_sentiment_stats_plot.jpg", plot = uses_sentiment_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
uses_before_answer_stats_plot <- ggplot(subset(answer_uses_stats, themes_uses!="other tools address ChatGPT's uses"), 
                                               aes(x=before_answer_mean, xmin=before_answer_mean-before_answer_se, xmax=before_answer_mean+before_answer_se,
                                                   y=themes_uses, fill=themes_uses)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "Before answer mean", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + coord_cartesian(xlim=c(0, 5)) +
  theme_minimal() + plot_theme_uses 
ggsave("plots/uses_before_answer_stats_plot.jpg", plot = uses_before_answer_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
uses_after_answer_stats_plot <- ggplot(subset(answer_uses_stats, themes_uses!="other tools address ChatGPT's uses"), 
                                              aes(x=after_answer_mean, xmin=after_answer_mean-after_answer_se, xmax=after_answer_mean+after_answer_se,
                                                  y=themes_uses, fill=themes_uses)) + 
  geom_bar(stat = "identity", size = 6, position = "dodge") + 
  geom_errorbar(linewidth = 1, width = 0.1, color = "black") + 
  labs(x = "After answer mean", y = "Themes", fill = "Themes") + ggtitle("") + 
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) + coord_cartesian(xlim=c(0, 5)) +
  theme_minimal() + plot_theme_uses 
ggsave("plots/uses_after_answer_stats_plot.jpg", plot = uses_after_answer_stats_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)

# Proportion plots
plot_theme_proportion <- theme(legend.position = "right",
                                plot.title = element_text(face="bold",size=24,hjust = 0.5),
                                plot.tag = element_text(face="bold",size=24,hjust = 0.5),
                                legend.key.size = unit(0.5, "cm"),
                                legend.title = element_text(face="bold",size=16),
                                legend.text=element_text(size=20),
                                axis.ticks = element_line(size = 1, colour = "black", linetype=1),
                                axis.text.x = element_text(face="bold", color="black",size=20, angle=0),
                                axis.title.x = element_text(face="bold",color="black",size=18),
                                axis.line.y = element_line(linewidth = 1, colour = "black", linetype=1),
                                axis.text.y = element_text(color="black",size=16, angle=0),
                                axis.title.y = element_text(face="bold",color="black",size=18, angle=90))

proportion_limitations_plot <- ggplot(proportion_limitations, aes(x = themes, y=proportion, fill=themes_limitations)) + 
  geom_bar(stat = "identity", size = 6, position = "stack") + 
  labs(x = "", y = "Proportion", fill = "Themes") + ggtitle("") + 
  scale_y_continuous() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal() + plot_theme_proportion
ggsave("plots/proportion_limitations_plot.jpg", plot = proportion_limitations_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
proportion_uses_plot <- ggplot(proportion_uses, aes(x = themes, y=proportion, fill=themes_uses)) + 
  geom_bar(stat = "identity", size = 6, position = "stack") + 
  labs(x = "", y = "Proportion", fill = "Themes") + ggtitle("") + 
  scale_y_continuous() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_minimal() + plot_theme_proportion
ggsave("plots/proportion_uses_plot.jpg", plot = proportion_uses_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
