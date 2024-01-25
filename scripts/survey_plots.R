# ANOVA and TukeyHSD
# Before and after instruction
ai_data_aov <- aov(answer_number ~ before_after, ai_data)
summary(ai_data_aov)
ai_data_thsd <- TukeyHSD(ai_data_aov)
ai_data_thsd
# By subject
ai_data_subject_aov <- aov(answer_number ~ before_after+subject, ai_data)
summary(ai_data_subject_aov)
ai_data_subject_thsd <- TukeyHSD(ai_data_subject_aov)
ai_data_subject_thsd
# Mean and standard error
ai_data_stats <- ai_data %>% group_by(before_after, subject) %>% summarize(mean=mean(as.numeric(answer_number)), sd=sd(as.numeric(answer_number)), se=sd(as.numeric(answer_number)/sqrt(n())), n=n())
ai_data_stats_total <- ai_data %>% group_by(before_after) %>% summarize(mean=mean(as.numeric(answer_number)), sd=sd(as.numeric(answer_number)), se=sd(as.numeric(answer_number)/sqrt(n())), n=n())
ai_data_stats_total$subject <- "total"
ai_data_stats_all <- rbind(ai_data_stats, ai_data_stats_total)

# Top 3 media plot
plot_theme <- theme(legend.position = c(.7, .18), 
                    plot.tag.position = c(.10, .95),
                    plot.title = element_text(face="bold",size=24,hjust = 0.5),
                    plot.tag = element_text(face="bold",size=24,hjust = 0.5),
                    legend.key.size = unit(0.5, "cm"),
                    legend.title = element_text(face="bold",size=16),
                    legend.text=element_text(size=20),
                    axis.ticks = element_line(size = 1, colour = "black", linetype=1),
                    axis.text.x = element_text(face="bold", color="black",size=20, angle=0),
                    axis.title.x = element_blank(),
                    axis.line.y = element_line(linewidth = 1, colour = "black", linetype=1),
                    axis.text.y = element_text(color="black",size=16, angle=0),
                    axis.title.y = element_text(face="bold",color="black",size=18, angle=90))

ai_data_stats_all$before_after <- factor(ai_data_stats_all$before_after, levels = c("before", "after"), labels = c("Before", "After"))
ai_data_stats_all$subject <- factor(ai_data_stats_all$subject, levels = c("total", "BME", "CE", "CHBE", "CHEM", "CPBP", "CSET", "ECE", "ENGM", "ES"),
                          labels = c("Total", "BME", "CE", "CHBE", "CHEM", "CPBP", "CSET", "ECE", "ENGM", "ES"))
ai_data_stats_all_plot <- ggplot() + 
  geom_point(data = subset(ai_data_stats_all), 
             mapping = aes(x = before_after, y = mean, group = subject, color = subject, shape = subject),
             size = 6, position = "dodge", alpha = 0.3) + 
  geom_text_repel(data = subset(subset(ai_data_stats_all, before_after=="After")), 
      mapping = aes(x = before_after, y = mean, group = subject, color = subject, label = n), fontface = "bold", size= 5, hjust = -2) + 
  geom_line(data = subset(ai_data_stats_all), 
            mapping = aes(x = before_after, y = mean, group = subject, color = subject),
            linewidth = 1, linetype = "dashed", position = "dodge", alpha = 0.3) +
  geom_point(data = subset(ai_data_stats_all, subject == "Total"), 
             mapping = aes(x = before_after, y = mean),
             size = 6, color = "black", position = "dodge") + 
  geom_line(data = subset(ai_data_stats_all, subject == "Total"), 
            mapping = aes(x = before_after, y = mean, group = subject),
            linewidth = 1, linetype = "solid", position = "dodge", alpha = 1) +
  geom_errorbar(data = subset(ai_data_stats_all, subject == "Total"), 
                mapping = aes(x = before_after, ymin = mean - se, ymax = mean + se),
                linewidth = 1, width = 0.1, color = "black") + 
  geom_hline(yintercept=0, color = "black", linewidth=1) +
  labs(y = "Mean likelihood of using AI tools for research\n (1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Very Often)", 
       shape = "Subject", color = "Subject") + ggtitle("") + 
  scale_y_continuous(breaks=c(1,2,3,4,5)) +
  scale_shape_manual(values = c(16, 2, 3, 4, 5, 6, 7, 8, 9, 0)) + 
  scale_color_manual(values = c("Total"="black", "ENGM"="red", "ES"="blue", "ECE"="green", "CHBE"="darkgreen", "CE"="orange", "CPBP"="magenta","CSET"="purple", "CHEM"="coral","BME"="brown")) + 
  scale_fill_manual(values = c("Total"="black", "ENGM"="red", "ES"="blue", "ECE"="green", "CHBE"="darkgreen", "CE"="orange", "CPBP"="magenta","CSET"="purple", "CHEM"="coral","BME"="brown")) +
  ylim(1, 5) + theme_minimal() + guides(color = guide_legend(override.aes = list(alpha = 1))) + plot_theme
ggsave("plots/ai_data_stats_all_plot.jpg", plot = ai_data_stats_all_plot, scale = 1, width = 8, height = 10,
       dpi = 300,limitsize = TRUE)
  