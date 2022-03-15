
# Creating visualizations for manuscript/presentations. 
# Create Bland-Altman plots and scatterplot of predicted and measured.

library(readxl)
library(ggplot2)
library(dplyr)

df_test <- read_excel(here::here("../data/CVD_datasets.xlsx"), sheet = "Test")

###############################################################################
# Bland-Altman plots.
###############################################################################

###############################################################################
####### Start with the CVD Equation.
df_test <- mutate(df_test, avg_CVD_eq = (vo2_ml_kg_min + new_CRF_pred)/2)
df_test <- mutate(df_test, diff_CVD_eq = new_CRF_pred - vo2_ml_kg_min)

ba_CVD <- ggplot(df_test, aes(x = avg_CVD_eq, y = diff_CVD_eq)) +
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(alpha = 0.5, size = 3) +
  # mean and SD lines.
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T), color = "red", size = 2) +
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T) - (1.96 * sd(df_test$diff_CVD_eq, na.rm = T)), 
             color = "royalblue3", size = 2, linetype = "dashed") +
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T) + (1.96 * sd(df_test$diff_CVD_eq, na.rm = T)), 
             color = "royalblue3", size = 2, linetype = "dashed") +
  # axis labels.
  labs(x=expression(paste("Average Measure ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted - Measured (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # x- and y-axis size.
  coord_cartesian(xlim = c(0, 60), ylim = c(-25, 30)) +
  # graph label.
  labs(title = "CVD Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.9, size = 19, face = "bold")) +
  # print the mean.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)+2, label = "Mean", size = 6) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-2, 
           label = sprintf("%.1f", round(mean(df_test$diff_CVD_eq, na.rm = T), 3)), size = 6) +
  # print the positive SD values.
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+
             mean(df_test$diff_CVD_eq, na.rm = T)+2, label = "+1.96SD", size = 6) +
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+
             mean(df_test$diff_CVD_eq, na.rm = T)-2, 
           label = sprintf("%.1f", round((sd(df_test$diff_CVD_eq, na.rm = T))*1.96+mean(df_test$diff_CVD_eq, na.rm = T),1)), 
           size = 6) +
  # print the negative SD values.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-
             (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+2, label = "-1.96SD", size = 6) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-
             (sd(df_test$diff_CVD_eq, na.rm = T))*1.96-2, 
           label = sprintf("%.1f",round(mean(df_test$diff_CVD_eq, na.rm = T)-(sd(df_test$diff_CVD_eq, na.rm = T)*1.96),1)), 
           size = 6) +
  theme(text = element_text(size = 17))

###############################################################################
########### Healthy Equation
df_test <- mutate(df_test, avg_healthy_eq = (vo2_ml_kg_min + silva_CRF_pred)/2)
df_test <- mutate(df_test, diff_healthy_eq = silva_CRF_pred - vo2_ml_kg_min)

ba_healthy <- ggplot(df_test, aes(x = avg_healthy_eq, y = diff_healthy_eq)) +
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(alpha = 0.5, size = 3) +
  # mean and SD lines.
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T), color = "red", size = 2) +
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T) - (1.96 * sd(df_test$diff_healthy_eq, na.rm = T)), 
             color = "royalblue3", size = 2, linetype = "dashed") +
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T) + (1.96 * sd(df_test$diff_healthy_eq, na.rm = T)), 
             color = "royalblue3", size = 2, linetype = "dashed") +
  # axis labels.
  labs(x=expression(paste("Average Measure ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted - Measured (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # x- and y-axis size.
  coord_cartesian(xlim = c(0, 60), ylim = c(-25, 30)) +
  # graph label.
  labs(title = "Healthy Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.9, size = 19, face = "bold")) +
  # print the mean.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)+2, label = "Mean", size = 6) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-2, 
           label = sprintf("%.1f", round(mean(df_test$diff_healthy_eq, na.rm = T), 3)), size = 6) +
  # print the positive SD values.
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+
             mean(df_test$diff_healthy_eq, na.rm = T)+2, label = "+1.96SD", size = 6) +
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+
             mean(df_test$diff_healthy_eq, na.rm = T)-2, 
           label = sprintf("%.1f", round((sd(df_test$diff_healthy_eq, na.rm = T))*1.96+mean(df_test$diff_healthy_eq, na.rm = T),1)), 
           size = 6) +
  # print the negative SD values.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-
             (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+2, label = "-1.96SD", size = 6) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-
             (sd(df_test$diff_healthy_eq, na.rm = T))*1.96-2, 
           label = sprintf("%.1f",round(mean(df_test$diff_healthy_eq, na.rm = T)-(sd(df_test$diff_healthy_eq, na.rm = T)*1.96),1)), 
           size = 6) +
  theme(text = element_text(size = 17))

# Save as 1500x700.
gridExtra::grid.arrange(ba_healthy, ba_CVD, nrow = 1)



###############################################################################
# Regression fit.
###############################################################################

###############################################################################
##### Graph the CVD-specific Equation Prediction.

# Find differences between predicted and measured (for color coding graphs).
# METS difference.
df_test <- mutate(df_test, mets_off_CVD = ((new_CRF_pred - vo2_ml_kg_min)/3.5))
df_test <- mutate(df_test, mets_grp_off_CVD = ifelse(abs(mets_off_CVD)< .5, "<0.5",
                                                     ifelse(abs(mets_off_CVD)<1, "<1",
                                                            ifelse(abs(mets_off_CVD)<2, "<2", "≥2"))))
table(df_test$mets_grp_off_CVD)
label_mets_grp_off_CVD <- c(paste("<0.5 METs\nn=", sum(df_test$mets_grp_off_CVD == "<0.5"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<0.5")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""), 
                            paste("0.5-0.99 METs\nn=", sum(df_test$mets_grp_off_CVD == "<1"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<1")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("1-1.99 METs\nn=", sum(df_test$mets_grp_off_CVD == "<2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("≥2 METs\nn=", sum(df_test$mets_grp_off_CVD == "≥2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "≥2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""))

# Better to create the correlation statement to paste here and then call it in ggplot.
r_label_CVD <- paste("r = ", sprintf("%.2f", cor(df_test$new_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")))
rsq_label_CVD <- paste("R^2== ", sprintf("%.2f", cor(df_test$new_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")^2))

reg_CVD <- ggplot(df_test, aes(x = vo2_ml_kg_min, 
                               y = new_CRF_pred, color=mets_grp_off_CVD)) +
  geom_abline(intercept = 0, slope = 1, color = "grey85", size = 2) +
  scale_color_manual("Difference from\nMeasured in\nAbsolute METs *",
                     values = c("≥2"="red3", "<0.5"="darkgreen",
                                "<2"="chocolate2", "<1"="gold2"),
                     labels = label_mets_grp_off_CVD) +
  # Want an enclosed graph but without the gridlines
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(size = 3) +
  labs(x=expression(paste("Measured VO"["2peak"], " (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted VO"["2peak"], " (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # Rotate the new y-axis label so it reads vertically. 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  # Axis sizes.
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  # Graph title (centered and bold).
  labs(title = "CVD Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22)) +
  theme(legend.key = element_rect(color ="white")) +
  # Add in the correlation and sig values.
  annotate(geom = "text", x=5.8, y=50, label = r_label_CVD, size=6) +
  annotate(geom = "text", x=6.4, y=47, label = rsq_label_CVD, parse=T, size = 6) +
  annotate(geom = "text", x=6, y=44, label = "P < 0.05", size=6) + 
  # These are for adjusting text size and legend dot size (since the figure is huge).
  theme(text = element_text(size = 17)) +
  guides(colour = guide_legend(override.aes = list(size=7)))


###############################################################################
##### Graph the Healthy Equation Prediction
# METS difference.
df_test <- mutate(df_test, mets_off_healthy = ((silva_CRF_pred - vo2_ml_kg_min)/3.5))
df_test <- mutate(df_test, mets_grp_off_healthy = ifelse(abs(mets_off_healthy)< 0.5, "<0.5",
                                                         ifelse(abs(mets_off_healthy)<1, "<1",
                                                                ifelse(abs(mets_off_healthy)<2, "<2", "≥2"))))
table(df_test$mets_grp_off_healthy)
label_mets_grp_off_healthy <- c(paste("<0.5 METs\nn=", sum(df_test$mets_grp_off_healthy == "<0.5"), " (",
                                      sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<0.5")/nrow(df_test)*100), 
                                      "%)", "\n", sep=""), 
                                paste("0.5-0.99 METs\nn=", sum(df_test$mets_grp_off_healthy == "<1"), " (",
                                      sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<1")/nrow(df_test)*100), 
                                      "%)", "\n", sep=""),
                                paste("1-1.99 METs\nn=", sum(df_test$mets_grp_off_healthy == "<2"), " (",
                                      sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<2")/nrow(df_test)*100), 
                                      "%)", "\n", sep=""),
                                paste("≥2 METs\nn=", sum(df_test$mets_grp_off_healthy == "≥2"), " (",
                                      sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "≥2")/nrow(df_test)*100), 
                                      "%)", "\n", sep=""))


# Better to create the correlation statement to paste here and then call it in ggplot.
r_label_healthy <- paste("r = ", sprintf("%.2f", cor(df_test$silva_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")))
rsq_label_healthy <- paste("R^2== ", sprintf("%.2f", cor(df_test$silva_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")^2))

reg_healthy <- ggplot(df_test, aes(x = vo2_ml_kg_min, 
                                   y = silva_CRF_pred, color=mets_grp_off_healthy)) +
  geom_abline(intercept = 0, slope = 1, color = "grey85", size = 2) +
  scale_color_manual("Difference from\nMeasured in\nAbsolute METs",
                     values = c("≥2"="red3", "<0.5"="darkgreen",
                                "<2"="chocolate2", "<1"="gold2"),
                     labels = label_mets_grp_off_healthy) +
  # Want an enclosed graph but without the gridlines
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(size = 3) +
  labs(x=expression(paste("Measured VO"["2peak"], " (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted VO"["2peak"], " (ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # Rotate the new y-axis label so it reads vertically. 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  # Axis sizes.
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  # Graph title (centered and bold).
  labs(title = "Healthy Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22)) +
  theme(legend.key = element_rect(color ="white")) +
  # Add in the correlation and sig values.
  annotate(geom = "text", x=5.8, y=50, label = r_label_healthy, size=6) +
  annotate(geom = "text", x=6.4, y=47, label = rsq_label_healthy, parse=T, size = 6) +
  annotate(geom = "text", x=6, y=44, label = "P < 0.05", size=6) + 
  # These are for adjusting text size and legend dot size (since the figure is huge).
  theme(text = element_text(size = 17)) +
  guides(colour = guide_legend(override.aes = list(size=7)))

# Save as 1600x700
gridExtra::grid.arrange(reg_healthy, reg_CVD, nrow = 1)


###############################################################################
# Chi-square to compare distribution of those in the different MET groups
# (difference between measured and predicted CRF).
###############################################################################

chi_df <- rbind(data.frame(temp_var = df_test$mets_grp_off_healthy, group="Healthy"), 
                data.frame(temp_var = df_test$mets_grp_off_CVD, group="CVD"))
temp_tbl <- table(chi_df$group, chi_df$temp_var)
chisq.test(temp_tbl)




