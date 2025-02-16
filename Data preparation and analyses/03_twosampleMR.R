

# Title: Two-Sample MR for Returns to education using genetically informed designs
# Author: Tarjei Widding-Havneraas
# Date: 11.02.2025

setwd("C:/Users/tarjeiw/OneDrive - Universitetet i Oslo/Dokumenter/Artikler/MR income/MR R/Data")
# Install necessary packages
library(devtools)
library(knitr)
library(rmarkdown)
library(TwoSampleMR) 
library(data.table)
library(tidyverse)
# Import data
exposure_dat <- read.csv2("EA4_instruments.csv")
outcome_dat <- fread("C:/Users/tarjeiw/OneDrive - Universitetet i Oslo/Dokumenter/Artikler/MR income/MR R/Datav2/gwas_mr_income.fastGWAS")
outcome_dat <- read.csv2("C:/Users/tarjeiw/OneDrive - Universitetet i Oslo/Dokumenter/Artikler/MR income/MR R/Datav2/gwas_mr_income.csv", sep = ",")
head(outcome_dat)
head(exposure_dat)
# Format data
exp_dat <- format_data(exposure_dat,
                       type="exposure",
                       snp_col = "rsID",
                       beta_col = "BETA",
                       se_col = "SE",
                       eaf_col = "EAF",
                       effect_allele_col = "EA",
                       other_allele_col = "OA",
                       pval_col = "P",
                       samplesize_col = "N")
out_dat <- format_data(outcome_dat,
                       type="outcome",
                       snp_col = "SNP",
                       beta_col = "BETA",
                       se_col = "SE",
                       eaf_col = "AF1",
                       effect_allele_col = "A1",
                       other_allele_col = "A2",
                       pval_col = "P",
                       samplesize_col = "N")
head(exp_dat)
head(out_dat)
# Rescale to the standard deviation of years of schooling from Lee et al.
exp_dat$beta.exposure <- 3.9*exp_dat$beta.exposure
# Harmonize the exposure and outcome data
H_data <- harmonise_data(exp_dat, out_dat, action = 2)

# MR analysis - all estimators -------------------------------------------------
mr_results <- mr(H_data)
mr_results

# MR-Egger pleiotropy test -----------------------------------------------------
mr_pleiotropy_test(H_data)
-0.0008887687-(1.96*0.0004539497)
-0.0008887687+(1.96*0.0004539497)

# I-squared --------------------------------------------------------------------
install.packages("MendelianRandomization")
library(MendelianRandomization)
Isq(H_data$beta.exposure, H_data$se.exposure)


# Supplementary figures --------------------------------------------------------
# Fig S8 -----------------------------------------------------------------------

mr_data <- data.frame(
  method = c("IVW", "MR Egger", "Weighted median", "Weighted mode", "Simple mode"),
  b = c(0.04876925, 0.06475648, 0.05047412, 0.05536654, 0.05147944),
  se = c(0.002123071, 0.008434915, 0.002785946, 0.009764287, 0.010656730)
)
# Step 2: Calculate the 95% confidence intervals
mr_data <- mr_data %>%
  mutate(
    lower_ci = b - 1.96 * se,
    upper_ci = b + 1.96 * se
  )
# Step 3: Ensure the 'method' column is a factor with the specified order
mr_data$method <- factor(mr_data$method, 
                         levels = c("IVW", "MR Egger", "Weighted median", "Weighted mode","Simple mode"))

# Step 3: Create the ggplot
figs8 <- ggplot(mr_data, aes(x = method, y = b)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "grey10", 
                  shape = 21, fill = "grey10", stroke = 1, size = 0.8) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Coefficient Plot",
       x = "",
       y = "Estimate (b)",
       caption = "") +
  scale_y_continuous(limits = c(0, 0.08), breaks = seq(0, 0.08, by = 0.01)) +
  scale_x_discrete() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(5, "pt"),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(3, "pt")) +
  geom_text(aes(label = sprintf("%.3f", b)), vjust = -1)
# Coefficient plot
figs8 <- ggplot(mr_data, aes(x = fct_rev(method), y = b)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "grey10",
                  shape = 21, fill = "grey10", stroke = 1, size = 0.8) +
  theme_minimal() +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "Log(Earnings)",
       caption = "") +
  scale_y_continuous(limits = c(0, 0.12), breaks = seq(0, .12, by = 0.02)) +
  scale_x_discrete() +
  theme(legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(5, "pt"),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(3, "pt")) +
  geom_text(aes(label = sprintf("%.3f", b)), vjust = -1)
figs8+theme(axis.text.x = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())

# Fig S9  ----------------------------------------------------------------------
# Leave one out
loo_egger <- mr_leaveoneout(H_data, method = mr_egger_regression)
loo_plot <- mr_leaveoneout_plot(loo_egger)[[1]] + theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank(),  
        panel.grid.major.y = element_line(),   
        panel.grid.minor.y = element_line())   
print(loo_plot)

# Fig S10 ----------------------------------------------------------------------
figs10 <- mr_scatter_plot(mr_results, H_data) 
figs10[[1]] + 
  labs(x = "SNP effect on schooling", y = "SNP effect on earnings") +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank() 
  )

