

# Figures for Estimating returns to education using genetically informed designs
# AU: Tarjei Widding-Havneraas

# Libraries
library(haven)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggridges)
library(summarytools)
library(binsreg)
library(patchwork)
library(forcats)
library("cowplot")

# Fig 1A -----------------------------------------------------------------------

# Load data
dat <- read_dta("N:/durable/projects/EQOP/MR_Income/temp/full_population.dta")
# Convert variables
dat %>%
  mutate(
    male = as.factor(male),
    birthorder = as.factor(birthorder),
    bmonth = as.factor(bmonth),
    kids = as.factor(kids),
    #famid = as.factor(famid),
    faar = as.factor(faar),
    earnings = as.numeric(earnings),
    yos = as.factor(yos)
  )
dat$yos <- as.integer(haven::zap_labels(dat$yos))
dat$earnings_1k <- dat$earnings/1000
yos<-dat$yos
yos_labels <- c(
  "9" = "LSS (20.6%)",
  "11" = "Some HS (7.0%)",
  "12" = "Complete HS (31.9%)",
  "13" = "Extra HS (3.9%)",
  "16" = "BA (27.7%)",
  "18" = "MA (8.6%)",
  "23" = "PhD (0.4%)"
)
dat<-dat %>%
  drop_na(earnings_1k, yos) %>%
  mutate(yos = factor(yos, 
                      levels = c(23, 18, 16, 13, 12, 11, 9), 
                      labels = yos_labels[as.character(c(23, 18, 16, 13, 12, 11, 9))]))
class(dat$yos)
# Median
medians<- medians <- dat %>%
  group_by(yos) %>%
  summarize(median_income = median(earnings_1k))
medians
# Add median
fig1a_bw <- ggplot(dat, aes(x = earnings_1k, y = yos, group=interaction(male,yos), fill = factor(male)), alpha = 0.7) +
  geom_density_ridges(alpha = 0.7, scale = 1, aes(color = factor(male)), 
                      quantile_lines = TRUE, 
                      quantile_fun = function(x, ...) median(x),  # For mean
                      # quantile_fun = function(x, ...) median(x),  # For median (uncomment this line if you prefer median)
                      rel_min_height = 0.01) +
  scale_fill_manual(values = c("grey25", "grey70"), name = "Sex", labels = c("Women", "Men")) +
  scale_color_manual(values = c("grey10", "grey10"), name = "Sex", labels = c("Women", "Men")) +
  scale_x_continuous(breaks = seq(0, 2000, by = 250)) + 
  theme_ridges() +
  guides(fill = guide_legend(override.aes = list(labels = c("Men", "Women"))),  
         color = guide_legend(override.aes = list(labels = c("Men", "Women"))))+
  labs(title="A", fill="Sex",color="Sex", 
       x = "Average earnings (in 1000 NOK)", 
       y = "") + 
  theme(legend.position = "bottom",
        legend.box = "horizontal",  # Center the legend horizontally
        legend.direction = "horizontal",  # Arrange legend items horizontally
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))+
  guides(fill = guide_legend(ncol = 2), 
         color = guide_legend(ncol = 2)) 
fig1a_bw <- fig1a_bw + guides(fill = guide_legend(ncol = 1)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
fig1a_bw <- fig1a_bw + guides(fill = guide_legend(ncol = 1)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
fig1a_bw


# Fig 1B -----------------------------------------------------------------------
# Load data
lifecycle <- read_dta("N:/durable/projects/EQOP/MR_Income/temp/income_by_age_and_college.dta")
lifecycle %>%
group_by(college) %>%
summarize(income_1k = mean(income_1k))

line_data <- data.frame(
    yintercept = c(601,505),
    label = c("College graduates","Less than college")
  )
p<-ggplot(lifecycle, aes(x = age, y = income_1k, color = factor(college))) +
  stat_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = 1.3) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, 150)) +
  scale_x_continuous(limits = c(17, 62), breaks = seq(17, 62, 5)) +
  geom_hline(data = line_data, aes(yintercept = yintercept, linetype = label), color = "black", show.legend = TRUE, linewidth = .75) +
  scale_color_manual(
    values = c("1" = "black", "0" = "darkgrey"),
    labels = c("1" = "College graduates", "0" = "Less than college"),
    breaks = c("1", "0")
  ) +
  scale_linetype_manual(
    values = c("College graduates" = "34", "Less than college" = "22")
  ) +
  labs(
    title = "B",  
    y = expression(atop("Age-earnings profile by education", "(in 1000 NOK)")),
    x = "Age",
    color = "Age-earnings profile",
    linetype = "Average lifetime earnings"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2, ncol = 1)) +  
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
p

# Combine to Figure 1 A and B
figure1_bw <- fig1a_bw / p
figure1_bw
figure1_bw <- plot_grid(
  fig1a_bw, p,
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(1, 1),
  scale = 1,
  vjust = 0  # adjust vertical justification
)
figure1_bw

# Fig 2A: Log ------------------------------------------------------

    groups <- c("OLS", "Sibling model", "Twin model (DZ)", 
                "Twin model (MZ)", "MR", "Sibling MR")
    estimate <- c(0.0575,    # OLS
                  0.0515,    # sibling-regression
                  0.0423,    # Twin model (DZ)
                  0.0319,    # Twin model (MZ)
                  0.0773,    # MR
                  0.0608)    # sibling MR
    
    lower_ci <- c(0.0561,    # OLS
                  0.0508,    # sibling-regression
                  0.0324,    # Twin model (DZ)
                  0.0185,    # Twin model (MZ)
                  0.0693,    # MR
                  0.0179)    # sibling MR
    
    upper_ci <- c(0.0589,    # OLS
                  0.0522,    # sibling-regression
                  0.0522,    # Twin model (DZ)
                  0.0452,    # Twin model (MZ)
                  0.0854,    # MR
                  0.1037)    # sibling MR
    # Reorder levels of groups factor
    coefdata <- data.frame(groups, estimate, lower_ci, upper_ci)
    # Convert groups to a factor with specified levels
    coefdata$groups <- factor(coefdata$groups, levels = groups)
    # Coefficient plot
    fig2 <- ggplot(coefdata, aes(x = fct_rev(groups), y = estimate)) +
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
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks.y = element_line(color = "black"),
            axis.ticks.length.y = unit(5, "pt"),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_line(color = "black"),
            axis.ticks.length.x = unit(3, "pt")) +
      geom_text(aes(label = sprintf("%.3f", estimate)), vjust = -1)
    fig2
    
    fig2 +
      theme(axis.text.x = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())  
 
    
  # Fig 2B: NOK ----------------------------------------------------------------
    
    groups2 <- c("OLS", "Sibling model", "Twin model (DZ)",
                 "Twin model (MZ)", "MR", "Sibling MR")
    estimate2 <- c(34617.58,    # OLS
                   30824.83,    # sibling-regression
                   26194.33,    # Twin model (DZ)
                   19196.15,    # Twin model (MZ)
                   53622.05,    # MR
                   45559.85)    # sibling MR
    lower_ci2 <- c(34087.22,    # OLS
                   30510.66,    # sibling-regression
                   19509.85,    # Twin model (DZ)
                   11112.61,    # Twin model (MZ)
                   47875.39,    # MR
                   14629.25)    # sibling MR
    upper_ci2 <- c(35147.93,    # OLS
                   31139.00,    # sibling-regression
                   32878.80,    # Twin model (DZ)
                   27279.69,    # Twin model (MZ)
                   59368.70,    # MR
                   76490.44)    # sibling MR
     estimate2k<-estimate2/1000
     lower_ci2k<-lower_ci2/1000
     upper_ci2k<-upper_ci2/1000
    
    # Reorder levels of groups factor
    coefdata2 <- data.frame(groups2, estimate2k, lower_ci2k, upper_ci2k)
    # Convert groups to a factor with specified levels
    coefdata2$groups2 <- factor(coefdata2$groups2, levels = groups2)
    # Coefficient plot
    fig2b <- ggplot(coefdata2, aes(x = fct_rev(groups2), y = estimate2k)) +
      geom_pointrange(aes(ymin = lower_ci2k, ymax = upper_ci2k), color = "grey10",
                      shape = 21, fill = "grey10", stroke = 1, size = 0.8) +
      theme_minimal() +
      coord_flip() +
      labs(title = "",
           x = "",
           y = "Earnings (in 1000 NOK)",
           caption = "") +
      scale_y_continuous(limits = c(0,90), breaks = seq(0,90, by = 10)) +
      scale_x_discrete() +
      theme(legend.position = "right",
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks.y = element_line(color = "black"),
            axis.ticks.length.y = unit(5, "pt"),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_line(color = "black"),
            axis.ticks.length.x = unit(3, "pt")) +
      geom_text(aes(label = sprintf("%2.1f", estimate2k)), vjust = -1)
    fig2b
    fig2b +
      theme(axis.text.x = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())  
    
    figure2 <- fig2 + fig2b
    figure2

    # Add bold titles inside the individual plots
    fig2 <- fig2 + ggtitle("A") + theme(plot.title = element_text(face = "bold"))
    fig2b <- fig2b + ggtitle("B") + theme(plot.title = element_text(face = "bold"))
    # Combine the figures without external labels
    figure2 <- plot_grid(fig2, fig2b)
    # Remove grid lines by customizing the theme of the plots
    fig2 <- fig2 + theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank())
    fig2b <- fig2b + theme(panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank())
    # Combine the figures without external labels
    figure2 <- plot_grid(fig2, fig2b)
    # Show the combined figure
    print(figure2)  
    
    # AR. Fig 2A: Log ------------------------------------------------------
    
    groups <- c("OLS", "Sibling model", "Twin model (DZ)", 
                "Twin model (MZ)", "MR", "Sibling MR")
    estimate <- c(0.0575,    # OLS
                  0.0515,    # sibling-regression
                  0.0423,    # Twin model (DZ)
                  0.0319,    # Twin model (MZ)
                  0.0773,    # MR
                  0.0608)    # sibling MR
    
    lower_ci <- c(0.0561,    # OLS
                  0.0508,    # sibling-regression
                  0.0324,    # Twin model (DZ)
                  0.0185,    # Twin model (MZ)
                  0.0690,    # MR
                  0.0150)    # sibling MR
    
    upper_ci <- c(0.0589,    # OLS
                  0.0522,    # sibling-regression
                  0.0522,    # Twin model (DZ)
                  0.0452,    # Twin model (MZ)
                  0.0857,    # MR
                  0.1100)    # sibling MR
    # Reorder levels of groups factor
    coefdata <- data.frame(groups, estimate, lower_ci, upper_ci)
    # Convert groups to a factor with specified levels
    coefdata$groups <- factor(coefdata$groups, levels = groups)
    # Coefficient plot
    fig2 <- ggplot(coefdata, aes(x = fct_rev(groups), y = estimate)) +
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
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks.y = element_line(color = "black"),
            axis.ticks.length.y = unit(5, "pt"),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_line(color = "black"),
            axis.ticks.length.x = unit(3, "pt")) +
      geom_text(aes(label = sprintf("%.3f", estimate)), vjust = -1)
    fig2
    
    fig2 +
      theme(axis.text.x = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())  
    
    
    # AR. Fig 2B: NOK ----------------------------------------------------------------
    
    groups2 <- c("OLS", "Sibling model", "Twin model (DZ)",
                 "Twin model (MZ)", "MR", "Sibling MR")
    estimate2 <- c(34617.58,    # OLS
                   30824.83,    # sibling-regression
                   26194.33,    # Twin model (DZ)
                   19196.15,    # Twin model (MZ)
                   53622.05,    # MR
                   45559.85)    # sibling MR
    lower_ci2 <- c(34087.22,    # OLS
                   30510.66,    # sibling-regression
                   19509.85,    # Twin model (DZ)
                   11112.61,    # Twin model (MZ)
                   47700.50,    # MR
                   12546.00)    # sibling MR
    upper_ci2 <- c(35147.93,    # OLS
                   31139.00,    # sibling-regression
                   32878.80,    # Twin model (DZ)
                   27279.69,    # Twin model (MZ)
                   59543.60,    # MR
                   80891.50)    # sibling MR
    estimate2k<-estimate2/1000
    lower_ci2k<-lower_ci2/1000
    upper_ci2k<-upper_ci2/1000
    
    # Reorder levels of groups factor
    coefdata2 <- data.frame(groups2, estimate2k, lower_ci2k, upper_ci2k)
    # Convert groups to a factor with specified levels
    coefdata2$groups2 <- factor(coefdata2$groups2, levels = groups2)
    # Coefficient plot
    fig2b <- ggplot(coefdata2, aes(x = fct_rev(groups2), y = estimate2k)) +
      geom_pointrange(aes(ymin = lower_ci2k, ymax = upper_ci2k), color = "grey10",
                      shape = 21, fill = "grey10", stroke = 1, size = 0.8) +
      theme_minimal() +
      coord_flip() +
      labs(title = "",
           x = "",
           y = "Earnings (in 1000 NOK)",
           caption = "") +
      scale_y_continuous(limits = c(0,90), breaks = seq(0,90, by = 10)) +
      scale_x_discrete() +
      theme(legend.position = "right",
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks.y = element_line(color = "black"),
            axis.ticks.length.y = unit(5, "pt"),
            axis.text.y = element_text(size = 12),
            axis.ticks.x = element_line(color = "black"),
            axis.ticks.length.x = unit(3, "pt")) +
      geom_text(aes(label = sprintf("%2.1f", estimate2k)), vjust = -1)
    fig2b
    fig2b +
      theme(axis.text.x = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())  
    
    figure2 <- fig2 + fig2b
    figure2
    
    # Add bold titles inside the individual plots
    fig2 <- fig2 + ggtitle("A") + theme(plot.title = element_text(face = "bold"))
    fig2b <- fig2b + ggtitle("B") + theme(plot.title = element_text(face = "bold"))
    # Combine the figures without external labels
    figure2 <- plot_grid(fig2, fig2b)
    # Remove grid lines by customizing the theme of the plots
    fig2 <- fig2 + theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank())
    fig2b <- fig2b + theme(panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank())
    # Combine the figures without external labels
    figure2 <- plot_grid(fig2, fig2b)
    # Show the combined figure
    print(figure2)      

# Supplementary ----------------------------------------------------------------    

# Fig S2: Binscatter -----------------------------------------------------------
    
    library(haven)
    library(tidyr)
    library(binsreg)
    
    mobdat <- read_dta("N:/durable/projects/EQOP/MR_Income/temp/moba_genetics.dta")
    # Convert variables
    mobdat %>%
      mutate(
        yos = as.numeric(yos),
        piv = as.numeric(piv),
      )
    piv <- mobdat$piv
    yos <- mobdat$yos
    bindata <- data.frame(yos,piv)
    a <- binsreg(yos,piv, nbins = 100, bycolors="grey20")
    fig <- a$bins_plot +
      geom_smooth(data = bindata, aes(x = piv, y = yos), method = "lm",
                  color = "grey50") + 
      scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4)) +
      scale_y_continuous(breaks = c(12,13,14,15,16,17)) +
      labs(x = "PIV-EA (Standardized)", y = "Years of schooling") 
    print(fig)
    figt <- fig + theme(axis.text.x = element_text(size = 12),
                        axis.text.y = element_text(size = 12),
                        axis.title.x = element_text(size = 12), # x label
                        axis.title.y = element_text(size = 12), # y label 
                        panel.border = element_rect(colour = "black", fill=NA, size=1),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())
    figt
    model <- lm(yos ~ piv, data=mobdat)
    summary(model)
    confint(model)
    
    
    
# Fig S11: Experience ----------------------------------------------------------
    
    labels_order <- c("Sibling MR + experience", "Sibling MR", 
                      "MR + experience", "MR",
                      "Twin model (MZ) + experience", "Twin model (MZ)",
                      "Twin model (DZ) + experience", "Twin model (DZ)",
                      "Sibling model + experience", "Sibling model",
                      "OLS + experience", "OLS")
    
    coef_data <- data.frame(
      model = c("moba_gen_sib1", "moba_gen_sib0", 
                "moba_gen1", "moba_gen0",
                "mz1", "mz0",
                "dz1", "dz0",
                "sib1", "sib0",
                "fullpop1", "fullpop0"),
      labels = factor(labels_order, levels = labels_order),
      estimate = c(0.079, 0.061,
                   0.098, 0.077,
                   0.037, 0.032,
                   0.053, 0.042,
                   0.059, 0.051,
                   0.066, 0.058),
      se = c(0.0241, 0.0207,
             0.0048, 0.0039,
             0.0064, 0.0064,
             0.0052, 0.0047,
             0.0004, 0.0003,
             0.0008, 0.0007)
    )
    
    coef_data$ci_lower <- coef_data$estimate - 1.96*coef_data$se
    coef_data$ci_upper <- coef_data$estimate + 1.96*coef_data$se
    
    ggplot(coef_data, aes(y = labels)) +
      geom_point(aes(x = estimate, shape = grepl("1$|gen1|pop1", model)), size = 3) +
      geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0) +
      geom_text(aes(x = estimate, label = sprintf("%.3f", estimate)), vjust = -1) +
      scale_shape_manual(values = c(16, 17), guide = "none") +
      labs(x = "Log(Earnings)", y = "") +
      theme_bw() +
      theme(panel.grid = element_blank(), axis.text = element_text(size = 12)) +
      scale_x_continuous(limits = c(0, 0.13), breaks = seq(0, 0.13, by = 0.02)) 
    
 

    
    