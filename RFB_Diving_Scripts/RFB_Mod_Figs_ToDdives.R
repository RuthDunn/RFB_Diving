rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(ggpubr) # for saving 2 plots side by side
library(brms) # for the modelling
library(tidybayes) # for plotting model outputs

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Processed/", pattern = "*_dive_stats.csv")) %>%
  separate(1, into = "files", sep = "_dive_stats.csv")

# Colour palette: https://personal.sron.nl/~pault/#sec:qualitative

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

df.dives.depth <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini.depth <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv")) %>%
    drop_na() %>%
    dplyr::select(DateTime, Dive, MaxDepth_cm, DiveDuration_s, TripID, Bout, Sun.Alt) %>%
    mutate(DateTime2 = DateTime + 21600) %>% # Add on 6 hours for IO time (21600 s)
    mutate(Year = format(DateTime2, format = "%Y")) %>%
    mutate(Hour = format(DateTime2, format = "%H")) %>%
    mutate(BirdID = i) %>%
    rename(MaxDepth_m = MaxDepth_cm)
  
  df.dives.depth <- rbind(df.dives.depth, df.mini.depth)

  # df.mini.number <- df.mini.depth %>%
  #   group_by(c(ToD)) %>%
  #   summarise(no_rows = length(ToD)) %>%
  #   mutate(BirdID = i) %>%
  #   rename(NumDives = 2)
  
  # df.dives.number <- rbind(df.dives.number, df.mini.number)
  
}

rm(df.mini.depth, i, files)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create dive metrics vs ToD plots #####

# Add hours that don't have dives as empty factors to df
df.dives.depth$Hour <- as.factor(df.dives.depth$Hour)
levels(df.dives.depth$Hour)
levels(df.dives.depth$Hour) <- c(levels(df.dives.depth$Hour), 00:05, 20:24)
df.dives.depth$Hour <- ordered(df.dives.depth$Hour, levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                                   "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots the frequency of dives at different times of day
    
a <- ggplot(df.dives.depth, aes(x = as.numeric(Hour), y = Dive)) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.6) +
  annotate("rect", xmin = 6, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 7, xmax = 19, ymin = -Inf, ymax = Inf, fill = "#DDAA33", alpha = 0.6) +
  annotate("rect", xmin = 19, xmax = 20, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 20, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.6) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), fill = NA, col = "black") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  theme_light() +
  ylab("Relative frequency of dives") +
  xlab("Hour of the day")

a

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Modelling ####

# Make quick relationship plots

ggplot(df.dives.depth) +
  geom_point(aes(x = Sun.Alt, y = MaxDepth_m))

ggplot(df.dives.depth) +
  geom_point(aes(x = log(Sun.Alt), y = log(MaxDepth_m)))

ggplot(df.dives.depth) +
  geom_point(aes(x = (Sun.Alt), y = log(MaxDepth_m)))

hist(df.dives.depth$MaxDepth_m)
hist(log(df.dives.depth$MaxDepth_m))

hist(df.dives.depth$Sun.Alt)

# Model relationship between max depth and sun altitude

depth.mod <- brm(log(MaxDepth_m) ~ 0 + Sun.Alt +
                      (1 | BirdID/Bout),
                    data = df.dives.depth)

# to reduce divergent transitions after warm up try something along these lines:
# control = list(adapt_delta = 0.9 , max_treedepth = 9)

plot(depth.mod)
pp_check(depth.mod)
pp_check(depth.mod, type='stat', stat='mean')
pp_check(depth.mod, type='error_scatter_avg')
plot(loo(depth.mod))

summary(depth.mod)

mcmc_plot(depth.mod)
bayes_R2(depth.mod, digits=2)

conditional_effects(depth.mod)

# Try adding in a smooth for the sun altitude variable instead (GAM-like?)

depth.mod.smooth <- brm(log(MaxDepth_m) ~ 0 + s(Sun.Alt) +
                   (1 | BirdID/Bout),
                 data = df.dives.depth,
                 control = list(adapt_delta = 0.99, max_treedepth = 12))

plot(depth.mod.smooth)
pp_check(depth.mod.smooth)
pp_check(depth.mod.smooth, type='stat', stat='mean')
pp_check(depth.mod.smooth, type='error_scatter_avg')
plot(loo(depth.mod.smooth))

summary(depth.mod.smooth)

mcmc_plot(depth.mod.smooth)
bayes_R2(depth.mod.smooth, digits=2)

conditional_smooths(depth.mod.smooth)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot the results ####

# Data frame of conditional effects:
ce.depth.mod <- plot(conditional_effects(depth.mod.smooth, effects = "Sun.Alt",
                                        re_formula = NULL)) # Incorporate random effects variance
ce.depth.mod <- ce.depth.mod$Sun.Alt$data

c <- ggplot(df.dives.depth, aes(x = Sun.Alt, y = (MaxDepth_m))) +
  geom_jitter(alpha = 0.6, col = "#004488") +
  geom_smooth(data = ce.depth.mod, aes(x = Sun.Alt, y = exp(estimate__),
                                      ymin = exp(lower__), ymax = exp(upper__)),
              stat = "identity", alpha = 0.2, col = "black") +
  theme_light() %+replace% theme(legend.position = "bottom") +
  ylab("Maximum depth (m)") +
  xlab("Sun altitude") +
  scale_y_continuous(trans = "log10")

c

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots the dive depths at different times of day

b <- ggplot(df.dives.depth, aes(x = as.numeric(Hour), y = MaxDepth_m, col = Sun.Alt)) +
  scale_colour_gradient2(low = "#004488", high = "#DDAA33", mid = "#BB5566", midpoint = 40, "Sun
altitude (°)") +
  geom_jitter(alpha = 0.6) +
  geom_violin(aes(group = as.factor(Hour)), fill = NA, col = "black") +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  theme_light() %+replace% theme(axis.title.y = element_text(angle = 90, margin = margin(l = 3, r = 9)),
                                 axis.text.y = element_text(margin = margin(r = 5)),
                                 legend.position = "right",
                                 axis.title.x = element_text(margin = margin(b = 0, t = 0)),
                                 legend.margin = margin(b = -0.5, t = -0.5)) +
  ylab("Maximum depth (m)") +
  xlab("Hour of the day")

b

# Save plots:

ggarrange(a, 
          ggarrange(b, c, ncol = 2, widths = c(1.2, 0.8)),
          nrow = 2, heights = c(0.9, 1.1))

ggsave("RFB_Diving_Plots/Dive_fig.png", width = 8, height = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Summary stats for text ####

# Max dive depth:
summary(df.dives.depth$MaxDepth_m)
# Info about this dive:
df.dives.depth[which(df.dives.depth$MaxDepth_m == max(df.dives.depth$MaxDepth_m)),]

# Pull out effect sizes:
summary(depth.mod.smooth)

# Max altitude:
summary(df.dives.depth$Sun.Alt)
df.dives.depth[which(df.dives.depth$Sun.Alt == max(df.dives.depth$Sun.Alt)),]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(df.dives.depth) +
  geom_point(aes(x = log(DiveDuration_s), y = log(MaxDepth_m)))
