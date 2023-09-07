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
    mutate(DateTime_IO = DateTime + 21600) %>% # Add on 6 hours for IO time (21600 s)
    mutate(Year = format(DateTime_IO, format = "%Y")) %>%
    mutate(Hour = format(DateTime_IO, format = "%H")) %>%
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

# Edit the time of day variable

df.dives.depth <- df.dives.depth %>%
  mutate(time_of_day = as.numeric(DateTime_IO - trunc(DateTime_IO, "days")))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Modelling ####

# Make quick relationship plots of ToD and dive depth

hist(df.dives.depth$MaxDepth_m)
hist(log(df.dives.depth$MaxDepth_m))

hist(df.dives.depth$time_of_day)

ggplot(df.dives.depth) +
  geom_point(aes(x = time_of_day, y = log(MaxDepth_m)))

# Model relationship between max depth and sun altitude

depth.mod <- brm(MaxDepth_m ~ 0 + s(time_of_day) +
                   (1 | BirdID/Bout),
                 data = df.dives.depth,
                 family = lognormal,
                 control = list(adapt_delta = 0.9 , max_treedepth = 9))

plot(depth.mod)
pp_check(depth.mod)
plot(loo(depth.mod))

summary(depth.mod)

mcmc_plot(depth.mod)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create plot #####

# Plots the frequency of dives at different times of day
# as well as the depths

# Add hours that don't have dives as empty factors to df
df.dives.depth$Hour <- as.factor(df.dives.depth$Hour)
levels(df.dives.depth$Hour)
levels(df.dives.depth$Hour) <- c(levels(df.dives.depth$Hour), 00:05, 20:24)
df.dives.depth$Hour <- ordered(df.dives.depth$Hour, levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                                               "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# Data frame of conditional effects:
ce.depth.mod <- plot(conditional_effects(depth.mod, effects = "time_of_day",
                                         re_formula = NULL)) # Incorporate random effects variance
ce.depth.mod <- ce.depth.mod$time_of_day$data

# Make actual plots:

a <- ggplot(df.dives.depth, aes(x = as.numeric(Hour), y = Dive)) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.6) +
  annotate("rect", xmin = 6, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 7, xmax = 19, ymin = -Inf, ymax = Inf, fill = "#DDAA33", alpha = 0.6) +
  annotate("rect", xmin = 19, xmax = 20, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 20, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.6) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), fill = "white", col = NA, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  theme_light()  +
  ylab("Relative frequency of dives") +
  xlab("Hour of the day")

b <- ggplot() +
  annotate("rect", xmin = 0, xmax = 6, ymin = 0, ymax = Inf, fill = "#004488", alpha = 0.6) +
  annotate("rect", xmin = 6, xmax = 7, ymin = 0, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 7, xmax = 19, ymin = 0, ymax = Inf, fill = "#DDAA33", alpha = 0.6) +
  annotate("rect", xmin = 19, xmax = 20, ymin = 0, ymax = Inf, fill = "#BB5566", alpha = 0.6) +
  annotate("rect", xmin = 20, xmax = 24, ymin = 0, ymax = Inf, fill = "#004488", alpha = 0.6) +
  geom_point(data = df.dives.depth, aes(x=time_of_day, y = MaxDepth_m), shape = 21, alpha = 0.4, col = "black") +
  geom_smooth(data = ce.depth.mod, aes(x = time_of_day, y = estimate__),
              stat = "identity", alpha = 0, col = "black", linetype = 2) +
  geom_line(data = ce.depth.mod, aes(x = time_of_day, y = lower__),
            stat = "identity", col = "black", linetype = 2) +
  geom_line(data = ce.depth.mod, aes(x = time_of_day, y = upper__),
            stat = "identity", col = "black", linetype = 2) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  scale_y_continuous(trans = "log10") +
  theme_light() +
  ylab("Maximum dive depth (m)") +
  xlab("Hour of the day")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save plots:

ggarrange(a, b, ncol = 2)

ggsave("RFB_Diving_Plots/Dive_fig.png", width = 8, height = 4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Summary stats for text ####

# Max dive depth:
summary(df.dives.depth$MaxDepth_m)
# Info about this dive:
df.dives.depth[which(df.dives.depth$MaxDepth_m == max(df.dives.depth$MaxDepth_m)),]

# Pull out effect sizes:
summary(depth.mod)
