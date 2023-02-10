rm(list = ls(all = TRUE))

library(tidyverse) # for piping etc
library(ggpubr) # for saving 2 plots side by side

files <- as.data.frame(list.files(path = "RFB_Diving_Data/BIOT_AxyTrek_Processed/", pattern = "*_dive_stats.csv")) %>%
  separate(1, into = "files", sep = "_dive_stats.csv")

# Colour palette: https://personal.sron.nl/~pault/#sec:qualitative

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loop through .csv files to load data ####

df.dives.depth <- NULL
df.dives.number <- NULL

for (i in 1:nrow(files)) {
  
  # i = 1
  
  # Load data ####
  
  df.mini.depth <- read_csv(paste0("RFB_Diving_Data/BIOT_AxyTrek_Processed/", files[i,], "_dive_stats.csv")) %>%
    drop_na() %>%
    select(DateTime, Dive, MaxDepth, TripID, Bout, ToD) %>%
    mutate(DateTime = DateTime + 43200) %>% # Currently I'm just adding on 12 hours to make this look more realistic. Need to fix this!
    mutate(Year = format(DateTime, format = "%Y")) %>%
    mutate(Hour = format(DateTime, format = "%H")) %>%
    mutate(BirdID = i)
  
  df.dives.depth <- rbind(df.dives.depth, df.mini.depth)
  
  df.mini.number <- df.mini.depth %>% 
    group_by(c(ToD)) %>%
    summarise(no_rows = length(ToD)) %>%
    mutate(BirdID = i) %>%
    rename(NumDives = 2)
  
  df.dives.number <- rbind(df.dives.number, df.mini.number)
  
}

rm(df.mini.depth, df.mini.number, i, files)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create dive metrics vs ToD plots #####

# Add hours that don't have dives as empty factors to df
df.dives.depth$Hour <- as.factor(df.dives.depth$Hour)
levels(df.dives.depth$Hour)
levels(df.dives.depth$Hour) <- c(levels(df.dives.depth$Hour), 00:04, 21:24)
df.dives.depth$Hour <- ordered(df.dives.depth$Hour, levels = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                                                   "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots the frequency of dives at different times of day
    
a <- ggplot(df.dives.depth, aes(x = as.numeric(Hour), y = Dive)) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.3) +
  annotate("rect", xmin = 6, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.3) +
  annotate("rect", xmin = 7, xmax = 19, ymin = -Inf, ymax = Inf, fill = "#DDAA33", alpha = 0.3) +
  annotate("rect", xmin = 19, xmax = 20, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.3) +
  annotate("rect", xmin = 20, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.3) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), fill = NA, col = "black") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  theme_light() %+replace% theme(axis.title.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.text.x = element_blank()) +
  ylab("Relative frequency of dive events")

a

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots the dive depths at different times of day

b <- ggplot(df.dives.depth, aes(x = as.numeric(Hour), y = MaxDepth)) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.3) +
  annotate("rect", xmin = 6, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.3) +
  annotate("rect", xmin = 7, xmax = 19, ymin = -Inf, ymax = Inf, fill = "#DDAA33", alpha = 0.3) +
  annotate("rect", xmin = 19, xmax = 20, ymin = -Inf, ymax = Inf, fill = "#BB5566", alpha = 0.3) +
  annotate("rect", xmin = 20, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#004488", alpha = 0.3) +
  geom_jitter(fill = "grey60", alpha = 0.1) +
  geom_violin(aes(group = as.factor(Hour)), fill = NA, col = "black") +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +
  theme_light() %+replace% theme(axis.title.y = element_text(angle = 90, margin = margin(l = 3, r = 9)),
                                 axis.text.y = element_text(margin = margin(r = 5))) +
  ylab("Maximum depth (m)") +
  xlab("Hour of the day")

b

# Save plots:

ggarrange(a, b, nrow = 2)

# ggsave("RFB_Diving_Plots/Dive_fig.png", width = 8, height = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run model of dive depth vs time of day ####

numdives.mod <- brm(NumDives ~ 0 + ToD +
                   (1|BirdID|TripID),
                 data = df.dives.depth,
                 control = list(adapt_delta = 0.99))

depth.mod <- brm(MaxDepth ~ 0 + ToD +
                      (1|BirdID|TripID),
                    data = df.dives.depth,
                 control = list(adapt_delta = 0.99))
