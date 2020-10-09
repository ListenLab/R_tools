# Analyze dominance of top-n popular names by sex
# load packages
library(ggplot2)
library(reshape2)
library(dplyr)
# lear workspace
rm(list = ls())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# If you don't already have the dataset, then:
# Download data from: http://www.ssa.gov/oact/babynames/limits.html

setwd("C:\\Users\\Matt\\Documents\\R\\NameData")
# yob_files <- list.files(pattern = "yob")
# 
# read_year_data <- function(filename){
#   message(paste0("Processing... ", filename))
#   year <- substr(filename,4,7)
#   temp <- read.csv(filename, header = FALSE)
#   names(temp) <- c("Name","Sex","Count")
#   temp$Year <- as.numeric(year)
#   temp <- temp %>%
#     arrange(-Count) %>%
#     mutate(rank_overall = 1:n()) %>%
#     ungroup %>%
#     group_by(Sex)%>%
#     arrange(-Count) %>%
#     mutate(Rank = 1:n()) %>%
#     ungroup
#   
#   return(temp)
# }
# 
#----------------------------------------------------#
# # compile it all into a data frame
# df <- lapply(yob_files, read_year_data) %>% bind_rows()
# # save it
# save(df, file = "babyname_full_dataset.RData")
#----------------------------------------------------#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# # Then, after you've done that, call up the dataset
# from where you saved it
setwd("C:\\Users\\Matt\\Documents\\R\\NameData")
load("babyname_full_dataset.RData")

#----------------------------------------------------------#
# Express proportion relative to entire year
df <- 
  df %>%
  group_by(Year) %>%
  mutate(prop_overall = Count / sum(Count))

# Express proportion within same sex
df <- 
  df %>%
  group_by(Year, Sex) %>%
  mutate(prop_within = Count / sum(Count))

#----------------------------------------------------------#
# Analyze accumulation of top-rank dominance
sum_proportion_of_top_n <- function(dsub, ranks){
  lapply(ranks, FUN = function(x){
    df %>%
      dplyr::filter(Rank <= x) %>%
      group_by(Year, Sex) %>%
      summarise(prop_within = sum(prop_within), .groups = "drop") %>%
      mutate(top_rank = x) %>%
      return()
  }) %>%
    bind_rows() %>%
    return()
}
#----------------------------------------------------------#
# Run that function for each year, for each sex
df_rank_proportions <- df %>%
  group_by(Year, Sex) %>%
  do(., sum_proportion_of_top_n(., ranks = c(5, 10, 25, 50)))

# Remove duplicate rows
df_rank_proportions <- unique(df_rank_proportions)

#----------------------------------------------------------#
# Make a pretty version of the labels for the plot
df_rank_proportions$top_rank_label <- 
  factor(df_rank_proportions$top_rank, levels = c(5, 10, 25, 50), 
         labels = paste0("Top ",c(5, 10, 25, 50)," names"))

nrow(df_rank_proportions)
nrow(unique(df_rank_proportions))

#----------------------------------------------------------#
# Make a little data frame to label the lines directly
df_labels <- 
  df_rank_proportions %>%
  dplyr::filter(Year == 1940, top_rank == 50)

# Control position of labels
df_labels$prop_within <- c(0.35, 0.72)
df_labels$label <- c("girls' names","boys' names")

top_rank_dominance_title <- 
  paste0("Boys' names are more likely to be constrained to a small set of popular names")
top_rank_dominance_subtitle <- 
  paste0("~~ top-ranked names are more highly represented among all names for boys than for girls ~~")

#----------------------------------------------------------#
# Plot
px_top_rank_dominance <- ggplot(df_rank_proportions)+
  aes(x = Year, y = prop_within, 
      color = Sex)+
  geom_line(size = 1.4)+
  scale_color_manual(values = c(`F` = "black",
                                `M` = "steelblue3"),
                     labels = c("girls' names",
                                "boys' names"))+
  scale_y_continuous(
    name = "Percent of new births\nwith those names (within sex)",
    labels = scales::label_percent())+
  geom_label(data = df_labels, aes(label = label), size = 2.8)+
  coord_cartesian(ylim = c(0, 0.79), xlim = c(1900, 2020))+
  scale_x_continuous(breaks = seq(1900, 2020, 40))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(title = element_text(size = 9))+
  theme(panel.spacing.x = unit(1.2, "line"))+
  guides(color = guide_legend(reverse = TRUE))+
  ggtitle(top_rank_dominance_title, subtitle = top_rank_dominance_subtitle)+
  facet_grid(. ~ top_rank_label)
px_top_rank_dominance

# save
ggsave(px_top_rank_dominance, file = "px_top_rank_dominance.png",
       height = 3, width = 6, dpi = 200)
