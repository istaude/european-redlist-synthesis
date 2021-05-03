source("R/00_preamble.R")



# Publishing year of red lists -----------------------------------------
bib <- read_excel("Data/bibliography.xlsx")
length(bib$Country)
length(unique(redlist_data$Country))
# w468 h522

ggplot(bib, aes(x=Publ_year, y = reorder(Country, -Publ_year))) +
  geom_bar(stat="identity", fill="#d1495b", alpha=.6, width=.8) +
  geom_text(aes(label=Country), hjust=-0.1, fontface = "italic") +
  coord_cartesian(xlim=c(1989, 2032)) +
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        text = element_text(size=14),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  labs( x = "Year of publication", y = "") 


ggsave(
  "Figures/bib.png",
  width = 5.4,
  height = 7.4,
  dpi = 1200,
  family="Helvetica"
)




# Number of Botanical Countries in which species are threatened -----------
rl_dis <- fread("Data/data_outputs/rlspecies_distribution.csv")

rl_dis %>% 
  group_by(species) %>% 
  summarise(bcountries_threatened = sum(Threatened)) %>% 
  count(bcountries_threatened) %>% 
ggplot() + 
  aes(x = bcountries_threatened, y = n) +
  geom_chicklet(width = 1, fill = "#d1495b") +
  geom_text( aes(label=n), vjust=-.2, fontface = "italic") +
  ylab("Number of species") + 
  xlab("Number of botanical countries threatened") +
  coord_cartesian(xlim=c(1, 21), ylim=c(0, 4600)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "gray40"),
        axis.text.y = element_text(color = "gray40"),
        text = element_text(size=14)) +
  theme_ipsum_rc(grid="Y", base_family = "Helvetica", axis_title_just = "mm", axis_title_size = 12)

ggsave(
  "Figures/nbot.png",
  width = 7.6,
  height = 2.8,
  dpi = 1200,
  family="Helvetica"
  )



# Percentage of range threatened ------------------------------------------

rl_dis_en <- fread("Data/data_outputs/rlspecies_percentage_threatened.csv")

rl_dis_en %>% 
  mutate(percentage_threatened = cut(percentage_threatened, breaks = c(0, .25, .5, .75, 1))) %>% 
  count(percentage_threatened) %>% 
  ggplot() + 
  aes(x = percentage_threatened, y = n) +
  geom_chicklet(width = 0.75, fill = "#d1495b") +
  geom_text( aes(label=n), vjust=-.2, fontface = "italic") +
  ylab("Number of species") + 
  xlab("Botanical countries in range threatened (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "gray40"),
        axis.text.y = element_text(color = "gray40"),
        text = element_text(size=14)) +
  scale_x_discrete(
    expand = c(0, 0.0625),
    labels = c("0 \u2013 25%", "25 \u2013 50%", "50 \u2013 75%", "75 \u2013 100%")
  ) +
  theme_ipsum_rc(grid="Y", base_family = "Helvetica", axis_title_just = "mm", axis_title_size = 12)


ggsave(
  "Figures/range_threat.png",
  width = 7.6,
  height = 2.8,
  dpi = 1200,
  family="Helvetica"
)


# Percentage IUCN assessed ------------------------------------------
iucn_threat <- fread("Data/data_outputs/iucn_threat.csv")

iucn_threat %>%
  mutate(redlistCategory = fct_relevel(as.factor(redlistCategory), 
                                       c("Extinct", "Threatened", "Lower Risk",  "Data Deficient",  "Not Evaluated")) ) %>% 
  ggplot( aes(threat, n, fill = redlistCategory)) +
  geom_chicklet(width = 0.75) +
  scale_y_continuous(expand = c(0, 0.0625), 
                     position = "right", 
                     breaks = seq(0, 100, 25), 
                     labels = c(0, sprintf("%d%%", seq(25, 100, 25)))) +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Extinct" = "#000000",
      "Threatened"  = "#CD3030",
      "Lower Risk" = "#2F6767",
      "Data Deficient" = "#C0C0C0",
      "Not Evaluated" = "#C0C0C0")) + 
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_ipsum_rc(grid="X", base_family = "Helvetica") +
  theme(legend.position = "top",
        axis.text.y = element_blank())


ggsave(
  "Figures/iucn_cross.png",
  width = 7.6,
  height = 2.8,
  dpi = 1200
)
