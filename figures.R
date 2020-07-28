# load libraries--------
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(forcats)
library(ggalluvial)
library(alluvial)
library(ggforce)
library(pals)
library(ggrepel)
library(stringr)
library(ggpmisc)

# load files-----

# list of OEMs
OEMlist <-
  read.csv(paste(getwd(), '/data/OEM.csv', sep = "")) %>%
  mutate(key = "OEM") %>% mutate_all(tolower) %>%
  rename("firms" = "target") %>% select(-reports) %>%
  unique()

# supplychain
supplychain.full <-
  read.csv(paste(getwd(), "/data/base-data.csv", sep =
                   ""),na.strings=c("","NA")) %>%
  mutate(SourceComponentYear = paste(name_supplier, component, year,sep=";"))

#countryorder
countryorder <-
  read.csv(paste(getwd(), "/data/countryorder.csv", sep = "")) %>%
  dplyr::rename("country_source" = "country_source")

# supplychain
firmcomponentyear <-
  read.csv(paste(getwd(), "/data/processed-data.csv", sep = ""),na.strings=c("","NA"))

# adding info on OEMs to supply chain
tmp <-
  firmcomponentyear %>% 
  mutate(SourceComponentYear = paste(name_supplier, component, year,sep=";"))%>%
  select(key, SourceComponentYear) %>% unique()
supplychain.full <- left_join(supplychain.full, tmp)
remove(tmp)

# componentscomplexity
complexity <-
  read.csv(paste(getwd(), "/data/complexity-HS.csv", sep = "")) %>%
  mutate_all(tolower) %>%
  unique() %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2006 & year <= 2016) %>%
  mutate(PCI_hs02 = as.numeric(PCI_hs02)) %>%
  unique() %>%
  group_by(component) %>%
  mutate(avg_complexity = mean(PCI_hs02)) %>%
  ungroup() %>%
  select(component, avg_complexity) %>%
  mutate(component = fct_reorder(component, avg_complexity, .desc = TRUE)) %>%
  unique()
#mutate(year = as.numeric(year))





#### FIGURES ----------
# Figure 1 -------
figure1 <- supplychain.full %>%
  select(name_supplier, country_source, component) %>%
  filter(component != "other") %>%
  # filter(key=="supplier") %>%
  unique() %>%
  tidyr::drop_na() %>%
  group_by(component, country_source) %>% summarise(component.n = length(component)) %>%
  ungroup() %>%
  group_by(component) %>% mutate(component.all = sum(component.n)) %>% arrange(component.all) %>% ungroup() %>%
  mutate(component = fct_reorder(component, component.all)) %>%
  left_join(countryorder) %>%
  mutate(country_source = fct_reorder(country_source, country_order)) %>%
  mutate(country_group = fct_reorder(country_group, country_order)) %>%
  filter(component != "")


OEM.countries <-
  c("germany", "denmark", "spain", "usa", "japan", "china", "india")


figure1 <- figure1 %>%
  mutate(country_source = ifelse(
    country_source %in% c("germany", "denmark", "spain", "usa", "japan", "china", "india"),
    as.character(country_source),
    "other"
  )) %>%
  group_by(component, country_source) %>%
  mutate(component.total = sum(component.n)) %>%
  ungroup() %>%
  select(country_source, component, component.total) %>% unique() %>%
  left_join(countryorder) %>%
  mutate(country_source = fct_reorder(country_source, country_order)) %>%
  select(-country_order)

figure1.plot <-
  ggplot(data = figure1, aes(x = component, y = component.total)) +
  geom_bar(stat = "identity",
           position = "stack",
           aes(fill = country_source)) +
  #geom_text(data=globalsupp.all, aes(x=component ,y=component.all, label=round(component.all)),hjust=-0.2,size=4) +
  #labs(title = paste("Suppliers for major wind turbine components (2006-2016)")) +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, 25),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2", name = "") +
  scale_color_brewer(palette = "Set2", name = "") +
  ylab("Number of suppliers") +
  xlab("Type of component") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6)
    
  ) +
  coord_flip()

plot(figure1.plot)



# Figure 2 ----

# we use the full data for the box plot, here based on HS02 (since most complete)
figure2 <-
  read.csv(paste(getwd(), "/data/complexity-HS.csv", sep = "")) %>%
  mutate_all(tolower) %>%
  select(year, PCI_hs02,  component) %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2006 & year <= 2016) %>%
  mutate(PCI_hs02 = as.numeric(PCI_hs02)) %>%
  unique() %>%
  group_by(component) %>%
  mutate(component.avg = mean(PCI_hs02)) %>%
  ungroup() %>%
  mutate(component = fct_reorder(component, component.avg, .desc = TRUE))

figure2.average <- figure2 %>%
  select(component, component.avg) %>%
  unique()

figure2.plot <- ggplot(figure2, aes(component, PCI_hs02)) +
  geom_boxplot(width = 0.4, coef = 1.5, size=0.3, outlier.size=0.8) + #geom_jitter(width = 0.05,alpha =0.5) +
  geom_point(
    data = figure2.average,
    aes(x = component, y = component.avg),
    colour = "red",
    size = 0.8
  ) +
  #labs(title = paste("Technology complexity estimates of wind turbine components")) +
  scale_y_continuous(
    limits = c(-1.5, 2.5),
    breaks = seq(-1.5, 2.5, 0.5),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2", name = "") +
  scale_color_brewer(palette = "Set2", name = "") +
  ylab("Technology complexity") +
  xlab("Wind turbine component") +
  theme_bw() +
  theme(
    legend.position = "right",
    title = element_text(size = 6),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot(figure2.plot)

remove(figure2.average)



# Figure 3 -----

figure3 <- supplychain.full %>%
  select(name_supplier, country_source, country_target, component, year) %>%
  filter(component != "other") %>%
  unique() %>%
  tidyr::drop_na() %>%
  left_join(countryorder) %>%
  mutate(country_source = fct_reorder(country_source, country_order)) %>%
  mutate(country_target = fct_reorder (country_target, country_order)) %>%
  group_by(component, year, country_source, country_target) %>%
  mutate(alluvialfreq = as.numeric(length(component))) %>%
  ungroup() %>%
  #select(-source_clean) %>%
  unique() %>%
  mutate(homeint = ifelse(
    as.character(country_target) == as.character(country_source),
    "home",
    "international"
  )) %>%
  select (-country_order) %>%
  filter(year == 2006 | year == 2016) %>%
  left_join(complexity) %>%
  mutate(component = fct_reorder(component, avg_complexity))

aw <- 0.05
sp <- 0.1
mypalette <- c("#b3b3b3", "#66c2a5")

# 2016 (need to change years each time)
figure3.plot <- figure3 %>% filter(year == 2016) %>%
  filter (component %in% c("blade", "control.system", "gearbox", "generator",  "tower")) %>%
  gather_set_data(c(2:3)) %>%
  ggplot(aes(
    x,
    id = id,
    split = y,
    value = 1
  ))  +
  geom_parallel_sets(
    aes(fill = homeint),
    show.legend = FALSE,
    alpha = 0.8,
    axis.width = aw,
    sep = sp
  ) +
  scale_fill_manual(values = mypalette) +
  geom_parallel_sets_axes(
    color = "black",
    fill = "black",
    axis.width = aw,
    sep = sp
  ) +
  geom_parallel_sets_labels(
    angle = 0,
    axis.width = aw,
    sep = sp,
    color = "black",
    size = 3
  ) +
  #theme_no_axes() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11)
  ) +
  ggtitle("Year: 2016") +
  guides(fill = guide_legend(title = "Supplier-OEM")) +
  facet_wrap( ~ component, scales = "free", nrow = 1)
plot(figure3.plot)

remove(aw, sp, mypalette)


# Figure 4 -------
figure4 <- supplychain.full %>%
  select(country_source, component, name_supplier) %>%
  filter(component != "other") %>%
  tidyr::drop_na() %>%
  group_by(component) %>%
  mutate(ncountry = as.numeric(n_distinct(country_source))) %>%
  mutate(nfirms = as.numeric(n_distinct(name_supplier))) %>%
  ungroup() %>%
  select(-country_source,-name_supplier) %>%
  unique() %>%
  left_join(complexity) %>%
  na.omit()

figure4.plot <-
  ggplot(figure4,
         aes(
           x = avg_complexity,
           y = ncountry,
           size = nfirms,
           label = component
         )) +
  geom_point (colour = "grey40") +
  scale_size(range = c(1, 6), name = "Number of Firms") +
  geom_text_repel(
    aes(label = component),
    hjust = 0,
    vjust = 0,
    size = 2,
    min.segment.length = Inf
  ) +
  #geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(-0.5, 1.75),
    breaks = seq(-0.5, 1.75, .25),
    expand = c(0, 0)
  ) +
  ylab("Number of supplier countries") +
  xlab("Average complexity") +
  theme_bw() +
  theme(
    legend.position = "right",
    title = element_text(size = 6),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot(figure4.plot)

# Figure 5 -----
 
 ## included with statistical analysis
 
# Figure 6 -----

figure6.complexity <- complexity %>%
  rename("value" = "avg_complexity")

OEMlist2 <-
  OEMlist %>% rename("name_supplier" = "firms") %>% select(name_supplier
                                                           , key)

figure6 <- supplychain.full %>%
  select(name_supplier, country_source, country_manuf, year, component) %>%
  unique() %>%
  gather(2:3, key = "country_type", value = "country") %>%
  #filter(country!="") %>%
  na.omit() %>%
  filter (component != "other") %>%
  left_join(OEMlist2) %>%
  left_join(figure6.complexity) %>%
  #filter (is.na(key)==TRUE) %>% #excluding OEMs here
  group_by(country, year) %>%
  mutate(max.complexity = max(value, na.rm = TRUE)) %>% #here we take the max complexity in a year
  ungroup() %>%
  select(year, country, max.complexity) %>%
  unique() %>%
  filter (
    country %in% c(
      "turkey",
      "mexico",
      "brazil",
      "india",
      "china",
      "indonesia",
      "vietnam",
      "egypt"
    )
  ) %>%
  filter(year %in% c(2006, 2008, 2010, 2012, 2014))

# plot
set.seed(3)
colourCount <- length(unique(figure6$country))
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))

figure6.plot <-
  ggplot(data = figure6, aes(x = year, y = max.complexity)) + 
  geom_line(
    aes(colour = factor(country)),
    alpha = 0.9,
    size = 0.5,
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(colour = factor(country)), 
                 size = (2), 
                 stroke=0.2,
             alpha = 1,
             position = position_dodge(width = 1)) +
  scale_fill_brewer(palette = "Set2", name = "") +
  scale_color_brewer(palette = "Set2", name = "") +
  scale_x_continuous(
    limits = c(2005, 2015),
    breaks = seq(2006, 2014, 2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 1.8),
    breaks = seq(-0.5, 1.75, .25),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    title = element_text(size = 6),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  ylab("Maximum complexity in a country") +
  xlab("Year")

plot(figure6.plot)

remove(OEMlist2)


# Supplementary Figure 1 -------
figureSI1 <- read.csv(paste(getwd(), "/data/complexity-comparison.csv", sep = ""), sep=",") %>%
  pivot_longer(3:6,names_to="type",values_to = "complexity") %>%
  dplyr::mutate(component = as.factor(component)) %>%
  dplyr::group_by(component,type) %>%
  dplyr::mutate(complexity.av = mean(complexity)) %>%
  ungroup() %>%
  select(-year, -complexity) %>%
  unique()  %>%
  group_by(type) %>%
  dplyr::mutate(complexity.max = max(complexity.av)) %>%
  dplyr::mutate(complexity.min = min(complexity.av)) %>%
  dplyr::mutate(complexity.norm = (complexity.av-complexity.min)/(complexity.max-complexity.min)) %>%
  ungroup()

figureSI1$component <- factor(figureSI1$component, 
                                          levels = c("gearbox","bearing","blade","nacelle",
                                                     "control.system","forgings","generator","power.converter",
                                                     "tower"))
figureSI1$type <- factor(figureSI1$type, 
                                     levels = rev(c("PCI_hs02","PCI_hs07",
                                                    "FlemingSorenson",
                                                    "Broeckel")))

my.cols <- rev(c("#1f78b4","#a6cee3","#b2df8a", "#fb9a99"))

figureSI1.plot <- ggplot(figureSI1, aes(x=component, y=complexity.norm, group=type)) + 
  geom_line(aes(color=type, size=type)) + 
  geom_point(aes(color=type)) +
  labs(title= paste("Technology complexity comparison of wind turbine components")) +
  scale_y_continuous(limits=c(-0.05,1.05), breaks=seq(0,1,0.25),expand = c(0,0)) +
  scale_fill_manual(values =my.cols, name="") +
  scale_color_manual(values = my.cols, name="") +
  scale_size_manual(values = c(0.8,0.8,0.8,0.8, 1.4))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  ylab("Technology complexity (normalized)") + 
  xlab("Wind turbine component") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=11),
        axis.text=element_text(size=11))  

plot(figureSI1.plot)


# Supplementary Figure 2 -----
figureSI2 <- firmcomponentyear %>%
  select(name_supplier,
         component,
         key,
         size,
         wind_specialization,
         country_source) %>%
  mutate(country_source = ifelse(
    country_source %in% c("germany", "denmark", "spain", "usa", "japan", "china", "india"),
    as.character(country_source),
    "other"
  )) %>%
  filter(component != "other" & key != "oem") %>%
  unique() %>%
  left_join(countryorder) %>%
  mutate(name_supplier = fct_reorder(country_source, country_order)) %>%
  tidyr::drop_na() %>%
  group_by(name_supplier, country_source) %>%
  mutate(component.n = length(component)) %>%
  ungroup() %>%
  select(-component) %>%
  unique() %>%
  mutate(comp_specialization = ifelse(component.n > 1, 0, 1)) %>%
  select(-component.n) %>%
  unique() %>%
  mutate(spl.wind = ifelse(wind_specialization == 1, 1, 0)) %>%
  mutate(spl.none = ifelse(wind_specialization == 0, 1, 0)) %>%
  mutate(value.cut = cut(
    size,
    c(0, 250, 500, 1000, 2000, 4000, 8000, 100000),
    labels = c(
      "0.Under 250",
      "1.250 to 500",
      "2.500 to 1,000",
      "3.1,000 to 2,000",
      "4.2,000 to 4,000",
      "5.4,000 to 8,000",
      "6.Over 8,000"
    )
  )) %>%
  group_by(value.cut, country_source) %>%
  mutate(firms.splwind = sum(spl.wind)) %>%
  mutate(firms.splnone = sum(spl.none)) %>%
  ungroup() %>%
  select(value.cut, country_source, firms.splwind, firms.splnone) %>%
  unique() %>%
  tidyr::drop_na() %>%
  gather(firms, value, 3:4)

figureSI2$firms <-
  factor(figureSI2$firms, levels = rev(c("firms.splwind", "firms.splnone")))

figureSI2.plot <- ggplot(figureSI2, aes(value.cut, value)) +
  geom_bar(stat = "identity", position = "stack", aes(fill = firms)) +
  labs(title = paste("Distribution of wind component suppliers")) +
  scale_y_continuous(
    limits = c(0, 26),
    breaks = seq(0, 25, 5),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2", name = "") +
  scale_color_brewer(palette = "Set2", name = "") +
  ylab("Number of supplier firms") +
  xlab("Number of employees") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  facet_wrap( ~ country_source, nrow = 2) +
  theme(
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 8),
    strip.background = element_rect(colour = "white", fill = "white")
  ) +
  coord_flip()

plot(figureSI2.plot)

