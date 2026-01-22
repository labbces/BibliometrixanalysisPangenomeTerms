library(ggplot2)
library(dplyr)

#Number of paper per years come from SCOPUS searches
#
#Group 1 Query in Scopus
## TITLE-ABS-KEY(pangenome OR "pan-genome")
## AND TITLE-ABS-KEY("core genome")
## AND TITLE-ABS-KEY("accessory genome" 
## OR "dispensable genome" 
## OR "unique genome" 
## OR "exclusive genome")
#Group 2 Query in Scopus:
## TITLE-ABS-KEY(pangenome OR "pan-genome")
## AND TITLE-ABS-KEY("core")
## AND TITLE-ABS-KEY("shell")
## AND TITLE-ABS-KEY("cloud")
#Pangenome Query in Scopus, used for normalization:
## TITLE-ABS-KEY(pangenome OR "pan-genome")

txt <- "
Year\tCategory\tNumber Papers
2026\tPangenome\t59
2025\tPangenome\t936
2024\tPangenome\t771
2023\tPangenome\t605
2022\tPangenome\t575
2021\tPangenome\t408
2020\tPangenome\t321
2019\tPangenome\t228
2018\tPangenome\t176
2017\tPangenome\t156
2016\tPangenome\t108
2015\tPangenome\t101
2014\tPangenome\t86
2013\tPangenome\t63
2012\tPangenome\t51
2011\tPangenome\t48
2010\tPangenome\t36
2009\tPangenome\t28
2008\tPangenome\t7
2007\tPangenome\t9
2006\tPangenome\t6
2005\tPangenome\t4
2004\tPangenome\t0
2003\tPangenome\t2
2002\tPangenome\t0
2001\tPangenome\t0
2000\tPangenome\t0
1999\tPangenome\t0
1998\tPangenome\t0
1997\tPangenome\t0
1996\tPangenome\t0
1995\tPangenome\t0
1994\tPangenome\t0
1993\tPangenome\t0
1992\tPangenome\t0
1991\tPangenome\t0
1990\tPangenome\t0
1989\tPangenome\t0
1988\tPangenome\t0
1987\tPangenome\t0
1986\tPangenome\t0
1985\tPangenome\t0
1984\tPangenome\t0
1983\tPangenome\t0
1982\tPangenome\t1
2026\tGroup 2\t1
2025\tGroup 2\t6
2024\tGroup 2\t7
2023\tGroup 2\t10
2022\tGroup 2\t8
2021\tGroup 2\t4
2020\tGroup 2\t4
2025\tGroup 1\t30
2024\tGroup 1\t20
2023\tGroup 1\t23
2022\tGroup 1\t28
2021\tGroup 1\t22
2020\tGroup 1\t16
2019\tGroup 1\t18
2018\tGroup 1\t13
2017\tGroup 1\t7
2016\tGroup 1\t6
2015\tGroup 1\t7
2014\tGroup 1\t7
2013\tGroup 1\t2
2012\tGroup 1\t3
2011\tGroup 1\t4
2010\tGroup 1\t1
2009\tGroup 1\t1
2008\tGroup 1\t0
2007\tGroup 1\t0
2006\tGroup 1\t0
2005\tGroup 1\t2
"

data<-read.table(text=txt,header=T, sep = "\t", stringsAsFactors = FALSE)
head(data)

# Make sure types are correct
data$Year <- as.numeric(data$Year)
data$Category <- as.factor(data$Category)

table(data$Category)

ggplot(subset(data, Category != "Pangenome"),
       aes(x = Year, y = Number.Papers,
                 group = Category, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_y_log10() +
  labs(x = "Year",
       y = "Number of papers",
       color = "Category")

# Denominator: total pangenome papers per year
denom <- data %>%
  filter(Category == "Pangenome") %>%
  select(Year, PangenomeTotal = Number.Papers)

# Numerator: terminology groups only, joined to the denominator
plot_pct <- data %>%
  filter(Category != "Pangenome") %>%
  left_join(denom, by = "Year") %>%
  mutate(Percent = 100 * Number.Papers / PangenomeTotal)

# Plot percentages
ggplot(plot_pct, aes(x = Year, y = Percent, group = Category, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  xlim(2020,2026) +
  ylim(0,6) +
  labs(x = "Year",
       y = "Percent of pangenome papers (%)",
       color = "Terminology group")       

#with linear regression line
ggplot(plot_pct, aes(x = Year, y = Percent, group = Category, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  theme_minimal() +
  xlim(2020, 2026) +
  ylim(0, 6) +
  labs(x = "Year",
       y = "Percent of pangenome papers (%)",
       color = "Terminology group")

plot_overlap <- plot_pct %>%
  filter(Year >= 2020, Year <= 2026)

lm_stats_overlap <- plot_overlap %>%
  group_by(Category) %>%
  do({
    fit <- lm(Percent ~ Year, data = .)
    data.frame(
      slope = coef(fit)[["Year"]],
      r2    = summary(fit)$r.squared
    )
  })

lm_stats_overlap

lm_stats_overlap_fmt <- lm_stats_overlap %>%
  mutate(
    slope = round(slope, 3),
    r2    = round(r2, 3)
  )

lm_stats_overlap_fmt

# --- Choose label positions (adjust if needed) ---
# Put labels near the right side, stacked by category
label_pos <- plot_pct %>%
  filter(Year == 2025) %>%
  group_by(Category) %>%
  summarise(y = max(Percent, na.rm = TRUE), .groups = "drop") %>%
  left_join(lm_stats_overlap, by = "Category") %>%   # lm_stats contains `label`
  mutate(
    x = 2025.2,
    y = pmin(y + 0.4, 5.8)
  )

label_pos <- label_pos %>%
  mutate(label = paste0("slope = ", round(slope, 3),
                        "\nR² = ", round(r2, 3)))
str(label_pos)


# --- Plot ---
ggplot(plot_pct, aes(x = Year, y = Percent,
                     group = Category, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(
    data = plot_overlap,
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_text(
    data = label_pos,
    aes(x = x, y = y, label = label, color = Category),
    inherit.aes = FALSE,
    hjust = 0, vjust = 0,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = 2020:2026) +
  coord_cartesian(xlim = c(2020, 2026), ylim = c(0, 6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Year",
       y = "Percent of pangenome papers (%)",
       color = "Terminology group")


