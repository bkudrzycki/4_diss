## ---- fig-neet ----

pop2 <- pop %>% filter(Time < 2050 & Time > 1950) %>% pivot_wider(names_from = Age, values_from = Value) %>% 
  mutate(pop_tot = rowSums(across(everything()))-Time) %>% 
  mutate(youth_tot = rowSums(across(c("15-19", "20-24")))) %>% 
  select(Time, youth_tot, pop_tot)

plot1 <- ggplot(data=neet, aes(x=time, y=obs_value)) +
  geom_line()+
  xlab("") + ylab("% of youth population") +
  scale_x_continuous(limits = c(2004,2022), n.breaks = 6, guide = guide_axis(angle = 45), name = " ") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_label(
    label= "NEET share",
    x= 2011,
    y= 22.5,
    label.size = NA,
    color = "black",
  ) + ggtitle("Youth not in education, employment\nor training (NEET), SSA")


plot2 <- ggplot(pop2, aes(x=Time)) +
  geom_line( aes(y=youth_tot/pop_tot*100)) +
  geom_line( aes(y=youth_tot/10000000)) + 
  scale_x_continuous(limits = c(1950,2050), n.breaks = 6, guide = guide_axis(angle = 45), name = "") +
  scale_y_continuous(name="% of total population",
                     sec.axis = sec_axis(~.*10, name = "Total (millions)")) +
  xlab("") +
  geom_label(
    label= "Youth population",
    x= 2010,
    y= 6,
    label.size = NA,
    color = "black",
  ) +
  geom_label(
    label= "% of total",
    x= 1975,
    y= 20.7,
    label.size = NA,
    color = "black",
  ) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Youth population in SSA,\ntotal and as % of total population.")


grid.arrange(plot1, plot2, ncol=2, widths = c(.9,1))

## ---- fig-inf ----

ggplot(data=inf, aes(x=age, y=value, group = ref_area.label)) +
  geom_line() +
  geom_point()+
  ylab("Informal employment, % of total employed") +
  xlab("Age group") +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  scale_x_continuous(breaks = c(19.5, 27, 32, 39.5, 49.5, 59.5, 69.5),
                     labels = c("15-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65+")) +
  geom_label(
    label= "High-income",
    x= 42,
    y= 16.5,
    label.size = NA,
    color = "black",
  ) +
  geom_label(
    label= "Low-income",
    x= 42,
    y= 91,
    label.size = NA,
    color = "black",
  ) +
  geom_label(
    label= "Middle-income",
    x= 40,
    y= 66,
    label.size = NA,
    color = "black",
  ) +
  geom_label(
    label= "World",
    x= 45,
    y= 52,
    label.size = NA,
    color = "black",
  ) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
