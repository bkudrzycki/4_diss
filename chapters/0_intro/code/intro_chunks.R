## ---- fig-neet ----

ggplot(data=df, aes(x=time, y=obs_value)) +
  geom_line()+
  geom_point()+
  xlab("") + ylab("Percent, ILO modelled estimates") +
  scale_x_continuous(breaks = c(2005:2022), guide = guide_axis(angle = 45)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
