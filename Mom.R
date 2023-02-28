zion <-
  gsheet2tbl("https://docs.google.com/spreadsheets/d/1dbt0HmaxPWpMboaYot60q7nlsLtc4yUAu16ZJ7HDJjo/edit#gid=0") %>%
  head(-4) %>%
  tail(-1) %>%
  mutate(Year = as.integer(substr(as.character(`...1`), nchar(`...1`)-3, nchar(`...1`))),
         `Total Accepted` = as.integer(Acceptance),
         Total = as.integer(`Total in class`),
         Rate = 100 * `Total Accepted` / Total,
         Status = case_when(((Year > 2014) & (Year != 2018) & (Year < 2022)) ~ "Active",
                            TRUE ~ "Inactive")) %>%
  select(Year, `Total Accepted`, Total, Rate, Status)

zion %>%
  ggplot(aes(x=Year, 
             y=Rate, 
             size=`Total Accepted`)) +
  geom_hline(yintercept=mean(zion$Rate),
             linetype="dashed",
             alpha=0.5) +
  geom_line(size=1,
            alpha=0.5) +
  geom_point(aes(color=Status),
             alpha=0.5) +
  scale_color_manual(values=c("#1a5d3c", "#69b691")) +
  scale_size(range=c(5, 10)) +
  labs(title="Impact on Zion Lutheran's High School Acceptances",
       subtitle="Angela Dumalig | High School Admissions Coach",
       x="Academic Year",
       y="Independent High School Acceptance Rate",
       caption="Last 10 Years (2013-2022)") +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.background = element_rect(color="black"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    plot.title = element_text(size=18,
                              face="bold"),
    plot.subtitle = element_text(size=14),
    plot.caption = element_text(size=10))
