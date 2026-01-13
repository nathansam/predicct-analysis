library(gganimate)


p <- demo_cumulative %>%
  ggplot(aes(x = entry_date, y = cumulative_count)) +
  geom_line(color = rgb(34, 122, 145, maxColorValue = 255),
            size = 1.2) +
  theme_minimal() +
  xlab("Year") +
  ylab("Study recruitment") +
  xlim(as.Date("2016-11-01"), as.Date("2020-04-01")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, 3000)) +
  theme(text = element_text(color = "#1C285A"),
        axis.text = element_text(face = "bold", color = "#1C285A"))


anim <- p +  transition_reveal(entry_date, keep_last=FALSE)

an <- animate(anim, renderer =  av_renderer(file = "test.mp4"),
              width=16 * 3/4,
              height=9 * 3/4,
              units = "in",
              res = 300,
              fps = 25)


png("end-frame.png",
    width=16 * 3/4,
    height=9 * 3/4,
    units = "in",
    res = 300)
p
dev.off()


p <- demo_cumulative %>%
  ggplot(aes(x = entry_date, y = cumulative_count)) +
  theme_minimal() +
  xlab("Year") +
  ylab("Study recruitment") +
  xlim(as.Date("2016-11-01"), as.Date("2020-04-01")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, 3000)) +
  theme(text = element_text(color = "#1C285A"),
        axis.text = element_text(face = "bold", color = "#1C285A"))

png("start-frame.png",
    width=16 * 3/4,
    height=9 * 3/4,
    units = "in",
    res = 300)
p
dev.off()
