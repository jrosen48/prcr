# for cowplot

filter <- attributes(statistics)$cases_to_keep

esm_df$ch_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1, 1, 0)
esm_df$ch_doing_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_doing, 1, 0)

aov_df <- data.frame(who = esm_df$ch_who[filter], mat = esm_df$ch_mat[filter],
                     time = esm_df$ch_time[filter], howdo = esm_df$ch_howdo[filter],
                     doing = esm_df$ch_doing[filter], define = esm_df$ch_defin[filter],
                     topic = esm_df$ch_topic[filter], other = esm_df$ch_other[filter], 
                     framing = esm_df$ch_doing_defining_topic[filter],
                     none = esm_df$ch_none[filter],
                     cluster = statistics[[4]], ip = factor_df$ip[filter],
                     subject = esm_df$subject[filter])

aov_df$ip <- as.character(aov_df$ip)

table(aov_df$ip)

# aov_df <- filter(aov_df, ip == "Individual Work")

str(aov_df)

aov_df$subject[aov_df$subject == 1] <- "Integrated Science"
aov_df$subject[aov_df$subject == 2] <- "Biology"
aov_df$subject[aov_df$subject == 3] <- "Chemistry"
aov_df$subject[aov_df$subject == 4] <- "Physics"

# aov_df <- filter(aov_df, subject %in% c("Integrated Science", "Biology"))

aov_df$subject <- as.vector(aov_df$subject)

# aov_df$subject <- factor(explored_factors[[2]]$subject, 
#                                         levels = c("Integrated Science", "Biology", "Chemistry", "Physics"))

aov_df$any_choice <- ifelse(aov_df$framing == 1 | aov_df$mat == 1 | aov_df$howdo == 1 | aov_df$time == 1 |
                                aov_df$who == 1 | aov_df$other == 1, 1, 0)

str(aov_df)

plot <-
    aov_df %>% 
    dplyr::select(any_choice, none, cluster, ip) %>% 
    group_by(cluster, ip) %>% 
    summarize(Any = sum(any_choice),
              None = sum(none)) %>% 
    arrange(ip, cluster)

plot %>% group_by(ip) %>% summarize(sum = sum(Any + None))

plot <- plot[!is.na(plot$ip), ]

plot

y <- chisq.test(plot[1:6, 3:4])
y <- as.data.frame(y$stdres)
y
the_vec1 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[7:12, 3:4])
y <- as.data.frame(y$stdres)
y
the_vec2 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[13:18, 3:4])
y
y <- as.data.frame(y$stdres)
y$observed
y
the_vec3 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[19:24, 3:4])
y <- as.data.frame(y$stdres)
the_vec4 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[25:30, 3:4])
y <- as.data.frame(y$stdres)
the_vec5 <- as.vector(as.matrix(t(y)))

the_vec <- c(the_vec1, the_vec2, the_vec3, the_vec4, the_vec5)

plot$cluster <- factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")

to_plot <- 
    plot %>% 
    select(cluster:None) %>% 
    tidyr::gather(key, val, -cluster, -ip) %>% 
    arrange(ip, cluster)

to_plot

to_plot %>% group_by(ip) %>% summarize(sum = sum(val))

to_plot

to_plot$cluster <- factor(to_plot$cluster, levels = c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low"))

plot
to_plot

to_plot$label <- ifelse(the_vec > 1.96, "+",
                        ifelse(the_vec < -1.96, "-", ""))

to_plot %>% group_by(ip) %>% summarize(sum = sum(val))

to_plot$key <- factor(to_plot$key, c("Any", "None"))

to_plot %>% group_by(ip, key) %>% summarize(sum = sum(val))

to_plot %>% group_by(key) %>% summarize(sum = sum(val))

to_plot %>% group_by(cluster, key) %>% summarize(sum = sum(val))

to_plot$key <- forcats::fct_recode(to_plot$key,
                                   "Any Choice" = "Any",
                                   "No Choice" = "None")

to_plot <- dplyr::arrange(to_plot, desc(ip), key)

View(to_plot)

ggplot(to_plot, aes(x = key, y = val, fill = cluster, label = label)) +
    geom_bar(stat = "identity") +
    facet_grid( ~ ip) +
    geom_text(position = position_stack(.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    theme(text=element_text(size = 18, family = "Times")) +
    theme(legend.position = "right") +
    scale_fill_discrete("") +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    guides(fill = guide_legend(reverse = T))

ggsave("choice_ip.png", width = 10, height = 6)

# saving

p_lab <- p

ggsave("any_versus_none_12-4-16.png", width = 10, height = 7.25)