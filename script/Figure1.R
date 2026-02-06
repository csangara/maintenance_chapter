library(tidyverse)

# Trying to recreate the plots
links_raw <- read.csv("data/links.bulk.csv", quote = "")

links_raw %>% head
links_raw$year %>% table

colors <- c("-1" = rgb(0.96,0.65,0.51),
            "1" = rgb(0.57,0.77,0.87),
            "3" = rgb(0.02,0.44,0.69),
            "4" = rgb(0.8,0.,0.13))

status_names <- c("Time out", "Accessible", "Redirected", "Broken") %>% 
  setNames(names(colors))

colors2 <- c('bg_unreach' = rgb(1.0,0.8,0.6), 
  'fg_unreach' = rgb(0.8,0.,0.), 
  'bg_accessb' = rgb(0.7,1.0,0.7), 
  'fg_accessb' = rgb(0.0,0.0,0.8)
)


# Parse status code into human-readable status
# -1 : time out         => -1
# 0..299 : good         => 1
# 300..399 : redirected => 3
# \>= 400 : not found   => 4

initial_year <- 2005
links <- links_raw %>% filter(year >= initial_year) %>%
  mutate(status = floor(code/100)) %>% 
  mutate(status = case_when(status > 4 ~ 4,
                            status >= 0 & status < 3 ~ 1,
                           TRUE ~ status)) %>% 
  mutate(status = factor(status, levels = c(-1, 1, 3, 4)))
links %>% nrow
table(links$status) %>% prop.table

# Avoid using repeated links
unique_links <- links %>% filter(flag.uniqueness == 0) %>% 
  select(-flag.uniqueness)

# Percent of repeated links
(1-nrow(unique_links)/nrow(links))*100

# Fixing http -> https redirection
redirection_checking <- read.csv("data/http2https.redirected.csv", quote="",
                                 header = FALSE, col.names = c("pubmed_id", "url", "status", "https_redirect"))
head(redirection_checking, 10)  

# Change status to 1 if http is changed to https
unique_links <- unique_links %>% 
  left_join(redirection_checking %>% select(url, https_redirect), by=c("link"="url")) %>% 
  mutate(status = case_when(https_redirect == "True" ~ "1", TRUE ~ status)) %>% 
  select(-https_redirect) %>% 
  mutate(status = factor(status, levels = rev(c(4, -1, 3, 1))))
table(unique_links$status) %>% prop.table


unique_links %>% count(broken = status %in% c(-1, 4)) %>% 
  mutate(frac = n/sum(n))

ggplot(unique_links, aes(x=status)) +
  geom_bar() +
  theme_classic()

# Number of links per year
links_per_year <- unique_links %>% 
  count(year, status)

links_per_year %>% group_by(recent = year >= 2019, broken = status %in% c(-1, 4)) %>% 
  summarise(n = sum(n)) %>% group_by(recent) %>% 
  mutate(frac = n/sum(n))

# Fig 1a - status per year
fig_1a <- ggplot(links_per_year, aes(x=year, y=n, fill=status)) +
  geom_bar(stat="identity", position="fill", width=0.8) +
  scale_fill_manual(values = colors, labels = status_names, name = "Status") +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024),
                     expand = expansion(mult = c(0.025,0.025), add = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0,0), add = c(0, 0.05)),
                     labels = scales::label_percent(suffix="")) +
  labs(y = "Percent of links", x = "Year") +
  theme_classic(base_size = 7) +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3, "cm"))
fig_1a

# Number of links as function of time
links_per_year_group <- links_per_year %>% 
  group_by(year, broken = status %in% c(-1, 4)) %>% 
  summarise(n = sum(n))

links_per_year_group %>% 
  mutate(frac = n/sum(n)) %>% filter(broken == FALSE) %>% 
  print(n=Inf)



# Fig 1b - line plot
fig_1b <- ggplot(links_per_year_group %>%
                   mutate(broken = as.character(broken)) %>% 
                   bind_rows(links_per_year_group %>% group_by(year) %>%
                               summarise(n = sum(n)) %>% mutate(broken = "None")) %>% 
                mutate(broken = factor(broken, levels = c("None", "FALSE", "TRUE"),
                                labels = c("Total", "Accessible +\nRedirected", "Broken +\nTime out"))),
                aes(x=year, y=n, group=broken, color=broken,
                                 fill=broken, shape=broken)) +
  geom_line(linewidth=0.3) +
  geom_point(size=1) +
  scale_fill_manual(values = c("#B3B3B3", unname(colors2[c("bg_accessb", "bg_unreach")]))) +
  scale_color_manual(values = c("#666666", unname(colors2[c("fg_accessb", "fg_unreach")]))) +
  scale_shape_manual(values = c(21, 21, 21)) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024)) +
  scale_y_continuous(limits = c(0, 5000),
                     expand = expansion(mult = c(0,0), add = c(0, 0.05)),
                     labels = scales::comma) +
  labs(y = "Number of links", x = "Year") +
  theme_bw(base_size = 7) +
  theme(legend.title = element_blank(),
        panel.border = element_blank(),
        axis.line.y.left = element_line(),
        axis.line.x.bottom = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #legend.key.spacing.y = unit(-1, "pt"),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))
        #legend.position = c(0.15, 0.925),
        #legend.background = element_rect(color="black"),
        
fig_1b


# Ratio between accessible links in first and last year
ref_year <- 2005
end_year <- 2024
links_per_year_group %>% filter(!broken) %>% ungroup() %>% 
  filter(year %in% c(ref_year, end_year)) %>% 
  summarise(ratio = last(n)/first(n),
            growth_rate = (last(n)-first(n))/(last(year)-first(year))/first(n)*100)

# Ratio between unreachable links
links_per_year_group %>% filter(broken) %>% ungroup() %>% 
  filter(year %in% c(ref_year, end_year)) %>% 
  summarise(ratio = last(n)/first(n),
            growth_rate = (last(n)-first(n))/(last(year)-first(year))/first(n)*100)

#### Parsing GitHub and SourceForge ####
unique_links <- unique_links %>% 
  mutate(link_source = case_when(grepl("github\\.", link) ~ "github",
                                 #grepl("gitlab|git\\.", link) ~"git",
                                 grepl("sourceforge\\.net|\\.sf\\.net", link) ~ "sourceforge",
                                 TRUE ~ "others"))
#tmp <- unique_links %>% filter(link_source == "git")
unique_links$link_source %>% table
unique_links %>% count(year, link_source) %>% 
  filter(link_source == "github")

unique_links %>% count(year, link_source) %>% 
  group_by(year) %>% 
  mutate(frac = n/sum(n)*100) %>% 
  filter(link_source == "github") %>% 
  print(n=Inf)
  

unique_links_source <- unique_links %>% count(link_source, status) %>% 
  mutate(link_source = factor(link_source, levels=c("others", "sourceforge", "github"),
                              labels = c("Others", "SourceForge", "GitHub")))

unique_links_source %>% 
  group_by(broken = status %in% c(-1, 4), link_source) %>% 
  summarise(n = sum(n)) %>% 
  group_by(link_source)
  mutate(frac = n/sum(n)*100)

  
  
# Fig 1f
fig_1f <- ggplot(unique_links_source, aes(x=link_source, y=n, fill=status)) +
  geom_bar(stat="identity", position="fill", width=0.7) +
  scale_fill_manual(values = colors, labels = status_names, name = "Status") +
  scale_y_continuous(expand = expansion(mult = c(0,0), add = c(0, 0.05)),
                     labels = scales::label_percent(suffix="")) +
  labs(y = "Percent of links") +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())
fig_1f

# Fig 1g
fig_1g <- unique_links %>% group_by(year, link_source) %>% 
  summarise(n=n()) %>%
  group_by(year) %>% 
  mutate(frac = n/sum(n)*100) %>% 
  filter(link_source != "others") %>% 
  mutate(link_source = factor(link_source, levels = c("github", "sourceforge"),
                              labels = c("GitHub", "SourceForge"))) %>% 
  ggplot(aes(x=year, y=frac,
             group=link_source, color=link_source,
             fill=link_source, shape=link_source)) +
  geom_line(linewidth=0.3) +
  geom_point(size=1) +
  scale_fill_manual(values = unname(colors2[c("bg_accessb", "bg_unreach")])) +
  scale_color_manual(values = unname(colors2[c("fg_accessb", "fg_unreach")])) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024)) +
  scale_y_continuous(limits = c(0, 32),
                     expand = expansion(mult = c(0,0.1), add = c(0, 0.05))) +
  scale_shape_manual(values = c(22, 21)) +
  labs(y = "Percent of links", x = "Year") +
  theme_bw(base_size = 7) +
  theme(legend.title = element_blank(),
        panel.border = element_blank(),
        axis.line.y.left = element_line(),
        axis.line.x.bottom = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.3, 0.925),
        legend.background = element_rect(color="black"),
        legend.margin = margin(t=2, r=2, b=2, l=2))
fig_1g

# Fig 1h
# fig_1h <- unique_links_source %>%
#   group_by(link_source, broken = status %in% c(-1, 4)) %>% 
#   summarise(n = sum(n)) %>% 
#   group_by(link_source) %>% mutate(frac = n/sum(n)*100) %>% 
#   filter(broken == TRUE) %>% 
#   ggplot(aes(x=frac, y=link_source)) +
#   geom_bar(stat="identity", fill=colors2["fg_unreach"], width=0.8) +
#   scale_x_continuous(expand = expansion(mult = c(0,0.07), add = c(0, 0.05))) +
#   scale_y_discrete(labels = c("SourceForge" = "Source\nForge")) +
#   labs(x = "Percent of unreachable links") +
#   theme_classic(base_size = 7) +
#   theme(axis.title.y = element_blank(),
#         axis.ticks.y = element_blank())
# fig_1h


journals_oi <- journals_oi <- c("BMC Genomics", "Genet Res (Camb)", "Genome Med", "Nat Methods",
                                "PLoS Comput Biol", "BMC Bioinformatics", "BMC Syst Biol",
                                "Genome Biol", "Nat Biotechnol", "Nucleic Acids Res") %>%
  setNames(gsub(" ", "_", .))

# Number of entries per journal
journal_order <- unique_links %>% count(journal) %>% arrange(n) %>% pull(journal)
journal_summ <- unique_links %>% count(journal, status) %>% 
  mutate(journal = factor(journal, levels = journal_order))
# journal_names <- str_extract(list.files("data/abstractLinks"), "abstractLinks_(\\S+)\\.prepared.tsv", group=1)
journal_links <- lapply(names(journals_oi), function(journal_name){
  abstract <- read.csv(paste0("data/abstractLinks/abstractLinks_", journal_name, ".prepared.tsv"), header=FALSE, sep="\t",
           col.names = c("journal", "pmid", "year", "link")) %>% filter(grepl("\\.|/", link)) %>% 
    distinct(journal, pmid, year) %>% filter(!is.na(pmid))
  body <- read.csv(paste0("data/bodyLinks/bodyLinks_", journal_name, ".prepared.tsv"), header=FALSE, sep="\t", quote="",
                   col.names = c("journal", "pmid", "year", "link")) %>% filter(grepl("\\.|/", link)) %>% 
    distinct(journal, pmid, year) %>% filter(!is.na(pmid))
  
  bind_rows(abstract, body) %>% distinct(journal, pmid, year)
}) %>% bind_rows()

journal_npapers_per_year <- lapply(names(journals_oi), function(journal_name){
  read.csv(paste0("data/abstractLinks/abstractLinks_", journal_name, ".prepared.tsv"), header=FALSE, sep="\t",
           col.names = c("journal", "pmid", "year", "link")) %>% 
    count(journal, year, name = "npapers")
}) %>% bind_rows()

journal_links %>% count(journal, year, name="npaper_with_links") %>% head
journal_links %>% head
journal_npapers_per_year %>% head

journal_npapers_stat <- journal_npapers_per_year %>% group_by(year) %>% 
  summarise(npapers = sum(npapers)) %>% 
  inner_join(journal_links %>% count(year, name="with_links"),
                                       by = c("year")) %>% 
  mutate(without_links = npapers - with_links) %>% select(-npapers) %>% 
  pivot_longer(cols=starts_with("with"), names_to="type", values_to="n") %>% 
  mutate(type = factor(type, levels = c("without_links", "with_links"),
                       labels = c("Without URL", "With URL")))

# My figure (c)
fig_1c <- ggplot(journal_npapers_stat %>% filter(year >= 2005),
       aes(x=year, y=n, fill=type)) +
  geom_bar(stat="identity", width=0.8) +
  scale_fill_manual(name = c("No. Articles"), values = c("#666666", "#1B9E77")) +
  theme_classic(base_size = 7) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024),
                     expand = expansion(mult = c(0.025,0.025), add = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0,0), add = c(0, 0.05)),
                     labels = scales::comma) +
  labs(y = "Number of articles", x = "Year") +
  theme_classic(base_size = 7) +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.925),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3, "cm"))
fig_1c
  
# journal_counts <- read.csv(paste0("data/counts.csv"), col.names = c("journal", "npapers"))
# journal_counts %>% inner_join(journal_links %>% count(journal), by="journal") %>% 
#   mutate(frac = n/npapers)
sum(journal_counts$npapers)

# Join all plots
library(patchwork)
first_row <- fig_1a + plot_spacer() + fig_1b + plot_layout(widths=c(0.6, -0.03, 1))
second_row <- fig_1c + fig_1f + fig_1g
all_plots <- free(first_row) / second_row +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face="bold"))

all_plots
ggsave("plots/fig1.pdf", all_plots,
       width = 160,
       height = 100,
       units = "mm")




unique_links %>% count(year)
unique_links %>% group_by(id, year) %>% 
  summarise(nlinks= n()) %>% 
  group_by(year) %>% 
  summarise(mean_links = mean(nlinks),
            sd_links = sd(nlinks),
            median_links = median(nlinks))
  

ggplot(journal_summ, aes(y=journal, x=n, color=status)) +
  geom_point(stat = "identity") +
  scale_x_log10() +
  theme_classic()
journal_summ %>% group_by(journal, broken = status %in% c(-1, 4)) %>% 
  summarise(n = sum(n)) %>% 
  mutate(frac = n/sum(n)) %>% 
  filter(broken == FALSE) %>% 
  arrange(frac) %>% 
  print(n=Inf)

journal_summ_usable <- unique_links %>% filter(status %in% c(1, 3)) %>% 
  group_by(journal) %>% 
  summarise(nlinks = n()) %>% 
  mutate(journal = factor(journal, levels = journal[order(nlinks)]))
ggplot(journal_summ_usable, aes(y=journal, x=nlinks)) +
  geom_bar(stat = "identity", position="fil") +
  theme_classic()
journal_summ_usable %>% arrange(nlinks)
