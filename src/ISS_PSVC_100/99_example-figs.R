################################################################################
################################################################################
rm(list = ls()); gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
source(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
library(patchwork)
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
################################################################################
psvc.all <- read_rds("data/derivatives/PSVC-USE-THIS.rds")


all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
all.cat <- read_csv("data/derivatives/tmp-use/words-categorized-by-archetype.csv")
umap <- read_rds("data/derivatives/tmp-use/umap-3d-from-psvc-embeddings.rds")
pairs.sim <- read_rds("data/derivatives/tmp-use/pairs-sim-by-word-by-participant.rds")
semantic.richness <- read_rds("data/derivatives/tmp-use/archetypes-semantic-richness.rds")
a.df <- read_rds("data/derivatives/tmp-use/archetypes-a-df.rds")
m1.m2 <- read_rds("../shared_data/data/m1m2.rds")
psvc <- read_rds("data/derivatives/USE-THIS.rds")
################################################################################
################################################################################
################################################################################
## community switching
cat.p <-all.cat %>%
  filter(start>0,
         te_id %in% c("2E_023", "2E_070"),
         word=="fly")%>%
  mutate(archetype = factor(archetype, levels = paste0("A", c(1:10))),
         start = start/1000,
         end = end/1000) %>%
  mutate(participant = ifelse(te_id == "2E_023", "X", "Y")) %>%
  ggplot(aes(x=participant, y=start, fill=archetype, label = text))+
  geom_tile(aes(height = (end - start)))+
  geom_text(angle = 90) +ylim(0,12)+geom_hline(yintercept = c(0,12),linetype=2,color="red")+
  scale_fill_manual(values = redblu.col.2) +
  coord_flip() +
  bw.theme +
  labs(x="participant", y="time (seconds)",
       subtitle = "responses to prompt: 'fly'")
ggsave2("figs/examples/clean-communities-plot.pdf", 6, 3.5)
cat.p %>% write_rds("figs/obj/clean-communities-plot.rds")
################################################################################
################################################################################
## euclidean distance
euc.p <- all.cat %>% filter(word=="bed", te_id %in% c("2E_064","2E_023")) %>%
  group_by(te_id) %>% mutate(first_word = ifelse(start==min(start),""," ")) %>% ungroup() %>%
  select(te_id, text, first_word) %>% 
  left_join(umap %>% distinct(text, Dim1, Dim2, Dim3)) %>%
  mutate(participant = ifelse(te_id == "2E_023", "X", "Y")) %>%
  ggplot(aes(x=Dim1, y=Dim2, color = participant, label=text)) +
  geom_path(linewidth=1) +
  geom_point(aes(shape = first_word), show.legend = F) +
  ggrepel::geom_text_repel(max.overlaps = 20, show.legend = F, color = "black",size=3) +
  # geom_text(size=3,show.legend = F, color = "black") +
  scale_color_manual(values = redblu.col.2) +
  scale_shape_manual(values = c(1,NA)) +
  bw.theme + theme(legend.position = "inside", legend.position.inside = c(0.84,0.2))+
  labs(subtitle = "responses to prompt: 'bed'", x= "UMAP Dim1", y="UMAP Dim2",
       caption = paste0("X:: FSIQ: 133 and VCI composite score: 113\n",
                        "Y:: FSIQ: 132 and VCI composite score: 125\n",
                        "Y ~ 2.5 X"))
ggsave2("figs/clean-euclidean-path-plot.png",4,4.5)
euc.p %>% write_rds("figs/obj/clean-euclidean-path-plot.rds")
################################################################################
################################################################################
## divergence
# div <- read_rds("data/derivatives/tmp-use/divergence-w-minimum-of-3-words-per-task.rds")

prompt.n="alone"
library(TSP)
id1="2E_074"
## actual
p.data.1 <- all[,c(1,4:5)] %>%
  filter(word != text) %>% distinct(te_id, word, text, .keep_all = T) %>% 
  filter(te_id == id1, word == prompt.n) %>%
  rownames_to_column("order") %>% mutate(path = "actual") %>% select(-c(word, te_id))
## optimal order
df10 <- pairs.sim %>% filter(te_id ==id1, word==prompt.n) %>% pivot_wider(names_from = "w2", values_from = "cos_similarity", id_cols = "w1") %>%
  column_to_rownames("w1"); df10 <- df10[,rownames(df10)]
dm.1 <- as.dist(1 - df10) ; tsp.dist.1 <- TSP(dm.1)
tsp.sol.1 <- as.integer(solve_TSP(tsp.dist.1, method = "repetitive_nn"))
df11 <- data.frame(w_order = tsp.sol.1, text = rownames(df10)[tsp.sol.1]) %>% 
  mutate(path = "optimal") %>% rownames_to_column("order") %>% select(-w_order)
plot.data <- full_join(df11, p.data.1) %>% mutate(participant = "X",embeddings="semantic") %>% 
  left_join(umap) %>% mutate(path = paste0(path, " path"))
# div%>%filter(word==prompt.n) %>% inner_join(m1.m2 %>% select(te_id, FSIQ, ends_with("composite_score"))) %>%filter(te_id == id1)

div.p <- plot.data %>%
  ggplot(aes(x=Dim1, y=Dim2, color = path, label = text)) +
  geom_point(shape = 1, data = plot.data %>% filter(order==1)) +
  geom_path(linewidth=1, aes(linetype=path)) + 
  ggrepel::geom_text_repel(color = "black", data = plot.data %>% distinct(text, Dim1, Dim2), size=3) +
  # ggh4x::facet_grid2(cols = vars(path)) +
  scale_color_manual(values = redblu.col.2) +
  bw.theme + labs(x="UMAP Dim1", y = "UMAP Dim2",
                  caption = paste0("X:: FSIQ: 129 and divergence of 0.467")) +
  theme(legend.position = "inside", legend.position.inside = c(0.173,0.2))
ggsave2("figs/examples/example-divergence-umap-2d_V3.png",4,4)
div.p %>% write_rds("figs/obj/example-divergence-umap-2d_V3.rds")
################################################################################
################################################################################
## archetpal area

# df.tmp <- semantic.richness %>% filter(prompt =="all") %>%
#   left_join(read_rds("../shared_data/data/m1m2.rds") %>% select(te_id, FSIQ, ends_with("composite_score"),Vocabulary))
id1="2E_043";id2="2E_094"
# df.tmp %>% filter(te_id %in% c(id1,id2)) %>% mutate(id=c("X","Y"))

p.data1 <- a.df %>% filter(text %in% unique(all$text[which(all$te_id == id1)]))
p1.c <- p.data1[chull(p.data1$x, p.data1$y),]
p.data2 <- a.df %>% filter(text %in% unique(all$text[which(all$te_id == id2)]))
p2.c <- p.data2[chull(p.data2$x, p.data2$y),]

hull_points <- rbind(p1.c %>% mutate(te_id = "X"),
                     p2.c %>% mutate(te_id = "Y")) %>%
  mutate(te_id = as.factor(te_id))
a.points <- a.df %>% mutate(lab = ifelse(text %in% c(paste0("A",c(1:10))),T,F)) %>% filter(lab==T) %>% select(x,y,text)

arch.area.p <- all%>% filter(te_id %in% c(id1,id2)) %>% select(te_id, text) %>%
  mutate(te_id = ifelse(te_id == id1,"X","Y")) %>%
  left_join(a.df %>%
              mutate(lab = ifelse(text %in% c(paste0("A",c(1:10)), p1.c$text, p2.c$text),T,F)))%>%
  rbind(a.df %>% mutate(lab = ifelse(text %in% c(paste0("A",c(1:10))),T,F)) %>%
          filter(lab==T|text=="") %>%
          mutate(te_id="") %>% left_join(a.points %>% rename(point1 = text))%>%
          left_join(a.points %>% rename(point2 = text, xend=x, yend=y))%>%
          filter(text!=""|parse_number(point2)==parse_number(point1)+1|point1=="A10"&point2=="A1") %>%
          select(-point1,-point2)) %>%
  filter(!((text=="nose"&te_id=="Y")|(text=="watch"&te_id=="X"))) %>% # it's duplicated for labeling
  ggplot(aes(x,y, color = te_id)) +
  geom_segment(aes(xend = xend, yend = yend), linewidth=0.5, show.legend = F) +
  geom_point(shape =1, show.legend = F) +
  geom_polygon(aes(x = x, y = y, color = te_id),data = hull_points, alpha = 0.5, linewidth = 1.5, fill = NA) +
  ggrepel::geom_text_repel(aes(label = ifelse(lab==F&!grepl("A",text), "", text), size = grepl("A", text)&lab),
            color = "black", show.legend = F, max.overlaps = 100)+
  scale_color_manual(values = c("Y" = redblu.col.2[1],"X" = redblu.col.2[2]), name = "participant")+
  scale_size_manual(values = c(3,5)) +
  bw.theme +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.border = element_blank()) +
  labs(caption = paste0("X:: FSIQ: 106 and VCI composite_score: 124\n",
                        "Y:: FSIQ: 107 and VCI composite_score: 127\n",
                        "Y ~ 1.1 X"))
arch.area.p;ggsave2("figs/examples/archetypal-area-example_v2.png", 6.7,7)
arch.area.p %>% write_rds("figs/obj/archetypal-area-example_v2.rds")
################################################################################
################################################################################
## trajectory
library(ggExtra)
png(filename = "figs/examples/word-count-by-time.png",width = 6,height = 4, 
    units = "in", bg = "white", res = 320)
traj.p <- ggMarginal(psvc.all$clean_tx %>% 
                       filter(te_id %in% c("2E_077","2E_096", "2E_130"), prompt == "a") %>%
                       group_by(te_id,prompt) %>% arrange(start) %>% mutate(index = c(1:n())) %>%
                       mutate(participant = case_when(te_id == "2E_077" ~ "Z",te_id == "2E_096" ~ "Y",te_id == "2E_130" ~ "X")) %>%
                       ggplot(aes(start, index, color=participant,label=response))+
                       geom_point(shape=1) + geom_path(linewidth=1.5) +
                       ggrepel::geom_text_repel(angle=90,size=2.5,hjust=1,vjust=0.5,show.legend = F,max.overlaps = 3)+
                       scale_color_manual(values = c("X" = redblu.col.2[2], "Y"= palette.1[3], "Z"=redblu.col.2[1]))+
                       bw.theme + theme(legend.position = "inside", legend.position.inside = c(0.1,0.7)) +
                       labs(x="time (seconds)", y="cumulative word count", subtitle = "prompt: A",
                            # caption = paste0("X:: FSIQ: 101\n",
                            #                  "Y:: FSIQ: 108\n",
                            #                  "Z:: FSIQ: 136\n")
                       ), 
                     type = "boxplot", groupColour = T)
traj.p
dev.off()
traj.p %>% write_rds( "figs/examples/word-count-by-time.rds")
################################################################################
################################################################################
## numbers
demo <- read_csv("../shared_data/data/demo-full.csv") %>%
  mutate(sex = case_when(sex == "Male" ~ "M", sex == "Female" ~ "F"))
m123 <- inner_join(m1.m2, demo) %>% filter(te_id %in% unique(psvc$lang_features_id_raw$te_id))
dd <- data.frame(count = nrow(m123),
                 male = paste0(sum(m123$sex=="M"), " (", round(sum(m123$sex=="M")/nrow(m123),2)*100, "%)"),
                 female = paste0(sum(m123$sex=="F"), " (", round(sum(m123$sex=="F")/nrow(m123),2)*100, "%)"),
                 age = paste0(round(mean(m123$nih_age/12),2), " (", round(sd(m123$nih_age/12),2), ")"),
                 ASD = paste0(sum(m123$ASD_dx==T), " (", round(sum(m123$ASD_dx==T)/nrow(m123),2)*100, "%)"),
                 ADHD = paste0(sum(m123$ADHD_dx==T), " (", round(sum(m123$ADHD_dx==T)/nrow(m123),2)*100, "%)"),
                 FSIQ = paste0(round(mean(m123$FSIQ),2), " (", round(sd(m123$FSIQ),2), ")"),
                 VCI = paste0(round(mean(m123$VCI_composite_score),2), " (", round(sd(m123$VCI_composite_score),2), ")"),
                 PSI = paste0(round(mean(m123$PSI_composite_score),2), " (", round(sd(m123$PSI_composite_score),2), ")"),
                 WM = paste0(round(mean(m123$WM_composite_score),2), " (", round(sd(m123$WM_composite_score),2), ")"))
write_csv(dd, "../language/data/derivatives/demographics-table.csv")

p1 <- m123 %>% inner_join(demo) %>%
  ggplot(aes(x = nih_age, color = sex)) +
  geom_density() +
  scale_color_manual(values = redblu.col.2) + bw.theme +
  labs(x = "age (mo)") + theme(legend.position = "inside", 
                               legend.position.inside = c(0.8,0.6))
p2 <- m123 %>% inner_join(demo) %>% group_by(sex) %>% summarise(count = n()) %>%
  ggplot(aes(x = sex, y=count, fill = sex)) +
  geom_bar(width = 0.4, show.legend = F, stat = "identity") + geom_text(aes(label = count)) +
  scale_fill_manual(values = redblu.col.2) + bw.theme +
  labs(x = "sex")
p3 <- m123 %>% inner_join(demo) %>%
  ggplot(aes(x = sex, y=FSIQ, fill = sex)) +
  geom_violin(show.legend = F) + geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(label.y.npc = 0.9) +
  scale_fill_manual(values = redblu.col.2) + bw.theme +
  labs(x = "sex")
p4 <- m123 %>% inner_join(demo) %>% pivot_longer(cols = c(ASD_dx,ADHD_dx)) %>%
  group_by(name, value) %>% summarise(count = n()) %>%
  ggplot(aes(x = value, y=count, fill = value)) +
  geom_bar(width = 0.4, show.legend = F, stat = "identity") + geom_text(aes(label = count)) +
  ggh4x::facet_grid2(cols = vars(name)) +
  scale_fill_manual(values = redblu.col.2) + bw.theme +
  labs(x = "")
patchwork::wrap_plots(patchwork::wrap_plots(p1,p3, widths = c(2,1)),
                      patchwork::wrap_plots(p2,p4, widths = c(1,2)),
                      ncol = 1)
ggsave2("../shared_data/figs/summary-plots.png", 7,7)
list("age" = p1, "sex_iq"=p3, "sex"=p2, "dx"=p4) %>% write_rds("../shared_data/figs/obj/summary-plots.rds")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
## read the figures
traj.p <- read_rds( "figs/obj/word-count-by-time.rds")
wc.p <- read_rds( "figs/obj/fsiq-vs-word-count.rds")
big.wm.p <- read_rds( "figs/obj/wm-vs-bigram.rds")
arch.area.p <- read_rds("figs/obj/archetypal-area-example_v2.rds")
div.p <- read_rds("figs/obj/example-divergence-umap-2d_V3.rds")
euc.p <- read_rds("figs/obj/clean-euclidean-path-plot.rds")
cat.p <- read_rds("figs/obj/clean-communities-plot.rds")
upset.p <- read_rds("../shared_data/figs/obj/iq-lang-mri-beh-numbers-upset.rds")
nih.iq.p <- read_rds("../shared_data/figs/obj/iq-nih-tests-w-sampling.rds")
summary.p <- read_rds("../shared_data/figs/obj/summary-plots.rds")
################################################################################
################################################################################
# wrap_plots(upset.p,wrap_plots(summary.p$age, summary.p$sex_iq), widths = c(2,1))
cowplot::plot_grid(upset.p,summary.p$age, summary.p$sex_iq, nrow = 1, rel_widths = c(2,1,1))
wrap_plots(nih.iq.p$p1+theme(strip.text.y.right = element_blank()),
           nih.iq.p$p2+theme(axis.text.y = element_blank(),legend.position = "right")+labs(subtitle = ""))


nih.iq.p$p2 + scale_color_manual(values = palette.1[c(7,3,5,6)], name="FSIQ percentile")
wrap_plots(wrap_plots(summary.p$age,summary.p$sex, summary.p$sex_iq,
                      summary.p$dx+ggh4x::facet_grid2(rows = vars(name)),
                      ncol = 1,heights = c(1,1,1,2)),
           wrap_plots(upset.p,
                      wrap_plots(nih.iq.p$p1+guides(alpha=guide_legend(direction = "vertical"))+
                                   theme(strip.text.y.right = element_blank()),
                                 nih.iq.p$p2 + scale_color_manual(values = palette.1[c(7,3,5,6)], name="FSIQ percentile")+
                                   guides(color=guide_legend(direction = "vertical"))+
                                   theme(axis.text.y = element_blank())+
                                   labs(subtitle = "")),
                      heights = c(1,1.5)),
           widths = c(1,2.5))

p1 <- cowplot::plot_grid(cowplot::plot_grid(upset.p,
                              wrap_plots(summary.p$age, summary.p$sex_iq, ncol = 1),
                              rel_widths = c(2,1)),
                   wrap_plots(nih.iq.p$p1+guides(alpha=guide_legend(direction = "vertical"))+
                                theme(strip.text.y.right = element_blank()),
                              nih.iq.p$p2 + scale_color_manual(values = palette.1[c(7,3,5,6)], name="FSIQ percentile")+
                                guides(color=guide_legend(direction = "vertical"))+
                                theme(axis.text.y = element_blank())+
                                labs(subtitle = "")),
                   rel_heights = c(1,1.5), ncol = 1)
png("figs/001.png", width = 8,height = 10, units = "in", res = 320)
p1
dev.off()


cat.p+labs(subtitle ="prompt: fly")+theme(legend.position = "inside",
                                          legend.position.inside = c(0.8,0.3))
p2.b <- wrap_plots(wrap_plots(euc.p+labs(subtitle="prompt: bed"), div.p+labs(subtitle="prompt: alone")),
                   cat.p+labs(subtitle ="prompt: fly")+theme(legend.position = "inside",
                                                             legend.position.inside = c(0.8,0.3)),
                   ncol = 1)
p2.a <- wrap_plots(wc.p,traj.p, widths = c(1,2))
p2.c <- wrap_plots(p2.b, arch.area.p, nrow = 1, widths = c(2,1))
p2 <- wrap_plots(p2.a, p2.c, ncol = 1, heights = c(1,1.3))


p2 <- wrap_plots(wrap_plots(wc.p,euc.p+labs(subtitle="prompt: bed",caption = NULL), 
                            div.p+labs(subtitle="prompt: alone",caption = NULL),ncol = 1),
           wrap_plots(traj.p,
                      arch.area.p+labs(caption=NULL), heights = c(1,1.5)),
           widths = c(1,1.7))
png("figs/002.png", width = 10,height = 10, units = "in", res = 320)
p2
dev.off()
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
