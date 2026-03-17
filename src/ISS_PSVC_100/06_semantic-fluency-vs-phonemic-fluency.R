################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis")
setwd(project.dir)
################################################################################
################################################################################
categ.df <- read_rds("data/vars-categories.rds")
demo <- read_csv("data/demo-full.csv")
load("data/all-data-raw-and-resid-for-age-and-sex-and-their-PCs.rda")
m12 <- inner_join(all.resid$iq, all.resid$nih)
m123 <- inner_join(m12, all.resid$lang_all_and_sep)
################################################################################
################################################################################
################################################################################
################################################################################
## word count comparison in both
m123 %>%
  ggplot(aes(WAT___unique_word_count, COWAT___unique_word_count))+
  geom_point(shape=1)+geom_smooth(method="lm",se=F,color=redblu.col.2[2])+
  ggpubr::stat_cor(color=palette.1[1])+ ci_ribbon1+ bw.theme+
  labs(x="word count | semantic fluency \n(corrected for age and sex effects)", 
       y="word count | phonemic fluency \n(corrected for age and sex effects)")
ggsave2("figs/word-count-semantic-to-phonemic.pdf", 3.2,3.2)

################################################################################
################################################################################
################################################################################
## comparison of all features
corr.table(all.resid.df%>%select(starts_with(c("WAT___","COWAT___")))) %>%
  mutate(task_1 = sub("___.*","",V1),task_2=sub("___.*","",V2),
         V1=sub(".*___","",V1),V2=sub(".*___","",V2))%>%
  filter(task_1=="WAT",task_2=="COWAT")%>%
  left_join(categ.df%>%select(V1=feature,V1_cl=feature_clean2,V1_cat=cat))%>%
  left_join(categ.df%>%select(V2=feature,V2_cl=feature_clean2,V2_cat=cat))%>%
  mutate(V1_cat=factor(V1_cat,rev(unique(categ.df$cat))),diag=V1==V2)%>%
  ggplot(aes(V1_cl,V2_cl,fill=r,label=case_when(FDR<0.05~"*",pval<0.05~small.circle,T~"")))+
  geom_tile()+my.guides+redblu.col.gradient.2(label="r")+geom_text(vjust=0.6)+
  geom_tile(data = ~ subset(.x, diag),color = "black", size = 0.3, fill = NA)+
  ggh4x::facet_grid2(rows=vars(V2_cat),cols=vars(V1_cat),scales="free",space="free")+
  bw.theme+theme(strip.text.y.right = element_text(angle=0),
                 axis.text.x.bottom = element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x="semantic fluency features",y="phonemic fluency features")
ggsave2("figs/lang-semantic-to-phonemic.pdf",28,24)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################