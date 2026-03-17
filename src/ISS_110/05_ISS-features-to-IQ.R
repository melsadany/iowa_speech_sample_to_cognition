################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
demo <- read_csv("../shared_data/data/demo-full.csv") %>% mutate(devGenes_id=sub("_1","",devGenes_id))
m1.m2 <- read_rds("../shared_data/data/m1m2.rds")[,c(1,3:9,15:26)] %>% rename_all(.funs = function(x)sub("_age_corrected_standard_score","_NIH",x))
load("../shared_data/data/all-data-raw-and-resid-for-age-and-sex-and-their-PCs.rda")
categ.df <- read_rds("../shared_data/data/vars-categories.rds")
################################################################################
################################################################################
################################################################################
iss.feat <- all.resid$lang_ext[rowSums(is.na(all.resid$lang_ext))<400,]
iss.feat <- iss.feat[,colSums(is.na(iss.feat))<10]
iss.feat.meta <- data.frame(column = colnames(iss.feat)[-1]) %>%
  mutate(type = case_when(grepl("wmem",column)~"word memory",
                          grepl("__",column)~sub("__.*","", column),
                          grepl("bigram",column)~"SRT",
                          grepl("_check|_hi|_vis_aud",column)~"visual/auditory",
                          grepl("avg_positive|avg_negative|avg_neutral",column)~"reading",
                          T~""),
         feature = sub("sound__|abstract__|morph__|lexical__|letter__|abstract__","",column),
         feature = sub("pho_","pwe__",feature),
         feature = sub("sem_","sem__",feature),
         feature_type = case_when(grepl("sem__",feature)~"semantic embeddings",
                                  grepl("pwe__",feature)~"phonetic embeddings",
                                  T~""))
colnames(iss.feat)
################################################################################
################################################################################
################################################################################
### VIZ
iss.corr <- corr.table(inner_join(iss.feat,all.resid$cog) %>% select(-te_id),method="spearman") %>%
  filter(V1 %in% colnames(iss.feat), !V2 %in% colnames(iss.feat)) %>% mutate(FDR = p.adjust(pval,"fdr"))

iss.feat %>% select(te_id, contains("word_count")) %>% pivot_longer(cols = -1) %>%
  mutate(type = sub("__.*", "", name)) %>% 
  left_join(all.resid$cog %>% pivot_longer(cols = -1,names_to="feature", values_to="cog_score"), relationship = "many-to-many") %>%
  left_join(categ.df %>% mutate(feature = sub("_age_corre.*","_NIH",feature))) %>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=value,cog_score,color = type))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
      ggpubr::stat_cor(label.y.npc = 0.95, show.legend = F,size=3) + scale_color_manual(values = palette.1)+
      ggh4x::facet_wrap2(~feature_clean2,scales="free")+
      bw.theme+ labs(x="word count (corrected for age and sex)", y = "cognition score (corrected for age and sex)",
                     caption = paste0("n(sample): ",n))
  }
ggsave2("figs/ISS/ISS-app-based/word-assoc-count-to-iq.pdf",13,12)

iss.feat %>% select(te_id, iss.feat.meta$column[iss.feat.meta$type=="visual/auditory"]) %>% pivot_longer(cols = -1) %>%
  left_join(all.resid$cog %>% pivot_longer(cols = -1,names_to="feature", values_to="cog_score"), relationship = "many-to-many") %>%
  left_join(categ.df %>% mutate(feature = sub("_age_corre.*","_NIH",feature))) %>%
  filter(grepl("fastest|avg",name))%>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=value,cog_score,color = name))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
      ggpubr::stat_cor(label.y.npc = 0.95, show.legend = F,size=3) + scale_color_manual(values = palette.1)+
      ggh4x::facet_wrap2(~feature_clean2,scales="free")+
      bw.theme+ labs(x="feature (corrected for age and sex)", y = "cognition score (corrected for age and sex)",
                     caption = paste0("n(sample): ",n))
  }
ggsave2("figs/ISS/ISS-app-based/visual-auditory-to-iq.pdf",13,12)


iss.feat %>% select(te_id, iss.feat.meta$column[iss.feat.meta$type=="reading"]) %>% pivot_longer(cols = -1) %>%
  left_join(all.resid$cog %>% pivot_longer(cols = -1,names_to="feature", values_to="cog_score"), relationship = "many-to-many") %>%
  left_join(categ.df %>% mutate(feature = sub("_age_corre.*","_NIH",feature))) %>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=value,cog_score,color = name))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
      ggpubr::stat_cor(label.y.npc = 0.95, show.legend = F,size=3) + scale_color_manual(values = palette.1)+
      ggh4x::facet_wrap2(~feature_clean2,scales="free")+
      bw.theme+ labs(x="feature (corrected for age and sex)", y = "cognition score (corrected for age and sex)",
                     caption = paste0("n(sample): ",n))
  }
ggsave2("figs/ISS/ISS-app-based/word-reading-to-iq.pdf",13,12)


iss.feat %>% select(te_id, iss.feat.meta$column[iss.feat.meta$type%in%c("SRT","word memory")]) %>% pivot_longer(cols = -1) %>%
  mutate(cc=ifelse(grepl("wmem",name),"word memory", "SRT")) %>% group_by(te_id,cc) %>% summarise(value=mean(value)) %>%
  left_join(all.resid$cog %>% pivot_longer(cols = -1,names_to="feature", values_to="cog_score"), relationship = "many-to-many") %>%
  left_join(categ.df %>% mutate(feature = sub("_age_corre.*","_NIH",feature))) %>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=value,cog_score,color = cc))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
      ggpubr::stat_cor(label.y.npc = 0.95, show.legend = F,size=3) + scale_color_manual(values = palette.1)+
      ggh4x::facet_wrap2(~feature_clean2,scales="free")+
      bw.theme+ labs(x="feature (corrected for age and sex)", y = "cognition score (corrected for age and sex)",
                     caption = paste0("n(sample): ",n),color="")
  }
ggsave2("figs/ISS/ISS-app-based/word-mem-SRT-to-iq.pdf",13,12)

showtext::showtext.auto()
iss.corr %>% left_join(iss.feat.meta%>%rename(V1=column))%>%
  mutate(V1=sub(".*__","",V1))%>% 
  filter(V2 %in% colnames(all.resid$iq),type!="ALL",
         !grepl("norm",V1),type%in%c("morph","abstract","sound","lexical","letter"))%>%
  mutate(V2=factor(V2,colnames(all.resid$iq)),V1=sub("pho_|sem_","",V1)) %>%
  ggplot(aes(V2,V1,fill=r,label=case_when(FDR<0.05~"*",pval<0.05~".",T~"")))+
  geom_tile()+my.guides+redblu.col.gradient.2(label = rho2)+geom_text()+
  ggh4x::facet_nested(cols=vars(type),rows=vars(feature_type), scales="free",space="free")+
  bw.theme+theme(axis.text.x.bottom = element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x="",y="",caption = paste0("n(samples): ", nrow(iss.feat)))
ggsave2("figs/ISS/ISS-app-based/word-assoc-heatmap.pdf",width = 14,height = 12)
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
## figs to keep

p1 <- iss.feat %>% select(te_id, iss.feat.meta$column[iss.feat.meta$type=="visual/auditory"]) %>% pivot_longer(cols = -1) %>%
  left_join(all.resid$cog %>% pivot_longer(cols = -1,names_to="feature", values_to="cog_score"), relationship = "many-to-many") %>%
  left_join(categ.df %>% mutate(feature = sub("_age_corre.*","_NIH",feature))) %>%
  filter(name %in% c("fastest_check","avg_check","fastest_hi","avg_hi"),
         grepl("Block|Symbol|Visual|PSI", feature_clean2))%>%
  mutate(name = str_replace_all(name,"_"," "))%>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=value,cog_score,color = name))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
      ggpubr::stat_cor(label.y.npc = 0.98, show.legend = F,size=3) + 
      scale_color_manual(values = palette.2[-4],name="")+
      ggh4x::facet_wrap2(~feature_clean2,scales="free")+
      bw.theme+ labs(x="feature (corrected for age and sex)", y = "cognition score \n(corrected for age and sex)",
                     caption = paste0("n(sample): ",n))
  }

p2 <- all.resid$lang_ext %>% select(te_id, accurate_bigram_percentage_score) %>% 
  left_join(all.resid$cog %>% select(te_id, WM_composite_score)) %>% drop_na() %>%
  {
    n=length(unique(.$te_id))
    ggplot(., aes(x=accurate_bigram_percentage_score,WM_composite_score))+
      geom_point(shape=1,alpha=0.2)+geom_smooth(color = palette.1[2],method="lm",se=F)+ci_ribbon1+
      ggpubr::stat_cor(label.y.npc = 0.98, show.legend = F,color=palette.1[1],size=3) + 
      bw.theme+ labs(x="bigram accuracy\n(corrected for age and sex)", y = "WM composite score\n(corrected for age and sex)",
                     caption=paste0("n(samples): ", n))
  }

p3 <- iss.feat %>% select(te_id, contains(c("sem_sim_to_prompt_initi"))) %>% pivot_longer(cols = -1) %>%
  separate(name, into = c("prompt","feature"),sep="__") %>%
  left_join(all.resid$cog %>% select(te_id, Matrix_Reasoning)) %>%
  mutate(feature=sub("sem_","",feature)) %>%
  left_join(categ.df) %>%
  ggplot(aes(x=value,Matrix_Reasoning,color=prompt))+
  geom_point(shape=1,alpha=0.2)+geom_smooth(method="lm",se=F)+ci_ribbon.multi+
  ggpubr::stat_cor(label.y.npc = 0.98, show.legend = F,size=3) + 
  scale_color_manual(values = palette.1) +
  bw.theme+ labs(x="initial response semantic similarity to prompt\n(corrected for age and sex)", 
                 y = "Matrix Reasoning score\n(corrected for age and sex)",color="")+
  guides(color=guide_legend(nrow=2))


patchwork::wrap_plots(p1,patchwork::wrap_plots(p2,p3,ncol=1),nrow = 1,widths = c(2.1,1))
ggsave2("figs/ISS/ISS-app-based/features-to-iq_select.pdf",10,7)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
