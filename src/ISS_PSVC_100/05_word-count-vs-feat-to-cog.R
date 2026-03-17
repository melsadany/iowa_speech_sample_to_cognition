################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis")
setwd(project.dir)
################################################################################
################################################################################
demo <- read_csv("data/demo-full.csv")
load("data/all-data-raw-and-resid-for-age-and-sex-and-their-PCs.rda")
m1.m2 <- inner_join(all.resid$iq,all.resid$nih)
categ.df <- read_rds("data/vars-categories.rds")
m123 <- inner_join(m1.m2,all.resid$lang_all_and_sep)
m125 <- full_join(m123, all.resid$lang_ext)
################################################################################
################################################################################
################################################################################
################################################################################
iq.domains <- colnames(all.resid$iq)[-1]
psvc.features <- colnames(all.resid$lang_all_and_sep)[-1]
iss.features <- colnames(all.resid$lang_ext)[-1]
lang.feat.all <- c(psvc.features,iss.features)
################################################################################
################################################################################
################################################################################
################################################################################
read_rds("data/m1m2.rds") %>% select(te_id, FSIQ) %>%
  inner_join(read_rds("data/derivatives/PSVC-USE-THIS.rds")$per_participant_features%>%
               select(te_id,unique_word_count)) %>%
  ggplot(aes(unique_word_count, FSIQ))+
  geom_point(shape=1)+ geom_smooth(method="lm",se=F,color=redblu.col.2[2])+
  ci_ribbon1+ggpubr::stat_cor(color=palette.1[1]) +
  bw.theme+ labs(x="total unique word count",y="FSIQ")
ggsave2("figs/word-count-to-iq.pdf",3,2.8)
################################################################################
################################################################################
################################################################################
################################################################################

registerDoMC(5)
models.comparisons <- foreach(i=1:length(iq.domains),.combine = rbind) %dopar%{
  foreach(j=1:length(lang.feat.all),.combine = rbind) %dopar% {
    
    base = ifelse(grepl("___",lang.feat.all[j]),
                  paste0(sub("___.*","",lang.feat.all[j]),"___","unique_word_count"),
                  "ALL___unique_word_count")
    
    if(lang.feat.all[j]==base){return(NULL)}
    df <- m125 %>% select(y=iq.domains[i],"base" = base,x=lang.feat.all[j]) %>% drop_na()
    if(sd(df$x)==0){return(NULL)}
    base.m <- lm(y~base,data=df)
    solo.m <- lm(y~x,data=df)
    base.solo.m <- lm(y~.,data=df)
    
    data.frame(iq_test = iq.domains[i], language_feature=lang.feat.all[j],
               base_model_r_squared = summary(base.m)$adj.r,
               base_model_direction = ifelse(summary(base.m)$coef[2,1]>0,"up","down"),
               base_model_pval = summary(base.m)$coef[2,4],
               feature_model_r_squared = summary(solo.m)$adj.r,
               feature_model_direction = ifelse(summary(solo.m)$coef[2,1]>0,"up","down"),
               feature_model_pval = summary(solo.m)$coef[2,4],
               base_feature_model_r_squared=summary(base.solo.m)$adj.r)%>%
      cbind(anova(base.m,base.solo.m)[-1,c("F","Pr(>F)")]%>%rename(F_statistic=1,pval=2) %>%
              as.data.frame())
  }
}
models.comparisons %>% write_rds("data/derivatives/solo-vs-base-model.results.rds")
models.comparisons <- read_rds("data/derivatives/solo-vs-base-model.results.rds")

models.comparisons %>% 
  filter(feature_model_pval< 0.05)%>%
  mutate(feature_model = paste0(signif(feature_model_r_squared,3), " (", 
                                signif(feature_model_pval,3),")"), 
         ANOVA = paste0(signif(F_statistic,3)," (",signif(pval,3),")"), 
         y=str_replace_all(iq_test,"_"," "), delta_r = signif(abs(base_feature_model_r_squared) - abs(base_model_r_squared),3)%>%as.character(),
         source = sub("___.*","",language_feature), 
         feature = sub(".*___","",language_feature)) %>% 
  left_join(categ.df %>% select(feature, feature_clean2, cat_detailed)) %>%
  select(y, feature_clean2, source, cat_detailed, feature_model, delta_r,ANOVA) %>% 
  mutate(source = case_when(source=="ALL"~"combined",source=="WAT"~"semantic fluency", 
                            source=="COWAT"~"phonemic fluency",T~"")) %>% 
  # arrange(y,source,cat_detailed,feature_clean2) %>%
  pivot_longer(c(feature_model, ANOVA,delta_r)) %>%
  filter(source!="") %>%
  mutate(nn = paste0(source,"__",name)) %>%
  pivot_wider(names_from = nn, values_from = value, id_cols = c(y, feature_clean2, cat_detailed))%>%
  mutate(y = factor(y,unique(categ.df$feature_clean2[1:50]))) %>%
  arrange(y,cat_detailed) %>% mutate_all(.funs = function(x) replace_na(x," ")) %>%
  write_tsv("data/derivatives/sig-solo-vs-base-model.results_V2.tsv")



models.comparisons %>% mutate(diff = feature_model_r_squared-base_model_r_squared,imp=pval<0.05) %>%
  select(iq_test,language_feature,diff,imp, feature_model_direction,feature_model_pval)%>%
  separate(language_feature, into=c("task","lang_feature"),sep="___")%>%
  inner_join(categ.df%>%filter(source=="PSVC")%>%select(lang_feature=feature,lang_clean=feature_clean2,lang_cat=cat))%>%
  left_join(categ.df%>%select(iq_test=feature,iq_clean=feature_clean,iq_cat=cat))%>%
  mutate(task=case_when(task=="WAT"~"semantic fluency",task=="COWAT"~"phonemic fluency",T~"ALL")) %>%
  ggplot(aes(iq_clean,lang_clean,fill=diff,
             label=paste0(ifelse(imp,"*",""),
                          ifelse(feature_model_pval<0.05,ifelse(feature_model_direction=="up","\u2206", "\u2207"),""))))+
  geom_tile()+redblu.col.gradient.2(label="R squared\n(feature model - word count model)")+
  geom_text()+
  my.guides + ggh4x::facet_grid2(rows = vars(lang_cat),cols=vars(task),scales="free",space="free")+
  bw.theme+labs(x="",y="")+theme(axis.text.x.bottom = element_text(angle=90,hjust=1,vjust=0.5))+
  labs(caption=paste0("* (ANOVA) significant difference between:    (IQ~word count) and (IQ~word count + feature)",
                      "\n\u2206 significant positive ",beta," coefficient (IQ~feature)",
                      "\n\u2207 significant negative ",beta," coefficient (IQ~feature)"))+
  theme(strip.text.y.right = element_text(angle = 0))
ggsave2("figs/model-comparison-heatmap.pdf",14,20)

################################################################################
################################################################################