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
################################################################################
################################################################################
################################################################################
################################################################################
demo <- read_csv("data/demo-full.csv")
load("data/all-data-raw-and-resid-for-age-and-sex-and-their-PCs.rda")
m12 <- inner_join(all.resid$iq, all.resid$nih)
m123 <- inner_join(m12, all.resid$lang_all_and_sep)
################################################################################
################################################################################
################################################################################
################################################################################
## word count predicting IQ
m123 %>% select(colnames(all.resid$iq)[-1], contains("word_count")) %>%
  pivot_longer(cols = -c(contains("word_count"))) %>%
  pivot_longer(cols = contains("word_count"), names_to = "task", values_to = "wc")%>%
  mutate(task=case_when(grepl("COWAT",task)~"phonemic fluency",
                        grepl("WAT",task)~"semantic fluency",
                        T~"ALL"),
         name=factor(name, unique(categ.df$feature))) %>%
  ggplot(aes(wc, value, color = task))+
  geom_point(shape=1, alpha=0.2)+geom_smooth(method="lm",se=F)+ggpubr::stat_cor(size=3,show.legend = F)+
  ci_ribbon.multi+ scale_color_manual(values = palette.1[c(3,1,2)], name="")+
  facet_wrap(~name, scales = "free")+bw.theme+
  labs(x="word count (corrected for age and sex effects)", y="IQ test score (corrected for age and sex effects)")
ggsave2("figs/word-count-to-iq-by-task.pdf", 10,8)

# forest plot
registerDoMC(4)
wc.to.iq.lm <- foreach(i=2:ncol(all.resid$iq), .combine = rbind)%dopar% {
  iq.d <- colnames(all.resid$iq)[i]
  df <- m123 %>% select(iq.d, contains("word_count"))
  do.call(rbind,lapply(c("ALL","COWAT","WAT"),function(t) 
    coefs_table(lm(as.formula(paste0(iq.d, "~ ", t, "___unique_word_count")),data=df))))%>%
    filter(!grepl("Intercept",x))%>% mutate(y=iq.d,x=sub("___.*","",x))%>%relocate(y)
}
wc.to.iq.lm %>% mutate(task=case_when(x=="COWAT"~"phonemic fluency",x=="WAT"~"semantic fluency",T~x),
                       FDR=p.adjust(pval,"fdr"))%>%
  left_join(categ.df%>%select(y=feature,feature_clean2,cat_detailed))%>%
  filter(task!="ALL")%>%
  ggplot(aes(Estimate,feature_clean2,color=task,group=task,alpha=pval<0.05))+
  geom_vline(xintercept = 0,linetype=2,color="pink") + geom_point(position = position_dodge(0.6),aes(shape=FDR<0.05))+
  geom_errorbar(aes(xmin=confin_min,xmax=confin_max),position=position_dodge(0.6),height=0.1)+
  scale_color_manual(values = c(palette.1[c(1,2)]))+ggh4x::facet_grid2(rows=vars(cat_detailed),scales = "free",space="free")+
  bw.theme+labs(y="")+theme(legend.direction = "vertical",strip.text.y.right = element_text(angle=0))
ggsave2("figs/word-count-to-iq-by-task_forest.pdf", 6,7)

wc.to.iq.lm %>% mutate(task=case_when(x=="COWAT"~"phonemic fluency",x=="WAT"~"semantic fluency",T~x),
                       FDR=p.adjust(pval,"fdr"))%>%
  left_join(categ.df%>%select(y=feature,feature_clean2,cat_detailed)) %>%
  mutate(ess = paste0(signif(Estimate,3), " (",signif(confin_min,3),
                      "-",signif(confin_max,3),")"),
         pval = signif(pval,3), FDR = signif(FDR,3)) %>%
  select(y = feature_clean2, `word count source` = task, 
         `beta (LCI-UCI)`=ess, pval, FDR) %>% arrange(y, `word count source`) %>%
  write_tsv("data/derivatives/word-count-to-iq-by-task_forest.tsv")


## word count comparison in both
m123 %>%
  ggplot(aes(WAT___unique_word_count, COWAT___unique_word_count))+
  geom_point(shape=1,alpha=0.6)+geom_smooth(method="lm",se=F,color=redblu.col.2[2])+
  ggpubr::stat_cor(size=4,show.legend = F, color=palette.1[1])+
  ci_ribbon1+ bw.theme+
  labs(x="word count | semantic fluency \n(corrected for age and sex effects)", 
       y="word count | phonemic fluency \n(corrected for age and sex effects)")
ggsave2("figs/word-count-semantic-to-phonemic.pdf", 4,4)

################################################################################
################################################################################
################################################################################
## plot language to IQ
corr.table(m123[-1]) %>%
  filter(V1 %in% colnames(m12), V2 %in% colnames(all.resid$lang_all_and_sep)) %>%
  mutate(feature=sub(".*___","",V2),V1=sub("_NIH","",V1),
         task=ifelse(grepl("___",V2),sub("___.*","",V2),"ALL"),
         task=case_when(task=="WAT"~"semantic fluency",
                        task=="COWAT"~"phonemic fluency",
                        T~"ALL")) %>%
  left_join(categ.df) %>% 
  left_join(categ.df %>% select(V1=feature,V1_c=feature_clean2,cat1=cat)%>%mutate(V1=sub("_age_.*","",V1)))%>%
  ggplot(aes(V1_c,feature_clean2,fill=r,label=case_when(FDR<0.05~"*",pval<0.05~small.circle2,T~"")))+
  geom_tile()+my.guides+redblu.col.gradient.2(label = "r")+
  geom_text()+
  ggh4x::facet_nested(rows=vars(cat),cols=vars(task,cat1),scales="free",space="free",
                      nest_line=element_line(linewidth=0.6))+
  bw.theme+labs(x="",y="")+theme(axis.text.x.bottom = element_text(angle=90,hjust=1,vjust=0.5),
                                 strip.text.y.right = element_text(angle=0))+
  labs(caption=paste0("* FDR < 0.05","\n",small.circle2," pval < 0.05"))
ggsave2("figs/lang-task-to-cognition.pdf",13,20)
################################################################################
################################################################################
################################################################################
## highlighted features better than word count
wc.and.feat.to.iq.lm <- foreach(i=2:ncol(all.resid$iq), .combine = rbind)%dopar% {
  iq.d <- colnames(all.resid$iq)[i]
  df <- m123 %>% select(iq.d, starts_with(c("WAT","COWAT")))
  do.call(rbind,lapply(c("COWAT","WAT"),function(t) {
    df2 <- df %>% select(iq.d, starts_with(t))
    do.call(rbind,lapply(colnames(df2)[-1],function(feat){
      coefs_table(lm(as.formula(paste0(iq.d, "~ ", feat)),data=df2))[2,]
    })) %>% mutate(y=iq.d)%>%relocate(y)%>%separate(x,into=c("task","feature"),sep="___")
  }))
}
wc.and.feat.to.iq.lm %>% mutate(tt = ifelse(grepl("unique_word",feature),"base","x")) %>%
  filter(y%in%categ.df$feature[c(18)]&(tt=="base"|(grepl("mean_AoA|mean_GPT",feature)))|
           y%in%categ.df$feature[c(20)]&(tt=="base"|(grepl("sem__community_returns_sum|sem__communities",feature)))|
           y%in%categ.df$feature[c(21)]&(tt=="base"|(grepl("repetition|sem__sim_to_prompt",feature)))|
           y%in%categ.df$feature[c(24)]&(tt=="base"|(grepl("ks_statistic",feature)))|
           y%in%categ.df$feature[c(23)]&(tt=="base"|(grepl("spatial",feature)))|
           y%in%categ.df$feature[c(22)]&(tt=="base"|(grepl("Gini",feature)))) %>%
  left_join(categ.df%>%select(y=feature,iq_cl=feature_clean2,iq_cat=cat_detailed))%>%
  left_join(categ.df%>%select(feature,feature_clean2,cat))%>%
  mutate(x=case_when(tt=="base"~"word count",tt=="x"~"language feature"),
         task=case_when(task=="COWAT"~"phonemic fluency",T~"semantic fluency"))%>%
  ggplot(aes(Estimate,feature_clean2,color=x,alpha=pval<0.05))+
  geom_vline(xintercept = 0,linetype=2,color="pink") + geom_point(position = position_dodge(0.6))+
  geom_errorbar(aes(xmin=confin_min,xmax=confin_max),position=position_dodge(0.6),height=0.1)+
  scale_color_manual(values = c(palette.1[c(2,1)]))+ggh4x::facet_nested(rows=vars(iq_cat,iq_cl),cols=vars(task),scales = "free",space="free",
                                                                        nest_line=element_line(linewidth=0.6))+
  bw.theme+labs(y="")+theme(legend.direction = "vertical",strip.text.y.right = element_text(angle=0))
ggsave2("figs/word-count-vs-select-features-per-task-to-iq.pdf",10,5)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
feat.to.keep <- categ.df$feature[c(69,63,67,51,60,53,58,56,114,95,89,77,71,119,57)]

corr.table(m123[-1]) %>%
  filter(V1 %in% colnames(m12), V2 %in% colnames(all.resid$lang_all_and_sep)) %>%
  mutate(feature=sub(".*___","",V2),V1=sub("_NIH","",V1),
         task=ifelse(grepl("___",V2),sub("___.*","",V2),"ALL"),
         task=case_when(task=="WAT"~"semantic fluency",task=="COWAT"~"phonemic fluency",T~"ALL")) %>%
  left_join(categ.df) %>% filter(task!="ALL") %>%
  left_join(categ.df %>% select(V1=feature,V1_c=feature_clean2,cat1=cat)%>%mutate(V1=sub("_age_.*","",V1)))%>%
  filter(feature %in% feat.to.keep)%>% mutate(FDR=p.adjust(pval,"fdr"))%>%
  ggplot(aes(V1_c,feature_clean2,fill=r,label=case_when(FDR<0.05~"*",pval<0.05~small.circle2,T~"")))+
  geom_tile()+my.guides+redblu.col.gradient.2(label = "r")+
  geom_text()+
  ggh4x::facet_nested(rows=vars(cat),cols=vars(task,cat1),scales="free",space="free",
                      nest_line=element_line(linewidth=0.6))+
  bw.theme+labs(x="",y="")+theme(axis.text.x.bottom = element_text(angle=90,hjust=1,vjust=0.5),
                                 strip.text.y.right = element_text(angle=0))+
  labs(caption=paste0("* FDR < 0.05","\n",small.circle2," pval < 0.05"))
ggsave2("figs/lang-task-to-cognition_select.pdf",10,6.5)

## number of nominal significant correlations
corr.summ <- corr.table(m123[-1]) %>%
  filter(V1 %in% colnames(m12), V2 %in% colnames(all.resid$lang_all_and_sep)) %>%
  mutate(feature=sub(".*___","",V2),V1=sub("_NIH","",V1),
         task=ifelse(grepl("___",V2),sub("___.*","",V2),"ALL"),
         task=case_when(task=="WAT"~"semantic fluency",task=="COWAT"~"phonemic fluency",T~"combined")) %>%
  left_join(categ.df) %>% 
  left_join(categ.df %>% select(V1=feature,V1_c=feature_clean2,cat1=cat)%>%mutate(V1=sub("_age_.*","",V1)))%>%
  group_by(V1_c,task,cat1) %>% summarise(pval_significant=sum(pval<0.05),FDR_significant=sum(FDR<0.05))
corr.summ %>% pivot_longer(cols = c(pval_significant,FDR_significant))%>%
  mutate(name=sub("_significant"," < 0.05",name))%>%
  ggplot(aes(value,V1_c,fill=name))+
  geom_bar(stat="identity",width=0.4,position=position_dodge(0.6))+
  scale_fill_manual(values = palette.1,name="")+
  geom_text(aes(x=value+3,label=value),position=position_dodge(0.6),size=2)+
  ggh4x::facet_grid2(cols=vars(task),rows=vars(cat1),scales="free",space = "free")+
  bw.theme+labs(x="count of significant correlations", y="")
ggsave2("figs/lang-task-to-cognition-significance-count.pdf",6,7)
################################################################################
################################################################################