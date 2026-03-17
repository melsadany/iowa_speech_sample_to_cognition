################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
demo <- read_csv("../shared_data/data/demo-full.csv")
lang.all.0 <- read_rds("data/derivatives/PSVC-USE-THIS.rds")
iss.all.0 <- read_rds("data/derivatives/ISS_USE-THIS.rds")
m1.m2 <- read_rds("../shared_data/data/m1m2.rds")
categ.df.0 <- read_rds("../shared_data/data/vars-categories.rds")
m123 <- inner_join(m1.m2,lang.all.0$per_participant_features)%>%
  left_join(lang.all.0$raw_features$PS_faces_duration)%>%
  left_join(lang.all.0$raw_features$RAN_duration)
m124 <- inner_join(m1.m2, iss.all.0$per_type)%>%
  inner_join(iss.all.0$raw_features$bigram_accuracy%>%dplyr::filter(prompt=="all")%>%select(te_id,accurate_bigram_percentage_score))
################################################################################
################################################################################
################################################################################
## sparse PLS
library(mixOmics)
X <- m123 %>% left_join(m124%>%dplyr::select(-colnames(m1.m2)[-1])) %>%
  dplyr::select(-colnames(m1.m2)[c(1:2,10:28)],-c(19:39,44:52)) %>%
  mutate_all(.funs = function(x) scale(x,T,T)[,1])
X2 <- X %>% 
  dplyr::select(-ends_with("age_corrected_standard_score"))

Y <- m123 %>% dplyr::select(colnames(m1.m2)[c(19:26)]) %>%
  mutate_all(.funs = function(x) scale(x,T,T)[,1])

x.keep <-1
x2.keep <-1
n.comp <- ncol(Y)

## with NIH-TB
mo.spls <- spls(X,Y, ncomp = n.comp, mode = "canonical",keepX = rep(x.keep,n.comp), keepY = rep(1,n.comp))
sign.flip <- mo.spls$loadings$Y %>%as.data.frame() %>% pivot_longer(cols = c(1:n.comp)) %>% filter(value!=0) %>% dplyr::rename(vv=value)
## no NIH-TB
mo.spls2 <- spls(X2,Y, ncomp = n.comp, mode = "canonical",keepX = rep(x2.keep,n.comp), keepY = rep(1,n.comp))
sign.flip2 <- mo.spls2$loadings$Y %>%as.data.frame() %>% pivot_longer(cols = c(1:n.comp)) %>% filter(value!=0) %>% dplyr::rename(vv=value)


rbind(rbind(mo.spls$loadings$X %>% as.data.frame() %>% mutate(block = "language"),
            mo.spls$loadings$Y %>% as.data.frame() %>% mutate(block = "IQ")) %>% mutate(X = "X: language + NIH-TB") %>%
        rownames_to_column("feature") %>%
        pivot_longer(cols = starts_with("comp"))%>%
        left_join(sign.flip) %>% mutate(value = value*vv),
      rbind(mo.spls2$loadings$X %>% as.data.frame() %>% mutate(block = "language"),
            mo.spls2$loadings$Y %>% as.data.frame() %>% mutate(block = "IQ")) %>% mutate(X = "X: language") %>%
        rownames_to_column("feature")%>%
        pivot_longer(cols = starts_with("comp"))%>%
        left_join(sign.flip2) %>% mutate(value = value*vv)) %>%
  left_join(categ.df.0) %>%
  mutate(cat = factor(cat,levels=unique(categ.df.0$cat)),
         name=sub("comp","C",name),source=factor(source,levels=unique(categ.df.0$source)[c(2,1,3,4)]),
         feature_clean=factor(feature_clean, levels=unique(categ.df.0$feature_clean))) %>%
  ggplot(aes(x=name,y=feature_clean, fill = value,label=ifelse(value!=0,round(value,2),""))) +
  geom_tile() + redblu.col.gradient.2(label = "loading") + my.guides + 
  ggh4x::facet_nested(rows = vars(source,cat), cols = vars(X), scales = "free", space = "free",
                      nest_line=element_line(linewidth=0.6),solo_line=T) +
  bw.theme + theme(strip.text.y.right = element_text(angle=0),axis.text.x.bottom = element_text(angle = 90,hjust = 1,vjust = 0.5))+
  labs(x="", y="", 
       caption = paste0("n(samples): ", nrow(X),"\n",
                        "sparse PLS where Y is the IQ metrics.\n",
                        "X was forced to retain top ",x.keep," and Y to retain top feature per component\n",
                        "raw values without correction for both X and Y\n"))
ggsave2("figs/spls-res_subtests-iq-vs-lang-w-wo-nih.png",9,18)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
