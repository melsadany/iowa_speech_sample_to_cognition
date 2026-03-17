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
## CCA
cca.2 <- RGCCA::rgcca(blocks = list("cog"=m123%>%dplyr::select(19:26),"language"=m123%>%dplyr::select(29:114,116:117)), 
                      ncomp = 6, sparsity = c(0.43,0.35),scale = T)
cor.v <- cor(cca.2$Y$cog,cca.2$Y$language) %>% as.data.frame() %>% rownames_to_column("V1") %>%pivot_longer(cols = -V1) %>%
  filter(V1==name)%>% mutate(dir=sign(value))

rbind(do.call(cbind,lapply(1:ncol(cca.2$a$language),function(x) cca.2$a$language[,x] * cor.v$dir[x])),
      cca.2$a$cog)%>%
  as.data.frame()%>%rownames_to_column("feature")%>%
  pivot_longer(cols = -c(feature))%>%mutate(name=sub("V","C",name))%>%
  left_join(categ.df.0) %>%mutate(cat=ifelse(source=="IQ",source,cat),
                                  cat=factor(cat,levels=c("IQ",unique(categ.df.0$cat))),
                                  feature_clean=factor(feature_clean,levels=unique(categ.df.0$feature_clean)))%>%
  ggplot(aes(name,feature_clean,fill=value))+
  geom_tile()+redblu.col.gradient.2()+my.guides+
  ggh4x::facet_grid2(rows = vars(cat),scales = "free",space="free")+
  bw.theme+
  labs(x="",y="")
################################################################################
################################################################################
################################################################################
## factor analysis
library(psych)
fa.lang <- fa(m123%>%dplyr::select(-colnames(m1.m2)),
              nfactors = 4,fm="ml",)
fa.lang %>% write_rds("data/derivatives/PSVC-features-FA.rds",compress = "gz")

# plot
p1 <- cbind(loadings(fa.lang)) %>% as.data.frame() %>%
  rename_with(~paste0("F",seq_along(.)))%>%
  rownames_to_column("feature") %>%
  pivot_longer(cols = -feature) %>%
  left_join(categ.df.0) %>%
  mutate(ss="") %>%
  full_join(data.frame(feature_clean = "cumulative variance",
                       value = as.numeric(fa.lang$Vaccounted[3,]),
                       name = paste0("F",c(1:length(fa.lang$Vaccounted[3,]))),
                       cat="",ss=""))%>%
  mutate(feature_clean=factor(feature_clean,levels=c(unique(categ.df.0$feature_clean),"cumulative variance")))%>%
  ggplot(aes(name,feature_clean,fill=value,label=ifelse(feature_clean=="cumulative variance",round(value,3),""))) +
  geom_tile() + redblu.col.gradient.2(label="loading")+
  geom_text(size=3)+
  ggh4x::facet_grid2(rows = vars(cat),scales = "free",space="free")+
  my.guides+bw.theme+null_labs

# img_ME(fa.lang$loadings)
# heatmap(scale(cbind(fa.lang$scores,m123%>%dplyr::select(19:26))))
p2 <- corr.table(fa.lang$scores%>%as.data.frame()%>%rename_with(~paste0("F",seq_along(.))),m123%>%dplyr::select(19:26))%>%
  filter(V2%in%colnames(m1.m2),!V1%in%colnames(m1.m2))%>%mutate(V2=factor(V2,levels=unique(categ.df.0$feature)))%>%
  ggplot(aes(V1,V2,fill=r))+geom_tile()+redblu.col.gradient.2()+my.guides+
  bw.theme+null_labs
patchwork::wrap_plots(p1,p2,heights = c(11,1))
ggsave2("figs/PSVC-features-FA.png",5,17)
################################################################################
################################################################################
################################################################################
## archetypes on features
library(archetypes)
feat.arch <- archetypes(scale(m123%>%dplyr::select(-colnames(m1.m2))),k = 5)
simplexplot(feat.arch)
################################################################################
################################################################################
################################################################################
