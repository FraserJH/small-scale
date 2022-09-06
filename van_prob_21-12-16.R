## 2021-12-16
## code for the probability of capture analysis/figures

## libraries
library(here); library(tidyverse); library(sjPlot); library(ggarrange); library(cowplot)

## data
dat<-read.csv("data/fid_van.csv")

#####remove the Chaets since not looking at them, and Zebrasoma as only 1 individual
dat<-dat %>% filter(Family != "Chaetodontidae")
dat<-dat %>% filter(Genus != "Zebrasoma")
dat %>%
  group_by(Area, Genus) %>% summarise(n = n())

## probabilities of capture where if can approach closer than effective range P = 1
dat <- dat %>%
  mutate(pf_capture = ifelse(FID > 337, 0,1), ##mean of effective max range
         pf_capture_low = ifelse(FID > 305,0,1), ## lower CI of effective max range
         pf_capture_high = ifelse(FID > 365,0,1)) ## upper CI of effective max range

#### separate into families and standardize by size due to potential differences in size
#### across areas
scar<-dat %>%filter(Family == "Scaridae", size >14, size < 50)
acan<-dat %>%filter(Family == "Acanthuridae",size > 14)
acan$z_size <- as.numeric(scale(acan$size, center = TRUE, scale = TRUE))
scar$z_size <- as.numeric(scale(scar$size, center = TRUE, scale = TRUE))

## Binomial regression (for all areas)
m1a<-glm(pf_capture~ z_size*Area ,family = binomial(link = 'cloglog'), data = scar)
summary(m1a)

m2a<-glm(pf_capture~  z_size* Area,family = binomial(link = 'cloglog'), data = acan)
summary(m2a)

### construct the marginal plots of capture probability vs size for each area/family
### repeat for low or high using 'pf_capture_low' and 'pf_capture_high' for supplemental
### figures

### SCARIDAE FISHED
scar_f <- scar %>% filter(Area == 'Fished')
m3<-glm(pf_capture~ size, family = binomial(link = 'cloglog'), data = scar_f)
ilink <- family(m3)$linkinv
nd <- with(scar_f,
           data.frame(size = seq(15, max(size),
                                 length = 100)))
pd <- predict(m3, newdata = nd, type = "link", se.fit = TRUE)

pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (1.96 * se.fit)),
                Lower = ilink(fit - (1.96 * se.fit)),
                Upper8 = ilink(fit + (1.26 * se.fit)),
                Lower8 = ilink(fit - (1.26 *se.fit)))
pd$size <- nd$size


(scar_fish<-ggplot(scar_f, aes(x = size, y = as.numeric(pf_capture)), title = "Fished")+
  geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
              fill = "#deeefd", alpha = 1, inherit.aes = FALSE) +
  geom_line(data = pd, aes(y = Fitted, x = size), colour ="#49b3df") +
 # geom_line(data = pd, aes(y = Upper8, x = size), colour = "#49b3df", lty = 2)+
  #  geom_line(data = pd, aes(y = Lower8, x = size), colour = "#49b3df", lty = 2)+
  geom_point(position = position_dodge2(width = 1))+  scale_x_continuous(limits= c(20,35),
                                                                  name = "Size (cm TL)")+
  scale_y_continuous(name = "Probability of capture") + theme_bw()+
    theme(panel.grid  = element_blank())+ggtitle('Fished')+
geom_point(x = 26, y = 0.112, pch = 21, fill = 'grey', size = 3))

#### SCARIDAE PHC ####
scar_phc<-scar %>%filter(Area == 'PHC')
m4<-glm(pf_capture~ size , family = binomial(link = 'cloglog'), data = scar_phc)
ilink <- family(m4)$linkinv
nd <- with(scar_phc,
           data.frame(size = seq(20, max(size),
                                 length = 100)))
pd <- predict(m4, newdata = nd, type = "link", se.fit = TRUE)

pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (1.97 * se.fit)),
                Lower = ilink(fit - (1.97 * se.fit)), Upper8 = ilink(fit + (1.26 * se.fit)),
                Lower8 = ilink(fit - (1.26 *se.fit)))
pd$size <- nd$size
(scar_PHC<-ggplot(scar_phc, aes(x = size, y = as.numeric(pf_capture)))+
    geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
                fill = "#c9f4e0ff", alpha = 1, inherit.aes = FALSE) +
    geom_line(data = pd, aes(y = Fitted, x = size), colour ="43d4a2") +
 #   geom_line(data = pd, aes(y = Upper8, x = size), colour = "43d4a2", lty = 2)+
  #  geom_line(data = pd, aes(y = Lower8, x = size), colour = "43d4a2", lty = 2)+
    geom_point(position = position_dodge2(width = 1))+  scale_x_continuous(name = "Size (cm TL)")+
    geom_point(x = 26, y = 0.48, pch = 21, fill = 'grey', size = 3)+
    scale_y_continuous(name = " ") + theme_bw()+ theme(panel.grid  = element_blank())+ggtitle('Partially protected area *'))


#### SCARIDAE PROTECTED ####
scar_prot<-scar %>%filter(Area == 'MPA')
m5<-glm(pf_capture~ size, family = binomial(link = 'cloglog'), data = scar_prot)
ilink <- family(m5)$linkinv
pd <- predict(m5, type = "link", se.fit = TRUE)
pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (1.28 * se.fit)),
                Lower = ilink(fit - (1.28 * se.fit)))

pd$size <- scar_prot$size
(scar_Prot<-ggplot(scar_prot, aes(x = size, y = as.numeric(pf_capture)), title = "Fished")+
    geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
                fill = "#fad2d3ff", alpha = 1, inherit.aes = FALSE) +
    geom_line(data = pd, aes(y = Fitted, x = size), colour ="#e46f72ff") +
    geom_point(x = 26, y = 0.82, pch = 21, fill = 'grey', size = 3)+
    geom_point(position = position_dodge2(width = 1))+  scale_x_continuous(name = "Size (cm TL)")+
    scale_y_continuous(name = " ") + theme_bw()+ theme(panel.grid  = element_blank())+ggtitle('No-take *'))


#### ACANTHURIDAE FISHED ####
acan_f<-acan %>%filter(Area == 'Fished')

m6<-glm(pf_capture~ size , family = binomial, data = acan_f)
ilink <- family(m6)$linkinv
pd <- with(acan_f,
           data.frame(size = seq(min(size), max(size),
                                 length = 100)))
pd <- cbind(pd, predict(m6, pd, type = "link", se.fit = TRUE)[1:2])
pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit)))
(acan_fish<-ggplot(acan_f, aes(x = size, y = as.numeric(pf_capture)), title = "Fished") +
    geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
                fill = "#deeefd", alpha = 1, inherit.aes = FALSE) +
    geom_line(data = pd, aes(y = Fitted, x = size), colour ="#49b3df") +
    geom_point(x = 22, y = 0.217, pch = 21, fill = 'grey', size = 3)+
    geom_point(position = position_dodge2(width = 1))+  scale_x_continuous(name = "Size (cm TL)")+
    scale_y_continuous(name = "Probability of capture") + theme_bw()+ theme(panel.grid  = element_blank())+ggtitle('Fished *'))


#### ACANTHURIDAE PHC ####
acan_phc<-acan %>%filter(Area == 'PHC')

m7<-glm(pf_capture~ size, family = binomial, data = acan_phc)
ilink <- family(m7)$linkinv
pd <- with(acan_phc,
           data.frame(size = seq(min(size), max(size),
                                 length = 100)))
pd <- cbind(pd, predict(m7, pd, type = "link", se.fit = TRUE)[1:2])
pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit)))

(acan_PHC<-ggplot(acan_phc, aes(x = size, y = as.numeric(pf_capture))) +
    geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
                fill = "#c9f4e0ff", alpha = 1, inherit.aes = FALSE) +
    geom_line(data = pd, aes(y = Fitted, x = size), colour = "43d4a2") +
    geom_point(position = position_dodge2(width = 1))+  scale_x_continuous(name = "Size (cm TL)")+
    geom_point(x = 22, y = 0.53, pch = 21, fill = 'grey', size = 3)+
    scale_y_continuous(name = " ") + theme_bw()+ theme(panel.grid  = element_blank())+ggtitle('Partially protected area'))


#### ACANTHURIDAE PROTECTED ####
acan_prot<-dat %>%filter(Area == 'MPA', Family == "Acanthuridae")

m8<-glm(pf_capture~ size, family = binomial, data = acan_prot)
ilink <- family(m8)$linkinv
pd <- with(acan_prot,
           data.frame(size = seq(min(size), max(size),
                                 length = 100)))
pd <- cbind(pd, predict(m8, pd, type = "link", se.fit = TRUE)[1:2])
pd <- transform(pd, Fitted = ilink(fit), Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit)))

(acan_P<-ggplot(acan_prot, aes(x = size, y = as.numeric(pf_capture))) +
    geom_ribbon(data = pd, aes(ymin = Lower, ymax = Upper, x = size),
                fill = "#fad2d3ff", alpha = 1, inherit.aes = FALSE) +
    geom_line(data = pd, aes(y = Fitted, x = size), colour = "#e46f72ff") +
    geom_point(position = position_dodge2(width = 1))+
    scale_x_continuous(name = "Size (cm TL)")+
    geom_point(x = 22, y = 0.852, pch = 21, fill = 'grey', size = 3)+
    scale_y_continuous(name = " ") + theme_bw() + theme(panel.grid  = element_blank())+ggtitle('No-take'))

###get the model outputs
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

### put the graphs together
ggarrange( scar_fish, scar_PHC, scar_Prot, nrow=1, ncol =3)
ggarrange(acan_fish, acan_PHC, acan_P, scar_fish, scar_PHC, scar_Prot, nrow=2, ncol =3)
ggsave('Prob_capture_fig.pdf', height = 8, width = 12)
# ggsave('supple_spear-low.pdf', height = 8, width = 12)
# ggsave('supple_spear-low.pdf', height = 8, width = 12)

