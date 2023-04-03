i = 1

tmp <- supplemental[,i]

tmp1 <- na.omit(tmp)

tmp2 <- apply(tmp1,2,as.factor)

g1 <- tmp2 %>%  ggplot(aes(x = fct_infreq(tmp2[,1]))) +
  geom_bar() + labs(x = supp_names[i], y='Frequency')

i = 2

tmp <- supplemental[,i]

tmp1 <- na.omit(tmp)

tmp2 <- apply(tmp1,2,as.factor)

g2 <- tmp2 %>%  ggplot(aes(x = fct_infreq(tmp2[,1]))) +
  geom_bar() + labs(x = supp_names[i], y='Frequency')


i = 3

tmp <- supplemental[,i]

tmp1 <- na.omit(tmp)

tmp2 <- apply(tmp1,2,as.factor)

g3 <- tmp2 %>%  ggplot(aes(x = fct_infreq(tmp2[,1]))) +
  geom_bar() + labs(x = supp_names[i], y='Frequency')

i = 4

tmp <- supplemental[,i]

tmp1 <- na.omit(tmp)

tmp2 <- apply(tmp1,2,as.factor)

g4 <- tmp2 %>%  ggplot(aes(x = fct_infreq(tmp2[,1]))) +
  geom_bar() + labs(x = supp_names[i], y='Frequency')


ggarrange(g1, g2, g3, g4,
          ncol = 2, nrow = 2)

kps <- table1_dat[,11]

colnames(kps) <- c('KPS score')

ggplot(data = kps)+
  geom_density(aes(kps$`KPS score`))+
  labs(x='KPS score',y='')
