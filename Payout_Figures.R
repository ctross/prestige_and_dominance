
################################################################## Payouts SC
######################### Give
 A_Give_Alter_SC = A_Give_SC
 diag(A_Give_Alter_SC) = 0

 G_Self_SC = diag(A_Give_SC)
 G_Others_SC = colSums(A_Give_Alter_SC)


######################### Leave
 A_Leave_Alter_SC = A_Leave_SC
 diag(A_Leave_Alter_SC) = 0

 L_Self_SC = diag(A_Leave_SC)
 L_Others_SC = colSums(A_Leave_Alter_SC)


######################### Reduce
 A_Reduce_Alter_SC = A_Reduce_SC
 diag(A_Reduce_Alter_SC) = 0

 R_Self_SC = diag(A_Reduce_SC)
 R_Others_SC = colSums(A_Reduce_Alter_SC)

########################## Sum
 Self_SC = normalize(G_Self_SC + 0.5*L_Self_SC + R_Self_SC)
 Others_SC = normalize(G_Others_SC + 0.5*L_Others_SC - 4*R_Others_SC)

 ###### Dom
 Dom = colSums(A_Dominant_SC)
 Pas = colSums(A_Passive_SC)

 Res = colSums(A_Respect_SC)
 Con = colSums(A_Contempt_SC)

 Dom2 = normalize(Dom - Pas)

 Res2 = normalize(Res - Con)


 df_plot_SC = data.frame(Status = c(Dom2, Dom2, Res2, Res2), 
                         Type = rep(c("Dominance", "Dominance", "Prestige", "Prestige"), each=length(Res2)),
                         Payout = c(Self_SC, Others_SC, Self_SC, Others_SC),
                         Direction = rep(c("Self", "Others","Self", "Others"), each=length(Self_SC)),
                         Site = "Lowland"
                         )

 df_plot_SC_2 = data.frame(Dom = Dom2, 
                           Resp = Res2,  
                           Site = "Lowland"
                         )

############################################################### Payouts BS
######################### Give
 A_Give_Alter_BS = A_Give_BS
 diag(A_Give_Alter_BS) = 0

 G_Self_BS = diag(A_Give_BS)
 G_Others_BS = colSums(A_Give_Alter_BS)


######################### Leave
 A_Leave_Alter_BS = A_Leave_BS
 diag(A_Leave_Alter_BS) = 0

 L_Self_BS = diag(A_Leave_BS)
 L_Others_BS = colSums(A_Leave_Alter_BS)


######################### Reduce
 A_Reduce_Alter_BS = A_Reduce_BS
 diag(A_Reduce_Alter_BS) = 0

 R_Self_BS = diag(A_Reduce_BS)
 R_Others_BS = colSums(A_Reduce_Alter_BS)

########################## Sum
 Self_BS = normalize(G_Self_BS + 0.5*L_Self_BS + R_Self_BS)
 Others_BS = normalize(G_Others_BS + 0.5*L_Others_BS - 4*R_Others_BS)

 ###### Dom
 Dom = colSums(A_Dominant_BS)
 Pas = colSums(A_Passive_BS)

 Res = colSums(A_Respect_BS)
 Con = colSums(A_Contempt_BS)

 Dom2 = normalize(Dom - Pas)

 Res2 = normalize(Res - Con)


 df_plot_BS = data.frame(Status = c(Dom2, Dom2, Res2, Res2), 
                         Type = rep(c("Dominance", "Dominance", "Prestige", "Prestige"), each=length(Res2)),
                         Payout = c(Self_BS, Others_BS, Self_BS, Others_BS),
                         Direction = rep(c("Self", "Others","Self", "Others"), each=length(Self_BS)),
                         Site = "Coast"
                         )

 df_plot_BS_2 = data.frame(Dom = Dom2, 
                           Resp = Res2,  
                           Site = "Coast"
                         )
################################################################## Payouts SU
######################### Give
 A_Give_Alter_SU = A_Give_SU
 diag(A_Give_Alter_SU) = 0

 G_Self_SU = diag(A_Give_SU)
 G_Others_SU = colSums(A_Give_Alter_SU)


######################### Leave
 A_Leave_Alter_SU = A_Leave_SU
 diag(A_Leave_Alter_SU) = 0

 L_Self_SU = diag(A_Leave_SU)
 L_Others_SU = colSums(A_Leave_Alter_SU)


######################### Reduce
 A_Reduce_Alter_SU = A_Reduce_SU
 diag(A_Reduce_Alter_SU) = 0

 R_Self_SU = diag(A_Reduce_SU)
 R_Others_SU = colSums(A_Reduce_Alter_SU)

########################## Sum
 Self_SU = normalize(G_Self_SU + 0.5*L_Self_SU + R_Self_SU)
 Others_SU = normalize(G_Others_SU + 0.5*L_Others_SU - 4*R_Others_SU)

 ###### Dom
 Dom = colSums(A_Dominant_SU)
 Pas = colSums(A_Passive_SU)

 Res = colSums(A_Respect_SU)
 Con = colSums(A_Contempt_SU)

 Dom2 = normalize(Dom - Pas)

 Res2 = normalize(Res - Con)


 df_plot_SU = data.frame(Status = c(Dom2, Dom2, Res2, Res2), 
                         Type = rep(c("Dominance", "Dominance", "Prestige", "Prestige"), each=length(Res2)),
                         Payout = c(Self_SU, Others_SU, Self_SU, Others_SU),
                         Direction = rep(c("Self", "Others","Self", "Others"), each=length(Self_SU)),
                         Site = "Altiplano"
                         )

 df_plot_SU_2 = data.frame(Dom = Dom2, 
                           Resp = Res2,  
                           Site = "Altiplano"
                         )

################################################################## Payouts TB
######################### Give
 A_Give_Alter_TB = A_Give_TB
 diag(A_Give_Alter_TB) = 0

 G_Self_TB = diag(A_Give_TB)
 G_Others_TB = colSums(A_Give_Alter_TB)


######################### Leave
 A_Leave_Alter_TB = A_Leave_TB
 diag(A_Leave_Alter_TB) = 0

 L_Self_TB = diag(A_Leave_TB)
 L_Others_TB = colSums(A_Leave_Alter_TB)


######################### Reduce
 A_Reduce_Alter_TB = A_Reduce_TB
 diag(A_Reduce_Alter_TB) = 0

 R_Self_TB = diag(A_Reduce_TB)
 R_Others_TB = colSums(A_Reduce_Alter_TB)

########################## Sum
 Self_TB = normalize(G_Self_TB + 1*L_Self_TB + R_Self_TB)
 Others_TB = normalize(G_Others_TB + 1*L_Others_TB - 4*R_Others_TB)

 ###### Dom
 Dom = colSums(A_Dominant_TB)
 Pas = colSums(A_Passive_TB)

 Res = colSums(A_Respect_TB)
 Con = colSums(A_Contempt_TB)

 Dom2 = normalize(Dom - Pas)

 Res2 = normalize(Res - Con)

 df_plot_TB = data.frame(Status = c(Dom2, Dom2, Res2, Res2), 
                         Type = rep(c("Dominance", "Dominance", "Prestige", "Prestige"), each=length(Res2)),
                         Payout = c(Self_TB, Others_TB, Self_TB, Others_TB),
                         Direction = rep(c("Self", "Others","Self", "Others"), each=length(Self_TB)),
                         Site = "Highland"
                         )

 df_plot_TB_2 = data.frame(Dom = Dom2, 
                           Resp = Res2,  
                           Site = "Highland"
                         )

############################################
df_plot = rbind(df_plot_SC, df_plot_BS, df_plot_SU, df_plot_TB)

df_plot$Site = factor(df_plot$Site)
df_plot$Site = factor(df_plot$Site, levels=c("Coast", "Lowland", "Highland", "Altiplano"))

df_plot1 = df_plot[which(df_plot$Direction=="Self"),]
df_plot2 = df_plot[which(df_plot$Direction=="Others"),]

cols = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)

p1 = ggplot(df_plot1, aes(x = Status, y = Payout, color = Site, fill = Site)) +
  geom_point() + theme(legend.position="bottom") + facet_grid(Type ~ Site, scales="free") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7]))

p2 = ggplot(df_plot2, aes(x = Status, y = Payout, color = Site, fill = Site)) +
  geom_point() + theme(legend.position="bottom") + facet_grid(Type ~ Site, scales="free") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7]))

ggsave("payouts_from_self.pdf", p1, width=10, height=6)
ggsave("payouts_from_others.pdf", p2, width=10, height=6)


df_plot = rbind(df_plot_SC_2, df_plot_BS_2, df_plot_SU_2, df_plot_TB_2)
p3 = ggplot(df_plot, aes(x = Resp, y = Dom, color = Site, fill = Site)) +
  geom_point() + theme(legend.position="bottom") + facet_grid(. ~ Site, scales="free") +
  geom_smooth(method=lm) +
  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7]))





############################################# Social perception
con = colSums(A_Contempt_BS)
res = colSums(A_Respect_BS)
dom = colSums(A_Dominant_BS)
pas = colSums(A_Passive_BS)
fear = standardize(colSums(A_Fear_BS))
like = standardize(colSums(A_Like_BS))
cent = standardize(eigen_centrality(graph_from_adjacency_matrix(A_Friend_BS))$vector)

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_bs = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, cent),Outcome=rep(c("Liked","Feared","Centrality"),each=length(like)), Site="Coast")

con = colSums(A_Contempt_SC)
res = colSums(A_Respect_SC)
dom = colSums(A_Dominant_SC)
pas = colSums(A_Passive_SC)
fear = standardize(colSums(A_Fear_SC))
like = standardize(colSums(A_Like_SC))
cent = standardize(eigen_centrality(graph_from_adjacency_matrix(A_Friend_SC))$vector)

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_sc = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, cent),Outcome=rep(c("Liked","Feared","Centrality"),each=length(like)), Site="Lowland")

con = colSums(A_Contempt_SU)
res = colSums(A_Respect_SU)
dom = colSums(A_Dominant_SU)
pas = colSums(A_Passive_SU)
fear = standardize(colSums(A_Fear_SU))
like = standardize(colSums(A_Like_SU))
cent = standardize(eigen_centrality(graph_from_adjacency_matrix(A_Friend_SU))$vector)

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_su = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, cent),Outcome=rep(c("Liked","Feared","Centrality"),each=length(like)), Site="Altiplano")

con = colSums(A_Contempt_TB)
res = colSums(A_Respect_TB)
dom = colSums(A_Dominant_TB)
pas = colSums(A_Passive_TB)
fear = standardize(colSums(A_Fear_TB))
like = standardize(colSums(A_Like_TB))
cent = standardize(eigen_centrality(graph_from_adjacency_matrix(A_Friend_TB))$vector)

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_tb = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, cent),Outcome=rep(c("Liked","Feared","Centrality"),each=length(like)), Site="Highland")

data_lf = rbind(data_lf_bs, data_lf_sc, data_lf_su, data_lf_tb)

data_lf = data_lf[which(data_lf$Outcome == "Centrality"),]

data_lf$Site = factor(data_lf$Site)
data_lf$Site = factor(data_lf$Site, levels=c("Coast", "Lowland", "Highland", "Altiplano"))

p3 = ggplot(data=data_lf, aes(x=Prestigious, y=Dominant)) +
  geom_point(aes(size=Perception, color=Site, fill=Site),shape=21,alpha = 0.2) +
  geom_point(aes(size=Perception, color=Site),shape=21) + 
  facet_grid(Outcome ~ Site, scale="free")+
    scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_size(range = c(0.01, 5),guide = "none") + theme(legend.position="none")  

ggsave("DomPrestPerception_Cent.pdf", p3, width=10, height=3)




############################################# Social perception
con = colSums(A_Contempt_BS)
res = colSums(A_Respect_BS)
dom = colSums(A_Dominant_BS)
pas = colSums(A_Passive_BS)
fear = standardize(colSums(A_Fear_BS))
like = standardize(colSums(A_Like_BS))
trust = standardize(colSums(A_Trusted_BS))

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_bs = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, trust),Outcome=rep(c("Liked","Feared","Trusted"),each=length(like)), Site="Coast")

con = colSums(A_Contempt_SC)
res = colSums(A_Respect_SC)
dom = colSums(A_Dominant_SC)
pas = colSums(A_Passive_SC)
fear = standardize(colSums(A_Fear_SC))
like = standardize(colSums(A_Like_SC))
trust = standardize(colSums(A_Trusted_SC))

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_sc = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, trust),Outcome=rep(c("Liked","Feared","Trusted"),each=length(like)), Site="Lowland")

con = colSums(A_Contempt_SU)
res = colSums(A_Respect_SU)
dom = colSums(A_Dominant_SU)
pas = colSums(A_Passive_SU)
fear = standardize(colSums(A_Fear_SU))
like = standardize(colSums(A_Like_SU))
trust = standardize(colSums(A_Trusted_SU))

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_su = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, trust),Outcome=rep(c("Liked","Feared","Trusted"),each=length(like)), Site="Altiplano")

con = colSums(A_Contempt_TB)
res = colSums(A_Respect_TB)
dom = colSums(A_Dominant_TB)
pas = colSums(A_Passive_TB)
fear = standardize(colSums(A_Fear_TB))
like = standardize(colSums(A_Like_TB))
trust = standardize(colSums(A_Trusted_TB))

dom2 = standardize(dom-pas)
res2 = standardize(res-con)

data_lf_tb = data.frame(Dominant = c(dom2,dom2,dom2), Prestigious = c(res2,res2,res2), Perception = c(like, fear, trust),Outcome=rep(c("Liked","Feared","Trusted"),each=length(like)), Site="Highland")

data_lf = rbind(data_lf_bs, data_lf_sc, data_lf_su, data_lf_tb)

data_lf = data_lf[which(data_lf$Perception != "Centrality"),]

data_lf$Site = factor(data_lf$Site)
data_lf$Site = factor(data_lf$Site, levels=c("Coast", "Lowland", "Highland", "Altiplano"))

p3 = ggplot(data=data_lf, aes(x=Prestigious, y=Dominant)) +
  geom_point(aes(size=Perception, color=Site, fill=Site),shape=21,alpha = 0.2) +
  geom_point(aes(size=Perception, color=Site),shape=21) + 
  facet_grid(Outcome ~ Site, scale="free")+
    scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
  scale_size(range = c(0.01, 5),guide = "none") + theme(legend.position="none")  

ggsave("DomPrestPerception.pdf", p3, width=10, height=8)
