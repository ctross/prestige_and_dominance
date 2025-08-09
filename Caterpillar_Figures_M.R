############################################################################################################ DR
######################################## Process TB
tab_TB_dr = multiplex_plot_d(fit_TB_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE, mode="adj")
tab_TB_dr$x = 1:length(tab_TB_dr$rs_m)

tab_TB_dr = tab_TB_dr[which(tab_TB_dr$measure2 %in% c("Dominant\n(i to j)", "Prestigous\n(i to j)")),]
tab_TB_dr$Site = "Highland"

######################################## Process SU
tab_SU_dr = multiplex_plot_d(fit_SU_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE, mode="adj")
tab_SU_dr$x = 1:length(tab_SU_dr$rs_m)

tab_SU_dr = tab_SU_dr[which(tab_SU_dr$measure2 %in% c("Dominant\n(i to j)", "Prestigous\n(i to j)")),]

tab_SU_dr$Site = "Altiplano"

######################################## Process SC
tab_SC_dr = multiplex_plot_d(fit_SC_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE, mode="adj")
tab_SC_dr$x = 1:length(tab_SC_dr$rs_m)

tab_SC_dr = tab_SC_dr[which(tab_SC_dr$measure2 %in% c("Dominant\n(i to j)", "Prestigous\n(i to j)")),]

tab_SC_dr$Site = "Lowland"

######################################## Process BS
tab_BS_dr = multiplex_plot_d(fit_BS_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE, mode="adj")
tab_BS_dr$x = 1:length(tab_BS_dr$rs_m)

tab_BS_dr = tab_BS_dr[which(tab_BS_dr$measure2 %in% c("Dominant\n(i to j)", "Prestigous\n(i to j)")),]

tab_BS_dr$Site = "Coast"



######################################## Merged
tab_dr = rbind(tab_SU_dr, tab_TB_dr, tab_SC_dr, tab_BS_dr)
tab_dr = tab_dr[which(tab_dr$measure1 %in% c("Like\n(i to j)", "Fear\n(i to j)", "Trust\n(i to j)", "Distrust\n(i to j)", "Give\n(i to j)", "Reduce\n(i to j)", "Take\n(i to j)", "Friend\n(i to j)")),]
tab_dr$Valence = ifelse(tab_dr$measure1 %in% c("Like\n(i to j)", "Trust\n(i to j)",  "Give\n(i to j)", "Friend\n(i to j)"), "Positive", "Negative")
tab_dr$Valence = factor(tab_dr$Valence)
tab_dr$Valence = factor(tab_dr$Valence, levels=c("Positive", "Negative"))

tab_dr$Site = factor(tab_dr$Site)
tab_dr$Site = factor(tab_dr$Site, levels=c("Coast", "Lowland", "Highland", "Altiplano"))


c1 = ggplot(tab_dr, aes(x = measure1, y = rs_m, group = Site, color=Site, ymin = l, ymax = h)) + 
        geom_hline(aes(yintercept = 0), linetype="dashed") + 
        geom_linerange(size = 1,, position = position_dodge(width = 0.6)) + 
        geom_point(size = 2,, position = position_dodge(width = 0.6))  + facet_grid(Valence ~measure2, scales = "free") +
        coord_flip() +  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        labs(y = "Regression parameters", x = "") + 
        theme(strip.text.x = element_text(size = 12, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), axis.title.y = element_text(size = 14, face = "bold"), axis.title.x = element_blank()) +
        theme(strip.text.y = element_text(angle = 360)) +  theme(panel.spacing = grid::unit(1, "lines")) + theme(legend.position="bottom")


############################################################################################################ GR
######################################## Process TB
tab_TB_gr = multiplex_plot_g(fit_TB_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE)
tab_TB_gr$x = 1:length(tab_TB_gr$rs_m)

tab_TB_gr = tab_TB_gr[which(tab_TB_gr$measure2 %in% c("Dominant\n(receiver)", "Prestigous\n(receiver)")),]
tab_TB_gr$Site = "Highland"

######################################## Process SU
tab_SU_gr = multiplex_plot_g(fit_SU_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE)
tab_SU_gr$x = 1:length(tab_SU_gr$rs_m)

tab_SU_gr = tab_SU_gr[which(tab_SU_gr$measure2 %in% c("Dominant\n(receiver)", "Prestigous\n(receiver)")),]
tab_SU_gr$Site = "Altiplano"

######################################## Process SC
tab_SC_gr = multiplex_plot_g(fit_SC_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE)
tab_SC_gr$x = 1:length(tab_SC_gr$rs_m)

tab_SC_gr = tab_SC_gr[which(tab_SC_gr$measure2 %in% c("Dominant\n(receiver)", "Prestigous\n(receiver)")),]
tab_SC_gr$Site = "Lowland"


######################################## Process BS
tab_BS_gr = multiplex_plot_g(fit_BS_M, HPDI=0.9, export_as_table = TRUE, plot = FALSE)
tab_BS_gr$x = 1:length(tab_BS_gr$rs_m)

tab_BS_gr = tab_BS_gr[which(tab_BS_gr$measure2 %in% c("Dominant\n(receiver)", "Prestigous\n(receiver)")),]
tab_BS_gr$Site = "Coast"

######################################## Merged
tab_gr = rbind(tab_SU_gr, tab_TB_gr, tab_SC_gr, tab_BS_gr)
tab_gr = tab_gr[which(tab_gr$measure1 %in% c("Like\n(receiver)", "Fear\n(receiver)", "Trust\n(receiver)", "Distrust\n(receiver)", "Give\n(receiver)", "Reduce\n(receiver)", "Take\n(receiver)", "Friend\n(receiver)")),]
tab_gr$Valence = ifelse(tab_gr$measure1 %in% c("Like\n(receiver)", "Trust\n(receiver)",  "Give\n(receiver)", "Friend\n(receiver)"), "Positive", "Negative")
tab_gr$Valence = factor(tab_gr$Valence)
tab_gr$Valence = factor(tab_gr$Valence, levels=c("Positive", "Negative"))

tab_gr$Site = factor(tab_gr$Site)
tab_gr$Site = factor(tab_gr$Site, levels=c("Coast", "Lowland", "Highland", "Altiplano"))



c2 = ggplot(tab_gr, aes(x = measure1, y = rs_m, group = Site, color=Site, ymin = l, ymax = h)) + 
        geom_hline(aes(yintercept = 0), linetype="dashed") + 
        geom_linerange(size = 1,, position = position_dodge(width = 0.6)) + 
        geom_point(size = 2,, position = position_dodge(width = 0.6))  + facet_grid(Valence ~measure2, scales = "free") +
        coord_flip() +  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        labs(y = "Regression parameters", x = "") + 
        theme(strip.text.x = element_text(size = 12, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), axis.title.y = element_text(size = 14, face = "bold"), axis.title.x = element_blank()) +
        theme(strip.text.y = element_text(angle = 360)) +  theme(panel.spacing = grid::unit(1, "lines")) + theme(legend.position="bottom")


ggsave("dr_all_sites_M.pdf", c1, width=8, height=6)
ggsave("gr_all_sites_M.pdf", c2, width=8, height=6)


tab_dr_M = tab_dr
tab_dr_M$Gender = "M"

tab_gr_M = tab_gr
tab_gr_M$Gender = "M"
