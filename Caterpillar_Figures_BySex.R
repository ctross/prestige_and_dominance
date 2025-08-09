############################################################################################################ DR
tab_dr_BySex = rbind(tab_dr_M, tab_dr_F)
tab_gr_BySex = rbind(tab_gr_M, tab_gr_F)

c1 = ggplot(tab_dr_BySex, aes(x = measure1, y = rs_m, group = interaction(Gender, Site), color=Site, ymin = l, ymax = h, linetype=Gender)) + 
        geom_hline(aes(yintercept = 0), linetype="dashed") + 
        geom_linerange(size = 1, position = position_dodge(width = 0.55)) + 
        geom_point(size = 2, position = position_dodge(width = 0.55))  + facet_grid(Valence ~measure2, scales = "free") +
        coord_flip() +  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        labs(y = "Regression parameters", x = "") + 
        theme(strip.text.x = element_text(size = 12, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), axis.title.y = element_text(size = 14, face = "bold"), axis.title.x = element_blank()) +
        theme(strip.text.y = element_text(angle = 360)) +  theme(panel.spacing = grid::unit(1, "lines")) + theme(legend.position="bottom")


c2 = ggplot(tab_gr_BySex, aes(x = measure1, y = rs_m, group = interaction(Gender, Site), color=Site, ymin = l, ymax = h, linetype=Gender)) + 
        geom_hline(aes(yintercept = 0), linetype="dashed") + 
        geom_linerange(size = 1, position = position_dodge(width = 0.55)) + 
        geom_point(size = 2, position = position_dodge(width = 0.55))  + facet_grid(Valence ~measure2, scales = "free") +
        coord_flip() +  scale_color_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
        labs(y = "Regression parameters", x = "") + 
        theme(strip.text.x = element_text(size = 12, face = "bold"), strip.text.y = element_text(size = 12, face = "bold"), 
              axis.text = element_text(size = 12), axis.title.y = element_text(size = 14, face = "bold"), axis.title.x = element_blank()) +
        theme(strip.text.y = element_text(angle = 360)) +  theme(panel.spacing = grid::unit(1, "lines")) + theme(legend.position="bottom")


ggsave("dr_all_sites_BySex.pdf", c1, width=12, height=8)
ggsave("gr_all_sites_BySex.pdf", c2, width=12, height=8)

