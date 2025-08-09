################################################################## Table
make_descriptives = function(A1, A2, A3, A4, B){
  diag(A1) = 0
  diag(A2) = 0
  diag(A3) = 0
  diag(A4) = 0

  res1 = res2 = res3 = res4 =c()
  net = graph_from_adjacency_matrix(A1, mode = c("directed"))
  res1[1] = B
  res1[2] = gorder(net)
  res1[3] = gsize(net)
  res1[4] = round(edge_density(net, loops = FALSE),3)
  res1[5] = round(reciprocity(net, ignore.loops = TRUE, mode = c("default")),3)
  res1[6] = round(transitivity(net),3)
  res1[7] = round(mean(igraph::degree(net,mode="total")),1)
  res1[8] = paste0(min(igraph::degree(net,mode="out")),"--", max(igraph::degree(net,mode="out")))
  res1[9] = paste0(min(igraph::degree(net,mode="in")),"--", max(igraph::degree(net,mode="in")))

  net = graph_from_adjacency_matrix(A2, mode = c("directed"))
  res2[1] = B
  res2[2] = gorder(net)
  res2[3] = gsize(net)
  res2[4] = round(edge_density(net, loops = FALSE),3)
  res2[5] = round(reciprocity(net, ignore.loops = TRUE, mode = c("default")),3)
  res2[6] = round(transitivity(net),3)
  res2[7] = round(mean(igraph::degree(net,mode="total")),1)
  res2[8] = paste0(min(igraph::degree(net,mode="out")),"--", max(igraph::degree(net,mode="out")))
  res2[9] = paste0(min(igraph::degree(net,mode="in")),"--", max(igraph::degree(net,mode="in")))

  net = graph_from_adjacency_matrix(A3, mode = c("directed"))
  res3[1] = B
  res3[2] = gorder(net)
  res3[3] = gsize(net)
  res3[4] = round(edge_density(net, loops = FALSE),3)
  res3[5] = round(reciprocity(net, ignore.loops = TRUE, mode = c("default")),3)
  res3[6] = round(transitivity(net),3)
  res3[7] = round(mean(igraph::degree(net,mode="total")),1)
  res3[8] = paste0(min(igraph::degree(net,mode="out")),"--", max(igraph::degree(net,mode="out")))
  res3[9] = paste0(min(igraph::degree(net,mode="in")),"--", max(igraph::degree(net,mode="in")))

  net = graph_from_adjacency_matrix(A4, mode = c("directed"))
  res4[1] = B
  res4[2] = gorder(net)
  res4[3] = gsize(net)
  res4[4] = round(edge_density(net, loops = FALSE),3)
  res4[5] = round(reciprocity(net, ignore.loops = TRUE, mode = c("default")),3)
  res4[6] = round(transitivity(net),3)
  res4[7] = round(mean(igraph::degree(net,mode="total")),1)
  res4[8] = paste0(min(igraph::degree(net,mode="out")),"--", max(igraph::degree(net,mode="out")))
  res4[9] = paste0(min(igraph::degree(net,mode="in")),"--", max(igraph::degree(net,mode="in")))

  bob = rbind(res1,res2,res3,res4)

  return(cbind(c("Coastal", "Lowland", "Highland", "Altiplano"), bob))
}


bob1 = make_descriptives(A_Friend_BS, A_Friend_SC, A_Friend_TB, A_Friend_SU, "Friendship")

bob2 = make_descriptives(ifelse(A_Give_BS>0, 1, 0), ifelse(A_Give_SC>0, 1, 0), ifelse(A_Give_TB>0, 1, 0), ifelse(A_Give_SU>0, 1, 0), "Give")
bob3 = make_descriptives(ifelse(A_Leave_BS>0, 0, 1), ifelse(A_Leave_SC>0, 0, 1), ifelse(A_Leave_TB>0, 0, 1), ifelse(A_Leave_SU>0, 0, 1), "Take")
bob4 = make_descriptives(ifelse(A_Reduce_BS>0, 1, 0), ifelse(A_Reduce_SC>0, 1, 0), ifelse(A_Reduce_TB>0, 1, 0), ifelse(A_Reduce_SU>0, 1, 0), "Reduce")

bob5 = make_descriptives(A_Respect_BS, A_Respect_SC, A_Respect_TB, A_Respect_SU, "Prestigious")
bob6 = make_descriptives(A_Contempt_BS, A_Contempt_SC, A_Contempt_TB, A_Contempt_SU, "Contemptible")
bob7 = make_descriptives(A_Passive_BS, A_Passive_SC, A_Passive_TB, A_Passive_SU, "Passive")
bob8 = make_descriptives(A_Dominant_BS, A_Dominant_SC, A_Dominant_TB, A_Dominant_SU, "Dominant")

bob9 = make_descriptives(A_Trusted_BS, A_Trusted_SC, A_Trusted_TB, A_Trusted_SU, "Trust")
bob10 = make_descriptives(A_Untrusted_BS, A_Untrusted_SC, A_Untrusted_TB, A_Untrusted_SU, "Distrust")
bob11 = make_descriptives(A_Like_BS, A_Like_SC, A_Like_TB, A_Like_SU, "Like")
bob12 = make_descriptives(A_Fear_BS, A_Fear_SC, A_Fear_TB, A_Fear_SU, "Fear")

bob = rbind(bob1,bob2,bob3,bob4,bob5,bob6,bob7,bob8,bob9,bob10,bob11,bob12)
write.csv(bob,"descriptives.csv")
