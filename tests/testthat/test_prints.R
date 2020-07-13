test_that("test prints",{


  fh <- fairness_heatmap(fobject)
  print(fh)

  cm <- choose_metric(fobject)
  print(cm)

  print(fobject)

  fpca <- fairness_pca(fobject)
  print(fpca)

  fheatmap <- fairness_heatmap(fobject)
  print(fheatmap)

  gm <- group_metric(fobject)
  prt <- print(gm)

  sm <- stack_metrics(fobject)
  print(sm)

  fr <- fairness_radar(fobject)
  print(fr)

  ac <- all_cutoffs(fobject)
  print(ac)

  cpc <- ceteris_paribus_cutoff(fobject, "Asian")
  print(cpc)


})
