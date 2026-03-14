for (p in c("DT", "plotly", "htmlwidgets", "webshot2"))
  cat(p, ":", system.file(package = p) != "", "\n")
