rmarkdown::render("R/prem_sing_feat_result.Rmd",
                  output_format = "html_document",
                  output_dir = "Results/")

rmarkdown::render("R/prem_sing_poly_feat_result.Rmd",
                  output_format = "html_document",
                  output_dir = "Results/")
