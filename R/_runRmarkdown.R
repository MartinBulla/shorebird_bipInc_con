require(rmarkdown)

rmarkdown::render('R/zDraft.R', output_dir = 'Output',  output_file = 'MS_draft_v1.html')

rmarkdown::render('R/Out_Robutstness_check.r', output_dir = 'Output',  output_file = 'Out_Robustness_check.html')
