require(rmarkdown)

rmarkdown::render('R/zDraft.R', output_dir = 'Output',  output_file = 'MS_draft_v1.html')