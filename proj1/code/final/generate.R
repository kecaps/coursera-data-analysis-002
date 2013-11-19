
# load packages
require(knitr)
# require(markdown)

setwd("~/classes/dataAnalysis002/proj1")
srcDir <- file.path("code", "final")
outDir <- 'out'
if (!file.exists(outDir)) {
  dir.create(outDir)
}
setwd(outDir)
srcDir <- file.path("..", srcDir)

rmdFiles <- c("loansFinal.Rmd", "loansFigure.Rmd")
for (file in rmdFiles) { 
  srcFile <- file.path(srcDir, file)
  rootName <- strsplit(basename(file), '\\.')[[1]][1]
  mdFile <- paste(sep='.', rootName, 'md')
  htmlFile <- paste(sep='.', rootName, "html")
  pdfFile <- paste(sep='.', rootName, "pdf")

  knit(input=srcFile, output=mdFile)

  # use pandoc for prettier footnotes
  # markdownToHTML(file=mdFile, output=htmlFile)
  
  # could use pandoc() cmd, but cannot turn on extensions since it 
  # hard-codes -f markdown
  system(paste(sep=" ", "pandoc -S -s -f markdown+autolink_bare_uris -t html5",
                        "--mathjax", mdFile, "-o", htmlFile))
  system(paste(sep=" ", "pandoc -S -s -f markdown+autolink_bare_uris",
                        "--latex-engine=xelatex", mdFile, "-o", pdfFile))
}

