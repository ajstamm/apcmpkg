#' Read PDF plots and convert to png

read_crossplot_pdf <- function(path, file, page) {
  pdf <- paste0(path, "/", file, ".pdf")
  img <- pdftools::pdf_render_page(pdf, dpi = 600, page = page)
  png <- paste0("images/", file, page, ".png")
  png::writePNG(img, png)
}
