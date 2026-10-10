## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(knitr.table.format = "html"),
  options(knitr.kable.max_rows = 30)
)

## ----echo=FALSE, error=FALSE--------------------------------------------------
    find_extdata <- function(filename) {
      candidates <- c(
        system.file("extdata", filename, package = "ACRER"),
        file.path("..", "extdata", filename),
        file.path("..", "inst", "extdata", filename),
        file.path(".", "extdata", filename),
        file.path(".", "inst", "extdata", filename)
      )
      for (p in candidates) {
        if (nchar(p) > 0 && file.exists(p)) return(p)
      }
      return("")
    }
    d2_path <- find_extdata("dataset2.RData")
    if (nchar(d2_path) > 0 && file.exists(d2_path)) load(d2_path)
    d3_path <- find_extdata("dataset3.RData")
    if (nchar(d3_path) > 0 && file.exists(d3_path)) load(d3_path)

## ----echo=FALSE, error=FALSE--------------------------------------------------
    library(knitr)
    is_docx <- knitr::pandoc_to("docx")
    is_html <- knitr::is_html_output()

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(flextable)
   library(kableExtra)
   ActInfo <- data.frame(ActInfo)
   if(is_docx){
     ft <- flextable(head(ActInfo, n = 10))
     ft <- autofit(ft)
     ft <- add_header_lines(ft, values = "Table 1: ActInfo")
     ft
     } else if(is_html){ x_html <- kable(ActInfo, row.names = FALSE, 
    col.names = c('activity_pattern','maximum_number_of_times_per_activity'), format = "html", digits = 2, 
    align = 'c',caption = 'Table 1: ActInfo')
    kable_classic_2(x_html, full_width = F,  position = "center", fixed_thead = T)} else {
    x_pdf <- kable(ActInfo, format = "latex",align = 'c', digits = 2, caption = 'ActInfo')
    kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(flextable)
   library(kableExtra)
   TimeInfo <- data.frame(TimeInfo)
   if (is_docx) {
   ft <- flextable(head(TimeInfo, n = 4)) 
   ft <- autofit(ft,add_w = 0.5) 
   ft <- add_header(x = ft, Time_segment = "Table 2:TimeInfo", top = TRUE)
   ft <- height_all(ft,height = 0.7, part = 'all')
   ft
   } else if(is_html) { 
   x_html <- kable(file = TimeInfo, head(TimeInfo), format = "html",align = 'c',caption = "Table 2: TimeInfo")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(TimeInfo, format = "latex",caption = "TimeInfo")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(rmarkdown)
   library(kableExtra)
   library(flextable)
   data7 <- data.frame(data7)
   if (is_docx) {
   ft0 <- flextable(head(data7)) 
   ft0 <- autofit(ft0) 
   ft0 <- add_header_row(x = ft0, top = TRUE, values = "Table 3: data7", colwidths = 6) 
   ft0
   } else if(is_html)  { 
   x_html <- kable(file = data7, head(data7), format = "html",align = 'c',caption = "Table 3: data7",col.names = c('rows','groupsRecr_act_ts','col1','col2','col3','col4'))
   kable_classic_2(x_html, full_width = F, position = "center")  
   } else {x_pdf <- kable(data7, format = "latex",caption = "Table 3: data7")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   library(rmarkdown)
   c3 <- data.frame(c3)
   if (is_docx) {
   ft <- flextable(head(c3, n = 9)) 
   ft <- autofit(ft) 
   ft <- add_header_row(x = ft, top = TRUE, values = "Table 4: c3 size(9x3)", colwidths = 4) 
   ft
   } else if(is_html) { 
   y_html <- kable(c3,row.names = FALSE,col.names = c('rows','col1','col2','col3'), format = "html", align =  'c', caption = "Table 4: c3 size(9x3)")
   kable_classic_2(y_html, full_width = F,position = "center")     
   } else {x_pdf <- kable(c3, format = "latex",caption = "c3 size(9x3)")
   kable_styling(x_pdf, position = "center",font_size = 10, fixed_thead = TRUE)}   

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   p <- data.frame(p)
   if (is_docx) {
   ft1 <- flextable(head(p, n = 9)) 
   ft1 <- autofit(ft1) 
   ft1 <- add_header_row(x = ft1, top = TRUE, values = "Table 5: p (size: 9x4)", colwidths = 5) 
   ft1
   } else if(is_html) { 
   x_html <- kable(p, row.names = FALSE,col.names = c('rows','col1','col2','col3','col4'),format = "html",align = 'c',caption = "Table 5: p (size: 9x4)")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(p, format = "latex",caption = "Table 5: p (size: 9x4)")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   NumberAvail <- data.frame(NumberAvail)
   if (is_docx) {
   ft2 <- flextable(head(NumberAvail)) 
   ft2 <- autofit(ft2) 
   ft2 <- add_header_row(x = ft2,top = TRUE, values = "Table 6: NumberAvail",colwidths = 2) 
   ft2
   } else if(is_html) { 
   x_html <- kable(NumberAvail,row.names = FALSE,col.names = c('Time segment','NumberAvail'),align = 'c',caption = "Table 6:NumberAvail")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(NumberAvail, format = "latex",caption = "NumberAvail")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   timeMax <- data.frame(timeMax)
   if (is_docx) {
   ft3 <- flextable(head(timeMax)) 
   ft3 <- autofit(ft3, part = 'all') 
   ft3 <- add_header_row(x = ft3,top = TRUE, values = "Table7:timeMax",colwidths = 2) 
   ft3
   } else if(is_html) { 
   x_html <- kable(timeMax, row.names = FALSE,col.names = c('rows','timeMax'),align = 'c',caption = "Table 7:timeMax")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(timeMax, format = "latex",caption = "timeMax")
   kable_styling(x_pdf, position = "center",font_size = 10)}   

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   lin_cond_max <- data.frame(lin_cond_max)
   if (is_docx) {
   ft4 <- flextable(head(lin_cond_max, n = 29)) 
   ft4 <- width(ft4, width = 1) 
   ft4 <- add_header_row(x = ft4, top = TRUE, values = "Table 8: lin_cond_max", colwidths = 2) 
   ft4
    } else if(is_html)  { 
   y_html <- kable(lin_cond_max,row.names = FALSE,col.names = c('rows','lin_cond_max'), format = "html", align = 'c', caption = "Table 8: lin_cond_max")
   kable_classic_2(y_html, full_width = F,position = "center")
   } else {x_pdf <- kable(lin_cond_max, format = "latex", align = 'c', caption = 'lin')
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   Table_l1 <- data.frame(Table_l1)
   if (is_docx)   {
   ft6 <- flextable(head(Table_l1,cwidth = 4)) 
   ft6 <- autofit(ft6) 
   ft6 <- add_header_row(x = ft6,   top = TRUE, values = "Table :Tablell1", colwidths = 4) 
   } else if(is_html) { 
   x_html <- kable(Table_l1, row.names = FALSE,col.names = c('activity_pattern','ActMax','gl1','l1'), format="html",align = 'c',caption = "Table 9:Tablel1")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(Table_l1, format = "latex",caption = "NumberAvail")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   qp_solution <- data.frame(qp_solution)
   if (is_docx) {
   ft7 <- flextable(head(qp_solution,cwidth = 4)) 
   ft7 <- autofit(ft7) 
   ft7 <- add_header_row(x = ft7,   top = TRUE, values = "Table 10:qpsolution", colwidths = 2) 
    } else if(is_html)  { 
   x_html <- kable(qp_solution, row.names = FALSE,col.names = c('rows','qpsolution'), format="html",align = 'c',caption = "Table 10:qpsolution")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(qp_solution, format = "latex",caption = "qpsolution")
   kable_styling(x_pdf, position = "center",font_size = 10)}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
   library(knitr)
   library(kableExtra)
   library(flextable)
   linear_solution <- data.frame(linear_solution)
   if (is_docx) {
   ft8 <- flextable(head(linear_solution,cwidth = 4)) 
   ft8 <- autofit(ft8) 
   ft8 <- add_header_row(x = ft8,   top = TRUE, values = "Table 11:linearsolution", colwidths = 4) 
    } else if(is_html)  { 
   x_html <- kable(linear_solution, row.names = FALSE,col.names = c('rows','units','time_segment','linear_solution'), format = "html",align = 'c',caption = "Table 11:linearsolution")
   kable_classic_2(x_html, full_width = F, position = "center")
   } else {x_pdf <- kable(linear_solution, format = "latex",caption = "linearsolution")
   kable_styling(x_pdf, position = "center",font_size = 10)}

