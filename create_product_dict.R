# load libraries
library(dplyr)
library(xml2)
library(rvest)

## define data path
PATH_ULTA_TEXT_DATA <- Sys.getenv("ULTA_TEXT_DATA")

## in brand page
brands <- read.csv(paste0(PATH_ULTA_TEXT_DATA, '/brandnames', '/brandnames.csv'))

# clean names for url
brands <- tolower(brands[[1]])
brands <- gsub(" \\& ", "-", brands)
brands <- gsub(" ", "-", brands)
brands <- gsub("\\'", "", brands)
brands <- gsub("\\.", "", brands)

# init product dictionary
prod_df <- data.frame(matrix(ncol = 2, nrow = 0))

# provide column names
colnames(prod_df) <- c('id', 'name')

# collect data
for (brand in brands) {
  
  html_main <-
    try(
      read_html(paste0('https://www.ulta.com/brand/', brand)),
      silent=TRUE
    )
  
  if("try-error" %in% class(html_main)) {
    
    cat("Could not read URL:", paste0('https://www.ulta.com/brand/', brand), "\n")
    break
    
  }
  else {
    
    cat("\n\nReading brand:", brand, "\n\n")
    
    pcs <- html_main %>% 
      html_nodes('.ProductCard') 
    
    for (i in 1:length(pcs)) {
      
      ilink <- pcs[i] %>%  
        html_nodes('a') %>%
        html_attr('href')
      
      html_dat <-
        try(
          read_html(ilink),
          silent=TRUE
        )
      
      if("try-error" %in% class(html_dat)) {
        
        cat("Could not read URL:", ilink, "\n")
        break
        
      } else {
        
        cat("Reading product URL:", ilink, "\n")
        
        ##### item number #####
        xxt <- html_dat %>%
          html_nodes('p.Text-ds.Text-ds--body-3.Text-ds--left.Text-ds--neutral-600') %>%
          html_nodes('span.Text-ds.Text-ds--body-3.Text-ds--left') %>% 
          html_text()
        
        item_number <- xxt[length(xxt)]
        item_number <- gsub("Item ", "", item_number)
        
        ##### item name #####
        pr_name <- html_dat %>%
          html_nodes('.Text-ds.Text-ds--title-5.Text-ds--left') %>%
          html_text()
        
        
      df <- data.frame(id=item_number, name=pr_name)
      prod_df <- rbind(prod_df, df)
      
      }
    }
  }
}

# write the item dataframe
write.csv(prod_df, paste0(PATH_ULTA_TEXT_DATA, "/product_dict.csv"))


