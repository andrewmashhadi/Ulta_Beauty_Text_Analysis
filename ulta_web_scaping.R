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
    
    brand_df <- data.frame(matrix(ncol = 6, nrow = 0))
    
    #provide column names
    colnames(brand_df) <- c('id', 'class', 'text', 'reviews', 'rating', 'price')
    
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
  
      
        ##### product info #####
        product_info <- html_dat %>%
                          html_nodes('.Text-ds.Text-ds--subtitle-1.Text-ds--left') %>%
                          html_text()
        
        ##### product type #####
        xx <- html_dat %>%
                html_nodes('.Breadcrumbs__List--item') %>%
                html_text()
        
        prod_type <- gsub(" ", "", xx[length(xx)])

        ##### details #####
        product_details <- html_dat %>%
                             html_nodes('.Accordion_Huge') %>%
                             html_text()
        
        product_text <- paste(product_info, product_details[1], sep=" ", collapse = " ")
        
        ##### price #####
        pr_price <- html_dat %>%
          html_nodes('.ProductPricing') %>%
          html_text() 
        
        pr_price <- gsub("\\$", "", pr_price)
        pr_price <- as.double(gsub(" ", "", pr_price))
        
        ##### reviews and ratings #####
        revs <- html_dat %>%
                  html_nodes('.ProductReviews') %>%
                  html_nodes('p') %>% 
                  html_text()
      
        if (length(revs)==17) {
          
          revs <- revs[-c(1, 2)]
          pr_rating <- mean(as.double(revs[c(3, 6, 9, 12, 15)-2]))
          revs <- revs[c(3, 6, 9, 12, 15)]
          
        } else if (length(revs)==14) {
          
          revs <- revs[-c(1, 2)]
          pr_rating <- mean(as.double(revs[c(3, 6, 9, 12)-2]))
          revs <- revs[c(3, 6, 9, 12)]
          
        } else if (length(revs)==11) {
          
          revs <- revs[-c(1, 2)]
          pr_rating <- mean(as.double(revs[c(3, 6, 9)-2]))
          revs <- revs[c(3, 6, 9)]
          
        } else if (length(revs)==8) {
          
          revs <- revs[-c(1, 2)]
          pr_rating <- mean(as.double(revs[c(3, 6)-2]))
          revs <- revs[c(3, 6)]
          
        } else if (length(revs)==5) {
          
          revs <- revs[-c(1, 2)]
          pr_rating <- mean(as.double(revs[c(3)-2]))
          revs <- revs[3]
          
        } else if (length(revs)==15) {
            
          pr_rating <- mean(as.double(revs[c(3, 6, 9, 12, 15)-2]))
          revs <- revs[c(3, 6, 9, 12, 15)]
          
        } else if (length(revs)==12) {
          
          pr_rating <- mean(as.double(revs[c(3, 6, 9, 12)-2]))
          revs <- revs[c(3, 6, 9, 12)]
          
        } else if (length(revs)==9) {
          
          pr_rating <- mean(as.double(revs[c(3, 6, 9)-2]))
          revs <- revs[c(3, 6, 9)]
          
        } else if (length(revs)==6) {
          
          pr_rating <- mean(as.double(revs[c(3, 6)-2]))
          revs <- revs[c(3, 6)]
          
        } else if (length(revs)==3) {
          
          pr_rating <- mean(as.double(revs[c(3)-2]))
          revs <- revs[c(3)]
          
        } else {
          
          pr_rating <- -1
        }

      }
      
      # concatenate all the reviews into one string
      #revs_tot <- paste(revs, collapse = " ") 
      
      if (length(revs) > 0) {
        df <- data.frame(id=item_number, class=prod_type, text=product_text, 
                         reviews=revs, rating=pr_rating, price=pr_price)
        brand_df <- rbind(brand_df, df)
      }
    
      
    }
    
    # write the brand dataframe
    write.csv(brand_df, paste0(PATH_ULTA_TEXT_DATA, "/", brand, ".csv"))
    
  }
  
}


