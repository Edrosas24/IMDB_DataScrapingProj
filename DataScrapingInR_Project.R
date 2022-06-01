#####Srape through one page####
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("eeptools")

library(rvest) # For web scraping
library(dplyr) # %>% 
library(eeptools) # For decomma()

# The webpage you want to scrape.
# Feature Film, Rating Count at least 25,000, Action (Sorted by US Box Office Descending)
link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc"
#Read the page
page = read_html(link) #Creates an html document from a url

# We want to scapp=er the Name, year, and Metascore, Gross, votes, Stars rating.
rank = page %>% html_nodes(".text-primary") %>% 
  html_text() %>% as.numeric()

name = page %>% html_nodes(".lister-item-header a") %>% # Selects parts of a document using CSS slectors
  html_text() # Extracts the texts from selected nodes (here, it got rid of the link tags)
# Make sure to deselect aany text you do not want

year = page %>% html_nodes(".text-muted.unbold") %>% 
  html_text()
year = year %>% 
  substr(start = 2, stop = 5) %>% as.numeric()

rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% 
  html_text() %>% as.numeric()

metascore = page %>% html_nodes(".metascore") %>%
  html_text()
metascore = metascore %>% 
  substr(start = 0, stop = 3) %>% as.numeric()

rating = page %>% html_nodes(".ratings-imdb-rating strong") %>%
  html_text() %>% as.numeric()

gross = page %>% html_nodes(".ghost~ .text-muted+ span") %>%
  html_text() 
gross = substr(gross, start = 2, stop = nchar(gross) - 1) %>% 
  as.numeric()

votes = page %>% html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% 
  html_text()
votes = votes %>% decomma()
  
synopsis= page %>% html_nodes(".ratings-bar+ .text-muted") %>% 
  html_text()


movies50 = data.frame(rank,name,year,rating,metascore,votes, gross, synopsis, stringsAsFactors = F) #prevent from factoring the synopsis 

# what is to come
# 1.How to go into individual pages
# 2. What happens when you have multiple pages of results
# 3. what if collector gadget does not give you the correct css tag

# 1. Nested Links
# Grab the movie links
movie_links = page %>% html_nodes(".lister-item-header a") %>% # this contains the links to the movie page (same node from 'name')
  html_attr('href') %>% # Extracts the 'a tag' and gets us what is equal to the href.We need to complete the link.
  paste0('https://www.imdb.com', .) %>% # makes it so it is pasted in as the second argument
  substr(1, nchar(.)-15) %>% paste0("fullcredits/") #insert insert 15 letters from the end of the link 
# Note
# movie_links = movie_links %>% substr(1, nchar(movie_links)-15) %>% # I had to manipulate the link to get to a page that is static.
#   paste0("fullcredits/") <- this and the line above did not belong here. it locked in the first 50 cast for the rest of the movies
 # The first paste is sufficient, if the link doesnt take you to a dynamic page, in this case it did, and I had to find a static page with the cast members. 
 # The links are to those pages 
  

# Now we have to find a way to go into the link and srcape the actors
# Rather than use a for loop, we will us the 's' and 'l' apply functions

# We create a function that will scrape the movie links and grab the prinary actors.

get_cast = function(movie_link){ # what our input is called, not to be confused with 'movie_links'
  # movie_link ="https://www.imdb.com/title/tt2488496/fullcredits/" # TEST LINK
  movie_page = read_html(movie_link) # read in the movie link
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>% 
    html_text() %>% # grabs the name 
    paste(collapse = ",") %>%  # Collapses each name to one string, rather than having them have their own.
  return(movie_cast)
}


cast = sapply(movie_links , 
              FUN = get_cast, #Function that the vectors will pass through 
              USE.NAMES = FALSE) # Default is set to True, which create a column of what went into the function in this case a column of the movie links.
# sapply() : given the fist variable (movie_links vector), it will run its
#            contents through the specified fuction(FUN = get_cast)


movies50v2 = data.frame(rank,name,year,rating,metascore,votes, synopsis, gross, cast, stringsAsFactors = F) 

#####Scrape through multiple pages####

# How to go through multiple pages.
# step 1: How is the page changing as we go change pages
# page 1:https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc&ref_=adv_prv 
# We can manipulate this url to follow the same pattern as the others
# Add start=1& to the url in its respective spot
# page 1:https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc&start=1&ref_=adv_prv 
# page 2:https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc&start=51&ref_=adv_nxt
# page 2:https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc&start=101&ref_=adv_nxt

# Make a for loop that will go through each of the pages we want to scrape
# And then we do everything we did before
# Bring the get_cast() before the 






get_cast = function(movie_link){ # what our input is called, not to be confused with 'movie_links'
  # movie_link ="https://www.imdb.com/title/tt2488496/fullcredits/" # TEST LINK
  movie_page = read_html(movie_link) # read in the movie link
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>% 
    html_text() %>% # grabs the name 
    paste(collapse = ",") %>%  # Collapses each name to one string, rather than having them have their own.
    return(movie_cast)
}




movies200 = data.frame()

for (page_result in seq(from=1, to = 151, by = 50)) { #create a sequence that will increase by 50 (number of movies on page)
  # We will us the altered first link
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=action&sort=boxoffice_gross_us,desc&start=",
                page_result,
                "&ref_=adv_nxt")
  page = read_html(link)
  movie_links = page %>% html_nodes(".lister-item-header a") %>% 
    html_attr('href') %>% paste0('https://www.imdb.com', .) %>% substr(1, nchar(.)-15) %>% paste0("fullcredits/") # the periods are a way to call of the data before the pipe
  
  rank = page %>% html_nodes(".text-primary") %>% html_text() %>% as.numeric()
  name = page %>% html_nodes(".lister-item-header a") %>% html_text() 
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  # year = year %>% substr(start = 2, stop = 5) %>% as.numeric()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  metascore = page %>% html_nodes(".metascore") %>% html_text()
  metascore = metascore %>% substr(start = 0, stop = 3) %>% as.numeric()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text() %>% as.numeric()
  gross = page %>% html_nodes(".ghost~ .text-muted+ span") %>%html_text()
  # gross = substr(gross, start = 1, stop = nchar(gross) - 1) %>% as.numeric()
  votes = page %>% html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% html_text()
  votes = votes %>% decomma()
  synopsis= page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text() 
  cast = sapply(movie_links, 
                FUN = get_cast, 
                USE.NAMES = FALSE)
  
  print(link)

  movies200 = rbind(movies200, data.frame(rank,name,year,rating,metascore,votes,gross, synopsis, cast, stringsAsFactors = F))
  # As it loops, the new page will be apended to the movies data frame
  print(paste("Page:",page_result)) # this is a check to see if the code is running.
}

# write.csv(movies200, file = "movies200.csv")

