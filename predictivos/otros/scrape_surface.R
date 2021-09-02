library(RSelenium)

eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))
driver <- rsDriver(browser = "chrome", port = 4444L, chromever = "92.0.4515.107", extraCapabilities = eCaps)
# driver <- rsDriver(browser = "firefox", port = 4444L)
remDr <- driver[["client"]] 

manual_surface <- propiedades_test %>% 
  filter(is.na(surface_total)) %>%
  select(id, description, title) %>% 
  nest(ids=id) %>% 
  mutate(
    n_prop=map_dbl(ids, nrow),
    is_lui = str_detect(description, 'XINTEL\\(LUI-'),
    xintel = str_extract(description, '\\d+\\)$') %>% 
      str_remove('\\)') %>%
      as.numeric(),
    m2 = NA
  ) %>% 
  arrange(desc(n_prop), description) %>% 
  filter(n_prop>25)

for(nr in 1:nrow(manual_surface)){
  
  cat('Starting: ', nr, ' xintel \n')
  
  if(!is.na(manual_surface$m2[nr])){next}
  
  xintel <- manual_surface %>% slice(nr) %>% pull(xintel)
  
  .url <- sprintf('https://www.luisio.com.ar/departamento-en-venta-en-recoleta-ficha-lui%s', xintel)
  
  remDr$navigate(.url)
  Sys.sleep(5)
  m2 <- remDr$findElement(using = 'xpath', '//*[@id="ficha"]/div[7]/div[4]/ul/li[1]/span')$getElementAttribute("outerHTML") %>%
    unlist() %>%
    str_extract('(?<="stofic\">).*(?=</span>)') %>%
    str_remove('m²') %>% 
    as.numeric()
  
  manual_surface$m2[nr] <- m2
  
}
beepr::beep()
driver$server$stop()
