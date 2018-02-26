

library(tidyverse)
library(igraph)

in_out <- read_csv('H:/ukioanalyticaltables2013.csv')



long <- in_out %>% 
    slice(6) %>% 
    gather(key,value) 


clean.names <- long$value %>% 
    str_to_lower(.) %>% 
    str_replace_all('[^A-z]',' ') %>% 
    str_replace_all('\\b[aeio]n\\b',' ') %>% 
    str_replace_all('\\band\\b',' ') %>% 
    str_replace_all('npish$',' ') %>% 
    str_replace_all('\\bof\\b',' ') %>% 
    str_replace_all(' +',' ') %>% 
    str_replace_all(' +','.') 


names(in_out) <- clean.names

names(in_out)[2] <- 'Source'

in_out <- in_out[-c(1:6),-1] 


in_out$Source <- in_out$Source %>% 
    str_to_lower(.) %>% 
    str_replace_all('[^A-z]',' ') %>% 
    str_replace_all('\\b[aeio]n\\b',' ') %>% 
    str_replace_all('\\band\\b',' ') %>% 
    str_replace_all('npish$',' ') %>% 
    str_replace_all('\\bof\\b',' ') %>% 
    str_replace_all(' +',' ') %>% 
    str_replace_all(' +','.') 







column.not.row <- which(names(in_out) %in% in_out$Product == F) # variable index of columns that are NOT rows
which(in_out$Product %in% names(in_out) == F) # variable index of columns that are NOT rows



alpha <- in_out %>% 
    select(-column.not.row[-1]) 


alpha_edges <- alpha %>% 
    gather(Target,value,-Source) %>% 
    mutate(value = as.numeric(value))




map(alpha_edges,function(x){
    sum(is.na(x))
})




# Create Igraph -----------------------------------------------------------


# Imports

ag_imports <- alpha_edges %>% 
    group_by(Source) %>% 
    mutate(percent = round(value/sum(value,na.rm = T)*100,1)) %>% 
    filter_at(vars(1,2),any_vars(str_detect(.,'agriculture'))) %>% 
    filter(value != 0) %>% 
    print(n = Inf)



ag_imports_g <- graph_from_data_frame(ag_imports)

ag_import_attr <- data_frame(Source = V(ag_imports_g)$name)

## A node attributes

ag_import_attr <- left_join(ag_import_attr,ag_imports)

V(ag_imports_g)$Percent.imported.from.agriculture <- ag_import_attr %>% pull(percent)
V(ag_imports_g)$Value.imported.from.agriculture <- ag_import_attr %>% pull(value)

### node name formats
ag_import_attr$Source.regex <- ag_import_attr$Source %>% 
    str_replace_all('\\.',' ') %>% 
    str_replace_all('\\bservices\\b',' ') %>% 
    str_replace_all('\\ba\\b',' ') %>% 
    str_replace_all('\\bor\\b',' ') %>% 
    str_replace_all('\\bfor\\b',' ') %>% 
    str_replace_all('transport$',' ') %>% 
    str_replace_all('\\bproducts\\b',' ') %>% 
    str_replace_all(' +',' ') %>% 
    str_replace_all('\\btransport transport\\b','transport') %>% 
    str_replace_all('\\bn e c\\b','') %>% 
    str_replace_all('\\bother\\b','') %>% 
    str_replace_all('\\bbasic\\b','') %>% 
    str_replace_all('\\b([a-z]+)\\s+\\1\\b','') %>% 
    str_trim(.) %>% 
    str_to_title(.) %>% 
    str_replace_all('  +','') %>% 
    str_replace_all('\\bCleaning Polishing Preparations Perfumes Toilet Preparations\\b','') 



ag_import_attr[[5]][12] <- 'Paper'


V(ag_imports_g)$name <- ag_import_attr[[5]]



to_source_target(ag_imports_g,save_as_csv = T)
to_source_target_labels(ag_imports_g,save_as_csv = T)





# Exports


ag_exports <- alpha_edges %>% 
    group_by(Target) %>% 
    mutate(percent = round(value/sum(value,na.rm = T)*100,1)) %>% 
    filter_at(vars(1,2),any_vars(str_detect(.,'agriculture'))) %>% 
    filter(value != 0) %>% 
    print(n = Inf)

ag_exports_g <- graph_from_data_frame(ag_exports)


ag_exports_attr <- data_frame(Target = V(ag_exports_g)$name)

## A node attributes
ag_exports_attr <- left_join(ag_exports_attr,ag_exports)

ag_exports_attr$Target.regex <-  ag_exports_attr$Target %>% 
    str_replace_all('\\.',' ') %>% 
    str_replace_all('\\bservices\\b',' ') %>% 
    str_replace_all('\\ba\\b',' ') %>% 
    str_replace_all('\\bor\\b',' ') %>% 
    str_replace_all('\\bfor\\b',' ') %>% 
    str_replace_all('transport$',' ') %>% 
    # str_replace_all('\\b\\w+\\b(?=\\Wproducts\\b)',' ') %>% 
    str_replace_all('\\bproducts\\b',' ') %>%
    str_replace_all(' +',' ') %>% 
    str_replace_all('\\btransport transport\\b','transport') %>% 
    str_replace_all('\\bn e c\\b','') %>% 
    str_replace_all('\\bother\\b','') %>% 
    str_replace_all('\\bbasic\\b','') %>% 
    str_replace_all('\\b([a-z]+)\\s+\\1\\b','') %>% 
    str_trim(.) %>% 
    str_to_title(.) %>% 
    str_replace_all('  +','')%>% 
    str_replace_all('\\ Tv Programme Production Sound Recording Music Publishing Programming Broadcasting\\b','')%>% 
    str_replace_all('\\bCleaning Polishing Preparations Perfumes Toilet Preparations\\b','') %>%  
    str_replace_all('\\bSupportBusiness\\b','Support Business') 



V(ag_exports_g)$Percent.exported.to.agriculture <- ag_exports_attr%>% pull(percent)
V(ag_exports_g)$Value.exported.to.agriculture <- ag_exports_attr %>% pull(value)
V(ag_exports_g)$name. <- ag_exports_attr %>% pull(Target.regex)


to_source_target(ag_exports_g,save_as_csv = T)
to_source_target_labels(ag_exports_g,save_as_csv = T)


# ag_vars <- alpha_edges_non_0 %>% 
#     filter_at(vars(1:2),any_vars(str_detect(.,'agriculture'))) 
# 
# 
# 
# ag_graph <- graph_from_data_frame(ag_vars,directed = T)
# 
# 
# E(ag_graph)$value <- ag_vars$value
# 
# 
# 
# write_csv(to_source_target(ag_graph),'input_output_edges_2013.csv')
# write_csv(to_source_target_labels(ag_graph),'input_output_nodes_2013.csv')
# 
# 
# 
# to_source_target
# 
# 
# 
# 
to_source_target <- function(i.graph,save_as_csv = F){
    
    a <- sapply(
        dplyr::as_data_frame(
            igraph::as_edgelist(i.graph)),
        match,
        table=unique(
            unlist(
                dplyr::as_data_frame(
                    igraph::as_edgelist(i.graph)))))
    
    b <- dplyr::as_data_frame(a) %>% rename(Source = V1,
                                            Target = V2)
    atts <- dplyr::bind_rows(igraph::get.edge.attribute(i.graph))
    f <- dplyr::bind_cols(b,atts)
    if ( save_as_csv == T){
        pa <- paste0(getwd(),'/',deparse(substitute(i.graph)),'.csv')
        readr::write_csv(f,path = pa)
        print(message(paste('++++ file saved to',pa,'++++')))
    }
    f
}





















