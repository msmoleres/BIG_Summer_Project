#install.packages("stringr")
# install.packages("tidyverse")
library(ggnewscale)
library(enrichplot)
library(ggplot2)
library(ggExtra)
library(oddsratio)
library(stringr)


library(BiocManager)
library(clusterProfiler)
library(org.Hs.eg.db)


#list for storing the id,name, and is_a
current_hpo_node <- list(id = character(), name = character(), is_a = character())
hpo_list <- list()
#connects file in read only mode
con <- file("hp.obo.txt", "r")
while ( TRUE ) {
  # read line of document one line at a time
  line <- readLines(con, n = 1)
  #if the length of the line is 0 then add current_hpo_node to the hpo_list and break while loop
  ## if false then we continue through the code to next if statement
  if ( length(line) == 0 ) {
    hpo_list[[length(hpo_list)+1]] <- current_hpo_node
    break
  }
  #find line with ^id: HP:
  if (grepl("^id: HP:",line)){
    
    # Test if current_hpo_node is empty, if current node is not empty, (mid of list) we store the current node into the
    # # list the reintialize the current node. If it is empty it signals we are at the beginning of the list
    if (length(current_hpo_node$id) != 0){
      hpo_list[[length(hpo_list)+1]] <- current_hpo_node
      current_hpo_node <- list(id = character(), name = character(), is_a = character())
    }
    
    #extract only numbers from line using strngr and store in current_hpo_node$id
    current_hpo_node$id <- str_replace(line, "id: HP:", "")
    
  }
  #find ^name, store in current_hpo_node$name if true, if false continue
  if (grepl("^name",line)){
    current_hpo_node$name <- str_replace(line, "name:", "")
    
  }
  #find i^s_a, concatenate current_hpo_node$is_a if true by adding the string that matches condition to
  ## current_hpo_node$is_a; allows us to add multiple is_a 
  if (grepl("^is_a",line)){
    # stop()
    temp <-str_replace(line, "is_a: HP:", "")
    ## modify temp to strip all but numerics
    # temp <- gsub("\\D", "", temp)
    temp <- str_extract(temp,"[0-9]{7}")
    current_hpo_node$is_a <- c(current_hpo_node$is_a,
                               temp)
     
  }
  # if all are false we return to the top to stop break the while loop if conditions are met
  ## if all are true we return to top and see if condition is met to continue through loop again
}
#closes the connection to 
close(con)

  
#Binary Tree
node_list = hpo_list 

# creating a function using arguments tree and node to add a node to the binary tree
add_node_to_tree <- function(tree, node){
  ### test if node$id < tree$id is true
  if (node$id < tree$id){
    ## if true, test if tree$left is na, if true, add node as left child
    if (all(is.na(tree$left))){
      tree$left <- node
      
    }else {
      # if either condition is false thse function is reinitialized with tree = tree$left
      tree$left <- add_node_to_tree(tree$left, node)
      # returns a tree that is the same as tree$left
      return(tree)
    }
  }
  # test is node$id is greater than the tree$id, if true then test if the right node is NA
  if (node$id > tree$id){
    if (all(is.na(tree$right))){
      #if the tree$right is na then the node is added to the tree$right
      tree$right <- node
      # if not then the tree$right is set to this function but with tree$right as the new argument
    }else {
      tree$right <- add_node_to_tree(tree$right, node)
      #we return tree with a right node
      return(tree)
    }
  
  }
  ### if node$id == tree$id ## throw error or warning
  if (node$id == tree$id){
    print('Warning: Current Node is equal to Tree Node')
  }
  # once all above arguments have been run through return(tree)
  return(tree)
}

### creating a function that buidles a tree

build_tree <- function(node_list){
  # setting which_node to take any random number  from the length of node_list, (node_list == hpo_list) 
  which_node <- sample(1:length(node_list),1)
  # save the node that the number correlates to, to "node" 
  node <- node_list[[which_node]]
  # go back to the list and set which_node to null so there are no repeats
  node_list[[which_node]] <- NULL
  #set node left and right to NA so that items may be added to them
  node$left <- NA
  node$right <- NA
  #set tree equal to the node that was defined above
  tree <- node
  
  #while the length of node_list is greater than zero continue through while loop
  while(length(node_list)>0){
    # sets which_node to a random node from the node_list
    which_node <- sample(1:length(node_list),1)
    # sets node to the node from the node_list based on the random sample
    node <- node_list[[which_node]]
    # sets the nodes left adn right to NA
    node$left <- NA
    node$right <- NA
    #makes the random sample node we pulled set to node so code does not run into itself or duplicate
    node_list[[which_node]] <- NULL
    # sets tree to the function defined above using the tree and node as arguments
    tree <- add_node_to_tree(tree, node)
  }
  # gives use the tree as build_tree
  return(tree)
} 

hpo_bin_search_tree <- build_tree(hpo_list)

## get_depth_of_node_in_tree
get_depth_of_node_in_tree = function(id, tree, depth=0){
  if (id < tree$id){
    if (all(is.na(tree$left))){
      return(-1)
    }
    return(get_depth_of_node_in_tree(id, tree$right, depth = depth+1))
  }
  if (id > tree$id){
    if (all(is.na(tree$right))){
      return(-1)
    }
    return(get_depth_of_node_in_tree(id, tree$right, depth = depth+1))
  }
  if (id == tree$id){
    return(depth)
  }
  else 
    return(get_depth_of_node_in_tree(id, tree$right, depth = depth+1))
}
  

## get_node

get_node_from_tree <- function(id, tree){
  if (id < tree$id){
    if (all(is.na(tree$left))){
      return(-1)
    }
    return(get_node_from_tree(id, tree$left))
  }
  if (id > tree$id){
      if (all(is.na(tree$right))){
        return(-1)
      }
    return(get_node_from_tree(id, tree$right))
  }
  if (id == tree$id){
    tree$left = NULL
    tree$right = NULL
    node <- tree
    return(node)
  }
}


phen_to_gene <- read.csv("phenotype_to_genes.txt", sep = "\t")
phen_to_gene$HPO.id <- str_replace(phen_to_gene$HPO.id, "HP:", "")
unique_hpo <- unique(phen_to_gene$HPO.id)[1:length(unique(phen_to_gene$HPO.id))]


# function for adding genes to node
add_genes_to_node <- function(id, tree, gene_ids){
  if(id < tree$id){
    if(all(is.na(tree$left))){
      return(tree)
    }
    tree$left <- add_genes_to_node(id, tree$left, gene_ids)
    return(tree)
  }
  if(id > tree$id){
    if(all(is.na(tree$right))){
      return(tree)
    }
    tree$right <- add_genes_to_node(id, tree$right, gene_ids)
    return(tree)
  }
  if(id == tree$id){
    tree$genes_assorted <- unique(c(tree$genes_assorted, gene_ids))
    return(tree)
  }
}



get_ancestors <- function(id,tree,ancestors = character(0)){
  node <- get_node_from_tree(id,tree)
  ancestors <- c(ancestors, node$id)
  if (length(node$is_a)== 0){
    return(ancestors)
  }
  for (is_a in node$is_a){
      ancestors <- unique(c(ancestors,get_ancestors(is_a, tree, ancestors)))
  }
  return(ancestors)
}

for (i in 1:length(unique(phen_to_gene$HPO.id))){
  current_hpo_id <- unique(phen_to_gene$HPO.id)[i]
  current_hpo_id <- str_replace(current_hpo_id, "HP:", "")
  which <- which(phen_to_gene$HPO.id == current_hpo_id)
  temp_genes <- phen_to_gene[which,]$entrez.gene.symbol
  current_hpo_id_w_ancestors <- get_ancestors(current_hpo_id, hpo_bin_search_tree)
  for(j in 1:length(current_hpo_id_w_ancestors)){
    hpo_bin_search_tree <- add_genes_to_node(current_hpo_id_w_ancestors[j], hpo_bin_search_tree, temp_genes)
  }
  
}
#####################################################################################################



## inserting gene ensemble

ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", GRCh=37)
gene_ensemble <- getBM(attributes=c('chromosome_name','hgnc_symbol', 'ensembl_gene_id','gene_biotype'),
               filters = list('biotype'='protein_coding'),
               mart = ensembl, useCache = F)

## getting gene ensemble ID based on gene name

gene_ensemble$ensembl_gene_id[which(gene_ensemble$hgnc_symbol == gene_name)]

## GO enrichment

node <- get_node_from_tree("0005948", hpo_bin_search_tree)

  temp_genes <- node$genes_assorted
  gene_subset <- gene_ensemble$ensembl_gene_id[which(is.element(gene_ensemble$hgnc_symbol, temp_genes))]
  ego <- enrichGO(gene = gene_subset,
                  universe = gene_ensemble$ensembl_gene_id,
                  keyType = "ENSEMBL",
                  OrgDb = org.Hs.eg.db,
                  ont = "BP",
                  pAdjustMethod = "BH",
                  qvalueCutoff = 0.05,
                  readable = TRUE,
                  pool=TRUE)


head(ego)

##Odds Ratio

test <- fisher.test(temp)

#Chromatin Modifiers

chrom <- read.csv("chromatin_modifiers.csv")
chromatin_modifiers <- character()
chromatin_modifiers <- unique(chrom$hgnc_symbol) 

## Adding Chromatin Modifier to node function

add_chrom_modifier_to_node <- function(tree, chrom_m){
    if(!all(is.na(tree$left))){tree$left <- add_chrom_modifier_to_node(tree$left, chrom_m)
  }
    if(!all(is.na(tree$right))){tree$right <- add_chrom_modifier_to_node(tree$right, chrom_m)
  }
  tree$chrom_assorted <- tree$genes_assorted[which(is.element(tree$genes_assorted, chromatin_modifiers))]
 return(tree)
}
#replace temp w/ hpo_binary_search_tree(when done)#
hpo_bin_search_tree <- add_chrom_modifier_to_node(hpo_bin_search_tree, chromatin_modifiers)

##Add Fisher Test to tree function
add_fisher_to_node <- function(tree){
  if(!all(is.na(tree$left))){tree$left <- add_fisher_to_node(tree$left)
  }
  if(!all(is.na(tree$right))){tree$right <- add_fisher_to_node(tree$right)
  }
  if (length(tree$chrom_assorted) != 0){
   # print(tree$id)
    tree$fisher_test <- fisher.test(t(matrix(c(length(tree$chrom_assorted)
                                             ,length(chromatin_modifiers) - length(tree$chrom_assorted) 
                                             ,length(tree$genes_assorted) - length(tree$chrom_assorted)
                                             ,length(unique(gene_ensemble$ensembl_gene_id)) - length(tree$genes_assorted)),
                                           nrow=2)))
  }
  return(tree)
}

# replace temp w/ hpo_binary_search_tree, replace temp2 w/ hpo_binary_search_tree
hpo_bin_search_tree <- add_fisher_to_node(hpo_bin_search_tree)

# node$chrom_assorted <- node$genes_assorted[which(is.element(node$genes_assorted, chromatin_modifiers))]

#tree to dataframe function,

tree_to_dataframe <- function(tree, current_dataframe=NULL){

  dataframe <- current_dataframe
  current_dataframe <- data.frame(
    HPO_ID = tree$id,
    phenotype = tree$name,
    ancestors = paste(tree$is_a, collapse = ","),
    genes_associated = paste(c(tree$genes_assorted), collapse = ","),
    chrom_modifiers_associated = paste(c(tree$chrom_assorted), collapse = ","),
    p_value = ifelse(is.null(tree$fisher_test$p.value), NA, tree$fisher_test$p.value), 
    odds_ratio = ifelse(is.null(tree$fisher_test$estimate), NA, tree$fisher_test$estimate[[1]]), 
    confidence_interval = ifelse(is.null(tree$fisher_test$conf.int),NA,paste(c(tree$fisher_test$conf.int), collapse = "--"))
  )
  current_dataframe <- rbind(dataframe, current_dataframe)
  
  if(!all(is.na(tree$left))){current_dataframe <- tree_to_dataframe(tree$left, current_dataframe)
  }
  if(!all(is.na(tree$right))){current_dataframe <- tree_to_dataframe(tree$right, current_dataframe)
  }
  return(current_dataframe)
}

search_tree_dataframe <- tree_to_dataframe(hpo_bin_search_tree)


# test <- data.frame(first= "a", second = "b", third = "c")
# test2 <- data.frame(second = "b",first= "a",  third = "c")
# test <- rbind(test, test2)
# rownames(test) <- c("x","y","z")

#Go Enrightment on TOP 5
plot(-log10(search_tree_dataframe$p_value), log2(search_tree_dataframe$odds_ratio) );abline(v=-log10(0.05/nrow(search_tree_dataframe)));abline(h=2)
which(log2(search_tree_dataframe$odds_ratio) > 4 & -log10(search_tree_dataframe$p_value) > 10)
# Highest oddsratio and really reject the null (super small p.value)
## For BP ontology
top_ids <- list("0001212","0008398", "0009928", "0011231", "0011298", "0011937", "0012523", "0012808", "0012810" )
ego_list <- list()
for (id in 1:length(top_ids)){
  node <- get_node_from_tree(top_ids[id], hpo_bin_search_tree)
  temp_genes <- setdiff(node$genes_assorted, node$chrom_assorted)
  gene_subset <- gene_ensemble$ensembl_gene_id[which(is.element(gene_ensemble$hgnc_symbol, temp_genes))]
  ego_list[[node$id]] <- enrichGO(gene = gene_subset,
                  universe = gene_ensemble$ensembl_gene_id,
                  keyType = "ENSEMBL",
                  OrgDb = org.Hs.eg.db,
                  ont = "BP", ## CC , BP, MF
                  pAdjustMethod = "BH",
                  qvalueCutoff = 0.05,
                  readable = TRUE,
                  pool=TRUE)
  
  png(filename = paste("temp_image_p_",
                       signif(node$fisher_test$p.value,2),
                       "_or_",
                       signif(node$fisher_test$estimate[[1]]),
                       "_term_", node$name,
                       "_id_", node$id,
                       ".png"
  ))
  print(dotplot(ego_list[[node$id]], showCategory = 10))
  dev.off()
}

head(ego)

temp_ego <- pairwise_termsim(ego_list$`0011298`)
emapplot(temp_ego, showCategory = 10 )


emapplot(temp_ego,showCategory = 15, cex_category=1,layout="kk") 
cowplot::plot_grid(p1, p2, p3, p4, ncol=2, labels=LETTERS[1:4])

#Plots
head(search_tree_dataframe)
which <- which(log2(search_tree_dataframe$odds_ratio) > 4 & -log10(search_tree_dataframe$p_value) > 10)
sig <- which(-log10(search_tree_dataframe$p_value) > 35)

#GGplot for dataframe
ggplot(data = search_tree_dataframe, aes(x= -log10(p_value), y= log2(odds_ratio))) +
  #geom_bin_2d(bins=10)+
  geom_point(size=1, color="black", aes(alpha= -log10(p_value) )) +
  # geom_point(data = search_tree_dataframe[which,], aes(x=-log10(p_value)+0.5),color="blue", alpha=0.3, size=6)+
  geom_point(data = search_tree_dataframe[which,], color="red", alpha=1, size=3)+
  geom_point(data = search_tree_dataframe[sig,], color="green", alpha=1, size=3)+
  theme(legend.position="none")+
  geom_vline(xintercept = 10) +
  geom_hline(yintercept = 4)
  

#Bar Plot for Most significant 
which(-log10(search_tree_dataframe$p_value) > 35)
most_significant <- list("0000159", "0000177","0000233","0000315","0000436","0000492","0030669","0032039")

most_dataframe <- data.frame()
for (x in 1:length(most_significant)){
  node <- get_node_from_tree(most_significant[[x]], hpo_bin_search_tree)
  most_data <- data.frame(HPO_id = node$id ,  
    odds_ratio = node$fisher_test$estimate
  )
  most_dataframe <- rbind(most_dataframe, most_data)
}

ggplot(most_dataframe, aes(x= HPO_id, y=odds_ratio), fill = x) +
  aes(reorder(HPO_id, -odds_ratio)) +
  geom_bar(stat = "identity")+
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

#Bar plot for highly enriched significant nodes
top_dataframe <- data.frame()
for (x in 1:length(top_ids)){
  node <- get_node_from_tree(top_ids[[x]], hpo_bin_search_tree)
  top_data <- data.frame(HPO_id = node$id ,  
                          odds_ratio = node$fisher_test$estimate
  )
  top_dataframe <- rbind(top_dataframe, top_data)
}

ggplot(top_dataframe, aes(x= HPO_id, y=odds_ratio),  fill = x) +
  aes(reorder(HPO_id, -odds_ratio)) +
  geom_bar(stat = "identity")+
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

#GO Analysis for most significant
most_significant <- list("0000159", "0000177","0000233","0000315","0000436","0000492","0030669","0032039")
most_list <- list()
for (id in 1:length(most_significant)){
  node <- get_node_from_tree(most_significant[[id]], hpo_bin_search_tree)
  temp_genes <- setdiff(node$genes_assorted, node$chrom_assorted)
  gene_subset <- gene_ensemble$ensembl_gene_id[which(is.element(gene_ensemble$hgnc_symbol, temp_genes))]
  most_list[[node$id]] <- enrichGO(gene = gene_subset,
                                  universe = gene_ensemble$ensembl_gene_id,
                                  keyType = "ENSEMBL",
                                  OrgDb = org.Hs.eg.db,
                                  ont = "BP", ## CC , BP, MF
                                  pAdjustMethod = "BH",
                                  qvalueCutoff = 0.05,
                                  readable = TRUE,
                                  pool=TRUE)
  
  png(filename = paste("temp_image_p_",
                       signif(node$fisher_test$p.value,2),
                       "_or_",
                       signif(node$fisher_test$estimate[[1]]),
                       "_term_", node$name,
                       "_id_", node$id,
                       ".png"
  ))
  print(dotplot(most_list[[node$id]], showCategory = 10))
  dev.off()
}

temp_most <- pairwise_termsim(most_list$`0000315`)
emapplot(temp_most, showCategory = 10 )
