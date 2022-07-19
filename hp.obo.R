# install.packages("stringr")
# install.packages("tidyverse")
# install.packages("data.tree")

library(data.tree)
library(stringr)

#line = "is_a"
# grepl("^is_a", line)
# if (grepl("^is_a", line)){
#   print("Okay")
# }

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
    ##stop()
    temp <-str_replace(line, "is_a: HP:", "")
    ## modify temp to strip all but numerics
    temp <- gsub("\\D", "", temp)
    current_hpo_node$is_a <- c(current_hpo_node$is_a,
                               temp)
     
  }
  # if all are false we return to the top to stop break the while loop if conditions are met
  ## if all are true we return to top and see if condition is met to continue through loop again
}
#closes the connection to 
close(con)

  
fib <- function(n){
  if (n==1){
    return(0)
  }
  if (n==2){
    return(1)
  }
  return(fib(n-1)+fib(n-2))
}

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

unique_hpo <- unique(phen_to_gene$HPO.id)[1:length(unique(phen_to_gene$HPO.id))]

## pulls HPO.id based off the first one in the table and gives all gene symbols unique to that Id
phen_to_gene[which(phen_to_gene$HPO.id == unique(phen_to_gene$HPO.id)[1]),]$entrez.gene.symbol

for (i in 1:length(unique(phen_to_gene$HPO.id))){
  current_hpo_id <- unique(phen_to_gene$HPO.id)[i]
  current_hpo_id <- str_replace(current_hpo_id, "HP:", "")
  # which <- grep(current_hpo_id, phen_to_gene$HPO.id)
  which <- which(phen_to_gene$HPO.id == current_hpo_id)
  temp_genes <- phen_to_gene[which,]$entrez.gene.symbol
  current_hpo_id_w_ancestors <- get_ancestors(current_hpo_id, hpo_bin_search_tree)
  for(j in 1:length(current_hpo_id_w_ancestors)){
    hpo_bin_search_tree <- add_genes_to_node(current_hpo_id_w_ancestors[j], hpo_bin_search_tree, temp_genes)
  }
  
}



# function for adding genes to node
add_genes_to_node <- function(id, tree, gene_ids){
  if(all(is.na(tree$left))){
    return(tree)
  }
  if(all(is.na(tree$right))){
    return(tree)
  }
  if(id < tree$id){
    tree$left <- add_genes_to_node(id, tree$left, gene_ids)
    return(tree)
  }
  if(id > tree$id){
    tree$right <- add_genes_to_node(id, tree$right, gene_ids)
    return(tree)
  }
  if(id == tree$id){
    tree$genes_assorted <- unique(c(tree$genes_assorted), gene_ids)
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
