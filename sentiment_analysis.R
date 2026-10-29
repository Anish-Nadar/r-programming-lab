library(igraph)

edges <- data.frame(
  from = c("Alice", "Alice", "Bob",     "Bob",   "Charlie", "David", "Eve"),
  to   = c("Bob",   "Charlie", "Charlie", "David", "David",   "Eve",   "Alice")
)

net <- graph_from_data_frame(d = edges, directed = FALSE)

get_mutual_friends <- function(graph, user1, user2) {
  n1 <- names(neighbors(graph, user1))
  n2 <- names(neighbors(graph, user2))
  intersect(n1, n2)
}

people_you_may_know <- function(graph, user) {
  all_users <- V(graph)$name
  friends <- names(neighbors(graph, user))
  non_friends <- setdiff(all_users, c(user, friends))
  
  if (length(non_friends) == 0) {
    return(data.frame(Person = character(0), Mutual_Count = integer(0)))
  }
  
  counts <- sapply(non_friends, function(person) {
    length(get_mutual_friends(graph, user, person))
  })
  
  res <- data.frame(Person = non_friends, Mutual_Count = counts)
  res <- res[res$Mutual_Count > 0, ]
  res <- res[order(-res$Mutual_Count), ]
  rownames(res) <- NULL
  return(res)
}

cat("Mutual friends between Alice and David:\n")
print(get_mutual_friends(net, "Alice", "David"))

cat("\nPeople Alice may know:\n")
print(people_you_may_know(net, "Alice"))

cat("\nPeople Eve may know:\n")
print(people_you_may_know(net, "Eve"))

plot(net, vertex.color = "skyblue", vertex.size = 30, vertex.label.color = "black")
