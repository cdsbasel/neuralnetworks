require(tidyverse)
fl = readRDS('~/Dropbox (2.0)/Work/Projects/Memory/-- AgingLexicon/2 Clean Data/Tablet_Fluency.RDS')
# cats = read_delim('~/Dropbox (2.0)/Work/Projects/Memory/-- AgingLexicon/0 Material/TroyerCategories_german.txt',col_names = F,delim=',[:blank:]')
# cats$X2 = str_to_lower(str_remove_all(cats$X2 , '[:blank:]'))
# cats = cats %>% na.omit()

fl = fl %>% filter(!is.na(correct), !str_detect(correct, ' '), correct != 'NA') 
fluency = split(fl$correct, fl$subject)
words = fl %>% pull(correct)

# w_sel = unique(words)[(unique(words) %in% cats[[2]])]
# fluency = lapply(fluency, function(x) unique(x[x %in% w_sel]))

write_lines(unique(unlist(fluency)), 'Cognitive/1_Data/anis.txt')

vecs = read_lines('Cognitive/1_Data/animal_vectors.txt')
vecs = str_split(vecs, ' ')
words = str_to_lower(sapply(vecs, function(x) x[1]))
vecs = do.call(rbind, lapply(vecs, function(x) x[-1]))
rownames(vecs) = str_to_lower(words)
colnames(vecs) = paste0('node_',1:300)
mode(vecs) = 'numeric'

fluency = lapply(fluency, function(x) x[x %in% rownames(vecs)])

cosine = function(vecs) vecs %*% t(vecs) / (sqrt(rowSums(vecs ** 2)) %*% t(sqrt(rowSums(vecs ** 2))))

cos_vec = cosine(vecs)
cos_vec[cos_vec == 1] = 0
norm = function(x) {x = x + abs(min(x)) ; x / max(x)}
cos_vec = norm(cos_vec)



plot_cosine_mds = function(cos, items){
    ani_cat = unique(cats$X1)
    col = viridis::viridis(length(ani_cat))
    names(col) = ani_cat
    cols = col[cats$X1]
    names(cols) = cats$X2
    norm = function(x) {x = x + abs(min(x)) ; x / max(x)}
    mds <<- cmdscale(1-norm(cos))
    par(mar=c(0,0,0,0))
    plot.new();plot.window(range(mds[,1]),range(mds[,2]))
    sel = items %in% names(cols)
    text(mds[!sel,1],mds[!sel,2],labels = items[!sel], 
         col = 'grey75', cex=.4, font=1)
    text(mds[sel,1],mds[sel,2],labels = items[sel], 
         col = cols[items[sel]], cex=.5, font=2)
    #mtext(expression(italic(z)^2),side=3,cex=1.2)
  }

add_path = function(x){
  pos = get("mds", pos = '.GlobalEnv')[x,]
  cols = colorRampPalette(c('black','steelblue'))(length(x))
  points(pos, pch = 16, cex=.5, col = cols)
  for(i in 1:(length(x)-1)){
    lines(c(pos[i,1],pos[i+1,1]),c(pos[i,2],pos[i+1,2]), col=cols[i], lwd=.5) 
    }
  }


plot_cosine_mds(cos_vec, rownames(cos_vec))
add_path(fluency[[1]][1:10])








