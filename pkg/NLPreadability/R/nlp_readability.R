
#'
#' ## Traditional Features
#'
#' `avg_sl`           average sentence length
#' `avg_wl`           average word length
#' `r_long_words`     ratio of long words
#' `r_polysy`         ratio of polysyllable
#' `r_unique_lemmas`  ratio of unique lemmas (type token ratio)
#' r_unique_words     is the same type token ratio
create_traditional_features <- function(dwords, dsents, dsyll_count, dlemmas) {
  list(avg_sl = length(dwords) / length(dsents),
       avg_char_sl = mean(sapply(dsents, function(s) sum(nchar(s)))),
       avg_wl = mean(nchar(dwords)),
       avg_syll = mean(dsyll_count),
       r_long_words = sum(dsyll_count > 3) / length(dsyll_count),
       r_polysy = sum(dsyll_count > 2) / length(dsyll_count),
       r_unique_words = length(unique(dwords)) / length(dwords),
       r_unique_lemmas = length(unique(dlemmas)) / length(dlemmas))
}


#'
#' ## POS based Features
#'
count_pos_tags <- function(a) {
    stopifnot(inherits(a, "AnnotatedPlainTextDocument"))
    pos <- features(a, "word")$POS
    b <- pos %in% word_pos_tags()
    short_pos <- substr(pos, 1, 2)
    tokens <- words(a)
    lem <- tolower(features(a, "word")$lemma)

    n_sentences <- length(sents(a))

    b_adjectives <- short_pos == "JJ" ## Feng+et.al.:2010
    b_adverbs <- short_pos == "RB" ## Feng+et.al.:2010
    b_nouns <- short_pos == "NN" ## Feng+et.al.:2010
    b_prepositions <- short_pos == "IN" ## Feng+et.al.:2010
    b_verbs <- short_pos == "VB" ## Feng+et.al.:2010
    b_pronouns  <- (short_pos == "PP") | (short_pos == "PR") ## Islam+Mehler:2013
    b_determiners <- short_pos == "DT" ## Islam+Mehler:2013
    b_cooconj <- short_pos == "CC" ## Vogel+Washburne:1928

    b_unique_lemma <- !duplicated(lem)

    n_words        <- sum(b)
    n_adjectives   <- sum(b_adjectives)
    n_adverbs      <- sum(b_adverbs)
    n_nouns        <- sum(b_nouns)
    n_prepositions <- sum(b_prepositions)
    n_verbs        <- sum(b_verbs)
    n_pronouns     <- sum(b_pronouns)
    n_determiners  <- sum(b_determiners)
    n_cooconj      <- sum(b_cooconj) #coordinating conjunctions

    unique_class_count <- function(b) length(unique(tokens[b]))
    n_unique <- length(unique(lem))
    n_unique_adjectives   <- unique_class_count(n_adjectives)
    n_unique_adverbs      <- unique_class_count(b_adverbs)
    n_unique_nouns        <- unique_class_count(b_nouns)
    n_unique_prepositions <- unique_class_count(b_prepositions)
    n_unique_verbs        <- unique_class_count(b_verbs)
    n_unique_pronouns     <- unique_class_count(b_pronouns)
    n_unique_determiners  <- unique_class_count(b_determiners)
    n_unique_cooconj      <- unique_class_count(b_cooconj)

    unique_lemma_class_count <- function(b) length(unique(lem[b]))
    n_unique_lemmas <- length(unique(lem))
    n_unique_lemmas_adjectives   <- unique_lemma_class_count(n_adjectives)
    n_unique_lemmas_adverbs      <- unique_lemma_class_count(b_adverbs)
    n_unique_lemmas_nouns        <- unique_lemma_class_count(b_nouns)
    n_unique_lemmas_prepositions <- unique_lemma_class_count(b_prepositions)
    n_unique_lemmas_verbs        <- unique_lemma_class_count(b_verbs)
    n_unique_lemmas_pronouns     <- unique_lemma_class_count(b_pronouns)
    n_unique_lemmas_determiners  <- unique_lemma_class_count(b_determiners)
    n_unique_lemmas_cooconj      <- unique_lemma_class_count(b_cooconj)

    ## nchar
    nc_words <- nchar(words(a))
    nc_adjectives   <- sum(nc_words[b_adjectives])
    nc_adverbs      <- sum(nc_words[b_adverbs])
    nc_nouns        <- sum(nc_words[b_nouns])
    nc_prepositions <- sum(nc_words[b_prepositions])
    nc_verbs        <- sum(nc_words[b_verbs])
    nc_pronouns     <- sum(nc_words[b_pronouns])
    nc_determiners  <- sum(nc_words[b_determiners])
    nc_cooconj      <- sum(nc_words[b_cooconj])


    pc <- list(
      n_sentences = n_sentences, n_words = n_words, n_adjectives = n_adjectives, 
      n_adverbs = n_adverbs, n_nouns = n_nouns, n_prepositions = n_prepositions, 
      n_verbs = n_verbs, n_pronouns = n_pronouns, n_determiners = n_determiners,
      n_cooconj = n_cooconj, 

      n_unique = n_unique, n_unique_adjectives = n_unique_adjectives, 
      n_unique_adverbs = n_unique_adverbs, n_unique_nouns = n_unique_nouns, 
      n_unique_prepositions = n_unique_prepositions, n_unique_verbs = n_unique_verbs,
      n_unique_pronouns = n_unique_pronouns, n_unique_determiners = n_unique_determiners,
      n_unique_cooconj = n_unique_cooconj,

      n_unique_lemmas = n_unique_lemmas, 
      n_unique_lemmas_adjectives = n_unique_lemmas_adjectives, 
      n_unique_lemmas_adverbs = n_unique_lemmas_adverbs, 
      n_unique_lemmas_nouns = n_unique_lemmas_nouns, 
      n_unique_lemmas_prepositions = n_unique_lemmas_prepositions, 
      n_unique_lemmas_verbs = n_unique_lemmas_verbs,
      n_unique_lemmas_pronouns = n_unique_lemmas_pronouns,
      n_unique_lemmas_determiners = n_unique_lemmas_determiners,
      n_unique_lemmas_cooconj = n_unique_lemmas_cooconj,
      
      nc_adjectives = nc_adjectives, nc_adverbs = nc_adverbs, nc_nouns = nc_nouns, 
      nc_prepositions = nc_prepositions, nc_verbs = nc_verbs, nc_pronouns = nc_pronouns,
      nc_determiners = nc_determiners, nc_cooconj = nc_cooconj
    )
    return(pc)
}

## pc ... pos counts 
## nc ... number of characters
create_pos_features <- function(pc) {
  list(
    # 1 percentage of POS per document
    r_adjectives = pc$n_adjectives / pc$n_words,
    r_adverbs = pc$n_adverbs / pc$n_words,
    r_nouns = pc$n_nouns / pc$n_words,
    r_prepositions = pc$n_prepositions / pc$n_words,
    r_verbs = pc$n_verbs / pc$n_words,
    r_pronouns = pc$n_pronouns / pc$n_words,
    r_determiners = pc$n_determiners / pc$n_words,
    r_cooconj = pc$n_cooconj / pc$n_words,

    # 2 percentage of unique POS per document
    r_unique_adjectives = pc$n_unique_adjectives / pc$n_words,
    r_unique_adverbs = pc$n_unique_adverbs / pc$n_words,
    r_unique_nouns = pc$n_unique_nouns / pc$n_words,
    r_unique_prepositions = pc$n_unique_prepositions / pc$n_words,
    r_unique_verbs = pc$n_unique_verbs / pc$n_words,
    r_unique_pronouns = pc$n_unique_pronouns / pc$n_words,
    r_unique_determiners = pc$n_unique_determiners / pc$n_words,
    r_unique_cooconj = pc$n_unique_cooconj / pc$n_words,

    # 3 percentage of unique POS per unique words in a document
    r_unique_adjectives_pty = pc$n_unique_adjectives / pc$n_unique,
    r_unique_adverbs_pty = pc$n_unique_adverbs / pc$n_unique,
    r_unique_nouns_pty = pc$n_unique_nouns / pc$n_unique,
    r_unique_prepositions_pty = pc$n_unique_prepositions / pc$n_unique,
    r_unique_verbs_pty = pc$n_unique_verbs / pc$n_unique,
    r_unique_pronouns_pty = pc$n_unique_pronouns / pc$n_unique,
    r_unique_determiners_pty = pc$n_unique_determiners / pc$n_unique,
    r_unique_cooconj_pty = pc$n_unique_cooconj / pc$n_unique,

    # 4 average number of POS per sentence
    avg_adjectives_ps = pc$n_adjectives / pc$n_sentences,
    avg_adverbs_ps = pc$n_adverbs / pc$n_sentences,
    avg_nouns_ps = pc$n_nouns / pc$n_sentences,
    avg_prepositions_ps = pc$n_prepositions / pc$n_sentences,
    avg_verbs_ps = pc$n_verbs / pc$n_sentences,
    avg_pronouns_ps = pc$n_pronouns / pc$n_sentences,
    avg_determiners_ps = pc$n_determiners / pc$n_sentences,
    avg_cooconj_ps = pc$n_cooconj / pc$n_sentences,

    # 5 average number of unique POS per sentence
    avg_unique_adjectives_ps = pc$n_unique_adjectives / pc$n_sentences,
    avg_unique_adverbs_ps = pc$n_unique_adverbs / pc$n_sentences,
    avg_unique_nouns_ps = pc$n_unique_nouns / pc$n_sentences,
    avg_unique_prepositions_ps = pc$n_unique_prepositions / pc$n_sentences,
    avg_unique_verbs_ps = pc$n_unique_verbs / pc$n_sentences,
    avg_unique_pronouns_ps = pc$n_unique_pronouns / pc$n_sentences,
    avg_unique_determiners_ps = pc$n_unique_determiners / pc$n_sentences,
    avg_unique_cooconj_ps = pc$n_unique_cooconj / pc$n_sentences,

    # 6 average number of characters by POS tag
    avg_nc_adjectives = pc$nc_adjectives / pc$n_adjectives,
    avg_nc_adverbs = pc$nc_adverbs / pc$n_adverbs,
    avg_nc_nouns = pc$nc_nouns / pc$n_nouns,
    avg_nc_prepositions = pc$nc_prepositions / pc$n_prepositions,
    avg_nc_verbs = pc$nc_verbs / pc$n_verbs,
    avg_nc_pronouns = pc$nc_pronouns / pc$n_verbs,
    avg_nc_determiners = pc$nc_determiners / pc$n_verbs,
    avg_nc_cooconj = pc$nc_cooconj / pc$n_verbs #, 

    # 7 percentage of unique POS lemmas per document
    #r_unique_lem_adjectives = pc$n_unique_lemmas_adjectives / pc$n_words,
    #r_unique_lem_adverbs = pc$n_unique_lemmas_adverbs / pc$n_words,
    #r_unique_lem_nouns = pc$n_unique_lemmas_nouns / pc$n_words,
    #r_unique_lem_prepositions = pc$n_unique_lemmas_prepositions / pc$n_words,
    #r_unique_lem_verbs = pc$n_unique_lemmas_verbs / pc$n_words,
    #r_unique_lem_pronouns = pc$n_unique_lemmas_pronouns / pc$n_words,
    #r_unique_lem_determiners = pc$n_unique_lemmas_determiners / pc$n_words,
    #r_unique_lem_cooconj = pc$n_unique_lemmas_cooconj / pc$n_words,
    
    # 8 percentage of unique POS lemmas per unique words in a document
    #r_unique_lem_adjectives_pty = pc$n_unique_lemmas_adjectives / pc$n_unique_lemmas,
    #r_unique_lem_adverbs_pty = pc$n_unique_lemmas_adverbs / pc$n_unique_lemmas,
    #r_unique_lem_nouns_pty = pc$n_unique_lemmas_nouns / pc$n_unique_lemmas,
    #r_unique_lem_prepositions_pty = pc$n_unique_lemmas_prepositions / pc$n_unique_lemmas,
    #r_unique_lem_verbs_pty = pc$n_unique_lemmas_verbs / pc$n_unique_lemmas,
    #r_unique_lem_pronouns_pty = pc$n_unique_lemmas_pronouns / pc$n_unique_lemmas,
    #r_unique_lem_determiners_pty = pc$n_unique_lemmas_determiners / pc$n_unique_lemmas,
    #r_unique_lem_cooconj_pty = pc$n_unique_lemmas_cooconj / pc$n_unique_lemmas

    # r_verbs_to_nouns = (pc$n_adverbs + pc$n_verbs) / pc$n_nouns
    )
}


#'
#' ## Parse based Features
#'
#' ### Parse Trees
#'
#' average parse tree height
#' average number of Subordinating Conjunctions
#' average number of noun phrases
#' average number of verb phrases
#' average number of prepositional phrases
tree_height <- function(x) {
  .height <- function(child) {
    if ( inherits(child, what = "Tree") ) {
      return(tree_height(child))
    }
    return(0L)
  }
  return(1 + max(as.integer(lapply(x$children, .height))))
}

tree_values <- function(tree) {
  val <- character()
  .tree_values <- function(tree) {
    if ( inherits(tree, what = "Tree") ) {
      val <<- c(val, tree$value)
      lapply(tree$children, .tree_values)
      return(NULL)
    } else {
      return(NULL)
    }
  }
  .tree_values(tree)
  val
}

count_sbars <- function(ptree) sum(unlist(ptree, use.names = FALSE) == "SBAR")

# NP noun phrases
# VP verb phrases
# PP prepositional phrases
# ADVP adverb phrases
# ADJP adjective phrases
# ALLP all phrases
count_phrases <- function(ptree) {
  phrase_types <- c("NP", "VP", "PP", "ADVP", "ADJP", "ALLP")
  # syn_tag_count .. syntactic tag counts
  syn_tag_count <- table(tree_values(ptree))
  counts <- setNames(syn_tag_count[phrase_types], phrase_types)
  ## all phrases Vogel+Washburne:1928
  nc <- nchar(names(syn_tag_count))
  counts['ALLP'] <- sum(syn_tag_count[substr(names(syn_tag_count), nc, nc) == "P"])
  counts[is.na(counts)] <- 0L
  counts
}

ratio_phrases <- function(ptrees) {
  phrase_types <- c("NP", "VP", "PP", "ADVP", "ADJP", "ALLP")
  rel_freq <- prop.table(table(unlist(lapply(ptrees, tree_values))))
  nc <- nchar(names(rel_freq))
  rel_freq['ALLP'] <- sum(rel_freq[substr(names(rel_freq), nc, nc) == "P"])
  rel_freq <- setNames(rel_freq[phrase_types], phrase_types)
  rel_freq[is.na(rel_freq)] <- 0
  rel_freq
}

create_parse_tree_features <- function(a) {
  stopifnot(inherits(a, "AnnotatedPlainTextDocument"))
  ptrees <- lapply(features(a, "sentence")$parse, Tree_parse)
  phrase_counts <- sapply(ptrees, count_phrases)
  avg_phrase_counts <- rowMeans(phrase_counts)
  phrase_ratios <- ratio_phrases(ptrees)

  ## Kate+2010 proportion of sentences with no verb phrase
  avg_no_VP_ps <- mean(phrase_counts["VP",] == 0)

  list(avg_ptree_height = mean(sapply(ptrees, tree_height)),
    avg_subord_conj = mean(sapply(ptrees, count_sbars)),
    avg_NP = unname(avg_phrase_counts["NP"]),
    avg_VP = unname(avg_phrase_counts["VP"]),
    avg_PP = unname(avg_phrase_counts["PP"]),
    avg_ADVP = unname(avg_phrase_counts["ADVP"]),
    avg_ADJP = unname(avg_phrase_counts["ADJP"]),
    avg_ALLP = unname(avg_phrase_counts["ALLP"]),
    r_NP = unname(phrase_ratios["NP"]), ## proportion of noun phrases (relative to all tags assigned in the parse tree)
    r_VP = unname(phrase_ratios["VP"]),
    r_PP = unname(phrase_ratios["PP"]),
    r_ADVP = unname(phrase_ratios["ADVP"]),
    r_ADJP = unname(phrase_ratios["ADJP"]),
    r_ALLP = unname(phrase_ratios["ALLP"]), 
    avg_no_VP_ps = avg_no_VP_ps)
}


#'
#' ## Dependency Parse
#'
#' average number of passives
## 
create_passive_features <- function(a) {
  stopifnot(inherits(a, "AnnotatedPlainTextDocument"))
  bdeps <- features(a, "sentence")[["basic-dependencies"]]
  count_passives <- function(bdep) {
    b <- bdep$type %in% c("auxpass", "csubjpass", "nsubjpass")
    length(unique(bdep[b, "gid"]))
  }
  list(avg_passives = mean(sapply(bdeps, count_passives)))
}


#'
#' # Coreference based Features
#'
#' `avg_coref_count` average number of coreferences by document
#'

## <<FIXME!:
##     Die Coref Spans
get_coreferences <- function(a) {
  d <- as.data.frame(annotation(a))[-5]
  d$sid <- cumsum(d$type == "sentence")
  d <- d[d$type == "word", -1]
  colnames(d)[2:3] <- c("char_start", "char_end")
  d$wid <- seq_len(nrow(d))
  d$words <- words(a)
  swid <- split(seq_along(d$sid), d$sid)
  d$wid2 <- unlist(lapply(swid, seq_along))

  corefs <- features(a, "document")$coreference[[1L]]
  names(corefs) <- seq_along(corefs)
  corefs <- do.call(rbind, corefs)
  corefs$cid <- as.integer(gsub("\\..*", "", rownames(corefs)))  
  corefs <- merge(corefs, d, by.x = c("sentence", "start"), by.y = c("sid", "wid2"))
  i <- with(corefs, order(cid, char_start))
  col_order <- c("cid", "sentence", "wid", "char_start", "char_end", "representative", "words")
  corefs[i, col_order]
}

## average chain span 
## (Feng 2010)
## The length of a chain is the number of entities contained in the chain. 
## The span of chain is the distance between the index of the first and 
## last entity in a chain.
coref_spans <- function(corefs, a) {
  anno_type <- annotation(a)$type
  sentence_id <- cumsum(anno_type == "sentence")[anno_type == "word"]
  swid <- split(seq_along(sentence_id), sentence_id)
  chain_span <- function(coref) {
    coref <- coref[order(coref[, "sentence"], coref[, "start"]),]
    first <- swid[[coref[1, "sentence"]]][coref[1, "start"]]
    last <- swid[[coref[nrow(coref), "sentence"]]][coref[nrow(coref), "end"]]
    last - first + 1L
  }
  sapply(corefs, chain_span)
}

create_coref_features <- function(a, n_sentences, n_words, n_entities) {
  stopifnot(inherits(a, "AnnotatedPlainTextDocument"))
  corefs <- features(a, "document")$coreference[[1L]]
  n_coref_chains <- length(corefs)
  cospans <- coref_spans(corefs, a)
  long_coref_count <- sum(cospans >= (length(words(a)) / 2))

  avg_num_of_coreferences_per_chain <- mean(sapply(corefs, NROW))
  avg_coref_chain_span <- mean(cospans)

  corefs <- get_coreferences(a)
  inference_distance <- tapply(corefs$char_start, corefs$cid, diff)
  avg_coref_inference_distance <- mean(unlist(inference_distance))
  median_coref_inference_distance <- median(unlist(inference_distance))
  r_coref_per_words <- nrow(corefs) / n_words
  r_coref_per_entities <- nrow(corefs) / n_entities

  list(avg_num_coref_per_chain = avg_num_of_coreferences_per_chain,
       avg_coref_chain_span = avg_coref_chain_span,
       r_long_corefs = long_coref_count / n_coref_chains, # changed!
       avg_coref_chains_ps = n_coref_chains / n_sentences,
       avg_coref_inference_distance = avg_coref_inference_distance,
       median_coref_inference_distance = median_coref_inference_distance,
       r_coref_per_words = r_coref_per_words,
       r_coref_per_entities = if (is.finite(r_coref_per_entities)) r_coref_per_entities else 0
       )
}


#'
#' # Dictonary based Features
#' (Syntactic Features)
#' Content Words.
is_function_word <- function(x) x %in% function_words()

count_content_words <- function(x) sum(!is_function_word(x))
ratio_content_words <- function(x) count_content_words(x) / length(x)

create_dictionary_based_features <- function(dwords) {
  list(r_content_words = ratio_content_words(dwords))
}


#'
#' ## NER based Features
#'
named_entities <- function() {
  c("CITY", "COUNTRY", "LOCATION", "MISC", "NATIONALITY", "ORGANIZATION", 
    "PERSON", "STATE_OR_PROVINCE")
}

is_named_entity <- function(x) {
  x %in% named_entities()
}

# Feng+2009
# The first set of features incorporates the Ling-Pipe named entity detection 
# software (Alias-i, 2008), which detects three types of entities: 
# person, location, and organization. 
# We also use the part-of-speech tagger in LingPipe to identify the
# common nouns in the document, and we find the 
# union of the common nouns and the named entity noun phrases in the text. 
# The union of these two sets is our definition of “entity” for this set of features. 
# We count both the 
#   total number of “entity mentions” in a text (each token appearance of an entity) 
# and the 
#   total number of unique entities (exact-string-match duplicates only counted once). 
# Table 1 lists these features: nEM, nUE, aEM, and aUE.
extract_named_entities <- function(wdf) {
  ne_tags <- wdf$ner
  # table(ne_tags)
  is_eq_prev <- head(ne_tags, -1) == tail(ne_tags, -1)
  is_ne <- is_named_entity(head(ne_tags, -1))
  ne_ids <- cumsum(c(1, !(is_eq_prev & is_ne)))
  ne <- split(wdf$word, ne_ids)
  ne_tag <- sapply(split(ne_tags, ne_ids), "[[", 1)
  b <- is_named_entity(ne_tag)
  d <- data.frame(ids = seq_len(sum(b)), stringsAsFactors = FALSE)
  ## ids matches to wdf$id
  d$ids <- split(wdf$id, ne_ids)[b]
  d$sid <- sapply(split(wdf$sid, ne_ids)[b], "[[", 1)
  d$entity <- ne[b]
  d$tag <- ne_tag[b]
  d  
}

## Hier beruecksischtige ich nicht dass 2 entites direkt hinereinander
## auftreten koennten und trotzdem unterschiedliche sind.
## We want to count ``Indian National Chemistry Olympiad'' as one entity, 
## if we would count it as 4 entities we just could take the sum.
# count_entities <- function(a) {
#   ner <- features(a, "word")$NER 
#   b <- is_entity(ner)
#   sum(diff(seq_along(b)[b]) > 1) + 1L
# }

## *_ps    per sentence
##
## Lijun Feng - Automatic Readability Assessment - 2010 - Dissertation 
## "overlapping nouns" refer to general nouns that appear in named entities
## "remaining nouns" refer to the set of general nouns with overlapping nouns removed
## entities.
##
## general nouns (nouns and proper nouns)
##
## TODO: Sollte ``Indian National Chemistry Olympiad'' als 1 Wort gezaehlt werden?
count_entities <- function(wdf) {
  stopifnot(inherits(wdf, "word.annotation.frame"))
  short_pos <- substr(wdf$pos, 1, 2)
  xwords <- tolower(wdf$word)
  
  is_noun <- short_pos == "NN"
  is_ne <- is_named_entity(wdf$ner)
  
  n_entities <- sum(is_noun | is_ne)
  # n_unique_entities <- length(unique(lem[is_noun | is_ne]))
  n_unique_entities <- length(unique(xwords[is_noun | is_ne]))
  n_named_entities <- sum(is_ne)
  # n_unique_named_entities <- length(unique(lem[is_ne]))
  n_unique_named_entities <- length(unique(xwords[is_ne]))

  n_overlapping_nouns <- sum(is_noun & is_ne)
  n_remaining_nouns <- sum(is_noun & !is_ne)

  ec <- list(n_entities = n_entities, n_unique_entities = n_unique_entities,
    n_named_entities = n_named_entities, n_unique_named_entities = n_unique_named_entities,
    n_overlapping_nouns = n_overlapping_nouns, n_remaining_nouns = n_remaining_nouns)
  ec
}

create_entity_features <- function(ec, wdf, n_words, n_types, n_sentences) {
  entities <- extract_named_entities(wdf)
  nchar_entities <- if (NROW(entities)) sapply(entities$entity, function(s) sum(nchar(s))) else 0
  avg_entity_len <- mean(nchar_entities)
  r_entity_len <- sum(nchar_entities) / sum(nchar(wdf$word[wdf$pos %in% word_pos_tags()]))
  r_nent_to_ent <- ec$n_named_entities / ec$n_entities



  list(
    r_entities = ec$n_entities / n_words,
    r_uentities = ec$n_unique_entities / n_types,
    avg_entities_ps = ec$n_entities / n_sentences, 
    avg_uentities_ps = ec$n_unique_entities / n_sentences,
    
    r_named_entities = ec$n_named_entities / n_words, 
    r_unamed_entities = ec$n_unique_named_entities / n_types, 
    avg_named_entities_ps = ec$n_named_entities / n_sentences, 
    avg_unamed_entities_ps = ec$n_unique_named_entities / n_sentences,
    r_nent_to_ent = if (is.finite(r_nent_to_ent)) r_nent_to_ent else 0,

    ## r_remaining_nouns = ec$n_remaining_nouns / ec$n_entities, ## adds to 1 with overlapping nouns
    r_overlapping_nouns = ec$n_overlapping_nouns / n_words,

    avg_named_entity_len = avg_entity_len,
    r_named_entity_len = r_entity_len
  )
}

## word overlap between adjacent sentences
word_overlap <- function(wdf) {
  stopifnot(inherits(wdf, "word.annotation.frame"))
  b <- wdf$pos %in% word_pos_tags()
  wdf_words <- wdf[b,]
  short_pos <- substr(wdf$pos, 1, 2)
  # Pittler:2008 word overlap between nouns and pronouns 
  wdf_nouns <- wdf[substr(wdf$pos, 1, 2) == (short_pos == "NN") | (short_pos == "PR"),] 
  sids <- as.character(unique(wdf$sid))

  sent_lemma <- split(tolower(wdf_words$lemma), wdf_words$sid)[sids]
  sent_noun_lemma <- split(tolower(wdf_nouns$lemma), wdf_nouns$sid)[sids]

  overlap <- function(s1, s2) {
    if ( length(s1) == 0L | length(s2) == 0L ) return(0)
    length(intersect(s1, s2)) / length(union(s1, s2))
  }
  
  noun_overlap_index <- overlap_index <- double(length(sent_lemma) - 1L)
  for (i in seq_len(length(sent_lemma) - 1L)) {
    overlap_index[i] <- overlap(sent_lemma[[i]], sent_lemma[[i + 1L]])
    noun_overlap_index[i] <- overlap(sent_noun_lemma[[i]], sent_noun_lemma[[i + 1L]])
  }

  list(avg_word_overlap = mean(overlap_index), 
       avg_noun_overlap = mean(noun_overlap_index))
}

create_lexical_cohesion_features <- function(wdf) {
  w_overlap <- word_overlap(wdf)
  # head(wdf)
}

#' @title Readability Features
#' @description 
#' @param a an object inheriting from class \code{"AnnotatedPlainTextDocument"}.
#' @param id an optional identifier.
#' @param counts a boolean selecting if the count variables should be returned.
#' @return an object inheriting from \code{"data.frame"}.
#' @examples
#'
readability_features <- function(a, id = NULL, counts = FALSE) {
  if ( is.null(id) ) {
    if ( is.null(meta(a, "id")) ) {
      id <- "row"
    } else {
      id <- meta(a, "id")
    }
  }

  is_word <- annotation(a)$type == "word"
  wdf <- data.frame(id = annotation(a)$id[is_word],
    sid = cumsum(annotation(a)$type == "sentence")[is_word],
    word = words(a), lemma = features(a, "word")$lemma,
    pos = features(a, "word")$POS, ner = features(a, "word")$NER,
    stringsAsFactors = FALSE)
  class(wdf) <- c("word.annotation.frame", "data.frame")

  b <- features(a, "word")$POS %in% word_pos_tags()
  dwords <- wdf[b, "word"]
  dlemmas <- wdf[b, "lemma"]
  dsents <- sents(a)

  n_words <- length(dwords)
  n_types <- length(unique(dwords))
  n_sentences <- length(dsents)

  dsyll_count <- unlist(sylcount::sylcount(dwords), FALSE, FALSE)
  
  trad_feat <- create_traditional_features(dwords, dsents, dsyll_count, dlemmas)

  pos_counts <- count_pos_tags(a)
  pos_feat <- create_pos_features(pos_counts)

  entity_counts <- count_entities(wdf)
  # ec <- entity_counts
  entity_feat <- create_entity_features(entity_counts, wdf, n_words, n_types, n_sentences)
  n_entities <- entity_counts$n_entities

  ptree_feat <- create_parse_tree_features(a)
  pass_feat <- create_passive_features(a)
  coref_feat <- create_coref_features(a, n_sentences, n_words, n_entities)
  word_overlap_feat <- word_overlap(wdf)
  dict_feat <- create_dictionary_based_features(dwords)

  if ( isTRUE(counts) ) {
    counts <- list(n_words = n_words, n_sentences = n_sentences, n_syl = sum(dsyll_count))
    counts <- c(counts, pos_counts, entity_counts)
    feat <- c(trad_feat, pos_feat, ptree_feat, entity_feat, pass_feat, 
      coref_feat, word_overlap_feat, dict_feat)
    read_feat <- c(counts, feat)
  } else {
    read_feat <- c(trad_feat, pos_feat, ptree_feat, entity_feat, pass_feat, 
      coref_feat, word_overlap_feat, dict_feat)  
  }

  class(read_feat) <- c("feature.frame", "data.frame")
  attr(read_feat, "row.names") <- id

  read_feat
}

