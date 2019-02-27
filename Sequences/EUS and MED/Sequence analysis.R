library(ggplot2)

# Using ggplot to look at the evenness and faith's diversity split by region of sample and species.
Sequence.data <- read.csv("/media/removable/USBDRIVEONE/Research/Sequences/EUS and MED/EUSMED.sample.results.csv")

ggplot(Sequence.data,aes(x = Region, y = faithpd, color = Species)) + geom_boxplot()

ggplot(Sequence.data,aes(x = Region, y = pielouevenness, color = Species)) + geom_boxplot()


library(themetagenomics)

# Using themetagenomics package to explore the functional profiles of the 16S sequences

exported.table <- read.table("/home/erikpark/external/Research/Sequences/EUS and MED/exported/feature-table.txt", header = TRUE, sep = "\t")
exported.tax <- read.table("/home/erikpark/external/Research/Sequences/EUS and MED/exported/taxonomy.tsv", header = TRUE, sep = "\t")
tax.table <- merge(exported.table,exported.tax, by = "Feature.ID")
final.table <- tax.table[,-1]
rownames(final.table) <- tax.table[,1]

taxonomy.table <- tax.table[,c(1,14)]
# Generating new dataframe with just taxonomy information


library(reshape2)
taxa <- with(taxonomy.table, cbind(Feature.ID, colsplit(taxonomy.table$Taxon, pattern = "\\;", names = c('Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species'))))
# Splitting existing 'Taxon' column into 7 new appropriate taxonomy level columns

taxa[] <- lapply(taxa, gsub, pattern = '.*__', replacement = '')
# Remove non-informative characters from taxa names

taxonomy.table.final <- taxa[,-1]
rownames(taxonomy.table.final) <- taxa[,1]
# Setting 'Feature.ID' as the row name, because that's what the package wants to see.

#Above to import feature table, taxonomy, then merge the two by featureID, and then filter the tables to be in the proper format.


meta <- read.table("/home/erikpark/external/Research/Sequences/EUS and MED/EUSMED-metadata.tsv", header = TRUE, sep = "\t", row.names = 1)

# Above to import and prepare metadata file for use in prepare_data command


refs <- c('MED','O. taurus', 'Native')

Clean.data <- prepare_data(otu_table = final.table[,1:12],
                           rows_are_taxa = TRUE,
                           tax_table = taxonomy.table.final,
                           metadata = meta,
                           formula = ~ Region + Species + Introduction.status,
                           refs = refs,
                           cn_normalize = FALSE,
                           seed = 1411,
                           drop = TRUE)
# Above to generate themetadata object by introducing my data including the taxonomic info, and sample metadata.
# The entered formula says that I am interested in examining how taxa differ between samples of the listed factors.

Topics <- find_topics(Clean.data, K = 15)

Topic.effects <- est(Topics)

# Above to convert OTU counts to estimated topics, and then compare them to measured effects from factors from our formula.
#vis(Topic.effects, type = 'taxa')

#Now that we have topics defined, will move to functional analysis.

system.time({
  tmp <- tempdir()
  download_ref(tmp,reference='silva_ko',overwrite=FALSE)
  Functions <- predict(Topics , reference='silva_ko' , reference_path=tmp , cn_normalize=TRUE , sample_normalize=FALSE, scalar = 100)
})
# Functions object now contains all of the predicted KO terms for Silva annotated abundances.
# Will now identify meaningful genes for these functions using the below, for hierarchical level 3 of KEGG.

system.time(Function.effects <- est(Functions, level = 3, iters = 600,
                                    chains = 2, cores = 2,
                                    verbose = TRUE,
                                    prior = c('normal','normal','normal')))


vis(Function.effects, Topic.effects, ui_level = .95)
