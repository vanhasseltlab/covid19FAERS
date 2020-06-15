### Plot results for figures in paper, export results table                            ###
### Sonja Boman and Laura Zwep                                                         ###
### ---------------------------------------------------------------------------------

#required packages
packages <- c("tidyverse", "xlsx")

for (package in packages) {
  #install missing packages
  if (!package %in% installed.packages()[, "Package"]) {
    install.packages(package)
  }
  #load packages
  library(package, character.only = TRUE)
}




#Import data
#load ADR_results_curated in the environment (curation_of_ADRs.R)
load("results/ADR_results_low_level_curated.Rdata")
#load ADR_top_results_curated in the environment (curation_of_ADRs.R)
load("results/ADR_results_top_level_curated.Rdata")
#load ADR_high_results_curated in the environment (curation_of_ADRs.R)
load("results/ADR_high_results_curated.Rdata")

ADR_ontology <- read.delim("data/clean/ADR_ontology.txt", quote = "\"", header = T, sep = "\t", 
                           stringsAsFactors = F)

#Figure 1
level_classes <- rbind(data.frame(grid = "1", ADR_top = "blood_and_lymphatic_tissue", ADR_high = c('white blood cell disorders',"venous varices","spleen, lymphatic and reticuloendothelial system disorders","anaemias nonhaemolytic and marrow depression","haematopoietic neoplasms (excl leukaemias and lymphomas)","haemolyses and related conditions","plasma cell neoplasms","red blood cell disorders")),
                       data.frame(grid = "1", ADR_top = "urinary_system", ADR_high = c("urolithiases","urinary tract signs and symptoms","urethral disorders","nephropathies")),
                       data.frame(grid = "1", ADR_top = "metabolic_system", ADR_high = c("vitamin related disorders","iron and trace metal metabolism disorders","lipid metabolism disorders","metabolism disorders nec","purine and pyrimidine metabolism disorders")),
                       data.frame(grid = "1", ADR_top = "muscoskeletal_system", ADR_high = c("synovial and bursal disorders","skeletal neoplasms malignant and unspecified","skeletal neoplasms benign","bone disorders (excl congenital and fractures)","bone, calcium, magnesium and phosphorus metabolism disorders","joint disorders","musculoskeletal and connective tissue deformities (incl intervertebral disc disorders)")),
                       data.frame(grid = "1", ADR_top = "neurological", ADR_high = c("spinal cord and nerve root disorders","central nervous system infections and inflammations","demyelinating disorders","neurological disorders of the eye","ocular neuromuscular disorders","peripheral neuropathies","retina, choroid and vitreous haemorrhages and vascular disorders")),
                       data.frame(grid = "2", ADR_top = "dermatological", ADR_high = c("skin neoplasms malignant and unspecified","skin and subcutaneous tissue disorders nec","cutaneous neoplasms benign")),
                       data.frame(grid = "2", ADR_top = "endocrinal", ADR_high = c("adrenal gland disorders","endocrine neoplasms benign","parathyroid gland disorders")),
                       data.frame(grid = "2", ADR_top = "gastrointestinal", ADR_high = c("anal and rectal conditions nec","gastrointestinal stenosis and obstruction")),
                       data.frame(grid = "2", ADR_top = "immunological", ADR_high = c("autoimmune disorders","body temperature conditions","immunodeficiency syndromes")),
                       data.frame(grid = "2", ADR_top = "cardiac", ADR_high = c("cardiac valve disorders", "cardiac disorder signs and symptoms", "cardiac neoplasms", "coronary artery disorders", "cardiac arrhythmias", "heart failures", "endocardial disorders", "	pericardial disorders", "myocardial disorders", "cardiac valve disorders")),
                       data.frame(grid = "2", ADR_top = "hepatic", ADR_high = c("hepatic and hepatobiliary disorders","hepatobiliary neoplasms")),
                       data.frame(grid = "2", ADR_top = "psychological", ADR_high = c("disturbances in thinking and perception","hepatobiliary neoplasms malignant and unspecified","psychiatric and behavioural symptoms nec")),
                       data.frame(grid = "2", ADR_top = "respiratory", ADR_high = c("lower respiratory tract disorders (excl obstruction and infection)","respiratory and mediastinal neoplasms benign (excl mesotheliomas)")),
                       data.frame(grid = "2", ADR_top = "other", ADR_high = c("soft tissue neoplasms malignant and unspecified","administration site reactions","connective tissue disorders (excl congenital)","dental and gingival conditions","exposures, chemical injuries and poisoning","peritoneal and retroperitoneal conditions","reproductive neoplasms female benign")))

lvl <- levels(level_classes$ADR_top)
lvl <- Hmisc::capitalize(str_replace_all(lvl, "_", " "))
levels(level_classes$ADR_top) <- lvl

ADR_high_plot <- ADR_high_results_curated %>% 
  filter(ADR %in% level_classes$ADR_high) %>% 
  rename(ADR_high = ADR) %>% 
  left_join(level_classes) %>% 
  filter(reject_null)

ADR_high_plot$ADR_high[ADR_high_plot$ADR_high == "musculoskeletal and connective tissue deformities (incl intervertebral disc disorders)"] <- "musculoskeletal and connective tissue deformities"

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

ROR_range <- range(ADR_high_plot$ROR)
figure1a_unscaled <- ADR_high_plot %>% filter(grid == 1) %>% 
  ggplot(aes(x = drug, y = ADR_high, fill = ROR)) +
    geom_tile() +
    geom_hline(yintercept = (1:70 + 0.5), colour = "grey60") +
    geom_vline(xintercept = (1:20 + 0.5), colour = "grey60") +
    scale_fill_gradient(low = "royalblue", high = "orangered", limits = ROR_range) +
    labs(y = "", x = "", fill = "Minimal ROR") +
    scale_x_discrete(drop = FALSE) +
    theme_bw() +
    facet_wrap(~ ADR_top, scales = "free_y", ncol = 1) +
    #facet_grid(~ grid, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid.major = element_blank(), strip.background = element_rect(fill = NA),
          legend.position = "top",
          panel.spacing = unit(0,"cm"),
          strip.text.x = element_text(angle = 0, margin = margin(0.05, 0, 0.05, 0, "cm")))+
    guides(fill = guide_colorbar(label.position = "bottom",
                                 title.position = "left",
                                 # draw border around the legend
                                 frame.colour = "black",
                                 barwidth = 8,
                                 barheight = 1.5))
figure1b_unscaled <- ADR_high_plot %>% filter(grid == 2) %>% 
  ggplot(aes(x = drug, y = ADR_high, fill = ROR)) +
  geom_tile() +
  geom_hline(yintercept = (1:70 + 0.5), colour = "grey60") +
  geom_vline(xintercept = (1:20 + 0.5), colour = "grey60") +
  scale_fill_gradient(low = "royalblue", high = "orangered", limits = ROR_range) +
  labs(y = "", x = "Drugs", fill = "Minimal ROR") +
  scale_x_discrete(drop = FALSE) +
  theme_bw() +
  facet_wrap(~ ADR_top, scales = "free_y", ncol = 1) +
  #facet_grid(~ grid, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_blank(), strip.background = element_rect(fill = NA),
        legend.position = "none",
        panel.spacing = unit(0,"cm"),
        strip.text.x = element_text(angle = 0, margin = margin(0.05, 0, 0.05, 0, "cm")))

fig1_legend <- g_legend(figure1a_unscaled)

figure1a_unscaled <- figure1a_unscaled + theme(legend.position = "none")

ScaleFacets <- function(fig) {
  fig_scaled <- ggplotGrob(fig)
  facet.columns <- fig_scaled$layout$t[grepl("panel", fig_scaled$layout$name)]
  y.var <- sapply(ggplot_build(fig)$layout$panel_scales_y,
                  function(l) length(l$range$range))
  fig_scaled$heights[facet.columns] <- fig_scaled$heights[facet.columns] * y.var
  return(fig_scaled)
}

figure1a <- ScaleFacets(figure1a_unscaled)
figure1b <- ScaleFacets(figure1b_unscaled)

grid_mat_long <- matrix(c(1, rep(2, 6), rep(3, 6)), ncol = 1)

pdf(file = "results/figure1.pdf", width = 8, height = 15)
gridExtra::grid.arrange(fig1_legend, figure1a, figure1b,
                        layout_matrix = grid_mat_long, left = "ADE class")
dev.off()


#Figure 2

ADR_fig2 <- ADR_results_curated %>% 
  mutate(literature = ifelse(literature == "Related ADR", "Known ADR", literature)) %>% 
  mutate(literature = as.factor(literature))

PlotFigure2 <- function(ADR_fig2, drug_choice, tag = "A", title_adj = 0) {
  
  g_legend<-function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  plot_hydroxychloroquine1 <- ADR_fig2 %>%
    filter(drug == drug_choice) %>% 
    top_n(n = 20, ROR) %>%
    ggplot(aes(y = ROR, x = reorder(ADR, ROR), color = literature, shape = literature))+
    geom_point()+
    geom_pointrange(aes(ymin = CI_low, ymax = CI_up))+
    coord_flip() +
    labs(x = "ADE", title = tools::toTitleCase(drug_choice)) +
    scale_colour_manual(values = c("firebrick", "darkorange", "blue"), drop = FALSE)+
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    scale_shape_manual(values = c(16, 15, 18), drop = FALSE) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), plot.tag = element_text(size = 19), 
          plot.title = element_text(hjust = title_adj)) +
    guides(colour=guide_legend(nrow = 1, byrow = TRUE))
  
  plot_hydroxychloroquine2 <- ADR_fig2 %>%
    filter(drug == drug_choice & literature == "Unknown ADR") %>% 
    top_n(n = 10, ROR) %>%
    ggplot(aes(y = ROR, x = reorder(ADR, ROR), color = literature, shape = literature))+
    geom_point()+
    geom_pointrange(aes(ymin = CI_low, ymax = CI_up))+
    coord_flip() +
    labs(x = "", title = "   ") +
    scale_colour_manual(values = c("firebrick", "darkorange", "blue"), drop = FALSE) +
    scale_shape_manual(values = c(16, 15, 18), drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_bw() +
    theme(legend.position = "none", plot.tag = element_text(size = 19))
  
  fig_legend <- g_legend(plot_hydroxychloroquine1)
  plot_hydroxychloroquine1 <- plot_hydroxychloroquine1 + theme(legend.position = "none")
  
  return(list(plot_hydroxychloroquine1, plot_hydroxychloroquine2, fig_legend))
}

hydroxychloroquine <- PlotFigure2(ADR_fig2, drug_choice = "hydroxychloroquine", tag = "B", title_adj = 4.05)
chloroquine <- PlotFigure2(ADR_fig2, drug_choice = "chloroquine", tag = "A", title_adj = -3)
lopinavir <- PlotFigure2(ADR_fig2, drug_choice = "lopinavir and ritonavir", tag = "C", title_adj = 6.4)
ribavirin <- PlotFigure2(ADR_fig2, drug_choice = "ribavirin", tag = "D", title_adj = -2.1)


layout_mat2 <- matrix(rep(c(1, 2), each = 4), ncol = 2)
layout_mat_figure2_long <- rbind(rbind(9, layout_mat2, layout_mat2 + 2),
                                 rbind(layout_mat2 + 4, layout_mat2 + 6))

pdf(file = paste0("results/figure2.pdf"), width = 8, height = 15)
gridExtra::grid.arrange(chloroquine[[1]], chloroquine[[2]],
                        hydroxychloroquine[[1]], hydroxychloroquine[[2]],
                        lopinavir[[1]], lopinavir[[2]],
                        ribavirin[[1]], ribavirin[[2]], ribavirin[[3]],
                        layout_matrix = layout_mat_figure2_long)
dev.off()

#Supporting information
results_table <- ADR_results_curated %>% 
  select(c(ADR, ADR_count_total, drug, count, ROR, SE, CI_low, CI_up)) %>% 
  rename(`Adverse Drug Effect` = ADR,
         `number of reports with ADE` = ADR_count_total,
         Drug = drug,
         `Number of reports with ADE/Drug combination` = count,
         `Confidence interval (.95) - lower bound` = CI_low,
         `Confidence interval (.95) - upper bound` = CI_up)

write.xlsx(results_table, file = "results/supporting_information.xlsx", sheetName = "ADE_results", row.names = FALSE)

