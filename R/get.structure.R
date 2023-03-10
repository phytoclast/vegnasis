#Function estimates the vegetation structure based on differential dominance of trees, shrubs, and herbs.

get.structure <- function(x, simple = TRUE){
  if(simple == FALSE){
    x <- x |> mutate(stratum = case_when(
      is.na(ht.max) | is.na(type) ~ 'excluded',
      ht.max > 5  & type %in% c('shrub/vine', 'tree')  ~ 'tree',
      type %in% c('tree') ~ 'sapling',
      type %in% c('shrub/vine') ~ 'shrub',
      type %in% c('grass/grasslike') ~ 'grass',
      type %in% c('forb') ~ 'forb',
      !type %in% c('tree', 'shrub/vine', 'forb','grass/grasslike') ~ 'moss',
      TRUE ~ 'excluded'))

    x.ht <- veg |> group_by(plot) |> summarise(ht.max = max(ht.max))

    x2 <- x |> group_by(plot, stratum) |> summarise(cover = cover.agg(cover))
    x3 <- data.frame(plot = unique(x2$plot), tree = 0, sapling = 0, shrub=0, forb=0, grass=0, moss=0)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'tree')) |> mutate(tree=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'sapling')) |> mutate(sapling=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'shrub')) |> mutate(shrub=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'forb')) |> mutate(forb=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'grass')) |> mutate(grass=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'moss')) |> mutate(moss=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(x.ht)


    x3 <- x3 |> mutate(structure =
                         case_when(tree+shrub+sapling+forb+grass < 10 ~
                                     case_when(moss < 10 ~ 'barren',
                                               TRUE ~ 'mossland'),
                                   TRUE ~ case_when(tree < 10 ~
                                                      case_when(shrub+sapling < 10 ~
                                                                  case_when(grass > forb ~  'grassland',
                                                                            TRUE ~ 'forb meadow'),
                                                                TRUE ~ case_when(tree+shrub+sapling < 75 ~
                                                                                   case_when(sapling > shrub ~ 'woodland regen',
                                                                                             TRUE ~ 'open shrubland'),
                                                                                 TRUE ~ case_when(sapling > shrub ~ 'forest regen',
                                                                                                  TRUE ~ 'shrub thicket')
                                                                )
                                                      ),
                                                    TRUE ~ case_when(tree < 65 ~
                                                                       case_when(shrub+sapling < 10 ~
                                                                                   case_when(ht.max < 15 ~ 'open low woodland',
                                                                                             ht.max < 30 ~ 'open medium woodland',
                                                                                             TRUE ~ 'open high woodland'),
                                                                                 TRUE ~ case_when(sapling > shrub ~
                                                                                                    case_when(shrub+sapling < 75 ~
                                                                                                                case_when(ht.max < 15 ~ 'open regenerating low forest gap',
                                                                                                                          ht.max < 30 ~ 'open regenerating medium forest gap',
                                                                                                                          TRUE ~ 'open regenerating high forest gap'),
                                                                                                              TRUE ~ case_when(ht.max < 15 ~ 'dense regenerating low forest gap',
                                                                                                                               ht.max < 30 ~ 'dense regenerating medium forest gap',
                                                                                                                               TRUE ~ 'dense regenerating high forest gap')),
                                                                                                  TRUE ~ case_when(shrub+sapling < 75 ~
                                                                                                                     case_when(ht.max < 15 ~ 'open shrubby low woodland',
                                                                                                                               ht.max < 30 ~ 'open shrubby medium woodland',
                                                                                                                               TRUE ~ 'open shrubby high woodland'),
                                                                                                                   TRUE ~ case_when(ht.max < 15 ~ 'shrubby low woodland',
                                                                                                                                    ht.max < 30 ~ 'shrubby medium woodland',
                                                                                                                                    TRUE ~ 'shrubby high woodland'))
                                                                                 )
                                                                       ),
                                                                     TRUE ~ case_when(shrub+sapling < 10 ~
                                                                                        case_when(ht.max < 15 ~ 'open low forest',
                                                                                                  ht.max < 30 ~ 'open medium forest',
                                                                                                  ht.max < 45 ~ 'open high forest',
                                                                                                  ht.max < 60 ~ 'open tall forest',
                                                                                                  TRUE  ~ 'open giant forest'),

                                                                                      TRUE ~ case_when(sapling > shrub ~
                                                                                                         case_when(ht.max < 15 ~ 'advance regen low forest',
                                                                                                                   ht.max < 30 ~ 'advance regen medium forest',
                                                                                                                   ht.max < 45 ~ 'advance regen high forest',
                                                                                                                   ht.max < 60 ~ 'advance regen tall forest',
                                                                                                                   TRUE  ~ 'advance regen giant forest'),
                                                                                                       TRUE ~
                                                                                                         case_when(ht.max < 15 ~ 'shrubby low forest',
                                                                                                                   ht.max < 30 ~ 'shrubby medium forest',
                                                                                                                   ht.max < 45 ~ 'shrubby high forest',
                                                                                                                   ht.max < 60 ~ 'shrubby tall forest',
                                                                                                                   TRUE  ~ 'shrubby giant forest')))

                                                    )
                                   )))
  }else{

    x <- x |> mutate(stratum = case_when(
      is.na(ht.max) | is.na(type) ~ 'excluded',
      ht.max > 5  & type %in% c('shrub/vine', 'tree')  ~ 'tree',
      type %in% c('tree', 'shrub/vine') ~ 'shrub',
      type %in% c('grass/grasslike', 'forb') ~ 'herb',
      !type %in% c('tree', 'shrub/vine', 'herb') ~ 'moss',
      TRUE ~ 'excluded'))

    x.ht <- veg |> group_by(plot) |> summarise(ht.max = max(ht.max))

    x2 <- x |> group_by(plot, stratum) |> summarise(cover = cover.agg(cover))
    x3 <- data.frame(plot = unique(x2$plot), tree = 0, shrub=0, herb=0, moss=0)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'tree')) |> mutate(tree=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'shrub')) |> mutate(shrub=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'herb')) |> mutate(herb=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'moss')) |> mutate(moss=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(x.ht)


    x3 <- x3 |> mutate(structure =
                         case_when(tree+shrub+herb < 10 ~
                                     case_when(moss < 10 ~ 'barren',
                                               TRUE ~ 'mossland'),
                                   TRUE ~ case_when(tree < 10 ~
                                                      case_when(shrub+tree < 25 ~ 'grassland',
                                                                TRUE ~ 'shrubland'),
                                                    TRUE ~ case_when(tree < 25 ~ 'savanna',
                                                                     TRUE~ case_when(tree < 60 ~ 'woodland',
                                                                                     TRUE ~ 'forest'))
                                   )))
  }}

#alternative cover aggregation
get.structure.alt <- function(x, simple = TRUE){
  if(simple == FALSE){
    x <- x |> mutate(stratum = case_when(
      is.na(ht.max) | is.na(type) ~ 'excluded',
      ht.max > 5  & type %in% c('shrub/vine', 'tree')  ~ 'tree',
      type %in% c('tree') ~ 'sapling',
      type %in% c('shrub/vine') ~ 'shrub',
      type %in% c('grass/grasslike') ~ 'grass',
      type %in% c('forb') ~ 'forb',
      !type %in% c('tree', 'shrub/vine', 'forb','grass/grasslike') ~ 'moss',
      TRUE ~ 'excluded'))

    x.ht <- veg |> group_by(plot) |> summarise(ht.max = max(ht.max))

    x2 <- x |> group_by(plot, stratum) |> summarise(cover = cover.agg.stratum(cover))
    x3 <- data.frame(plot = unique(x2$plot), tree = 0, sapling = 0, shrub=0, forb=0, grass=0, moss=0)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'tree')) |> mutate(tree=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'sapling')) |> mutate(sapling=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'shrub')) |> mutate(shrub=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'forb')) |> mutate(forb=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'grass')) |> mutate(grass=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'moss')) |> mutate(moss=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(x.ht)


    x3 <- x3 |> mutate(structure =
                         case_when(tree+shrub+sapling+forb+grass < 10 ~
                                     case_when(moss < 10 ~ 'barren',
                                               TRUE ~ 'mossland'),
                                   TRUE ~ case_when(tree < 10 ~
                                                      case_when(shrub+sapling < 10 ~
                                                                  case_when(grass > forb ~  'grassland',
                                                                            TRUE ~ 'forb meadow'),
                                                                TRUE ~ case_when(tree+shrub+sapling < 75 ~
                                                                                   case_when(sapling > shrub ~ 'woodland regen',
                                                                                             TRUE ~ 'open shrubland'),
                                                                                 TRUE ~ case_when(sapling > shrub ~ 'forest regen',
                                                                                                  TRUE ~ 'shrub thicket')
                                                                )
                                                      ),
                                                    TRUE ~ case_when(tree < 65 ~
                                                                       case_when(shrub+sapling < 10 ~
                                                                                   case_when(ht.max < 15 ~ 'open low woodland',
                                                                                             ht.max < 30 ~ 'open medium woodland',
                                                                                             TRUE ~ 'open high woodland'),
                                                                                 TRUE ~ case_when(sapling > shrub ~
                                                                                                    case_when(shrub+sapling < 75 ~
                                                                                                                case_when(ht.max < 15 ~ 'open regenerating low forest gap',
                                                                                                                          ht.max < 30 ~ 'open regenerating medium forest gap',
                                                                                                                          TRUE ~ 'open regenerating high forest gap'),
                                                                                                              TRUE ~ case_when(ht.max < 15 ~ 'dense regenerating low forest gap',
                                                                                                                               ht.max < 30 ~ 'dense regenerating medium forest gap',
                                                                                                                               TRUE ~ 'dense regenerating high forest gap')),
                                                                                                  TRUE ~ case_when(shrub+sapling < 75 ~
                                                                                                                     case_when(ht.max < 15 ~ 'open shrubby low woodland',
                                                                                                                               ht.max < 30 ~ 'open shrubby medium woodland',
                                                                                                                               TRUE ~ 'open shrubby high woodland'),
                                                                                                                   TRUE ~ case_when(ht.max < 15 ~ 'shrubby low woodland',
                                                                                                                                    ht.max < 30 ~ 'shrubby medium woodland',
                                                                                                                                    TRUE ~ 'shrubby high woodland'))
                                                                                 )
                                                                       ),
                                                                     TRUE ~ case_when(shrub+sapling < 10 ~
                                                                                        case_when(ht.max < 15 ~ 'open low forest',
                                                                                                  ht.max < 30 ~ 'open medium forest',
                                                                                                  ht.max < 45 ~ 'open high forest',
                                                                                                  ht.max < 60 ~ 'open tall forest',
                                                                                                  TRUE  ~ 'open giant forest'),

                                                                                      TRUE ~ case_when(sapling > shrub ~
                                                                                                         case_when(ht.max < 15 ~ 'advance regen low forest',
                                                                                                                   ht.max < 30 ~ 'advance regen medium forest',
                                                                                                                   ht.max < 45 ~ 'advance regen high forest',
                                                                                                                   ht.max < 60 ~ 'advance regen tall forest',
                                                                                                                   TRUE  ~ 'advance regen giant forest'),
                                                                                                       TRUE ~
                                                                                                         case_when(ht.max < 15 ~ 'shrubby low forest',
                                                                                                                   ht.max < 30 ~ 'shrubby medium forest',
                                                                                                                   ht.max < 45 ~ 'shrubby high forest',
                                                                                                                   ht.max < 60 ~ 'shrubby tall forest',
                                                                                                                   TRUE  ~ 'shrubby giant forest')))

                                                    )
                                   )))
  }else{

    x <- x |> mutate(stratum = case_when(
      is.na(ht.max) | is.na(type) ~ 'excluded',
      ht.max > 5  & type %in% c('shrub/vine', 'tree')  ~ 'tree',
      type %in% c('tree', 'shrub/vine') ~ 'shrub',
      type %in% c('grass/grasslike', 'forb') ~ 'herb',
      !type %in% c('tree', 'shrub/vine', 'herb') ~ 'moss',
      TRUE ~ 'excluded'))

    x.ht <- veg |> group_by(plot) |> summarise(ht.max = max(ht.max))

    x2 <- x |> group_by(plot, stratum) |> summarise(cover = cover.agg.stratum(cover))
    x3 <- data.frame(plot = unique(x2$plot), tree = 0, shrub=0, herb=0, moss=0)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'tree')) |> mutate(tree=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'shrub')) |> mutate(shrub=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'herb')) |> mutate(herb=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(subset(x2, stratum %in% 'moss')) |> mutate(moss=ifelse(is.na(cover),0,cover), stratum=NULL, cover=NULL)
    x3 <- x3 |> left_join(x.ht)


    x3 <- x3 |> mutate(structure =
                         case_when(tree+shrub+herb < 10 ~
                                     case_when(moss < 10 ~ 'barren',
                                               TRUE ~ 'mossland'),
                                   TRUE ~ case_when(tree < 10 ~
                                                      case_when(shrub+tree < 25 ~ 'grassland',
                                                                TRUE ~ 'shrubland'),
                                                    TRUE ~ case_when(tree < 25 ~ 'savanna',
                                                                     TRUE~ case_when(tree < 60 ~ 'woodland',
                                                                                     TRUE ~ 'forest'))
                                   )))
  }}
