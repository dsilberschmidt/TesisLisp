
(setq presidente2
     '(  ( (presidente alfonsin) or (presidente bustelo) )
         ( (presidente alfonsin) or (presidente cafiero) )
         ( (presidente alfonsin)
                -> (not ((presidente bustelo) or (presidente cafiero)))
         )
         ( (presidente bustelo)
                -> (not ((presidente alfonsin) or (presidente cafiero)))
         )
         ( (presidente cafiero)
                -> (not ((presidente alfonsin) or (presidente bustelo)))
         )
      )
)
