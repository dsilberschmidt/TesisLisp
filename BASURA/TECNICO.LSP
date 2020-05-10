
(de monitor-tecnico (eco &aux clausula flag)
   (if flag-monitor
      (for (i 1 (length monitor-c))
           (setq clausula (nth monitor-c i))
           (setq flag    (vref monitor-f i))
           (prin0 '+)
           (console-in)
           (prin2 "Padre:      ")
           (pp-clausula (fourth clausula))
           (prin2 "  Madre:      ")
           (pp-clausula (fifth clausula))
           (prin2 (selectq flag ( 'N "  Hijo:       " )
                                ( 'T "  Tautologia: " )
                                ( 'F "  Fusion:     " )
                                ( t  "  Subsumida:  " )
                  )
           )
           (if (= flag 'F)
               (pp-clausula (fusionar clausula))
               (pp-clausula clausula)
           )
           (if (pertenece flag '(N T F))
               nil
               (prin2 "  Subsumida por:")
               (pp-clausula flag)
           )
      )
   )
   eco
)

(de monitor-clausulas (clausulas)
   (setq monitor-f (newvector (length clausulas) 'N))
   (setq monitor-c clausulas)
)

(de monitor-flags (clausula flag &aux (pos (posicion clausula monitor-c)))
   (if pos
       (setq monitor-f (store monitor-f pos flag))
   )
   'true
)
