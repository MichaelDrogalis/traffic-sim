(ns traffic-lights.core)

(def light {:light/ident :standard
            :light/states [:red :yellow :green]
            :light/init [:red]})

(def schedule
  '{:light.schedule/ident :shamrock-schedule
    :light-set/schedule [{:states {?w [:green] ?y [:green]} :duration 8000}
                         {:states {?w [:yellow] ?y [:yellow]} :duration 1000}
                         {:states {?w [:red] ?y [:red]} :duration 1000}
                         {:states {?x [:green] ?z [:green]} :duration 8000}
                         {:states {?x [:yellow] ?z [:yellow]} :duration 1000}
                         {:states {?x [:red] ?z [:red]} :duration 1000}]})
 
(def light-set
  '{:light-set/ident :standard-light-set
    :light-set/schedule :shamrock-schedule
    :light-set/substitute {?w :standard ?x :standard ?y :standard ?z :standard}})

(def light-catalog {:standard light})

(defn construct-light [light-spec]
  (apply merge
         (map (fn [[face light-ident]]
                {face (:light/init (light-catalog light-ident))})
              (:light-set/substitute light-spec))))

(reductions
 (fn [lights {:keys [states]}]
   (merge lights states))
 (construct-light light-set) (:light-set/schedule schedule))

