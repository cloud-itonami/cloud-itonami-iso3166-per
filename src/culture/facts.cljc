(ns culture.facts
  "Country-level regional-culture catalog for Peru (PER) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"PER"
   [{:culture/id "per.dish.ceviche"
     :culture/name "Ceviche"
     :culture/country "PER"
     :culture/kind :dish
     :culture/summary "Considered the national dish of Peru and recognized by UNESCO as an expression of Peruvian traditional cuisine and Intangible Cultural Heritage of Humanity; the modern version originated during colonial times in present-day Peru."
     :culture/url "https://en.wikipedia.org/wiki/Ceviche"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.dish.lomo-saltado"
     :culture/name "Lomo Saltado"
     :culture/country "PER"
     :culture/kind :dish
     :culture/summary "Traditional Peruvian dish, a stir-fry that typically combines marinated strips of sirloin with onions, tomatoes and French fries."
     :culture/url "https://en.wikipedia.org/wiki/Lomo_saltado"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.dish.cuy"
     :culture/name "Cuy"
     :culture/country "PER"
     :culture/kind :dish
     :culture/summary "Guinea pig, part of Peruvian cuisine, often served whole or used as filling for foods such as tacos and ravioli."
     :culture/url "https://en.wikipedia.org/wiki/Peruvian_cuisine"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.beverage.pisco"
     :culture/name "Pisco"
     :culture/country "PER"
     :culture/kind :beverage
     :culture/summary "Colorless or yellowish-to-amber-colored spirit produced in winemaking regions of Peru and Chile; historians generally believe pisco originated in Peru, though both countries claim it as their national drink."
     :culture/url "https://en.wikipedia.org/wiki/Pisco"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.product.alpaca-fiber"
     :culture/name "Alpaca fiber"
     :culture/country "PER"
     :culture/kind :product
     :culture/summary "Fiber from alpacas, which were domesticated from vicunas over 5,000 years ago by ancient tribes of the Andean highlands of Ecuador, Peru, Chile, Bolivia and northwest Argentina."
     :culture/url "https://en.wikipedia.org/wiki/Alpaca_fiber"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.craft.retablo"
     :culture/name "Retablo"
     :culture/country "PER"
     :culture/kind :craft
     :culture/summary "Peruvian portable altarpiece art form; underwent a 1940s resurgence in Peru led by artist Joaquin Lopez Antay with support from the Peruvian indigenista movement, becoming a family heirloom symbolizing protection, fertility and healing."
     :culture/url "https://en.wikipedia.org/wiki/Retablo"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.festival.inti-raymi"
     :culture/name "Inti Raymi"
     :culture/country "PER"
     :culture/kind :festival
     :culture/summary "Celebrated on June 24, the most important festival of the Inca Empire, historically held in Haukaypata, the main square of Cusco, Peru."
     :culture/url "https://en.wikipedia.org/wiki/Inti_Raymi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "per.heritage.machu-picchu"
     :culture/name "Machu Picchu"
     :culture/country "PER"
     :culture/kind :heritage
     :culture/summary "Inca citadel in Peru designated a UNESCO World Heritage Site in 1983, described by UNESCO as 'a masterpiece of art, urbanism, architecture and engineering'."
     :culture/url "https://en.wikipedia.org/wiki/Machu_Picchu"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-per culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "PER"))
                 " PER entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
