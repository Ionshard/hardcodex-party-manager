(ns hardcodex-party-manager.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.string :as str]
            [clojure.set :as set]))

(def classes {:bard "Bard"
              :cleric "Cleric"
              :cleric-arcana "Cleric (Arcana)"
              :cleric-death "Cleric (Death)"
              :cleric-knowledge "Cleric (Knowledge)"
              :cleric-life "Cleric (Life)"
              :cleric-light "Cleric (Light)"
              :cleric-nature "Cleric (Nature)"
              :cleric-tempest "Cleric (Tempest)"
              :cleric-trickery "Cleric (Trickery)"
              :cleric-war "Cleric (War)"
              :druid "Druid"
              :druid-arctic "Druid (Arctic)"
              :druid-coast "Druid (Coast)"
              :druid-desert "Druid (Desert)"
              :druid-forest "Druid (Forest)"
              :druid-grassland "Druid (Grassland)"
              :druid-mountain "Druid (Mountain)"
              :druid-swamp "Druid (Swamp)"
              :druid-underdark "Druid (Underdark)"
              :fighter "Fighter (Eldritch Knight)"
              :paladin "Paladin"
              :paladin-ancients "Paladin (Ancients)"
              :paladin-crown "Paladin (Crown)"
              :paladin-devotion "Paladin (Devotion)"
              :paladin-oathbreaker "Paladin (Oathbreaker)"
              :paladin-vengeance "Paladin (Vengeance)"
              :ranger "Ranger"
              :rogue "Rogue (Arcane Trickster)"
              :sorcerer "Sorcerer"
              :warlock "Warlock"
              :warlock-archfey "Warlock (Archfey)"
              :warlock-fiend "Warlock (Fiend)"
              :warlock-great-old-one "Warlock (Great Old One)"
              :warlock-undying "Warlock (Undying)"
              :wizard "Wizard"})

(def players
  [{:name "William A McEwen"
    :classes [:ranger]
    :spells #{"Hail of Thorns" "Ensnaring Strike" "Alarm"}}
   {:name "Mr Pibb"
    :classes [:rogue]
    :spells #{"Mage Hand" "Minor Illusion" "Prestidigitation" "Disguise Self" "Thunderwave" "Silent Image"}}
   {:name "Reginald Bigsby"
    :classes [:bard]
    :spells #{"Vicious Mockery" "Minor Illusion" "Cure Wounds" "Sleep" "Detect Magic" "Unseen Servant" "Disguise Self" "Shatter"}}
   {:name "Sereh-Na"
    :classes [:sorcerer]
    :spells #{"Fire Bolt" "Mage Hand" "Message" "Shocking Grasp" "Feather Fall" "Shield" "Sleep" "Detect Thoughts" "Invisibility" "Misty Step"}}
   {:name "Razul"
    :classes [:warlock :warlock-fiend]
    :spells #{"Eldritch Blast" "Hex" "Detect Magic"}}])

(defn spell-book
  [filename]
  (xml/parse (io/file (io/resource filename))))

(defn spell->name
  [spell]
  (->> spell
       :content
       (filter #(= :name (:tag %)))
       first
       :content
       first))

(defn class-intersection
  [player-classes spell-classes]
  (->> spell-classes
       first
       (#(str/split % #", "))
       set
       (set/intersection player-classes)
       (str/join ", ")
       (vector)))

(defn spell-filter-classes
  [classes spell]
  (update spell
          :content
          #(map (fn [tag-map]
                  (if (= :classes (:tag tag-map))
                    (update tag-map :content (partial class-intersection classes))
                    tag-map))
                %)))

(defn player->classes
  [player]
  (->> player
       :classes
       (map classes)
       set))

(defn filter-spells
  [spell-list classes spells]
  (->> spells
       (filter (fn [spell] (spell-list (spell->name spell))))
       (mapv (partial spell-filter-classes classes))))

(defn player-spell-book
  [player spell-book]
  (update spell-book :content (partial filter-spells (:spells player) (player->classes player))))

(defn party-spell-list
  [players]
  (apply set/union (map :spells players)))

(defn party-class-list
  [players]
  (apply set/union (map player->classes players)))

(defn party-spell-book
  [players spell-book]
  (update spell-book :content (partial filter-spells (party-spell-list players) (party-class-list players))))

(defn write-book
  ([book]
   (write-book "output" book))
  ([name book]
   (spit (str "resources/" name ".xml") (with-out-str (xml/emit-element book)))))

(defn make-player-book
  [player]
  (->> "PHB.xml"
       spell-book
       (player-spell-book player)
       (write-book (:name player))))

(defn make-party-book
  [players]
  (->> "PHB.xml"
       spell-book
       (party-spell-book players)
       (write-book "Party")))

(defn build-spell-books
  []
  (do
    (doall (map make-player-book players))
    (make-party-book players)))

(defn -main
  [& args]
  (println "Writing Spell Books")
  (build-spell-books)
  (println "Finished. Check resources folder"))
