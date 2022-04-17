(ns miyazapi.repl
  (:require [clojure.tools.deps.alpha.repl :refer [add-libs find-lib]]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [miyazapi.trakt :as trakt]
            [clojure.edn :as edn]))

(comment
  ;; add-libs stuff
  (add-libs '{clj-http/clj-http {:mvn/version "3.12.3"}})

  (add-libs '{io.github.dakrone/clj-http {:git/sha "7aa6d02ad83dff9af6217f39e517cde2ded73a25"}})
  (add-libs '{io.github.clojure/tools.deps.alpha {:git/sha "e4fb92eef724fa39e29b39cc2b1a850567d490dd"}})
  ,)

(defn init-config []
  (let [cfg {:trakt/user          "..." ;; your trakt.tv user name (this is used to get your watched items)
             :trakt/client-id     "..." ;; trakt.tv API key (create a trakt.tv app to get one)
             :trakt/client-secret "..." ;; trakt.tv API key (not used for now)
             :trakt/refresh-token "..." ;; trakt.tv refresh token (not used for now - you can generate this with an authorization code that you'll get from trakt.tv)
             }
        config-home (or (System/getenv "XDG_CONFIG_HOME") (str (System/getProperty "user.home") "/.config"))
        path (str config-home "/miyazapi/config.edn")]
    (spit path (pr-str cfg))))

(defn load-config
  "Load your config"
  [& path]
  (let [config-home (or (System/getenv "XDG_CONFIG_HOME") (str (System/getProperty "user.home") "/.config"))
        path (or path (str config-home "/miyazapi/config.edn"))]
    (-> path slurp edn/read-string)))

(comment

  (def cfg (load-config))

  (def movies (trakt/watched-movies cfg))
  (def movies (trakt/movies (assoc cfg :top 1000)))

  (->> (trakt/unwatched (assoc cfg :order :popular))
       (map #(str (:title %) " (" (:year %) ")"))
       (drop 100)
       (take 100))

  (->> (trakt/movies (assoc cfg :order :popular))
       (take 10))

  (let [client-id (:client-id cfg)
        idx 2]
    (-> (client/get "https://api.trakt.tv/movies/popular"
                {:query-params {:page idx}
                 :headers      {"trakt-api-key" client-id
                                "Content-type" "application/json"
                                "trakt-api-version" "2"
                                "Authorization" (str "Bearer " access-token)}})
        :headers))

  (->> (assoc cfg :order :recommended)
       trakt/movies
       (take-while #(> (:user_count %) 50))
       first)

  ;;=> {:user_count 1228, :movie {:title "Dune", :year 2021, :ids {:trakt 287071, :slug "dune-2021", :imdb "tt1160419", :tmdb 438631}}}

  (trakt/movie-stats 287071 cfg)
  ;;=> {:watchers 232690, :plays 310254, :collectors 60474, :comments 203, :lists 35585, :votes 18367, :recommended 1228}

  (def recommendations
    (trakt/user-recommendations cfg))

  (->> recommendations
       (take 10)
       (map #(assoc % :stats/recommended-rate (float (trakt/recommended-rate %))))
       (map #(select-keys % [:title :year :trakt/slug :stats/watchers :stats/recommended-rate]))
       clojure.pprint/print-table)

  ,)
