(ns miyazapi.trakt
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.spec.alpha :as s]))

(s/def :trakt/period #{:daily :weekly :monthly :yearly :all})

(def default-redirect-uri "urn:ietf:wg:oauth:2.0:oob")

(def endpoints
  {:token "https://api.trakt.tv/oauth/token"
   :device/code "https://api.trakt.tv/oauth/device/code"})

(defn parse-body [resp] (json/read-str (:body resp) :key-fn keyword))

(defn refresh-token
  "Uses an authorization code to generate a refresh token.
  TODO: refresh token not working, fix. You can use an auth-code to get an
  access-token though (valid for 3 months)"
  [{:keys [api-key auth-code redirect-uri client-secret]}]
  (-> (client/post (:token endpoints)
                   {:form-params {"grant_type" "authorization_code"
                                  "client_id" api-key
                                  "client_secret" client-secret
                                  "redirect_uri" (or redirect-uri default-redirect-uri)
                                  "code" auth-code}
                    :headers     {"Content-type" "application/json"
                                  "trakt-api-key" api-key
                                  "trakt-api-version" "2"
                                  ;;"Authorization" (str "Bearer" )
                                  }
                    :accept      :json})
      parse-body
      :refresh_token))

(defn access-token
  [{:keys [api-key refresh-token redirect-uri client-secret]}]
  (-> (client/post (:token endpoints)
                   {:form-params {"refresh_token" refresh-token
                                  "client_id" api-key
                                  "client_secret" client-secret
                                  "redirect_uri" (or redirect-uri default-redirect-uri)
                                  "grant_type" "refresh_token"}
                    :headers     {"Content-type" "application/json"
                                  "trakt-api-key" api-key
                                  "trakt-api-version" "2"
                                  ;;"Authorization" (str "Bearer" )
                                  }
                    :accept      :json})
      parse-body
      :access_token))

(defn single-page [{:keys [trakt/client-id endpoint access-token]}]
  (fn [idx]
    (client/get endpoint
                {:query-params {:page idx}
                 :headers      {"trakt-api-key" client-id
                                "Content-type" "application/json"
                                "trakt-api-version" "2"
                                "Authorization" (str "Bearer " access-token)}})))

(defmulti order :order)

(defmethod order :watchers [_]
  {:endpoint "https://api.trakt.tv/movies/trending"
   :paginated true})

(defmethod order :trending [_]
  {:endpoint "https://api.trakt.tv/movies/trending"
   :paginated true})

(defmethod order :popular [_]
  {:endpoint "https://api.trakt.tv/movies/popular"
   :paginated true})

(defmethod order :recommended [{:keys [period]}]
  {:endpoint (str "https://api.trakt.tv/movies/recommended/" (or period "all"))
   :paginated true})

(defmethod order :played [{:keys [period]}]
  {:endpoint (str "https://api.trakt.tv/movies/played/" (or period "all"))
   :paginated true})

(defmethod order :watched [{:keys [period]}]
  {:endpoint (str "https://api.trakt.tv/movies/watched/" (or period "all"))
   :paginated true})

(defmethod order :collected [{:keys [period]}]
  {:endpoint (str "https://api.trakt.tv/movies/collected/" (or period "all"))
   :paginated true})

(defmethod order :anticipated [{:keys [period]}]
  {:endpoint (str "https://api.trakt.tv/movies/anticipated/" (or period "all"))
   :paginated true})

(defn recommended-rate [{:stats/keys [recommended watchers]}]
  (/ recommended watchers))

(defn movie-stats [movie-id {:trakt/keys [client-id]}]
  (let [stats (-> (client/get (str "https://api.trakt.tv/movies/" movie-id "/stats")
                              {:query-params {:extended "full"}
                               :headers {"trakt-api-key" client-id
                                         "Content-type" "application/json"
                                         "trakt-api-version" "2"}})
                  parse-body)]
    (update-keys stats (fn [k] (keyword "stats" (name k))))))

(defn enrich-movie [m cfg]
  (let [details {:title      (-> m :movie :title)
                 :year       (-> m :movie :year)
                 :trakt/id   (-> m :movie :ids :trakt)
                 :trakt/slug (-> m :movie :ids :slug)
                 :imdb/id    (-> m :movie :ids :imdb)
                 :tmdb/id    (-> m :movie :ids :tmdb)}
        stats   (movie-stats (:trakt/id details) cfg)]
    (merge (dissoc m :movie) details stats)))

(defn movies
  "Lists movies sorted by `miyazapi.trakt/order`; continues to next page, if any."
  [{:keys [trakt/client-id] :as params}]
  ;; TODO: smarter pagination - use response header
  (let [{:keys [endpoint paginated]} (order params)
        params (assoc params :endpoint endpoint)]
    (if paginated
      (mapcat (comp parse-body (single-page params) inc) (range))
      (-> (client/get endpoint
                      {:query-params {:extended "full"}
                       :headers {"trakt-api-key" client-id
                                 "Content-type" "application/json"
                                 "trakt-api-version" "2"}})
          parse-body))))

(defn- watched-movies [{:keys [trakt/client-id user]}]
  (-> (client/get (str "https://api.trakt.tv/users/" user "/watched/movies")
                  {:headers {"trakt-api-key" client-id
                             "Content-type" "application/json"
                             "trakt-api-version" "2"}})
      parse-body))

(def trakt-id (comp :trakt :ids))

(defn unwatched [cfg]
  (let [watched-ids (->> (watched-movies cfg) (map (comp trakt-id :movie)) set)
        movie-list  (movies (assoc cfg :top (+ 100 (count watched-ids))))]
    (remove #(watched-ids (trakt-id %)) movie-list)))

(defn user-recommendations [{:keys [user] :as cfg}]
  (let [watched-ids (if user
                      (->> (watched-movies cfg) (map (comp trakt-id :movie)) set)
                      #{})
        watched?    (fn [m] (watched-ids (:trakt/id m)))
        recommendation-pool (->> (assoc cfg :order :recommended)
                                 movies
                                 (take-while #(> (:user_count %) 100))
                                 (map #(enrich-movie % cfg)))]
    (->> recommendation-pool
         (remove watched?)
         (sort-by recommended-rate)
         reverse)))
