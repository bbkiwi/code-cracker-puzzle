(ns code-cracker-puzzle.webinterface
  (:use (compojure handler
                 [core :only (GET POST defroutes)]))
  (:require compojure.route
    [ring.adapter.jetty :as jetty]
    [ring.util.response :as response]
    [net.cgrand.enlive-html :as enlive]
    [clojure.pprint]
    [cheshire.core :as cheshire]
    [clojure.repl :refer [doc]]
    [code-cracker-puzzle.data-assembly :refer :all]))

(def datab (atom {}))
;enlive sets up templates and routines to create dynamic content
;here gsscript is the id for a greensock script with parameters
; give by data-* attribute which can now be changed dynamically
; with enlive/set-attr
; alternatively can inject script body with enlive/content

; defines function that will modify the tokens in public/index.html and
; produce new tokens that when concatenated will be the generated web page
; will clone BoardCells and fill with code cracker data for puzzle specified
;   localhost:8080/codecracker?cc=21
(enlive/deftemplate
  ccpage
  (enlive/xml-resource "public/index.html")
  [request] ; parameter list as many as like
  [:#ccBoardCells :div] (let [cells (range 169)
                                 ccnum (Integer. (:cc (:params request))) ;(:cc (:params request)) is java.lang.String
                                 cc (get-cc ccnum)
                                 codes (flatten (:rows cc))
                                 letmap (:encodemap cc)]
                             (enlive/clone-for [n cells]
                                               (comp
                                                 (enlive/content (format "%s" (clojure.string/upper-case (get letmap (nth codes n) (nth codes n)))))
                                                 (enlive/set-attr :data-view (nth codes n))
                                                 (enlive/set-attr :id (str "Cell" (quot n 13) "_" (rem n 13)))
                                                 (enlive/set-attr :style (if (zero? (nth codes n)) "background:black"))))))


;set up routing for use in compojure in order of priority
; e.g. /getdata.txt will use the file if in resources rather than the
;  GET associated with /getdata.txt
(defroutes app* ;name convention lower level primative func
           ;where to find static resources which will be served immediately as
           ; these resources are specified first
           (compojure.route/resources "/") ; defaults to resources/public
           ;(compojure.route/resources "/" {:root "public"}) ; resource/public

           ;(GET "/getdata.txt" request (str @datab)) will be ignored the file is in resources
           ; using cheshire to convert to json for all returned data
           (GET "/getdata" request (cheshire/generate-string @datab))
           (POST "/savedata" request
                 (let [data (-> request :params)]
                   #_(clojure.pprint/pprint request)
                   ; convert to json so data will be accepted
                   (cheshire/generate-string (swap! datab merge data))))
           (GET "/savedata" request
                (let [data (-> request :params)]
                  #_(clojure.pprint/pprint request)
                  (cheshire/generate-string (swap! datab merge data))))
           (GET "/codecracker" request (ccpage request))
           (GET "/showrequest" request {:status 200 :body (with-out-str (clojure.pprint/pprint request))}))

; apply middleware compojure.handler/site which
; extracts all type of useful info nicely
; bundles of lots of middleware parse input and output
; converts to keys - great for frontend apps
(def app (compojure.handler/site app*))

(defonce server (jetty/run-jetty #'app {:port 8080 :join? false}))







