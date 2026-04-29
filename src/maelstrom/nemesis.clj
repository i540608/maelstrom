(ns maelstrom.nemesis
  "Fault injection"
  (:require [clojure.tools.logging :refer [info warn]]
            [jepsen [generator :as gen]
                    [nemesis :as n]
                    [util :refer [pprint-str]]]
            [jepsen.nemesis.combined :as nc]
            [maelstrom.nemesis.chaos-mesh :as cm]
            [slingshot.slingshot :refer [try+ throw+]]))

(defn package
  "A full nemesis package. Options are those for
  jepsen.nemesis.combined/nemesis-package."
  [opts]
  (nc/compose-packages
    (filterv some?
      [(nc/partition-package opts)
       (nc/db-package opts)
       (cm/package opts)])))
