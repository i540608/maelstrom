(ns maelstrom.nemesis.chaos-mesh
  "Chaos Mesh nemesis package — injects real Kubernetes infrastructure faults
  during Maelstrom tests by calling the Chaos Mesh REST API."
  (:require [cheshire.core :as json]
            [clojure.tools.logging :refer [info warn]]
            [jepsen [generator :as gen]
                    [nemesis :as n]]
            [org.httpkit.client :as http]))

(defn- request-opts [body token]
  (cond-> {:headers {"Content-Type" "application/json"}
           :body    (json/generate-string body)
           :timeout 5000}
    token (assoc-in [:headers "Authorization"] (str "Bearer " token))))

(defn- http-post! [endpoint path body token]
  @(http/post (str endpoint path) (request-opts body token)))

(defn- http-delete! [endpoint path token]
  @(http/delete (str endpoint path)
                (cond-> {:timeout 5000}
                  token (assoc-in [:headers "Authorization"] (str "Bearer " token)))))

(defn- experiment-body [chaos-type namespace selector]
  (let [ts   (System/currentTimeMillis)
        meta {:name (str "maelstrom-" (name chaos-type) "-" ts) :namespace namespace}
        sel  {:namespaces [namespace] :labelSelectors selector}]
    (case chaos-type
      :network-partition
      {:apiVersion "chaos-mesh.org/v1alpha1" :kind "NetworkChaos"
       :metadata meta
       :spec {:action "partition" :mode "all" :direction "both"
              :selector sel :targetSelector sel}}

      :network-loss
      {:apiVersion "chaos-mesh.org/v1alpha1" :kind "NetworkChaos"
       :metadata meta
       :spec {:action "loss" :mode "all" :selector sel
              :loss {:loss "50" :correlation "25"}}}

      :network-latency
      {:apiVersion "chaos-mesh.org/v1alpha1" :kind "NetworkChaos"
       :metadata meta
       :spec {:action "delay" :mode "all" :selector sel
              :delay {:latency "200ms" :jitter "50ms" :correlation "25"}}}

      :pod-kill
      {:apiVersion "chaos-mesh.org/v1alpha1" :kind "PodChaos"
       :metadata meta
       :spec {:action "pod-kill" :mode "one" :selector sel :gracePeriod 0}})))

(def fault-types [:network-partition :network-loss :network-latency :pod-kill])

(defn chaos-mesh-nemesis [{:keys [endpoint namespace selector token]}]
  (let [uid (atom nil)]
    (reify
      n/Nemesis
      (setup! [this _test] this)

      (invoke! [this _test {:keys [f value] :as op}]
        (case f
          :start-chaos
          (let [body (experiment-body value namespace selector)
                resp (http-post! endpoint "/api/experiments" body token)]
            (if (= 200 (:status resp))
              (let [experiment-uid (:uid (json/parse-string (:body resp) true))]
                (reset! uid experiment-uid)
                (info "Chaos Mesh started" value "uid:" experiment-uid)
                (assoc op :value {:type value :uid experiment-uid}))
              (do (warn "Chaos Mesh start failed:" (:status resp) (:body resp))
                  (assoc op :value {:error (:body resp)}))))

          :stop-chaos
          (if-let [u @uid]
            (let [resp (http-delete! endpoint (str "/api/experiments/" u) token)]
              (reset! uid nil)
              (info "Chaos Mesh stopped uid:" u)
              (assoc op :value {:uid u :status (:status resp)}))
            (assoc op :value :no-experiment-running))))

      (teardown! [this _test]
        (when-let [u @uid]
          (warn "Chaos Mesh teardown: cleaning up leftover experiment" u)
          (http-delete! endpoint (str "/api/experiments/" u) token)
          (reset! uid nil))
        this)

      n/Reflection
      (fs [_] #{:start-chaos :stop-chaos}))))

(defn package
  "Returns a Chaos Mesh nemesis package, or nil when :chaos-mesh is not in
  faults or endpoint is not configured. Callers must filter nils before
  passing to nc/compose-packages."
  [{:keys [chaos-mesh faults interval] :as _opts}]
  (when (and (contains? (or faults #{}) :chaos-mesh)
             (:endpoint chaos-mesh))
    (let [endpoint (:endpoint chaos-mesh)
          ns       (:namespace chaos-mesh "default")
          selector (:selector chaos-mesh {})
          token    (:token chaos-mesh)
          start    (fn [_ _] {:type :info :f :start-chaos :value (rand-nth fault-types)})
          stop-op  {:type :info :f :stop-chaos :value nil}
          gen      (->> (gen/flip-flop start (gen/repeat stop-op))
                        (gen/stagger (or interval 30)))]
      {:generator       gen
       :final-generator stop-op
       :nemesis         (chaos-mesh-nemesis {:endpoint  endpoint
                                             :namespace ns
                                             :selector  selector
                                             :token     token})
       :perf            #{{:name  "chaos-mesh"
                           :start #{:start-chaos}
                           :stop  #{:stop-chaos}
                           :color "#FF4444"}}})))
