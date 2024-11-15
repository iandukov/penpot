;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

#_:clj-kondo/ignore
(ns app.common.uuid
  (:refer-clojure :exclude [next uuid zero? short])
  (:require
   #?(:clj [clojure.core :as c])
   #?(:cljs [app.common.uuid-impl :as impl])
   #?(:cljs [cljs.core :as c])
   #?(:cljs [goog.array :as garray])
   [app.common.data.macros :as dm])
  #?(:clj (:import
           app.common.UUIDv8
           java.util.UUID
           java.nio.ByteBuffer)))

#?(:cljs
   (defprotocol IUUIDOps
     (get-u32 [_])))

#?(:cljs
   (deftype CUUID [uuid ^:mutable __u32_buffer ^:mutable __hash]
     cljs.core/IUUID

     Object
     (toString [_] uuid)
     (equiv [this other]
       (-equiv this other))

     IEquiv
     (-equiv [_ other]
       (and (implements? IUUID other) (identical? uuid (.-uuid ^CUUID other))))

     IPrintWithWriter
     (-pr-writer [_ writer _]
       (-write writer (str "#uuid \"" uuid "\"")))

     IUUIDOps
     (get-u32 [_]
       (when (nil? __u32_buffer)
         (set! __u32_buffer (impl/parse-u32 uuid)))
       __u32_buffer)

     IHash
     (-hash [this]
       (when (nil? __hash)
         (set! __hash (hash uuid)))
       __hash)

     IComparable
     (-compare [this other]
       (if (instance? CUUID other)
         (garray/defaultCompare uuid (.-uuid other))
         (throw (js/Error. (str "Cannot compare " this " to " other)))))))


#?(:cljs
   (def buffer-sym (js/Symbol "buffer")))

#?(:cljs
   (extend-type UUID
     IUUIDOps
     (get-u32 [this]
       (let [buffer (unchecked-get this "__buffer")]
         ;; (js/console.log "get-u32" (some? buffer))
         (if (nil? buffer)
           (let [buffer (impl/parse-u32 (.-uuid ^UUID this))]
             (unchecked-set this "__buffer" buffer)
             buffer)
           buffer)))))

(def ^:private uuid-re
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(defn uuid
  "Parse string uuid representation into proper UUID instance."
  [s]
  #?(:clj (UUID/fromString s)
     :cljs (c/uuid s) #_(CUUID. s nil nil)))

(defn parse
  [s]
  (prn "uuid/parse" s)
  (if (string? s)
    (some->> (re-matches uuid-re s) uuid)
    nil))

(defn next
  []
  #?(:clj (UUIDv8/create)
     :cljs (uuid (impl/v8))))

(defn random
  "Alias for clj-uuid/v4."
  []
  #?(:clj (UUID/randomUUID)
     :cljs (uuid (impl/v4))))

(defn custom
  ([a] #?(:clj (UUID. 0 a) :cljs (uuid (impl/custom 0 a))))
  ([b a] #?(:clj (UUID. b a) :cljs (uuid (impl/custom b a)))))

(def zero (uuid "00000000-0000-0000-0000-000000000000"))

(defn zero?
  [v]
  (= zero v))

#?(:clj
   (defn get-word-high
     [id]
     (.getMostSignificantBits ^UUID id)))

#?(:clj
   (defn get-word-low
     [id]
     (.getLeastSignificantBits ^UUID id)))

#?(:clj
   (defn get-bytes
     [^UUID o]
     (let [buf (ByteBuffer/allocate 16)]
       (.putLong buf (.getMostSignificantBits o))
       (.putLong buf (.getLeastSignificantBits o))
       (.array buf))))

#?(:clj
   (defn from-bytes
     [^bytes o]
     (let [buf (ByteBuffer/wrap o)]
       (UUID. ^long (.getLong buf)
              ^long (.getLong buf)))))

#?(:cljs
   (defn uuid->short-id
     "Return a shorter string of a safe subset of bytes of an uuid encoded
     with base62. It is only safe to use with uuid v4 and penpot custom v8"
     [id]
     (impl/short-v8 (dm/str id))))

#?(:cljs
   (defn uuid->u32
     [id]
     (impl/get-u32 (dm/str id))))

#?(:clj
   (defn hash-int
     [id]
     (let [a (.getMostSignificantBits ^UUID id)
           b (.getLeastSignificantBits ^UUID id)]
       (+ (clojure.lang.Murmur3/hashLong a)
          (clojure.lang.Murmur3/hashLong b)))))

