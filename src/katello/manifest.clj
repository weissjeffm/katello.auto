(ns katello.manifest
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [katello.tasks :refer [unique-format tmpfile]])
  (:import [java.util.zip ZipEntry ZipFile ZipOutputStream ZipInputStream]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [org.bouncycastle.openssl PEMParser]))

(defn zis [f]
  (-> f java.io.FileInputStream. ZipInputStream.))

(defn new-zip-inputstream [old-zis]
  (let [baos (ByteArrayOutputStream.)]
    (io/copy old-zis baos)
    (ZipInputStream. (ByteArrayInputStream. (.toByteArray baos)))))

(defn update-in-nested-zip
  "Takes a zip inputstream and returns an inputstream with all the
  files in replacements replaced with their contents."
  [zis path content]
  (if (empty? path) zis
      (with-open [baos (ByteArrayOutputStream.)
                  dzos (ZipOutputStream. baos)]
        (loop [this-entry (.getNextEntry zis)]
          (when this-entry
            (.putNextEntry dzos (ZipEntry. (.getName this-entry)))
            (if (= (first path) (.getName this-entry))
              (if (= (count path) 1)
                (do (println "overwriting " (.getName this-entry))
                    (io/copy content dzos))

                ;;nested zip
                (with-open [this-entry-is (new-zip-inputstream zis)]
                  (io/copy (update-in-nested-zip this-entry-is (pop path) content)
                           dzos)))
              ;;non-matching file
              (io/copy zis dzos))
            
            (recur (.getNextEntry zis))))
        (ByteArrayInputStream. (.toByteArray baos)))))

(defn read-bytes [o]
  (with-open [bo (java.io.ByteArrayOutputStream.)]
    (io/copy o bo)
    (.toByteArray bo)))

(defn get-zip-bytes
  "Gets the bytes of consumer_export.zip (to be used in signature)"
  [zis]
  (loop [this-entry (.getNextEntry zis)]
    (cond (not this-entry) nil

          (= (.getName this-entry) "consumer_export.zip")
          (read-bytes zis)

          :else (recur (.getNextEntry zis)))))

(defn new-tmp-loc
  "Returns a unique file path in the system tmp dir, ending in .zip"
  []
  (-> "manifest-%s.zip" unique-format tmpfile))

(defn sign [manifest-path key-url]
  (let [r (io/reader (java.net.URL. key-url))
        sig (Signature/getInstance "SHA256withRSA")]
    (.initSign sig (-> r PEMReader. .readObject))
    (.update sig (-> manifest-path zis get-zip-bytes ))
    (.sign sig)))

(defn clone
  "Takes a manifest file location, copies it,and updates it internally
   so that it will be accepted by katello as a new manifest. Also
   specify file output location for the clone (full path string), and
   a url for the key that should be used to sign the manifest (this
   key should be present in the /etc/candlepin/certs/upstream/ dir"
  [source-path dest-path key-url]
  (let [tmp (new-tmp-loc)]
    (with-open [bais (update-in-nested-zip (zis source-path) 
                                           (list "consumer_export.zip" "export/consumer.json")
                                           (->> (java.util.UUID/randomUUID) .toString (assoc default-consumer-info :uuid) json/json-str))]
      (io/copy bais (java.io.File. tmp)))
    (with-open [bais (update-in-nested-zip (zis tmp), (list "signature"), (sign source-path key-url))]
      (io/copy bais (java.io.File. dest-path)))))


