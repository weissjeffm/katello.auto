(ns webdriver
  (:use [clj-webdriver.element :only [element-like?]]
   :require [clj-webdriver.taxi :as taxi]))

(declare my-driver)

(defprotocol SeleniumLocatable
  (sel-locator [x]))

(defn locator-args
  "If any args are keywords, look them up via
SeleniumLocatable protocol (which should return a selenium String
locator). Returns the args list with those Strings in place of the
keywords."
  [& args]
  (for [arg args]
    (if (keyword? arg) 
      (or (sel-locator arg)
          (throw (IllegalArgumentException.
                  (str "Locator " arg " not found in UI mapping."))))
      arg)))

(defn locator-finder-fn 
  ([q] (locator-finder-fn taxi/*driver* q))
  ([driver q]
     (println (str "Q: " q))
     (if (keyword? q)
                (taxi/xpath-finder (first (locator-args q)))
                (taxi/xpath-finder q))))

(def ^{:doc "A function to format locators out of a template. Example:
              ((template \"//div[.='%s']\") \"foo\") =>
                \"//div[.='foo']\""}
  template (partial partial format))

(defmacro template-fns
  "Expands into a function definition for each entry in m, where the
  key is a symbol for the function, and the value is a format string.
  When called, the function will format the its arguments with the
  format string."
  [m]
  `(do ~@(for [[sym fmt] m]
           `(def ~sym 
              (template ~fmt)))))

(defn new-local-driver
  "returns a local selenium webdriver instance.
Default browser-spec: firefox"
  ([] (new-local-driver {:browser :firefox}))
  ([browser-spec] (taxi/new-driver browser-spec)))

(defn connect "Create a new selenium instance." [driver url]
  ([url] (connect (new-local-driver) url))
  ([driver url] )
  (def ^:dynamic my-driver driver)
  (taxi/set-driver! my-driver)
  (taxi/set-finder! locator-finder-fn))
