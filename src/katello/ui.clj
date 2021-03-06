(ns katello.ui
  (:require [com.redhat.qe.auto.selenium.selenium :as sel]
            [katello.rest :as rest])
  (:refer-clojure :exclude [read]))

;; Protocols

(defprotocol CRUD
  "Create/read/update/delete operations on katello entities via the UI"
  (create [x] "Create an entity in the UI")
  (read [x] "Get details on an entity from the UI")
  (update* [x new-x] "Change an existing entity in UI, from x to new-x")
  (delete [x] "Delete an existing entity in the UI"))

(defrecord Notification [validationErrors notices level])

;; because protocols don't support varargs
(defn update [x f & args]
  (let [updated (apply f x args)]
    (update* x updated)
    updated))

;;convenience
(defn create-all [ents]
  (doseq [ent ents]
    (create ent)))

(defn ensure-exists [ent]
  {:pre [(satisfies? CRUD ent)]}
  (when-not (rest/exists? ent)
    (create ent)))

(defn create-recursive
  "Recursively create in katello, all the entites that satisfy
   katello.ui/CRUD (innermost first).  Example, an env that contains
   a field for its parent org, the org would be created first, then
   the env." [ent & [{:keys [check-exist?] :or {check-exist? true}}]]
   (doseq [field (vals ent) :when (satisfies? CRUD field)]
     (create-recursive field))
   (if check-exist? (ensure-exists ent)
       (create ent)))

(defn create-all-recursive [ents & [{:keys [check-exist?] :as m}]]
  (doseq [ent ents]
    (create-recursive ent m)))
;; Locators

(sel/template-fns
 {button-div           "//div[contains(@class,'button') and normalize-space(.)='%s']"  
  editable             "//div[contains(@class, 'editable') and descendant::text()[substring(normalize-space(),2)='%s']]"
  environment-link     "//div[contains(@class,'jbreadcrumb')]//a[normalize-space(.)='%s']"
  left-pane-field-list "xpath=(//div[contains(@class,'left')]//div[contains(@class,'ellipsis') or @class='block tall'])[%s]"
  link                 "link=%s"
  remove-link          "//a[@class='remove_item' and contains(@href,'%s')]"
  third-level-link     "//*[@id='%s']/a"
  menu-link            "//a[contains(@class,'menu-item-link') and normalize-space(.)='%s']"
  menu-dropdown-link   "//ul[contains(@class,'flyout')]//a[normalize-space(.)='%s']"
  search-favorite      "//span[contains(@class,'favorite') and @title='%s']"
  slide-link           "//li[contains(@class,'slide_link') and normalize-space(.)='%s']"
  tab                  "link=%s"
  textbox              "xpath=//*[self::input[(@type='text' or @type='password' or @type='file') and @name='%s'] or self::textarea[@name='%<s']]"
  default-star         "//ul[@id='organizationSwitcher']//a[normalize-space(.)='%s']/../span[contains(@id,'favorite')]"
  switcher-link        "//ul[@id='organizationSwitcher']//li//a[normalize-space(.)='%s' and contains(@class, 'dropdown-menu-item-link')]"
  custom-keyname-list  "xpath=(//td[@class='ra']/label[contains(@for, 'default_info') or contains(@for, 'custom_info')])[%s]"
  custom-value-list    "xpath=(//td/div[@class='editable edit_textfield'])[%s]"})

;;
;; Tells the clojure selenium client where to look up keywords to get
;; real selenium locators.
;;

(defmulti locator (comp find-ns symbol namespace))

(defmacro deflocators
  "Define locators needed in this namespace and its dependent namespaces.
   m is a map of locators.  Optionally, provide other maps containing
   locators needed in this namespace, for m to be merged with.
     Example, (deflocators {:foo 'bar'} other.ns/locators)"
  [m & others]
  `(do (def ~'locators ~m)
       (defmethod locator *ns* [k#]
         (k# (merge ~@others ~'locators)))))

(deflocators
  {::save-inplace-edit       "//div[contains(@class, 'editable')]//button[@type='submit']|//span[contains(@class, 'editable')]//button[@type='submit']"
   ::confirmation-dialog     "//div[contains(@class, 'confirmation')]"

   ;; use index, no other identifiable info in the DOM
   ::confirmation-yes        "xpath=(//div[contains(@class, 'confirmation')]//span[@class='ui-button-text'])[1]" 

   ::switcher                "organizationSwitcher"
   ::active-org              "//a[contains(@class,'organization-name')]"
   ::manage-orgs             "//li[@id='manage_orgs']//span[contains(.,'Manage Organizations')]"
   ::back                    "//div[@id='nav-container']/a[contains(.,'Back')]"
   
   ::search-bar              "search"
   ::search-menu             "//form[@id='search_form']//span[@class='arrow']"
   ::search-save-as-favorite "search_favorite_save"
   ::search-clear-the-search "search_clear"
   ::search-submit           "//button[@form='search_form']"
   ::expand-path             "path-collapsed"
   ::total-results-count     "total_results_count"
   ::current-items-count     "current_items_count"
   ::log-out                 "//div[@ng-controller='MenuController']//a[contains(@href,'logout')]"})

(extend-protocol sel/SeleniumLocatable
  clojure.lang.Keyword
  (sel/sel-locator [k] (locator k))
  String
  (sel/sel-locator [x] x))

(defn toggler
  "Returns a function that returns a locator for the given on/off text
   and locator strategy. Used for clicking things like +Add/Remove for
   items in changesets or permission lists."
  [[on-text off-text] loc-strategy]
  (fn [associated-text on?]
    (loc-strategy (if on? on-text off-text) associated-text)))

(def add-remove ["+ Add" "Remove"])

(defn- item-count [loc]
  (->> loc
     (sel/browser getText)
     Integer/parseInt))

(def current-items
  ^{:doc "Returns the number of shown left pane items according to the katello ui."}
  (partial item-count ::current-items-count))

(def total-items
  ^{:doc "Returns the number of total left pane items according to the katello ui."}
  (partial item-count ::total-results-count))
