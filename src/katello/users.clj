(ns katello.users
  (:require [com.redhat.qe.auto.selenium.selenium :refer [browser fill-ajax-form]]
            [slingshot.slingshot :refer [throw+ try+]]
            (katello [locators :as locators] 
                     [conf :refer [config *session-user*
                                   *session-password* *session-org*]] 
                     [ui-tasks :refer [navigate in-place-edit]] 
                     [notifications :as notification] 
                     [organizations :as organization])))

;;
;; Users
;;

(defn logged-in?
  "Returns true if the browser is currently showing a page where a
  user is logged in."
  []
  (browser isElementPresent :log-out))

(defn logged-out?
  "Returns true if the login page is displayed."
  []
  (browser isElementPresent :log-in))

(defn logout
  "Logs out the current user from the UI."
  []
  (when-not (logged-out?)
    (browser clickAndWait :log-out)))

(defn login
  "Logs in a user to the UI with the given username and password. If
   none are given, the current value of katello.conf/*session-user*
   *session-password* and *session-org* are used. If any user is
   currently logged in, he will be logged out first. If the user
   doesn't have a default org selected, the value of optional org
   provided will be selected, and optionally also select a future
   default-org. The org and default-org do not have to be the same. If
   the user does have a default already, the org and/or default-org
   will be set after logging in on the dashboard page."
  ([] (login *session-user* *session-password* {:org *session-org*}))
  ([username password & [{:keys [org default-org]}]]
     (when (logged-in?) (logout))
     (fill-ajax-form {:username-text username
                      :password-text password}
                     :log-in)
     (let [retval (notification/check-for-success {:timeout-ms 20000})
           direct-login? (some (fn [n] (or (= "Login Successful" n)
                                          (re-find #"logging into" n)))
                               (mapcat :notices retval))]
       ;; if user only has access to one org, he will bypass org select
       (if direct-login? 
         (browser waitForPageToLoad)
         (do (Thread/sleep 3000)
             (organization/switch (or org
                                      (throw+ {:type ::login-org-required
                                               :msg (format "User %s has no default org, cannot fully log in without specifying an org."
                                                            username)}))
                                  {:default-org default-org})))
       retval)))

(defn create
  "Creates a user with the given name and properties."
  [username {:keys [password password-confirm email default-org default-env]}]
  (navigate :users-page)
  (browser click :new-user)
  (let [env-chooser (fn [env] (when env
                               (locators/select-environment-widget env)))]
    (fill-ajax-form [:user-username-text username
                     :user-password-text password
                     :user-confirm-text (or password-confirm password)
                     :user-email-text email
                     :user-default-org default-org
                     env-chooser [default-env]]
                    :save-user))
  (notification/check-for-success {:match-pred (notification/request-type? :users-create)}))

(defn delete "Deletes the given user."
  [username]
  (navigate :named-user-page {:username username})
  (browser click :remove-user)
  (browser click :confirmation-yes)
  (notification/check-for-success {:match-pred (notification/request-type? :users-destroy)}))
  
(defn edit
  "Edits the given user, changing any of the given properties (can
  change more than one at once)."
  [username {:keys [inline-help clear-disabled-helptips
                    new-password new-password-confirm new-email]}]
  (navigate :named-user-page {:username username})
  (when new-password
    (browser setText :user-password-text new-password)
    (browser setText :user-confirm-text (or new-password-confirm new-password))

    ;;hack alert - force the page to check the passwords (selenium
    ;;doesn't fire the event by itself
    (browser getEval "window.KT.user_page.verifyPassword();")

    (when (browser isElementPresent :password-conflict)
      (throw+ {:type :password-mismatch :msg "Passwords do not match"}))
    (browser click :save-user-edit) 
    (notification/check-for-success))
  (when new-email
    (in-place-edit {:user-email-text new-email})))

(defn current
  "Returns the name of the currently logged in user, or nil if logged out."
  []
  (when (logged-in?)
    (browser getText :account)))

(defn assign-default-org-and-env 
  "Assigns a default organization and environment to a user"
  [username org-name env-name]
  (navigate :user-environments-page {:username username})
  (browser select :user-default-org-select org-name)
  (browser click (locators/environment-link env-name))
  (browser click :save-user-environment)
  (notification/check-for-success))

