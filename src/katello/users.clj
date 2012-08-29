(ns katello.users
  (:require [katello.locators :as locators]) 
  (:use [com.redhat.qe.auto.selenium.selenium :only [browser]]
        [katello.conf :only [config]]
        [slingshot.slingshot :only [throw+ try+]]
        [katello.ui-tasks :only [navigate fill-ajax-form in-place-edit]]
        [katello.notifications :only [check-for-success]]
        [katello.organizations :only [switch-organization]]))

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
   any user is currently logged in, he will be logged out first."
  [username password & [org]]
  (when (logged-in?) (logout))
  (fill-ajax-form {:username-text username
                   :password-text password}
                  :log-in)
  (let [retVal (check-for-success)]
    (switch-organization (or org (@config :admin-org)))
    retVal))

(defn create-user
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
  (check-for-success))

(defn delete-user "Deletes the given user."
  [username]
  (navigate :named-user-page {:username username})
  (browser click :remove-user)
  (browser click :confirmation-yes)
  (check-for-success))
  
(defn edit-user
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
    (check-for-success))
  (when new-email
    (in-place-edit {:user-email-text new-email})))

(defn current-user
  "Returns the name of the currently logged in user, or nil if logged out."
  []
  (when (logged-in?)
    (browser getText :account)))

(defn ensure-current-user
  "If username is already logged in, does nothing. Otherwise logs in
  with given username and password."
  [username password]
  (if-not (= (current-user) username)
    (login username password)))

(defn assign-user-default-org-and-env 
  "Assigns a default organization and environment to a user"
  [username org-name env-name]
  (navigate :user-environments-page {:username username})
  (browser select :user-default-org-select org-name)
  (browser click (locators/environment-link env-name))
  (browser click :save-user-environment)
  (check-for-success))
