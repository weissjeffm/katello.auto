(ns katello.locators
  (:require [com.redhat.qe.auto.selenium.selenium :refer
              [fill-form SeleniumLocatable browser ->browser sel-locator]]
            (katello [conf :refer [config]] 
                     [tasks :refer [capitalize-all]]) 
            
            [clojure.string :refer [capitalize]])
  (:import [com.thoughtworks.selenium SeleniumException]))

;;ui layer

(define-strategies
  {add-repository                  "//div[@id='products']//div[contains(.,'%s')]/..//div[normalize-space(.)='Add Repository' and contains(@class, 'button')]"
   auto-complete-item              "//ul[@role='listbox']//a[contains(.,'%s')]"
   button-div                      "//div[contains(@class,'button') and normalize-space(.)='%s']"

   changeset-status                "//span[.='%s']/..//span[@class='changeset_status']"
   content-search-result-item-n    "//ul[@id='grid_row_headers']/li[%s]"
   content-search-package-name     "//ul[@id='grid_row_headers']/li[%s]/span/span[1]"
   content-search-compare-checkbox "//input[@type='checkbox' and @name='%s']"
   search-result-repo-errata-link  "//a[@data-type='repo_errata' and @data-repo_id='%s']"
   content-search-compare-checkbox-all "//div[@id='grid_content']//input[%s]"
   content-search-repo-remove      "//div[@id='repo_autocomplete_list']/ul/li[@data-name='%s']/i[contains(@class,'remove')]"
   content-search-repo-header-name "//ul[@id='column_headers']/li[%s]/span[1]"
   content-search-repo-column-name "//ul[@id='grid_row_headers']//li[contains(@data-id,'repo')][%s]"
   content-search-column           "//div/span[contains(@class,'checkbox_holder')]/input[@type='checkbox' and @data-node_name='%s']"
   content-search-span-text        "//article[@id='comparison_grid']//span[text()='%s']"
   default-org-star                "//div[@id='orgbox']//a[.='%s']/../span[starts-with(@id,'favorite')]"
   editable                        "//div[contains(@class, 'editable') and descendant::text()[substring(normalize-space(),2)='%s']]"
   environment-link                "//div[contains(@class,'jbreadcrumb')]//a[normalize-space(.)='%s']"
   left-pane-field-list            "xpath=(//div[contains(@class,'left')]//div[contains(@class,'ellipsis') or @class='block tall'])[%s]"
   link                            "link=%s"
   notification-close-index        "xpath=(//div[contains(@class,'jnotify-notification')]//a[@class='jnotify-close'])[%s]"
   notification-index              "xpath=(//div[contains(@class,'jnotify-notification')])[%s]"
   org-switcher                    "//div[@id='orgbox']//a[.='%s']"
   permission-org                  "//li[@class='slide_link' and starts-with(normalize-space(.),'%s')]"
   plus-icon                       "//li[.='%s']//span[contains(@class,'ui-icon-plus')]"
   product-edit                    "//div[@id='products']//div[contains(@data-url, 'edit') and contains(.,'%s')]"
   product-expand                  "//div[@id='products']//div[contains(@data-url,'products') and contains(.,'%s')]/..//img[@alt='Expand']"
   product-schedule                "//div[normalize-space(.)='%s']/following-sibling::div[1]"
   promotion-add-content-item      "//a[@data-display_name='%s' and contains(.,'Add')]"
   promotion-content-category      "//div[@id='%s']"
   promotion-content-item-n        "//div[@id='list']//li[%s]//div[contains(@class,'simple_link')]/descendant::text()[(position()=0 or parent::span) and string-length(normalize-space(.))>0]"
   promotion-remove-content-item   "//a[@data-display_name='%s' and contains(.,'Remove')]"
   provider-sync-checkbox          "//table[@id='products_table']//label[normalize-space(.)='%s']/..//input"
   provider-sync-progress          "//tr[td/label[normalize-space(.)='%s']]/td[5]"
   repo-enable-checkbox            "//table[@id='products_table']//label[normalize-space(.)='%s']/..//input"
   system-environment-checkbox     "//input[@class='node_select' and @type='checkbox' and @data-node_name='%s']" 
   role-action                     "//li[.//span[@class='sort_attr' and .='%2$s']]//a[.='%s']"
   schedule                        "//div[normalize-space(.)='%s']"
   search-favorite                 "//span[contains(@class,'favorite') and @title='%s']"
   search-result-repo-id           "//ul[@id='grid_row_headers']//ul[contains(@id,'child_header_list')]//li[contains(.,'%s')]"
   search-result-col-id            "//ul[@id='column_headers']//li[contains(.,'%s')]"
   search-result-row-id            "//ul[@id='grid_row_headers']/li[contains(.,'%s')]"
   search-result-cell              "//div[@id='grid_row_%s']/div[contains(@class,'cell_%s')]/i"
   slide-link                      "//li[contains(@class,'slide_link') and normalize-space(.)='%s']"
   subscription-available-checkbox "//div[@id='panel-frame']//table[@id='subscribeTable']//td[contains(normalize-space(.),'%s')]//input[@type='checkbox']"
   subscription-current-checkbox   "//div[@id='panel-frame']//table[@id='unsubscribeTable']//td[contains(normalize-space(.),'%s')]//input[@type='checkbox']"
   fetch-applied-subscriptions     "xpath=(//table[@class='filter_table']//a[contains(@href, 'providers') or contains(@href, 'subscriptions')])[%s]"
   fetch-environments-in-org       "xpath=(//div[@id='path-selected']//a/div)[%s]"
   sync-plan                       "//div[@id='plans']//div[normalize-space(.)='%s']"
   system-checkbox                 "//input[@class='system_checkbox' and @type='checkbox' and parent::td[normalize-space(.)='%s']]"
   subscription-checkbox           "//a[.='%s']/../span/input[@type='checkbox']"
   tab                             "link=%s"
   


   textbox                         "xpath=//*[self::input[(@type='text' or @type='password' or @type='file') and @name='%s'] or self::textarea[@name='%<s']]"
   user                            "//div[@id='list']//div[contains(@class,'column_1') and normalize-space(.)='%s']"
   username-field                  "//div[@id='users']//div[normalize-space(.)='%s']"
   select-product                  "//span[contains(.,'%s')]"})

(defn get-all-of-locator [locatorfn] 
  "For locators that accept position and '*' as input, counts xpath-count and returns list of all available locators."
  (let [count (browser getXpathCount (locatorfn "*"))]
     (reduce (fn [accumulator number]
               (conj 
                 accumulator 
                  (locatorfn (str number))))
             []
             (range 1 (inc count)))))

(defn- tabs
  "Takes a list of keywords, and creates mapping eg: {:my-tab 'link=My Tab'}"
  [kws]
  (->> kws
     (map (comp tab
                capitalize-all
                #(.replace % "-" " ")
                name))
     (zipmap kws)))

;;
;;UI locators - mapping of names to selenium locator strings.
;;

(def common
  {:notification            "//div[contains(@class,'jnotify-notification')]"
   :notification-container  "//div[contains(@class,'jnotify-container')]"
   :error-message           "//div[contains(@class,'jnotify-notification-error')]"
   :success-message         "//div[contains(@class,'jnotify-notification-message')]"
   :spinner                 "//img[contains(@src,'spinner.gif')]"
   :save-inplace-edit       "//button[.='Save']"
   :save-inplace-edit-inputbutton       "//input[@value='Save']"
   :confirmation-dialog     "//div[contains(@class, 'confirmation')]"
   :confirmation-yes        "//div[contains(@class, 'confirmation')]//span[.='Yes']"
   :confirmation-no         "//div[contains(@class, 'confirmation')]//span[.='No']"
   :search-bar              "search"
   :search-menu             "//form[@id='search_form']//span[@class='arrow']"
   :search-save-as-favorite "search_favorite_save"
   :search-clear-the-search "search_clear"
   :search-submit           "//button[@form='search_form']"
   ;;main banner
   :account             "//a[@class='header-widget' and contains(@href,'users')]"
   :log-out             "//a[normalize-space(.)='Log Out']"
   :org-switcher        "switcherButton"
   :active-org          "//*[@id='switcherButton']"
   :default-org         "//div[@id='orgbox']//input[@checked='checked' and @class='default_org']/../"
   ;;inside the org switcher
   :manage-organizations-link  "manage_orgs"
   })

(def organizations
  {:new-organization          "//a[@id='new']"
   :create-organization       "organization_submit"
   :org-name-text             "organization[name]"
   :org-description-text      "organization[description]"
   :org-environments          (link "Environments")
   :edit-organization         (link "Edit")
   :remove-organization       (link "Remove Organization")
   :org-initial-env-name-text "environment[name]"
   :org-initial-env-desc-text "environment[description]"})

(def environments
  {:env-name-text             "kt_environment[name]"
   :env-label-text             "kt_environment[label]"
   :env-description-text      "kt_environment[description]"
   :prior-environment         "kt_environment[prior]"
   :create-environment        "//input[@value='Create']"
   :new-environment           "//div[normalize-space(.)='Add New Environment']"
   :remove-environment        (link "Remove Environment")
   :env-prior-select-edit     "kt_environment[prior]" })

(def providers
  {:new-provider                        "new"
   :provider-name-text                  "provider[name]"
   :provider-description-text           "provider[description]"
   :provider-repository-url-text        "provider[repository_url]"
   :provider-cert-text                  (textbox "provider[certificate_attributes][contents]")
   :provider-create-save                "provider_submit"
   :remove-provider                     (link "Remove Provider")
   :subscriptions                       (link "Subscriptions")
   :import-manifest                     "new"
   :redhat-provider-repository-url-text "provider[repository_url]"
   :choose-file                         "provider_contents"
   :upload                              "upload_form_button"
   :force-import-checkbox               "force_import"
   :products-and-repositories           "//nav[contains(@class,'subnav')]//a[contains(.,'Products')]"
                
   ;;add product
   :add-product                         (button-div "Add Product")
   :create-product                      "//input[@value='Create']"
   :product-name-text                   "//*[@name='product[name]']"
   :product-label-text                  "//*[@name='product[label]']"
   :product-description-text            "//*[@name='product[description]']"
   :remove-product                      (link "Remove Product")
   ;;add repo
   :repo-name-text                      "repo[name]"
   :repo-label-text                     "repo[label]"
   :repo-url-text                       "repo[feed]" 
   :save-repository                     "//input[@value='Create']"
   :remove-repository                   (link "Remove Repository")

   ;;redhat page
   :subscriptions-items                 "//table[@id='redhatSubscriptionTable']/tbody/tr"

   ;;gpg keys
   :gpg-key-name-text                   "gpg_key_name"
   :gpg-key-file-upload-text            "gpg_key_content_upload"
   :gpg-key-upload-button               "upload_gpg_key"
   :gpg-key-content-text                "gpg_key_content"
   :gpg-keys                            "//a[.='GPG Keys']"
   :gpg-keys-save                       "save_gpg_key"
   :new-gpg-key                         "new"
   :remove-gpg-key                      (link "Remove GPG Key")


   ;;Package Filters
   :create-new-package-filter                (link "+ New Filter")
   :new-package-filter-name                  "filter[name]"
   :new-package-filter-description           "filter[description]"
   :save-new-package-filter                  "filter_submit"
   :remove-package-filter-key                (link "Remove Filter")})
   
   
   
(def promotions
  {:products-category           (promotion-content-category "products")
   :expand-path                 "path-collapsed"
   :errata-category             (promotion-content-category "errata")
   :packages-category           (promotion-content-category "packages")
   :kickstart-trees-category    (promotion-content-category "kickstart trees")
   :templates-category          (promotion-content-category "templates")
   :promotion-eligible-home     "//div[@id='content_tree']//span[contains(@class,'home_img_inactive')]"

   :review-for-promotion        "review_changeset"
   :promote-to-next-environment "//div[@id='promote_changeset' and not(contains(@class,'disabled'))]"
   :promotion-empty-list        "//div[@id='left_accordion']//ul[contains(.,'available for promotion')]"
   :new-changeset               "//a[contains(.,'New Changeset')]"
   :changeset-name-text         "changeset[name]"
   :save-changeset              "save_changeset_button"
   :changeset-content           "//div[contains(@class,'slider_two') and contains(@class,'has_content')]"
   :changeset-type              "changeset[action_type]"
   :select-deletion-changeset   "//div[@data-cs_type='deletion']"
   :select-repos                "//div[contains(@class,'simple_link') and contains(.,'Repositories')]"
   :select-packages             "//div[contains(@class,'simple_link') and contains(.,'Packages')]"
   :select-errata               "//div[contains(@class,'simple_link') and contains(.,'Errata')]"
   :select-errata-all           "//div[contains(@class,'simple_link') and contains(.,'All')]"})

(def users
  {:roles-subsubtab             "//div[@class='panel-content']//a[.='Roles']"
   :environments-subsubtab      "//div[@class='panel-content']//a[.='Environments']"
   :user-default-org-select     "org_id[org_id]"
   :save-user-environment       "update_user"
   :new-user                    "//a[@id='new']"
   :user-username-text          "user[username]"
   :user-password-text          "password_field" ; use id attr 
   :user-confirm-text           "confirm_field"  ; for these two (name
                                                 ; is the same)
   :user-default-org            "org_id[org_id]"
   :user-email-text             "user[email]"
   :save-user                   "save_user"
   :remove-user                 (link "Remove User")
   :enable-inline-help-checkbox "user[helptips_enabled]"
   :clear-disabled-helptips     "clear_helptips"
   :save-roles                  "save_roles"
   :add-all                     (link "Add all")
   :all-types                   "all_types"
   :password-conflict           "//div[@id='password_conflict' and string-length(.)>0]"})

(def sync-plans
  {:new-sync-plan              "new"
   :sync-plan-name-text        "sync_plan[name]"
   :sync-plan-description-text "sync_plan[description]"
   :sync-plan-interval-select  "sync_plan[interval]"
   :sync-plan-date-text        "sync_plan[plan_date]"
   :sync-plan-time-text        "sync_plan[plan_time]"
   :save-sync-plan             "plan_save"})

(def content-search
  {:content-search-type        "//select[@id='content']"
   :add-prod                   "add_product"
   :add-repo                   "add_repo"
   :repo-result-type-select    "//article[@id='maincontent']//article[@id='comparison_grid']//header//div[@id='left_select']//select"
   :repo-result-filter-select  "//div[@id='right_select']//select"

   :row-headers                "//ul[@id='grid_row_headers']/li"
   :col-headers                "//ul[@id='column_headers']/li"
   :repo-auto-complete-radio   "repos_auto_complete_radio"
   :prod-auto-complete         "product_auto_complete"
   :repo-auto-complete         "repo_auto_complete"
   :pkg-search                 "//div[@id='package_search']/input[@id='search']"
   :errata-search              "//div[@id='errata_search']//input[@id='search']"
   :browse-button              "//input[@id='browse_button']"
   :repo-compare-button        "//a[@id='compare_repos_btn']"
   :content-search-load-more   "//a[contains(@class,'load_row_link')]"
   :column-selector            "//div[@id='column_selector']/span[contains(@class,'path_button')]"
   :details-container          "//div[contains(@class,'details_container')]"
  })

(def systems
  {:new-system                             "new"
   :create-system                          "system_submit"
   :system-name-text                       "system[name]"
   :system-sockets-text                    "system[sockets]"
   :system-arch-select                     "arch[arch_id]"

   ;;system-edit details
   :system-name-text-edit                  "system[name]"
   :system-description-text-edit           "system[description]"
   :system-location-text-edit              "system[location]"
   :system-service-level-select            "system[serviceLevel]"
   :system-release-version-select          "system[releaseVer]"
   :system-environment                     "//div[@id='environment_path_selector']"
   :system-operating-system                "//label[contains(.,'OS')]/../following-sibling::*[1]"
   :system-save-environment                "//input[@value='Save']"

   ;;systemgroups pane
   :new-system-groups                      "//a[@id='new']"
   :create-system-groups                   "group_save"
   :system-group-name-text                 "system_group[name]"
   :system-group-description-text          "system_group[description]"
   :systems-sg                             "//div[@class='panel-content']//a[.='Systems']"
   :system-groups-hostname-toadd           "add_system_input"
   :system-groups-add-system               "add_system"
   :system-groups-remove-system            "remove_systems"
   :system-group-copy                      (link "Copy")
   :system-group-copy-name-text            "name_input"
   :system-group-copy-description-text     "description_input"
   :system-group-copy-submit               "copy_button"
   :system-group-remove                    (link "Remove")
   :system-group-total                     "//fieldset[contains(.,'Total')]/div[2]/a"
   :system-group-confirm-only-system-group "//span[.='No, only delete the system group.']"
   :system-group-unlimited                 "//input[@class='unlimited_members']"
   :save-new-limit                          "//button[.='Save']"
   :system-group-limit-value               "system_group[max_systems]"
   
   ;;subscriptions pane
   :subscribe                              "sub_submit"
   :unsubscribe                            "unsub_submit"

   ;;Activationkeys subtab
   :new-activation-key                     "new"
   :activation-key-name-text               "activation_key[name]"
   :activation-key-description-text        "activation_key[description]"
   :activation-key-template-select         "activation_key[system_template_id]"
   :save-activation-key                    "save_key"
   :applied-subscriptions                  "//a[.='Applied Subscriptions']"
   :available-subscriptions                "//a[.='Available Subscriptions']"
   :add-subscriptions-to-activation-key    "//input[@id='subscription_submit_button']"            
   :remove-activation-key                  (link "Remove Activation Key")
   :subscriptions-right-nav                "//div[contains(@class, 'panel-content')]//a[.='Subscriptions']"
   :release-version-text                   "system[releaseVer]"})

(def roles
  {:new-role                        "//a[@id='new']"
   :new-role-name-text              "role[name]"
   :new-role-description-text       "role[description]"
   :save-role                       "role_save"
   :save-user-edit                  "save_password"
   :role-users                      "role_users"
   :role-permissions                "role_permissions"
   :next                            "next_button"
   :permission-resource-type-select "permission[resource_type_attributes[name]]"
   :permission-verb-select          "permission[verb_values][]"
   :permission-tag-select           "tags"
   :permission-name-text            "permission[name]"
   :permission-description-text     "permission[description]"
   :save-permission                 "save_permission_button"
   :remove-role                     "remove_role"
   :add-permission                  "add_permission"})

(def sync-schedules
  {:apply-sync-schedule "apply_button"})


;;merge all the preceeding maps together, plus a few more items.
(def ^{:doc "All the selenium locators for the Katello UI. Maps a
  keyword to the selenium locator. You can pass the keyword to
  selenium just the same as you would the locator string. See also
  SeleniumLocatable protocol."}
  uimap
  (merge all-tabs common organizations environments roles users systems sync-plans
         content-search sync-schedules promotions providers templates
         { ;; login page
          :username-text     "username"
          :password-text     "password"
          :log-in            "//input[@value='Log In' or @value='Login']"

              
          ;;tabs with special chars in name
          :sub-organizations (tab "Sub-Organizations")
                   

          ;;Sync Management subtab
          :synchronize-now   "sync_button"}))

;;Tells the clojure selenium client where to look up keywords to get
;;real selenium locators (in uimap in this namespace).
(extend-protocol SeleniumLocatable
  clojure.lang.Keyword
  (sel-locator [k] (uimap k))
  String
  (sel-locator [x] x))

(defn promotion-env-breadcrumb
  "Locates a link in the environment breadcrumb UI widget. If there
  are multiple environment paths, and you wish to select Library,
  'next' is required."
  [name & [next]]
  (let [prefix "//a[normalize-space(.)='%s' and contains(@class, 'path_link')"]
    (format 
     (str prefix (if next " and ../../..//a[normalize-space(.)='%s']" "") "]")
     name next)))

(defn inactive-edit-field
  "Takes a locator for an active in-place edit field, returns the
  inactive version"
  [loc]
  (format "//div[@name='%1s']" (sel-locator loc)))

(defn content-search-expand-strategy
  "Returns a locator strategy function for the expansion of the
  current row. The function returned will get any cell by index
  number."
  [current-loc n]
  (partial (template "%s/../ul[%s]/li[%s]") current-loc n))

;;nav tricks



(def user-role-toggler (toggler add-remove role-action))


(def tab-list '(:roles-page
                :users-page 
                :systems-all-page
                :activation-keys-page
                :systems-by-environment-page))

(def ^{:doc "Tabs that don't exist in headpin"}
  katello-only-tabs
  '(:redhat-repositories-page))
